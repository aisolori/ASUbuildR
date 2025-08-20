#!/usr/bin/env python3
"""
asu_cpsat.py — Build Areas of Substantial Unemployment (ASUs) with OR-Tools CP-SAT.

Modified to ensure:
1. Seeds are selected from unassigned tracts with high unemployment rate
2. Stops when no remaining unassigned tract has UR >= tau (6.45%)

Two ways to provide adjacency (contiguity):
  A) Precomputed neighbors JSON (recommended on servers without a Geo stack)
     - JSON format: list of lists of tract indices (0- or 1-based). E.g., [[1,4],[2],...]
  B) Geometry file (GeoPackage/Shapefile) + libpysal Queen contiguity

Input table must contain:
  geoid, tract_ASU_unemp, tract_ASU_emp, tract_pop2024

Examples:
  # Using precomputed neighbors (fastest to deploy)
  python asu_cpsat.py --input OR_asu26.xlsx --neighbors nb_queen.json \
    --tau 0.0645 --pop-thresh 10000 --max-asus 30 --time-limit 1200 --workers 8 --verbose

  # Compute queen contiguity from geometry (requires geopandas, libpysal)
  python asu_cpsat.py --input OR_asu26.xlsx --geometry tracts_or_2024.gpkg \
    --geom-col geometry --geoid-col GEOID \
    --tau 0.0645 --pop-thresh 10000 --max-asus 30 --time-limit 1200 --workers 8 --verbose
"""

from __future__ import annotations

import argparse
import json
import math
import os
from typing import List, Optional, Dict, Tuple

import numpy as np
import pandas as pd
import networkx as nx
from ortools.sat.python import cp_model

# Optional (only needed if you compute contiguity on the fly)
try:
    import geopandas as gpd
    from libpysal.weights import Queen
    from shapely.validation import make_valid as shapely_make_valid
except Exception:
    gpd = None
    Queen = None
    shapely_make_valid = None


# ---------- Helpers ----------
def as_fraction_tau(tau: float) -> Tuple[int, int]:
    """Represent k = tau/(1-tau) as num/den using exact integers when tau has 4 decimals."""
    T = int(round(tau * 10000))
    one_minus = 10000 - T
    g = math.gcd(T, one_minus)
    return T // g, one_minus // g  # k = num/den


def ur_of(u_sum: int, E_sum: int) -> float:
    return 0.0 if (u_sum + E_sum) == 0 else u_sum / (u_sum + E_sum)


def bfs_ball(nb: List[List[int]], center: int, r: int, allowed: np.ndarray) -> List[int]:
    allowed_set = set(int(a) for a in allowed)
    vis = {center}
    frontier = [center]
    layer = 0
    while layer < r:
        nxt = []
        for v in frontier:
            for w in nb[v]:
                if (w in allowed_set) and (w not in vis):
                    vis.add(w)
                    nxt.append(w)
        if not nxt:
            break
        frontier = list(set(nxt))
        layer += 1
    return sorted(vis)


def component_ok(S: List[int], u: np.ndarray, E: np.ndarray, P: np.ndarray,
                 tau: float, pop_thresh: int, nb: List[List[int]]) -> bool:
    if not S:
        return False
    Sset = set(S)
    # connectivity (BFS)
    seen = {S[0]}
    Q = [S[0]]
    while Q:
        v = Q.pop()
        for w in nb[v]:
            if (w in Sset) and (w not in seen):
                seen.add(w)
                Q.append(w)
    if len(seen) != len(S):
        return False
    su, sE, sP = int(u[S].sum()), int(E[S].sum()), int(P[S].sum())
    return (sP >= pop_thresh) and (ur_of(su, sE) >= tau)


def can_hit_tau(u: np.ndarray, E: np.ndarray, P: np.ndarray,
                nb_local: List[List[int]], tau: float, pop_thresh: int) -> bool:
    """Quick optimistic screen: if no component can meet UR/pop, skip solving."""
    if len(u) == 0:
        return False
    num, den = as_fraction_tau(tau)  # k = num/den
    D = den * u - num * E
    UR = u / np.maximum(u + E, 1e-12)
    if UR.max(initial=0.0) < tau:
        return False

    G = nx.Graph()
    G.add_nodes_from(range(len(nb_local)))
    for i, nbrs in enumerate(nb_local):
        for j in nbrs:
            if i != j:
                G.add_edge(i, j)

    for comp in nx.connected_components(G):
        idx = np.array(sorted(comp), dtype=int)
        rho = D[idx] / np.maximum(P[idx], 1e-12)
        ord_idx = idx[np.argsort(-rho)]
        need = pop_thresh
        cumD = 0.0
        for j in ord_idx:
            pj, dj = int(P[j]), float(D[j])
            if pj <= 0:
                continue
            take = min(pj, need)
            cumD += dj * (take / pj)
            need -= take
            if need <= 0:
                break
        if need <= 0 and cumD >= 0:
            return True
    return False


def queen_neighbors_from_geometries(gdf: "gpd.GeoDataFrame", geom_col: str = "geometry") -> List[List[int]]:
    if gpd is None or Queen is None:
        raise RuntimeError("geopandas + libpysal required to compute contiguity from geometry.")
    if geom_col not in gdf.columns:
        raise ValueError(f"Geometry column '{geom_col}' not found.")

    gdf = gdf.reset_index(drop=True)
    # basic validity repair
    if hasattr(gdf.geometry, "is_valid"):
        invalid = ~gdf.geometry.is_valid
        if invalid.any():
            gdf.loc[invalid, geom_col] = gdf.loc[invalid, geom_col].buffer(0)
            invalid = ~gdf.geometry.is_valid
            if invalid.any() and shapely_make_valid is not None:
                gdf.loc[invalid, geom_col] = gdf.loc[invalid, geom_col].apply(shapely_make_valid)

    W = Queen.from_dataframe(gdf, ids=list(range(len(gdf))))
    nb = [[] for _ in range(len(gdf))]
    for i, neigh in W.neighbors.items():
        nb[i] = sorted(neigh)
    return nb


# ---------- CP-SAT core: solve one ASU within a window ----------
class CpsatResult:
    def __init__(self, sel_idx_local: List[int], root_local: int, obj: int, status: str):
        self.sel_idx_local = sel_idx_local
        self.root_local = root_local
        self.obj = obj
        self.status = status


def solve_one_asu_cpsat(
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    time_limit: int = 1200,
    workers: int = 8,
    log: bool = True,
    rel_gap: Optional[float] = None,
) -> Optional[CpsatResult]:
    """
    Connectivity via single-commodity flow:
      x_i ∈ {0,1}
      f_e ∈ [0, M] integers for each directed edge (i→j)
    Constraints:
      x_root = 1
      den*Σ u_i x_i - num*Σ E_i x_i ≥ 0    (UR >= tau)
      Σ P_i x_i ≥ pop_thresh
      f_e ≤ M*x_i and f_e ≤ M*x_j
      For i ≠ root:   Σ_in f - Σ_out f = x_i
      For root:       Σ_out f - Σ_in f = Σ_i x_i - 1
    Objective: maximize Σ u_i x_i
    """
    N = len(nb_local)
    if N == 0:
        return None

    # Directed edges (both directions)
    edges = []
    for i, nbrs in enumerate(nb_local):
        for j in nbrs:
            if i != j:
                edges.append((i, j))
    # dedupe exact duplicates; (i,j) and (j,i) are distinct and kept
    edges = list(dict.fromkeys(edges))
    m = len(edges)  # noqa: F841 (kept for clarity)

    model = cp_model.CpModel()

    # Decision variables
    x = [model.NewBoolVar(f"x_{i}") for i in range(N)]
    # Safe upper bound for flows: M = N-1 (tighter M per-edge risks infeasibility)
    M = max(1, N - 1)
    f = [model.NewIntVar(0, M, f"f_{i}_{j}") for (i, j) in edges]

    # Root selection
    model.Add(x[root_local] == 1)

    # Population threshold
    pop_expr = sum(int(P_g[i]) * x[i] for i in range(N))
    model.Add(pop_expr >= int(pop_thresh))

    # UR >= tau as exact integer linear inequality
    num, den = as_fraction_tau(tau)
    lhs = sum(int(den) * int(u_g[i]) * x[i] for i in range(N)) \
        - sum(int(num) * int(E_g[i]) * x[i] for i in range(N))
    model.Add(lhs >= 0)

    # Build incidence lists
    in_edges_for = [[] for _ in range(N)]
    out_edges_for = [[] for _ in range(N)]
    for eidx, (i, j) in enumerate(edges):
        out_edges_for[i].append(eidx)
        in_edges_for[j].append(eidx)

    # Link flows to selected endpoints
    for eidx, (i, j) in enumerate(edges):
        model.Add(f[eidx] <= M * x[i])
        model.Add(f[eidx] <= M * x[j])

    # Flow balances
    for i in range(N):
        inflow = sum(f[e] for e in in_edges_for[i]) if in_edges_for[i] else 0
        outflow = sum(f[e] for e in out_edges_for[i]) if out_edges_for[i] else 0
        if i == root_local:
            model.Add(outflow - inflow == sum(x) - 1)
        else:
            model.Add(inflow - outflow == x[i])

    # Objective: maximize unemployment captured
    model.Maximize(sum(int(u_g[i]) * x[i] for i in range(N)))

    # Solver params
    solver = cp_model.CpSolver()
    solver.parameters.num_search_workers = max(1, int(workers))
    solver.parameters.max_time_in_seconds = float(time_limit)
    solver.parameters.log_search_progress = bool(log)
    solver.parameters.cp_model_presolve = True
    if rel_gap is not None:
        solver.parameters.relative_gap_limit = float(rel_gap)

    status = solver.Solve(model)
    status_name = solver.StatusName(status)
    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        sel = [i for i in range(N) if solver.BooleanValue(x[i])]
        objval = int(round(solver.ObjectiveValue()))
        return CpsatResult(sel_idx_local=sel, root_local=root_local, obj=objval, status=status_name)
    return None


# ---------- Simple improver (local trades) ----------
def frontier_candidates(S: List[int], nb: List[List[int]], allowed: np.ndarray) -> List[int]:
    Sset = set(S)
    allowed_set = set(int(a) for a in allowed)
    cand = set()
    for v in S:
        for w in nb[v]:
            if (w not in Sset) and (w in allowed_set):
                cand.add(w)
    return sorted(cand)


def improve_by_trades(S0: List[int], u: np.ndarray, E: np.ndarray, P: np.ndarray, nb: List[List[int]],
                      tau: float, pop_thresh: int, allowed: np.ndarray, max_iter: int = 200) -> List[int]:
    S = sorted(set(S0))
    for _ in range(max_iter):
        improved = False
        # Greedy adds by descending u
        for t in sorted(frontier_candidates(S, nb, allowed), key=lambda i: u[i], reverse=True):
            S_try = S + [t]
            if component_ok(S_try, u, E, P, tau, pop_thresh, nb):
                S = sorted(S_try)
                improved = True
                break
        if improved:
            continue
        # Swap: drop worst u, add best neighbor
        if len(S) > 1:
            for r in sorted(S, key=lambda i: u[i]):
                S2 = [i for i in S if i != r]
                if not component_ok(S2, u, E, P, tau, pop_thresh, nb):
                    continue
                for a in sorted(frontier_candidates(S2, nb, allowed), key=lambda i: u[i], reverse=True):
                    S_da = sorted(S2 + [a])
                    if component_ok(S_da, u, E, P, tau, pop_thresh, nb) and (u[S_da].sum() > u[S].sum()):
                        S = S_da
                        improved = True
                        break
                if improved:
                    break
        if not improved:
            break
    return S


# ---------- High-level multi-ASU builder ----------
def build_many_asus_cpsat(
    df: pd.DataFrame,
    nb: List[List[int]],
    tau: float,
    pop_thresh: int,
    max_asus: int = 25,
    r_start: int = 50,
    r_step: int = 1,
    r_max: int = 50,
    hard_cap_nodes: int = 10000,
    min_pop_margin: float = 1.0,
    time_limit: int = 1200,
    workers: int = 8,
    rel_gap: Optional[float] = None,
    verbose: bool = True,
) -> Dict[str, np.ndarray]:
    u = df["tract_ASU_unemp"].to_numpy(dtype=np.int64)
    E = df["tract_ASU_emp"].to_numpy(dtype=np.int64)
    P = df["tract_pop2024"].to_numpy(dtype=np.int64)
    UR = u / np.maximum(u + E, 1e-12)

    n = len(df)
    remaining = np.ones(n, dtype=bool)
    tried = np.zeros(n, dtype=bool)
    asu_id = np.full(n, -1, dtype=int)

    k = 0
    while k < max_asus:
        rem_idx = np.where(remaining)[0]
        if rem_idx.size < 2:
            break

        # MODIFIED: First filter for tracts with UR >= tau
        rem_UR = UR[rem_idx]
        high_ur_mask = rem_UR >= tau
        
        # If no remaining tract has UR >= tau, stop building ASUs
        if not high_ur_mask.any():
            if verbose:
                print(f"\nNo remaining tracts have UR >= {tau*100:.2f}%. Stopping ASU creation.")
            break
        
        # Filter to only consider high UR tracts as potential seeds
        high_ur_rem_idx = rem_idx[high_ur_mask]
        
        # Among high UR tracts, find those with at least one remaining neighbor
        deg_rem = np.array([np.sum(remaining[np.array(nb[i], dtype=int)]) for i in high_ur_rem_idx])
        cand_seeds = high_ur_rem_idx[deg_rem > 0]
        
        if cand_seeds.size == 0:
            if verbose:
                print(f"\nNo high-UR tracts (UR >= {tau*100:.2f}%) have remaining neighbors. Stopping.")
            break

        # Prioritize by UR (descending) then population (descending)
        # Since we already filtered for high UR, this sorts among them
        order = np.lexsort((-df.loc[cand_seeds, "tract_pop2024"].to_numpy(), -UR[cand_seeds]))
        seed_pool = cand_seeds[order]

        # Pick first untried seed from high-UR candidates
        seed = None
        for s in seed_pool:
            if not tried[int(s)]:
                seed = int(s)
                break
        if seed is None:
            if verbose:
                print("No remaining high-UR seeds produce a feasible window; stopping.")
            break

        # Expand BFS window until pop threshold (with margin) or radius bound
        r = int(r_start)
        sub = bfs_ball(nb, seed, r, rem_idx)
        while P[sub].sum() < min_pop_margin * pop_thresh and r < r_max and len(sub) < hard_cap_nodes:
            r += r_step
            sub = bfs_ball(nb, seed, r, rem_idx)
        if len(sub) > hard_cap_nodes:
            while len(sub) > hard_cap_nodes and r > 1:
                r -= 1
                sub = bfs_ball(nb, seed, r, rem_idx)
            if len(sub) > hard_cap_nodes:
                sub = sub[:hard_cap_nodes]

        if verbose:
            su, sE = int(u[sub].sum()), int(E[sub].sum())
            URw = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
            seed_ur = 100.0 * UR[seed]
            print(f"\n[ASU {k+1}] seed={seed} (UR={seed_ur:.2f}%) | window: r={r}, nodes={len(sub)}, pop={int(P[sub].sum())}, UR={URw:.2f}%")

        # Build local adjacency for nodes in 'sub'
        local_index = {g: i for i, g in enumerate(sub)}
        nb_local: List[List[int]] = []
        for gnode in sub:
            nb_local.append(sorted([local_index[h] for h in nb[gnode] if h in local_index]))

        u_g, E_g, P_g = u[sub], E[sub], P[sub]
        deg_w = np.array([len(v) for v in nb_local])
        cand = np.where(deg_w > 0)[0]
        if cand.size == 0:
            tried[seed] = True
            continue

        # quick screens
        if (u_g / np.maximum(u_g + E_g, 1e-12)).max(initial=0.0) < tau:
            if verbose:
                print("  skip: window max(UR) < tau")
            tried[seed] = True
            continue
        if not can_hit_tau(u_g, E_g, P_g, nb_local, tau, pop_thresh):
            if verbose:
                print("  skip: quick screen fails")
            tried[seed] = True
            continue

        # choose root (highest UR, then pop)
        top = cand[np.argmax((u_g[cand] / np.maximum(u_g[cand] + E_g[cand], 1e-12)))]
        tie = np.where((u_g / np.maximum(u_g + E_g, 1e-12)) == (u_g[top] / max(u_g[top] + E_g[top], 1e-12)))[0]
        root_local = int(tie[np.argmax(P_g[tie])]) if len(tie) > 1 else int(top)
        if verbose:
            print(f"  root_local={root_local} (UR={100*(u_g[root_local]/max(u_g[root_local]+E_g[root_local],1e-12)):.3f}%, pop={int(P_g[root_local])})")

        # Solve one ASU in the window
        sol = solve_one_asu_cpsat(
            nb_local=nb_local, u_g=u_g, E_g=E_g, P_g=P_g,
            tau=tau, pop_thresh=pop_thresh, root_local=root_local,
            time_limit=time_limit, workers=workers, rel_gap=rel_gap, log=verbose
        )
        if sol is None:
            tried[seed] = True
            continue

        # Map back to global indices
        S_global = np.array(sub, dtype=int)[np.array(sol.sel_idx_local, dtype=int)].tolist()

        # Optional local improvement
        allowed = rem_idx
        S_improved = improve_by_trades(S_global, u, E, P, nb, tau, pop_thresh, allowed, max_iter=200)
        if not component_ok(S_improved, u, E, P, tau, pop_thresh, nb):
            S_improved = S_global

        # Commit ASU
        k += 1
        asu_id[S_improved] = k
        remaining[S_improved] = False
        tried[S_improved] = False

        if verbose:
            su, sE, sP = int(u[S_improved].sum()), int(E[S_improved].sum()), int(P[S_improved].sum())
            URv = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
            print(f"  [OK] ASU {k} committed: tracts={len(S_improved)}, pop={sP}, UR={URv:.3f}%, unemp={su}")

    # Final summary if stopped due to no high-UR tracts
    if verbose and k < max_asus:
        rem_idx_final = np.where(remaining)[0]
        if rem_idx_final.size > 0:
            max_ur_remaining = UR[rem_idx_final].max() * 100
            print(f"\nStopped after {k} ASUs. Max UR among {rem_idx_final.size} remaining tracts: {max_ur_remaining:.3f}%")

    return {"asu_id": asu_id, "n_asu": int(k)}


# ---------- CLI ----------
def main():
    ap = argparse.ArgumentParser(description="ASU builder with OR-Tools CP-SAT (queen contiguity supported)")
    ap.add_argument("--input", required=True, help="Excel/CSV with geoid, tract_ASU_unemp, tract_ASU_emp, tract_pop2024")
    ap.add_argument("--sheet", default=None, help="Excel sheet name (if Excel)")
    ap.add_argument("--neighbors", default=None, help="Adjacency JSON (list of int lists; 0- or 1-based)")
    ap.add_argument("--geometry", default=None, help="GeoPackage / Shapefile with tract polygons (for queen contiguity)")
    ap.add_argument("--geom-col", default="geometry", help="Geometry column name")
    ap.add_argument("--geoid-col", default="geoid", help="Join key in geometry file (to match input geoid)")
    ap.add_argument("--tau", type=float, default=0.0645)
    ap.add_argument("--pop-thresh", type=int, default=10000)
    ap.add_argument("--max-asus", type=int, default=30)
    ap.add_argument("--r-start", type=int, default=50)
    ap.add_argument("--r-step", type=int, default=1)
    ap.add_argument("--r-max", type=int, default=50)
    ap.add_argument("--hard-cap-nodes", type=int, default=10000)
    ap.add_argument("--min-pop-margin", type=float, default=1.0)
    ap.add_argument("--time-limit", type=int, default=1200, help="CP-SAT time limit per window (seconds)")
    ap.add_argument("--workers", type=int, default=8, help="CP-SAT parallel workers")
    ap.add_argument("--rel-gap", type=float, default=None, help="Optional relative gap (e.g., 0.01 for 1%)")
    ap.add_argument("--output", default=None, help="Output CSV path (default: <stem>_with_asu.csv)")
    ap.add_argument("--verbose", action="store_true", help="Verbose CP-SAT logs")
    args = ap.parse_args()

    # Load input table
    inp = args.input
    if inp.lower().endswith((".xlsx", ".xls")):
        if args.sheet is None:
            # pick the first visible sheet
            tmp = pd.read_excel(inp, sheet_name=None)
            first_key = next(iter(tmp.keys()))
            df = tmp[first_key]
        else:
            df = pd.read_excel(inp, sheet_name=args.sheet)
    else:
        df = pd.read_csv(inp)

    # Normalize geoid (strip 14000US prefix if present)
    if "geoid" in df.columns:
        df["geoid"] = df["geoid"].astype(str).str.replace(r"^14000US", "", regex=True)

    required = ["tract_ASU_unemp", "tract_ASU_emp", "tract_pop2024"]
    for col in required:
        if col not in df.columns:
            raise ValueError(f"Missing required column: {col}")

    # Build adjacency
    if args.neighbors:
        with open(args.neighbors, "r") as f:
            nb_raw = json.load(f)
        if not isinstance(nb_raw, list):
            raise ValueError("neighbors JSON must be a list of lists")
        # Convert each row to 0-based ints; handle 1-based input from R
        n = len(nb_raw)
        nb: List[List[int]] = []
        for row in nb_raw:
            row = [int(v) for v in (row or [])]
            is_one_based = len(row) > 0 and max(row) >= n
            if is_one_based:
                row = [v - 1 for v in row]
            nb.append(sorted([v for v in row if 0 <= v < n]))
    elif args.geometry:
        if gpd is None or Queen is None:
            raise RuntimeError("geopandas/libpysal not installed. Use --neighbors JSON instead, or install geo deps.")
        gdf = gpd.read_file(args.geometry)
        if args.geoid_col not in gdf.columns:
            raise ValueError(f"Column '{args.geoid_col}' not found in geometry file.")
        # Join geometry to df by geoid
        gdf2 = gdf[[args.geoid_col, args.geom_col]].rename(columns={args.geoid_col: "geoid"})
        merged = df.merge(gdf2, on="geoid", how="left")
        if merged[args.geom_col].isna().any():
            missing = merged["geoid"][merged[args.geom_col].isna()].unique()[:5]
            raise RuntimeError(f"Missing geometry for some geoids (e.g., {missing}).")
        gdf_merged = gpd.GeoDataFrame(merged, geometry=args.geom_col, crs=gdf.crs).reset_index(drop=True)
        nb = queen_neighbors_from_geometries(gdf_merged, geom_col=args.geom_col)
        # Drop geometry for output size
        df = pd.DataFrame(gdf_merged.drop(columns=[args.geom_col]))
    else:
        raise RuntimeError("Provide either --neighbors JSON or --geometry to compute contiguity.")

    # Build ASUs
    out = build_many_asus_cpsat(
        df=df, nb=nb, tau=args.tau, pop_thresh=args.pop_thresh,
        max_asus=args.max_asus, r_start=args.r_start, r_step=args.r_step, r_max=args.r_max,
        hard_cap_nodes=args.hard_cap_nodes, min_pop_margin=args.min_pop_margin,
        time_limit=args.time_limit, workers=args.workers, rel_gap=args.rel_gap,
        verbose=args.verbose,
    )

    df_out = df.copy()
    df_out["asu_id"] = out["asu_id"]

    out_path = args.output or f"{os.path.splitext(os.path.basename(inp))[0]}_with_asu.csv"
    df_out.to_csv(out_path, index=False)
    print(f"\nDone. Built {out['n_asu']} ASU(s) → {out_path}")


if __name__ == "__main__":
    main()
