#!/usr/bin/env python3
"""
asu_cpsat.py — Build Areas of Substantial Unemployment (ASUs) with OR-Tools CP-SAT.

Modified to ensure:
1. Seeds are selected from unassigned tracts with high unemployment rate
2. Stops when no remaining unassigned tract has UR >= tau (6.45%)
3. Warm starts can reroute around articulation points before pruning/refilling
4. Proven-optimal ties prefer more threshold slack, then fewer tracts

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
import concurrent.futures
import heapq
import json
import math
import os
import threading
import time
from dataclasses import dataclass
from typing import List, Optional, Dict, Sequence, Tuple

import numpy as np
import pandas as pd
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
    """Calculate unemployment rate from counts, avoiding divide-by-zero."""
    return 0.0 if (u_sum + E_sum) == 0 else u_sum / (u_sum + E_sum)


def _stop_requested(stop_flag_path: Optional[str]) -> bool:
    """Return True once an external stop-signal file has been created."""
    return bool(stop_flag_path) and os.path.exists(stop_flag_path)


def _consume_flag(flag_path: Optional[str]) -> None:
    """Best-effort delete of a one-shot signal file so it doesn't re-trigger."""
    if not flag_path:
        return
    try:
        os.remove(flag_path)
    except OSError:
        pass


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


def greedy_snake_hint(
    nb_local: List[List[int]],
    u_g: np.ndarray, # unemployment counts per tract
    E_g: np.ndarray, # employment counts per tract
    P_g: np.ndarray, # population counts per tract
    tau: float, # unemployment rate threshold for ASU selection
    pop_thresh: int, # minimum population threshold for ASU selection
    root_local: int,
) -> List[int]:
    """
    Run Simple Snake across the full local window, then combine touching groups.
    Phase 1: seed root_local first, then exhaust all remaining high-UR seeds.
    Phase 2: merge any groups adjacent to root's group that keep UR >= tau.
    Returns the resulting merged group (local indices) as the CP-SAT warm-start hint.
    """
    N = len(nb_local)
    UR = u_g / np.maximum(u_g + E_g, 1e-12)
    assigned = np.full(N, -1, dtype=int)   # group id per node, -1 = unassigned
    remaining = np.ones(N, dtype=bool)

    def _expand(seed: int) -> set:
        sel = {seed}
        sel_u = int(u_g[seed])
        sel_e = int(E_g[seed])
        sel_p = int(P_g[seed])
        # incremental frontier: O(N·degree) total instead of O(N²·degree)
        frontier = {w for w in nb_local[seed] if remaining[w]}
        while frontier:
            best, best_ur = None, -1.0
            for cand in frontier:
                cu = sel_u + int(u_g[cand])
                ce = sel_e + int(E_g[cand])
                cur = cu / (cu + ce) if cu + ce > 0 else 0.0
                if cur > best_ur:
                    best_ur, best = cur, cand
            if best is None or (best_ur < tau and sel_p >= pop_thresh):
                break
            sel.add(best)
            frontier.discard(best)
            sel_u += int(u_g[best])
            sel_e += int(E_g[best])
            sel_p += int(P_g[best])
            for w in nb_local[best]:
                if w not in sel and remaining[w]:
                    frontier.add(w)
        return sel

    # Phase 1: seed root_local first, then exhaust remaining high-UR seeds
    groups: List[set] = []

    root_set = _expand(root_local)
    root_gid = 0
    for v in root_set:
        assigned[v] = root_gid
        remaining[v] = False
    groups.append(root_set)

    while True:
        rem = np.where(remaining & (UR >= tau))[0]
        if rem.size == 0:
            break
        seed = int(rem[np.argmax(UR[rem])])
        sel = _expand(seed)
        gid = len(groups)
        for v in sel:
            assigned[v] = gid
            remaining[v] = False
        groups.append(sel)

    # Phase 2: merge groups that touch root_gid and keep combined UR >= tau
    pending: set = set()
    for v in groups[root_gid]:
        for w in nb_local[v]:
            g = assigned[w]
            if g not in (-1, root_gid) and len(groups[g]) > 0:
                pending.add(g)

    while pending:
        gid = pending.pop()
        if len(groups[gid]) == 0:
            continue
        combined = list(groups[root_gid] | groups[gid])
        cu = int(u_g[combined].sum())
        ce = int(E_g[combined].sum())
        if cu + ce > 0 and cu / (cu + ce) >= tau:
            new_members = groups[gid]
            for v in new_members:
                assigned[v] = root_gid
            groups[root_gid] = groups[root_gid] | new_members
            groups[gid] = set()
            # discover neighbours newly exposed by the merged members
            for v in new_members:
                for w in nb_local[v]:
                    g = assigned[w]
                    if g not in (-1, root_gid) and len(groups[g]) > 0:
                        pending.add(g)

    return sorted(groups[root_gid])


def _articulation_points(nb_local: List[List[int]], selected: np.ndarray) -> set:
    """
    Iterative Tarjan articulation-point finder restricted to the induced subgraph
    on `selected` nodes. A cut vertex's removal disconnects the remainder of its
    connected component, so these are never valid drop candidates for
    reverse_prune_hint. Mirrors the iterative low-link style of
    _bridge_edge_bounds but for vertices instead of edges.
    Returns a set of local node indices that are cut vertices.
    """
    N = len(nb_local)
    disc = [-1] * N
    low = [0] * N
    parent = [-1] * N
    skipped_parent = [False] * N
    root_children = [0] * N
    is_art: set = set()
    timer = 0

    for start in range(N):
        if not selected[start] or disc[start] != -1:
            continue
        stack = [(start, iter(nb_local[start]))]
        disc[start] = low[start] = timer
        timer += 1
        while stack:
            u, it = stack[-1]
            recursed = False
            for w in it:
                if not selected[w]:
                    continue
                if w == parent[u] and not skipped_parent[u]:
                    skipped_parent[u] = True
                    continue
                if disc[w] == -1:
                    parent[w] = u
                    if parent[u] == -1:
                        root_children[start] += 1
                    disc[w] = low[w] = timer
                    timer += 1
                    stack.append((w, iter(nb_local[w])))
                    recursed = True
                    break
                else:
                    low[u] = min(low[u], disc[w])
            if not recursed:
                stack.pop()
                if stack:
                    p = stack[-1][0]
                    low[p] = min(low[p], low[u])
                    if parent[p] != -1 and low[u] >= disc[p]:
                        is_art.add(p)
        if root_children[start] > 1:
            is_art.add(start)
    return is_art


def _root_articulation_implications(
    nb_local: List[List[int]], root_local: int,
) -> List[Tuple[int, int]]:
    """Return (node, cut_vertex) pairs where selecting node requires cut_vertex."""
    selected = np.ones(len(nb_local), dtype=bool)
    cut_vertices = _articulation_points(nb_local, selected)

    root_component = {root_local}
    stack = [root_local]
    while stack:
        node = stack.pop()
        for neighbor in nb_local[node]:
            if neighbor not in root_component:
                root_component.add(neighbor)
                stack.append(neighbor)

    implications: List[Tuple[int, int]] = []
    for cut_vertex in sorted(cut_vertices - {root_local}):
        reachable = {root_local}
        stack = [root_local]
        while stack:
            node = stack.pop()
            for neighbor in nb_local[node]:
                if neighbor != cut_vertex and neighbor not in reachable:
                    reachable.add(neighbor)
                    stack.append(neighbor)
        implications.extend(
            (node, cut_vertex)
            for node in sorted(root_component - reachable - {cut_vertex})
        )
    return implications


def _bounded_root_vertex_separator(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    max_size: int,
) -> Optional[Tuple[int, ...]]:
    """Return a minimum root-target vertex separator when its size is 2..max_size."""
    N = len(nb_local)
    if target == root_local or max_size < 2:
        return None

    cutoff = int(max_size) + 1
    residual: List[Dict[int, int]] = [dict() for _ in range(2 * N)]

    def _add_arc(start: int, end: int, capacity: int) -> None:
        residual[start][end] = residual[start].get(end, 0) + capacity
        residual[end].setdefault(start, 0)

    for node in range(N):
        capacity = cutoff if node in (root_local, target) else 1
        _add_arc(2 * node, 2 * node + 1, capacity)

    undirected_edges = {
        (min(node, neighbor), max(node, neighbor))
        for node, neighbors in enumerate(nb_local)
        for neighbor in neighbors
        if node != neighbor
    }
    for left, right in undirected_edges:
        _add_arc(2 * left + 1, 2 * right, cutoff)
        _add_arc(2 * right + 1, 2 * left, cutoff)

    source = 2 * root_local + 1
    sink = 2 * target
    flow = 0
    while flow < cutoff:
        parent = [-1] * (2 * N)
        parent[source] = source
        queue = [source]
        head = 0
        while head < len(queue) and parent[sink] < 0:
            node = queue[head]
            head += 1
            for neighbor, capacity in residual[node].items():
                if capacity > 0 and parent[neighbor] < 0:
                    parent[neighbor] = node
                    queue.append(neighbor)
                    if neighbor == sink:
                        break
        if parent[sink] < 0:
            break

        amount = cutoff - flow
        node = sink
        while node != source:
            previous = parent[node]
            amount = min(amount, residual[previous][node])
            node = previous
        node = sink
        while node != source:
            previous = parent[node]
            residual[previous][node] -= amount
            residual[node][previous] = residual[node].get(previous, 0) + amount
            node = previous
        flow += amount

    if flow < 2 or flow > max_size:
        return None

    reachable = {source}
    queue = [source]
    head = 0
    while head < len(queue):
        node = queue[head]
        head += 1
        for neighbor, capacity in residual[node].items():
            if capacity > 0 and neighbor not in reachable:
                reachable.add(neighbor)
                queue.append(neighbor)

    separator = tuple(
        node for node in range(N)
        if node not in (root_local, target)
        and 2 * node in reachable
        and 2 * node + 1 not in reachable
    )
    return separator if 2 <= len(separator) <= max_size else None


def _small_root_separator_implications(
    nb_local: List[List[int]],
    root_local: int,
    node_value: np.ndarray,
    q_surplus: Optional[np.ndarray] = None,
    max_size: int = 3,
    clause_limit: int = 200,
    target_limit: int = 128,
) -> Tuple[List[Tuple[int, Tuple[int, ...]]], List[Tuple[Tuple[int, ...], Tuple[int, ...], int]]]:
    """
    Find capped size-2/3 separators and the nodes they disconnect from root.
    Returns (implications, component_bounds):
      - implications: per-node clauses x_i <= OR(x_s for s in separator)
      - component_bounds: (separator, affected_nodes, K_C) aggregate cardinality
        bounds, where K_C is the max number of nodes in `affected` that could
        possibly be selected given the UR-surplus (q_surplus) available to the
        rest of the graph. Combined with an activation var z_C <= sum(x_s), the
        caller can add sum(x_i for i in affected) <= K_C * z_C -- ignoring
        connectivity/population/root restrictions like the global cardinality
        bound already used for M, so it stays a valid upper bound even though
        it's cheap (just a sort) and computed per-component.
    """
    N = len(nb_local)
    max_size = max(2, int(max_size))
    clause_limit = max(0, int(clause_limit))
    implications: List[Tuple[int, Tuple[int, ...]]] = []
    component_bounds: List[Tuple[Tuple[int, ...], Tuple[int, ...], int]] = []
    if N <= 2 or clause_limit == 0:
        return implications, component_bounds

    total_positive_q = (
        int(np.clip(q_surplus, 0, None).sum()) if q_surplus is not None else None
    )

    target_order = sorted(
        (node for node in range(N) if node != root_local),
        key=lambda node: (-int(node_value[node]), node),
    )[:max(1, int(target_limit))]
    seen_separators: set = set()

    for target in target_order:
        separator = _bounded_root_vertex_separator(
            nb_local, root_local, target, max_size
        )
        if separator is None or separator in seen_separators:
            continue
        seen_separators.add(separator)

        blocked = set(separator)
        reachable = {root_local}
        queue = [root_local]
        head = 0
        while head < len(queue):
            node = queue[head]
            head += 1
            for neighbor in nb_local[node]:
                if neighbor not in blocked and neighbor not in reachable:
                    reachable.add(neighbor)
                    queue.append(neighbor)

        affected = sorted(
            (
                node for node in range(N)
                if node not in reachable and node not in blocked
            ),
            key=lambda node: (-int(node_value[node]), node),
        )
        clause_limit_hit = False
        for node in affected:
            implications.append((node, separator))
            if len(implications) >= clause_limit:
                clause_limit_hit = True
                break

        if affected and q_surplus is not None and total_positive_q is not None:
            positive_within = int(np.clip(q_surplus[affected], 0, None).sum())
            budget = total_positive_q - positive_within
            sorted_q = sorted((int(v) for v in q_surplus[affected]), reverse=True)
            k, running = 0, 0
            for value in sorted_q:
                if running + value < -budget:
                    break
                running += value
                k += 1
            if k < len(affected):
                component_bounds.append((separator, tuple(affected), k))

        if clause_limit_hit:
            break

    return implications, component_bounds


def _rank01(values: np.ndarray) -> np.ndarray:
    """Return average ranks scaled to [0, 1], with equal values tied."""
    values = np.asarray(values, dtype=float)
    n = len(values)
    if n <= 1:
        return np.ones(n, dtype=float)

    order = np.argsort(values, kind="stable")
    ranks = np.empty(n, dtype=float)
    sorted_values = values[order]
    start = 0
    while start < n:
        end = start + 1
        while end < n and sorted_values[end] == sorted_values[start]:
            end += 1
        ranks[order[start:end]] = 0.5 * (start + end - 1)
        start = end
    return ranks / (n - 1)


def _asu_branch_order(
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    root_local: int,
    num: int,
    den: int,
    hint: Optional[List[int]] = None,
    root_implications: Optional[Sequence[Tuple[int, int]]] = None,
) -> Tuple[List[int], np.ndarray]:
    """Rank tract variables for one domain-specific partial-search worker."""
    N = len(nb_local)
    rate_slack = (
        int(den) * u_g.astype(np.int64)
        - int(num) * E_g.astype(np.int64)
    )
    degree = np.array([len(neighbors) for neighbors in nb_local], dtype=float)
    downstream = np.zeros(N, dtype=float)

    implications = root_implications
    if implications is None:
        implications = _root_articulation_implications(nb_local, root_local)
    for _, cut_vertex in implications:
        downstream[cut_vertex] += 1

    score = (
        0.45 * _rank01(u_g)
        + 0.25 * _rank01(rate_slack)
        + 0.10 * _rank01(degree)
        + 0.20 * _rank01(downstream)
    )
    if hint is not None:
        hint_set = set(hint)
        score += 0.10 * np.array(
            [i in hint_set for i in range(N)],
            dtype=float,
        )

    order = np.argsort(-score, kind="stable")
    return order.astype(int).tolist(), score


def _asu_branch_challenges(
    nb_local: List[List[int]],
    branch_order: Sequence[int],
    hint: Optional[Sequence[int]],
    protected: Sequence[int],
    max_prefix: int = 64,
) -> Tuple[List[int], List[int]]:
    """Rank one-hop additions and removable nodes on the incumbent boundary."""
    prefix_budget = max(1, int(max_prefix))
    add_budget = (prefix_budget + 1) // 2
    drop_budget = prefix_budget // 2
    hint_set = set(int(i) for i in (hint or []))
    protected_set = set(int(i) for i in protected)

    selected_mask = np.zeros(len(nb_local), dtype=bool)
    if hint_set:
        selected_mask[list(hint_set)] = True
    protected_set.update(_articulation_points(nb_local, selected_mask))

    frontier = {
        neighbor
        for node in hint_set
        for neighbor in nb_local[node]
        if neighbor not in hint_set
    }
    boundary = {
        node for node in hint_set
        if any(neighbor not in hint_set for neighbor in nb_local[node])
    }

    additions = [
        int(i) for i in branch_order
        if i in frontier
    ][:add_budget]
    removals = [
        int(i) for i in reversed(branch_order)
        if i in boundary and i not in protected_set
    ][:drop_budget]
    return additions, removals


_ASU_FULL_SUBSOLVER_PATTERN = (
    "asu_probe_standard",
    "asu_probe_very_deep",
    "lb_tree_search",
    "portfolio_max_lp",

    "max_lp",
    "asu_probe_fast",
    "asu_probe_very_deep",

    "asu_probe_deep",
    "quick_restart_no_lp",
    "core_max_lp",
    "portfolio_max_lp",
    "asu_probe_fast",

    "asu_probe_standard",
    "lb_tree_search",
    "pseudo_costs",
    "reduced_costs",
)
def _asu_full_subsolvers(
    workers: int,
    use_tract_first_search: bool = False,
    use_tract_first_probing: bool = False,
) -> List[str]:
    """Return the bounded full-problem portfolio for one ASU solve."""
    workers = max(1, int(workers))
    if workers < 8:
        return []

    full_budget = max(3, min(16, round(workers / 3)))
    full_subsolvers = list(_ASU_FULL_SUBSOLVER_PATTERN[:full_budget])
    if use_tract_first_search:
        # Preserve reduced-cost and pseudo-cost search. At large budgets, use
        # one of the duplicate max-LP slots for the boundary worker instead.
        max_lp_indices = [
            index for index, name in enumerate(full_subsolvers)
            if name == "max_lp"
        ]
        replace_index = (
            max_lp_indices[-1]
            if len(max_lp_indices) > 1
            else full_subsolvers.index("portfolio_max_lp")
            if "portfolio_max_lp" in full_subsolvers
            else len(full_subsolvers) - 1
        )
        full_subsolvers[replace_index] = "asu_tract_first"
    if use_tract_first_probing:
        for source, replacement in (
            ("asu_probe_fast", "asu_probe_fast_tract_first"),
            ("asu_probe_standard", "asu_probe_standard_tract_first"),
        ):
            source_indices = [
                index for index, name in enumerate(full_subsolvers)
                if name == source
            ]
            if len(source_indices) > 1:
                full_subsolvers[source_indices[-1]] = replacement
    return full_subsolvers


def _append_asu_subsolver_params(params, name: str, **overrides) -> None:
    """Append owned subsolver parameters compatible with OR-Tools 9.14 and 9.15."""
    subsolver_params = type(params)()
    subsolver_params.name = name
    for field, value in overrides.items():
        setattr(subsolver_params, field, value)
    params.subsolver_params.append(subsolver_params)


def _supports_tract_first_probing(params) -> bool:
    """Return whether this OR-Tools build supports Boolean-first probing."""
    descriptor = getattr(params, "DESCRIPTOR", None)
    return (
        descriptor is not None
        and "continuous_probing_order" in descriptor.fields_by_name
        and hasattr(type(params), "CONTINUOUS_PROBING_BOOLEANS_FIRST")
    )


def _configure_asu_lp_search_params(params) -> None:
    """Apply LP branching to compatible full and local solves."""
    _append_asu_subsolver_params(
        params,
        "lns_base",
        linearization_level=2,
        search_branching=cp_model.LP_SEARCH,
    )


def _configure_asu_probe_variants(
    params,
    tract_first: bool = False,
) -> bool:
    """Register ContinuousProber workers and return tract-first availability."""

    common = {
        # ContinuousProber worker, not normal tree search.
        "search_branching": cp_model.AUTOMATIC_SEARCH,
        "use_probing_search": True,
        "use_extended_probing": True,
        "at_most_one_max_expansion_size": 2,

        "shaving_deterministic_time_in_probing_search": 0.001,
        # NOTE: CP-SAT default is 20000; was 0 here, which fully disabled
        # random-triplet-of-bool-vars probing (see integer_search.cc,
        # ContinuousProber::Probe: loop_limit=limit when num_bool_vars
        # exceeds sqrt/cbrt(2*limit), always true for our 1000+ bool var
        # models). Random-pair probing is unaffected either way (hardcoded
        # 10000-iteration fallback). A/B tested on real data (seed=1173,
        # matched 600s budget): asu_probe_fast/standard's own "improving
        # bounds shared" dropped a lot (47->11, 23->14) -- triplet probing
        # does eat into their productive time, as expected -- but
        # lb_tree_search's share grew far more (53->431) and objective/
        # gap_integral came out flat-to-slightly-better (75425 vs 75422,
        # gap_integral 9339.88 vs 9485.54). Kept at the CP-SAT default.
        "probing_num_combinations_limit": 0,

        "linearization_level": 2,
        "add_lp_constraints_lazily": False,
        "max_cut_rounds_at_level_zero": 4,
    }

    
    variants = [
        ("asu_probe_fast",       25_000, 0.05, False),
        ("asu_probe_standard", 50_000, 0.01, False),
        ("asu_probe_deep", 100_000, 0.01, False),
        ("asu_probe_very_deep", 200_000, 0.01, False)
    ]

    tract_first_enabled = tract_first and _supports_tract_first_probing(params)
    if tract_first_enabled:
        variants.extend([
            ("asu_probe_fast_tract_first", 50_000, 0.0005, True),
            ("asu_probe_standard_tract_first", 100_000, 0.0005, True),
        ])

    for name, root_iterations, shaving_time, boolean_first in variants:
        overrides = dict(common)
        overrides["root_lp_iterations"] = root_iterations
        overrides["shaving_search_deterministic_time"] = shaving_time
        if boolean_first:
            overrides["continuous_probing_order"] = (
                type(params).CONTINUOUS_PROBING_BOOLEANS_FIRST
            )

        _append_asu_subsolver_params(
            params,
            name,
            **overrides,
        )

    return tract_first_enabled


def _configure_asu_pseudo_costs(params) -> None:
    """Register a genuine pseudo-cost branch-and-bound worker."""

    _append_asu_subsolver_params(
        params,
        "asu_pseudo_costs",

        search_branching=cp_model.PSEUDO_COST_SEARCH,

        # Important: allow normal tree search.
        use_probing_search=False,

        linearization_level=2,

        # OR-Tools also enables this on its built-in pseudo_costs worker.
        exploit_best_solution=True,
    )


def _configure_asu_shared_tree(params, tract_first: bool = False) -> None:
    """Configure coordinated proof workers around the global objective bound."""
    params.shared_tree_num_workers = 0
    params.shared_tree_split_strategy = (
        type(params).SPLIT_STRATEGY_OBJECTIVE_LB
    )
    if tract_first:
        _append_asu_subsolver_params(
            params,
            "shared_tree",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
        )


def reverse_prune_hint(
    nb_local: List[List[int]],
    u_g: np.ndarray,  # tract unemployment counts
    E_g: np.ndarray,  # tract employment counts
    P_g: np.ndarray,  # tract population counts
    tau: float,
    pop_thresh: int,
    root_local: int,
) -> List[int]:
    """
    Warm start via reverse pruning.

    Start with every tract selected, then repeatedly remove the valid tract
    with the lowest economic efficiency:

        efficiency = unemployed / rate-capacity cost

        rate-capacity cost =
            tau * employed - (1 - tau) * unemployed

    A positive capacity cost means the tract's unemployment rate is below
    tau and therefore consumes unemployment-rate slack.

    The root, articulation points, and removals that violate the population
    threshold are excluded. Stops when aggregate UR reaches tau or no valid
    removal remains.
    """
    N = len(nb_local)
    selected = np.ones(N, dtype=bool)

    U_sum = int(u_g.sum())
    E_sum = int(E_g.sum())
    P_sum = int(P_g.sum())

    # Amount of threshold capacity consumed by each tract.
    capacity_cost = tau * E_g - (1.0 - tau) * u_g

    while ur_of(U_sum, E_sum) < tau:
        cut_vertices = _articulation_points(nb_local, selected)

        droppable = selected.copy()
        droppable[root_local] = False

        for v in cut_vertices:
            droppable[v] = False

        # Removing a tract must preserve the population requirement.
        droppable &= (P_sum - P_g) >= pop_thresh

        # Only below-threshold tracts consume rate capacity.
        # Removing one of these necessarily improves aggregate threshold slack.
        droppable &= capacity_cost > 0

        cand_idx = np.flatnonzero(droppable)

        if cand_idx.size == 0:
            break

        candidate_cost = capacity_cost[cand_idx]

        # Lower efficiency means fewer unemployed are sacrificed for each
        # unit of rate capacity recovered.
        efficiency = np.divide(
            u_g[cand_idx].astype(float),
            candidate_cost,
            out=np.full(cand_idx.size, np.inf, dtype=float),
            where=candidate_cost > 0,
        )

        best = int(cand_idx[np.argmin(efficiency)])

        selected[best] = False
        U_sum -= int(u_g[best])
        E_sum -= int(E_g[best])
        P_sum -= int(P_g[best])

    return np.flatnonzero(selected).astype(int).tolist()

def _spanning_tree_flows(hint: List[int], nb_local: List[List[int]], root_local: int) -> Dict[Tuple[int, int], int]:
    """
    Compute single-commodity flow values for the BFS spanning tree of hint.
    Each tree edge (parent→child) carries flow = subtree size at child.
    Returns a dict of (i, j) -> flow for non-zero entries only.
    """
    if not hint:
        return {}
    hint_set = set(hint)
    parent: Dict[int, Optional[int]] = {root_local: None}
    children: Dict[int, List[int]] = {v: [] for v in hint}
    queue = [root_local]
    order: List[int] = []
    while queue:
        v = queue.pop(0)
        order.append(v)
        for w in nb_local[v]:
            if w in hint_set and w not in parent:
                parent[w] = v
                children[v].append(w)
                queue.append(w)
    size: Dict[int, int] = {v: 1 for v in hint}
    for v in reversed(order):
        for c in children[v]:
            size[v] += size[c]
    flows: Dict[Tuple[int, int], int] = {}
    for v in hint:
        p = parent[v]
        if p is not None:
            flows[(p, v)] = size[v]
    return flows


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
    UR = u / np.maximum(u + E, 1e-12)
    if UR.max(initial=0.0) < tau:
        return False
    # BFS-ball windows are always connected; single-component knapsack is sufficient
    num, den = as_fraction_tau(tau)
    D = den * u - num * E
    rho = D / np.maximum(P, 1e-12)
    ord_idx = np.argsort(-rho)
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
    return need <= 0 and cumD >= 0


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


def contract_high_ur_nodes(
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
) -> Tuple[List[List[int]], np.ndarray, np.ndarray, np.ndarray, List[List[int]], np.ndarray]:
    """
    Fuse each connected cluster of UR>=tau tracts into one super-node (exact, lossless).
    Mediant inequality: mixing two UR>=tau distributions keeps combined UR>=tau, so
    every cluster member co-occurs in every optimal solution — no branching needed.
    Returns: nb_r, u_r, E_r, P_r, expand, node_map
      expand[ri]  = sorted list of original local indices for reduced node ri
      node_map[v] = reduced index for original local node v
    """
    N = len(nb_local)
    num, den = as_fraction_tau(tau)
    high_set = {
        i for i in range(N)
        if int(den) * int(u_g[i]) - int(num) * int(E_g[i]) >= 0
    }

    # BFS over induced high-UR subgraph to label connected components
    comp = np.full(N, -1, dtype=int)
    c = 0
    for v in sorted(high_set):
        if comp[v] >= 0:
            continue
        stk = [v]
        comp[v] = c
        while stk:
            cur = stk.pop()
            for w in nb_local[cur]:
                if w in high_set and comp[w] < 0:
                    comp[w] = c
                    stk.append(w)
        c += 1
    n_super = c

    # Reduced indices: super-nodes 0..n_super-1, then one index per low-UR node
    node_map = np.empty(N, dtype=int)
    next_low = n_super
    for v in range(N):
        if v in high_set:
            node_map[v] = comp[v]
        else:
            node_map[v] = next_low
            next_low += 1
    N_r = next_low

    expand: List[List[int]] = [[] for _ in range(N_r)]
    for v in range(N):
        expand[node_map[v]].append(v)

    u_r = np.array([int(u_g[expand[ri]].sum()) for ri in range(N_r)], dtype=np.int64)
    E_r = np.array([int(E_g[expand[ri]].sum()) for ri in range(N_r)], dtype=np.int64)
    P_r = np.array([int(P_g[expand[ri]].sum()) for ri in range(N_r)], dtype=np.int64)

    adj_sets: List[set] = [set() for _ in range(N_r)]
    for v in range(N):
        rv = int(node_map[v])
        for w in nb_local[v]:
            rw = int(node_map[w])
            if rv != rw:
                adj_sets[rv].add(rw)
                adj_sets[rw].add(rv)
    nb_r = [sorted(s) for s in adj_sets]

    return nb_r, u_r, E_r, P_r, expand, node_map


# ---------- CP-SAT core: solve one ASU within a window ----------
def _bridge_edge_bounds(nb_local: List[List[int]], root_local: int) -> Dict[Tuple[int, int], int]:
    """
    Find bridges (cut edges) of the FIXED underlying graph via a DFS rooted at
    `root_local`, and for each bridge, the size of the side away from the root.
    This is directional and one-sided, tighter than a generic split-size bound:
    every unit of flow originates at the fixed root and terminates at a selected
    node, so a bridge's flow can ONLY go from the root's side to the far side --
    the far side has no supply of its own to send back the other way. Rooting the
    DFS at `root_local` guarantees that for every tree edge (p, u) found to be a
    bridge, u's DFS subtree never contains root_local (root_local is always an
    ancestor of u), so u's subtree IS exactly the far-from-root side. The reverse
    direction (u -> p) is therefore not just bounded but forced to exactly 0 --
    callers should treat any (u, v) pair absent from this dict, where (v, u) IS
    present, as a hard 0 bound rather than falling back to the uniform M bound.
    Non-bridge edges (on any cycle) get no entry -- flow could route around the
    cycle, so no valid per-edge bound below N-1 can be derived from local
    structure alone. Only root_local's connected component is processed; nodes
    unreachable from root can never be selected, so their edges don't matter.
    Returns {(near, far): far_side_node_count} for bridges on the root's side.
    """
    N = len(nb_local)
    disc = [-1] * N
    low = [0] * N
    subtree_size = [1] * N
    parent = [-1] * N
    skipped_parent = [False] * N
    bounds: Dict[Tuple[int, int], int] = {}
    timer = 0

    stack = [(root_local, iter(nb_local[root_local]))]
    disc[root_local] = low[root_local] = timer
    timer += 1
    while stack:
        u, it = stack[-1]
        recursed = False
        for w in it:
            if w == parent[u] and not skipped_parent[u]:
                skipped_parent[u] = True
                continue
            if disc[w] == -1:
                parent[w] = u
                disc[w] = low[w] = timer
                timer += 1
                stack.append((w, iter(nb_local[w])))
                recursed = True
                break
            else:
                low[u] = min(low[u], disc[w])
        if not recursed:
            stack.pop()
            if stack:
                p = stack[-1][0]
                low[p] = min(low[p], low[u])
                subtree_size[p] += subtree_size[u]
                if low[u] > disc[p]:
                    # (p, u) is a bridge on the root-rooted DFS tree -- u's
                    # subtree is exactly the far-from-root side, and flow can
                    # only travel p -> u (never the reverse).
                    bounds[(p, u)] = subtree_size[u]
    return bounds


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
    hint: Optional[List[int]] = None,
    hint_obj: Optional[int] = None,
    forced_selected: Optional[List[int]] = None,
    cluster_groups: Optional[List[List[int]]] = None,
    deterministic_ties: bool = True,
    tie_break_rank: Optional[List[int]] = None,
    objective_shaving: bool = False,
    use_root_articulation_implications: bool = False,
    use_signed_flow: bool = True,
    use_arborescence: bool = False,
    configure_subsolvers: bool = True,
    use_tract_first_search: bool = False,
    use_flow_count_envelope: bool = True,
    use_small_root_separators: bool = True,
    root_separator_max_size: int = 3,
    root_separator_clause_limit: int = 200,
    root_separator_target_limit: int = 128,
    use_separator_cardinality_bounds: bool = True,
    solution_pool_size: int = 32,
    use_bridge_edge_bounds: bool = False,
    max_nodes: Optional[int] = None,
    stop_flag_path: Optional[str] = None,
    skip_flag_path: Optional[str] = None,
) -> Optional[CpsatResult]:
    """
        Connectivity via iterative vertex-separator cuts. Each disconnected incumbent
        adds valid constraints requiring a selected path from its components to the root.
    Objective: maximize Σ u_i x_i. If the primary objective is proven optimal,
    remaining time is used to maximize exact threshold slack, minimize tract
    count, and finally minimize a stable GEOID/local-index rank sum.

    `max_nodes`, when given, hard-caps the number of *original-graph* tracts the
    solution may select (`sum(x) <= max_nodes`), counted before UR-cluster
    contraction so it reflects real tract counts. Optional; `None` (default)
    leaves ASU size unconstrained.

    `stop_flag_path`, when given, names a file whose mere existence is polled by
    a dedicated watchdog thread during the main solve; on detection it calls
    `solver.stop_search()` (a CP-SAT callback API) so the current incumbent is
    returned immediately instead of waiting for `time_limit` or optimality.

    `skip_flag_path` behaves identically for the current window's solve, except
    the file is consumed (deleted) once detected, so only this ASU's search is
    cut short -- callers such as `build_many_asus_cpsat` keep building further
    ASU windows afterward instead of halting entirely.
    """
    N = len(nb_local)
    if N == 0:
        return None

    # Contract UR>=tau clusters so the model has fewer variables; expand at every
    # return point. All cluster members co-occur in any feasible solution (mediant
    # inequality), so this is exact — not an approximation.
    nb_c, u_c, E_c, P_c, expand_c, node_map_c = contract_high_ur_nodes(nb_local, u_g, E_g, P_g, tau)
    nb_local_orig, u_g_orig, root_local_orig = nb_local, u_g, root_local
    nb_local, u_g, E_g, P_g = nb_c, u_c, E_c, P_c
    N = len(nb_local)
    root_local = int(node_map_c[root_local_orig])
    tract_first_enabled = (
        configure_subsolvers
        and use_tract_first_search
        and max(1, int(workers)) >= 8
    )
    if hint is not None:
        hint = sorted({int(node_map_c[v]) for v in hint})

    def _to_orig(sel: Optional[List[int]], status: str) -> "CpsatResult":
        orig = sorted({v for ri in sel for v in expand_c[ri]}) if sel else []
        return CpsatResult(orig, root_local_orig, int(u_g_orig[orig].sum()) if orig else 0, status)

    model = cp_model.CpModel()

    # Decision variables
    x = [model.NewBoolVar(f"x_{i}") for i in range(N)]

    # Selecting the root permits its entire connected high-UR component at no cost.
    forced_set = {int(node_map_c[v]) for v in (forced_selected or [])}
    forced_set.add(root_local)
    for i in forced_set:
        model.Add(x[i] == 1)

    # Valid for ANY connected subgraph: every selected non-root node must have at
    # least one selected neighbor (it can't be reached from root otherwise). Free
    # to add upfront (no separation needed) and tightens both the cut phase and the
    # flow phase since they share this same model.
    for v in range(N):
        if v == root_local:
            continue
        if nb_local[v]:
            model.Add(x[v] <= sum(x[w] for w in nb_local[v]))
        else:
            model.Add(x[v] == 0)

    root_implications = (
        _root_articulation_implications(nb_local, root_local)
        if use_root_articulation_implications or tract_first_enabled else []
    )
    if use_root_articulation_implications:
        for node, cut_vertex in root_implications:
            model.Add(x[node] <= x[cut_vertex])

    # q_i = den*u_i - num*E_i is the exact-integer form of UR-surplus
    # u_i - tau*(u_i+E_i); sum(q_i * x_i) >= 0 is exactly the UR constraint below.
    num, den = as_fraction_tau(tau)
    q_surplus = den * u_g.astype(np.int64) - num * E_g.astype(np.int64)

    separator_implications, separator_component_bounds = (
        _small_root_separator_implications(
            nb_local,
            root_local,
            u_g,
            q_surplus if use_separator_cardinality_bounds else None,
            max_size=root_separator_max_size,
            clause_limit=root_separator_clause_limit,
            target_limit=root_separator_target_limit,
        )
        if use_small_root_separators else ([], [])
    )
    for node, separator in separator_implications:
        model.AddBoolOr([x[node].Not()] + [x[cut_vertex] for cut_vertex in separator])
    for separator, affected, k_bound in separator_component_bounds:
        # z_c relaxes to min(1, sum(x_s)) in the LP, so the cardinality bound
        # tightens continuously as the separator's fractional selection grows.
        z_c = model.NewBoolVar(f"zsep_{'_'.join(map(str, separator))}")
        model.Add(z_c <= sum(x[s] for s in separator))
        model.Add(sum(x[i] for i in affected) <= k_bound * z_c)
    if log and use_small_root_separators:
        separator_count = len({separator for _, separator in separator_implications})
        print(
            f"  small root separators: {len(separator_implications)} clause(s) "
            f"from {separator_count} separator(s), "
            f"{len(separator_component_bounds)} cardinality cut(s)",
            flush=True,
        )

    # Population threshold
    pop_expr = sum(int(P_g[i]) * x[i] for i in range(N))
    model.Add(pop_expr >= int(pop_thresh))

    # Optional hard cap on total selected tracts, counted in original-graph units
    # (each contracted node may represent multiple original tracts).
    if max_nodes is not None:
        size_c = [len(grp) for grp in expand_c]
        model.Add(sum(size_c[i] * x[i] for i in range(N)) <= int(max_nodes))

    # UR >= tau as exact integer linear inequality
    lhs = sum(int(den) * int(u_g[i]) * x[i] for i in range(N)) \
        - sum(int(num) * int(E_g[i]) * x[i] for i in range(N))
    model.Add(lhs >= 0)

    # Objective: maximize unemployment captured
    obj_expr = sum(int(u_g[i]) * x[i] for i in range(N))
    model.Maximize(obj_expr)

    # Warm-start with the connected reverse-prune solution.
    if hint is not None:
        hint_set = set(hint)
        for i in range(N):
            model.AddHint(x[i], 1 if i in hint_set else 0)

    # Lower bound: reject solutions worse than the reverse-prune warm start.
    if hint_obj is not None and hint_obj > 0:
        model.Add(obj_expr >= hint_obj)

    # Solver params
    best_connected = sorted(hint) if hint and component_ok(
        hint, u_g, E_g, P_g, tau, pop_thresh, nb_local
    ) else None
    best_obj = int(u_g[best_connected].sum()) if best_connected else -1
    lower_bound = hint_obj if (hint_obj is not None and hint_obj > 0) else -1
    start_time = time.monotonic()
    cut_round = 0
    # NOTE: a tight objective/bound in the cut-only (disconnected) relaxation does
    # NOT imply cuts are close to finding a *connected* solution -- the "price of
    # connectivity" gap can be large and take many more rounds to close than the
    # relaxation's bound suggests (measured: 15 rounds over ~30s shrank components
    # from 18->8 without ever reaching a single connected component). The exact
    # flow-based phase is what actually *guarantees* progress toward a connected
    # answer, so it must keep the majority of the time budget; cuts are only a
    # cheap pre-pass to prune obviously-disconnected structure.
    #
    # NOTE: tried removing this cut pre-pass entirely (going straight to the
    # exact flow model) and A/B tested it on real Colorado data -- it was a
    # clear regression (0.35%->0.79% gap, 75,357->75,204 unemp @300s) with no
    # wall-time savings (still used the full 300s budget either way). The
    # boundary constraints these cuts add evidently still prune the flow
    # phase's search space usefully even when they never converge to a single
    # connected component. See SKILL.md.
    cut_time_budget = 0.0  # cut pre-pass disabled
    stall_rounds = 0
    prev_num_components: Optional[int] = None
    first_components: Optional[int] = None

    while cut_round < 15:
        elapsed = time.monotonic() - start_time
        remaining_for_cuts = cut_time_budget - elapsed
        if remaining_for_cuts <= 0:
            break

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = max(1, int(workers))
        solver.parameters.max_time_in_seconds = remaining_for_cuts
        solver.parameters.log_search_progress = False  # silent; summary logged after loop
        solver.parameters.cp_model_presolve = True
        solver.parameters.linearization_level = 2

        status = solver.Solve(model)
        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            break

        selected = [i for i in range(N) if solver.BooleanValue(x[i])]
        selected_set = set(selected)
        root_component = {root_local}
        stack = [root_local]
        while stack:
            v = stack.pop()
            for w in nb_local[v]:
                if w in selected_set and w not in root_component:
                    root_component.add(w)
                    stack.append(w)

        if len(root_component) == len(selected):
            objective = int(round(solver.ObjectiveValue()))
            if objective > best_obj:
                best_connected, best_obj = selected, objective
                if best_obj > lower_bound:
                    model.Add(obj_expr >= best_obj)
                    lower_bound = best_obj
            if status == cp_model.OPTIMAL and not deterministic_ties:
                return _to_orig(selected, "OPTIMAL")
            break

        unseen = selected_set - root_component
        components: List[set] = []
        while unseen:
            seed = unseen.pop()
            component = {seed}
            stack = [seed]
            while stack:
                v = stack.pop()
                for w in nb_local[v]:
                    if w in unseen:
                        unseen.remove(w)
                        component.add(w)
                        stack.append(w)
            components.append(component)

        if first_components is None:
            first_components = len(components)
        if prev_num_components is not None and len(components) >= prev_num_components:
            stall_rounds += 1
        else:
            stall_rounds = 0
        prev_num_components = len(components)

        for component in components:
            boundary = sorted({
                w for v in component for w in nb_local[v]
                if w not in component
            })
            if boundary:
                # BoolOr lives in the SAT core (unit propagation) and is also
                # auto-linearized into the LP at linearization_level=2.
                for v in component:
                    model.AddBoolOr([x[v].Not()] + [x[w] for w in boundary])
            else:
                for v in component:
                    model.Add(x[v] == 0)
        cut_round += 1
        if stall_rounds >= 3:
            break

    # Finish with exact connectivity, strengthened by the cuts.
    flow_source = best_connected if best_connected is not None else hint
    flow_hints = (
        _spanning_tree_flows(flow_source, nb_local, root_local)
        if flow_source is not None else {}
    )

    if use_arborescence:
        # Boolean arborescence formulation.
        # par_vars[(i,j)] = 1  ↔  j is the parent of i in the rooted spanning tree.
        # For each selected non-root node exactly one parent is assigned; acyclicity
        # is enforced by strictly increasing depth along parent edges (big-M
        # linearisation).  Advantages vs. integer flow:
        #   - ~8 k BoolVars replace ~4 k large-domain IntVars → CP-SAT can apply
        #     clause learning and unit propagation far more aggressively.
        #   - Only N depth IntVars (domain [0, N-1]) replace 4 k flow IntVars with
        #     the same domain, cutting total integer domain size ~6×.
        par_vars = {}   # (i, j) -> BoolVar: j is the parent of i
        for i, nb_i in enumerate(nb_local):
            for j in nb_i:
                if i != j:
                    par_vars[(i, j)] = model.NewBoolVar(f"par_{i}_{j}")

        depth_vars = [model.NewIntVar(0, N - 1, f"d_{i}") for i in range(N)]
        model.Add(depth_vars[root_local] == 0)

        for i in range(N):
            if i == root_local:
                continue
            parent_choices = [par_vars[(i, j)] for j in nb_local[i]]
            # exactly one parent when selected, zero when unselected
            model.Add(sum(parent_choices) == x[i])
            for j in nb_local[i]:
                model.Add(par_vars[(i, j)] <= x[j])   # parent must be selected
                # Acyclicity: if j is parent of i then depth[i] > depth[j]
                # depth[i] >= depth[j] + 1 - (N-1)*(1 - par_vars[(i,j)])
                model.Add(
                    depth_vars[i] - depth_vars[j]
                    >= 1 - (N - 1) * (1 - par_vars[(i, j)])
                )

        # Warm-start: derive parent assignments and depths from spanning tree.
        # flow_hints has {(p, v): subtree_size} where p is the parent of v.
        tree_parent = {v: p for (p, v) in flow_hints}   # child -> parent
        ch_map = {v: [] for v in range(N)}
        for v, p in tree_parent.items():
            if 0 <= p < N:
                ch_map[p].append(v)
        depth_hint = {root_local: 0}
        bfs_q = [root_local]
        while bfs_q:
            node = bfs_q.pop(0)
            for child in ch_map[node]:
                if child not in depth_hint:
                    depth_hint[child] = depth_hint[node] + 1
                    bfs_q.append(child)
        for (i, j), pvar in par_vars.items():
            model.AddHint(pvar, 1 if tree_parent.get(i) == j else 0)
        for i in range(N):
            model.AddHint(depth_vars[i], depth_hint.get(i, 0))

        if log:
            print(
                f"  flow formulation: arborescence "
                f"({len(par_vars)} parent vars + {N} depth vars)",
                flush=True,
            )
    else:
        # Single-commodity integer flow connectivity
        if use_signed_flow:
            edges = sorted({
                (min(i, j), max(i, j))
                for i, neighbors in enumerate(nb_local) for j in neighbors if i != j
            })
        else:
            edges = list(dict.fromkeys(
                (i, j) for i, neighbors in enumerate(nb_local) for j in neighbors if i != j
            ))

        # NOTE: a per-edge bound derived from a single fixed reference spanning tree
        # (e.g. subtree size) is UNSOUND on graphs with cycles -- the actual flow can
        # legitimately need to route around a different topology than any one fixed
        # tree, and a fixed-tree bound can wrongly reject genuinely feasible connected
        # selections. Verified empirically: a 5-cycle counterexample where excluding
        # one low-value node forces routing 3 units through what a BFS tree treats as
        # a capacity-2 edge. The uniform bound below is the correct, universally valid
        # one (flow on any edge can never exceed total selected nodes - 1).
        # Tighten M via a fast connectivity-free solve: max tracts s.t. UR/pop only.
        _mM = cp_model.CpModel()
        _mx = [_mM.NewBoolVar(f"mx_{i}") for i in range(N)]
        for _fi in forced_set:
            _mM.Add(_mx[_fi] == 1)
        _mM.Add(sum(int(P_g[i]) * _mx[i] for i in range(N)) >= int(pop_thresh))
        _mM.Add(
            sum(int(den) * int(u_g[i]) * _mx[i] for i in range(N))
            - sum(int(num) * int(E_g[i]) * _mx[i] for i in range(N))
            >= 0
        )
        _mM.Maximize(sum(_mx))
        _ms = cp_model.CpSolver()
        _ms.parameters.num_search_workers = max(1, int(workers))
        _ms.parameters.max_time_in_seconds = 10.0
        _ms.parameters.cp_model_presolve = True
        _ms.parameters.linearization_level = 2
        _ms.parameters.log_search_progress = False
        _ms_status = _ms.Solve(_mM)
        if _ms_status == cp_model.OPTIMAL:
            max_selected = max(1, int(round(_ms.ObjectiveValue())))
        else:
            max_selected = N
        M = max(1, max_selected - 1)
        # NOTE: tried adding a connectivity-free relaxed-objective bound here (same
        # _mM model, maximize sum(u*x) instead of count, add as obj_expr <= ub cut)
        # -- valid (dropping connectivity only enlarges the feasible region) but
        # A/B tested on real data and it never beat the bound the main solver's own
        # search already converges to, so it only cost ~10s for no benefit. Reverted.
        # NOTE: an earlier, non-root-aware version of _bridge_edge_bounds() (symmetric
        # split-size bound, both directions bounded) was A/B tested on real Colorado
        # data and was a clear regression (0.52%->2.62% gap, 75,214->74,160 unemp
        # @300s) -- see SKILL.md. The current _bridge_edge_bounds() is a materially
        # different, strictly tighter formulation (root-rooted DFS, one direction
        # forced to exactly 0 rather than just bounded) gated behind
        # use_bridge_edge_bounds -- treat this as unproven until A/B tested again.
        bridge_bounds = _bridge_edge_bounds(nb_local, root_local) if use_bridge_edge_bounds else {}
        edge_bounds = [M] * len(edges)

        selected_count = model.NewIntVar(
            len(forced_set), max_selected, "selected_count"
        )
        model.Add(selected_count == sum(x))
        if flow_source is not None:
            model.AddHint(selected_count, len(flow_source))

        if use_signed_flow:
            f = []
            for idx, (i, j) in enumerate(edges):
                far_bound = bridge_bounds.get((i, j))
                rev_bound = bridge_bounds.get((j, i))
                if far_bound is not None:
                    lo, hi = 0, min(far_bound, M)
                elif rev_bound is not None:
                    lo, hi = -min(rev_bound, M), 0
                else:
                    lo, hi = -edge_bounds[idx], edge_bounds[idx]
                f.append(model.NewIntVar(lo, hi, f"f_{i}_{j}"))
            net_out_for = [[] for _ in range(N)]
            for edge_index, (i, j) in enumerate(edges):
                model.Add(f[edge_index] == 0).OnlyEnforceIf(x[i].Not())
                model.Add(f[edge_index] == 0).OnlyEnforceIf(x[j].Not())
                if use_flow_count_envelope:
                    model.Add(f[edge_index] <= selected_count - 1)
                    model.Add(f[edge_index] >= 1 - selected_count)

                net_out_for[i].append(f[edge_index])
                net_out_for[j].append(-f[edge_index])

                model.AddHint(
                    f[edge_index],
                    flow_hints.get((i, j), 0) - flow_hints.get((j, i), 0),
                )
            for i in range(N):
                net_outflow = sum(net_out_for[i]) if net_out_for[i] else 0
                model.Add(net_outflow == (selected_count - 1 if i == root_local else -x[i]))
        else:
            directed_bounds = []
            for idx, (i, j) in enumerate(edges):
                if (i, j) in bridge_bounds:
                    directed_bounds.append(min(bridge_bounds[(i, j)], edge_bounds[idx]))
                elif (j, i) in bridge_bounds:
                    directed_bounds.append(0)
                else:
                    directed_bounds.append(edge_bounds[idx])
            f = [model.NewIntVar(0, directed_bounds[idx], f"f_{i}_{j}") for idx, (i, j) in enumerate(edges)]
            in_edges_for = [[] for _ in range(N)]
            out_edges_for = [[] for _ in range(N)]
            for edge_index, (i, j) in enumerate(edges):
                out_edges_for[i].append(edge_index)
                in_edges_for[j].append(edge_index)
                model.Add(f[edge_index] <= directed_bounds[edge_index] * x[i])
                model.Add(f[edge_index] <= directed_bounds[edge_index] * x[j])
                model.AddHint(f[edge_index], flow_hints.get((i, j), 0))
            for i in range(N):
                inflow = sum(f[e] for e in in_edges_for[i]) if in_edges_for[i] else 0
                outflow = sum(f[e] for e in out_edges_for[i]) if out_edges_for[i] else 0
                if i == root_local:
                    model.Add(outflow - inflow == selected_count - 1)
                else:
                    model.Add(inflow - outflow == x[i])

        if log:
            print(f"  flow formulation: {'signed' if use_signed_flow else 'directed'} "
                  f"({len(edges)} edge variables)", flush=True)
            if use_signed_flow and use_flow_count_envelope:
                print(
                    f"  flow count envelope: selected <= {max_selected}, "
                    "|flow| <= selected - 1",
                    flush=True,
                )
            if use_bridge_edge_bounds:
                print(
                    f"  bridge edge bounds: {len(bridge_bounds)} directed bridge(s) "
                    "tightened (reverse direction forced to 0)",
                    flush=True,
                )

    remaining_time = float(time_limit) - (time.monotonic() - start_time)
    if log and cut_round > 0:
        _fc = first_components or "?"
        _lc = prev_num_components if prev_num_components is not None else "?"
        print(f"  cut phase: {cut_round} round(s), components {_fc}->{_lc}, "
              f"{time_limit - remaining_time:.1f}s used; {remaining_time:.1f}s for flow phase", flush=True)
    if remaining_time <= 0:
        return _to_orig(best_connected, "FEASIBLE") if best_connected else None

    def _seed_solution_hints(target_model: cp_model.CpModel, selection: Sequence[int]) -> None:
        """Refresh variable hints from a connected incumbent selection."""
        sel_set = {int(i) for i in selection}
        target_model.ClearHints()
        for i in range(N):
            target_model.AddHint(x[i], 1 if i in sel_set else 0)

        if not use_arborescence:
            sf = _spanning_tree_flows(sorted(sel_set), nb_local, root_local)
            target_model.AddHint(selected_count, len(sel_set))
            if use_signed_flow:
                for edge_index, (edge_u, edge_v) in enumerate(edges):
                    target_model.AddHint(
                        f[edge_index],
                        sf.get((edge_u, edge_v), 0) - sf.get((edge_v, edge_u), 0),
                    )
            else:
                for edge_index, (edge_u, edge_v) in enumerate(edges):
                    target_model.AddHint(f[edge_index], sf.get((edge_u, edge_v), 0))
            return

        sf_arb = _spanning_tree_flows(sorted(sel_set), nb_local, root_local)
        tree_parent = {v: p for (p, v) in sf_arb}
        children: Dict[int, List[int]] = {v: [] for v in range(N)}
        for v, p in tree_parent.items():
            if 0 <= p < N:
                children[p].append(v)
        depth_hint: Dict[int, int] = {root_local: 0}
        bfs_q = [root_local]
        while bfs_q:
            node = bfs_q.pop(0)
            for child in children[node]:
                if child not in depth_hint:
                    depth_hint[child] = depth_hint[node] + 1
                    bfs_q.append(child)
        for (i, j), pvar in par_vars.items():
            target_model.AddHint(pvar, 1 if tree_parent.get(i) == j else 0)
        for i in range(N):
            target_model.AddHint(depth_vars[i], depth_hint.get(i, 0))

    # Scout: 10 s LNS-only pass on the full flow model to lift the warm-start
    # incumbent before the lbts-heavy main solve. lbts proves bounds but is slow
    # to improve the primal; the LNS subsolvers do the opposite -- suppress lbts
    # here so all 18 workers focus on finding better connected incumbents fast.
    _SCOUT_SECS = 10.0
    if (
        remaining_time > _SCOUT_SECS + 30.0
        and not _stop_requested(stop_flag_path)
        and not _stop_requested(skip_flag_path)
    ):
        _scout = cp_model.CpSolver()
        _scout.parameters.num_search_workers = max(1, int(workers))
        _scout.parameters.max_time_in_seconds = _SCOUT_SECS
        _scout.parameters.log_search_progress = False
        _scout.parameters.cp_model_presolve = True
        _scout.parameters.linearization_level = 2
        _scout.parameters.cp_model_probing_level = 2
        _scout.parameters.cut_level = 1
        if configure_subsolvers:
            _scout.parameters.ignore_subsolvers.extend([
                "lb_tree_search",
                "probing",
                "objective_shaving_max_lp", "objective_shaving_no_lp",
                "objective_lb_search_max_lp",
                "feasibility_pump",
            ])
        _scout_status = _scout.Solve(model)
        if _scout_status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            _scout_sel = [i for i in range(N) if _scout.BooleanValue(x[i])]
            _scout_obj = int(u_g[_scout_sel].sum())
            if _scout_obj > best_obj:
                best_connected, best_obj = _scout_sel, _scout_obj
                model.Add(obj_expr >= _scout_obj)
                lower_bound = _scout_obj
                _seed_solution_hints(model, _scout_sel)
                if log:
                    print(f"  scout: improved incumbent to {_scout_obj} "
                          f"(+{_scout_obj - (hint_obj or 0)} vs hint)", flush=True)

    status = cp_model.UNKNOWN
    status_name = "UNKNOWN"
    selected: List[int] = []
    objective = -1

    if objective_shaving and best_connected is not None and rel_gap is None:
        proof_model = model.clone()
        proof_model.ClearObjective()
        proof_model.ClearHints()
        proof_iterations = 0

        while True:
            proof_remaining = float(time_limit) - (time.monotonic() - start_time)
            if proof_remaining <= 0.01:
                break
            target = best_obj + 1
            proof_model.Add(obj_expr >= target)
            if log:
                print(f"  objective shaving: testing objective >= {target} "
                      f"with {proof_remaining:.1f}s remaining", flush=True)

            proof_solver = cp_model.CpSolver()
            proof_solver.parameters.num_search_workers = max(1, int(workers))
            proof_solver.parameters.max_time_in_seconds = proof_remaining
            proof_solver.parameters.log_search_progress = bool(log)
            proof_solver.parameters.cp_model_presolve = True
            proof_solver.parameters.linearization_level = 2
            proof_iterations += 1
            proof_status = proof_solver.Solve(proof_model)

            if proof_status == cp_model.INFEASIBLE:
                status = cp_model.OPTIMAL
                status_name = "OPTIMAL"
                selected = best_connected
                objective = best_obj
                if log:
                    print(f"  objective shaving: proved optimal at {best_obj} "
                          f"after {proof_iterations} feasibility test(s)", flush=True)
                break
            if proof_status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                if log:
                    print(f"  objective shaving: no proof after {proof_iterations} "
                          f"test(s); retaining incumbent {best_obj}", flush=True)
                break

            candidate = [i for i in range(N) if proof_solver.BooleanValue(x[i])]
            candidate_obj = int(u_g[candidate].sum())
            if candidate_obj <= best_obj:
                break
            best_connected, best_obj = candidate, candidate_obj
            if log:
                print(f"  objective shaving: improved incumbent to {best_obj}; "
                      f"testing {best_obj + 1}", flush=True)

    if status != cp_model.OPTIMAL:
        remaining_time = float(time_limit) - (time.monotonic() - start_time)
        if remaining_time <= 0.01:
            return _to_orig(best_connected, "FEASIBLE") if best_connected else None

        if tract_first_enabled:
            branch_order, _ = _asu_branch_order(
                nb_local=nb_local,
                u_g=u_g,
                E_g=E_g,
                root_local=root_local,
                num=num,
                den=den,
                hint=best_connected if best_connected is not None else hint,
                root_implications=root_implications,
            )
            branch_hint = best_connected if best_connected is not None else hint
            protected_branch_nodes = forced_set | {
                cut_vertex for _, cut_vertex in root_implications
            }
            branch_additions, branch_removals = _asu_branch_challenges(
                nb_local=nb_local,
                branch_order=branch_order,
                hint=branch_hint,
                protected=protected_branch_nodes,
            )
            if branch_removals:
                model.AddDecisionStrategy(
                    [x[i] for i in branch_removals],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MIN_VALUE,
                )
            if branch_additions:
                model.AddDecisionStrategy(
                    [x[i] for i in branch_additions],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MAX_VALUE,
                )
            challenged_tracts = set(branch_removals) | set(branch_additions)
            remaining_tracts = [
                int(i) for i in branch_order
                if i not in challenged_tracts
            ]
            if remaining_tracts:
                model.AddDecisionStrategy(
                    [x[i] for i in remaining_tracts],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MAX_VALUE,
                )
            if log:
                print(
                    f"  tract-first worker: drop {len(branch_removals)} boundary, "
                    f"add {len(branch_additions)} frontier, then branch on "
                    f"{len(remaining_tracts)} remaining tract variables before flow",
                    flush=True,
                )

        tract_first_probing_enabled = (
            tract_first_enabled
            and _supports_tract_first_probing(cp_model.CpSolver().parameters)
        )
        if tract_first_enabled and log:
            if tract_first_probing_enabled:
                print(
                    "  tract-first probing: one fast and one standard worker "
                    "probe tract Booleans before flow bounds",
                    flush=True,
                )
            else:
                print(
                    "  tract-first probing: unavailable in this OR-Tools build; "
                    "retaining integer-first probe workers",
                    flush=True,
                )

        def configure_asu_subsolvers(params, workers):
            workers = max(1, int(workers))

            if workers < 8:
                return

            # Prevent old/default custom subsolvers from being inserted
            # ahead of our explicitly ordered ASU portfolio.
            params.extra_subsolvers.clear()
            params.subsolvers.clear()
            params.filter_subsolvers.clear()

            if tract_first_enabled:
                _append_asu_subsolver_params(
                    params,
                    "asu_tract_first",
                    search_branching=cp_model.PARTIAL_FIXED_SEARCH,
                    linearization_level=2,
                )

            # Warm-starts each node's LP from the nearest ancestor's saved
            # basis instead of resolving from scratch, and (as a side effect)
            # activates lb_tree_search's pseudo-cost branching heuristic.
            # Marked "Experimental" upstream -- needs A/B validation.
            _append_asu_subsolver_params(
                params,
                "lb_tree_search",
                save_lp_basis_in_lb_tree_search=False,
                max_cut_rounds_at_level_zero=4,
                add_objective_cut=True,
                root_lp_iterations = 50_000
            )

            # Dedicated worker that only tries to shave the upper bound
            # (our objective is maximized, so "upper bound" == dual/proof
            # bound) instead of searching for better incumbents. Registered
            # as its own named entry so the override merges onto a fresh
            # SatParameters and never leaks into the other portfolio members.
            # _append_asu_subsolver_params(
            #     params,
            #     "asu_upper_bound",
            #     use_objective_shaving_search=True,
            #     cp_model_probing_level=0,
            #     symmetry_level=0,
            #     linearization_level=2,
            #     add_lp_constraints_lazily=False,
            # )

            # Ordered so that every prefix is useful.
            #
            # ASU roles observed in logs:
            #
            #   asu_probe_fast / asu_probe_standard / asu_probe_deep
            #       Controlled root-LP-effort variants of the primary proof worker.
            #
            #   lb_tree_search
            #       Useful for larger early bound jumps.
            #
            #   variables_shaving
            #       Strong early domain/bound reduction on large states.
            #
            #   max_lp
            #       Full LP search and sharing worker.
            #
            #   quick_restart_no_lp
            #       SAT diversification / clause and bound sharing.
            #       Keep one copy, but don't spend multiple workers on it.
            #
            #   probing
            #       Lower priority; retain one copy only at large budgets.
            #
            full_subsolvers = _asu_full_subsolvers(
                workers,
                use_tract_first_search=tract_first_enabled,
                use_tract_first_probing=tract_first_probing_enabled,
            )

            params.subsolvers.extend(full_subsolvers)
            params.num_full_subsolvers = len(full_subsolvers)

            # NOTE: tried dedicating shared_tree_num_workers=max(2, workers//4)
            # workers to shared tree search (+ "shared_tree" in filter_subsolvers)
            # to target proof speed. A/B tested on real data (seed=1173, matched
            # 600s budget): objective -100 vs baseline (75322 vs 75422) and worse
            # gap_integral (11295 vs 9486) -- the dedicated workers barely shared
            # any bounds (3 vs 47/23/53 for asu_probe_fast/standard/lb_tree_search)
            # while permanently occupying 3 of 14 threads. Reverted.

            allowed = list(dict.fromkeys(
                full_subsolvers + [
                    "rins*",
                    "lb_relax_lns",

                    "graph_arc_lns",
                    "graph_var_lns",
                    "graph_cst_lns",

                    # Diversification / basin escape
                    "rnd_var_lns",
                    "variables_shaving_max_lp",

                    "ls*",
                ]
            ))

            params.filter_subsolvers.extend(allowed)

        def _configure_main_solver_params(
            params,
            max_seconds: float,
            *,
            search_log: bool,
            collect_tightened_domains: bool = False,
        ) -> None:
            params.num_search_workers = max(1, int(workers))
            params.max_time_in_seconds = max(0.01, float(max_seconds))
            params.log_search_progress = bool(search_log)
            params.cp_model_presolve = True
            params.linearization_level = 2
            params.cp_model_probing_level = 2
            params.cut_level = 2
            if hasattr(params, "fill_tightened_domains_in_response"):
                params.fill_tightened_domains_in_response = bool(
                    collect_tightened_domains
                )
            if hasattr(params, "keep_all_feasible_solutions_in_presolve"):
                params.keep_all_feasible_solutions_in_presolve = bool(
                    collect_tightened_domains
                )

            # LNS settings
            params.lns_initial_difficulty = 0.7
            params.lns_initial_deterministic_limit = 0.4
            params.solution_pool_size = max(1, int(solution_pool_size))
            params.diversify_lns_params = True

            params.add_objective_cut = True
            params.variables_shaving_level = 3
            _configure_asu_lp_search_params(params)
            _configure_asu_probe_variants(
                params,
                tract_first=tract_first_enabled,
            )
            _configure_asu_pseudo_costs(params)
            _configure_asu_shared_tree(
                params,
                tract_first=tract_first_enabled,
            )
            if configure_subsolvers:
                configure_asu_subsolvers(params, workers)
            if rel_gap is not None:
                params.relative_gap_limit = float(rel_gap)

        if log:
            print(
                "ASU probing experiment:\n"
                "  asu_probe_fast     root_lp_iterations=50000\n"
                "  asu_probe_standard root_lp_iterations=100000",
                flush=True,
            )
        # stall_window_seconds has been disabled on purpose
        # Currently every time the solver restarts, we end up losing progress
        # We lose clauses, learned constraints, and any other progress made in the previous window.
        # We were testing the effect of the stall window, but right now it seems to cause more harm than good.
        stall_window_seconds = 30000.0  # Time window to detect solver stalling in seconds.
        proof_feasibility_cap_seconds = 600.0
        proof_mid_gap_trigger = 5
        max_stall_restart_no_progress = 2  # consecutive no-progress cycles before giving up
        stall_restart_no_progress_cycles = 0
        best_bound_seen = math.inf
        bound_cap_applied: Optional[int] = None
        if best_obj >= 0:
            model.Add(obj_expr >= best_obj)
            lower_bound = max(lower_bound, best_obj)

        def _carry_objective_bound(bound_value: float) -> None:
            """Persist the best proven objective upper bound across real restarts."""
            nonlocal best_bound_seen, bound_cap_applied
            if not math.isfinite(bound_value):
                return
            if bound_value >= best_bound_seen - 1e-9:
                return

            best_bound_seen = bound_value
            cap = max(best_obj, int(math.floor(best_bound_seen)))
            if bound_cap_applied is None or cap < bound_cap_applied:
                model.Add(obj_expr <= cap)
                bound_cap_applied = cap

        proof_fix_min_target: Optional[int] = None
        proof_x_fixes: Dict[int, int] = {}

        def _extract_tightened_x_fixes(response_proto) -> Dict[int, int]:
            """Extract fixed Boolean tract domains from a solver response."""
            tightened_variables = getattr(response_proto, "tightened_variables", None)
            if not tightened_variables:
                return {}

            fixes: Dict[int, int] = {}
            for i, var in enumerate(x):
                var_index = var.Index()
                if var_index < 0 or var_index >= len(tightened_variables):
                    continue
                domain = list(tightened_variables[var_index].domain)
                if domain == [0, 0]:
                    fixes[i] = 0
                elif domain == [1, 1]:
                    fixes[i] = 1
            return fixes

        def _update_cached_proof_fixes(
            proof_target: int,
            fixes: Dict[int, int],
        ) -> int:
            """Cache tract fixes only for proof targets at least this restrictive."""
            nonlocal proof_fix_min_target, proof_x_fixes
            if not fixes:
                return 0

            if proof_fix_min_target is None:
                proof_fix_min_target = proof_target
                proof_x_fixes = dict(fixes)
                return len(proof_x_fixes)

            if proof_target < proof_fix_min_target:
                proof_fix_min_target = proof_target
                proof_x_fixes = dict(fixes)
                return len(proof_x_fixes)

            if proof_target > proof_fix_min_target:
                proof_fix_min_target = proof_target

            added = 0
            for idx, value in fixes.items():
                prior = proof_x_fixes.get(idx)
                if prior is None:
                    proof_x_fixes[idx] = value
                    added += 1
                elif prior != value:
                    # Defensive: drop contradictory fix if domains disagree.
                    proof_x_fixes.pop(idx, None)
            return added

        while True:
            remaining_time = float(time_limit) - (time.monotonic() - start_time)
            if remaining_time <= 0.01:
                break
            if _stop_requested(stop_flag_path):
                if log:
                    print("  [stop] Stop flag detected before solve cycle; halting with current incumbent.", flush=True)
                break
            if _stop_requested(skip_flag_path):
                _consume_flag(skip_flag_path)
                if log:
                    print("  [skip] Skip-current-ASU flag detected before solve cycle; halting this window.", flush=True)
                break

            cycle_start_obj = best_obj
            cycle_start_bound = best_bound_seen

            solver = cp_model.CpSolver()
            _configure_main_solver_params(
                solver.parameters,
                remaining_time,
                search_log=log,
            )

            progress_lock = threading.Lock()
            solve_done = threading.Event()
            stalled = threading.Event()
            stopped = threading.Event()
            stop_kind: Optional[str] = None
            watchdog_start = time.monotonic()
            progress = {
                "last_incumbent_time": watchdog_start,
                "last_bound_time": watchdog_start,
                "best_obj": best_obj,
                "best_selection": best_connected,
                "best_bound": best_bound_seen,
            }

            class _MainSolveCallback(cp_model.CpSolverSolutionCallback):
                def on_solution_callback(self) -> None:
                    candidate_obj = int(round(self.ObjectiveValue()))
                    with progress_lock:
                        if candidate_obj <= progress["best_obj"]:
                            return

                    candidate = [i for i in range(N) if self.BooleanValue(x[i])]
                    with progress_lock:
                        if candidate_obj > progress["best_obj"]:
                            progress["best_obj"] = candidate_obj
                            progress["best_selection"] = candidate
                            progress["last_incumbent_time"] = time.monotonic()

            def _on_best_bound(bound: float) -> None:
                with progress_lock:
                    if bound < progress["best_bound"] - 1e-9:
                        progress["best_bound"] = float(bound)
                        progress["last_bound_time"] = time.monotonic()

            def _stall_watchdog() -> None:
                # Triggers on incumbent stagnation alone: a bound that keeps
                # trickling down must not mask a primal search that is stuck,
                # since a stuck incumbent is exactly what the feasibility
                # probe below is meant to shake loose.
                while not solve_done.is_set():
                    with progress_lock:
                        idle_seconds = time.monotonic() - progress["last_incumbent_time"]
                    wait_seconds = max(0.01, stall_window_seconds - idle_seconds)
                    if solve_done.wait(wait_seconds):
                        return
                    with progress_lock:
                        idle_seconds = time.monotonic() - progress["last_incumbent_time"]
                    if idle_seconds >= stall_window_seconds:
                        stalled.set()
                        solver.stop_search()
                        return

            def _stop_watchdog() -> None:
                # Polls external stop-signal files (e.g. from dashboard "Stop
                # Solve"/"Skip to Next ASU" buttons) and calls the CP-SAT
                # `stop_search()` callback API as soon as one appears, so the
                # current incumbent is returned promptly instead of running
                # to `time_limit`. The skip flag is consumed (deleted) so it
                # only affects this window's solve.
                nonlocal stop_kind
                while not solve_done.wait(0.5):
                    if _stop_requested(stop_flag_path):
                        stop_kind = "stop"
                        stopped.set()
                        solver.stop_search()
                        return
                    if _stop_requested(skip_flag_path):
                        stop_kind = "skip"
                        _consume_flag(skip_flag_path)
                        stopped.set()
                        solver.stop_search()
                        return

            callback = _MainSolveCallback()
            solver.best_bound_callback = _on_best_bound
            watchdog = threading.Thread(target=_stall_watchdog, daemon=True)
            watchdog.start()
            stop_watchdog: Optional[threading.Thread] = None
            if stop_flag_path or skip_flag_path:
                stop_watchdog = threading.Thread(target=_stop_watchdog, daemon=True)
                stop_watchdog.start()
            try:
                main_status = solver.Solve(model, callback)
            finally:
                solve_done.set()
                watchdog.join()
                if stop_watchdog is not None:
                    stop_watchdog.join()

            status = main_status
            status_name = solver.StatusName(main_status)
            with progress_lock:
                callback_obj = int(progress["best_obj"])
                callback_selection = progress["best_selection"]
                callback_bound = float(progress["best_bound"])

            if callback_selection is not None and callback_obj > best_obj:
                best_connected = list(callback_selection)
                best_obj = callback_obj
                model.Add(obj_expr >= best_obj)
                lower_bound = max(lower_bound, best_obj)
                _seed_solution_hints(model, best_connected)
            _carry_objective_bound(callback_bound)
            _carry_objective_bound(solver.BestObjectiveBound())

            if main_status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                selected = [i for i in range(N) if solver.BooleanValue(x[i])]
                objective = int(u_g[selected].sum())
                if objective > best_obj:
                    best_connected = selected
                    best_obj = objective
            
            if stopped.is_set():
                if best_connected is not None and best_obj >= 0:
                    status = cp_model.FEASIBLE
                    status_name = "SKIPPED_FEASIBLE" if stop_kind == "skip" else "STOPPED_FEASIBLE"
                    selected = list(best_connected)
                    objective = best_obj
                if log:
                    tag = stop_kind or "stop"
                    label = "Skip-current-ASU" if tag == "skip" else "Stop"
                    print(
                        f"  [{tag}] {label} signal received; returning "
                        f"current incumbent {best_obj if best_obj >= 0 else 'none'}.",
                        flush=True,
                    )
                break

            if main_status == cp_model.OPTIMAL:
                break
            if not stalled.is_set():
                break
            if best_connected is None or best_obj < 0:
                break

            proof_remaining = float(time_limit) - (time.monotonic() - start_time)
            if proof_remaining <= 0.01:
                break
            proof_seconds = min(proof_remaining, proof_feasibility_cap_seconds)

            best_upper: Optional[int] = None
            gap_to_bound: Optional[int] = None
            if math.isfinite(best_bound_seen):
                best_upper = int(math.floor(best_bound_seen + 1e-6))
                gap_to_bound = max(0, best_upper - best_obj)

            if best_upper is not None and best_upper <= best_obj:
                status = cp_model.OPTIMAL
                status_name = "OPTIMAL"
                selected = list(best_connected)
                objective = best_obj
                break

            if gap_to_bound is not None and gap_to_bound > proof_mid_gap_trigger:
                proof_target = best_obj + max(
                    1,
                    int(math.ceil(gap_to_bound / 2.0)),
                )
            else:
                proof_target = best_obj + 1

            if (
                proof_fix_min_target is not None
                and proof_target < proof_fix_min_target
            ):
                proof_fix_min_target = None
                proof_x_fixes.clear()

            proof_model = model.clone()
            proof_model.ClearObjective()
            proof_model.ClearHints()
            proof_model.Add(obj_expr >= proof_target)
            cached_fix_count = 0
            if (
                proof_fix_min_target is not None
                and proof_target >= proof_fix_min_target
            ):
                for idx, value in sorted(proof_x_fixes.items()):
                    proof_model.Add(x[idx] == value)
                    cached_fix_count += 1
            probe_upper_ceiling: Optional[int] = None
            if best_upper is not None:
                probe_upper_ceiling = best_upper
                proof_model.Add(obj_expr <= probe_upper_ceiling)

            if log:
                upper_text = str(best_upper) if best_upper is not None else "unknown"
                gap_text = str(gap_to_bound) if gap_to_bound is not None else "unknown"
                ceiling_text = (
                    str(probe_upper_ceiling)
                    if probe_upper_ceiling is not None
                    else "none"
                )
                print(
                    f"  stall watchdog: stopped main solve after "
                    f"{stall_window_seconds:.0f}s without incumbent movement; "
                    f"testing feasibility at objective >= {proof_target} "
                    f"(incumbent {best_obj}, upper {upper_text}, gap {gap_text}) "
                    f"with probe ceiling {ceiling_text} and "
                    f"{cached_fix_count} cached x-fix(es) "
                    f"for up to {proof_seconds:.1f}s",
                    flush=True,
                )

            proof_solver = cp_model.CpSolver()
            _configure_main_solver_params(
                proof_solver.parameters,
                proof_seconds,
                search_log=log,
                collect_tightened_domains=True,
            )
            proof_status = proof_solver.Solve(proof_model)
            if proof_status in (cp_model.OPTIMAL, cp_model.FEASIBLE, cp_model.UNKNOWN):
                new_fixes = _extract_tightened_x_fixes(proof_solver.ResponseProto())
                added_fixes = _update_cached_proof_fixes(proof_target, new_fixes)
                if log and new_fixes:
                    print(
                        f"  stall watchdog: cached {len(new_fixes)} tightened "
                        f"x-domain fix(es) at target >= {proof_target} "
                        f"({added_fixes} new)",
                        flush=True,
                    )

            if proof_status == cp_model.INFEASIBLE:
                new_upper = proof_target - 1
                model.Add(obj_expr <= new_upper)
                best_bound_seen = min(best_bound_seen, float(new_upper))
                bound_cap_applied = (
                    new_upper
                    if bound_cap_applied is None
                    else min(bound_cap_applied, new_upper)
                )
                proof_fix_min_target = None
                proof_x_fixes.clear()
                if log:
                    print(
                        f"  stall watchdog: objective >= {proof_target} is "
                        f"infeasible; proven upper bound tightened to {new_upper}",
                        flush=True,
                    )
                if new_upper <= best_obj:
                    status = cp_model.OPTIMAL
                    status_name = "OPTIMAL"
                    selected = list(best_connected)
                    objective = best_obj
                    break

            elif proof_status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                proof_selected = [
                    i for i in range(N)
                    if proof_solver.BooleanValue(x[i])
                ]
                proof_obj = int(u_g[proof_selected].sum())
                if proof_obj > best_obj:
                    best_connected = proof_selected
                    best_obj = proof_obj
                    model.Add(obj_expr >= best_obj)
                    lower_bound = max(lower_bound, best_obj)
                    _seed_solution_hints(model, proof_selected)
                    if log:
                        print(
                            f"  stall watchdog: feasibility probe found "
                            f"improved incumbent {best_obj}",
                            flush=True,
                        )

            # A no-progress cycle is one where neither the incumbent nor the
            # proven bound moved; cap consecutive occurrences so a stuck
            # window (e.g. an unresolved or infeasible next ASU) can't repeat
            # the same stall/probe cycle forever.
            made_progress = (
                best_obj > cycle_start_obj
                or best_bound_seen < cycle_start_bound - 1e-9
            )
            if made_progress:
                stall_restart_no_progress_cycles = 0
            else:
                stall_restart_no_progress_cycles += 1
                if log:
                    print(
                        f"  stall watchdog: no incumbent/bound progress this "
                        f"cycle ({stall_restart_no_progress_cycles}/"
                        f"{max_stall_restart_no_progress})",
                        flush=True,
                    )
                if stall_restart_no_progress_cycles >= max_stall_restart_no_progress:
                    if log:
                        print(
                            "  stall watchdog: giving up after repeated "
                            "no-progress restarts; keeping current incumbent",
                            flush=True,
                        )
                    break

    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        # A secondary objective must never trade away even one unemployed
        # person, so tie-breaking is staged only after primary optimality is
        # proven and obj_expr is fixed exactly. Each stage uses only time left
        # from the original per-window budget.
        if status == cp_model.OPTIMAL and deterministic_ties and rel_gap is None:
            model.Add(obj_expr == objective)
            incumbent = selected

            N_orig = len(nb_local_orig)
            if tie_break_rank is None:
                stable_rank = list(range(N))
            else:
                if len(tie_break_rank) != N_orig:
                    raise ValueError("tie_break_rank must have one entry per local node")
                # Aggregate original per-tract ranks to contracted-node ranks.
                stable_rank = [
                    sum(int(tie_break_rank[v]) + 1 for v in expand_c[ri]) - 1
                    for ri in range(N)
                ]

            rank_expr = sum((stable_rank[i] + 1) * x[i] for i in range(N))
            # Count original tracts, not contracted nodes.
            tract_count = sum(len(expand_c[ri]) * x[ri] for ri in range(N))
            tie_stages = [
                ("slack", "max", lhs),
                ("count", "min", tract_count),
                ("rank", "min", rank_expr),
            ]

            def tie_value(stage_name: str, nodes: List[int]) -> int:
                if stage_name == "slack":
                    return sum(
                        int(den) * int(u_g[i]) - int(num) * int(E_g[i])
                        for i in nodes
                    )
                if stage_name == "count":
                    return sum(len(expand_c[ri]) for ri in nodes)
                return sum(stable_rank[i] + 1 for i in nodes)

            for stage_name, direction, expression in tie_stages:
                tie_remaining = float(time_limit) - (time.monotonic() - start_time)
                if tie_remaining <= 0.01:
                    break
                # Cap each tie-break stage so a quickly-solved window doesn't
                # burn the full remaining budget on secondary objectives.
                _TIE_CAP = 30.0
                tie_stage_secs = min(tie_remaining, _TIE_CAP)
                if direction == "max":
                    model.Maximize(expression)
                else:
                    model.Minimize(expression)

                tie_solver = cp_model.CpSolver()
                tie_solver.parameters.num_search_workers = max(1, int(workers))
                tie_solver.parameters.max_time_in_seconds = tie_stage_secs
                tie_solver.parameters.log_search_progress = False
                tie_solver.parameters.cp_model_presolve = True
                tie_solver.parameters.linearization_level = 2
                if configure_subsolvers:
                    tie_solver.parameters.ignore_subsolvers.extend([
                        "pseudo_costs", "reduced_costs", "default_lp", "quick_restart",
                    ])
                tie_status = tie_solver.Solve(model)
                if tie_status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
                    break

                candidate = [i for i in range(N) if tie_solver.BooleanValue(x[i])]
                candidate_value = tie_value(stage_name, candidate)
                incumbent_value = tie_value(stage_name, incumbent)
                if (
                    (direction == "max" and candidate_value >= incumbent_value)
                    or (direction == "min" and candidate_value <= incumbent_value)
                ):
                    incumbent = candidate
                if tie_status != cp_model.OPTIMAL:
                    break

                model.Add(expression == candidate_value)

            selected = incumbent

        return _to_orig(selected, status_name)
    if best_connected is not None:
        return _to_orig(best_connected, "FEASIBLE")
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
                      tau: float, pop_thresh: int, allowed: np.ndarray, max_iter: int = 200,
                      max_swap_checks: Optional[int] = None, max_size: Optional[int] = None) -> List[int]:
    S = sorted(set(S0))
    selected = set(S)
    sum_u = int(u[S].sum())
    sum_E = int(E[S].sum())
    sum_P = int(P[S].sum())
    for _ in range(max_iter):
        improved = False
        # Greedy adds: frontier nodes are adjacent to S so S∪{t} is always connected.
        # Skipped entirely once max_size is reached (swaps below keep size constant).
        if max_size is None or len(S) < max_size:
            for t in sorted(frontier_candidates(S, nb, allowed), key=lambda i: u[i], reverse=True):
                next_u = sum_u + int(u[t])
                next_E = sum_E + int(E[t])
                next_P = sum_P + int(P[t])
                if next_P >= pop_thresh and ur_of(next_u, next_E) >= tau:
                    selected.add(t)
                    S = sorted(selected)
                    sum_u, sum_E, sum_P = next_u, next_E, next_P
                    improved = True
                    break
        if improved:
            continue
        # Swap: drop worst u, add best neighbor.
        # max_swap_checks=0 skips this entirely (used after CP-SAT solve where
        # the solution is already optimal within the window).
        if len(S) > 1 and max_swap_checks != 0:
            n_checked = 0
            for r in sorted(S, key=lambda i: u[i]):
                if max_swap_checks is not None and n_checked >= max_swap_checks:
                    break
                n_checked += 1
                reduced_u = sum_u - int(u[r])
                reduced_E = sum_E - int(E[r])
                reduced_P = sum_P - int(P[r])
                if reduced_P < pop_thresh or ur_of(reduced_u, reduced_E) < tau:
                    continue
                S2 = sorted(selected - {r})
                if not component_ok(S2, u, E, P, tau, pop_thresh, nb):
                    continue
                for a in sorted(frontier_candidates(S2, nb, allowed), key=lambda i: u[i], reverse=True):
                    next_u = reduced_u + int(u[a])
                    next_E = reduced_E + int(E[a])
                    next_P = reduced_P + int(P[a])
                    if (next_u > sum_u and next_P >= pop_thresh
                            and ur_of(next_u, next_E) >= tau):
                        selected = set(S2)
                        selected.add(a)
                        S = sorted(selected)
                        sum_u, sum_E, sum_P = next_u, next_E, next_P
                        improved = True
                        break
                if improved:
                    break
        if not improved:
            break
    return S


def _selection_key(S: set, u: np.ndarray, slack: np.ndarray) -> Tuple:
    """Lexicographic warm-start score matching the CP-SAT tie-break policy."""
    idx = sorted(S)
    return (
        int(u[idx].sum()),
        int(slack[idx].sum()),
        -len(idx),
        tuple(-i for i in idx),
    )


def _repair_rate_after_augmentation(
    selected: set,
    protected: set,
    nb: List[List[int]],
    u: np.ndarray,
    P: np.ndarray,
    slack: np.ndarray,
    pop_thresh: int,
) -> Optional[set]:
    """
    Restore rate feasibility after temporarily adding a connector/path.

    Only non-articulation, below-threshold nodes may be removed. The removal
    score is unemployment sacrificed per unit of exact rate slack recovered.
    Augmentation nodes and the root's forced high-UR component are protected so
    a proposed reroute cannot simply undo itself during repair.
    """
    S = set(selected)
    slack_sum = int(slack[list(S)].sum())
    pop_sum = int(P[list(S)].sum())

    while slack_sum < 0:
        selected_mask = np.zeros(len(nb), dtype=bool)
        selected_mask[list(S)] = True
        cut_vertices = _articulation_points(nb, selected_mask)
        candidates = [
            i for i in S - protected - cut_vertices
            if int(slack[i]) < 0 and pop_sum - int(P[i]) >= pop_thresh
        ]
        if not candidates:
            return None

        def drop_key(i: int) -> Tuple:
            recovered = -int(slack[i])
            return (
                int(u[i]) / recovered,
                int(u[i]),
                -recovered,
                i,
            )

        dropped = min(candidates, key=drop_key)
        S.remove(dropped)
        slack_sum -= int(slack[dropped])
        pop_sum -= int(P[dropped])

    return S


def _small_leaf_bundles(
    selected: set,
    protected: set,
    root_local: int,
    nb: List[List[int]],
    u: np.ndarray,
    P: np.ndarray,
    slack: np.ndarray,
    pop_thresh: int,
    max_bundle_nodes: int,
    max_candidates: int,
) -> List[frozenset]:
    """
    Return cheap connectivity-safe ejections.

    Besides ordinary non-articulation singletons, this identifies a pendant
    branch as an articulation point plus every component it separates from the
    root. Removing the whole bundle leaves exactly the root-side component, so
    a poor branch can be traded even when none of its nodes is initially
    removable on its own.
    """
    S = set(selected)
    selected_mask = np.zeros(len(nb), dtype=bool)
    selected_mask[list(S)] = True
    cut_vertices = _articulation_points(nb, selected_mask)
    pop_sum = int(P[list(S)].sum())
    bundles: set = set()

    for i in S - protected - cut_vertices:
        if int(slack[i]) < 0 and pop_sum - int(P[i]) >= pop_thresh:
            bundles.add(frozenset((i,)))

    for articulation in sorted(cut_vertices - protected):
        reached = {root_local}
        stack = [root_local]
        while stack:
            v = stack.pop()
            for w in nb[v]:
                if w in S and w != articulation and w not in reached:
                    reached.add(w)
                    stack.append(w)

        branch = frozenset(S - reached)
        if (
            1 < len(branch) <= max_bundle_nodes
            and not (branch & protected)
            and int(slack[list(branch)].sum()) < 0
            and pop_sum - int(P[list(branch)].sum()) >= pop_thresh
        ):
            bundles.add(branch)

    def bundle_key(bundle: frozenset) -> Tuple:
        idx = list(bundle)
        recovered = -int(slack[idx].sum())
        lost_u = int(u[idx].sum())
        return (lost_u / recovered, lost_u, len(bundle), tuple(sorted(bundle)))

    return sorted(bundles, key=bundle_key)[:max_candidates]


def _fractional_refill_bound(
    candidates: List[int],
    start: int,
    capacity: int,
    gain: int,
    u: np.ndarray,
    slack: np.ndarray,
) -> float:
    """Optimistic fractional-knapsack bound used only to rank beam states."""
    bound = float(gain)
    remaining = max(0, int(capacity))
    for i in candidates[start:]:
        d_i = int(slack[i])
        u_i = int(u[i])
        if d_i >= 0:
            bound += u_i
            remaining += d_i
            continue
        cost = -d_i
        if remaining <= 0:
            break
        fraction = min(1.0, remaining / cost)
        bound += fraction * u_i
        remaining -= min(remaining, cost)
    return bound


def _beam_refill(
    selected: set,
    forbidden: set,
    nb: List[List[int]],
    u: np.ndarray,
    slack: np.ndarray,
    max_candidates: int,
    beam_width: int,
) -> set:
    """
    Refill available exact rate slack with a bounded connected knapsack search.

    Candidates come from the current one-hop frontier, so every subset tested by
    the beam remains connected to the base selection. The economic ordering is
    unemployment per unit of rate deficit; the fractional upper bound preserves
    capacity-rich states that a simple ratio-greedy pass would discard.
    """
    S = set(selected)
    frontier = {
        w for v in S for w in nb[v]
        if w not in S and w not in forbidden and int(u[w]) > 0
    }

    def add_key(i: int) -> Tuple:
        d_i = int(slack[i])
        efficiency = math.inf if d_i >= 0 else int(u[i]) / -d_i
        return (-efficiency, -int(u[i]), i)

    candidates = sorted(frontier, key=add_key)[:max_candidates]
    if not candidates:
        return S

    # (unemployment gain, remaining exact slack, selected-candidate bit mask)
    states: List[Tuple[int, int, int]] = [
        (0, int(slack[list(S)].sum()), 0)
    ]

    for pos, node in enumerate(candidates):
        node_slack = int(slack[node])
        node_u = int(u[node])
        bit = 1 << pos
        expanded = list(states)
        for gain, capacity, mask in states:
            if capacity + node_slack >= 0:
                expanded.append((gain + node_u, capacity + node_slack, mask | bit))

        # Pareto dominance: with no less slack and no less gain, a state can do
        # everything a dominated state can do on the remaining candidates.
        expanded.sort(key=lambda state: (-state[1], -state[0], state[2]))
        pareto: List[Tuple[int, int, int]] = []
        best_gain = -1
        for state in expanded:
            if state[0] > best_gain:
                pareto.append(state)
                best_gain = state[0]

        if len(pareto) > beam_width:
            pareto.sort(
                key=lambda state: (
                    _fractional_refill_bound(
                        candidates, pos + 1, state[1], state[0], u, slack
                    ),
                    state[0],
                    state[1],
                    -state[2].bit_count(),
                ),
                reverse=True,
            )
            pareto = pareto[:beam_width]
        states = pareto

    best = max(
        states,
        key=lambda state: (state[0], state[1], -state[2].bit_count(), -state[2]),
    )
    for pos, node in enumerate(candidates):
        if best[2] & (1 << pos):
            S.add(node)
    return S


def _drop_redundant_zero_tracts(
    selected: set,
    protected: set,
    root_local: int,
    nb: List[List[int]],
    u: np.ndarray,
    E: np.ndarray,
    P: np.ndarray,
    pop_thresh: int,
) -> set:
    """Apply the tract-count tie-break without changing unemployment or slack."""
    S = set(selected)
    protected = set(protected) | {root_local}
    while True:
        selected_mask = np.zeros(len(nb), dtype=bool)
        selected_mask[list(S)] = True
        cut_vertices = _articulation_points(nb, selected_mask)
        pop_sum = int(P[list(S)].sum())
        removable = sorted(
            (
                i for i in S - protected - cut_vertices
                if int(u[i]) == 0 and int(E[i]) == 0
                and pop_sum - int(P[i]) >= pop_thresh
            ),
            reverse=True,
        )
        if not removable:
            return S
        S.remove(removable[0])


def articulation_reroute(
    S0: List[int],
    u: np.ndarray,
    E: np.ndarray,
    P: np.ndarray,
    nb: List[List[int]],
    tau: float,
    pop_thresh: int,
    root_local: int,
    lambda_value: float = 2.2,
    max_removed_bundle: int = 5,
    max_reroute_candidates: int = 20,
    protected: Optional[List[int]] = None,
    refill_candidates: int = 32,
    beam_width: int = 512,
    time_limit_s: float = 30.0,
) -> List[int]:
    """
    Articulation-point rerouting heuristic.

    Identifies low-value connector bundles in the current selection and replaces
    them with alternative paths through more economically productive tracts.

    For each candidate bundle (articulation point + pendant branch, up to
    max_removed_bundle nodes):
    1. Remove the bundle from the selection.
    2. Find cheapest reconnecting paths via multi-source Dijkstra, where each
       unselected node v has path_cost = max(epsilon, -economic_value[v]) and
       already-selected nodes in S_remaining are traversed for free.
    3. Accept if delta_unemployment > 0 and all constraints are met.
    4. Refill released rate slack with beam search and drop redundant zero tracts.

    Economic score:
        cap_cost[i]      = tau * emp[i] - (1 - tau) * unemp[i]  (> 0 iff UR < tau)
        economic_value[i]= unemp[i] - lambda_value * cap_cost[i]
        path_cost[i]     = max(epsilon, -economic_value[i])

    Articulation keep score (low = candidate for replacement):
        keep_score[i] = economic_value[i]
                        + 5 * selected_neighbor_count
                        - 10 * unselected_neighbor_count
    """
    if not S0:
        return []

    N = len(nb)
    S = set(int(i) for i in S0)
    protected_base = set(int(i) for i in (protected or [])) | {root_local}

    if not component_ok(sorted(S), u, E, P, tau, pop_thresh, nb):
        return sorted(S)

    num, den = as_fraction_tau(tau)
    slack = den * u.astype(np.int64) - num * E.astype(np.int64)
    cap_cost_arr = tau * E.astype(float) - (1.0 - tau) * u.astype(float)
    economic_val = u.astype(float) - lambda_value * cap_cost_arr
    _eps = 0.01
    path_cost_arr = np.maximum(_eps, -economic_val)

    t_start = time.monotonic()
    best = set(S)

    def _comps_of(sel: set) -> List[set]:
        seen: set = set()
        result: List[set] = []
        for v in sel:
            if v not in seen:
                comp: set = {v}
                stk = [v]
                seen.add(v)
                while stk:
                    cur = stk.pop()
                    for w in nb[cur]:
                        if w in sel and w not in seen:
                            seen.add(w)
                            comp.add(w)
                            stk.append(w)
                result.append(comp)
        return result

    def _reconnect(S_rem: set, removed: set) -> Optional[frozenset]:
        """
        Dijkstra from root's component only.
        S_rem nodes (already selected) cost 0 to traverse; unselected nodes cost
        path_cost_arr[v]. Removed bundle nodes are excluded.
        Returns frozenset of unselected bridge nodes, or None if unreachable.
        """
        root_comp: set = set()
        stk = [root_local]
        root_comp.add(root_local)
        while stk:
            v = stk.pop()
            for w in nb[v]:
                if w in S_rem and w not in root_comp:
                    root_comp.add(w)
                    stk.append(w)

        non_root = [c for c in _comps_of(S_rem) if root_local not in c]
        if not non_root:
            return frozenset()  # already connected

        INF = float("inf")
        dist: Dict[int, float] = {}
        prev: Dict[int, int] = {}
        heap_q: List[Tuple[float, int]] = []

        for v in root_comp:
            dist[v] = 0.0
            prev[v] = -1  # sentinel: this is a root-comp source node
            heapq.heappush(heap_q, (0.0, v))

        while heap_q:
            d, v = heapq.heappop(heap_q)
            if d > dist.get(v, INF):
                continue
            for w in nb[v]:
                if w in removed:
                    continue
                nd = d if w in S_rem else d + float(path_cost_arr[w])
                if nd < dist.get(w, INF):
                    dist[w] = nd
                    prev[w] = v
                    heapq.heappush(heap_q, (nd, w))

        added: set = set()
        for comp in non_root:
            best_v = min(comp, key=lambda v: dist.get(v, INF))
            if dist.get(best_v, INF) == INF:
                return None  # component unreachable
            # Trace path back to root_comp, collecting unselected bridge nodes
            cur = best_v
            while True:
                p = prev.get(cur, -1)
                if p == -1:
                    break  # reached a root_comp source
                if cur not in S_rem and cur not in removed:
                    added.add(cur)
                cur = p
                if cur in root_comp:
                    break
        return frozenset(added)

    any_improved = True
    while any_improved and (time.monotonic() - t_start < time_limit_s):
        any_improved = False

        sel_mask = np.zeros(N, dtype=bool)
        sel_mask[list(best)] = True
        cut_verts = _articulation_points(nb, sel_mask)
        pop_sum = int(P[sorted(best)].sum())

        bundles_scored: List[Tuple[float, frozenset]] = []
        seen_bundles: set = set()

        for art in sorted(cut_verts - protected_base):
            n_sel = sum(1 for w in nb[art] if w in best)
            n_ext = sum(1 for w in nb[art] if w not in best)
            score = float(economic_val[art]) + 5.0 * n_sel - 10.0 * n_ext

            singleton = frozenset({art})
            if singleton not in seen_bundles:
                seen_bundles.add(singleton)
                bundles_scored.append((score, singleton))

            # Bundle: articulation point + its pendant branch disconnected from root
            reachable: set = {root_local}
            stk = [root_local]
            while stk:
                v = stk.pop()
                for w in nb[v]:
                    if w in best and w != art and w not in reachable:
                        reachable.add(w)
                        stk.append(w)
            branch = frozenset((best - reachable) - {art})

            if 1 <= len(branch) <= max_removed_bundle - 1 and not (branch & protected_base):
                full_bundle = frozenset({art} | branch)
                if full_bundle not in seen_bundles:
                    seen_bundles.add(full_bundle)
                    avg_ev = sum(float(economic_val[v]) for v in full_bundle) / len(full_bundle)
                    avg_ns = sum(
                        sum(1 for w in nb[v] if w in best) for v in full_bundle
                    ) / len(full_bundle)
                    avg_nx = sum(
                        sum(1 for w in nb[v] if w not in best) for v in full_bundle
                    ) / len(full_bundle)
                    bundles_scored.append((avg_ev + 5.0 * avg_ns - 10.0 * avg_nx, full_bundle))

        # Weakest connectors (lowest keep_score) first
        bundles_scored.sort(key=lambda x: x[0])

        for _, bundle in bundles_scored[:max_reroute_candidates]:
            if time.monotonic() - t_start >= time_limit_s:
                break

            bundle_list = sorted(bundle)
            if pop_sum - int(P[bundle_list].sum()) < pop_thresh:
                continue

            S_rem = best - set(bundle)
            if root_local not in S_rem:
                continue

            path_nodes = _reconnect(S_rem, set(bundle))
            if path_nodes is None:
                continue  # some non-root component unreachable

            S_new = S_rem | set(path_nodes)
            removed_u = int(u[bundle_list].sum())
            added_u = int(u[sorted(path_nodes)].sum()) if path_nodes else 0
            if added_u - removed_u <= 0:
                continue  # no unemployment gain

            # Restore rate feasibility if violated after the exchange
            if int(slack[sorted(S_new)].sum()) < 0:
                S_repaired = _repair_rate_after_augmentation(
                    S_new, protected_base, nb, u, P, slack, pop_thresh
                )
                if S_repaired is None:
                    continue
                S_new = S_repaired
                if int(u[sorted(S_new)].sum()) <= int(u[sorted(best)].sum()):
                    continue

            if int(P[sorted(S_new)].sum()) < pop_thresh:
                continue
            if not component_ok(sorted(S_new), u, E, P, tau, pop_thresh, nb):
                continue

            # Accept: refill released slack, drop redundant zero-employment tracts
            S_refilled = _beam_refill(
                S_new, set(bundle), nb, u, slack, refill_candidates, beam_width
            )
            if component_ok(sorted(S_refilled), u, E, P, tau, pop_thresh, nb):
                S_new = S_refilled

            S_new = _drop_redundant_zero_tracts(
                S_new, protected_base, root_local, nb, u, E, P, pop_thresh
            )

            if _selection_key(S_new, u, slack) > _selection_key(best, u, slack):
                best = S_new
                any_improved = True
                break  # restart outer loop with updated selection

    return sorted(best)


def augment_prune_hint(
    S0: List[int],
    u: np.ndarray,
    E: np.ndarray,
    P: np.ndarray,
    nb: List[List[int]],
    tau: float,
    pop_thresh: int,
    root_local: int,
    protected: Optional[List[int]] = None,
    max_augmentation_candidates: int = 96,
    max_anchor_candidates: int = 12,
    max_topology_states: int = 48,
    max_bundle_nodes: int = 12,
    max_ejection_candidates: int = 24,
    refill_candidates: int = 32,
    beam_width: int = 512,
    max_ejection_rounds: int = 4,
    time_limit_s: float = 5.0,
) -> List[int]:
    """
    Connectivity-aware augment-prune-refill warm-start improvement.

    The search may temporarily add one or two frontier nodes below the rate
    threshold, then remove old connectors that the new path makes redundant.
    It subsequently tests small pendant-branch ejections and refills their rate
    slack with a bounded beam search. Only completed connected, population- and
    rate-feasible states compete with the original hint.
    """
    if not S0:
        return []

    N = len(nb)
    S_base = set(int(i) for i in S0)
    protected_base = set(int(i) for i in (protected or [])) | {root_local}
    num, den = as_fraction_tau(tau)
    slack = den * u.astype(np.int64) - num * E.astype(np.int64)

    if not component_ok(sorted(S_base), u, E, P, tau, pop_thresh, nb):
        return sorted(S_base)

    selected_mask = np.zeros(N, dtype=bool)
    selected_mask[list(S_base)] = True
    base_articulations = _articulation_points(nb, selected_mask)
    frontier = {w for v in S_base for w in nb[v] if w not in S_base}

    def economic_key(i: int) -> Tuple:
        d_i = int(slack[i])
        efficiency = math.inf if d_i >= 0 else int(u[i]) / -d_i
        return (-efficiency, -int(u[i]), i)

    structural_count = max(1, max_augmentation_candidates // 2)
    structural = sorted(
        frontier,
        key=lambda i: (
            -sum(1 for w in nb[i] if w in S_base),
            int(slack[i]),
            -int(u[i]),
            i,
        ),
    )[:structural_count]
    economic = sorted(frontier, key=economic_key)[:structural_count]
    augmentation_pool = list(dict.fromkeys(structural + economic))
    augmentation_pool = augmentation_pool[:max_augmentation_candidates]

    # Each entry is (repaired state, augmentation, number of bypassed cuts).
    topology_states: List[Tuple[set, frozenset, int]] = [
        (set(S_base), frozenset(), 0)
    ]
    single_states: List[Tuple[set, frozenset, int]] = []

    for node in augmentation_pool:
        augmentation = frozenset((node,))
        trial = S_base | set(augmentation)
        trial_mask = np.zeros(N, dtype=bool)
        trial_mask[list(trial)] = True
        freed = base_articulations - _articulation_points(nb, trial_mask)
        if not freed:
            continue
        repaired = _repair_rate_after_augmentation(
            trial, protected_base | set(augmentation), nb, u, P, slack, pop_thresh
        )
        if repaired is not None:
            single_states.append((repaired, augmentation, len(freed)))

    def topology_state_key(item: Tuple[set, frozenset, int]) -> Tuple:
        state, augmentation, freed_count = item
        score = _selection_key(state, u, slack)
        return (
            score[0],
            score[1],
            score[2],
            freed_count,
            -len(augmentation),
            score[3],
        )

    anchors = sorted(single_states, key=topology_state_key, reverse=True)[
        :max_anchor_candidates
    ]
    topology_states.extend(single_states)

    seen_pairs: set = set()
    for _, anchor_augmentation, _ in anchors:
        anchor = next(iter(anchor_augmentation))
        # In addition to two direct-frontier nodes, allow a genuine two-node
        # path whose second node touches the anchor but not the base selection.
        path_extensions = sorted(
            (w for w in nb[anchor] if w not in S_base and w != anchor),
            key=economic_key,
        )
        pair_pool = list(dict.fromkeys(augmentation_pool + path_extensions))[
            :max_augmentation_candidates
        ]
        for other in pair_pool:
            if other == anchor:
                continue
            augmentation = frozenset((anchor, other))
            if augmentation in seen_pairs:
                continue
            seen_pairs.add(augmentation)
            trial = S_base | set(augmentation)
            trial_mask = np.zeros(N, dtype=bool)
            trial_mask[list(trial)] = True
            freed = base_articulations - _articulation_points(nb, trial_mask)
            if not freed:
                continue
            repaired = _repair_rate_after_augmentation(
                trial, protected_base | set(augmentation), nb, u, P, slack, pop_thresh
            )
            if repaired is not None:
                topology_states.append((repaired, augmentation, len(freed)))

    # Deduplicate repaired selections, then retain the strongest bounded set.
    unique_states: Dict[frozenset, Tuple[set, frozenset, int]] = {}
    for item in topology_states:
        state_key = frozenset(item[0])
        old = unique_states.get(state_key)
        if old is None or topology_state_key(item) > topology_state_key(old):
            unique_states[state_key] = item
    ranked_states = sorted(
        unique_states.values(), key=topology_state_key, reverse=True
    )[:max_topology_states]
    if frozenset(S_base) not in {frozenset(item[0]) for item in ranked_states}:
        ranked_states.append((set(S_base), frozenset(), 0))

    augment_start = time.monotonic()
    best = set(S_base)
    for repaired, augmentation, _ in ranked_states:
        if time.monotonic() - augment_start >= time_limit_s:
            break
        current = set(repaired)
        permanently_forbidden = (S_base | set(augmentation)) - current
        protected_state = protected_base | set(augmentation)

        for _ in range(max_ejection_rounds):
            options: List[Tuple[set, frozenset]] = [
                (
                    _beam_refill(
                        current, permanently_forbidden, nb, u, slack,
                        refill_candidates, beam_width,
                    ),
                    frozenset(),
                )
            ]
            for bundle in _small_leaf_bundles(
                current, protected_state, root_local, nb, u, P, slack,
                pop_thresh, max_bundle_nodes, max_ejection_candidates,
            ):
                pruned = current - set(bundle)
                refilled = _beam_refill(
                    pruned, permanently_forbidden | set(bundle), nb, u, slack,
                    refill_candidates, beam_width,
                )
                options.append((refilled, bundle))

            candidate, ejected = max(
                options, key=lambda item: _selection_key(item[0], u, slack)
            )
            if _selection_key(candidate, u, slack) <= _selection_key(current, u, slack):
                break
            current = candidate
            permanently_forbidden.update(ejected)

        current = _drop_redundant_zero_tracts(
            current, protected_state, root_local, nb, u, E, P, pop_thresh
        )
        if (
            component_ok(sorted(current), u, E, P, tau, pop_thresh, nb)
            and _selection_key(current, u, slack) > _selection_key(best, u, slack)
        ):
            best = current

    return sorted(best)


# ---------- Local CP-SAT repair heuristic ----------

@dataclass
class RepairResult:
    selected: List[int]
    old_unemployed: int
    new_unemployed: int
    improvement: int
    status: str
    best_bound: Optional[float]
    free_nodes: List[int]
    solve_seconds: float


def build_repair_neighborhood(
    selected: "Sequence[int]",
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    tau: float,
    root_local: int,
    max_free_nodes: int = 500,
    hops: int = 2,
) -> List[int]:
    """
    Build a free-node pool for local repair.

        Uses an explicit two-pool budget:
      - Up to 40% of max_free_nodes for interior weak selected nodes: non-root,
        non-articulation selected nodes sorted by cap = tau*E-(1-tau)*u descending
        (high cap = low UR = most worth dropping), regardless of BFS distance.
            - Remaining boundary budget reserves one third for selected structural nodes
                and uses the rest for unselected alternatives ranked by unemployment per
                rate-capacity cost.
        Root is always fixed.
    Returns a deterministic sorted list trimmed to max_free_nodes.
    """
    N = len(nb_local)
    sel_set = set(int(i) for i in selected)

    # Articulation points must stay fixed (removing them disconnects the selection)
    sel_mask = np.zeros(N, dtype=bool)
    for i in sel_set:
        sel_mask[i] = True
    art_pts = _articulation_points(nb_local, sel_mask) if sel_set else set()

    # --- BFS pool: boundary + frontier + hops expansion ---
    boundary_sel = {i for i in sel_set if any(w not in sel_set for w in nb_local[i])}
    frontier_unsel = {w for i in sel_set for w in nb_local[i] if w not in sel_set}

    bfs_dist: Dict[int, int] = {}
    current_front = boundary_sel | frontier_unsel
    for v in current_front:
        bfs_dist[v] = 0
    for h in range(1, hops + 1):
        next_front: set = set()
        for v in current_front:
            for w in nb_local[v]:
                if w not in bfs_dist:
                    bfs_dist[w] = h
                    next_front.add(w)
        current_front = next_front

    bfs_pool = set(bfs_dist.keys())

    # Articulation points adjacent to boundary but outside BFS
    extra_art = {
        w for v in (boundary_sel | frontier_unsel)
        for w in nb_local[v]
        if w in art_pts and w not in bfs_pool
    }
    for w in extra_art:
        bfs_dist[w] = hops
    bfs_pool |= extra_art

    # Pendant branches hanging off in-pool articulation points (size limit 30)
    pendant_members: set = set()
    for art in art_pts & sel_set & bfs_pool:
        reachable = {root_local}
        stk = [root_local]
        while stk:
            v = stk.pop()
            for w in nb_local[v]:
                if w in sel_set and w != art and w not in reachable:
                    reachable.add(w)
                    stk.append(w)
        branch = sel_set - reachable - {art}
        if 1 <= len(branch) <= 30:
            pendant_members |= branch
    for w in pendant_members:
        if w not in bfs_dist:
            bfs_dist[w] = hops + 1
    bfs_pool |= pendant_members

    # Unselected nodes with >=2 selected neighbors outside current pool
    multi_conn = {
        w for i in sel_set for w in nb_local[i]
        if w not in sel_set and w not in bfs_pool
        and sum(1 for v in nb_local[w] if v in sel_set) >= 2
    }
    for w in multi_conn:
        bfs_dist[w] = hops + 1
    bfs_pool |= multi_conn

    bfs_pool.discard(root_local)
    bfs_dist.pop(root_local, None)

    # --- Weak interior selected pool ---
    # Non-root, non-articulation selected nodes NOT already in bfs_pool.
    # Sorted by cap descending: high cap = low individual UR = most worth dropping.
    weak_budget = int(max_free_nodes * 0.4)
    interior_candidates = [
        i for i in sel_set
        if i != root_local and i not in art_pts and i not in bfs_pool
    ]
    interior_candidates.sort(
        key=lambda i: tau * float(E_g[i]) - (1.0 - tau) * float(u_g[i]),
        reverse=True,
    )
    weak_interior = set(interior_candidates[:weak_budget])

    # --- Explicit budget allocation: weak interior, structural, alternatives ---
    remaining = max_free_nodes - len(weak_interior)

    if len(bfs_pool) <= remaining:
        bfs_chosen = bfs_pool
    else:
        selected_bfs = bfs_pool & sel_set
        unselected_bfs = bfs_pool - sel_set

        def _score_selected(i: int) -> tuple:
            d = bfs_dist.get(i, hops + 2)
            is_tier1 = i in boundary_sel
            is_art = i in art_pts
            is_pendant = i in pendant_members
            n_sel_nb = sum(1 for w in nb_local[i] if w in sel_set)
            cap = tau * float(E_g[i]) - (1.0 - tau) * float(u_g[i])
            removal_efficiency = float(u_g[i]) / cap if cap > 0 else 1e12
            return (is_tier1, -d, is_art or is_pendant, n_sel_nb, -removal_efficiency, -i)

        def _score_unselected(i: int) -> tuple:
            d = bfs_dist.get(i, hops + 2)
            cap = tau * float(E_g[i]) - (1.0 - tau) * float(u_g[i])
            add_efficiency = float(u_g[i]) / cap if cap > 0 else 1e12
            n_sel_nb = sum(1 for w in nb_local[i] if w in sel_set)
            return (i in frontier_unsel, -d, add_efficiency, n_sel_nb, int(u_g[i]), -i)

        structural_budget = min(len(selected_bfs), remaining // 3)
        structural = sorted(selected_bfs, key=_score_selected, reverse=True)
        alternatives = sorted(unselected_bfs, key=_score_unselected, reverse=True)
        bfs_chosen = set(structural[:structural_budget])
        bfs_chosen.update(alternatives[:remaining - len(bfs_chosen)])
        if len(bfs_chosen) < remaining:
            bfs_chosen.update(structural[structural_budget:remaining])

    return sorted(bfs_chosen | weak_interior)


def _validate_repair_result(
    candidate: List[int],
    original: List[int],
    free_nodes: "Sequence[int]",
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    verbose: bool = False,
) -> Optional[List[int]]:
    """Validate a repair candidate; return sorted selection or None on failure."""
    N = len(nb_local)
    free_set = set(int(i) for i in free_nodes)
    orig_set = set(int(i) for i in original)
    cand_list = sorted(int(i) for i in candidate)
    cand_set = set(cand_list)

    if root_local not in cand_set:
        if verbose:
            print("  [repair validate] FAIL: root not selected", flush=True)
        return None

    if len(cand_list) != len(cand_set) or any(i < 0 or i >= N for i in cand_set):
        if verbose:
            print("  [repair validate] FAIL: invalid or duplicate indices", flush=True)
        return None

    for i in range(N):
        if i not in free_set and (i in orig_set) != (i in cand_set):
            if verbose:
                print(f"  [repair validate] FAIL: fixed node {i} changed", flush=True)
            return None

    if not component_ok(cand_list, u_g, E_g, P_g, tau, pop_thresh, nb_local):
        if verbose:
            print("  [repair validate] FAIL: connectivity/population/rate check failed", flush=True)
        return None

    old_u = int(u_g[sorted(orig_set)].sum())
    new_u = int(u_g[cand_list].sum())
    if new_u <= old_u:
        if verbose:
            print(f"  [repair validate] FAIL: no strict improvement ({new_u} <= {old_u})", flush=True)
        return None

    return cand_list


def solve_local_repair(
    current_selected: "Sequence[int]",
    free_nodes: "Sequence[int]",
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    time_limit: float = 15.0,
    num_workers: int = 8,
    random_seed: int = 1,
) -> "RepairResult":
    """
    Fix all tracts outside free_nodes to current selection; optimize the free neighborhood.
    Uses signed flow on the full graph for connectivity — presolve eliminates fixed variables.
    Returns a RepairResult; falls back to original selection on INFEASIBLE/UNKNOWN.
    """
    N = len(nb_local)
    current_set = set(int(i) for i in current_selected)
    free_set = set(int(i) for i in free_nodes)
    current_list = sorted(current_set)
    current_u = int(u_g[current_list].sum())

    t0 = time.monotonic()
    model = cp_model.CpModel()

    x = [model.NewBoolVar(f"x_{i}") for i in range(N)]

    for i in range(N):
        if i not in free_set:
            model.Add(x[i] == int(i in current_set))
    model.Add(x[root_local] == 1)

    model.Add(sum(int(P_g[i]) * x[i] for i in range(N)) >= int(pop_thresh))

    num, den = as_fraction_tau(tau)
    model.Add(
        sum(int(den) * int(u_g[i]) * x[i] for i in range(N))
        - sum(int(num) * int(E_g[i]) * x[i] for i in range(N))
        >= 0
    )

    obj_expr = sum(int(u_g[i]) * x[i] for i in range(N))
    model.Add(obj_expr >= current_u + 1)
    model.Maximize(obj_expr)

    for i in range(N):
        model.AddHint(x[i], int(i in current_set))

    # Signed flow connectivity over the full graph; presolve eliminates fixed-variable constraints
    edges = sorted({
        (min(i, j), max(i, j))
        for i, neighbors in enumerate(nb_local)
        for j in neighbors if i != j
    })
    M = max(1, N - 1)
    f = [model.NewIntVar(-M, M, f"rf_{i}_{j}") for i, j in edges]
    selected_count = sum(x)
    net_out: List[list] = [[] for _ in range(N)]
    for eidx, (i, j) in enumerate(edges):
      model.Add(f[eidx] == 0).OnlyEnforceIf(x[i].Not())
      model.Add(f[eidx] == 0).OnlyEnforceIf(x[j].Not())
  
      net_out[i].append(f[eidx])
      net_out[j].append(-f[eidx])
    for i in range(N):
        expr = sum(net_out[i]) if net_out[i] else 0
        model.Add(expr == (selected_count - 1 if i == root_local else -x[i]))

    fhints = _spanning_tree_flows(current_list, nb_local, root_local)
    for eidx, (i, j) in enumerate(edges):
        model.AddHint(f[eidx], fhints.get((i, j), 0) - fhints.get((j, i), 0))

    solver = cp_model.CpSolver()
    solver.parameters.num_search_workers = max(1, int(num_workers))
    solver.parameters.max_time_in_seconds = float(time_limit)
    solver.parameters.log_search_progress = False
    solver.parameters.cp_model_presolve = True
    solver.parameters.linearization_level = 2
    solver.parameters.random_seed = int(random_seed)

    status = solver.Solve(model)
    solve_secs = time.monotonic() - t0
    status_name = solver.StatusName(status)

    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        new_sel = [i for i in range(N) if solver.BooleanValue(x[i])]
        new_u = int(u_g[new_sel].sum())
        return RepairResult(
            selected=new_sel,
            old_unemployed=current_u,
            new_unemployed=new_u,
            improvement=new_u - current_u,
            status=status_name,
            best_bound=solver.BestObjectiveBound(),
            free_nodes=sorted(free_set),
            solve_seconds=solve_secs,
        )

    return RepairResult(
        selected=current_list,
        old_unemployed=current_u,
        new_unemployed=current_u,
        improvement=0,
        status=status_name,
        best_bound=None,
        free_nodes=sorted(free_set),
        solve_seconds=solve_secs,
    )


def improve_with_local_repair(
    initial_selected: "Sequence[int]",
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    *,
    max_rounds: int = 3,
    max_free_nodes: int = 1000,
    hops: int = 5,
    time_limit: float = 15.0,
    num_workers: int = 8,
    random_seed: int = 1,
    verbose: bool = False,
) -> List[int]:
    """Run up to max_rounds local CP-SAT repair passes; accept only strict improvements."""
    current = sorted(int(i) for i in initial_selected)
    num_r, den_r = as_fraction_tau(tau)

    for rnd in range(1, max_rounds + 1):
        free_nodes = build_repair_neighborhood(
            current, nb_local, u_g, E_g, tau, root_local, max_free_nodes, hops
        )
        if not free_nodes:
            break

        result = solve_local_repair(
            current, free_nodes, nb_local, u_g, E_g, P_g, tau, pop_thresh,
            root_local, time_limit, num_workers, random_seed + rnd - 1,
        )

        if verbose:
            bb_str = f"{result.best_bound:.1f}" if result.best_bound is not None else "N/A"
            print(
                f"  [local repair] round {rnd}: current_unemp={result.old_unemployed:,}, "
                f"free_tracts={len(free_nodes)}, status={result.status}, "
                f"best_bound={bb_str}, repaired_unemp={result.new_unemployed:,}, "
                f"improvement=+{result.improvement}, "
                f"solve_time={result.solve_seconds:.1f}s",
                flush=True,
            )

        if result.improvement <= 0:
            if verbose:
                print(f"  [local repair] round {rnd}: no strict improvement; stopping", flush=True)
            break

        validated = _validate_repair_result(
            result.selected, current, free_nodes,
            nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local, verbose=verbose,
        )
        if validated is None:
            if verbose:
                print(f"  [local repair] round {rnd}: validation failed; retaining previous", flush=True)
            break

        if verbose:
            added = sorted(set(validated) - set(current))
            removed = sorted(set(current) - set(validated))
            new_pop = int(P_g[np.array(validated, dtype=int)].sum())
            old_pop = int(P_g[np.array(current, dtype=int)].sum())
            new_slack = int(
                (den_r * u_g[np.array(validated, dtype=int)].astype(np.int64)
                 - num_r * E_g[np.array(validated, dtype=int)].astype(np.int64)).sum()
            )
            old_slack = int(
                (den_r * u_g[np.array(current, dtype=int)].astype(np.int64)
                 - num_r * E_g[np.array(current, dtype=int)].astype(np.int64)).sum()
            )
            print(
                f"    added={len(added)}, removed={len(removed)}, "
                f"changed={len(added)+len(removed)}, "
                f"pop: {old_pop}->{new_pop}, rate_slack: {old_slack}->{new_slack}",
                flush=True,
            )
            if len(added) <= 10:
                print(f"    added indices: {added}", flush=True)
            if len(removed) <= 10:
                print(f"    removed indices: {removed}", flush=True)

        current = validated

    return current


def _prepare_window_hint(
    nb_local: List[List[int]], u_g: np.ndarray, E_g: np.ndarray, P_g: np.ndarray,
    tau: float, pop_thresh: int, root_local: int, verbose: bool = False,
    max_nodes: Optional[int] = None,
) -> Dict:
    """
    Build a warm-start hint using reverse_prune on the original graph, then refine
    with improve_by_trades and articulation rerouting.
    Contraction is retained only to derive root_component and cluster_groups.
    `max_nodes`, when given, keeps the refined hint's greedy-add phase from
    growing past that many tracts (swaps still keep size constant).
    """
    nb_r, u_r, E_r, P_r, expand_r, node_map_r = contract_high_ur_nodes(nb_local, u_g, E_g, P_g, tau)
    root_r = int(node_map_r[root_local])
    root_component = expand_r[root_r]
    all_local = np.arange(len(nb_local))

    def _ur(u_arr, e_arr, idx):
        su, se = int(u_arr[idx].sum()), int(e_arr[idx].sum())
        return 100.0 * su / max(su + se, 1), su

    def _refine(hint_raw: List[int], label: str) -> Dict:
        hint_expanded = sorted(hint_raw)  # already original-graph indices
        if verbose:
            ur_raw, u_raw = _ur(u_g, E_g, hint_expanded)
            print(f"    [{label}] raw: tracts={len(hint_expanded)}, unemp={u_raw}, UR={ur_raw:.2f}%", flush=True)
        hint_improved = improve_by_trades(hint_expanded, u_g, E_g, P_g, nb_local, tau, pop_thresh, all_local, max_iter=100, max_size=max_nodes)
        hint_valid = component_ok(hint_improved, u_g, E_g, P_g, tau, pop_thresh, nb_local)
        hint_obj_val = int(u_g[hint_improved].sum()) if hint_valid else None
        if verbose:
            if hint_valid:
                ur_imp, _ = _ur(u_g, E_g, hint_improved)
                print(f"    [{label}] after trades: tracts={len(hint_improved)}, unemp={hint_obj_val}, UR={ur_imp:.2f}%", flush=True)
            else:
                print(f"    [{label}] infeasible after trades", flush=True)
        return {"hint_improved": hint_improved, "hint_valid": hint_valid, "hint_obj_val": hint_obj_val}

    if verbose:
        print(f"  [heuristic] reverse_prune ...", flush=True)
    best = _refine(
        reverse_prune_hint(nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local),
        "reverse_prune",
    )
    hint_source = "reverse_prune"

    if best["hint_valid"]:
        if verbose:
            print(f"  [heuristic] articulation_reroute ...", flush=True)
        rerouted = articulation_reroute(
            best["hint_improved"], u_g, E_g, P_g, nb_local, tau, pop_thresh,
            root_local, protected=root_component,
        )
        if component_ok(rerouted, u_g, E_g, P_g, tau, pop_thresh, nb_local):
            num_rr, den_rr = as_fraction_tau(tau)
            exact_slack_rr = den_rr * u_g.astype(np.int64) - num_rr * E_g.astype(np.int64)
            if _selection_key(set(rerouted), u_g, exact_slack_rr) > _selection_key(
                set(best["hint_improved"]), u_g, exact_slack_rr
            ):
                rr_u = int(u_g[rerouted].sum())
                rr_E = int(E_g[rerouted].sum())
                if verbose:
                    print(
                        f"  [heuristic] articulation_reroute improved: "
                        f"tracts={len(rerouted)}, unemp={rr_u}, "
                        f"UR={100.0 * rr_u / max(rr_u + rr_E, 1):.2f}%",
                        flush=True,
                    )
                best = {"hint_improved": rerouted, "hint_valid": True, "hint_obj_val": rr_u}
                hint_source += "+articulation_reroute"
            elif verbose:
                print(f"  [heuristic] articulation_reroute did not improve over {hint_source}", flush=True)

    return {
        "root_component": root_component,
        "n_contracted": len(nb_r),
        "hint_improved": best["hint_improved"],
        "hint_valid": best["hint_valid"],
        "hint_obj_val": best["hint_obj_val"],
        "hint_source": hint_source,
        "cluster_groups": [group for group in expand_r if len(group) > 1],
    }


# ---------- High-level multi-ASU builder ----------
def _export_window_comparison(
    w: Dict,
    sol_local: Optional[List[int]],
    df: pd.DataFrame,
    export_dir: str,
    asu_num: int,
    tau: float = 0.0645,
) -> None:
    """Write tract-comparison + neighbor-list Excel for one solved window."""
    import os
    try:
        import openpyxl  # noqa: F401
    except ImportError:
        print("  [EXPORT] skipped: openpyxl not installed (pip install openpyxl)", flush=True)
        return

    sub = w["sub"]
    nb_local: List[List[int]] = w["nb_local"]
    N = len(sub)
    geoids = [str(df.iloc[int(sub[i])]["geoid"]) for i in range(N)]
    unemp = w["u_g"]
    emp   = w["E_g"]
    pop   = w["P_g"]

    hint_set = set(w.get("hint_improved") or [])
    sol_set  = set(sol_local or [])
    root     = w["root_local"]
    hsrc     = w.get("hint_source", "")

    tract_rows = []
    for i in range(N):
        u_i, e_i, p_i = int(unemp[i]), int(emp[i]), int(pop[i])
        # positive = UR < tau (drains threshold slack); negative = UR >= tau (contributes)
        cap_slack_i = round(tau * e_i - (1.0 - tau) * u_i, 4)
        tract_rows.append({
            "global_idx":  int(sub[i]),
            "local_idx":   i,
            "geoid":       geoids[i],
            "unemp":       u_i,
            "emp":         e_i,
            "pop":         p_i,
            "ur_pct":      round(u_i / max(u_i + e_i, 1) * 100, 4),
            "cap_slack":   cap_slack_i,
            "in_hint":     i in hint_set,
            "hint_source": hsrc if i in hint_set else "",
            "in_solution": i in sol_set,
            "hint_not_sol": (i in hint_set) and (i not in sol_set),
            "sol_not_hint": (i not in hint_set) and (i in sol_set),
            "is_root":     i == root,
        })

    seen: set = set()
    edge_rows = []
    for i in range(N):
        for j in nb_local[i]:
            edge = (min(i, j), max(i, j))
            if edge not in seen:
                seen.add(edge)
                edge_rows.append({
                    "from_idx":       i,
                    "to_idx":         j,
                    "from_geoid":     geoids[i],
                    "to_geoid":       geoids[j],
                    "from_in_hint":   i in hint_set,
                    "to_in_hint":     j in hint_set,
                    "from_in_sol":    i in sol_set,
                    "to_in_sol":      j in sol_set,
                })

    os.makedirs(export_dir, exist_ok=True)
    path = os.path.join(export_dir, f"asu_{asu_num:03d}_seed{w['seed']}.xlsx")
    with pd.ExcelWriter(path, engine="openpyxl") as writer:
        pd.DataFrame(tract_rows).to_excel(writer, sheet_name="tracts",    index=False)
        pd.DataFrame(edge_rows).to_excel( writer, sheet_name="neighbors", index=False)
    print(f"  [EXPORT] {path}", flush=True)


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
    parallel_asus: int = 1,
    merge_adjacent: bool = True,
    export_dir: Optional[str] = None,
    deterministic_ties: bool = True,
    objective_shaving: bool = False,
    use_root_articulation_implications: bool = False,
    use_signed_flow: bool = True,
    use_arborescence: bool = False,
    configure_subsolvers: bool = True,
    use_tract_first_search: bool = False,
    use_flow_count_envelope: bool = True,
    use_small_root_separators: bool = True,
    root_separator_max_size: int = 3,
    root_separator_clause_limit: int = 200,
    root_separator_target_limit: int = 128,
    use_separator_cardinality_bounds: bool = True,
    solution_pool_size: int = 32,
    full_graph_window: bool = False,
    use_bridge_edge_bounds: bool = False,
    max_nodes_per_asu: Optional[int] = None,
    combine_capped_asus: bool = True,
    stop_flag_path: Optional[str] = None,
    skip_flag_path: Optional[str] = None,
) -> Dict[str, np.ndarray]:
    """
    Build ASUs in batches of up to `parallel_asus` disjoint candidate windows, solved
    concurrently. Two ASUs built in the same batch that end up touching (share a
    queen-contiguity edge) are merged into one: the mediant inequality guarantees
    that combining two groups whose UR is each >= tau keeps the combined UR >= tau
    (the combined ratio is a weighted average of the two, so it can't fall below
    the smaller one), and population/connectivity only improve on union.

    `max_nodes_per_asu`, when given, hard-caps every individual ASU built during
    the main loop at that many tracts (optional; `None`, the default, leaves ASU
    size unconstrained and disables the combine phase below). Capping cluster
    size lets many small, cheap-to-solve, non-overlapping ASU candidates be
    carved out of the state before any single window grows large.

    When `max_nodes_per_asu` is set and `combine_capped_asus` is True (default),
    a final pass runs after the main loop finishes carving capped, non-overlapping
    clusters: touching committed ASUs (sharing a queen-contiguity edge) are unioned
    into one candidate (always feasible via the same mediant-inequality argument
    as the in-batch merge above), the union's window is expanded by one hop into
    still-unclaimed tracts, and the *uncapped* CP-SAT solver is re-run on that
    window using the merged cluster as its warm-start hint -- giving it a chance
    to add a few more tracts now that the artificial per-cluster cap no longer
    applies.

    `stop_flag_path`, when given, names a file that a running solve polls; once
    it exists, each in-flight window's CP-SAT solve halts via `stop_search()`
    and returns its current incumbent, that incumbent is committed as the final
    (partial) ASU, and no further ASU windows are started.

    `skip_flag_path` similarly halts only the in-flight window(s) of the
    current batch -- the flag is consumed on detection -- so the (partial)
    incumbent is committed as its ASU and the loop continues on to build the
    next ASU window normally.
    """
    def _round_to_int64(col: pd.Series, name: str) -> np.ndarray:
        # BLS/ACS counts should already be whole numbers; round explicitly
        # instead of truncating so any upstream fractional noise is caught
        # and rounded to nearest rather than silently dropped.
        raw = col.to_numpy(dtype=np.float64)
        rounded = np.round(raw)
        if verbose and not np.allclose(raw, rounded, atol=1e-6):
            max_frac = float(np.max(np.abs(raw - rounded)))
            print(
                f"  [WARN] {name} has non-integer values "
                f"(max deviation {max_frac:.4g}); rounding to nearest integer",
                flush=True,
            )
        return rounded.astype(np.int64)

    u = _round_to_int64(df["tract_ASU_unemp"], "tract_ASU_unemp")
    E = _round_to_int64(df["tract_ASU_emp"], "tract_ASU_emp")
    P = _round_to_int64(df["tract_pop2024"], "tract_pop2024")
    UR = u / np.maximum(u + E, 1e-12)

    n = len(df)
    remaining = np.ones(n, dtype=bool)
    tried = np.zeros(n, dtype=bool)
    asu_id = np.full(n, -1, dtype=int)
    num, den = as_fraction_tau(tau)

    batch_size = max(1, int(parallel_asus))
    k = 0
    while k < max_asus:
        if _stop_requested(stop_flag_path):
            if verbose:
                print("\n[stop] Stop flag detected; no further ASU windows will be started.", flush=True)
            break

        rem_idx = np.where(remaining)[0]
        if rem_idx.size < 2:
            break

        # First filter for tracts with UR >= tau
        rem_UR = UR[rem_idx]
        high_ur_mask = rem_UR >= tau

        # If no remaining tract has UR >= tau, stop building ASUs
        if not high_ur_mask.any():
            if verbose:
                print(f"\nNo remaining tracts have UR >= {tau*100:.2f}%. Stopping ASU creation.", flush=True)
            break

        # Filter to only consider high UR tracts as potential seeds
        high_ur_rem_idx = rem_idx[high_ur_mask]

        # Among high UR tracts, find those with at least one remaining neighbor
        deg_rem = np.array([np.sum(remaining[np.array(nb[i], dtype=int)]) for i in high_ur_rem_idx])
        cand_seeds = high_ur_rem_idx[deg_rem > 0]

        if cand_seeds.size == 0:
            if verbose:
                print(f"\nNo high-UR tracts (UR >= {tau*100:.2f}%) have remaining neighbors. Stopping.", flush=True)
            break

        # Prioritize by UR (descending) then population (descending)
        order = np.lexsort((-df.loc[cand_seeds, "tract_pop2024"].to_numpy(), -UR[cand_seeds]))
        seed_pool = cand_seeds[order]

        # ---- Select up to batch_size disjoint feasible windows ----
        reserved = np.zeros(n, dtype=bool)
        windows: List[Dict] = []
        for s in seed_pool:
            if len(windows) >= batch_size:
                break
            s = int(s)
            if tried[s] or reserved[s] or not remaining[s]:
                continue
            allowed_idx = np.where(remaining & ~reserved)[0]
            if allowed_idx.size == 0:
                break

            if full_graph_window:
                r = "all"
                sub = allowed_idx.astype(int).tolist()
            else:
                r = int(r_start)
                sub = bfs_ball(nb, s, r, allowed_idx)
                while P[sub].sum() < min_pop_margin * pop_thresh and r < r_max and len(sub) < hard_cap_nodes:
                    r += r_step
                    sub = bfs_ball(nb, s, r, allowed_idx)
                if len(sub) > hard_cap_nodes:
                    while len(sub) > hard_cap_nodes and r > 1:
                        r -= 1
                        sub = bfs_ball(nb, s, r, allowed_idx)
                    if len(sub) > hard_cap_nodes:
                        sub = sub[:hard_cap_nodes]

            local_index = {g: i for i, g in enumerate(sub)}
            nb_local: List[List[int]] = [
                sorted(local_index[h] for h in nb[g] if h in local_index) for g in sub
            ]
            u_g, E_g, P_g = u[sub], E[sub], P[sub]
            deg_w = np.array([len(v) for v in nb_local])
            cand = np.where(deg_w > 0)[0]
            if cand.size == 0:
                tried[s] = True
                continue
            if (u_g / np.maximum(u_g + E_g, 1e-12)).max(initial=0.0) < tau:
                if verbose:
                    print(f"  [seed={s}] skip: window max(UR) < tau", flush=True)
                tried[s] = True
                continue
            if not can_hit_tau(u_g, E_g, P_g, nb_local, tau, pop_thresh):
                if verbose:
                    print(f"  [seed={s}] skip: quick screen fails", flush=True)
                tried[s] = True
                continue

            top = cand[np.argmax(u_g[cand] / np.maximum(u_g[cand] + E_g[cand], 1e-12))]
            tie = np.where(
                (u_g / np.maximum(u_g + E_g, 1e-12)) == (u_g[top] / max(u_g[top] + E_g[top], 1e-12))
            )[0]
            root_local = int(tie[np.argmax(P_g[tie])]) if len(tie) > 1 else int(top)

            if "geoid" in df.columns:
                stable_values = [str(df.iloc[int(g)]["geoid"]) for g in sub]
            else:
                stable_values = [str(int(g)).zfill(12) for g in sub]
            stable_order = sorted(range(len(sub)), key=lambda i: (stable_values[i], i))
            tie_break_rank = [0] * len(sub)
            for rank, local_i in enumerate(stable_order):
                tie_break_rank[local_i] = rank

            windows.append({
                "seed": s, "sub": sub, "nb_local": nb_local,
                "u_g": u_g, "E_g": E_g, "P_g": P_g, "root_local": root_local, "r": r,
                "tie_break_rank": tie_break_rank,
            })
            reserved[sub] = True

        if not windows:
            if verbose:
                print("No remaining high-UR seeds produce a feasible window; stopping.", flush=True)
            break

        # Reserved territory for this whole batch, computed once up front so a
        # window committed early (e.g. an uncapped rescue below) can safely
        # exclude still-pending sibling windows from its own greedy refinement
        # even though those siblings haven't been cleared from `remaining` yet.
        batch_mask = np.zeros(n, dtype=bool)
        for w in windows:
            batch_mask[w["sub"]] = True

        if verbose:
            print(f"\n[Batch] solving {len(windows)} window(s) concurrently (ASUs {k+1}..{k+len(windows)}) ...", flush=True)

        # ---- Build warm-start hints (sequential; cheap relative to CP-SAT) ----
        for w in windows:
            info = _prepare_window_hint(
                w["nb_local"], w["u_g"], w["E_g"], w["P_g"], tau, pop_thresh, w["root_local"],
                verbose=verbose, max_nodes=max_nodes_per_asu,
            )
            w.update(info)
            if verbose:
                su, sE = int(u[w["sub"]].sum()), int(E[w["sub"]].sum())
                URw = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
                seed_ur = 100.0 * UR[w["seed"]]
                print(f"\n[ASU seed={w['seed']}] (UR={seed_ur:.2f}%) | window: r={w['r']}, "
                      f"nodes={len(w['sub'])}, pop={int(w['P_g'].sum())}, UR={URw:.2f}%", flush=True)
                print(f"  root_local={w['root_local']} (UR={100*(w['u_g'][w['root_local']]/max(w['u_g'][w['root_local']]+w['E_g'][w['root_local']],1e-12)):.3f}%, "
                      f"pop={int(w['P_g'][w['root_local']])})", flush=True)
                if info["n_contracted"] < len(w["nb_local"]):
                    print(f"  contracted: {len(w['nb_local'])} -> {info['n_contracted']} nodes", flush=True)
                if len(info["root_component"]) > 1:
                    print(f"  fixed root high-UR component: {len(info['root_component'])} tracts", flush=True)
                if info["hint_valid"]:
                    hu = info["hint_obj_val"]
                    hE = int(w["E_g"][np.array(info["hint_improved"], dtype=int)].sum())
                    print(f"  [HINT] {info['hint_source']} warm start: tracts={len(info['hint_improved'])}, unemp={hu}, UR={100.0*hu/max(hu+hE,1):.2f}%", flush=True)

        # Under a per-ASU cap, a window can fail for structural reasons that are
        # a property of the (unchanged) remaining graph, not of which seed
        # happened to be picked -- root_local/root_component are recomputed
        # from the full remaining pool every attempt (especially under
        # full_graph_window, where every seed shares the exact same window).
        # Marking only the picked seed as tried and retrying with a "different"
        # seed re-derives the identical failing window forever and never makes
        # progress. Whenever that structural failure is caused by the cap, solve
        # this window once uncapped and commit it immediately so its tracts
        # leave `remaining` and the loop is guaranteed to advance.
        def _commit_uncapped_window(w: Dict, reason: str) -> None:
            nonlocal k
            all_local = np.arange(len(w["nb_local"]))
            hint_uncapped = improve_by_trades(
                w["root_component"], w["u_g"], w["E_g"], w["P_g"], w["nb_local"],
                tau, pop_thresh, all_local, max_iter=100,
            )
            hint_valid_uncapped = component_ok(
                hint_uncapped, w["u_g"], w["E_g"], w["P_g"], tau, pop_thresh, w["nb_local"]
            )
            hint_obj_uncapped = (
                int(w["u_g"][np.array(hint_uncapped, dtype=int)].sum()) if hint_valid_uncapped else None
            )
            result = solve_one_asu_cpsat(
                nb_local=w["nb_local"], u_g=w["u_g"], E_g=w["E_g"], P_g=w["P_g"],
                tau=tau, pop_thresh=pop_thresh, root_local=w["root_local"],
                time_limit=time_limit, workers=workers, rel_gap=rel_gap, log=verbose,
                hint=hint_uncapped, hint_obj=hint_obj_uncapped,
                forced_selected=w["root_component"],
                deterministic_ties=deterministic_ties,
                tie_break_rank=w["tie_break_rank"],
                objective_shaving=objective_shaving,
                use_root_articulation_implications=use_root_articulation_implications,
                use_signed_flow=use_signed_flow,
                use_arborescence=use_arborescence,
                configure_subsolvers=configure_subsolvers,
                use_tract_first_search=use_tract_first_search,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                # max_nodes intentionally omitted: this window is being solved
                # uncapped by necessity (see `reason`).
                stop_flag_path=stop_flag_path,
                skip_flag_path=skip_flag_path,
            )
            S_local = result.sel_idx_local if result is not None else w["root_component"]
            S_global = np.array(w["sub"], dtype=int)[np.array(S_local, dtype=int)].tolist()
            if not component_ok(S_global, u, E, P, tau, pop_thresh, nb):
                S_global = np.array(w["sub"], dtype=int)[np.array(w["root_component"], dtype=int)].tolist()

            own_mask = np.zeros(n, dtype=bool)
            own_mask[w["sub"]] = True
            allowed_idx = np.where(remaining & (~batch_mask | own_mask))[0]
            S_final = improve_by_trades(S_global, u, E, P, nb, tau, pop_thresh, allowed_idx,
                                         max_iter=200, max_swap_checks=0)
            if not component_ok(S_final, u, E, P, tau, pop_thresh, nb):
                S_final = S_global

            if not component_ok(S_final, u, E, P, tau, pop_thresh, nb):
                # Even an uncapped solve over the entire remaining pool can't
                # reach pop_thresh/tau with this forced component -- it's an
                # isolated leftover pocket, genuinely unbuildable from what's
                # left. Committing it anyway would silently violate pop_thresh,
                # so drop just the forced component from `remaining` (left
                # unassigned, asu_id stays -1) instead, which still guarantees
                # forward progress since this exact root can never be re-picked.
                root_global = np.array(w["sub"], dtype=int)[np.array(w["root_component"], dtype=int)].tolist()
                remaining[root_global] = False
                if verbose:
                    sP0 = int(P[root_global].sum())
                    print(f"  [SKIP] seed={w['seed']}: forced component ({len(root_global)} tract(s), "
                          f"pop={sP0}) cannot reach pop_thresh={pop_thresh} from remaining tracts; "
                          f"leaving unassigned", flush=True)
                return

            k += 1
            asu_id[S_final] = k
            remaining[S_final] = False
            tried[S_final] = False

            if verbose:
                su, sE, sP = int(u[S_final].sum()), int(E[S_final].sum()), int(P[S_final].sum())
                URv = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
                status_ = result.status if result is not None else "GREEDY FALLBACK"
                print(f"  [OK] ASU {k} committed (uncapped, {reason}): "
                      f"tracts={len(S_final)}, pop={sP}, UR={URv:.3f}%, unemp={su} "
                      f"(status={status_})", flush=True)

        if max_nodes_per_asu is not None:
            kept_windows = []
            for w in windows:
                if len(w["root_component"]) <= max_nodes_per_asu:
                    kept_windows.append(w)
                    continue
                if verbose:
                    print(f"  [seed={w['seed']}] forced high-UR component "
                          f"({len(w['root_component'])} tracts) exceeds max_nodes_per_asu "
                          f"({max_nodes_per_asu}); solving this ASU uncapped instead", flush=True)
                _commit_uncapped_window(w, "forced component too large")

            windows = kept_windows
            if k >= max_asus:
                break
            if not windows:
                continue

        # ---- Solve all windows in the batch concurrently ----
        workers_each = max(1, int(workers) // len(windows))

        def _solve(w: Dict) -> Optional[CpsatResult]:
            if verbose:
                print(f"  [seed={w['seed']}] >>> starting CP-SAT solve (nodes={len(w['nb_local'])}, "
                      f"workers={workers_each}, time_limit={time_limit}s)", flush=True)
            # The greedy warm-start hint is built on the full window (cap-unaware),
            # so an oversized hint's objective would become an infeasible hard
            # lower bound once max_nodes_per_asu is enforced in-model; drop it and
            # let CP-SAT search unassisted rather than hand it a false floor.
            hint_local, hint_obj_local = w["hint_improved"], w["hint_obj_val"]
            if (
                max_nodes_per_asu is not None
                and hint_local is not None
                and len(hint_local) > max_nodes_per_asu
            ):
                hint_local, hint_obj_local = None, None
            result = solve_one_asu_cpsat(
                nb_local=w["nb_local"], u_g=w["u_g"], E_g=w["E_g"], P_g=w["P_g"],
                tau=tau, pop_thresh=pop_thresh, root_local=w["root_local"],
                time_limit=time_limit, workers=workers_each, rel_gap=rel_gap, log=verbose,
                hint=hint_local, hint_obj=hint_obj_local,
                forced_selected=w["root_component"],
                deterministic_ties=deterministic_ties,
                tie_break_rank=w["tie_break_rank"],
                objective_shaving=objective_shaving,
                use_root_articulation_implications=use_root_articulation_implications,
                use_signed_flow=use_signed_flow,
                use_arborescence=use_arborescence,
                configure_subsolvers=configure_subsolvers,
                use_tract_first_search=use_tract_first_search,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                max_nodes=max_nodes_per_asu,
                stop_flag_path=stop_flag_path,
                skip_flag_path=skip_flag_path,
                # cluster_groups intentionally NOT passed here: tying high-UR
                # cluster members via equality is provably correct (validated
                # against brute force) but empirically hurts this time-limited
                # heuristic search -- see SKILL.md "Known Issues / Gotchas".
            )
            if verbose:
                status = result.status if result is not None else "NO SOLUTION"
                print(f"  [seed={w['seed']}] <<< solve finished: status={status}", flush=True)
            return result

        if len(windows) > 1:
            with concurrent.futures.ThreadPoolExecutor(max_workers=len(windows)) as pool:
                sols = list(pool.map(_solve, windows))
        else:
            sols = [_solve(windows[0])]

        # ---- Refine each window's result within its own reserved territory ----
        candidates: List[List[int]] = []
        for w, sol in zip(windows, sols):
            if sol is None:
                fallback_ok = w["hint_valid"] and (
                    max_nodes_per_asu is None or len(w["hint_improved"]) <= max_nodes_per_asu
                )
                if fallback_ok:
                    S_local = w["hint_improved"]
                    if verbose:
                        su = int(w["u_g"][np.array(S_local, dtype=int)].sum())
                        print(f"  [seed={w['seed']}] [GREEDY FALLBACK] tracts={len(S_local)}, unemp={su}", flush=True)
                elif max_nodes_per_asu is not None:
                    # The capped solve is infeasible/unsolved for a reason other
                    # than an oversized forced component (e.g. reaching
                    # pop_thresh needs more tracts than the cap allows). Retrying
                    # with a "different" seed would just re-derive this identical
                    # window under full_graph_window, so rescue it uncapped now.
                    if verbose:
                        print(f"  [seed={w['seed']}] capped solve infeasible/no-solution; "
                              f"solving this ASU uncapped instead", flush=True)
                    _commit_uncapped_window(w, "capped solve infeasible")
                    continue
                else:
                    tried[w["seed"]] = True
                    continue
            else:
                S_local = sol.sel_idx_local

            if export_dir is not None:
                _export_window_comparison(w, list(S_local), df, export_dir, k + 1, tau)

            S_global = np.array(w["sub"], dtype=int)[np.array(S_local, dtype=int)].tolist()
            own_mask = np.zeros(n, dtype=bool)
            own_mask[w["sub"]] = True
            allowed_idx = np.where(remaining & (~batch_mask | own_mask))[0]
            S_refined = improve_by_trades(S_global, u, E, P, nb, tau, pop_thresh, allowed_idx,
                                           max_iter=200, max_swap_checks=0, max_size=max_nodes_per_asu)
            if not component_ok(S_refined, u, E, P, tau, pop_thresh, nb):
                S_refined = S_global
            candidates.append(S_refined)

        if not candidates:
            continue

        # ---- Merge candidates that touch (share a queen-contiguity edge) ----
        parent = list(range(len(candidates)))

        def find(a: int) -> int:
            while parent[a] != a:
                parent[a] = parent[parent[a]]
                a = parent[a]
            return a

        def union(a: int, b: int) -> None:
            ra, rb = find(a), find(b)
            if ra != rb:
                parent[ra] = rb

        # With a per-ASU cap active, defer all touching-cluster merges to the
        # dedicated combine phase after the main loop (it re-solves uncapped);
        # merging in-batch here would silently produce over-cap ASUs.
        if merge_adjacent and max_nodes_per_asu is None and len(candidates) > 1:
            owner: Dict[int, int] = {}
            for gi, S in enumerate(candidates):
                for t in S:
                    owner[t] = gi
            for gi, S in enumerate(candidates):
                for t in S:
                    for w2 in nb[t]:
                        gj = owner.get(w2)
                        if gj is not None and gj != gi:
                            union(gi, gj)

        groups: Dict[int, List[int]] = {}
        for gi, S in enumerate(candidates):
            groups.setdefault(find(gi), []).append(gi)

        # Try the merged union first; fall back to each original candidate on the
        # (mathematically unexpected) chance the merged set fails a sanity check.
        final_units: List[List[int]] = []
        for members in groups.values():
            merged = sorted({t for gi in members for t in candidates[gi]})
            su, sE = int(u[merged].sum()), int(E[merged].sum())
            if len(members) > 1 and den * su - num * sE >= 0 and component_ok(merged, u, E, P, tau, pop_thresh, nb):
                if verbose:
                    print(f"  [MERGE] {len(members)} touching windows combined into one ASU ({len(merged)} tracts)", flush=True)
                final_units.append(merged)
            else:
                final_units.extend(candidates[gi] for gi in members)

        # ---- Commit each final unit, largest first, excluding not-yet-processed siblings ----
        final_units.sort(key=lambda S: -int(u[S].sum()))
        pending_mask = np.zeros(n, dtype=bool)
        for S in final_units:
            pending_mask[S] = True

        for S in final_units:
            pending_mask[S] = False
            allowed_idx = np.where(remaining & ~pending_mask)[0]
            S_final = improve_by_trades(S, u, E, P, nb, tau, pop_thresh, allowed_idx,
                                         max_iter=200, max_swap_checks=0, max_size=max_nodes_per_asu)
            if not component_ok(S_final, u, E, P, tau, pop_thresh, nb):
                S_final = S

            k += 1
            asu_id[S_final] = k
            remaining[S_final] = False
            tried[S_final] = False

            if verbose:
                su, sE, sP = int(u[S_final].sum()), int(E[S_final].sum()), int(P[S_final].sum())
                URv = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
                print(f"  [OK] ASU {k} committed: tracts={len(S_final)}, pop={sP}, UR={URv:.3f}%, unemp={su}", flush=True)

            if k >= max_asus:
                break

    # ---- Optional: combine touching capped ASUs, then improve via CP-SAT ----
    # Only relevant when max_nodes_per_asu carved many small clusters; disabled
    # entirely (no-op) unless both the cap and the combine flag are active.
    if max_nodes_per_asu is not None and combine_capped_asus and k > 1:
        committed_ids = np.unique(asu_id[asu_id > 0]).tolist()
        parent2: Dict[int, int] = {cid: cid for cid in committed_ids}

        def find2(a: int) -> int:
            while parent2[a] != a:
                parent2[a] = parent2[parent2[a]]
                a = parent2[a]
            return a

        def union2(a: int, b: int) -> None:
            ra, rb = find2(a), find2(b)
            if ra != rb:
                parent2[ra] = rb

        for t in range(n):
            a = int(asu_id[t])
            if a <= 0:
                continue
            for w2 in nb[t]:
                b = int(asu_id[w2])
                if b > 0 and b != a:
                    union2(a, b)

        merge_groups: Dict[int, List[int]] = {}
        for cid in committed_ids:
            merge_groups.setdefault(find2(cid), []).append(cid)
        groups_to_improve = [members for members in merge_groups.values() if len(members) > 1]

        if verbose and groups_to_improve:
            print(
                f"\n[COMBINE] {len(groups_to_improve)} touching-cluster group(s) found across "
                f"{sum(len(m) for m in groups_to_improve)} capped ASUs; improving via CP-SAT ...",
                flush=True,
            )

        for members in groups_to_improve:
            if _stop_requested(stop_flag_path):
                if verbose:
                    print("[COMBINE] Stop flag detected; halting combine phase.", flush=True)
                break

            group_tracts = np.where(np.isin(asu_id, members))[0]
            su0, sE0 = int(u[group_tracts].sum()), int(E[group_tracts].sum())
            if den * su0 - num * sE0 < 0 or not component_ok(
                group_tracts.tolist(), u, E, P, tau, pop_thresh, nb
            ):
                if verbose:
                    print(f"  [COMBINE] group {sorted(members)} failed sanity check; left as separate ASUs.", flush=True)
                continue

            # Expand one hop into still-unclaimed territory so CP-SAT has room to improve.
            frontier: set = set()
            for t in group_tracts:
                for w2 in nb[int(t)]:
                    if remaining[w2]:
                        frontier.add(int(w2))
            sub = sorted(set(group_tracts.tolist()) | frontier)
            local_index = {g: i for i, g in enumerate(sub)}
            nb_local = [sorted(local_index[h] for h in nb[g] if h in local_index) for g in sub]
            u_g, E_g, P_g = u[sub], E[sub], P[sub]

            group_local = [local_index[int(t)] for t in group_tracts]
            root_local = max(
                group_local,
                key=lambda i: (u_g[i] / max(u_g[i] + E_g[i], 1e-12), P_g[i]),
            )

            if "geoid" in df.columns:
                stable_values = [str(df.iloc[int(g)]["geoid"]) for g in sub]
            else:
                stable_values = [str(int(g)).zfill(12) for g in sub]
            stable_order = sorted(range(len(sub)), key=lambda i: (stable_values[i], i))
            tie_break_rank = [0] * len(sub)
            for rank, local_i in enumerate(stable_order):
                tie_break_rank[local_i] = rank

            hint_obj_val = int(u_g[np.array(group_local, dtype=int)].sum())
            if verbose:
                print(
                    f"  [COMBINE] group {sorted(members)}: {len(group_tracts)} tract(s) merged, "
                    f"window expanded to {len(sub)} tract(s) (+{len(frontier)} unclaimed neighbor(s))",
                    flush=True,
                )

            result = solve_one_asu_cpsat(
                nb_local=nb_local, u_g=u_g, E_g=E_g, P_g=P_g,
                tau=tau, pop_thresh=pop_thresh, root_local=root_local,
                time_limit=time_limit, workers=workers, rel_gap=rel_gap, log=verbose,
                hint=group_local, hint_obj=hint_obj_val,
                deterministic_ties=deterministic_ties,
                tie_break_rank=tie_break_rank,
                objective_shaving=objective_shaving,
                use_root_articulation_implications=use_root_articulation_implications,
                use_signed_flow=use_signed_flow,
                use_arborescence=use_arborescence,
                configure_subsolvers=configure_subsolvers,
                use_tract_first_search=use_tract_first_search,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                # max_nodes intentionally omitted: this phase's purpose is to
                # lift the per-ASU cap now that touching clusters are combined.
                stop_flag_path=stop_flag_path,
                skip_flag_path=skip_flag_path,
            )
            S_local = result.sel_idx_local if result is not None else group_local
            S_global = np.array(sub, dtype=int)[np.array(S_local, dtype=int)].tolist()
            if not component_ok(S_global, u, E, P, tau, pop_thresh, nb):
                S_global = group_tracts.tolist()

            new_id = min(members)
            old_mask = np.isin(asu_id, members)
            asu_id[old_mask] = -1
            remaining[old_mask] = False
            asu_id[S_global] = new_id
            remaining[S_global] = False

            if verbose:
                su2, sE2, sP2 = int(u[S_global].sum()), int(E[S_global].sum()), int(P[S_global].sum())
                URv2 = 100.0 * (0.0 if (su2 + sE2) == 0 else su2 / (su2 + sE2))
                gained = len(S_global) - len(group_tracts)
                status2 = result.status if result is not None else "GREEDY FALLBACK"
                print(
                    f"  [OK] Combined ASU {new_id}: tracts={len(S_global)} (+{gained} new), "
                    f"pop={sP2}, UR={URv2:.3f}%, unemp={su2} (status={status2})",
                    flush=True,
                )

    n_asu_final = int(np.unique(asu_id[asu_id > 0]).size)

    # Final summary if stopped due to no high-UR tracts
    if verbose and k < max_asus:
        rem_idx_final = np.where(remaining)[0]
        if rem_idx_final.size > 0:
            max_ur_remaining = UR[rem_idx_final].max() * 100
            print(f"\nStopped after {n_asu_final} ASUs. Max UR among {rem_idx_final.size} remaining tracts: {max_ur_remaining:.3f}%", flush=True)

    return {"asu_id": asu_id.tolist(), "n_asu": n_asu_final}


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
    ap.add_argument("--rel-gap", type=float, default=None, help="Optional relative gap (e.g., 0.01 for 1%%)")
    ap.add_argument("--parallel-asus", type=int, default=1, help="Number of ASU windows to solve concurrently")
    ap.add_argument("--no-merge-adjacent", action="store_true", help="Disable merging of touching ASUs built in the same batch")
    ap.add_argument(
        "--no-deterministic-ties",
        action="store_true",
        help="Skip secondary optimal-solution tie-break solves",
    )
    ap.add_argument(
        "--use-root-articulation-implications",
        action="store_true",
        help="Add root-based articulation implications to strengthen connectivity",
    )
    ap.add_argument(
        "--use-tract-first-search",
        action="store_true",
        help=(
            "Enable the experimental incumbent-boundary worker, trying safe "
            "exclusions before frontier additions"
        ),
    )
    ap.add_argument(
        "--no-flow-count-envelope",
        action="store_true",
        help="Disable dynamic signed-flow bounds based on selected-node count",
    )
    ap.add_argument(
        "--no-small-root-separators",
        action="store_true",
        help="Disable size-2/3 rooted vertex-separator clauses",
    )
    ap.add_argument("--root-separator-max-size", type=int, default=3)
    ap.add_argument("--root-separator-clause-limit", type=int, default=200)
    ap.add_argument("--root-separator-target-limit", type=int, default=128)
    ap.add_argument(
        "--no-separator-cardinality-bounds",
        action="store_true",
        help="Disable UR-surplus cardinality cuts (sum(x_i in C) <= K_C * z_C) for separator components",
    )
    ap.add_argument("--solution-pool-size", type=int, default=32)
    ap.add_argument(
        "--use-bridge-edge-bounds",
        action="store_true",
        help=(
            "Tighten flow variable domains on graph bridges using a root-rooted "
            "directional bound (reverse direction forced to 0); unproven, opt-in"
        ),
    )
    ap.add_argument(
        "--max-nodes-per-asu",
        type=int,
        default=None,
        help=(
            "Optional hard cap on tracts per ASU during the main build loop; "
            "carves many small non-overlapping clusters instead of fewer large ones"
        ),
    )
    ap.add_argument(
        "--no-combine-capped-asus",
        action="store_true",
        help=(
            "With --max-nodes-per-asu set, skip the final pass that merges "
            "touching capped ASUs and re-solves them uncapped via CP-SAT"
        ),
    )
    ap.add_argument("--output", default=None, help="Output CSV path (default: <stem>_with_asu.csv)")
    ap.add_argument(
        "--stop-file",
        default=None,
        help="Path to a file that, once created, halts the current solve and returns its incumbent",
    )
    ap.add_argument(
        "--skip-file",
        default=None,
        help=(
            "Path to a file that, once created, halts only the in-progress ASU "
            "window (returning its incumbent) and moves on to the next ASU"
        ),
    )
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
        verbose=args.verbose, parallel_asus=args.parallel_asus,
        merge_adjacent=not args.no_merge_adjacent,
        deterministic_ties=not args.no_deterministic_ties,
        use_root_articulation_implications=args.use_root_articulation_implications,
        use_tract_first_search=args.use_tract_first_search,
        use_flow_count_envelope=not args.no_flow_count_envelope,
        use_small_root_separators=not args.no_small_root_separators,
        root_separator_max_size=args.root_separator_max_size,
        root_separator_clause_limit=args.root_separator_clause_limit,
        root_separator_target_limit=args.root_separator_target_limit,
        use_separator_cardinality_bounds=not args.no_separator_cardinality_bounds,
        solution_pool_size=args.solution_pool_size,
        use_bridge_edge_bounds=args.use_bridge_edge_bounds,
        max_nodes_per_asu=args.max_nodes_per_asu,
        combine_capped_asus=not args.no_combine_capped_asus,
        stop_flag_path=args.stop_file,
        skip_flag_path=args.skip_file,
    )

    df_out = df.copy()
    df_out["asu_id"] = out["asu_id"]

    out_path = args.output or f"{os.path.splitext(os.path.basename(inp))[0]}_with_asu.csv"
    df_out.to_csv(out_path, index=False)
    print(f"\nDone. Built {out['n_asu']} ASU(s) → {out_path}")


if __name__ == "__main__":
    main()
