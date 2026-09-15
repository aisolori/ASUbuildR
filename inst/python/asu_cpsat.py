#!/usr/bin/env python3
"""
asu_cpsat.py — Build Areas of Substantial Unemployment (ASUs) with OR-Tools CP-SAT.

Modified to ensure:
1. Eligible high-UR seeds and automatic roots are ranked by exact rate capacity
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
from bisect import bisect_right
import concurrent.futures
import heapq
import json
import math
import os
import threading
import time
from dataclasses import dataclass
from fractions import Fraction
from typing import Callable, List, Optional, Dict, Sequence, Tuple, Set

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

# Optional (C-backed graph algorithms; falls back to pure-Python if absent)
try:
    import igraph as _ig
except Exception:
    _ig = None


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


def _capacity_root_order(candidates, u, E, P, tau):
    """Highest exact rate capacity first; ties use population, then tract index."""
    num, den = as_fraction_tau(tau)
    return sorted(
        map(int, candidates),
        key=lambda node: (den * int(u[node]) - num * int(E[node]),
                          int(P[node]), -node),
        reverse=True,
    )


def _polish_asu_order(asu_id, u, E, tau):
    """Highest aggregate signed surplus first, then unemployment and ASU ID."""
    num, den = as_fraction_tau(tau)
    ids = np.unique(asu_id[asu_id > 0]).astype(int).tolist()
    def priority(asu_number):
        nodes = np.flatnonzero(asu_id == asu_number)
        unemployed = sum(int(u[node]) for node in nodes)
        employed = sum(int(E[node]) for node in nodes)
        return (-(den * unemployed - num * employed), -unemployed, asu_number)
    return sorted(ids, key=priority)


def _pick_capacity_root(candidates, u, E, P, tau):
    """Choose a root only from the caller's eligible/current tracts."""
    ordered = _capacity_root_order(candidates, u, E, P, tau)
    if not ordered:
        raise ValueError("root selection requires at least one eligible tract")
    return ordered[0]


def _add_capacity_root_order(model, x, roots, u, E, P, tau, *, hint=None, prefix="root"):
    """Make the highest-capacity selected tract the variable-root flow source.

    This chooses a representation of each connected selection without forcing
    inclusion of the highest-capacity tract in the entire search window.
    """
    previous = 0
    hinted = set(hint) if hint is not None else None
    hinted_seen = False
    for i in _capacity_root_order(range(len(x)), u, E, P, tau):
        seen = model.NewBoolVar(f"{prefix}_prefix_{i}")
        model.Add(roots[i] <= x[i])
        model.Add(seen == previous + roots[i])
        model.Add(x[i] <= seen)
        if hinted is not None:
            hinted_seen = hinted_seen or i in hinted
            model.AddHint(seen, int(hinted_seen))
        previous = seen


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


def _partition_standalone_expansion_territories(
    standalone_units: Sequence[Sequence[int]],
    nb: List[List[int]],
    allowed: np.ndarray,
    u: Optional[np.ndarray] = None,
) -> List[List[int]]:
    """Assign reachable allowed tracts to the nearest standalone ASU seed.

    Ties (equal graph distance to two or more seeds) go to the unit with the
    most unemployment already captured in its own seed; `unit_index` is only
    the final, fully-deterministic tiebreaker.
    """
    n = len(nb)
    allowed_mask = np.asarray(allowed, dtype=bool)
    if allowed_mask.size != n:
        raise ValueError("allowed mask must match the adjacency size")

    if u is None:
        unit_priority = [0] * len(standalone_units)
    else:
        # Negated so a lower tuple value (min-heap win) means more unemployment.
        unit_priority = [-int(u[np.array(nodes, dtype=int)].sum()) for nodes in standalone_units]

    owner = np.full(n, -1, dtype=int)
    distance = np.full(n, np.iinfo(np.int32).max, dtype=np.int64)
    queue: List[Tuple[int, int, int, int]] = []
    for unit_index, nodes in enumerate(standalone_units):
        for raw_node in nodes:
            node = int(raw_node)
            if not (0 <= node < n) or not allowed_mask[node]:
                continue
            if owner[node] >= 0 and owner[node] != unit_index:
                raise ValueError("standalone ASU seeds must be disjoint")
            owner[node] = unit_index
            distance[node] = 0
            heapq.heappush(queue, (0, unit_priority[unit_index], unit_index, node))

    while queue:
        node_distance, _, unit_index, node = heapq.heappop(queue)
        if distance[node] != node_distance or owner[node] != unit_index:
            continue
        for neighbor in nb[node]:
            if not allowed_mask[neighbor]:
                continue
            candidate = (node_distance + 1, unit_priority[unit_index], unit_index)
            current_owner = int(owner[neighbor])
            current = (
                int(distance[neighbor]),
                unit_priority[current_owner] if current_owner >= 0 else 0,
                current_owner,
            )
            if candidate >= current:
                continue
            distance[neighbor] = node_distance + 1
            owner[neighbor] = unit_index
            heapq.heappush(
                queue,
                (node_distance + 1, unit_priority[unit_index], unit_index, int(neighbor)),
            )

    return [
        np.flatnonzero(owner == unit_index).astype(int).tolist()
        for unit_index in range(len(standalone_units))
    ]


def _merge_touching_asu_units(
    units: Sequence[Sequence[int]],
    nb: List[List[int]],
    max_nodes: Optional[int] = None,
) -> Tuple[List[List[int]], int]:
    """Merge touching disjoint ASU units while respecting an optional size cap."""
    normalized = [sorted({int(node) for node in unit}) for unit in units]
    owner: Dict[int, int] = {}
    for unit_index, nodes in enumerate(normalized):
        for node in nodes:
            if node in owner:
                raise ValueError("ASU units must be disjoint before merging")
            owner[node] = unit_index

    parent = list(range(len(normalized)))
    sizes = [len(nodes) for nodes in normalized]

    def find(unit_index: int) -> int:
        while parent[unit_index] != unit_index:
            parent[unit_index] = parent[parent[unit_index]]
            unit_index = parent[unit_index]
        return unit_index

    touching_pairs = sorted({
        (min(unit_index, owner[neighbor]), max(unit_index, owner[neighbor]))
        for node, unit_index in owner.items()
        for neighbor in nb[node]
        if neighbor in owner and owner[neighbor] != unit_index
    })
    merges = 0
    for left, right in touching_pairs:
        left_root, right_root = find(left), find(right)
        if left_root == right_root:
            continue
        if (
            max_nodes is not None
            and sizes[left_root] + sizes[right_root] > int(max_nodes)
        ):
            continue
        if left_root > right_root:
            left_root, right_root = right_root, left_root
        parent[right_root] = left_root
        sizes[left_root] += sizes[right_root]
        merges += 1

    groups: Dict[int, List[int]] = {}
    for unit_index in range(len(normalized)):
        groups.setdefault(find(unit_index), []).append(unit_index)
    merged = [
        sorted({
            node
            for unit_index in member_indices
            for node in normalized[unit_index]
        })
        for _, member_indices in sorted(
            groups.items(), key=lambda item: min(item[1])
        )
    ]
    return merged, merges


def _asu_units_touch(
    left: Sequence[int],
    right: Sequence[int],
    nb: List[List[int]],
) -> bool:
    """Return whether two tract sets overlap or share a graph edge."""
    left_set = {int(node) for node in left}
    right_set = {int(node) for node in right}
    if left_set.intersection(right_set):
        return True
    return any(
        int(neighbor) in right_set
        for node in left_set
        for neighbor in nb[node]
    )


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
        seed = _pick_capacity_root(rem, u_g, E_g, P_g, tau)
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


_igraph_cache = threading.local()


def _get_igraph_graph(nb_local: List[List[int]]):
    """Build (or reuse) a cached igraph.Graph mirroring `nb_local`.

    Cached per-thread and keyed by object identity (not content) so concurrent
    ASU solves (parallel_asus > 1) never share a cache entry, and holding a
    strong reference to `nb_local` guarantees `id()` can't be reused for a
    different adjacency list while the cache entry is alive.
    """
    cached = getattr(_igraph_cache, "entry", None)
    if cached is not None and cached[0] is nb_local:
        return cached[1]
    edges = [
        (i, w) for i, neighbors in enumerate(nb_local) for w in neighbors if w > i
    ]
    graph = _ig.Graph(n=len(nb_local), edges=edges)
    _igraph_cache.entry = (nb_local, graph)
    return graph


def _articulation_points_igraph(nb_local: List[List[int]], selected: np.ndarray) -> set:
    node_ids = np.flatnonzero(selected)
    if node_ids.size < 2:
        return set()
    sub = _get_igraph_graph(nb_local).induced_subgraph(node_ids.tolist())
    return {int(node_ids[i]) for i in sub.articulation_points()}


def _connected_components_igraph(
    nb_local: List[List[int]], selected: np.ndarray,
) -> List[List[int]]:
    node_ids = np.flatnonzero(selected)
    if node_ids.size == 0:
        return []
    sub = _get_igraph_graph(nb_local).induced_subgraph(node_ids.tolist())
    return [
        [int(node_ids[i]) for i in members]
        for members in sub.connected_components(mode="weak")
    ]


def _connected_components_python(
    nb_local: List[List[int]], selected: np.ndarray,
) -> List[List[int]]:
    N = len(nb_local)
    seen = np.zeros(N, dtype=bool)
    components: List[List[int]] = []
    for start in range(N):
        if not selected[start] or seen[start]:
            continue
        seen[start] = True
        component = [start]
        stack = [start]
        while stack:
            node = stack.pop()
            for neighbor in nb_local[node]:
                if selected[neighbor] and not seen[neighbor]:
                    seen[neighbor] = True
                    component.append(neighbor)
                    stack.append(neighbor)
        components.append(component)
    return components


def _connected_components(
    nb_local: List[List[int]], selected: np.ndarray,
) -> List[List[int]]:
    """Connected components of the induced subgraph on `selected` nodes.

    Same igraph-first/pure-Python-fallback dispatch as `_articulation_points`.
    """
    if _ig is not None:
        return _connected_components_igraph(nb_local, selected)
    return _connected_components_python(nb_local, selected)


def _articulation_points_python(nb_local: List[List[int]], selected: np.ndarray) -> set:
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


def _articulation_points(nb_local: List[List[int]], selected: np.ndarray) -> set:
    """Cut vertices of the induced subgraph on `selected` nodes.

    Uses igraph's C-backed implementation when available (much faster on
    large graphs, since this is called repeatedly per-node-removal in several
    hot repair/pruning loops); falls back to the pure-Python iterative Tarjan
    finder otherwise. Both return identical results (cross-checked against
    500 randomized graphs).
    """
    if _ig is not None:
        return _articulation_points_igraph(nb_local, selected)
    return _articulation_points_python(nb_local, selected)


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


def _get_igraph_separator_graph(nb_local: List[List[int]]):
    """Build (or reuse) the cached vertex-split directed graph used by
    `_bounded_root_vertex_separator_igraph`. Each node is split into an
    "in" copy (2*i) and "out" copy (2*i+1); every root-target pair reuses
    the same graph topology and only the per-call capacity list changes,
    so this (like `_get_igraph_graph`) is cached per-thread and keyed by
    object identity.
    """
    cached = getattr(_igraph_cache, "sep_entry", None)
    if cached is not None and cached[0] is nb_local:
        return cached[1], cached[2]
    N = len(nb_local)
    edges = [(2 * i, 2 * i + 1) for i in range(N)]
    undirected_edges = sorted({
        (min(node, neighbor), max(node, neighbor))
        for node, neighbors in enumerate(nb_local)
        for neighbor in neighbors
        if node != neighbor
    })
    for left, right in undirected_edges:
        edges.append((2 * left + 1, 2 * right))
        edges.append((2 * right + 1, 2 * left))
    graph = _ig.Graph(n=2 * N, edges=edges, directed=True)
    _igraph_cache.sep_entry = (nb_local, graph, N)
    return graph, N


def _bounded_root_vertex_separator_igraph(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    max_size: int,
) -> Optional[Tuple[int, ...]]:
    """igraph-backed equivalent of `_bounded_root_vertex_separator_python`.

    Reuses the same vertex-split reduction (see that function's docstring)
    but hands the resulting capacitated digraph to igraph's C-backed
    `Graph.mincut()` instead of a hand-rolled Edmonds-Karp loop. Edge
    capacity `cutoff = max_size + 1` is used in place of true infinity,
    which is safe here: any cut using one such edge already costs more
    than `max_size`, so it can never underbid a genuine vertex-only
    separator of size <= max_size (the only range this function reports).
    """
    if target == root_local or max_size < 2:
        return None
    cutoff = int(max_size) + 1
    graph, N = _get_igraph_separator_graph(nb_local)
    num_internal = N
    capacity = [1] * num_internal + [cutoff] * (graph.ecount() - num_internal)
    capacity[root_local] = cutoff
    capacity[target] = cutoff
    source = 2 * root_local + 1
    sink = 2 * target
    cut = graph.mincut(source=source, target=sink, capacity=capacity)
    flow = int(round(cut.value))
    if flow < 2 or flow > max_size:
        return None
    source_side = set(cut.partition[0] if source in cut.partition[0] else cut.partition[1])
    separator = tuple(
        sorted(
            node for node in range(N)
            if node not in (root_local, target)
            and (2 * node) in source_side
            and (2 * node + 1) not in source_side
        )
    )
    return separator if 2 <= len(separator) <= max_size else None


def _bounded_root_vertex_separator_python(
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


def _bounded_root_vertex_separator(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    max_size: int,
) -> Optional[Tuple[int, ...]]:
    """Minimum root-target vertex separator when its size is 2..max_size.

    Uses igraph's C-backed `mincut()` on the same vertex-split reduction
    when available, falling back to the hand-rolled bounded Edmonds-Karp
    search otherwise. Both report a minimum separator of the same size;
    when several minimum separators tie in size, the specific vertex set
    returned can differ between backends (any such set is equally valid/
    sound as a `x_i <= OR(x_s for s in separator)` clause).
    """
    if _ig is not None:
        return _bounded_root_vertex_separator_igraph(nb_local, root_local, target, max_size)
    return _bounded_root_vertex_separator_python(nb_local, root_local, target, max_size)


def _minimum_root_vertex_separator_igraph(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    protected_nodes: Optional[Set[int]] = None,
    max_size: Optional[int] = None,
) -> Optional[Tuple[int, ...]]:
    """Return a minimum root-target vertex separator, optionally avoiding protected nodes."""
    if target == root_local:
        return None
    graph, n_nodes = _get_igraph_separator_graph(nb_local)
    cutoff = int(max_size) + 1 if max_size is not None else n_nodes + 1
    num_internal = n_nodes
    capacity = [1] * num_internal + [cutoff] * (graph.ecount() - num_internal)

    blocked = {int(root_local), int(target)}
    if protected_nodes:
        blocked.update(int(node) for node in protected_nodes if 0 <= int(node) < n_nodes)
    for node in blocked:
        capacity[node] = cutoff

    source = 2 * root_local + 1
    sink = 2 * target
    cut = graph.mincut(source=source, target=sink, capacity=capacity)
    flow = int(round(cut.value))
    if flow <= 0:
        return None
    if max_size is not None and flow > int(max_size):
        return None

    source_side = set(cut.partition[0] if source in cut.partition[0] else cut.partition[1])
    separator = tuple(
        sorted(
            node for node in range(n_nodes)
            if node not in (root_local, target)
            and (2 * node) in source_side
            and (2 * node + 1) not in source_side
        )
    )
    if not separator:
        return None
    if protected_nodes and any(node in protected_nodes for node in separator):
        return None
    return separator


def _minimum_root_vertex_separator_python(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    protected_nodes: Optional[Set[int]] = None,
    max_size: Optional[int] = None,
) -> Optional[Tuple[int, ...]]:
    """Pure-Python minimum root-target separator with optional protected nodes."""
    n_nodes = len(nb_local)
    if target == root_local:
        return None

    cutoff = int(max_size) + 1 if max_size is not None else n_nodes + 1
    residual: List[Dict[int, int]] = [dict() for _ in range(2 * n_nodes)]

    def _add_arc(start: int, end: int, capacity: int) -> None:
        residual[start][end] = residual[start].get(end, 0) + capacity
        residual[end].setdefault(start, 0)

    blocked = {int(root_local), int(target)}
    if protected_nodes:
        blocked.update(int(node) for node in protected_nodes if 0 <= int(node) < n_nodes)

    for node in range(n_nodes):
        capacity = cutoff if node in blocked else 1
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
        parent = [-1] * (2 * n_nodes)
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

    if flow <= 0:
        return None
    if max_size is not None and flow > int(max_size):
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
        node for node in range(n_nodes)
        if node not in (root_local, target)
        and 2 * node in reachable
        and 2 * node + 1 not in reachable
    )
    if not separator:
        return None
    if protected_nodes and any(node in protected_nodes for node in separator):
        return None
    return separator


def _minimum_root_vertex_separator(
    nb_local: List[List[int]],
    root_local: int,
    target: int,
    protected_nodes: Optional[Set[int]] = None,
    max_size: Optional[int] = None,
) -> Optional[Tuple[int, ...]]:
    """Dispatch to igraph or Python minimum separator implementation."""
    if _ig is not None:
        return _minimum_root_vertex_separator_igraph(
            nb_local,
            root_local,
            target,
            protected_nodes=protected_nodes,
            max_size=max_size,
        )
    return _minimum_root_vertex_separator_python(
        nb_local,
        root_local,
        target,
        protected_nodes=protected_nodes,
        max_size=max_size,
    )


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


def _asu_flow_branch_order(
    edges: Sequence[Tuple[int, int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
) -> List[int]:
    """Order flow edges by incident tract UR, then unemployment count."""
    node_priority = []
    for node in range(len(u_g)):
        unemployed = int(u_g[node])
        labor_force = unemployed + int(E_g[node])
        unemployment_rate = (
            Fraction(unemployed, labor_force)
            if labor_force > 0 else Fraction(0, 1)
        )
        node_priority.append((unemployment_rate, unemployed))

    def edge_priority(edge_index: int) -> Tuple:
        left, right = edges[edge_index]
        primary, secondary = sorted(
            (node_priority[left], node_priority[right]),
            reverse=True,
        )
        return primary + secondary

    return sorted(range(len(edges)), key=edge_priority, reverse=True)


def _root_graph_distances(
    nb_local: List[List[int]],
    root_local: int,
) -> List[int]:
    """Shortest-path distances from root on the local graph."""
    n = len(nb_local)
    distances = [n] * n
    if not (0 <= int(root_local) < n):
        return distances

    distances[int(root_local)] = 0
    queue = [int(root_local)]
    head = 0
    while head < len(queue):
        node = queue[head]
        head += 1
        next_distance = distances[node] + 1
        for neighbor in nb_local[node]:
            if distances[neighbor] <= next_distance:
                continue
            distances[neighbor] = next_distance
            queue.append(int(neighbor))
    return distances


def _asu_tract_capacity_orders(
    u_g: np.ndarray,
    E_g: np.ndarray,
    num: int,
    den: int,
) -> Tuple[List[int], List[int]]:
    """Order tracts by exact UR-surplus sign and magnitude."""
    q_surplus = den * u_g.astype(np.int64) - num * E_g.astype(np.int64)

    select_order = sorted(
        (int(i) for i in range(len(q_surplus)) if q_surplus[i] >= 0),
        key=lambda i: (
            int(q_surplus[i]),
            int(u_g[i]),
            -i,
        ),
        reverse=True,
    )
    reject_order = sorted(
        (int(i) for i in range(len(q_surplus)) if q_surplus[i] < 0),
        key=lambda i: (
            int(q_surplus[i]),
            int(u_g[i]),
            i,
        ),
    )
    return select_order, reject_order


_ASU_HYBRID_PREFIX_SIZE = 256


def _asu_flow_capacity_hybrid_groups(
    edges: Sequence[Tuple[int, int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    num: int,
    den: int,
    root_distances: Sequence[int],
    max_prefix: int = _ASU_HYBRID_PREFIX_SIZE,
) -> Tuple[List[int], List[int], List[int], List[Tuple[str, int]]]:
    """Build hybrid branching groups: flow prefix, capacity prefix, far tail."""
    max_prefix = max(0, int(max_prefix))
    flow_order = _asu_flow_branch_order(edges, u_g, E_g)
    flow_prefix = flow_order[:max_prefix]

    select_order, reject_order = _asu_tract_capacity_orders(u_g, E_g, num, den)
    select_budget = (max_prefix + 1) // 2
    reject_budget = max_prefix // 2
    select_prefix = select_order[:select_budget]
    reject_prefix = reject_order[:reject_budget]

    used_flows = set(flow_prefix)
    used_tracts = set(select_prefix) | set(reject_prefix)

    far_items: List[Tuple[str, int, int]] = []
    for tract_index in range(len(u_g)):
        if tract_index in used_tracts:
            continue
        distance = (
            int(root_distances[tract_index])
            if tract_index < len(root_distances)
            else len(root_distances)
        )
        far_items.append(("tract", int(tract_index), distance))

    for edge_index, (left, right) in enumerate(edges):
        if edge_index in used_flows:
            continue
        left_distance = (
            int(root_distances[left]) if left < len(root_distances)
            else len(root_distances)
        )
        right_distance = (
            int(root_distances[right]) if right < len(root_distances)
            else len(root_distances)
        )
        far_items.append(("flow", int(edge_index), max(left_distance, right_distance)))

    far_items.sort(
        key=lambda item: (
            -item[2],
            0 if item[0] == "tract" else 1,
            -item[1],
        )
    )
    far_order = [(kind, index) for kind, index, _ in far_items]
    return flow_prefix, select_prefix, reject_prefix, far_order


_ASU_FULL_SUBSOLVER_PATTERN = (
    "portfolio_max_lp",
    "portfolio_max_lp",
    "max_lp",
    "lb_tree_search",
    "objective_lb_search_max_lp",

    "asu_probe_standard",
    "quick_restart_no_lp",
    "variables_shaving",
    "asu_probe_deep",

    "variables_shaving_max_lp",
    "variables_shaving_no_lp",
    "objective_shaving_max_lp",
    "max_lp",
    "reduced_costs",

    "pseudo_costs",
    "max_lp",
    "core_max_lp",
    "core",
    "objective_shaving_no_lp",
    "asu_probe_mega_deep"
)

# Every entry here remains applicable when a model has no objective. Keeping a
# separate list matters because CP-SAT removes objective-only entries and then
# pads back to num_full_subsolvers by cloning the first surviving entry.
_ASU_FEASIBILITY_SUBSOLVER_PATTERN = (
    "portfolio_max_lp",
    "quick_restart_no_lp",
    "asu_probe_fast",
    "core_max_lp",
    "max_lp",
    "asu_probe_deep",
    "portfolio_no_lp",
    "quick_restart_max_lp",
    "asu_probe_standard",
    "asu_probe_very_deep",
    "variables_shaving"
)


def _asu_full_subsolvers(
    workers: int,
    use_tract_first_search: bool = False,
    use_flow_first_search: bool = False,
    use_tract_capacity_search: bool = False,
    use_flow_capacity_hybrid_search: bool = False,
    use_tract_first_probing: bool = False,
    has_objective: bool = True,
) -> List[str]:
    """Return the bounded full-problem portfolio for one ASU solve."""
    workers = max(1, int(workers))
    if workers < 6:
        full_budget = workers
    else:
        full_budget = max(6, min(20, round(workers * .5)))
    pattern = (
        _ASU_FULL_SUBSOLVER_PATTERN
        if has_objective
        else _ASU_FEASIBILITY_SUBSOLVER_PATTERN
    )
    full_subsolvers: List[str] = list(pattern[:full_budget])
    custom_modes = [
        bool(use_tract_first_search),
        bool(use_flow_first_search),
        bool(use_tract_capacity_search),
        bool(use_flow_capacity_hybrid_search),
    ]
    if sum(custom_modes) > 1:
        raise ValueError("custom fixed-search workers are mutually exclusive")

    custom_worker_name = None
    if use_tract_first_search:
        custom_worker_name = "asu_tract_first"
    elif use_flow_first_search:
        custom_worker_name = "asu_flow_first"
    elif use_tract_capacity_search:
        custom_worker_name = "asu_tract_capacity"
    elif use_flow_capacity_hybrid_search:
        custom_worker_name = "asu_flow_capacity_hybrid"

    if custom_worker_name is not None:
        # Preserve reduced-cost and pseudo-cost search. At large budgets, use
        # one of the duplicate max-LP slots for the custom worker instead.
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
        full_subsolvers[replace_index] = custom_worker_name
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
        "probing_num_combinations_limit": 20_000,

        #"linearization_level": 0,
        "add_lp_constraints_lazily": False,
        "max_cut_rounds_at_level_zero": 10,
    }

    
    variants = [
        ("asu_probe_fast",       25_000, 0.05, False),
        ("asu_probe_standard", 50_000, 0.01, False),
        ("asu_probe_deep", 100_000, 0.01, False),
        ("asu_probe_very_deep", 200_000, 0.01, False),
        ("asu_probe_mega_deep", 500_000, 0.01, False),
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


def _configure_asu_shared_tree(
    params,
    workers: int,
    tract_first: bool = False,
) -> None:
    """Configure coordinated shared-tree proof workers."""

    # Use up to 3 workers, while always leaving at least one worker
    # for the rest of the portfolio.
    params.shared_tree_num_workers = min(
        3,
        max(0, int(workers) - 1),
    )

    params.shared_tree_split_strategy = (
        type(params).SPLIT_STRATEGY_OBJECTIVE_LB
    )

    if tract_first:
        _append_asu_subsolver_params(
            params,
            "shared_tree",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
        )


def _configure_asu_solver_portfolio(
    params,
    workers: int,
    *,
    use_tract_first_search: bool = False,
    use_flow_first_search: bool = False,
    use_tract_capacity_search: bool = False,
    use_flow_capacity_hybrid_search: bool = False,
    has_objective: bool = True,
) -> None:
    """Install the ASU portfolio on every CP-SAT solver invocation."""
    workers = max(1, int(workers))
    params.extra_subsolvers.clear()
    params.subsolvers.clear()
    params.filter_subsolvers.clear()

    tract_first_probing = (
        use_tract_first_search and _supports_tract_first_probing(params)
    )
    if use_flow_first_search:
        _append_asu_subsolver_params(
            params,
            "asu_flow_first",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
            linearization_level=2,
            root_lp_iterations=25_000,
            add_lp_constraints_lazily=False,
            max_cut_rounds_at_level_zero=10,
        )
    if use_flow_capacity_hybrid_search:
        _append_asu_subsolver_params(
            params,
            "asu_flow_capacity_hybrid",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
            linearization_level=2,
            root_lp_iterations=25_000,
            add_lp_constraints_lazily=False,
            max_cut_rounds_at_level_zero=10,
        )
    if use_tract_first_search:
        _append_asu_subsolver_params(
            params,
            "asu_tract_first",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
            linearization_level=2,
        )
    if use_tract_capacity_search:
        _append_asu_subsolver_params(
            params,
            "asu_tract_capacity",
            search_branching=cp_model.PARTIAL_FIXED_SEARCH,
            linearization_level=2,
        )

    _append_asu_subsolver_params(
        params,
        "lb_tree_search",
        save_lp_basis_in_lb_tree_search=True,
        max_cut_rounds_at_level_zero=4,
        add_objective_cut=True,
        root_lp_iterations=100_000,
    )
    _configure_asu_lp_search_params(params)
    _configure_asu_probe_variants(
        params,
        tract_first=tract_first_probing,
    )
    _configure_asu_pseudo_costs(params)

    full_subsolvers = _asu_full_subsolvers(
        workers,
        use_tract_first_search=use_tract_first_search,
        use_flow_first_search=use_flow_first_search,
        use_tract_capacity_search=use_tract_capacity_search,
        use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
        use_tract_first_probing=tract_first_probing,
        has_objective=has_objective,
    )
    params.subsolvers.extend(full_subsolvers)
    params.num_full_subsolvers = len(full_subsolvers)
    params.filter_subsolvers.extend(list(dict.fromkeys(
        full_subsolvers + [
            "rins*",
            "lb_relax_lns",
            "graph_arc_lns",
            "graph_var_lns",
            "graph_cst_lns",
            "rnd_var_lns",
            "rnd_cst_lns",
            "ls*",
        ]
    )))


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
    if root_local not in hint_set:
        return {}
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
    if len(parent) != len(hint_set):
        return {}
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
                 tau: float, pop_thresh: int, nb: List[List[int]], *,
                 required: Optional[Sequence[int]] = None,
                 max_nodes: Optional[int] = None,
                 exact_nodes: Optional[int] = None) -> bool:
    """Validate original-tract selections using the model's exact rate rule."""
    if not S:
        return False
    Sset = set(S)
    if (len(Sset) != len(S) or any(i < 0 or i >= len(nb) for i in Sset)
            or not set(required or []).issubset(Sset)):
        return False
    if exact_nodes is not None:
        if len(S) != int(exact_nodes):
            return False
    elif max_nodes is not None and len(S) > int(max_nodes):
        return False
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
    num, den = as_fraction_tau(tau)
    return (sP >= int(pop_thresh) and den * su - num * sE >= 0
            and (tau <= 0 or su + sE > 0))


def _add_asu_feasibility_constraints(
    model, x, u, emp, pop, tau, pop_thresh, *,
    tract_counts=None, max_nodes=None, exact_nodes=None, active=1,
):
    """Shared nonempty/population/rate/count rows for every ASU model.

    tract_counts maps contracted variables back to original tract counts.
    Connectivity and required/overlap nodes are supplied by each caller.
    Optional groups may supply an active literal; callers must also force all
    selection variables to zero when inactive (the root-order rows do this).
    """
    num, den = as_fraction_tau(tau)
    population = sum(int(pop[i]) * var for i, var in enumerate(x))
    surplus = sum((den * int(u[i]) - num * int(emp[i])) * var
                  for i, var in enumerate(x))
    model.Add(sum(x) >= active)
    model.Add(population >= int(pop_thresh) * active)
    model.Add(surplus >= 0)
    if tau > 0:
        model.Add(sum((int(u[i]) + int(emp[i])) * var
                      for i, var in enumerate(x)) >= active)
    if exact_nodes is not None or max_nodes is not None:
        weights = tract_counts if tract_counts is not None else [1] * len(x)
        count = sum(int(weights[i]) * var for i, var in enumerate(x))
        if exact_nodes is not None:
            model.Add(count == int(exact_nodes) * active)
        else:
            model.Add(count <= int(max_nodes))
    return population, surplus


_FEASIBILITY_CACHE_LOCK = threading.Lock()


def _connectivity_free_feasibility(u, emp, pop, tau, pop_thresh, *,
                                   seconds=5, workers=1, required=None,
                                   overlap=None, max_nodes=None, exact_nodes=None,
                                   stop_path=None, cache=None):
    """Necessary ASU conditions on whole tracts; only INFEASIBLE rejects an area."""
    if seconds <= 0 or _stop_requested(stop_path):
        return "UNKNOWN"
    # Run-local certificates only; unknown/invalid results are never cached.
    cache_key = None
    if cache is not None:
        cache_key = (
            tuple(map(int, u)), tuple(map(int, emp)), tuple(map(int, pop)),
            as_fraction_tau(tau), bool(tau > 0), int(pop_thresh),
            tuple(sorted(set(required or []))),
            None if overlap is None else tuple(sorted(set(overlap))),
            max_nodes, exact_nodes,
        )
        with _FEASIBILITY_CACHE_LOCK:
            if cache_key in cache:
                return cache[cache_key]
    model = cp_model.CpModel()
    x = [model.NewBoolVar(f"screen_{i}") for i in range(len(u))]
    _add_asu_feasibility_constraints(
        model, x, u, emp, pop, tau, pop_thresh,
        max_nodes=max_nodes, exact_nodes=exact_nodes,
    )
    for i in required or []:
        model.Add(x[int(i)] == 1)
    if overlap is not None:
        model.Add(sum(x[int(i)] for i in overlap) >= 1)
    solver = cp_model.CpSolver()
    solver.parameters.max_time_in_seconds = min(5.0, float(seconds))
    solver.parameters.num_search_workers = int(workers)
    _configure_asu_solver_portfolio(
        solver.parameters, workers, has_objective=False
    )
    # This model has no objective: stop as soon as feasibility is established.
    status = solver.Solve(model)
    status_name = solver.StatusName(status)
    if cache is not None and status_name in ("OPTIMAL", "FEASIBLE", "INFEASIBLE"):
        with _FEASIBILITY_CACHE_LOCK:
            if cache_key not in cache and len(cache) >= 32:
                cache.pop(next(iter(cache)), None)
            cache[cache_key] = status_name
    return status_name


def _search_unassigned_asu(nodes, nb, u, emp, pop, tau, pop_thresh,
                           seconds, workers, stop_path=None, donor_nodes=None,
                           max_nodes=None, exact_nodes=None, skip_path=None):
    """Search all supplied tracts with a variable root; INFEASIBLE is a proof.

    Donor repair maximizes retained unemployment and must retain a donor tract.
    Otherwise this is a feasibility search, without an objective or fixed root.
    """
    if _stop_requested(stop_path):
        return [], "STOPPED"
    if _stop_requested(skip_path):
        _consume_flag(skip_path)
        return [], "SKIPPED"
    if seconds <= 0:
        return [], "DISABLED"
    deadline = time.monotonic() + float(seconds)
    nodes = sorted(set(nodes))
    n = len(nodes)
    if not n:
        return [], "INFEASIBLE"
    index = {g: i for i, g in enumerate(nodes)}
    screen_status = _connectivity_free_feasibility(
        u[nodes], emp[nodes], pop[nodes], tau, pop_thresh,
        seconds=min(5.0, float(seconds) * 0.1), workers=workers,
        overlap=None if donor_nodes is None else [index[g] for g in donor_nodes if g in index],
        max_nodes=max_nodes, exact_nodes=exact_nodes, stop_path=stop_path,
    )
    if screen_status == "INFEASIBLE":
        print(f"[CONNECTIVITY-FREE SCREEN] {n} tracts: infeasible; skipping connected search", flush=True)
        return [], "INFEASIBLE"
    model = cp_model.CpModel()
    x = [model.NewBoolVar(f"selected_{i}") for i in range(n)]
    roots = [model.NewBoolVar(f"root_{i}") for i in range(n)]
    model.Add(sum(roots) == 1)
    _add_capacity_root_order(model, x, roots, u[nodes], emp[nodes], pop[nodes], tau)
    net = [[] for _ in nodes]
    injections = []
    for i, g in enumerate(nodes):
        injected = model.NewIntVar(0, n, f"injected_{i}")
        model.Add(injected <= n * roots[i])
        injections.append(injected)
        for v in sorted(set(nb[g])):
            if v not in index or i >= index[v]:
                continue
            j = index[v]
            flow = model.NewIntVar(-n, n, f"flow_{i}_{j}")
            model.Add(flow == 0).OnlyEnforceIf(x[i].Not())
            model.Add(flow == 0).OnlyEnforceIf(x[j].Not())
            net[i].append(flow)
            net[j].append(-flow)
    for i in range(n):
        model.Add(sum(net[i]) == injections[i] - x[i])
    model.Add(sum(injections) == sum(x))
    _add_asu_feasibility_constraints(
        model, x, u[nodes], emp[nodes], pop[nodes], tau, pop_thresh,
        max_nodes=max_nodes, exact_nodes=exact_nodes,
    )
    if donor_nodes is not None:
        donor = set(donor_nodes)
        model.Add(sum(x[i] for i, g in enumerate(nodes) if g in donor) >= 1)
        model.Maximize(sum(int(u[g]) * x[i] for i, g in enumerate(nodes)))
    solver = cp_model.CpSolver()
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        return [], "UNKNOWN"
    solver.parameters.max_time_in_seconds = remaining
    solver.parameters.num_search_workers = int(workers)
    _configure_asu_solver_portfolio(
        solver.parameters,
        workers,
        has_objective=donor_nodes is not None,
    )
    done = threading.Event()

    def watch():
        while not done.wait(0.1):
            if _stop_requested(stop_path) or _stop_requested(skip_path):
                _consume_flag(skip_path)
                solver.StopSearch()
                return

    watcher = threading.Thread(target=watch, daemon=True)
    watcher.start()
    try:
        status = solver.Solve(model) if not _stop_requested(stop_path) else cp_model.UNKNOWN
    finally:
        done.set()
        watcher.join()
    selected = []
    if status in (cp_model.FEASIBLE, cp_model.OPTIMAL):
        selected = [g for i, g in enumerate(nodes) if solver.BooleanValue(x[i])]
        if not component_ok(selected, u, emp, pop, tau, pop_thresh, nb,
                            max_nodes=max_nodes, exact_nodes=exact_nodes):
            raise ValueError("Residual search returned an invalid ASU")
    return selected, solver.StatusName(status)


def can_hit_tau(u: np.ndarray, E: np.ndarray, P: np.ndarray,
                nb_local: List[List[int]], tau: float, pop_thresh: int, *,
                cache=None, workers=1, stop_path=None) -> bool:
    """Reject only a proved-infeasible connectivity-free whole-tract model."""
    return _connectivity_free_feasibility(
        u, E, P, tau, pop_thresh, cache=cache, workers=workers, stop_path=stop_path,
    ) != "INFEASIBLE"


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


def _articulation_edge_bounds(
    nb_local: List[List[int]], root_local: int,
) -> Dict[Tuple[int, int], int]:
    """
    Bound every edge from a root-separating articulation into a far-side
    component. A connected selection always admits a rooted spanning-tree flow,
    so flow across these gateway edges can be restricted to point away from the
    articulation without changing the feasible selections. The component size
    bounds how many selected-node demands any one gateway edge can serve.

    Unlike bridge bounds, this also applies when several gateway edges enter a
    cyclic lobe. Returns {(gateway, far_neighbor): far_component_node_count}.
    """
    N = len(nb_local)
    if N == 0:
        return {}

    in_root_component = np.zeros(N, dtype=bool)
    stack = [root_local]
    in_root_component[root_local] = True
    while stack:
        node = stack.pop()
        for neighbor in nb_local[node]:
            if not in_root_component[neighbor]:
                in_root_component[neighbor] = True
                stack.append(neighbor)

    bounds: Dict[Tuple[int, int], int] = {}
    gateways = _articulation_points(nb_local, in_root_component) | {root_local}
    for gateway in sorted(gateways):
        reachable = np.zeros(N, dtype=bool)
        reachable[gateway] = True
        stack = []
        if gateway != root_local:
            reachable[root_local] = True
            stack.append(root_local)
        while stack:
            node = stack.pop()
            for neighbor in nb_local[node]:
                if not reachable[neighbor]:
                    reachable[neighbor] = True
                    stack.append(neighbor)

        for entry in sorted(set(nb_local[gateway])):
            if reachable[entry]:
                continue
            component = {entry}
            stack = [entry]
            reachable[entry] = True
            while stack:
                node = stack.pop()
                for neighbor in nb_local[node]:
                    if neighbor != gateway and not reachable[neighbor]:
                        reachable[neighbor] = True
                        component.add(neighbor)
                        stack.append(neighbor)
            component_size = len(component)
            for neighbor in set(nb_local[gateway]) & component:
                bounds[(gateway, neighbor)] = component_size

    return bounds


def _capacity_budget(q_surplus: np.ndarray, forced_set: set, N: int) -> int:
    """
    Best-case achievable sum(q_i*x_i) if every non-forced node with q_i > 0
    were selected: forced nodes' actual (possibly negative) q plus all
    non-negative q elsewhere. A valid upper bound on ANY feasible selection's
    total UR-surplus -- shared by the global and per-subtree capacity checks
    below so both use the exact same budget definition.
    """
    optional_idx = [i for i in range(N) if i not in forced_set]
    return int(q_surplus[list(forced_set)].sum()) + int(
        np.clip(q_surplus[optional_idx], 0, None).sum()
    )


def _surplus_knapsack_bounds(
    q_surplus: np.ndarray,
    forced_set: set,
    N: int,
    budget_global: Optional[int] = None,
) -> Tuple[Optional[int], List[int], int]:
    """Exact cardinality bounds for the rate-only selection relaxation.

    Forced nodes and every optional node with non-negative UR surplus are free
    (or beneficial) to include. Among optional negative-surplus nodes, taking
    the least-negative values first maximizes how many fit in the available
    surplus. This is the exact maximum selected-node count after relaxing
    population, connectivity, and the objective.

    Returns (max_selected, negative_idx, max_negative). max_selected is None
    when even all optional non-negative surplus cannot offset the forced nodes;
    in that case the rate row itself makes the model infeasible.
    """
    optional_idx = [i for i in range(N) if i not in forced_set]
    negative_idx = sorted(
        (i for i in optional_idx if q_surplus[i] < 0),
        key=lambda i: q_surplus[i],
        reverse=True,  # cheapest (closest to 0) consumer first
    )
    running = (
        _capacity_budget(q_surplus, forced_set, N)
        if budget_global is None
        else int(budget_global)
    )
    if running < 0:
        return None, negative_idx, 0

    max_negative = 0
    for i in negative_idx:
        running += int(q_surplus[i])
        if running < 0:
            break
        max_negative += 1

    # All nodes except optional negative-surplus nodes are included, followed
    # by the maximum affordable prefix of those negative-surplus nodes.
    max_selected = N - len(negative_idx) + max_negative
    return int(max_selected), negative_idx, max_negative


def _remaining_selection_count_bound(
    q_surplus, forced_set, excluded, tract_counts, *, max_nodes=None, exact_nodes=None,
) -> Optional[int]:
    """Upper bound after proven exclusions, including original-tract limits.

    Rate and tract-budget relaxations are bounded separately; their minimum
    remains optimistic for the joint problem. None means one is infeasible.
    """
    forced_set, excluded = set(forced_set), set(excluded)
    if forced_set & excluded:
        return None
    active = [i for i in range(len(q_surplus)) if i not in excluded]
    index = {node: i for i, node in enumerate(active)}
    rate_bound, _, _ = _surplus_knapsack_bounds(
        np.asarray([q_surplus[i] for i in active], dtype=np.int64),
        {index[i] for i in forced_set}, len(active),
    )
    if rate_bound is None:
        return None
    limit = exact_nodes if exact_nodes is not None else max_nodes
    if limit is None:
        return rate_bound
    remaining = int(limit) - sum(int(tract_counts[i]) for i in forced_set)
    if remaining < 0:
        return None
    count_bound = len(forced_set)
    for weight in sorted(int(tract_counts[i]) for i in active if i not in forced_set):
        if weight > remaining:
            break
        remaining -= weight
        count_bound += 1
    return min(rate_bound, count_bound)


def _profitable_closure_edges(nb, u, q):
    """Implications neighbor -> profitable node on the contracted graph."""
    return [
        (int(neighbor), node)
        for node in range(len(nb))
        if int(q[node]) >= 0 and int(u[node]) > 0
        for neighbor in nb[node]
        if int(neighbor) != node
    ]


def _lagrangian_conditional_bounds(u, q, forced):
    """Floor of min_lambda B_{forced union {i}}(lambda), using exact rationals.

    Nonnegative unemployment is required. Dropping connectivity/population
    gives a valid upper bound. Prefix sums over u/(-q) breakpoints permit each
    conditional minimum to be found by binary search, without another solve.
    -1 denotes an infeasible conditional rate row (nonnegative objectives).
    """
    u, q = list(map(int, u)), list(map(int, q))
    if any(value < 0 for value in u):
        raise ValueError("Lagrangian bounds require nonnegative unemployment")
    forced = set(forced)
    events = sorted(
        (Fraction(u[i], -q[i]), i)
        for i in range(len(u)) if i not in forced and q[i] < 0
    )
    breaks = sorted({Fraction(0)} | {value for value, _ in events})
    event_points = [value for value, _ in events]
    prefix_u, prefix_q = [0], [0]
    for _, i in events:
        prefix_u.append(prefix_u[-1] + u[i])
        prefix_q.append(prefix_q[-1] + q[i])
    total_u, total_q = sum(u), sum(q)

    def value_slope(lam, i):
        removed = bisect_right(event_points, lam)
        intercept = total_u - prefix_u[removed]
        slope = total_q - prefix_q[removed]
        # Force i back in after its coefficient would otherwise be omitted.
        if i not in forced and q[i] < 0 and lam * (-q[i]) >= u[i]:
            intercept += u[i]
            slope += q[i]
        return intercept + lam * slope, slope

    bounds = []
    for i in range(len(u)):
        if value_slope(breaks[-1], i)[1] < 0:
            bounds.append(-1)
            continue
        lo, hi = 0, len(breaks) - 1
        while lo < hi:
            mid = (lo + hi) // 2
            if value_slope(breaks[mid], i)[1] >= 0:
                hi = mid
            else:
                lo = mid + 1
        value, _ = value_slope(breaks[lo], i)
        bounds.append(value.numerator // value.denominator)
    return bounds


def _bridge_subtree_zero_fix(
    nb_local: List[List[int]],
    root_local: int,
    q_surplus: np.ndarray,
    forced_set: set,
    budget_global: int,
) -> List[int]:
    """Prove bridge gateways unaffordable using optimistic cyclic-block bounds.

    Remove true bridges to form blocks. Inside each block, charge its required
    entry and forced vertices but credit all other positive surplus for free:
    internal connectivity may cost more, never less. The remaining block graph
    is a tree, so optional negative child subtrees can safely be omitted there.
    A DFS spanning tree *inside* a cyclic block cannot supply an upper bound.
    """
    bridges = _bridge_edge_bounds(nb_local, root_local)
    if not bridges:
        return []
    blocked_edges = {frozenset(edge) for edge in bridges}
    block_of = {}
    blocks = []
    # Only the root's underlying component matters.
    reachable = {root_local}
    stack = [root_local]
    while stack:
        node = stack.pop()
        for neighbor in nb_local[node]:
            if neighbor not in reachable:
                reachable.add(neighbor)
                stack.append(neighbor)
    for start in sorted(reachable):
        if start in block_of:
            continue
        index = len(blocks)
        members = []
        stack = [start]
        block_of[start] = index
        while stack:
            node = stack.pop()
            members.append(node)
            for neighbor in nb_local[node]:
                if (neighbor not in block_of
                        and frozenset((node, neighbor)) not in blocked_edges):
                    block_of[neighbor] = index
                    stack.append(neighbor)
        blocks.append(members)

    children = [[] for _ in blocks]
    entry = {block_of[root_local]: root_local}
    for near, far in bridges:
        parent, child = block_of[near], block_of[far]
        children[parent].append(child)
        entry[child] = far
    order = [block_of[root_local]]
    for block in order:
        order.extend(children[block])
    best = [0] * len(blocks)
    positive = [0] * len(blocks)
    has_forced = [False] * len(blocks)
    to_fix = []
    for block in reversed(order):
        members = blocks[block]
        required = (set(members) & forced_set) | {entry[block]}
        best[block] = sum(int(q_surplus[node]) if node in required
                          else max(0, int(q_surplus[node])) for node in members)
        positive[block] = sum(max(0, int(q_surplus[node])) for node in members)
        has_forced[block] = bool(set(members) & forced_set)
        for child in children[block]:
            best[block] += best[child] if has_forced[child] else max(0, best[child])
            positive[block] += positive[child]
            has_forced[block] |= has_forced[child]
        if (entry[block] != root_local and not has_forced[block]
                and best[block] + int(budget_global) - positive[block] < 0):
            to_fix.append(entry[block])
    return sorted(to_fix)


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
    use_articulation_edge_bounds: bool = False,
    use_distance_flow_bounds: bool = False,
    use_global_capacity_cardinality_bound: bool = False,
    use_bridge_subtree_pruning: bool = False,
    max_nodes: Optional[int] = None,
    exact_nodes: Optional[int] = None,
    stop_flag_path: Optional[str] = None,
    skip_flag_path: Optional[str] = None,
    objective_upper_bound: Optional[int] = None,
    initial_connectivity_cuts: Optional[
        Sequence[Tuple[Sequence[int], Sequence[int]]]
    ] = None,
    initial_components: Optional[Sequence[Sequence[int]]] = None,
    use_flow_first_search: bool = False,
    use_tract_capacity_search: bool = False,
    use_flow_capacity_hybrid_search: bool = False,
    incumbent_stall_seconds: Optional[float] = None,
    objective_no_improve_stop: Optional[int] = None,
    incumbent_report_callback: Optional[Callable[[List[int], int], None]] = None,
    incumbent_report_interval_seconds: float = 60.0,
    incumbent_interrupt_callback: Optional[Callable[[List[int], int], bool]] = None,
    use_profitable_component_closure: bool = True,
    use_lagrangian_variable_fixing: bool = True,
    feasibility_cache: Optional[Dict] = None,
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

    `exact_nodes`, when given, replaces that cap with an equality constraint
    (`sum(x) == exact_nodes`, same original-graph tract counting) and takes
    precedence over `max_nodes` if both are set. Intended for experimenting
    with whether fixing the selected tract count speeds up a single ASU solve;
    infeasible if no valid ASU of exactly that size exists in this window.

    `use_profitable_component_closure` adds neighbor-to-profitable-component
    implications. `use_lagrangian_variable_fixing` excludes nodes whose exact
    rate-relaxed conditional objective bound is below a verified incumbent.
    Both default on, run only without max_nodes/exact_nodes, and preserve
    primary-objective ties. Each can be disabled for controlled comparisons.

    `objective_upper_bound` and `initial_connectivity_cuts` may be supplied by
    a connectivity-free solve over this same window. The former is valid because
    dropping connectivity only enlarges the feasible region; each latter cut
    requires a selected detached tract to select at least one vertex on that
    relaxed component's graph boundary.

    `stop_flag_path`, when given, names a file whose mere existence is polled by
    a dedicated watchdog thread during the main solve; on detection it calls
    `solver.stop_search()` (a CP-SAT callback API) so the current incumbent is
    returned immediately instead of waiting for `time_limit` or optimality.

    `skip_flag_path` behaves identically for the current window's solve, except
    the file is consumed (deleted) once detected, so only this ASU's search is
    cut short -- callers such as `build_many_asus_cpsat` keep building further
    ASU windows afterward instead of halting entirely.

    `incumbent_stall_seconds`, when given, finishes the solve early (keeping
    the current incumbent, just like `stop_flag_path`) once that many seconds
    pass with no incumbent improvement. Unlike the separate (currently
    disabled) stall-then-restart/probe mechanism below -- found to lose
    learned clauses and hurt performance when it rebuilds the solver --
    this only ever stops the search; it never restarts it. `None` (default)
    disables this early-finish check entirely.

    `objective_no_improve_stop`, when given, stops the search as soon as the
    solver's best objective upper bound drops to this threshold (or below)
    while the incumbent is still at or below the same threshold. This is used
    by late-stage callers that only care whether the solve can strictly beat a
    known baseline objective.

    `incumbent_report_callback`, when supplied, receives the latest connected
    local-node selection and objective on a periodic cadence during the main
    CP-SAT search. Reporting is observational only and never changes the model.

    `incumbent_interrupt_callback`, when supplied, is evaluated for every
    improving connected incumbent. Returning true stops the current solve after
    preserving that incumbent, allowing the caller to validate, merge, and
    restart outside CP-SAT's callback context.
    """
    start_time = time.monotonic()
    N = len(nb_local)
    if N == 0:
        return None
    custom_modes = [
        bool(use_tract_first_search),
        bool(use_flow_first_search),
        bool(use_tract_capacity_search),
        bool(use_flow_capacity_hybrid_search),
    ]
    if sum(custom_modes) > 1:
        raise ValueError("custom fixed-search workers are mutually exclusive")
    if use_flow_first_search and use_arborescence:
        raise ValueError("flow-first search requires an integer flow formulation")
    if use_flow_capacity_hybrid_search and use_arborescence:
        raise ValueError("flow-capacity hybrid search requires an integer flow formulation")

    required_orig = sorted({int(root_local)} | set(forced_selected or []))
    if hint is not None:
        hint = sorted({int(node) for node in hint})
        hint_valid = component_ok(
            hint, u_g, E_g, P_g, tau, pop_thresh, nb_local,
            required=required_orig, max_nodes=max_nodes, exact_nodes=exact_nodes,
        )
        if not hint_valid:
            if log:
                print(
                    "  [WARN] discarding invalid warm-start hint before flow construction",
                    flush=True,
                )
            hint = None
    # Metadata alone is never a certificate of a feasible objective floor.
    hint_obj = int(u_g[hint].sum()) if hint else None
    if hint is None:
        screen_status = _connectivity_free_feasibility(
            u_g, E_g, P_g, tau, pop_thresh,
            seconds=min(5.0, max(0.0, float(time_limit)) * 0.1,
                        max(0.0, float(time_limit) - (time.monotonic() - start_time))),
            workers=workers,
            required=required_orig, max_nodes=max_nodes, exact_nodes=exact_nodes,
            stop_path=stop_flag_path, cache=feasibility_cache,
        )
        if log:
            print(f"[CONNECTIVITY-FREE SCREEN] {N} tracts: {screen_status}" +
                  ("; skipping connected solve" if screen_status == "INFEASIBLE" else ""), flush=True)
        if screen_status == "INFEASIBLE":
            return None
    elif log:
        print(f"[CONNECTIVITY-FREE SCREEN] {N} tracts: verified connected hint; "
              "auxiliary solve skipped", flush=True)

    # Contract UR>=tau clusters so the model has fewer variables; expand at every
    # return point. All cluster members co-occur in any feasible solution (mediant
    # inequality), so this is exact — not an approximation.
    nb_c, u_c, E_c, P_c, expand_c, node_map_c = contract_high_ur_nodes(nb_local, u_g, E_g, P_g, tau)
    nb_local_orig, u_g_orig, root_local_orig = nb_local, u_g, root_local
    E_g_orig, P_g_orig = E_g, P_g
    nb_local, u_g, E_g, P_g = nb_c, u_c, E_c, P_c
    N = len(nb_local)
    root_local = int(node_map_c[root_local_orig])
    tract_first_enabled = (
        configure_subsolvers
        and use_tract_first_search
        and max(1, int(workers)) >= 8
    )
    flow_first_enabled = (
        configure_subsolvers
        and use_flow_first_search
        and max(1, int(workers)) >= 6
    )
    tract_capacity_enabled = (
        configure_subsolvers
        and use_tract_capacity_search
        and max(1, int(workers)) >= 6
    )
    flow_capacity_hybrid_enabled = (
        configure_subsolvers
        and use_flow_capacity_hybrid_search
        and max(1, int(workers)) >= 6
    )
    custom_fixed_search_enabled = (
        tract_first_enabled
        or flow_first_enabled
        or tract_capacity_enabled
        or flow_capacity_hybrid_enabled
    )
    flow_magnitude_branching_enabled = (
        flow_first_enabled or flow_capacity_hybrid_enabled
    )
    if hint is not None:
        hint = sorted({int(node_map_c[v]) for v in hint})
        expanded_hint = sorted({v for ri in hint for v in expand_c[ri]})
        if not component_ok(
            expanded_hint, u_g_orig, E_g_orig, P_g_orig, tau, pop_thresh,
            nb_local_orig, required=required_orig,
            max_nodes=max_nodes, exact_nodes=exact_nodes,
        ):
            hint = None
        hint_obj = int(u_g[hint].sum()) if hint else None

    def _to_orig(sel: Optional[List[int]], status: str) -> "CpsatResult":
        # Preserve the strongest verified incumbent even on timeout/interrupt.
        if best_connected is not None and (
            not sel or int(u_g[sel].sum()) < best_obj
        ):
            sel = best_connected
            if status == "OPTIMAL":
                status = "FEASIBLE"
        orig = sorted({v for ri in sel for v in expand_c[ri]}) if sel else []
        if not component_ok(
            orig, u_g_orig, E_g_orig, P_g_orig, tau, pop_thresh, nb_local_orig,
            required=required_orig, max_nodes=max_nodes, exact_nodes=exact_nodes,
        ):
            raise ValueError("Connected solve returned an invalid expanded ASU")
        return CpsatResult(orig, root_local_orig, int(u_g_orig[orig].sum()) if orig else 0, status)

    def _incumbent_requests_interrupt(
        selection: Sequence[int],
    ) -> bool:
        if incumbent_interrupt_callback is None:
            return False
        expanded_selection = sorted({
            original_node
            for contracted_node in selection
            for original_node in expand_c[int(contracted_node)]
        })
        try:
            return bool(incumbent_interrupt_callback(
                expanded_selection,
                int(u_g_orig[expanded_selection].sum()),
            ))
        except Exception as exc:
            if log:
                print(
                    f"  [incumbent] interrupt callback failed: {exc}",
                    flush=True,
                )
            return False

    model = cp_model.CpModel()

    # Decision variables
    x = [model.NewBoolVar(f"x_{i}") for i in range(N)]
    fixed_zero_nodes: Set[int] = set()

    # Selecting the root permits its entire connected high-UR component at no cost.
    forced_set = {int(node_map_c[v]) for v in (forced_selected or [])}
    forced_set.add(root_local)
    for i in forced_set:
        model.Add(x[i] == 1)

    # Seed the cut-pass phase from the caller's own smallest-first component
    # breakdown instead of a pre-built full hint: starting from the biggest
    # available incumbent locks the objective floor to that incumbent's size,
    # which forecloses smaller connected candidates that would leave tracts
    # free for other ASUs to form elsewhere.
    initial_pending: List[set] = []
    for component_orig in (initial_components or []):
        component_c = {
            int(node_map_c[int(v)]) for v in component_orig
            if 0 <= int(v) < len(node_map_c)
        } - forced_set
        if component_c:
            initial_pending.append(component_c)
    initial_pending.sort(key=lambda c: (len(c), int(u_g[list(c)].sum())))

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
            fixed_zero_nodes.add(v)

    seeded_cut_clauses = 0
    seen_seeded_cuts: set = set()
    for component_orig, boundary_orig in (initial_connectivity_cuts or []):
        component_c = {
            int(node_map_c[int(node)])
            for node in component_orig
            if 0 <= int(node) < len(node_map_c)
        }
        if not component_c or root_local in component_c:
            continue
        boundary_c = {
            int(node_map_c[int(node)])
            for node in boundary_orig
            if 0 <= int(node) < len(node_map_c)
        }
        boundary_tuple = tuple(sorted(boundary_c))
        for node in sorted(component_c):
            # If contraction maps a boundary tract into this same high-UR
            # supernode, x[node] appears on both sides and the cut is tautological.
            if node in boundary_c:
                continue
            cut_key = (node, boundary_tuple)
            if cut_key in seen_seeded_cuts:
                continue
            seen_seeded_cuts.add(cut_key)
            if boundary_tuple:
                model.AddBoolOr(
                    [x[node].Not()] + [x[boundary] for boundary in boundary_tuple]
                )
            else:
                model.Add(x[node] == 0)
                fixed_zero_nodes.add(node)
            seeded_cut_clauses += 1

    if log and seeded_cut_clauses:
        print(
            f"  connectivity-free seed cuts: {seeded_cut_clauses} clause(s)",
            flush=True,
        )

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

    # A connected selection containing node i needs at least dist(root, i) + 1
    # selected contracted nodes. Build the shared count variable before the cut
    # pass so this cheap projection of connectivity strengthens every solve,
    # not only the final flow formulation.
    surplus_count_bound, _, surplus_negative_bound = _surplus_knapsack_bounds(
        q_surplus, forced_set, N
    )
    max_selected = N if surplus_count_bound is None else surplus_count_bound
    max_selected = max(len(forced_set), min(N, int(max_selected)))
    selected_count = model.NewIntVar(
        len(forced_set), max_selected, "selected_count"
    )
    model.Add(selected_count == sum(x))
    root_distances = _root_graph_distances(nb_local, root_local)
    reachability_rows = 0
    unreachable_fixed = 0
    for node, distance in enumerate(root_distances):
        if node == root_local:
            continue
        if distance >= N:
            model.Add(x[node] == 0)
            fixed_zero_nodes.add(node)
            unreachable_fixed += 1
            continue
        model.Add(selected_count >= (int(distance) + 1) * x[node])
        reachability_rows += 1

    if log:
        if surplus_count_bound is None:
            print(
                "  analytical surplus bound: rate relaxation infeasible; "
                f"leaving selected-count upper bound at {N}",
                flush=True,
            )
        else:
            print(
                f"  analytical surplus bound: selected <= {max_selected} "
                f"({surplus_negative_bound} optional deficit tract(s))",
                flush=True,
            )
        print(
            f"  distance/cardinality reachability: {reachability_rows} row(s), "
            f"{unreachable_fixed} unreachable node(s) fixed to 0",
            flush=True,
        )

    # Completion improves the primary objective but can violate a tract count
    # constraint. Strictly positive unemployment avoids disturbing count ties.
    uncapped_reductions = max_nodes is None and exact_nodes is None
    if uncapped_reductions and use_profitable_component_closure:
        closure_edges = _profitable_closure_edges(nb_local, u_g, q_surplus)
        for neighbor, profitable in closure_edges:
            model.Add(x[neighbor] <= x[profitable])
        if log:
            print(f"  profitable component closure: {len(closure_edges)} implications",
                  flush=True)

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

    # Global capacity cardinality bound: unlike the separator K_C bounds above,
    # this needs no separator -- the single UR row already limits how many
    # below-threshold (q<0) tracts can jointly appear in ANY feasible solution,
    # using the same best-case, connectivity/pop-ignoring supply trick as K_C
    # and the analytical count bound below. Unproven vs. CP-SAT's own knapsack-
    # cover cuts at linearization_level=2 -- opt-in pending an A/B test.
    if use_global_capacity_cardinality_bound or use_bridge_subtree_pruning:
        budget_global = _capacity_budget(q_surplus, forced_set, N)

    if use_global_capacity_cardinality_bound:
        _, negative_idx, k_neg = _surplus_knapsack_bounds(
            q_surplus, forced_set, N, budget_global
        )
        if k_neg < len(negative_idx):
            model.Add(sum(x[i] for i in negative_idx) <= k_neg)
        if log:
            print(
                f"  global capacity cardinality bound: <= {k_neg} of "
                f"{len(negative_idx)} below-threshold tract(s) may be jointly selected",
                flush=True,
            )

    if use_bridge_subtree_pruning:
        bridge_fixed_zero = _bridge_subtree_zero_fix(
            nb_local, root_local, q_surplus, forced_set, budget_global
        )
        for i in bridge_fixed_zero:
            model.Add(x[i] == 0)
        fixed_zero_nodes.update(bridge_fixed_zero)
        if log:
            print(
                f"  bridge subtree pruning: {len(bridge_fixed_zero)} gateway tract(s) "
                "provably unreachable within UR-surplus budget, fixed to 0",
                flush=True,
            )

    pop_expr, lhs = _add_asu_feasibility_constraints(
        model, x, u_g, E_g, P_g, tau, pop_thresh,
        tract_counts=[len(grp) for grp in expand_c],
        max_nodes=max_nodes, exact_nodes=exact_nodes,
    )

    # Objective: maximize unemployment captured
    obj_expr = sum(int(u_g[i]) * x[i] for i in range(N))
    model.Maximize(obj_expr)

    if objective_upper_bound is not None:
        proposed_upper_bound = int(objective_upper_bound)
        if hint_obj is not None and proposed_upper_bound < int(hint_obj):
            if log:
                print(
                    f"  [WARN] ignoring relaxed objective upper bound "
                    f"{proposed_upper_bound} below feasible hint {hint_obj}",
                    flush=True,
                )
        else:
            model.Add(obj_expr <= proposed_upper_bound)
            if log:
                print(
                    f"  connectivity-free objective upper bound: "
                    f"unemp <= {proposed_upper_bound}",
                    flush=True,
                )

    # Keep the verified incumbent independently of exploratory hints. Defer
    # only the objective floor so the cut pass may explore smaller candidates.
    defer_hint_floor = bool(initial_pending)
    best_connected = sorted(hint) if hint else None
    best_obj = int(u_g[best_connected].sum()) if best_connected else -1
    if defer_hint_floor:
        lower_bound = -1
    else:
        # Warm-start with the connected reverse-prune solution.
        if hint is not None:
            hint_set = set(hint)
            for i in range(N):
                model.AddHint(x[i], 1 if i in hint_set else 0)

        # Lower bound: reject solutions worse than the reverse-prune warm start.
        if hint_obj is not None and hint_obj > 0:
            model.Add(obj_expr >= hint_obj)

        lower_bound = hint_obj if (hint_obj is not None and hint_obj > 0) else -1

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
    cut_time_budget = min(max(0.0, float(time_limit)),
                          60, max(2.0, float(time_limit) * 0.15))
    stall_rounds = 0
    prev_num_components: Optional[int] = None
    first_components: Optional[int] = None
    prev_detached_unemp: Optional[int] = None
    separator_pool: Dict[int, List[frozenset]] = {}
    for target, separator in separator_implications:
        seed_set = frozenset(
            int(node) for node in separator
            if int(node) != int(root_local) and int(node) != int(target)
        )
        if seed_set:
            separator_pool.setdefault(int(target), []).append(seed_set)
    separator_attempts = 0
    separator_accepted = 0
    separator_duplicates = 0
    separator_pool_superseded = 0
    separator_literals = 0
    separator_clause_literals = 0
    separator_sizes: List[int] = []
    fallback_components = 0
    fallback_clauses = 0
    fallback_literals = 0
    old_boundary_clauses_equivalent = 0
    old_boundary_literals_equivalent = 0
    _DYNAMIC_SEPARATOR_MAX = 16
    _DYNAMIC_TARGETS_PER_COMPONENT = 3
    def _component_key(component: set) -> Tuple[int, int]:
        # Smallest tract count first; ties broken by smallest unemployment.
        return (len(component), int(u_g[list(component)].sum()))

    def _set_expand_hint(nodes: set) -> None:
        hint_proto = model.Proto().solution_hint
        hint_proto.vars.clear()
        hint_proto.values.clear()
        for i in range(N):
            model.AddHint(x[i], 1 if i in nodes else 0)

    def _register_separator(target: int, separator: Sequence[int]) -> Optional[Tuple[int, ...]]:
        nonlocal separator_attempts
        nonlocal separator_accepted
        nonlocal separator_duplicates
        nonlocal separator_pool_superseded
        nonlocal separator_literals
        nonlocal separator_clause_literals
        nonlocal separator_sizes

        separator_attempts += 1
        candidate = frozenset(
            int(node) for node in separator
            if int(node) != int(root_local) and int(node) != int(target)
        )
        if not candidate:
            return None

        existing = separator_pool.setdefault(int(target), [])
        for prior in existing:
            if prior == candidate:
                separator_duplicates += 1
                return None
            if prior.issubset(candidate):
                separator_pool_superseded += 1
                return None

        reduced = [prior for prior in existing if not candidate.issubset(prior)]
        separator_pool[int(target)] = reduced + [candidate]
        separator_accepted += 1
        separator_literals += len(candidate)
        separator_clause_literals += len(candidate) + 1
        separator_sizes.append(len(candidate))
        return tuple(sorted(candidate))

    def _pick_component_targets(component: set) -> List[int]:
        """
        Pick target nodes within a component for expansion.

        The selection prioritizes nodes based on unemployment, surplus, and distance
        from the root, ensuring diverse criteria for choosing targets.

        Returns a list of target node indices, with a length up to
        _DYNAMIC_TARGETS_PER_COMPONENT.
        """

        u_target = max(
            component,
            key=lambda node: (int(u_g[node]), -int(node)),
        )
        q_target = max(
            component,
            key=lambda node: (int(q_surplus[node]), int(u_g[node]), -int(node)),
        )
        far_target = max(
            component,
            key=lambda node: (int(root_distances[node]), int(u_g[node]), -int(node)),
        )
        targets = list(dict.fromkeys([int(q_target), int(u_target), int(far_target)]))
        if len(targets) < _DYNAMIC_TARGETS_PER_COMPONENT:
            ranked = sorted(
                component,
                key=lambda node: (
                    -int(u_g[node]),
                    int(q_surplus[node]) <= 0,
                    -int(q_surplus[node]),
                    -int(root_distances[node]),
                    int(node),
                ),
            )
            for node in ranked:
                node_i = int(node)
                if node_i not in targets:
                    targets.append(node_i)
                if len(targets) >= _DYNAMIC_TARGETS_PER_COMPONENT:
                    break
        return targets

    # Populated once a round goes DISCONNECTED; nudges each following round
    # toward absorbing the smallest still-disconnected component first,
    # instead of leaving CP-SAT to re-solve the cuts unguided every round.
    # Pre-seeded from the caller's own smallest-first breakdown, if given, so
    # round 0 already targets the smallest component instead of the full hint.
    pending_expand: List[set] = list(initial_pending)

    while cut_round < 100:
        elapsed = time.monotonic() - start_time
        remaining_for_cuts = cut_time_budget - elapsed
        if remaining_for_cuts <= 0:
            break

        if pending_expand:
            expand_component = pending_expand.pop(0)
            expand_base = (
                set(best_connected) if best_connected is not None and not defer_hint_floor
                else set(forced_set)
            )
            _set_expand_hint(expand_base | expand_component)
            if log:
                print(
                    f"  [cut-pass] round {cut_round}: expand attempt, "
                    f"smallest pending component size={len(expand_component)}, "
                    f"{len(pending_expand)} more pending",
                    flush=True,
                )

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = max(1, int(workers))
        solver.parameters.max_time_in_seconds = remaining_for_cuts
        solver.parameters.log_search_progress = False  # silent; summary logged after loop
        solver.parameters.cp_model_presolve = True
        solver.parameters.linearization_level = 2
        if configure_subsolvers:
            _configure_asu_solver_portfolio(
                solver.parameters,
                workers,
                use_tract_first_search=tract_first_enabled,
                use_flow_first_search=flow_first_enabled,
                use_tract_capacity_search=tract_capacity_enabled,
                use_flow_capacity_hybrid_search=flow_capacity_hybrid_enabled,
            )

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
            improved_connected = objective > best_obj
            if improved_connected:
                best_connected, best_obj = selected, objective
                if best_obj > lower_bound:
                    model.Add(obj_expr >= best_obj)
                    lower_bound = best_obj
            if log:
                print(
                    f"  [cut-pass] round {cut_round}: CONNECTED, unemp={objective} "
                    f"(best so far={best_obj}), elapsed={time.monotonic() - start_time:.1f}s",
                    flush=True,
                )
            if improved_connected and _incumbent_requests_interrupt(selected):
                if log:
                    print(
                        "  [merge] cut-pass incumbent touches another ASU; "
                        "returning it for validation and immediate merge/restart.",
                        flush=True,
                    )
                return _to_orig(selected, "MERGE_STOPPED_FEASIBLE")
            if (
                status == cp_model.OPTIMAL
                and not deterministic_ties
                and not custom_fixed_search_enabled
            ):
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
        detached_unemp = sum(int(u_g[list(component)].sum()) for component in components)

        # Try the smallest disconnected component first next round -- cheapest
        # to absorb, and most likely for a CP-SAT-guided swap to succeed.
        pending_expand = sorted(components, key=_component_key)
        if log:
            best_text = str(best_obj) if best_obj >= 0 else "none"
            print(
                f"  [cut-pass] round {cut_round}: DISCONNECTED "
                f"({len(components)} component(s) cut), best_connected so far={best_text}, "
                f"detached_unemp={detached_unemp}, elapsed={time.monotonic() - start_time:.1f}s",
                flush=True,
            )

        round_separator_accepted = 0
        for component in components:
            dynamic_added = 0
            targets = _pick_component_targets(component)
            boundary = sorted({
                w for v in component for w in nb_local[v]
                if w not in component
            })
            old_boundary_clauses_equivalent += len(component)
            old_boundary_literals_equivalent += len(component) * (len(boundary) + 1)

            for target in targets:
                separator = _minimum_root_vertex_separator(
                    nb_local,
                    root_local,
                    int(target),
                    protected_nodes=selected_set,
                    max_size=_DYNAMIC_SEPARATOR_MAX,
                )
                if separator is None:
                    continue
                kept = _register_separator(int(target), separator)
                if not kept:
                    continue
                model.AddBoolOr([x[int(target)].Not()] + [x[s] for s in kept])
                dynamic_added += 1
                round_separator_accepted += 1

            if dynamic_added > 0:
                continue
            fallback_components += 1
            if boundary:
                # Fallback when dynamic separation yields no useful cut.
                for target in targets:
                    model.AddBoolOr([x[int(target)].Not()] + [x[w] for w in boundary])
                    fallback_clauses += 1
                    fallback_literals += len(boundary) + 1
            else:
                for target in targets:
                    model.Add(x[int(target)] == 0)
                    fixed_zero_nodes.add(int(target))
                    fallback_clauses += 1
                    fallback_literals += 1

        if (
            prev_num_components is not None
            and round_separator_accepted == 0
            and len(components) >= prev_num_components
            and prev_detached_unemp is not None
            and detached_unemp >= prev_detached_unemp
        ):
            stall_rounds += 1
        else:
            stall_rounds = 0
        prev_num_components = len(components)
        prev_detached_unemp = detached_unemp

        cut_round += 1
        if stall_rounds >= 3:
            break

    if log and separator_attempts > 0:
        avg_literals = separator_literals / max(1, separator_accepted)
        size_min = min(separator_sizes) if separator_sizes else 0
        size_median = float(np.median(separator_sizes)) if separator_sizes else 0.0
        size_p90 = float(np.percentile(separator_sizes, 90)) if separator_sizes else 0.0
        size_max = max(separator_sizes) if separator_sizes else 0
        boundary_clauses_avoided = max(0, old_boundary_clauses_equivalent - fallback_clauses)
        boundary_literals_avoided = max(0, old_boundary_literals_equivalent - fallback_literals)
        separator_to_old_boundary_ratio = (
            separator_clause_literals / old_boundary_literals_equivalent
            if old_boundary_literals_equivalent > 0 else 0.0
        )
        print(
            f"  [cut-pass] dynamic separators: attempts={separator_attempts}, "
            f"accepted={separator_accepted}, duplicates={separator_duplicates}, "
            f"pool_superseded={separator_pool_superseded}, avg_size={avg_literals:.2f}, "
            f"size(min/med/p90/max)=({size_min}/{size_median:.1f}/{size_p90:.1f}/{size_max}), "
            f"dynamic_clause_literals={separator_clause_literals}, "
            f"fallback_components={fallback_components}, fallback_clauses={fallback_clauses}, "
            f"fallback_literals={fallback_literals}, "
            f"old_boundary_equivalent_clauses={old_boundary_clauses_equivalent}, "
            f"old_boundary_equivalent_literals={old_boundary_literals_equivalent}, "
            f"boundary_clauses_avoided={boundary_clauses_avoided}, "
            f"boundary_literals_avoided={boundary_literals_avoided}, "
            f"separator_to_old_boundary_literal_ratio={separator_to_old_boundary_ratio:.3f}",
            flush=True,
        )

    # The cut-pass phase was deliberately left unfloored to explore smaller
    # candidates; now apply the known-good hint as a floor for the exact flow
    # phase so the final committed answer never regresses below it.
    if best_connected is not None and best_obj > lower_bound:
        model.Add(obj_expr >= best_obj)
        lower_bound = best_obj

    # Finish with exact connectivity, strengthened by the cuts. Fall back to
    # the full hint for the flow phase's own warm-start if cut-pass never
    # reached a connected candidate of its own.
    flow_source = best_connected
    if (
        uncapped_reductions and use_lagrangian_variable_fixing
        and flow_source is not None
        and forced_set.issubset(set(flow_source))
        and component_ok(flow_source, u_g, E_g, P_g, tau, pop_thresh, nb_local)
        and all(int(value) >= 0 for value in u_g)
    ):
        reduction_start = time.monotonic()
        # Recompute L from a verified feasible selection, not caller metadata.
        incumbent_floor = sum(int(u_g[i]) for i in flow_source)
        conditional_bounds = _lagrangian_conditional_bounds(
            u_g, q_surplus, forced_set
        )
        fixed_zero = [
            i for i, bound in enumerate(conditional_bounds)
            if i not in forced_set and bound < incumbent_floor
        ]
        for i in fixed_zero:
            model.Add(x[i] == 0)
        fixed_zero_nodes.update(fixed_zero)
        if log:
            print(
                f"  Lagrangian fixing: {len(fixed_zero)} nodes fixed to 0, "
                f"incumbent={incumbent_floor}, "
                f"elapsed={time.monotonic() - reduction_start:.3f}s",
                flush=True,
            )
    # Recompute connectivity and rate capacity after proven exclusions. In
    # particular, excluded positive-surplus nodes must no longer fund M.
    reachable_after_fixing = set()
    stack = [] if root_local in fixed_zero_nodes else [root_local]
    reachable_after_fixing.update(stack)
    while stack:
        node = stack.pop()
        for neighbor in nb_local[node]:
            if neighbor not in fixed_zero_nodes and neighbor not in reachable_after_fixing:
                reachable_after_fixing.add(neighbor)
                stack.append(neighbor)
    disconnected_after_fixing = set(range(N)) - reachable_after_fixing - fixed_zero_nodes
    for node in disconnected_after_fixing:
        model.Add(x[node] == 0)
    fixed_zero_nodes.update(disconnected_after_fixing)
    tightened_count = _remaining_selection_count_bound(
        q_surplus, forced_set, fixed_zero_nodes, [len(grp) for grp in expand_c],
        max_nodes=max_nodes, exact_nodes=exact_nodes,
    )
    if tightened_count is None:
        model.AddBoolOr([])
    elif tightened_count < max_selected:
        if log:
            print(f"  post-pruning count bound: {max_selected} -> {tightened_count}", flush=True)
        max_selected = tightened_count
        model.Add(selected_count <= max_selected)

    # Cut-pass rounds repeatedly overwrite the model's variable hints with
    # small, partial expand attempts (see _set_expand_hint); refresh x[] here
    # so the flow phase starts from a hint consistent with flow_source rather
    # than whatever tiny leftover component the last cut-pass round tried.
    if flow_source is not None:
        model.ClearHints()
        flow_source_set = set(flow_source)
        for i in range(N):
            model.AddHint(x[i], 1 if i in flow_source_set else 0)
    flow_hints = (
        _spanning_tree_flows(flow_source, nb_local, root_local)
        if flow_source is not None else {}
    )

    # The shared selection-count variable and its rate-relaxed upper bound were
    # installed before the cut pass so rooted-distance propagation was active
    # there too. Reuse the same bound as the flow capacity.
    M = max(1, max_selected - 1)
    if flow_source is not None:
        model.AddHint(selected_count, len(flow_source))

    if use_arborescence:
        # Boolean arborescence formulation.
        # par_vars[(i,j)] = 1  ↔  j is the parent of i in the rooted spanning tree.
        # For each selected non-root node exactly one parent is assigned; acyclicity
        # is enforced by an exact half-reified depth equality (not big-M), so the LP
        # relaxation stays tight instead of letting depth wander freely. Advantages
        # vs. integer flow:
        #   - ~8 k BoolVars replace ~4 k large-domain IntVars → CP-SAT can apply
        #     clause learning and unit propagation far more aggressively.
        #   - depth IntVars are bounded by the count sub-solve above (M), not the
        #     naive N-1, and pinned to 0 for unselected nodes / graph-distance for
        #     selected ones.
        dist = [-1] * N
        dist[root_local] = 0
        _dist_q = [root_local]
        _qh = 0
        while _qh < len(_dist_q):
            v = _dist_q[_qh]
            _qh += 1
            for w in nb_local[v]:
                if dist[w] < 0:
                    dist[w] = dist[v] + 1
                    _dist_q.append(w)

        par_vars = {}   # (i, j) -> BoolVar: j is the parent of i (root has none)
        for i, nb_i in enumerate(nb_local):
            if i == root_local:
                continue
            for j in nb_i:
                if i != j:
                    par_vars[(i, j)] = model.NewBoolVar(f"par_{i}_{j}")

        depth_vars = [model.NewIntVar(0, M, f"d_{i}") for i in range(N)]
        model.Add(depth_vars[root_local] == 0)

        for i in range(N):
            if i == root_local:
                continue
            parent_choices = [par_vars[(i, j)] for j in nb_local[i]]
            # exactly one parent when selected, zero when unselected
            model.Add(sum(parent_choices) == x[i])

            # Selection <-> useful depth: unselected pins to 0; selected is bounded
            # below by graph distance to root (a tree path is still a graph path)
            # and above by how many nodes are actually in the selection.
            model.Add(depth_vars[i] >= max(dist[i], 0) * x[i])
            model.Add(depth_vars[i] <= M * x[i])
            model.Add(depth_vars[i] <= selected_count - 1).OnlyEnforceIf(x[i])

            for j in nb_local[i]:
                pvar = par_vars[(i, j)]
                model.Add(pvar <= x[j])   # parent must be selected
                # Exact tree depth, half-reified -- no big-M slack for the LP.
                model.Add(depth_vars[i] == depth_vars[j] + 1).OnlyEnforceIf(pvar)

        # Root-separating articulation vertices are forced ancestors in ANY
        # arborescence (every root->node path passes through them), not merely
        # required-selected -- a cheap extra depth cut on top of that x<=x fact.
        for node, cut_vertex in root_implications:
            model.Add(
                depth_vars[node] >= depth_vars[cut_vertex] + 1
            ).OnlyEnforceIf(x[node])

        # SAT-level 2-cycle elimination: implied by depth already, but propagates
        # via a unit clause instead of integer arithmetic.
        for i in range(N):
            for j in nb_local[i]:
                if i < j:
                    pij, pji = par_vars.get((i, j)), par_vars.get((j, i))
                    if pij is not None and pji is not None:
                        model.AddBoolOr([pij.Not(), pji.Not()])

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
                f"({len(par_vars)} parent vars + {N} depth vars, depth <= {M})",
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
        # one (flow on any edge can never exceed total selected nodes - 1); M itself
        # comes from the count sub-solve shared with the arborescence branch above.
        # A standalone connectivity-free objective solve was previously A/B tested
        # here and reverted because its ~10s cost did not improve the final bound.
        # `objective_upper_bound`, when supplied, instead reuses the already-paid
        # hint relaxation's certificate and therefore adds no second sub-solve.
        # NOTE: an earlier, non-root-aware version of _bridge_edge_bounds() (symmetric
        # split-size bound, both directions bounded) was A/B tested on real Colorado
        # data and was a clear regression (0.52%->2.62% gap, 75,214->74,160 unemp
        # @300s) -- see SKILL.md. The current _bridge_edge_bounds() is a materially
        # different, strictly tighter formulation (root-rooted DFS, one direction
        # forced to exactly 0 rather than just bounded) gated behind
        # use_bridge_edge_bounds -- treat this as unproven until A/B tested again.
        bridge_bounds = _bridge_edge_bounds(nb_local, root_local) if use_bridge_edge_bounds else {}
        articulation_bounds = (
            _articulation_edge_bounds(nb_local, root_local)
            if use_articulation_edge_bounds else {}
        )
        edge_bounds = [M] * len(edges)

        # Distance-based per-edge caps, valid on cycles too: any connected
        # selection admits a rooted spanning-tree flow, a tree edge (i -> j)
        # carries |subtree(j)| <= |S| - 1 - depth(i), and tree depth in the
        # selected subgraph can never be below graph distance from root.
        # Unlike the fixed-tree subtree bound rejected above, this caps by
        # root distance, which every alternative routing still respects.
        if use_distance_flow_bounds:
            dist_from_root = [N] * N
            dist_from_root[root_local] = 0
            _bfs_q = [root_local]
            _bh = 0
            while _bh < len(_bfs_q):
                v = _bfs_q[_bh]
                _bh += 1
                for w in nb_local[v]:
                    if dist_from_root[w] > dist_from_root[v] + 1:
                        dist_from_root[w] = dist_from_root[v] + 1
                        _bfs_q.append(w)

            def _distance_cap(node: int) -> int:
                return max(0, M - dist_from_root[node])
        else:
            def _distance_cap(node: int) -> int:
                return M
        distance_tightened = 0

        def _directional_bound(start: int, end: int) -> Optional[int]:
            bounds = [
                bound for bound in (
                    bridge_bounds.get((start, end)),
                    articulation_bounds.get((start, end)),
                )
                if bound is not None
            ]
            return min(bounds) if bounds else None

        abs_flow = []
        if use_signed_flow:
            f = []
            for idx, (i, j) in enumerate(edges):
                far_bound = _directional_bound(i, j)
                rev_bound = _directional_bound(j, i)
                if far_bound is not None:
                    lo, hi = 0, min(far_bound, M)
                elif rev_bound is not None:
                    lo, hi = -min(rev_bound, M), 0
                else:
                    lo, hi = -edge_bounds[idx], edge_bounds[idx]
                if use_distance_flow_bounds:
                    cap_out, cap_in = _distance_cap(i), _distance_cap(j)
                    if cap_out < hi:
                        hi = cap_out
                        distance_tightened += 1
                    if -cap_in > lo:
                        lo = -cap_in
                        distance_tightened += 1
                flow_var = model.NewIntVar(lo, hi, f"f_{i}_{j}")
                f.append(flow_var)
                if flow_magnitude_branching_enabled:
                    magnitude = model.NewIntVar(
                        0, max(abs(lo), abs(hi)), f"abs_f_{i}_{j}"
                    )
                    model.AddAbsEquality(magnitude, flow_var)
                    abs_flow.append(magnitude)
            net_out_for = [[] for _ in range(N)]
            for edge_index, (i, j) in enumerate(edges):
                model.Add(f[edge_index] == 0).OnlyEnforceIf(x[i].Not())
                model.Add(f[edge_index] == 0).OnlyEnforceIf(x[j].Not())
                if use_flow_count_envelope:
                    model.Add(f[edge_index] <= selected_count - 1)
                    model.Add(f[edge_index] >= 1 - selected_count)

                net_out_for[i].append(f[edge_index])
                net_out_for[j].append(-f[edge_index])

                hinted_flow = (
                    flow_hints.get((i, j), 0) - flow_hints.get((j, i), 0)
                )
                model.AddHint(f[edge_index], hinted_flow)
                if flow_magnitude_branching_enabled:
                    model.AddHint(abs_flow[edge_index], abs(hinted_flow))
            for i in range(N):
                net_outflow = sum(net_out_for[i]) if net_out_for[i] else 0
                model.Add(net_outflow == (selected_count - 1 if i == root_local else -x[i]))
        else:
            directed_bounds = []
            for idx, (i, j) in enumerate(edges):
                far_bound = _directional_bound(i, j)
                rev_bound = _directional_bound(j, i)
                if far_bound is not None:
                    bound_value = min(far_bound, edge_bounds[idx])
                elif rev_bound is not None:
                    bound_value = 0
                else:
                    bound_value = edge_bounds[idx]
                if use_distance_flow_bounds and _distance_cap(i) < bound_value:
                    bound_value = _distance_cap(i)
                    distance_tightened += 1
                directed_bounds.append(bound_value)
            f = [model.NewIntVar(0, directed_bounds[idx], f"f_{i}_{j}") for idx, (i, j) in enumerate(edges)]
            if flow_magnitude_branching_enabled:
                abs_flow = f
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
            if use_articulation_edge_bounds:
                print(
                    f"  articulation edge bounds: {len(articulation_bounds)} gateway edge(s) "
                    "tightened (reverse direction forced to 0)",
                    flush=True,
                )
            if use_distance_flow_bounds:
                _reach = [d for d in dist_from_root if d < N]
                print(
                    f"  distance flow bounds: {distance_tightened} edge direction(s) "
                    f"tightened (max root distance {max(_reach) if _reach else 0})",
                    flush=True,
                )

    remaining_time = float(time_limit) - (time.monotonic() - start_time)
    if log and cut_round > 0:
        _fc = first_components or "?"
        _lc = prev_num_components if prev_num_components is not None else "?"
        print(f"  cut phase: {cut_round} round(s), components {_fc}->{_lc}, "
              f"{time_limit - remaining_time:.1f}s used; {remaining_time:.1f}s for flow phase", flush=True)

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
                    flow_value = (
                        sf.get((edge_u, edge_v), 0) - sf.get((edge_v, edge_u), 0)
                    )
                    target_model.AddHint(f[edge_index], flow_value)
                    if flow_magnitude_branching_enabled:
                        target_model.AddHint(abs_flow[edge_index], abs(flow_value))
            else:
                for edge_index, (edge_u, edge_v) in enumerate(edges):
                    target_model.AddHint(f[edge_index], sf.get((edge_u, edge_v), 0))
            return

        target_model.AddHint(selected_count, len(sel_set))
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
    status = cp_model.UNKNOWN
    status_name = "UNKNOWN"
    selected: List[int] = []
    objective = -1
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
            _configure_asu_solver_portfolio(
                _scout.parameters,
                workers,
                use_tract_first_search=tract_first_enabled,
                use_flow_first_search=flow_first_enabled,
                use_tract_capacity_search=tract_capacity_enabled,
                use_flow_capacity_hybrid_search=flow_capacity_hybrid_enabled,
            )
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
            if (_scout_status == cp_model.OPTIMAL and _scout_obj >= best_obj
                    and abs(_scout.BestObjectiveBound() - _scout_obj) < 1e-6):
                status, status_name = cp_model.OPTIMAL, "OPTIMAL"
                selected, objective = _scout_sel, _scout_obj
                if log:
                    print("  scout: primary optimum proved; skipping main "
                          "optimization and proceeding to requested tie-breaks", flush=True)
    bound_stop_threshold = (
        int(objective_no_improve_stop)
        if objective_no_improve_stop is not None
        else None
    )
    last_reported_incumbent: Optional[Tuple[int, ...]] = None

    if (status != cp_model.OPTIMAL and objective_shaving
            and best_connected is not None and rel_gap is None):
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
            if configure_subsolvers:
                _configure_asu_solver_portfolio(
                    proof_solver.parameters,
                    workers,
                    use_tract_first_search=tract_first_enabled,
                    use_flow_first_search=flow_first_enabled,
                    use_tract_capacity_search=tract_capacity_enabled,
                    use_flow_capacity_hybrid_search=flow_capacity_hybrid_enabled,
                )
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
        if flow_capacity_hybrid_enabled:
            root_distances = _root_graph_distances(nb_local, root_local)
            flow_prefix, select_prefix, reject_prefix, far_order = (
                _asu_flow_capacity_hybrid_groups(
                    edges,
                    u_g,
                    E_g,
                    num=num,
                    den=den,
                    root_distances=root_distances,
                    max_prefix=_ASU_HYBRID_PREFIX_SIZE,
                )
            )

            if flow_prefix:
                model.add_decision_strategy(
                    [abs_flow[edge_index] for edge_index in flow_prefix],
                    cp_model.CHOOSE_MAX_DOMAIN_SIZE,
                    cp_model.SELECT_MIN_VALUE,
                )
            if select_prefix:
                model.add_decision_strategy(
                    [x[i] for i in select_prefix],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MAX_VALUE,
                )
            if reject_prefix:
                model.add_decision_strategy(
                    [x[i] for i in reject_prefix],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MIN_VALUE,
                )

            far_tail_vars = [
                x[index] if kind == "tract" else abs_flow[index]
                for kind, index in far_order
            ]
            if far_tail_vars:
                model.add_decision_strategy(
                    far_tail_vars,
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MIN_VALUE,
                )
            if log:
                print(
                    f"  hybrid worker: flow prefix {len(flow_prefix)}, "
                    f"capacity select/reject {len(select_prefix)}/{len(reject_prefix)}, "
                    f"distance tail {len(far_order)}",
                    flush=True,
                )
        elif flow_first_enabled:
            flow_branch_order = _asu_flow_branch_order(edges, u_g, E_g)
            if flow_branch_order:
                model.AddDecisionStrategy(
                    [abs_flow[edge_index] for edge_index in flow_branch_order],
                    cp_model.CHOOSE_MAX_DOMAIN_SIZE,
                    cp_model.SELECT_MIN_VALUE,
                )
            if log:
                print(
                    f"  flow-first worker: branch on {len(flow_branch_order)} "
                    "absolute-flow variables by largest domain; ties use incident "
                    "tract UR then unemployment; select minimum magnitude",
                    flush=True,
                )

        if tract_capacity_enabled:
            select_order, reject_order = _asu_tract_capacity_orders(
                u_g,
                E_g,
                num=num,
                den=den,
            )
            if select_order:
                model.add_decision_strategy(
                    [x[i] for i in select_order],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MAX_VALUE,
                )
            if reject_order:
                model.add_decision_strategy(
                    [x[i] for i in reject_order],
                    cp_model.CHOOSE_FIRST,
                    cp_model.SELECT_MIN_VALUE,
                )
            if log:
                print(
                    f"  tract-capacity worker: select {len(select_order)} "
                    f"nonnegative-q tracts, then reject {len(reject_order)} "
                    "negative-q tracts",
                    flush=True,
                )
        elif tract_first_enabled:
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

        remaining_time = float(time_limit) - (time.monotonic() - start_time)
        if remaining_time <= 0.01:
            return _to_orig(best_connected, "FEASIBLE") if best_connected else None

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

            if workers < 6:
                return

            # Prevent old/default custom subsolvers from being inserted
            # ahead of our explicitly ordered ASU portfolio.
            params.extra_subsolvers.clear()
            params.subsolvers.clear()
            params.filter_subsolvers.clear()

            if flow_first_enabled:
                _append_asu_subsolver_params(
                    params,
                    "asu_flow_first",
                    search_branching=cp_model.PARTIAL_FIXED_SEARCH,
                    linearization_level=2,
                    root_lp_iterations=25_000,
                    add_lp_constraints_lazily=False,
                    max_cut_rounds_at_level_zero=10,
                )

            if flow_capacity_hybrid_enabled:
                _append_asu_subsolver_params(
                    params,
                    "asu_flow_capacity_hybrid",
                    search_branching=cp_model.PARTIAL_FIXED_SEARCH,
                    linearization_level=2,
                    root_lp_iterations=25_000,
                    add_lp_constraints_lazily=False,
                    max_cut_rounds_at_level_zero=10,
                )

            if tract_first_enabled:
                _append_asu_subsolver_params(
                    params,
                    "asu_tract_first",
                    search_branching=cp_model.PARTIAL_FIXED_SEARCH,
                    linearization_level=2,
                )

            if tract_capacity_enabled:
                _append_asu_subsolver_params(
                    params,
                    "asu_tract_capacity",
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
                save_lp_basis_in_lb_tree_search=True,
                max_cut_rounds_at_level_zero=10,
                add_objective_cut=True,
                root_lp_iterations = 100_000
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
                use_flow_first_search=flow_first_enabled,
                use_tract_capacity_search=tract_capacity_enabled,
                use_flow_capacity_hybrid_search=flow_capacity_hybrid_enabled,
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
                #"graph_dec_lns", usually not good

                "rnd_var_lns",
                "rnd_cst_lns",

                "ls*"
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
            params.lns_initial_difficulty = 0.3
            params.lns_initial_deterministic_limit = .3
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
        # The stall-then-restart/probe path below has been disabled on
        # purpose: every time the solver restarts, we end up losing progress
        # -- clauses, learned constraints, and any other progress made in the
        # previous window. We were testing the effect of the stall window,
        # but right now it seems to cause more harm than good.
        #
        # `incumbent_stall_seconds`, when set, is a separate, much simpler
        # early-finish check: it reuses the same watchdog/timer machinery to
        # detect the same stall condition, but on trigger it just keeps the
        # current incumbent and stops (like `stop_flag_path`) instead of
        # restarting the solver, so it can't hit the clause-loss problem above.
        finish_on_stall = incumbent_stall_seconds is not None
        stall_window_seconds = (
            float(incumbent_stall_seconds) if finish_on_stall else 30000.0
        )
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
            bound_stop_value: Optional[float] = None
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
                    nonlocal stop_kind
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

                    if incumbent_interrupt_callback is not None:
                        if _incumbent_requests_interrupt(candidate):
                            stop_kind = "merge"
                            stopped.set()
                            self.stop_search()

            def _on_best_bound(bound: float) -> None:
                nonlocal stop_kind
                nonlocal bound_stop_value
                with progress_lock:
                    if bound < progress["best_bound"] - 1e-9:
                        progress["best_bound"] = float(bound)
                        progress["last_bound_time"] = time.monotonic()
                    if (
                        bound_stop_threshold is not None
                        and progress["best_obj"] <= bound_stop_threshold
                        and bound <= bound_stop_threshold + 1e-9
                    ):
                        stop_kind = "bound"
                        bound_stop_value = float(bound)
                        stopped.set()
                        solver.stop_search()

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

            def _incumbent_reporter() -> None:
                nonlocal last_reported_incumbent
                interval = max(1.0, float(incumbent_report_interval_seconds))
                while not solve_done.is_set():
                    with progress_lock:
                        selection = progress["best_selection"]
                    if selection is not None:
                        expanded = tuple(sorted({
                            original_node
                            for contracted_node in selection
                            for original_node in expand_c[int(contracted_node)]
                        }))
                        if expanded != last_reported_incumbent:
                            try:
                                incumbent_report_callback(
                                    list(expanded),
                                    int(u_g_orig[list(expanded)].sum()),
                                )
                                last_reported_incumbent = expanded
                            except Exception as exc:
                                if log:
                                    print(
                                        f"  [incumbent] progress callback failed: {exc}",
                                        flush=True,
                                    )
                    if solve_done.wait(interval):
                        return

            callback = _MainSolveCallback()
            solver.best_bound_callback = _on_best_bound
            watchdog = threading.Thread(target=_stall_watchdog, daemon=True)
            watchdog.start()
            stop_watchdog: Optional[threading.Thread] = None
            if stop_flag_path or skip_flag_path:
                stop_watchdog = threading.Thread(target=_stop_watchdog, daemon=True)
                stop_watchdog.start()
            incumbent_reporter: Optional[threading.Thread] = None
            if incumbent_report_callback is not None:
                incumbent_reporter = threading.Thread(
                    target=_incumbent_reporter,
                    daemon=True,
                )
                incumbent_reporter.start()
            try:
                main_status = solver.Solve(model, callback)
            finally:
                solve_done.set()
                watchdog.join()
                if stop_watchdog is not None:
                    stop_watchdog.join()
                if incumbent_reporter is not None:
                    incumbent_reporter.join()

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
                    if stop_kind == "skip":
                        status_name = "SKIPPED_FEASIBLE"
                    elif stop_kind == "bound":
                        status_name = "BOUND_STOPPED_FEASIBLE"
                    elif stop_kind == "merge":
                        status_name = "MERGE_STOPPED_FEASIBLE"
                    else:
                        status_name = "STOPPED_FEASIBLE"
                    selected = list(best_connected)
                    objective = best_obj
                if log:
                    if stop_kind == "bound":
                        print(
                            "  [bound] objective upper bound reached the "
                            f"no-improvement threshold ({bound_stop_value:.1f} <= "
                            f"{bound_stop_threshold}); returning incumbent "
                            f"{best_obj if best_obj >= 0 else 'none'}.",
                            flush=True,
                        )
                    elif stop_kind == "merge":
                        print(
                            "  [merge] improving incumbent touches another ASU; "
                            f"returning incumbent {best_obj} for validation and "
                            "immediate merge/restart.",
                            flush=True,
                        )
                    else:
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

            if finish_on_stall:
                status = cp_model.FEASIBLE
                status_name = "STALLED_FEASIBLE"
                selected = list(best_connected)
                objective = best_obj
                if log:
                    print(
                        f"  stall watchdog: no incumbent movement for "
                        f"{stall_window_seconds:.0f}s; finishing early with "
                        f"current incumbent {best_obj}.",
                        flush=True,
                    )
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
                    _configure_asu_solver_portfolio(
                        tie_solver.parameters,
                        workers,
                        use_tract_first_search=tract_first_enabled,
                        use_flow_first_search=flow_first_enabled,
                        use_tract_capacity_search=tract_capacity_enabled,
                        use_flow_capacity_hybrid_search=flow_capacity_hybrid_enabled,
                    )
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
def frontier_candidates(S: List[int], nb: List[List[int]], allowed) -> List[int]:
    Sset = set(S)
    # Callers on a hot path (e.g. improve_by_trades) pass an already-built set
    # to skip re-deriving it from `allowed` on every single call.
    allowed_set = allowed if isinstance(allowed, set) else set(int(a) for a in allowed)
    cand = set()
    for v in S:
        for w in nb[v]:
            if (w not in Sset) and (w in allowed_set):
                cand.add(w)
    return sorted(cand)


def improve_by_trades(S0: List[int], u: np.ndarray, E: np.ndarray, P: np.ndarray, nb: List[List[int]],
                      tau: float, pop_thresh: int, allowed: np.ndarray, max_iter: int = 200,
                      max_swap_checks: Optional[int] = None, max_size: Optional[int] = None) -> List[int]:
    N = len(nb)
    allowed_set = set(int(a) for a in allowed)
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
            for t in sorted(frontier_candidates(S, nb, allowed_set), key=lambda i: u[i], reverse=True):
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
            selected_mask = np.zeros(N, dtype=bool)
            selected_mask[S] = True
            # S is always connected here, so a non-articulation-point candidate
            # is guaranteed to leave S2 connected -- this one igraph call replaces
            # a full connectivity BFS (component_ok) per removal candidate below.
            articulations = _articulation_points(nb, selected_mask)
            n_checked = 0
            for r in sorted(S, key=lambda i: u[i]):
                if max_swap_checks is not None and n_checked >= max_swap_checks:
                    break
                n_checked += 1
                if r in articulations:
                    continue
                reduced_u = sum_u - int(u[r])
                reduced_E = sum_E - int(E[r])
                reduced_P = sum_P - int(P[r])
                if reduced_P < pop_thresh or ur_of(reduced_u, reduced_E) < tau:
                    continue
                S2 = sorted(selected - {r})
                for a in sorted(frontier_candidates(S2, nb, allowed_set), key=lambda i: u[i], reverse=True):
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

    _add_asu_feasibility_constraints(model, x, u_g, E_g, P_g, tau, pop_thresh)

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
    _configure_asu_solver_portfolio(solver.parameters, num_workers)

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


@dataclass
class ConnectivityFreeCandidate:
    selected: List[int]
    objective: int


@dataclass
class ConnectivityFreeResult:
    selected: List[int]
    objective: Optional[int]
    best_bound: Optional[float]
    status: str
    solve_seconds: float
    candidates: List[ConnectivityFreeCandidate]


@dataclass
class ConnectivityFreeComponent:
    nodes: List[int]
    contains_root: bool
    unemployed: int
    employed: int
    population: int
    exact_slack: int
    boundary: List[int]
    independently_feasible: bool
    connector_nodes: List[int]
    connector_exact_slack: int
    connector_deficit: int
    connector_unemployed: int
    connector_population: int
    connector_reachable: bool


def _select_diverse_connectivity_free_candidates(
    candidates: Sequence[ConnectivityFreeCandidate],
    limit: int,
) -> List[ConnectivityFreeCandidate]:
    """Keep the best relaxed incumbent plus selections with distinct tract sets."""
    unique: Dict[Tuple[int, ...], ConnectivityFreeCandidate] = {}
    for candidate in candidates:
        key = tuple(sorted({int(i) for i in candidate.selected}))
        normalized = ConnectivityFreeCandidate(list(key), int(candidate.objective))
        prior = unique.get(key)
        if prior is None or normalized.objective > prior.objective:
            unique[key] = normalized

    ordered = sorted(
        unique.values(),
        key=lambda candidate: (-candidate.objective, tuple(candidate.selected)),
    )
    if len(ordered) <= max(1, int(limit)):
        return ordered

    chosen = [ordered[0]]
    remaining = ordered[1:]
    chosen_sets = [set(chosen[0].selected)]
    while remaining and len(chosen) < max(1, int(limit)):
        best_position = max(
            range(len(remaining)),
            key=lambda position: (
                min(
                    len(set(remaining[position].selected).symmetric_difference(selected))
                    for selected in chosen_sets
                ),
                remaining[position].objective,
                tuple(-i for i in remaining[position].selected),
            ),
        )
        candidate = remaining.pop(best_position)
        chosen.append(candidate)
        chosen_sets.append(set(candidate.selected))

    return sorted(
        chosen,
        key=lambda candidate: (-candidate.objective, tuple(candidate.selected)),
    )


def _analyze_connectivity_free_components(
    selected: Sequence[int],
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    include_connectors: bool = True,
) -> List[ConnectivityFreeComponent]:
    """Summarize relaxed islands and, when requested, their cheapest connectors."""
    N = len(nb_local)
    selected_set = {int(i) for i in selected if 0 <= int(i) < N}
    if not selected_set:
        return []

    unseen = set(selected_set)
    components: List[set] = []
    while unseen:
        seed = min(unseen)
        unseen.remove(seed)
        component = {seed}
        stack = [seed]
        while stack:
            node = stack.pop()
            for neighbor in nb_local[node]:
                if neighbor in unseen:
                    unseen.remove(neighbor)
                    component.add(neighbor)
                    stack.append(neighbor)
        components.append(component)

    root_component = next(
        (component for component in components if root_local in component),
        {root_local},
    )
    num, den = as_fraction_tau(tau)
    exact_slack = den * u_g.astype(np.int64) - num * E_g.astype(np.int64)

    infinity = (math.inf, math.inf)
    distances: List[Tuple[float, float]] = [infinity] * N
    previous = [-1] * N
    queue: List[Tuple[float, float, int]] = []
    if include_connectors:
        for node in root_component:
            distances[node] = (0.0, 0.0)
            heapq.heappush(queue, (0.0, 0.0, node))

    while include_connectors and queue:
        deficit, connector_count, node = heapq.heappop(queue)
        if (deficit, connector_count) != distances[node]:
            continue
        for neighbor in nb_local[node]:
            is_connector = neighbor not in selected_set
            step_deficit = max(0, -int(exact_slack[neighbor])) if is_connector else 0
            next_distance = (
                deficit + float(step_deficit),
                connector_count + float(is_connector),
            )
            if next_distance < distances[neighbor]:
                distances[neighbor] = next_distance
                previous[neighbor] = node
                heapq.heappush(queue, (*next_distance, neighbor))

    summaries: List[ConnectivityFreeComponent] = []
    for component in components:
        nodes = sorted(component)
        boundary = sorted({
            neighbor
            for node in component
            for neighbor in nb_local[node]
            if neighbor not in component
        })
        contains_root = root_local in component
        connector_reachable = contains_root
        connector_nodes: List[int] = []
        if not contains_root and include_connectors:
            endpoint = min(component, key=lambda node: (*distances[node], node))
            connector_reachable = math.isfinite(distances[endpoint][0])
            if connector_reachable:
                path_connectors = set()
                node = endpoint
                while node not in root_component:
                    if node not in selected_set:
                        path_connectors.add(node)
                    node = previous[node]
                    if node < 0:
                        path_connectors.clear()
                        connector_reachable = False
                        break
                connector_nodes = sorted(path_connectors)

        unemployed = int(u_g[nodes].sum())
        employed = int(E_g[nodes].sum())
        population = int(P_g[nodes].sum())
        component_slack = int(exact_slack[nodes].sum())
        connector_slack = (
            int(exact_slack[connector_nodes].sum()) if connector_nodes else 0
        )
        summaries.append(ConnectivityFreeComponent(
            nodes=nodes,
            contains_root=contains_root,
            unemployed=unemployed,
            employed=employed,
            population=population,
            exact_slack=component_slack,
            boundary=boundary,
            independently_feasible=(
                population >= int(pop_thresh) and component_slack >= 0
            ),
            connector_nodes=connector_nodes,
            connector_exact_slack=connector_slack,
            connector_deficit=(
                int(np.maximum(0, -exact_slack[connector_nodes]).sum())
                if connector_nodes else 0
            ),
            connector_unemployed=(
                int(u_g[connector_nodes].sum()) if connector_nodes else 0
            ),
            connector_population=(
                int(P_g[connector_nodes].sum()) if connector_nodes else 0
            ),
            connector_reachable=connector_reachable,
        ))

    return sorted(
        summaries,
        key=lambda component: (
            not component.contains_root,
            -component.unemployed,
            tuple(component.nodes),
        ),
    )


def solve_connectivity_free_relaxation(
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    *,
    forced_selected: Optional[Sequence[int]] = None,
    max_nodes: Optional[int] = None,
    time_limit: float = 10.0,
    workers: int = 8,
    objective_floor: Optional[int] = None,
    max_candidates: int = 1,
    log: bool = False,
    stop_path: Optional[str] = None,
    skip_path: Optional[str] = None,
) -> Optional[ConnectivityFreeResult]:
    """Maximize unemployment subject to ASU economics, ignoring connectivity."""
    N = len(u_g)
    if N == 0:
        return None

    def cancellation():
        if _stop_requested(stop_path):
            return "STOPPED"
        if _stop_requested(skip_path):
            _consume_flag(skip_path)
            return "SKIPPED"
        return None

    reason = cancellation()
    if reason:
        return ConnectivityFreeResult([], None, None, reason, 0.0, [])

    num, den = as_fraction_tau(tau)
    model = cp_model.CpModel()
    x = [model.NewBoolVar(f"relax_x_{i}") for i in range(N)]
    forced = {int(i) for i in (forced_selected or [])} | {int(root_local)}
    for i in forced:
        model.Add(x[i] == 1)

    pop_expr, slack_expr = _add_asu_feasibility_constraints(
        model, x, u_g, E_g, P_g, tau, pop_thresh, max_nodes=max_nodes,
    )
    objective_expr = sum(int(u_g[i]) * x[i] for i in range(N))
    if objective_floor is not None and objective_floor > 0:
        model.Add(objective_expr >= int(objective_floor))
    model.Maximize(objective_expr)

    solver = cp_model.CpSolver()
    solver.parameters.num_search_workers = max(1, int(workers))
    solver.parameters.max_time_in_seconds = max(0.01, float(time_limit))
    solver.parameters.cp_model_presolve = True
    solver.parameters.linearization_level = 2
    solver.parameters.log_search_progress = False
    _configure_asu_solver_portfolio(solver.parameters, workers)

    class _RelaxedIncumbentCollector(cp_model.CpSolverSolutionCallback):
        def __init__(self, log_enabled: bool, start_time: float) -> None:
            super().__init__()
            self.archive: List[ConnectivityFreeCandidate] = []
            self.seen: set = set()
            self._log = log_enabled
            self._start = start_time
            self._last_print = 0.0
            self._best = -1

        def on_solution_callback(self) -> None:
            selected_tuple = tuple(i for i in range(N) if self.BooleanValue(x[i]))
            if selected_tuple in self.seen:
                return
            self.seen.add(selected_tuple)
            obj = int(sum(int(u_g[i]) for i in selected_tuple))
            self.archive.append(ConnectivityFreeCandidate(
                selected=list(selected_tuple),
                objective=obj,
            ))
            if len(self.archive) > 64:
                discarded = self.archive.pop(0)
                self.seen.discard(tuple(discarded.selected))
            if self._log and obj > self._best:
                now = time.monotonic()
                if now - self._last_print >= 2.0:
                    self._best = obj
                    self._last_print = now
                    print(
                        f"    [connectivity-free] candidate unemp={obj}, "
                        f"elapsed={now - self._start:.1f}s",
                        flush=True,
                    )

    started = time.monotonic()
    collector = _RelaxedIncumbentCollector(log, started)
    done = threading.Event()
    interrupted = []

    def watch():
        while not done.wait(0.1):
            reason = cancellation()
            if reason:
                interrupted.append(reason)
                solver.StopSearch()
                return

    watcher = threading.Thread(target=watch, daemon=True)
    reason = cancellation()
    if reason:
        return ConnectivityFreeResult([], None, None, reason, 0.0, [])
    watcher.start()
    try:
        status = solver.Solve(model, collector)
    finally:
        done.set()
        watcher.join()
    elapsed = time.monotonic() - started
    status_name = interrupted[0] if interrupted else solver.StatusName(status)
    if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        raw_bound = float(solver.BestObjectiveBound())
        best_bound = (
            raw_bound
            if status != cp_model.INFEASIBLE and math.isfinite(raw_bound)
            else None
        )
        return ConnectivityFreeResult(
            selected=[],
            objective=None,
            best_bound=best_bound,
            status=status_name,
            solve_seconds=elapsed,
            candidates=[],
        )

    selected = [i for i in range(N) if solver.BooleanValue(x[i])]
    objective = int(u_g[selected].sum())
    candidates = _select_diverse_connectivity_free_candidates(
        collector.archive + [ConnectivityFreeCandidate(selected, objective)],
        max_candidates,
    )
    return ConnectivityFreeResult(
        selected=selected,
        objective=objective,
        best_bound=float(solver.BestObjectiveBound()),
        status=status_name,
        solve_seconds=elapsed,
        candidates=candidates,
    )


def repair_connectivity_free_selection(
    relaxed_selected: Sequence[int],
    fallback: Sequence[int],
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    *,
    forced_selected: Optional[Sequence[int]] = None,
    max_nodes: Optional[int] = None,
    log: bool = False,
    deadline: Optional[float] = None,
) -> List[int]:
    """Connect valuable relaxed components, then prune and refill to feasibility.

    `deadline`, if given, is an absolute `time.monotonic()` cutoff: once passed,
    the pending-component attachment loop stops starting new attachments, the
    prune loop gives up rather than keep trimming, and the (expensive) beam-
    refill/trade-improvement polish step is skipped in favor of returning
    whatever valid candidate is already in hand -- callers with their own
    overall time budget (e.g. graph-cut) should pass their own deadline so
    repair can't run past it.
    """
    N = len(nb_local)
    forced = {int(i) for i in (forced_selected or [])} | {int(root_local)}
    relaxed = {int(i) for i in relaxed_selected if 0 <= int(i) < N} | forced
    num, den = as_fraction_tau(tau)
    slack = den * u_g.astype(np.int64) - num * E_g.astype(np.int64)

    _progress_start = time.monotonic()
    _progress_last = [0.0]

    def _log_progress(msg: str) -> None:
        if not log:
            return
        now = time.monotonic()
        if now - _progress_last[0] < 10.0:
            return
        _progress_last[0] = now
        print(f"    [repair] {msg}, elapsed={now - _progress_start:.1f}s", flush=True)

    def _component(start: int, allowed: set) -> set:
        mask = np.zeros(N, dtype=bool)
        mask[list(allowed)] = True
        for comp in _connected_components(nb_local, mask):
            if start in comp:
                return set(comp)
        return {start}

    def _components(nodes: set) -> List[set]:
        if not nodes:
            return []
        mask = np.zeros(N, dtype=bool)
        mask[list(nodes)] = True
        return [set(comp) for comp in _connected_components(nb_local, mask)]

    def _valid(nodes: set) -> bool:
        selected = sorted(nodes)
        return (
            forced.issubset(nodes)
            and (max_nodes is None or len(selected) <= int(max_nodes))
            and component_ok(
                selected, u_g, E_g, P_g, tau, pop_thresh, nb_local
            )
        )

    def _prune(nodes: set) -> Optional[set]:
        candidate = set(nodes)
        # pop_sum/slack_sum/selected_mask are maintained incrementally below
        # instead of being resummed/rebuilt from scratch every iteration --
        # the O(N+E) articulation-point recompute is unavoidable per removal,
        # but there is no need to also redo O(N log N + N) bookkeeping around it.
        selected_mask = np.zeros(N, dtype=bool)
        selected_mask[sorted(candidate)] = True
        pop_sum = int(P_g[sorted(candidate)].sum())
        slack_sum = int(slack[sorted(candidate)].sum())
        while True:
            too_large = max_nodes is not None and len(candidate) > int(max_nodes)
            if not too_large and slack_sum >= 0:
                return candidate if pop_sum >= pop_thresh else None
            if deadline is not None and time.monotonic() >= deadline:
                return None

            _log_progress(
                f"pruning: candidate size={len(candidate)}, pop={pop_sum}, "
                f"slack={slack_sum}"
            )
            articulations = _articulation_points(nb_local, selected_mask)
            removable = [
                i for i in candidate - forced - articulations
                if pop_sum - int(P_g[i]) >= pop_thresh
            ]
            if not removable:
                return None

            if slack_sum < 0:
                removable = [i for i in removable if int(slack[i]) < 0]
                if not removable:
                    return None
                dropped = min(
                    removable,
                    key=lambda i: (
                        int(u_g[i]) / max(1, -int(slack[i])),
                        int(u_g[i]),
                        i,
                    ),
                )
            else:
                dropped = min(
                    removable,
                    key=lambda i: (
                        int(u_g[i]),
                        -int(slack[i]),
                        i,
                    ),
                )
            candidate.remove(dropped)
            selected_mask[dropped] = False
            pop_sum -= int(P_g[dropped])
            slack_sum -= int(slack[dropped])

    fallback_set = {int(i) for i in fallback}
    best = fallback_set if _valid(fallback_set) else set()
    current = _component(root_local, relaxed)
    root_candidate = _prune(current) if int(P_g[sorted(current)].sum()) >= pop_thresh else None
    if root_candidate is not None and _valid(root_candidate):
        if not best or _selection_key(root_candidate, u_g, slack) > _selection_key(best, u_g, slack):
            best = root_candidate

    pending = [component for component in _components(relaxed - current)]
    capacity_cost = tau * E_g.astype(float) - (1.0 - tau) * u_g.astype(float)
    economic_value = u_g.astype(float) - 2.2 * capacity_cost
    path_cost = np.maximum(0.01, -economic_value)

    # Edge weights (cost of entering the target node) depend only on `relaxed`
    # and `path_cost`, both fixed for the whole call, so the directed graph is
    # built ONCE here instead of redoing a full Python Dijkstra on every
    # pending-component attachment below. Each iteration only needs to refresh
    # the super-source's outgoing edges (one per node in the current multi-
    # source frontier) and run a single C-backed multi-target Dijkstra.
    if _ig is not None:
        _repair_ss = N
        _repair_digraph = _ig.Graph(
            n=N + 1,
            edges=[(a, b) for a in range(N) for b in nb_local[a]],
            directed=True,
        )
        _repair_digraph.es["weight"] = [
            0.0 if b in relaxed else float(path_cost[b])
            for a in range(N) for b in nb_local[a]
        ]

        def _shortest_paths(current_set: set):
            old_out = _repair_digraph.incident(_repair_ss, mode="out")
            if old_out:
                _repair_digraph.delete_edges(old_out)
            nodes = list(current_set)
            _repair_digraph.add_edges([(_repair_ss, node) for node in nodes])
            if nodes:
                new_eids = list(range(
                    _repair_digraph.ecount() - len(nodes), _repair_digraph.ecount()
                ))
                _repair_digraph.es[new_eids]["weight"] = [0.0] * len(nodes)
            dist_row = _repair_digraph.distances(
                source=[_repair_ss], weights="weight", mode="out"
            )[0]

            def _paths_batch(endpoints: List[int]) -> List[set]:
                if not endpoints:
                    return []
                vpaths = _repair_digraph.get_shortest_paths(
                    _repair_ss, to=endpoints, weights="weight", mode="out",
                    output="vpath",
                )
                return [set(vpath) - current_set - {_repair_ss} for vpath in vpaths]

            return dist_row, _paths_batch
    else:
        def _shortest_paths(current_set: set):
            distances = [math.inf] * N
            previous = [-1] * N
            queue: List[Tuple[float, int]] = []
            for node in current_set:
                distances[node] = 0.0
                heapq.heappush(queue, (0.0, node))

            while queue:
                distance, node = heapq.heappop(queue)
                if distance > distances[node]:
                    continue
                for neighbor in nb_local[node]:
                    step_cost = 0.0 if neighbor in relaxed else float(path_cost[neighbor])
                    next_distance = distance + step_cost
                    if next_distance + 1e-12 < distances[neighbor]:
                        distances[neighbor] = next_distance
                        previous[neighbor] = node
                        heapq.heappush(queue, (next_distance, neighbor))

            def _paths_batch(endpoints: List[int]) -> List[set]:
                result = []
                for endpoint in endpoints:
                    path = set()
                    node = endpoint
                    while node not in current_set:
                        path.add(node)
                        node = previous[node]
                        if node < 0:
                            path = set()
                            break
                    result.append(path)
                return result

            return distances, _paths_batch

    while pending:
        if deadline is not None and time.monotonic() >= deadline:
            break
        distances, paths_batch = _shortest_paths(current)

        reachable_positions = []
        endpoints = []
        for position, component in enumerate(pending):
            endpoint = min(component, key=lambda node: (distances[node], node))
            if not math.isfinite(distances[endpoint]):
                continue
            reachable_positions.append(position)
            endpoints.append(endpoint)
        paths = paths_batch(endpoints)

        choices = []
        for position, path in zip(reachable_positions, paths):
            if not path:
                continue
            component = pending[position]
            added = (component | path) - current
            connector = path - relaxed
            connector_burden = float(np.maximum(0.0, capacity_cost[list(connector)]).sum()) if connector else 0.0
            value = int(u_g[list(added)].sum())
            choices.append((value / (1.0 + connector_burden), value, -len(path), -position, position, path))

        if not choices:
            break
        _, _, _, _, position, path = max(choices)
        component = pending.pop(position)
        current.update(component)
        current.update(path)
        _log_progress(
            f"attached component ({len(component)} node(s) via {len(path)}-node "
            f"path), pending={len(pending)}, current size={len(current)}"
        )

        if int(P_g[sorted(current)].sum()) < pop_thresh:
            continue
        repaired = _prune(current)
        if repaired is None:
            continue
        current = repaired
        if _valid(current):
            candidate = set(current)
            if deadline is None or time.monotonic() < deadline:
                if max_nodes is None:
                    candidate = _beam_refill(
                        candidate, set(), nb_local, u_g, slack, 32, 256
                    )
                candidate = set(improve_by_trades(
                    sorted(candidate), u_g, E_g, P_g, nb_local, tau, pop_thresh,
                    np.arange(N), max_iter=100, max_size=max_nodes,
                ))
            if _valid(candidate) and (
                not best
                or _selection_key(candidate, u_g, slack) > _selection_key(best, u_g, slack)
            ):
                best = candidate

    return sorted(best)


def _reachable_polish_window(root, asu_number, assignments, nb):
    """Current root component using this ASU and unassigned tracts only."""
    reached = {int(root)}
    queue = [int(root)]
    for node in queue:
        for neighbor in nb[node]:
            if (neighbor not in reached
                    and (assignments[neighbor] <= 0 or assignments[neighbor] == asu_number)):
                reached.add(neighbor)
                queue.append(neighbor)
    return sorted(reached)


def _polish_attempt_key(root, selected, window, tau, pop_thresh, max_nodes):
    """Run-local identity; graph, counts and solver settings stay fixed in a run.

    Include the actual incumbent, not just its objective: equal-value shapes
    can expose different exchanges. An attempt is not an optimality certificate.
    """
    return (int(root), tuple(sorted(selected)), tuple(sorted(window)),
            as_fraction_tau(tau), int(pop_thresh), max_nodes)


class _RegionalExchangeState:
    """One build's shared wall-time budget and neighborhood attempt history."""

    def __init__(self, seconds):
        self.remaining_seconds = min(180.0, max(0.0, float(seconds)))
        self.attempted = set()
        self.attempt_count = 0


def _regional_exchange_windows(assignments, nb, u, *, hops=2, halo_hops=1, eligible_ids=None):
    """Nearby pairs/triples, initially with a one-hop unassigned halo.

    Neighbor discovery can cross unassigned tracts but never another ASU.
    Round-robin anchors before their alternate pairs; triples allow a transfer
    through a third group. Discovery distance is independent of halo size.
    Every incumbent tract remains eligible, including interior tracts.
    """
    ids = sorted(set(int(v) for v in assignments if v > 0))
    if eligible_ids is not None:
        ids = [k for k in ids if k in eligible_ids]
    eligible = set(ids)
    units = {k: set(np.flatnonzero(assignments == k).tolist()) for k in ids}
    value = {k: int(u[sorted(units[k])].sum()) for k in ids}
    seen = set()
    anchor_choices = []
    for anchor in sorted(ids, key=lambda k: (-value[k], k)):
        reached = set(units[anchor])
        frontier = set(reached)
        neighbors = {}
        for distance in range(1, hops + 2):
            following = set()
            for node in sorted(frontier):
                for other in nb[node]:
                    owner = int(assignments[other])
                    if owner in eligible and owner != anchor:
                        neighbors.setdefault(owner, distance)
                    elif owner <= 0 and other not in reached:
                        following.add(other)
            reached.update(following)
            frontier = following
        nearest = sorted(neighbors, key=lambda k: (neighbors[k], -value[k], k))[:2]
        choices = ([tuple([anchor] + nearest)] if len(nearest) == 2 else [])
        choices += [(anchor, other) for other in nearest]
        anchor_choices.append(choices)
    for rank in range(3):
        for choices in anchor_choices:
            if rank >= len(choices):
                continue
            choice = choices[rank]
            group = tuple(sorted(choice))
            if group in seen:
                continue
            seen.add(group)
            region = set().union(*(units[k] for k in group))
            frontier = set(region)
            for _ in range(halo_hops):
                following = {w for v in frontier for w in nb[v]
                             if assignments[w] <= 0 and w not in region}
                region.update(following)
                frontier = following
            yield group, sorted(region)


def _joint_expansion_batches(territories, nb):
    """Greedily pool up to three touching territories, in seed-priority order.

    Batches are disjoint; protected or committed territories are never added.
    Adjacency is based on territory boundaries, not just relaxed seed islands.
    """
    owner = {v: k for k, territory in enumerate(territories) for v in territory}
    adjacent = [set() for _ in territories]
    for v, k in owner.items():
        for w in nb[v]:
            other = owner.get(w)
            if other is not None and other != k:
                adjacent[k].add(other)
                adjacent[other].add(k)
    pending = set(range(len(territories)))
    batches = []
    while pending:
        batch = [min(pending)]
        pending.remove(batch[0])
        while len(batch) < 3:
            candidates = set().union(*(adjacent[k] for k in batch)) & pending
            if not candidates:
                break
            chosen = min(candidates)
            batch.append(chosen)
            pending.remove(chosen)
        batches.append(batch)
    return batches


def _regional_selection_bounds(q_surplus, group_count, *, max_nodes=None, exact_nodes=None,
                               mandatory_groups=None):
    """Rate-only count bounds; reserve tracts only for mandatory other ASUs."""
    total, _, _ = _surplus_knapsack_bounds(q_surplus, set(), len(q_surplus))
    total = len(q_surplus) if total is None else total
    mandatory = group_count if mandatory_groups is None else mandatory_groups
    per_group = max(0, total - max(0, mandatory - 1))
    cap = exact_nodes if exact_nodes is not None else max_nodes
    if cap is not None:
        per_group = min(per_group, int(cap))
        total = min(total, group_count * int(cap))
    return total, per_group


def _joint_minimum_tract_count(population, pop_thresh):
    """Necessary tract count: even the largest populations must reach threshold.

    Clamp negative entries to zero only for this optimistic bound. If all
    tracts are insufficient, n+1 excludes active groups without excluding empty
    optional groups. This is a derived bound, not a user-imposed tract cap.
    """
    pop_thresh = int(pop_thresh)  # Match the shared feasibility row exactly.
    if pop_thresh <= 0:
        return 1
    accumulated = 0
    for count, value in enumerate(sorted((max(0, int(p)) for p in population), reverse=True), 1):
        accumulated += value
        if accumulated >= pop_thresh:
            return count
    return len(population) + 1


def _joint_seed_distances(nb, seed):
    """Optimistic distance to any seed tract, not to a fixed flow root."""
    distances = [-1] * len(nb)
    queue = list(dict.fromkeys(seed))
    for v in queue:
        distances[v] = 0
    for v in queue:
        for w in nb[v]:
            if distances[w] < 0:
                distances[w] = distances[v] + 1
                queue.append(w)
    return distances


def _joint_small_separator_cuts(nb, u, q, seeds, deadline, cancelled,
                                limit=128, target_limit=128):
    """Bounded root-independent (a,b,S) cuts: x[a]+x[b] <= 1+sum(x[S]).

    Endpoints are merely witnesses, NOT mandatory tracts or fixed roots.
    Compute once for the graph and reuse across all group slots.
    """
    cuts, seen = [], set()

    def expired():
        return time.monotonic() >= deadline or cancelled()

    def add(a, b, separator):
        key = (min(a, b), max(a, b), tuple(sorted(separator)))
        if key not in seen and len(cuts) < limit:
            seen.add(key)
            cuts.append(key)

    if not nb or expired():
        return cuts
    for bridge in sorted(_articulation_points(nb, np.ones(len(nb), dtype=bool)))[:64]:
        if expired() or len(cuts) >= limit:
            break
        mask = np.ones(len(nb), dtype=bool)
        mask[bridge] = False
        neighbors = set(nb[bridge])
        sides = [c for c in _connected_components(nb, mask) if neighbors.intersection(c)]
        representatives = [max(c, key=lambda v: (int(u[v]), int(q[v]), -v)) for c in sides]
        for j, a in enumerate(representatives):
            for b in representatives[j+1:]:
                add(a, b, (bridge,))
                if len(cuts) >= limit:
                    break
            if len(cuts) >= limit:
                break
    anchors = list(dict.fromkeys(
        [max(seed, key=lambda v: (int(q[v]), int(u[v]), -v)) for seed in seeds if seed]
        + sorted(range(len(nb)), key=lambda v: (-int(u[v]), -int(q[v]), v))[:8]))[:8]
    attempts = 0
    for a in anchors:
        if expired() or len(cuts) >= limit or attempts >= target_limit:
            break
        distance = _joint_seed_distances(nb, [a])
        targets = sorted((v for v in range(len(nb)) if distance[v] > 1),
                         key=lambda v: (-distance[v], -int(u[v]), v))[:16]
        for b in targets:
            if expired() or len(cuts) >= limit or attempts >= target_limit:
                break
            attempts += 1
            separator = _bounded_root_vertex_separator(nb, a, b, 3)
            if separator:
                add(a, b, separator)
    return cuts


def _joint_connectivity_cut_pass(model, x, root_rows, nb, u,
                                 fallback, valid_candidate, deadline, workers,
                                 cancellation, *, log=False, report=None,
                                 max_rounds=8, cut_limit=1024,
                                 stage_prefix="STATEWIDE_JOINT"):
    """Solve/add root-aware cuts before flows; never publish disconnected groups.

    For v in C: x[v] <= sum(root[C]) + sum(x[boundary(C)]). A connected
    selection either roots inside C or must cross its external node boundary.
    The same model is extended with exact flows by the caller afterward.
    """
    started = time.monotonic()
    best = [list(unit) for unit in fallback]
    best_obj = sum(int(u[unit].sum()) for unit in best)
    seen, rows, rounds, status_name = set(), 0, 0, "DISABLED"
    if log:
        print(f"[STAGE] {stage_prefix}_CUT_PASS groups={len(x)} "
              f"baseline_unemp={best_obj} time_limit={max(0, deadline-started):.3f}s "
              f"max_rounds={max_rounds} cut_limit={cut_limit} workers={workers}", flush=True)
    for round_number in range(1, max_rounds + 1):
        reason = cancellation()
        remaining = deadline - time.monotonic()
        if reason or remaining <= 0 or rows >= cut_limit:
            status_name = reason or ("CUT_LIMIT" if rows >= cut_limit else "TIME_LIMIT")
            break
        scout = cp_model.CpSolver()
        scout.parameters.max_time_in_seconds = min(10.0, remaining)
        scout.parameters.num_search_workers = max(1, int(workers))
        scout.parameters.log_search_progress = bool(log)
        _configure_asu_solver_portfolio(scout.parameters, workers)
        done, interrupted = threading.Event(), []

        def watch():
            while not done.wait(.1):
                reason = cancellation()
                if reason:
                    interrupted.append(reason)
                    scout.StopSearch()
                    return

        watcher = threading.Thread(target=watch, daemon=True)
        watcher.start()
        try:
            status = scout.Solve(model)
        finally:
            done.set()
            watcher.join()
        rounds = round_number
        status_name = interrupted[0] if interrupted else scout.StatusName(status)
        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            break
        groups = [[i for i, var in enumerate(row) if scout.BooleanValue(var)] for row in x]
        connected = valid_candidate(groups)
        value = sum(int(u[unit].sum()) for unit in groups)
        if connected and value >= best_obj:
            best, best_obj = groups, value
            if report is not None:
                report([i for unit in best for i in unit], value)
        added, detached = 0, 0
        if not connected and not interrupted:
            for k, unit in enumerate(groups):
                mask = np.zeros(len(nb), dtype=bool)
                mask[unit] = True
                components = _connected_components(nb, mask)
                if len(components) <= 1:
                    continue
                detached += len(components) - 1
                for component in components:
                    if any(scout.BooleanValue(root_rows[k][v]) for v in component):
                        continue
                    region = tuple(sorted(component))
                    boundary = sorted({w for v in region for w in nb[v]} - set(region))
                    for target in sorted(region, key=lambda v: (-int(u[v]), v))[:3]:
                        key = (k, target, region)
                        if key in seen or rows >= cut_limit:
                            continue
                        seen.add(key)
                        model.Add(x[k][target] <= sum(root_rows[k][v] for v in region)
                                  + sum(x[k][v] for v in boundary))
                        rows += 1
                        added += 1
        if log:
            print(f"[STAGE] {stage_prefix}_CUT_ROUND round={round_number} "
                  f"status={status_name} relaxed_unemp={value} connected={connected} "
                  f"detached_components={detached} cuts_added={added} cuts_total={rows} "
                  f"valid_unemp={best_obj} elapsed={time.monotonic()-started:.3f}s", flush=True)
        if interrupted or connected or not added:
            break
    if log:
        print(f"[STAGE] {stage_prefix}_CUT_COMPLETE status={status_name} "
              f"rounds={rounds} cuts={rows} valid_unemp={best_obj} "
              f"elapsed={time.monotonic()-started:.3f}s", flush=True)
    return best, best_obj, status_name


def _joint_assignment_hints(model, x, roots, active, counts, selected_any,
                            units, u, E, P, tau):
    """Replace exploratory hints with a validated joint assignment (flows follow)."""
    model.ClearHints()
    positions = {v.name: i for i, v in enumerate(model.Proto().variables)}
    selected_all = {i for unit in units for i in unit}
    for i, var in enumerate(selected_any):
        model.AddHint(var, int(i in selected_all))
    order = _capacity_root_order(range(len(u)), u, E, P, tau)
    for k, unit in enumerate(units):
        selected = set(unit)
        root = _pick_capacity_root(unit, u, E, P, tau) if unit else -1
        model.AddHint(active[k], int(bool(unit)))
        model.AddHint(counts[k], len(unit))
        visited = False
        for i in order:
            model.AddHint(x[k][i], int(i in selected))
            model.AddHint(roots[k][i], int(i == root))
            visited = visited or i in selected
            prefix = model.GetIntVarFromProtoIndex(positions[f"regional_root_{k}_prefix_{i}"])
            model.AddHint(prefix, int(visited))


def _reoptimize_touching_asu_units(
    units, available_nodes, nb, u, E, P, tau, pop_thresh, seconds, workers, *,
    max_nodes=None, exact_nodes=None, stop_path=None, skip_path=None,
    attempted=None, log=False, rel_gap=None, incumbent_stall_seconds=None,
    source="partition", preview_factory=None,
):
    """Reoptimize one touching cluster; never replace a failed solve by a union.

    Other units are protected. The window contains all reachable unassigned
    tracts, not just a halo. Accept higher unemployment, or equal unemployment
    with fewer ASUs; reject equal-value boundary churn. Returned units are
    compact (empty slots removed). The second result counts accepted updates,
    not merges: an improvement can leave the number of ASUs unchanged.
    """
    original = [sorted(set(map(int, unit))) for unit in units]
    if len(original) < 2 or seconds <= 0 or _stop_requested(stop_path):
        return original, 0
    owner = {v: i for i, unit in enumerate(original) for v in unit}
    if len(owner) != sum(map(len, original)) or not all(
        component_ok(unit, u, E, P, tau, pop_thresh, nb,
                     max_nodes=max_nodes, exact_nodes=exact_nodes)
        for unit in original
    ):
        return original, 0
    adjacent = [set() for _ in original]
    for node, i in owner.items():
        adjacent[i].update(owner[v] for v in nb[node] if v in owner and owner[v] != i)
    clusters, seen = [], set()
    for i in range(len(original)):
        if i in seen:
            continue
        stack, cluster = [i], []
        seen.add(i)
        while stack:
            j = stack.pop()
            cluster.append(j)
            for other in adjacent[j] - seen:
                seen.add(other)
                stack.append(other)
        if len(cluster) > 1:
            clusters.append(sorted(cluster))
    clusters.sort(key=lambda cluster: (
        -sum(float((1 - tau) * u[original[i]].sum() - tau * E[original[i]].sum())
             for i in cluster), tuple(cluster)))
    free = set(map(int, available_nodes)) - set(owner)
    attempted = set() if attempted is None else attempted
    for cluster in clusters:
        if _stop_requested(stop_path):
            break
        seeds = [original[i] for i in cluster]
        seed_set = {v for unit in seeds for v in unit}
        allowed = free | seed_set
        reached, stack = set(seed_set), list(seed_set)
        while stack:
            for v in nb[stack.pop()]:
                if v in allowed and v not in reached:
                    reached.add(v)
                    stack.append(v)
        nodes = sorted(reached)
        signature = (tuple(sorted(tuple(unit) for unit in seeds)), tuple(nodes),
                     max_nodes, exact_nodes, float(tau), int(pop_thresh), float(seconds))
        if signature in attempted:
            if log:
                print(f"[STAGE] PARTITION_TOUCHING_JOINT_COMPLETE source={source} "
                      f"status=CACHED accepted=0 groups={len(seeds)} window={len(nodes)}",
                      flush=True)
            continue
        started = time.monotonic()
        baseline = sum(int(u[unit].sum()) for unit in seeds)
        if log:
            print(f"[STAGE] PARTITION_TOUCHING_JOINT source={source} "
                  f"groups={len(seeds)} window={len(nodes)} "
                  f"unassigned={len(reached - seed_set)} baseline_unemp={baseline} "
                  f"roots=movable consolidation=enabled graph_cuts=True "
                  f"workers={workers} seconds={seconds}",
                  flush=True)
        preview = preview_factory(nodes, seeds) if preview_factory else None
        candidate, status = _solve_regional_exchange(
            seeds, nodes, nb, u, E, P, tau, pop_thresh, seconds, workers,
            max_nodes=max_nodes, exact_nodes=exact_nodes, stop_path=stop_path,
            skip_path=skip_path, allow_inactive_seeds=True,
            allow_seed_consolidation=True, max_groups=None, tighten_model=True,
            use_joint_cuts=True, stage_prefix="PARTITION_TOUCHING_JOINT",
            log=log, rel_gap=rel_gap, incumbent_stall_seconds=incumbent_stall_seconds,
            incumbent_report_callback=preview,
        )
        selected = [int(v) for unit in candidate for v in unit]
        valid = (len(candidate) == len(seeds)
                 and len(selected) == len(set(selected))
                 and set(selected).issubset(reached)
                 and all(not unit or (set(unit).intersection(seed) and component_ok(
                     unit, u, E, P, tau, pop_thresh, nb,
                     max_nodes=max_nodes, exact_nodes=exact_nodes))
                     for unit, seed in zip(candidate, seeds)))
        objective = sum(int(u[v]) for v in selected) if valid else baseline
        active = sum(bool(unit) for unit in candidate) if valid else len(seeds)
        accepted = valid and (objective > baseline or
                              (objective == baseline and active < len(seeds)))
        if status not in ("STOPPED", "SKIPPED"):
            attempted.add(signature)
        if log:
            print(f"[STAGE] PARTITION_TOUCHING_JOINT_COMPLETE source={source} "
                  f"status={status} valid={int(bool(valid))} accepted={int(bool(accepted))} "
                  f"groups_before={len(seeds)} groups_after={active} "
                  f"deactivated={len(seeds) - active} baseline_unemp={baseline} "
                  f"unemp={objective} gain={objective - baseline} "
                  f"elapsed={time.monotonic() - started:.3f}s", flush=True)
        if accepted:
            updated = list(original)
            for i, unit in zip(cluster, candidate):
                updated[i] = sorted(unit)
            return [unit for unit in updated if unit], 1
        if status in ("STOPPED", "SKIPPED"):
            break
    return original, 0


def _solve_regional_exchange(units, nodes, nb, u, E, P, tau, pop_thresh,
                              seconds, workers, *, max_nodes=None, exact_nodes=None,
                              stop_path=None, skip_path=None, allow_inactive_seeds=False,
                              log=False, rel_gap=None, incumbent_report_callback=None,
                              incumbent_stall_seconds=None, max_groups=3,
                              allow_unseeded_groups=False, relaxed_selection_hint=None,
                              tighten_model=False, allow_seed_consolidation=False,
                              use_joint_cuts=False, stage_prefix=None):
    """Jointly maximize unemployment in 2/3 disjoint ASUs with movable roots.

    Each group retains an incumbent tract for identity, but its root can move
    to the highest-capacity selected tract. No original root is locked in place.
    Original tract variables avoid contractions that could prevent transfers.
    Full incumbent flow hints and a total objective floor protect the baseline.
    The budget includes model construction; cancellation retains valid seeds.
    With allow_inactive_seeds, 1-3 relaxed candidates are accepted. Valid seeds
    stay active; weak seeds may become valid ASUs or remain empty. Only valid
    seeds contribute to the objective floor and feasible flow hints.
    The separate statewide strategy sets max_groups=None and allows empty
    seeds: these optional group slots may form an ASU anywhere in the window.
    With allow_seed_consolidation, even valid seed slots may deactivate. Active
    seeded slots still retain an original tract; the combined objective floor
    and feasible fallback/hints remain intact. Ordinary exchanges do not opt in.
    """
    stage_prefix = stage_prefix or ("STATEWIDE_JOINT" if allow_unseeded_groups else "JOINT_EXPANSION")
    deadline = time.monotonic() + max(0.0, float(seconds))
    seeds = [sorted(set(unit)) for unit in units]
    nodes = sorted(set(nodes))
    seed_nodes = [v for unit in seeds for v in unit]
    if (len(seeds) < (1 if allow_inactive_seeds else 2)
            or (max_groups is not None and len(seeds) > max_groups)
            or (not all(seeds) and not (allow_inactive_seeds and allow_unseeded_groups))
            or len(seed_nodes) != len(set(seed_nodes))
            or not set(seed_nodes).issubset(nodes)):
        return seeds, "INVALID_SEED"
    valid_seeds = [component_ok(unit, u, E, P, tau, pop_thresh, nb,
                               max_nodes=max_nodes, exact_nodes=exact_nodes)
                   for unit in seeds]
    if not allow_inactive_seeds and not all(valid_seeds):
        return seeds, "INVALID_SEED"
    # A weak relaxed component is a candidate, never a feasible fallback.
    fallback = [seed if valid else [] for seed, valid in zip(seeds, valid_seeds)]
    required_seeds = [valid and not allow_seed_consolidation for valid in valid_seeds]
    mandatory_groups = sum(required_seeds)
    cancelled_reason = [None]

    def cancellation():
        if cancelled_reason[0]:
            return cancelled_reason[0]
        if _stop_requested(stop_path):
            cancelled_reason[0] = "STOPPED"
        elif _stop_requested(skip_path):
            _consume_flag(skip_path)
            cancelled_reason[0] = "SKIPPED"
        return cancelled_reason[0]

    reason = cancellation()
    if reason or time.monotonic() >= deadline:
        return fallback, reason or "DISABLED"
    index = {v: i for i, v in enumerate(nodes)}
    relaxed_hint = {index[int(v)] for v in
                    ([] if relaxed_selection_hint is None else relaxed_selection_hint) if int(v) in index}
    partial_hint = bool(relaxed_hint)
    local_nb = [[index[w] for w in nb[v] if w in index] for v in nodes]
    n = len(nodes)
    edges = sorted({(min(i, j), max(i, j)) for i in range(n)
                    for j in local_nb[i] if i != j})
    local_u, local_e, local_p = u[nodes], E[nodes], P[nodes]
    local_seeds = [[index[v] for v in seed] for seed in seeds]
    hint_units = [[index[v] for v in unit] for unit in fallback]
    baseline = sum(int(u[unit].sum()) for unit in fallback)

    def valid_local_candidate(groups):
        flat = [v for group in groups for v in group]
        return (len(groups) == len(seeds) and len(flat) == len(set(flat))
                and int(local_u[flat].sum()) >= baseline
                and all((not group and not required_seeds[k]) or (
                    component_ok(group, local_u, local_e, local_p, tau, pop_thresh,
                                 local_nb, max_nodes=max_nodes, exact_nodes=exact_nodes)
                    and (bool(set(group) & set(local_seeds[k]))
                         or (allow_unseeded_groups and not local_seeds[k])))
                    for k, group in enumerate(groups)))

    num, den = as_fraction_tau(tau)
    q = den * local_u.astype(np.int64) - num * local_e.astype(np.int64)
    separator_cuts = []
    if use_joint_cuts:
        separator_cuts = _joint_small_separator_cuts(
            local_nb, local_u, q, local_seeds,
            min(deadline, time.monotonic() + min(10.0, .05 * max(0.0, float(seconds)))),
            cancellation)
    total_bound, group_bound = _regional_selection_bounds(
        q, len(seeds), max_nodes=max_nodes, exact_nodes=exact_nodes,
        mandatory_groups=mandatory_groups,
    )
    minimum_count = 1
    components = [list(range(n))]
    component_of = [0] * n
    allowed_components = [set([0]) for _ in seeds]
    if tighten_model:
        minimum_count = _joint_minimum_tract_count(local_p, pop_thresh)
        group_bound = min(group_bound, max(0, total_bound - minimum_count * max(0, mandatory_groups-1)))
        components = _connected_components(local_nb, np.ones(n, dtype=bool))
        viable = set()
        for c, component in enumerate(components):
            for i in component:
                component_of[i] = c
            if (len(component) >= minimum_count
                    and sum(max(0, int(local_p[i])) for i in component) >= int(pop_thresh)):
                viable.add(c)
        allowed_components = [
            viable & {component_of[index[v]] for v in seed} if seed else viable.copy()
            for seed in seeds
        ]
    if group_bound < 1:
        return fallback, "NO_CAPACITY"
    flow_bound = group_bound - 1
    model = cp_model.CpModel()
    x = []
    for k in range(len(seeds)):
        row = []
        for i in range(n):
            if i % 256 == 0:
                reason = cancellation()
                if reason or time.monotonic() >= deadline:
                    return fallback, reason or "UNKNOWN"
            row.append(model.NewIntVar(
                0, int(component_of[i] in allowed_components[k]), f"regional_{k}_{i}"))
        x.append(row)
    selected_any = []
    for i in range(n):
        if i % 256 == 0:
            reason = cancellation()
            if reason or time.monotonic() >= deadline:
                return fallback, reason or "UNKNOWN"
        if partial_hint:
            selected_var = model.NewBoolVar(f"joint_selected_{i}")
            model.Add(selected_var == sum(row[i] for row in x))
            selected_any.append(selected_var)
            if i in relaxed_hint:
                model.AddHint(selected_var, 1)
        else:
            model.Add(sum(row[i] for row in x) <= 1)
    model.Add(sum(var for row in x for var in row) <= total_bound)
    previous_free_group = None
    active_groups, group_counts, root_rows = [], [], []
    distance_rows = separator_rows = 0
    for k, seed in enumerate(seeds):
        reason = cancellation()
        if reason or time.monotonic() >= deadline:
            return fallback, reason or "UNKNOWN"
        root_local = index[_pick_capacity_root(seed, u, E, P, tau)] if seed else 0
        selected = [index[v] for v in fallback[k]]
        selected_set = set(selected)
        row = x[k]
        active = model.NewBoolVar(f"regional_active_{k}")
        active_groups.append(active)
        if required_seeds[k]:
            model.Add(active == 1)
        if not partial_hint:
            model.AddHint(active, int(valid_seeds[k]))
        if seed:
            model.Add(sum(row[index[v]] for v in seed) >= active)
        roots = [model.NewBoolVar(f"regional_root_{k}_{i}") for i in range(n)]
        root_rows.append(roots)
        model.Add(sum(roots) == active)
        if not seed:
            # Empty slots are interchangeable. Activate them in order and sort
            # their canonical roots to remove permutations of the same groups.
            root_position = sum((i + 1) * roots[i] for i in range(n))
            if previous_free_group is not None:
                previous_active, previous_position = previous_free_group
                model.Add(active <= previous_active)
                model.Add(previous_position < root_position).OnlyEnforceIf(active)
            previous_free_group = (active, root_position)
        _add_capacity_root_order(
            model, row, roots, local_u, local_e, local_p, tau,
            hint=None if partial_hint else selected, prefix=f"regional_root_{k}",
        )
        _add_asu_feasibility_constraints(
            model, row, local_u, local_e, local_p, tau, pop_thresh,
            max_nodes=max_nodes, exact_nodes=exact_nodes, active=active,
        )
        count = model.NewIntVar(0, group_bound, f"regional_count_{k}")
        group_counts.append(count)
        model.Add(count == sum(row))
        if tighten_model:
            model.Add(count >= minimum_count * active)
            model.Add(count <= group_bound * active)
            if len(components) > 1:
                for c in allowed_components[k]:
                    component = components[c]
                    model.Add(sum(row[i] for i in component) <=
                              min(group_bound, len(component)) * sum(roots[i] for i in component))
        if use_joint_cuts:
            if seed:
                for i, distance in enumerate(_joint_seed_distances(local_nb, local_seeds[k])):
                    if i % 256 == 0:
                        reason = cancellation()
                        if reason or time.monotonic() >= deadline:
                            return fallback, reason or "UNKNOWN"
                    if distance < 0 or distance + 1 > group_bound:
                        model.Add(row[i] == 0)
                        distance_rows += 1
                    elif distance > 0:
                        model.Add(count >= (distance + 1) * row[i])
                        distance_rows += 1
            for a, b, separator in separator_cuts:
                model.Add(row[a] + row[b] <= 1 + sum(row[v] for v in separator))
                separator_rows += 1
        if not partial_hint:
            for i in range(n):
                model.AddHint(row[i], int(i in selected_set))
                model.AddHint(roots[i], int(bool(selected) and i == root_local))
            model.AddHint(count, len(selected))

    if tighten_model:
        for active, count in zip(active_groups, group_counts):
            model.Add(count + minimum_count * (sum(active_groups) - active) <= total_bound)
    if log:
        blocked = sum(sum(len(components[c]) for c in range(len(components))
                          if c not in allowed) for allowed in allowed_components)
        print(f"[STAGE] {stage_prefix}_MODEL groups={len(seeds)} tracts={n} "
              f"mandatory_groups={mandatory_groups} seed_consolidation={bool(allow_seed_consolidation)} "
              f"hint={'relaxed_selection_partial' if partial_hint else 'feasible_seeds'} "
              f"hinted_selected={len(relaxed_hint)} tightening={bool(tighten_model)} "
              f"min_tracts={minimum_count} max_tracts_derived={group_bound} "
              f"graph_cuts={bool(use_joint_cuts)} separator_cuts={separator_rows} "
              f"seed_distance_rows={distance_rows} "
              f"graph_components={len(components)} fixed_zero_assignments={blocked}", flush=True)

    objective = (sum(int(local_u[i]) * selected_any[i] for i in range(n)) if partial_hint
                 else sum(int(local_u[i]) * row[i] for row in x for i in range(n)))
    model.Add(objective >= baseline)
    model.Maximize(objective)
    if use_joint_cuts:
        remaining = max(0.0, deadline - time.monotonic())
        best, best_obj, cut_status = _joint_connectivity_cut_pass(
            model, x, root_rows, local_nb, local_u, hint_units,
            valid_local_candidate, time.monotonic() + min(60.0, .15 * remaining),
            workers, cancellation, log=log, report=incumbent_report_callback,
            stage_prefix=stage_prefix)
        if best_obj > baseline:
            fallback = [[nodes[i] for i in group] for group in best]
            baseline = best_obj
            model.Add(objective >= baseline)
            hint_units = best
            partial_hint = False
            _joint_assignment_hints(model, x, root_rows, active_groups, group_counts,
                                    selected_any, best, local_u, local_e, local_p, tau)
        if cut_status in ("STOPPED", "SKIPPED"):
            return fallback, cut_status

    # Build exact connectivity only AFTER the cut-only rounds. The cuts stay
    # in this model; disconnected relaxed assignments never become fallbacks.
    if use_joint_cuts and log:
        print(f"[STAGE] {stage_prefix}_FLOW groups={len(seeds)} "
              f"baseline_unemp={baseline} workers={workers} "
              f"remaining_seconds={max(0, deadline-time.monotonic()):.3f}", flush=True)
    for k, (row, roots, active, count) in enumerate(zip(x, root_rows, active_groups, group_counts)):
        selected = hint_units[k]
        root_local = _pick_capacity_root(selected, local_u, local_e, local_p, tau) if selected else 0
        hints = _spanning_tree_flows(selected, local_nb, root_local) if selected and not partial_hint else {}
        net = [[] for _ in nodes]
        for edge_index, (i, j) in enumerate(edges):
            if edge_index % 128 == 0:
                reason = cancellation()
                if reason or time.monotonic() >= deadline:
                    return fallback, reason or "UNKNOWN"
            edge_bound = flow_bound
            if tighten_model:
                edge_bound = (min(group_bound, len(components[component_of[i]])) - 1
                              if component_of[i] in allowed_components[k] else 0)
            flow = model.NewIntVar(-edge_bound, edge_bound, f"regional_flow_{k}_{i}_{j}")
            for endpoint in (i, j):
                model.Add(flow <= edge_bound * row[endpoint])
                model.Add(flow >= -edge_bound * row[endpoint])
            if tighten_model:
                model.Add(flow <= count - active)
                model.Add(-flow <= count - active)
            net[i].append(flow)
            net[j].append(-flow)
            if not partial_hint:
                model.AddHint(flow, hints.get((i, j), 0) - hints.get((j, i), 0))
        for i in range(n):
            if i % 256 == 0:
                reason = cancellation()
                if reason or time.monotonic() >= deadline:
                    return fallback, reason or "UNKNOWN"
            injected = model.NewIntVar(0, group_bound, f"regional_injected_{k}_{i}")
            model.Add(injected <= group_bound * roots[i])
            model.Add(sum(net[i]) == injected - row[i])
            if tighten_model:
                model.Add(injected <= count)
                model.Add(injected >= count - group_bound * (1 - roots[i]))
            if not partial_hint:
                model.AddHint(injected, len(selected) if i == root_local else 0)
    reason = cancellation()
    remaining_seconds = deadline - time.monotonic()
    if reason or remaining_seconds <= 0:
        return fallback, reason or "UNKNOWN"
    solver = cp_model.CpSolver()
    solver.parameters.max_time_in_seconds = remaining_seconds
    solver.parameters.num_search_workers = max(1, int(workers))
    solver.parameters.log_search_progress = bool(log)
    if rel_gap is not None:
        solver.parameters.relative_gap_limit = float(rel_gap)
    _configure_asu_solver_portfolio(solver.parameters, workers)
    done = threading.Event()
    interrupted = []
    last_improvement = [time.monotonic()]

    class _JointProgress(cp_model.CpSolverSolutionCallback):
        def __init__(self):
            super().__init__()
            self.best = -1
            self.last_report = float("-inf")

        def on_solution_callback(self):
            now = time.monotonic()
            value = int(round(self.ObjectiveValue()))
            if value > self.best:
                self.best = value
                last_improvement[0] = now
            if incumbent_report_callback is not None and now - self.last_report >= 60.0:
                self.last_report = now
                incumbent_report_callback(
                    [i for i in range(n) if any(self.BooleanValue(row[i]) for row in x)],
                    value,
                )

    def watch():
        while not done.wait(0.1):
            reason = cancellation()
            if (not reason and incumbent_stall_seconds is not None
                    and incumbent_stall_seconds > 0
                    and time.monotonic() - last_improvement[0] >= incumbent_stall_seconds):
                reason = "STALLED"
            if reason:
                interrupted.append(reason)
                solver.StopSearch()
                return

    watcher = threading.Thread(target=watch, daemon=True)
    watcher.start()
    try:
        status = solver.Solve(model, _JointProgress())
    finally:
        done.set()
        watcher.join()
    status_name = interrupted[0] if interrupted else solver.StatusName(status)
    if status not in (cp_model.FEASIBLE, cp_model.OPTIMAL):
        return fallback, status_name
    candidate = [[nodes[i] for i in range(n) if solver.BooleanValue(row[i])] for row in x]
    flat = [v for unit in candidate for v in unit]
    if (len(flat) != len(set(flat)) or int(u[flat].sum()) < baseline
            or not all((not unit and not required_seeds[k]) or (
                        component_ok(unit, u, E, P, tau, pop_thresh, nb,
                                     max_nodes=max_nodes,
                                     exact_nodes=exact_nodes)
                        and (bool(set(unit) & set(seed))
                             or (allow_unseeded_groups and not seed)))
                        for k, (unit, seed) in enumerate(zip(candidate, seeds)))):
        return fallback, "INVALID_RESULT"
    return candidate, status_name


def _solve_statewide_joint(nb, u, E, P, tau, pop_thresh, max_asus, seconds, workers,
                           *, seed_seconds=60.0, max_nodes=None, exact_nodes=None,
                           merge_adjacent=True, stop_path=None, skip_path=None,
                           log=False, rel_gap=None, incumbent_stall_seconds=None,
                           seed_report_callback=None, incumbent_report_callback=None,
                           use_relaxed_hint=True, tighten_model=True, initial_units=None,
                           use_graph_cuts=False):
    """One all-tract joint solve, seeded with valid components plus free slots.

    Seed search has its own budget. No sequential expansion, polishing, takeover,
    or residual solves follow this strategy. The relaxed objective is not used
    as an upper bound on the seeded multi-ASU problem.
    """
    started = time.monotonic()
    if any(not math.isfinite(float(limit)) or float(limit) < 0
           for limit in (seconds, seed_seconds)):
        raise ValueError("statewide and seed time limits must be finite and nonnegative")
    slot_count = min(len(u), max(0, int(max_asus)))
    if slot_count == 0:
        if log:
            print("[STAGE] STATEWIDE_JOINT_COMPLETE status=EMPTY slots=0 active=0 unemp=0", flush=True)
        return [], "EMPTY"
    num, den = as_fraction_tau(tau)
    surplus = den * u - num * E
    high_rate = (surplus >= 0) & ((u + E > 0) if tau > 0 else np.ones(len(u), dtype=bool))
    # Cheap valid components ensure seed-search interruption still has a fallback.
    candidates = _connected_components(nb, high_rate) if initial_units is None else []
    if initial_units is not None:
        initial_units = [list(unit) for unit in initial_units]
        flat_initial = [v for unit in initial_units for v in unit]
        if (len(initial_units) > slot_count or len(flat_initial) != len(set(flat_initial))
                or not all(component_ok(unit, u, E, P, tau, pop_thresh, nb,
                                        max_nodes=max_nodes, exact_nodes=exact_nodes)
                           for unit in initial_units)):
            raise ValueError("Warm-start groups must be disjoint valid ASUs within Max ASUs")
    relaxed_selected = []
    seed_status = "DISABLED"
    if log:
        print(f"[STAGE] STATEWIDE_JOINT_SEED tracts={len(u)} max_asus={slot_count} "
              f"source={'warm_start' if initial_units is not None else 'automatic'} "
              f"time_limit={0.0 if initial_units is not None else float(seed_seconds):.1f}s "
              f"workers={max(1, int(workers))}", flush=True)
    if _stop_requested(stop_path):
        seed_status = "STOPPED"
    elif _stop_requested(skip_path):
        _consume_flag(skip_path)
        seed_status = "SKIPPED"
    elif initial_units is not None:
        seed_status = "WARM_START"
    elif seed_seconds > 0:
        relaxed = solve_connectivity_free_relaxation(
            u, E, P, tau, pop_thresh,
            _pick_capacity_root(range(len(u)), u, E, P, tau),
            time_limit=seed_seconds, workers=workers, log=log,
            stop_path=stop_path, skip_path=skip_path,
        )
        if relaxed is not None:
            seed_status = relaxed.status
            relaxed_selected = list(relaxed.selected)
            selected = np.zeros(len(u), dtype=bool)
            selected[relaxed.selected] = True
            candidates.extend(_connected_components(nb, selected))
    valid_candidates = {
        tuple(sorted(unit)) for unit in candidates
        if component_ok(unit, u, E, P, tau, pop_thresh, nb,
                        max_nodes=max_nodes, exact_nodes=exact_nodes)
    }
    valid_seeds = [] if initial_units is None else initial_units
    occupied = {v for unit in valid_seeds for v in unit}
    for unit in sorted(valid_candidates,
                       key=lambda s: (-sum(int(surplus[v]) for v in s),
                                      -sum(int(u[v]) for v in s), s)):
        if not occupied.intersection(unit):
            valid_seeds.append(list(unit))
            occupied.update(unit)
            if len(valid_seeds) == slot_count:
                break
    baseline = sum(int(u[unit].sum()) for unit in valid_seeds)
    if seed_report_callback is not None:
        seed_report_callback(valid_seeds)
    if log:
        print(f"[STAGE] STATEWIDE_JOINT_SEED_COMPLETE status={seed_status} "
              f"valid_seeds={len(valid_seeds)} free_slots={slot_count-len(valid_seeds)} "
              f"baseline_unemp={baseline} "
              f"seed_priority={'imported_assignments' if initial_units is not None else 'q_surplus'} "
              f"elapsed={time.monotonic()-started:.3f}s", flush=True)
    groups, status = valid_seeds, seed_status
    consolidate_seeds = initial_units is not None and merge_adjacent
    if seed_status not in ("STOPPED", "SKIPPED") and not _stop_requested(stop_path):
        seeds = valid_seeds + [[] for _ in range(slot_count-len(valid_seeds))]
        edges = {tuple(sorted((i, j))) for i in range(len(u)) for j in nb[i] if i != j}
        if log:
            print(f"[STAGE] STATEWIDE_JOINT tracts={len(u)} edges={len(edges)} "
                  f"slots={slot_count} valid_seeds={len(valid_seeds)} "
                  f"baseline_unemp={baseline} assignment_vars={len(u)*slot_count} "
                  f"flow_vars={len(edges)*slot_count} workers={max(1, int(workers))} "
                  f"relaxed_hint={bool(use_relaxed_hint and relaxed_selected)} "
                  f"tightening={bool(tighten_model)} "
                  f"seed_consolidation={bool(consolidate_seeds)} "
                  f"graph_cuts={bool(use_graph_cuts)} "
                  f"time_limit={float(seconds):.1f}s incumbent_merge_check=disabled "
                  f"merge_check={'after_solve' if merge_adjacent else 'disabled'}", flush=True)
        groups, status = _solve_regional_exchange(
            seeds, list(range(len(u))), nb, u, E, P, tau, pop_thresh, seconds, workers,
            max_nodes=max_nodes, exact_nodes=exact_nodes,
            stop_path=stop_path, skip_path=skip_path, allow_inactive_seeds=True,
            max_groups=None, allow_unseeded_groups=True, log=log, rel_gap=rel_gap,
            relaxed_selection_hint=relaxed_selected if use_relaxed_hint else None,
            tighten_model=tighten_model,
            allow_seed_consolidation=consolidate_seeds,
            use_joint_cuts=use_graph_cuts,
            incumbent_stall_seconds=incumbent_stall_seconds,
            incumbent_report_callback=incumbent_report_callback,
        )
    elif _stop_requested(stop_path):
        status = "STOPPED"
    deactivated_seed_slots = sum(not unit for unit in groups[:len(valid_seeds)])
    groups = [unit for unit in groups if unit]
    # Validate again at the strategy boundary before publishing assignments.
    flat = [v for unit in groups for v in unit]
    if (len(flat) != len(set(flat)) or len(groups) > slot_count
            or sum(int(u[unit].sum()) for unit in groups) < baseline
            or not all(component_ok(unit, u, E, P, tau, pop_thresh, nb,
                                    max_nodes=max_nodes, exact_nodes=exact_nodes)
                       for unit in groups)):
        groups, status = valid_seeds, "INVALID_RESULT"
        deactivated_seed_slots = 0
    merges = 0
    if merge_adjacent and not _stop_requested(stop_path):
        groups, merges = _merge_touching_asu_units(
            groups, nb, max_nodes=exact_nodes if exact_nodes is not None else max_nodes)
    unemployment = sum(int(u[unit].sum()) for unit in groups)
    if log:
        print(f"[STAGE] STATEWIDE_JOINT_COMPLETE status={status} slots={slot_count} "
              f"active={len(groups)} unused_slots={slot_count-len(groups)} merges={merges} "
              f"deactivated_seed_slots={deactivated_seed_slots} "
              f"baseline_unemp={baseline} unemp={unemployment} gain={unemployment-baseline} "
              f"elapsed={time.monotonic()-started:.3f}s", flush=True)
    return groups, status


def _regional_exchange_pass(assignments, nb, u, E, P, tau, pop_thresh,
                            seconds, workers, *, max_nodes=None, exact_nodes=None,
                            stop_path=None, skip_path=None, log=False,
                            exchange_state=None, eligible_ids=None,
                            stop_after_gain=False, stage="REGIONAL_EXCHANGE"):
    """Share four neighborhood attempts and at most 180s across build stages.

    Each solve receives at most 60s. Only strict total gains are committed;
    other ASUs remain fixed. Rebuild neighborhoods after each accepted exchange.
    """
    current = assignments.copy()
    state = exchange_state if exchange_state is not None else _RegionalExchangeState(seconds)
    started = time.monotonic()
    deadline = started + min(state.remaining_seconds, max(0.0, float(seconds)))
    try:
        while state.attempt_count < 4:
            if time.monotonic() >= deadline or _stop_requested(stop_path):
                break
            if _stop_requested(skip_path):
                _consume_flag(skip_path)
                break
            chosen = None
            # Give distinct small neighborhoods a look before widening to two
            # hops. Identical narrow/wide windows share the attempt history.
            for halo_hops in (1, 2):
                for ids, nodes in _regional_exchange_windows(
                    current, nb, u, halo_hops=halo_hops, eligible_ids=eligible_ids
                ):
                    units = [np.flatnonzero(current == k).tolist() for k in ids]
                    # Identity survives provisional/final ASU relabeling.
                    key = (tuple(sorted(tuple(unit) for unit in units)), tuple(nodes),
                           as_fraction_tau(tau), int(pop_thresh), max_nodes, exact_nodes)
                    if key not in state.attempted:
                        chosen = ids, nodes, units, key
                        break
                if chosen is not None:
                    break
            if chosen is None:
                break
            ids, nodes, units, key = chosen
            seconds_left = min(60.0, deadline - time.monotonic())
            if seconds_left <= 0:
                break
            state.attempt_count += 1
            if log:
                print(f"[STAGE] {stage} attempt={state.attempt_count} asus={ids} "
                      f"tracts={len(nodes)} seconds={seconds_left:.1f}", flush=True)
            candidate, status = _solve_regional_exchange(
                units, nodes, nb, u, E, P, tau, pop_thresh, seconds_left, workers,
                max_nodes=max_nodes, exact_nodes=exact_nodes,
                stop_path=stop_path, skip_path=skip_path,
            )
            if status not in ("STOPPED", "SKIPPED", "DISABLED", "INVALID_SEED", "INVALID_RESULT"):
                state.attempted.add(key)
            flat = [v for unit in candidate for v in unit]
            valid = (len(candidate) == len(ids) and len(flat) == len(set(flat))
                     and set(flat).issubset(nodes)
                     and all(component_ok(unit, u, E, P, tau, pop_thresh, nb,
                                          max_nodes=max_nodes, exact_nodes=exact_nodes)
                             and bool(set(unit) & set(seed))
                             for unit, seed in zip(candidate, units)))
            before = sum(int(u[unit].sum()) for unit in units)
            gain = int(u[flat].sum()) - before if valid else 0
            if valid and gain > 0:
                trial = current.copy()
                trial[np.isin(trial, ids)] = -1
                for k, unit in zip(ids, candidate):
                    trial[unit] = k
                current = trial
            if log:
                action = "accepted" if valid and gain > 0 else "retained incumbent"
                print(f"[REGIONAL EXCHANGE] {action}: asus={ids}, gain={gain}, "
                      f"status={status}", flush=True)
            if (status in ("STOPPED", "SKIPPED") or _stop_requested(stop_path)
                    or (stop_after_gain and valid and gain > 0)):
                break
        return current
    finally:
        state.remaining_seconds = max(0.0, state.remaining_seconds - (time.monotonic() - started))


def _repair_takeover_donor(
    donor_remaining: Sequence[int],
    available_nodes: Sequence[int],
    nb: List[List[int]],
    u: np.ndarray,
    E: np.ndarray,
    P: np.ndarray,
    tau: float,
    pop_thresh: int,
    time_limit: float,
    workers: int,
    *,
    stable_values: Optional[Sequence[str]] = None,
    rel_gap: Optional[float] = None,
    log: bool = False,
    solve_kwargs: Optional[Dict[str, object]] = None,
) -> List[int]:
    """Preserve all donor survivors first, then salvage a qualifying subset.

    The donor may use only its surviving tracts and currently unassigned nodes.
    Forcing the survivors lets the solve reconnect a split donor or add enough
    population/rate capacity to make it qualify again. If the solve cannot
    improve a still-valid donor, its surviving assignment is retained. Invalid
    donors reserve half the solve budget for a variable-root partial repair.
    """
    donor = sorted({int(node) for node in donor_remaining})
    if not donor:
        return []

    deadline = time.monotonic() + max(0.0, float(time_limit))
    extra_kwargs = dict(solve_kwargs or {})
    max_nodes = extra_kwargs.get("max_nodes")
    exact_nodes = extra_kwargs.get("exact_nodes")
    allowed = set(donor) | {int(node) for node in available_nodes}

    def valid(nodes):
        return (bool(set(nodes) & set(donor)) and set(nodes).issubset(allowed)
                and component_ok(nodes, u, E, P, tau, pop_thresh, nb,
                                 max_nodes=max_nodes, exact_nodes=exact_nodes))

    donor_valid = valid(donor)

    def salvage():
        # Retain an already qualifying piece even if the search times out.
        mask = np.zeros(len(nb), dtype=bool)
        mask[donor] = True
        pieces = [sorted(piece) for piece in _connected_components(nb, mask)]
        candidates = [piece for piece in pieces if valid(piece)]
        best = max(candidates, key=lambda nodes: (int(u[nodes].sum()), -len(nodes)), default=[])
        seconds = deadline - time.monotonic()
        if (seconds <= 0 or _stop_requested(extra_kwargs.get("stop_flag_path"))
                or _stop_requested(extra_kwargs.get("skip_flag_path"))):
            return best
        repaired, status = _search_unassigned_asu(
            sorted(allowed), nb, u, E, P, tau, pop_thresh,
            seconds=seconds, workers=workers,
            stop_path=extra_kwargs.get("stop_flag_path"),
            skip_path=extra_kwargs.get("skip_flag_path"),
            donor_nodes=donor, max_nodes=max_nodes, exact_nodes=exact_nodes,
        )
        if valid(repaired) and (not best or int(u[repaired].sum()) > int(u[best].sum())):
            best = repaired
        if log:
            print(f"  donor partial salvage: {status}, retained unemployment="
                  f"{int(u[best].sum()) if best else 0}", flush=True)
        return best

    root_global = _pick_capacity_root(donor, u, E, P, tau)

    reachable = {root_global}
    queue = [root_global]
    head = 0
    while head < len(queue):
        node = queue[head]
        head += 1
        for neighbor in nb[node]:
            if neighbor in allowed and neighbor not in reachable:
                reachable.add(neighbor)
                queue.append(neighbor)

    # A forced survivor outside the root's available component makes repair
    # impossible because no allowed connector can join the pieces.
    if not set(donor).issubset(reachable):
        return salvage()

    sub = sorted(reachable)
    local_index = {global_node: local_node for local_node, global_node in enumerate(sub)}
    nb_local = [
        sorted(
            local_index[neighbor]
            for neighbor in nb[global_node]
            if neighbor in local_index
        )
        for global_node in sub
    ]
    donor_local = sorted(local_index[node] for node in donor)
    root_local = int(local_index[root_global])
    stable = (
        [str(stable_values[node]) for node in sub]
        if stable_values is not None
        else [str(node).zfill(12) for node in sub]
    )
    stable_order = sorted(range(len(sub)), key=lambda node: (stable[node], node))
    tie_break_rank = [0] * len(sub)
    for rank, local_node in enumerate(stable_order):
        tie_break_rank[local_node] = rank

    remaining = max(0.0, deadline - time.monotonic())
    if remaining <= 0:
        return donor if donor_valid else salvage()
    result = solve_one_asu_cpsat(
        nb_local=nb_local,
        u_g=u[sub],
        E_g=E[sub],
        P_g=P[sub],
        tau=tau,
        pop_thresh=pop_thresh,
        root_local=root_local,
        time_limit=remaining if donor_valid else remaining / 2.0,
        workers=workers,
        rel_gap=rel_gap,
        log=log,
        hint=donor_local,
        hint_obj=int(u[donor].sum()) if donor_valid else None,
        forced_selected=donor_local,
        tie_break_rank=tie_break_rank,
        **extra_kwargs,
    )
    if result is None:
        return donor if donor_valid else salvage()

    repaired = sorted(sub[local_node] for local_node in result.sel_idx_local)
    if valid(repaired) and set(donor).issubset(repaired):
        if donor_valid and int(u[repaired].sum()) < int(u[donor].sum()):
            return donor
        return repaired
    return donor if donor_valid else salvage()


def solve_asu_graph_cut_only(
    nb_local: List[List[int]],
    u_g: np.ndarray,
    E_g: np.ndarray,
    P_g: np.ndarray,
    tau: float,
    pop_thresh: int,
    root_local: int,
    time_limit: float,
    workers: int = 8,
    log: bool = True,
    hint: Optional[Sequence[int]] = None,
    forced_selected: Optional[Sequence[int]] = None,
    max_nodes: Optional[int] = None,
    objective_upper_bound: Optional[int] = None,
    initial_connectivity_cuts: Optional[
        Sequence[Tuple[Sequence[int], Sequence[int]]]
    ] = None,
    initial_components: Optional[Sequence[Sequence[int]]] = None,
    stall_rounds: Optional[int] = 5,
) -> Optional["CpsatResult"]:
    """
    Standalone ASU solver that enforces connectivity through iterative
    constraint-generation vertex-separator cuts -- no exact flow-based
    connectivity phase at all. Unlike the cut pre-pass inside
    `solve_one_asu_cpsat`, there is no round cap: each round either adds
    representative separator cuts for components disconnected from the root
    are disconnected from the root, or (once a connected candidate is found)
    tightens the objective floor to strictly require a better one next round.
    The loop stops when the model goes INFEASIBLE (the current incumbent is
    then provably optimal), when `time_limit` is exhausted, or -- unless
    `stall_rounds` is `None` -- once `stall_rounds` consecutive DISCONNECTED
    rounds pass without the cut components shrinking (the smallest
    disconnected-component count seen since the last incumbent improvement).
    A stall only ends the search early; whatever `best_connected` incumbent
    was already found is still returned. Intended to be run against a
    connectivity-free relaxation's selection/bound/cuts as a completely
    graph-cut repair path used while preparing a window hint.
    """
    N = len(nb_local)
    if N == 0:
        return None

    nb_c, u_c, E_c, P_c, expand_c, node_map_c = contract_high_ur_nodes(nb_local, u_g, E_g, P_g, tau)
    nb_local_orig, u_g_orig, root_local_orig = nb_local, u_g, root_local
    E_g_orig, P_g_orig = E_g, P_g
    required_orig = sorted({int(root_local)} | set(forced_selected or []))
    nb_local, u_g, E_g, P_g = nb_c, u_c, E_c, P_c
    N = len(nb_local)
    root_local = int(node_map_c[root_local_orig])

    def _to_orig(sel: Optional[List[int]], status: str) -> Optional["CpsatResult"]:
        if not sel:
            return None
        orig = sorted({v for ri in sel for v in expand_c[ri]})
        if not component_ok(orig, u_g_orig, E_g_orig, P_g_orig, tau, pop_thresh,
                            nb_local_orig, required=required_orig, max_nodes=max_nodes):
            raise ValueError("Graph-cut solve returned an invalid expanded ASU")
        return CpsatResult(orig, root_local_orig, int(u_g_orig[orig].sum()), status)

    forced_set = {int(node_map_c[int(v)]) for v in (forced_selected or [])}
    forced_set.add(root_local)

    # Seed round 0 with the caller's own smallest-first component ordering
    # (from its "best fixed candidate") instead of letting the first free
    # solve default to whatever biggest blob maximizes the objective.
    initial_pending: List[set] = []
    for component_orig in (initial_components or []):
        component_c = {
            int(node_map_c[int(v)]) for v in component_orig
            if 0 <= int(v) < len(node_map_c)
        } - forced_set
        if component_c:
            initial_pending.append(component_c)

    def _component_key(component: set) -> Tuple[int, int]:
        # Smallest tract count first; ties broken by smallest unemployment.
        return (len(component), int(u_g[list(component)].sum()))

    initial_pending.sort(key=_component_key)

    hint_c: Optional[List[int]] = None
    if hint is not None:
        candidate_hint = sorted({
            int(node_map_c[int(v)]) for v in hint if 0 <= int(v) < len(node_map_c)
        })
        expanded_hint = sorted({v for ri in candidate_hint for v in expand_c[ri]})
        if component_ok(
            expanded_hint, u_g_orig, E_g_orig, P_g_orig, tau, pop_thresh,
            nb_local_orig, required=required_orig, max_nodes=max_nodes,
        ):
            hint_c = candidate_hint

    model = cp_model.CpModel()
    x = [model.NewBoolVar(f"x_{i}") for i in range(N)]
    for i in forced_set:
        model.Add(x[i] == 1)

    for v in range(N):
        if v == root_local:
            continue
        if nb_local[v]:
            model.Add(x[v] <= sum(x[w] for w in nb_local[v]))
        else:
            model.Add(x[v] == 0)

    seen_cut_signatures: set = set()
    separator_pool: Dict[int, List[frozenset]] = {}
    for component_orig, boundary_orig in (initial_connectivity_cuts or []):
        component_c = {
            int(node_map_c[int(node)])
            for node in component_orig
            if 0 <= int(node) < len(node_map_c)
        }
        if not component_c or root_local in component_c:
            continue
        boundary_c = {
            int(node_map_c[int(node)])
            for node in boundary_orig
            if 0 <= int(node) < len(node_map_c)
        }
        boundary_tuple = tuple(sorted(boundary_c))
        for node in sorted(component_c):
            if node in boundary_c:
                continue
            cut_key = (node, boundary_tuple)
            if cut_key in seen_cut_signatures:
                continue
            seen_cut_signatures.add(cut_key)
            if boundary_tuple:
                model.AddBoolOr([x[node].Not()] + [x[boundary] for boundary in boundary_tuple])
                separator = frozenset(
                    boundary for boundary in boundary_tuple
                    if boundary != root_local and boundary != node
                )
                if separator:
                    separator_pool.setdefault(node, []).append(separator)
            else:
                model.Add(x[node] == 0)

    num, den = as_fraction_tau(tau)
    q_surplus = np.asarray(den * u_g - num * E_g, dtype=np.int64)
    pop_expr, lhs = _add_asu_feasibility_constraints(
        model, x, u_g, E_g, P_g, tau, pop_thresh,
        tract_counts=[len(grp) for grp in expand_c], max_nodes=max_nodes,
    )

    obj_expr = sum(int(u_g[i]) * x[i] for i in range(N))
    model.Maximize(obj_expr)

    if objective_upper_bound is not None:
        model.Add(obj_expr <= int(objective_upper_bound))

    if hint_c is not None:
        hint_set = set(hint_c)
        for i in range(N):
            model.AddHint(x[i], 1 if i in hint_set else 0)

    best_connected: Optional[List[int]] = hint_c
    best_obj = int(u_g[best_connected].sum()) if best_connected else -1
    if best_obj >= 0:
        model.Add(obj_expr >= best_obj)

    start_time = time.monotonic()
    cut_round = 0
    status_name = "FEASIBLE"
    pending_expand: List[set] = initial_pending
    best_num_components: Optional[int] = None
    stall_count = 0
    root_distances = _root_graph_distances(nb_local, root_local)
    separator_attempts = 0
    separator_accepted = 0
    separator_duplicates = 0
    separator_dominated = 0
    separator_superseded = 0
    fallback_clauses = 0
    _DYNAMIC_SEPARATOR_MAX = 16
    _DYNAMIC_TARGETS_PER_COMPONENT = 3

    def _set_expand_hint(nodes: set) -> None:
        hint = model.Proto().solution_hint
        hint.vars.clear()
        hint.values.clear()
        for i in range(N):
            model.AddHint(x[i], 1 if i in nodes else 0)

    def _pick_component_targets(component: set) -> List[int]:
        u_target = max(component, key=lambda node: (int(u_g[node]), -int(node)))
        q_target = max(
            component,
            key=lambda node: (int(q_surplus[node]), int(u_g[node]), -int(node)),
        )
        far_target = max(
            component,
            key=lambda node: (int(root_distances[node]), int(u_g[node]), -int(node)),
        )
        targets = list(dict.fromkeys([int(q_target), int(u_target), int(far_target)]))
        return targets[:_DYNAMIC_TARGETS_PER_COMPONENT]

    def _register_separator(target: int, separator: Sequence[int]) -> Optional[Tuple[int, ...]]:
        nonlocal separator_attempts, separator_accepted
        nonlocal separator_duplicates, separator_dominated, separator_superseded
        separator_attempts += 1
        candidate = frozenset(
            int(node) for node in separator
            if int(node) != root_local and int(node) != int(target)
        )
        if not candidate:
            return None
        existing = separator_pool.setdefault(int(target), [])
        for prior in existing:
            if prior == candidate:
                separator_duplicates += 1
                return None
            if prior.issubset(candidate):
                separator_dominated += 1
                return None
        kept = [prior for prior in existing if not candidate.issubset(prior)]
        separator_superseded += len(existing) - len(kept)
        separator_pool[int(target)] = kept + [candidate]
        separator_accepted += 1
        return tuple(sorted(candidate))

    class _GraphCutProgress(cp_model.CpSolverSolutionCallback):
        def __init__(self, log_enabled: bool, round_no: int, start: float) -> None:
            super().__init__()
            self._log = log_enabled
            self._round = round_no
            self._start = start
            self._last_print = 0.0
            self._best = -1

        def on_solution_callback(self) -> None:
            if not self._log:
                return
            obj = int(round(self.ObjectiveValue()))
            if obj <= self._best:
                return
            now = time.monotonic()
            if now - self._last_print < 2.0:
                return
            self._best = obj
            self._last_print = now
            print(
                f"    [graph-cut] round {self._round}: candidate unemp={obj}, "
                f"elapsed={now - self._start:.1f}s",
                flush=True,
            )

    while True:
        remaining = float(time_limit) - (time.monotonic() - start_time)
        if remaining <= 0:
            break

        if pending_expand:
            # Nudge the search toward absorbing the smallest still-disconnected
            # component -- a hint only, so tracts can still be swapped freely.
            expand_component = pending_expand.pop(0)
            expand_base = set(best_connected) if best_connected is not None else set(forced_set)
            _set_expand_hint(expand_base | expand_component)
            if log:
                print(
                    f"  [graph-cut] round {cut_round}: expand attempt, "
                    f"smallest pending component size={len(expand_component)}, "
                    f"{len(pending_expand)} more pending",
                    flush=True,
                )

        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = max(1, int(workers))
        solver.parameters.max_time_in_seconds = remaining
        solver.parameters.log_search_progress = False
        solver.parameters.cp_model_presolve = True
        solver.parameters.linearization_level = 2
        _configure_asu_solver_portfolio(solver.parameters, workers)

        progress = _GraphCutProgress(log, cut_round, start_time)
        status = solver.Solve(model, progress)
        if status == cp_model.INFEASIBLE:
            # The last objective floor bump can't be beaten -- best_connected is optimal.
            status_name = "OPTIMAL"
            break
        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            break

        selected = [i for i in range(N) if solver.BooleanValue(x[i])]
        selected_set = set(selected)
        selected_mask = np.zeros(N, dtype=bool)
        selected_mask[selected] = True
        # One igraph call replaces the old two-pass root-BFS + unseen-BFS: it
        # partitions the whole selection at once, root's own component included.
        all_components = _connected_components(nb_local, selected_mask)
        root_component = next(
            (set(c) for c in all_components if root_local in c), {root_local}
        )

        if len(root_component) == len(selected):
            best_connected = selected
            best_obj = int(round(solver.ObjectiveValue()))
            pending_expand = []
            best_num_components = None
            stall_count = 0
            if log:
                print(
                    f"  [graph-cut] round {cut_round}: CONNECTED, unemp={best_obj}, "
                    f"elapsed={time.monotonic() - start_time:.1f}s",
                    flush=True,
                )
            if status == cp_model.OPTIMAL:
                status_name = "OPTIMAL"
                break
            # Force next round to beat this incumbent or prove it's optimal.
            model.Add(obj_expr >= best_obj + 1)
            cut_round += 1
            continue

        # A disconnected result can't be the answer, but its components can
        # often be corridor-connected into a real (if suboptimal) feasible
        # ASU right now -- reuse the same repair used elsewhere, and treat it
        # like a connected round if it beats the current incumbent.
        repair_fallback = best_connected if best_connected is not None else sorted(forced_set)
        repaired = repair_connectivity_free_selection(
            selected, repair_fallback, nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local,
            forced_selected=sorted(forced_set), max_nodes=max_nodes, log=log,
            deadline=start_time + float(time_limit),
        )
        if component_ok(repaired, u_g, E_g, P_g, tau, pop_thresh, nb_local) and (
            max_nodes is None or len(repaired) <= int(max_nodes)
        ):
            repaired_obj = int(u_g[repaired].sum())
            if repaired_obj > best_obj:
                best_connected = repaired
                best_obj = repaired_obj
                if log:
                    print(
                        f"  [graph-cut] round {cut_round}: repaired disconnected "
                        f"candidate -> CONNECTED unemp={best_obj}, "
                        f"elapsed={time.monotonic() - start_time:.1f}s",
                        flush=True,
                    )
                model.Add(obj_expr >= best_obj + 1)

        components: List[set] = [set(c) for c in all_components if root_local not in c]
        round_cuts_added = 0
        for component in components:
            boundary = sorted({
                w for v in component for w in nb_local[v]
                if w not in component
            })
            targets = _pick_component_targets(component)
            dynamic_added = 0
            for target in targets:
                separator = _minimum_root_vertex_separator(
                    nb_local,
                    root_local,
                    target,
                    protected_nodes=selected_set,
                    max_size=_DYNAMIC_SEPARATOR_MAX,
                )
                if separator is None:
                    continue
                kept = _register_separator(target, separator)
                if not kept:
                    continue
                model.AddBoolOr([x[target].Not()] + [x[node] for node in kept])
                dynamic_added += 1
                round_cuts_added += 1

            if dynamic_added:
                continue

            # If a small minimum separator was unavailable or already present,
            # retain a valid representative boundary cut. This keeps progress
            # without restoring the old per-node, per-component clause flood.
            boundary_tuple = tuple(boundary)
            for target in targets:
                cut_key = (target, boundary_tuple)
                if cut_key in seen_cut_signatures:
                    continue
                seen_cut_signatures.add(cut_key)
                if boundary_tuple:
                    model.AddBoolOr([x[target].Not()] + [x[node] for node in boundary_tuple])
                else:
                    model.Add(x[target] == 0)
                fallback_clauses += 1
                round_cuts_added += 1
        num_components = len(components)
        # Try the smallest disconnected component first next round -- cheapest
        # to absorb, and most likely for a CP-SAT-guided swap to succeed.
        # Ties (equal tract count) break toward the smallest unemployment.
        pending_expand = sorted(components, key=_component_key)
        if best_num_components is None or num_components < best_num_components:
            best_num_components = num_components
            stall_count = 0
        else:
            stall_count += 1
        if log:
            best_text = str(best_obj) if best_obj >= 0 else "none"
            print(
                f"  [graph-cut] round {cut_round}: DISCONNECTED "
                f"({num_components} component(s), {round_cuts_added} cut(s) added), "
                f"best_connected so far={best_text}, "
                f"elapsed={time.monotonic() - start_time:.1f}s",
                flush=True,
            )
        cut_round += 1
        if stall_rounds is not None and stall_count >= stall_rounds:
            if log:
                print(
                    f"  [graph-cut] stalled: component count hasn't improved in "
                    f"{stall_count} round(s); stopping early",
                    flush=True,
                )
            break

    if log:
        elapsed = time.monotonic() - start_time
        obj_text = str(best_obj) if best_obj >= 0 else "none"
        print(
            f"  graph-cut-only solve: {cut_round} round(s), {elapsed:.1f}s, "
            f"status={status_name if best_connected is not None else 'INFEASIBLE'}, "
            f"best unemp={obj_text}; separator attempts={separator_attempts}, "
            f"accepted={separator_accepted}, duplicates={separator_duplicates}, "
            f"dominated={separator_dominated}, superseded={separator_superseded}, "
            f"fallback={fallback_clauses}",
            flush=True,
        )

    if best_connected is None:
        return None
    return _to_orig(best_connected, status_name)


def _prepare_window_hint(
    nb_local: List[List[int]], u_g: np.ndarray, E_g: np.ndarray, P_g: np.ndarray,
    tau: float, pop_thresh: int, root_local: int, verbose: bool = False,
    max_nodes: Optional[int] = None,
    use_connectivity_free_repair: bool = False,
    connectivity_free_time_limit: float = 10.0,
    workers: int = 8,
    harvest_connectivity_free_asus: bool = False,
    harvest_all_connectivity_free_components: bool = False,
    use_graph_cut_repair: bool = False,
    graph_cut_repair_time_limit: float = 30.0,
) -> Dict:
    """
    Build a warm-start hint using reverse_prune on the original graph, then refine
    with improve_by_trades and articulation rerouting.
    Contraction is retained only to derive root_component and cluster_groups.
    `max_nodes`, when given, keeps the refined hint's greedy-add phase from
    growing past that many tracts (swaps still keep size constant).

    `harvest_all_connectivity_free_components`, when True alongside
    `harvest_connectivity_free_asus`, seeds the standalone-expansion territory
    partition with every relaxed component, not only the ones that already
    independently satisfy population/UR on their own -- see the caller for the
    validity gate required before committing an expanded, previously-infeasible
    seed as an ASU.
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

    connectivity_free_status: Optional[str] = None
    connectivity_free_upper_bound: Optional[int] = None
    connectivity_free_infeasible = False
    connectivity_free_components: List[ConnectivityFreeComponent] = []
    connectivity_free_cuts: List[Tuple[Tuple[int, ...], Tuple[int, ...]]] = []
    connectivity_free_candidate_count = 0
    connectivity_free_relaxed_objective: Optional[int] = None
    connectivity_free_repaired_objective: Optional[int] = None
    connectivity_free_proves_optimal = False
    connectivity_free_standalone_asus: List[List[int]] = []
    best_fixed_components: List[List[int]] = []

    if use_connectivity_free_repair or harvest_connectivity_free_asus or use_graph_cut_repair:
        objective_floor = None
        if (
            best["hint_valid"]
            and (max_nodes is None or len(best["hint_improved"]) <= max_nodes)
        ):
            objective_floor = best["hint_obj_val"]
        if verbose:
            print(
                f"  [heuristic] connectivity-free relaxation "
                f"({connectivity_free_time_limit:.1f}s) ...",
                flush=True,
            )
        relaxed = solve_connectivity_free_relaxation(
            u_g, E_g, P_g, tau, pop_thresh, root_local,
            forced_selected=root_component,
            max_nodes=None if harvest_connectivity_free_asus else max_nodes,
            time_limit=connectivity_free_time_limit,
            workers=workers,
            objective_floor=objective_floor,
            log=verbose,
        )
        if relaxed is not None:
            connectivity_free_status = relaxed.status
            connectivity_free_infeasible = relaxed.status == "INFEASIBLE"
            if (
                relaxed.status in ("OPTIMAL", "FEASIBLE")
                and relaxed.best_bound is not None
                and math.isfinite(relaxed.best_bound)
            ):
                connectivity_free_upper_bound = int(
                    math.floor(relaxed.best_bound + 1e-6)
                )

            best_repaired: Optional[List[int]] = None
            best_repaired_source: Optional[ConnectivityFreeCandidate] = None
            if relaxed.objective is not None:
                connectivity_free_relaxed_objective = relaxed.objective
                connectivity_free_components = _analyze_connectivity_free_components(
                    relaxed.selected,
                    nb_local,
                    u_g,
                    E_g,
                    P_g,
                    tau,
                    pop_thresh,
                    root_local,
                    include_connectors=not harvest_connectivity_free_asus,
                )
                connectivity_free_cuts = [
                    (tuple(component.nodes), tuple(component.boundary))
                    for component in connectivity_free_components
                    if not component.contains_root
                ]
                relaxed_candidates = relaxed.candidates or [
                    ConnectivityFreeCandidate(relaxed.selected, relaxed.objective)
                ]
                connectivity_free_candidate_count = len(relaxed_candidates)
                if harvest_connectivity_free_asus:
                    eligible_components = (
                        connectivity_free_components
                        if harvest_all_connectivity_free_components
                        else [
                            component for component in connectivity_free_components
                            if component.independently_feasible
                        ]
                    )
                    connectivity_free_standalone_asus = [
                        component.nodes
                        for component in eligible_components
                        if (
                            max_nodes is None
                            or len(component.nodes) <= int(max_nodes)
                        )
                    ]

                if use_connectivity_free_repair and not connectivity_free_standalone_asus:
                    fallback = best["hint_improved"] if best["hint_valid"] else []
                    num_cf, den_cf = as_fraction_tau(tau)
                    slack_cf = (
                        den_cf * u_g.astype(np.int64)
                        - num_cf * E_g.astype(np.int64)
                    )
                    if verbose:
                        print(
                            f"  [heuristic] connectivity-free repair "
                            f"({len(relaxed_candidates)} candidate(s)) ...",
                            flush=True,
                        )
                    for cand_idx, relaxed_candidate in enumerate(relaxed_candidates):
                        repair_started = time.monotonic()
                        repaired_candidate = repair_connectivity_free_selection(
                            relaxed_candidate.selected,
                            fallback,
                            nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local,
                            forced_selected=root_component,
                            max_nodes=max_nodes, log=verbose,
                        )
                        if verbose:
                            print(
                                f"    [connectivity-free-repair] candidate "
                                f"{cand_idx + 1}/{len(relaxed_candidates)}: "
                                f"{time.monotonic() - repair_started:.1f}s",
                                flush=True,
                            )
                        repaired_valid = component_ok(
                            repaired_candidate,
                            u_g,
                            E_g,
                            P_g,
                            tau,
                            pop_thresh,
                            nb_local,
                        ) and (
                            max_nodes is None or len(repaired_candidate) <= max_nodes
                        )
                        if not repaired_valid:
                            continue
                        if (
                            best_repaired is None
                            or _selection_key(set(repaired_candidate), u_g, slack_cf)
                            > _selection_key(set(best_repaired), u_g, slack_cf)
                        ):
                            best_repaired = repaired_candidate
                            best_repaired_source = relaxed_candidate

                if best_repaired is not None:
                    connectivity_free_repaired_objective = int(
                        u_g[best_repaired].sum()
                    )
                    if (
                        not best["hint_valid"]
                        or _selection_key(set(best_repaired), u_g, slack_cf)
                        > _selection_key(set(best["hint_improved"]), u_g, slack_cf)
                    ):
                        best = {
                            "hint_improved": best_repaired,
                            "hint_valid": True,
                            "hint_obj_val": connectivity_free_repaired_objective,
                        }
                        hint_source = "connectivity_free_repair"

                # Separate experiment: re-optimize via lazy vertex-separator
                # cuts alone (no flow phase), seeded from this same relaxation.
                # Independent of repair_connectivity_free_selection above --
                # does not consume or alter its output.
                best_graph_cut: Optional[List[int]] = None
                # Once harvest has already carved off independently-feasible
                # standalone ASUs from this relaxation, each one is expanded
                # within its own disjoint territory (via solve_one_asu_cpsat's
                # own cut-pass) instead -- a whole-graph graph-cut-only solve
                # here would just be discarded, since this window's own main
                # solve is skipped once standalone units are harvested.
                run_graph_cut_repair = use_graph_cut_repair and not (
                    harvest_connectivity_free_asus and connectivity_free_standalone_asus
                )
                if use_graph_cut_repair and not run_graph_cut_repair and verbose:
                    print(
                        "  [heuristic] graph-cut-only solve skipped: "
                        f"{len(connectivity_free_standalone_asus)} candidate seed(s) "
                        "selected for harvest expansion",
                        flush=True,
                    )
                if run_graph_cut_repair:
                    graph_cut_hint = (
                        best_repaired if best_repaired is not None
                        else (best["hint_improved"] if best["hint_valid"] else None)
                    )

                    # Pick the relaxed candidate ranked best by (fewest
                    # components, highest unemp, largest smallest-component)
                    # and seed round 0 with its own components, smallest
                    # first, instead of letting the first free solve default
                    # to one big blob.
                    best_fixed_components: List[List[int]] = []
                    if relaxed_candidates:
                        ranked = []
                        for candidate in relaxed_candidates:
                            comps = _analyze_connectivity_free_components(
                                candidate.selected, nb_local, u_g, E_g, P_g, tau,
                                pop_thresh, root_local, include_connectors=False,
                            )
                            non_root_sizes = [
                                len(c.nodes) for c in comps if not c.contains_root
                            ]
                            smallest_non_root = min(non_root_sizes) if non_root_sizes else 0
                            rank = (len(comps), -candidate.objective, -smallest_non_root)
                            ranked.append((rank, candidate, comps))
                        best_rank, best_fixed_candidate, best_comps = min(
                            ranked, key=lambda item: item[0]
                        )
                        best_fixed_components = [
                            component.nodes for component in best_comps
                            if not component.contains_root
                        ]
                        if verbose:
                            print(
                                f"  [heuristic] graph-cut best fixed candidate: "
                                f"components={best_rank[0]}, "
                                f"unemp={best_fixed_candidate.objective}, "
                                f"smallest_component={-best_rank[2]}",
                                flush=True,
                            )

                    if verbose:
                        print(
                            f"  [heuristic] graph-cut-only solve "
                            f"({graph_cut_repair_time_limit:.1f}s) ...",
                            flush=True,
                        )
                    graph_cut_result = solve_asu_graph_cut_only(
                        nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local,
                        time_limit=graph_cut_repair_time_limit,
                        workers=workers,
                        log=verbose,
                        hint=graph_cut_hint,
                        forced_selected=root_component,
                        max_nodes=max_nodes,
                        objective_upper_bound=connectivity_free_upper_bound,
                        initial_connectivity_cuts=connectivity_free_cuts,
                        initial_components=best_fixed_components,
                    )
                    if graph_cut_result is not None:
                        candidate = sorted(graph_cut_result.sel_idx_local)
                        if component_ok(
                            candidate, u_g, E_g, P_g, tau, pop_thresh, nb_local
                        ) and (max_nodes is None or len(candidate) <= int(max_nodes)):
                            best_graph_cut = candidate

                if best_graph_cut is not None:
                    num_gc, den_gc = as_fraction_tau(tau)
                    slack_gc = (
                        den_gc * u_g.astype(np.int64)
                        - num_gc * E_g.astype(np.int64)
                    )
                    if (
                        not best["hint_valid"]
                        or _selection_key(set(best_graph_cut), u_g, slack_gc)
                        > _selection_key(set(best["hint_improved"]), u_g, slack_gc)
                    ):
                        best = {
                            "hint_improved": best_graph_cut,
                            "hint_valid": True,
                            "hint_obj_val": int(u_g[best_graph_cut].sum()),
                        }
                        hint_source = "graph_cut_repair"

            if verbose:
                bound_text = (
                    f"{relaxed.best_bound:.1f}"
                    if relaxed.best_bound is not None else "unknown"
                )
                repaired_text = (
                    str(connectivity_free_repaired_objective)
                    if connectivity_free_repaired_objective is not None
                    else (
                        "skipped"
                        if connectivity_free_standalone_asus
                        else "infeasible"
                    )
                )
                print(
                    f"    relaxed: status={relaxed.status}, tracts={len(relaxed.selected)}, "
                    f"unemp={relaxed.objective}, bound={bound_text}, "
                    f"candidates={connectivity_free_candidate_count}, "
                    f"components={len(connectivity_free_components)}, "
                    f"standalone_asus={len(connectivity_free_standalone_asus)}, "
                    f"solve={relaxed.solve_seconds:.2f}s; repaired={repaired_text}",
                    flush=True,
                )
                if connectivity_free_standalone_asus:
                    standalone_unemployed = sum(
                        int(u_g[nodes].sum())
                        for nodes in connectivity_free_standalone_asus
                    )
                    print(
                        f"      harvest: {len(connectivity_free_standalone_asus)} "
                        f"candidate seed(s), unemp={standalone_unemployed}; "
                        "corridor repair skipped",
                        flush=True,
                    )
                for component in connectivity_free_components[:6]:
                    labor_force = component.unemployed + component.employed
                    ur_pct = 100.0 * component.unemployed / max(labor_force, 1)
                    if component.contains_root:
                        connector_text = "root"
                    elif harvest_connectivity_free_asus:
                        connector_text = "connector=not needed"
                    elif component.connector_reachable:
                        connector_text = (
                            f"connector={len(component.connector_nodes)}, "
                            f"deficit={component.connector_deficit}"
                        )
                    else:
                        connector_text = "connector=unreachable"
                    print(
                        f"      {'root' if component.contains_root else 'island'}: "
                        f"tracts={len(component.nodes)}, unemp={component.unemployed}, "
                        f"pop={component.population}, UR={ur_pct:.2f}%, "
                        f"slack={component.exact_slack}, boundary={len(component.boundary)}, "
                        f"standalone={'yes' if component.independently_feasible else 'no'}, "
                        f"{connector_text}",
                        flush=True,
                    )
                if len(connectivity_free_components) > 6:
                    print(
                        f"      ... {len(connectivity_free_components) - 6} more component(s)",
                        flush=True,
                    )
                if best_repaired_source is not None and best_repaired is not None:
                    source_set = set(best_repaired_source.selected)
                    repaired_set = set(best_repaired)
                    upper_gap = (
                        connectivity_free_upper_bound
                        - connectivity_free_repaired_objective
                        if connectivity_free_upper_bound is not None
                        and connectivity_free_repaired_objective is not None
                        else None
                    )
                    print(
                        f"      repair source: relaxed_unemp={best_repaired_source.objective}, "
                        f"connectors_added={len(repaired_set - source_set)}, "
                        f"relaxed_dropped={len(source_set - repaired_set)}, "
                        f"certified_remaining_gap={upper_gap if upper_gap is not None else 'unknown'}",
                        flush=True,
                    )

            connectivity_free_proves_optimal = bool(
                connectivity_free_upper_bound is not None
                and best["hint_valid"]
                and best["hint_obj_val"] is not None
                and set(root_component).issubset(best["hint_improved"])
                and (max_nodes is None or len(best["hint_improved"]) <= max_nodes)
                and connectivity_free_upper_bound <= best["hint_obj_val"]
            )
            if verbose and connectivity_free_proves_optimal:
                print(
                    f"    connectivity-free bound proves connected hint optimal "
                    f"at {best['hint_obj_val']}",
                    flush=True,
                )

    return {
        "root_component": root_component,
        "n_contracted": len(nb_r),
        "hint_improved": best["hint_improved"],
        "hint_valid": best["hint_valid"],
        "hint_obj_val": best["hint_obj_val"],
        "hint_source": hint_source,
        "cluster_groups": [group for group in expand_r if len(group) > 1],
        "connectivity_free_status": connectivity_free_status,
        "connectivity_free_upper_bound": connectivity_free_upper_bound,
        "connectivity_free_infeasible": connectivity_free_infeasible,
        "connectivity_free_components": connectivity_free_components,
        "connectivity_free_cuts": connectivity_free_cuts,
        "connectivity_free_candidate_count": connectivity_free_candidate_count,
        "connectivity_free_relaxed_objective": connectivity_free_relaxed_objective,
        "connectivity_free_repaired_objective": connectivity_free_repaired_objective,
        "connectivity_free_proves_optimal": connectivity_free_proves_optimal,
        "connectivity_free_standalone_asus": connectivity_free_standalone_asus,
        "best_fixed_components": best_fixed_components,
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


def _validate_initial_asu_id(values, nb, u, E, P, tau, pop_thresh, max_asus,
                             *, max_nodes=None, exact_nodes=None):
    """Validate already aligned warm-start assignments, then compact labels.

    GEOID matching is performed by the dashboard import; this API takes exactly
    one ID per current tract. Invalid groups are never silently dropped.
    """
    if values is None:
        return np.full(len(u), -1, dtype=int)
    try:
        ids = np.asarray(values, dtype=float)
    except (TypeError, ValueError) as exc:
        raise ValueError("Warm-start ASU IDs must be integers") from exc
    if (ids.ndim != 1 or len(ids) != len(u) or not np.all(np.isfinite(ids))
            or np.any(ids != np.floor(ids)) or np.any(ids < -1)
            or np.any(ids > np.iinfo(np.int32).max)):
        raise ValueError("Warm start needs one integer ASU ID per tract; use 0 or -1 for unassigned")
    labels = np.unique(ids[ids > 0])
    if len(labels) > int(max_asus):
        raise ValueError(f"Warm start has {len(labels)} ASUs but Max ASUs is {max_asus}; increase Max ASUs")
    result = np.full(len(u), -1, dtype=int)
    invalid = []
    for new_id, old_id in enumerate(labels, 1):
        unit = np.flatnonzero(ids == old_id).tolist()
        if not component_ok(unit, u, E, P, tau, pop_thresh, nb,
                            max_nodes=max_nodes, exact_nodes=exact_nodes):
            invalid.append(str(int(old_id)))
        result[unit] = new_id
    if invalid:
        raise ValueError("Warm-start ASU(s) " + ", ".join(invalid) +
                         " fail current connectivity, population, rate, or tract-count requirements")
    return result


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
    use_articulation_edge_bounds: bool = False,
    use_distance_flow_bounds: bool = False,
    use_global_capacity_cardinality_bound: bool = False,
    use_bridge_subtree_pruning: bool = False,
    use_connectivity_free_repair: bool = False,
    connectivity_free_time_limit: float = 10.0,
    use_graph_cut_repair: bool = False,
    graph_cut_repair_time_limit: float = 30.0,
    max_nodes_per_asu: Optional[int] = None,
    exact_nodes_per_asu: Optional[int] = None,
    combine_capped_asus: bool = True,
    combine_time_limit: Optional[int] = None,
    stop_flag_path: Optional[str] = None,
    skip_flag_path: Optional[str] = None,
    harvest_connectivity_free_asus: bool = False,
    harvest_all_connectivity_free_components: bool = False,
    standalone_expansion_time_limit: float = 30.0,
    joint_partition_expansion: bool = False,
    final_asu_polish_time_limit: Optional[float] = None,
    use_flow_first_search: bool = False,
    use_tract_capacity_search: bool = False,
    use_flow_capacity_hybrid_search: bool = False,
    use_capacity_sweep: bool = False,
    capacity_sweep_time_limit: float = 30.0,
    progress_out_path: Optional[str] = None,
    incumbent_stall_seconds: Optional[float] = None,
    statewide_joint: bool = False,
    statewide_joint_time_limit: Optional[float] = None,
    statewide_seed_time_limit: float = 60.0,
    statewide_relaxed_hint: bool = True,
    statewide_tighten_model: bool = True,
    initial_asu_id: Optional[Sequence[int]] = None,
    statewide_graph_cuts: bool = False,
) -> Dict[str, np.ndarray]:
    """
    Build ASUs in batches of up to `parallel_asus` disjoint candidate windows, solved
    concurrently; `parallel_asus=1` runs every full-window ASU solve sequentially.
    Standalone-component harvest-expansion solves run sequentially and each receives
    the full configured worker budget, independent of `parallel_asus`. Two ASUs
    built in the same batch that end up touching (share a
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
    applies. This combine step repeats to a fixed point: each round can both
    absorb newly-claimed frontier tracts into a group and thereby create fresh
    queen-contiguity touches against still-other committed ASUs, so groups are
    recomputed from the current `asu_id` state and re-merged every round until
    no touching groups remain.

    `combine_time_limit`, when given, overrides `time_limit` as the CP-SAT time
    budget used specifically by this uncapped combine/re-solve pass (defaults
    to `time_limit` when `None`).

    When `harvest_connectivity_free_asus` is True, every connected component
    of the relaxed solution that independently satisfies the population, UR,
    and optional per-ASU tract-cap constraints seeds a separate ASU. Before
    commit, a bounded exact CP-SAT solve expands each seed within a connected
    territory assigned by nearest-seed graph distance. Territories are disjoint,
    so one expansion cannot consume another standalone ASU or its frontier.
    With merge_adjacent enabled, touching expansions are jointly reoptimized
    with all reachable unassigned tracts; other ASUs and pending seeds remain
    protected. Roots and boundaries can move and seed slots can deactivate.
    Higher combined unemployment, or equal unemployment with fewer ASUs, is
    accepted. No improvement leaves the original groups separate. Territories
    are repartitioned after an accepted joint update or expansion gain.
    A round with neither ends this loop; time-limited solves do not prove that
    further improvement is impossible. A final fixed-point
    pass applies the same joint rule to contacts across build batches. Each
    touching-cluster solve uses the standalone expansion limit and all workers;
    unchanged attempted neighborhoods are cached, not claimed optimal.

    After merge/expansion stabilizes, each committed ASU is polished sequentially
    against every currently unassigned tract. Its current selection is a hint and
    objective floor, not a forced set, so CP-SAT may replace tracts. Any dropped
    tracts become available to later ASUs in the same final pass. If polishing
    creates touching ASUs, jointly reoptimize each transitive touching group
    using the polish time limit. Accepted updates restart polishing. Each update
    increases total unemployment or reduces group count without losing coverage.
    Late exchanges, takeovers, and residual additions trigger this check too.
    The polish uses
    `final_asu_polish_time_limit` seconds per ASU, or the standalone expansion time
    limit when that option is `None`.

    `exact_nodes_per_asu`, when given, fixes every per-ASU CP-SAT solve (the
    main window solve and, if enabled, the standalone-expansion closure) to
    select exactly that many original-graph tracts instead of leaving size
    unconstrained or merely capped, as an experiment to see whether fixing the
    tract count speeds up solves. It takes precedence over `max_nodes_per_asu`.
    Because growing or merging an ASU afterward would break that exact-size
    invariant, the legacy touching-ASU combine phase, the final ASU polish pass, and
    the capacity-sweep phase are all skipped entirely whenever
    `exact_nodes_per_asu` is set; the uncapped-window rescue path is
    unaffected, so a window that cannot hit the exact target still falls back
    to an unconstrained solve rather than failing outright.

    `harvest_all_connectivity_free_components`, when True alongside
    `harvest_connectivity_free_asus`, seeds the territory partition/expansion
    step above with *every* relaxed component, not only the ones that already
    independently satisfy population/UR on their own. This trades a much
    finer up-front graph partition (every relaxed island gets its own bounded
    CP-SAT expansion window) for weaker seeds that may fail to expand into a
    valid ASU; any expanded unit that does not pass the same population/UR/
    connectivity check used everywhere else is not committed -- its tracts are
    released back to the remaining pool for the normal ball-growing loop to
    pick up instead. Ignored when `harvest_connectivity_free_asus` is False.

    `stop_flag_path`, when given, names a file that a running solve polls; once
    it exists, each in-flight window's CP-SAT solve halts via `stop_search()`
    and returns its current incumbent, that incumbent is committed as the final
    (partial) ASU, and no further ASU windows are started.

    `joint_partition_expansion` pools up to three neighboring partition
    territories per solve, using the standalone expansion time limit per batch.
    It is opt-in and only applies to connectivity-free harvesting. Weak seeds
    may remain inactive; valid seeds remain active with a combined objective
    floor. Batches run sequentially and touching joint checks follow each solve.

    `statewide_joint` selects a separate strategy: valid component seeds plus
    optional new groups are optimized jointly over all input tracts, using up
    to max_asus slots. The seed solve uses statewide_seed_time_limit; the joint
    model uses statewide_joint_time_limit (or time_limit when None). Valid seed
    identities and their combined objective floor are retained. Only a final
    touching-merge check follows; other build/polish/takeover stages are bypassed.

    `initial_asu_id` supplies one saved assignment per current dataframe row.
    Every positive group is validated before use; 0/-1 mean unassigned. IDs
    are compacted and imported groups count toward max_asus. The dashboard
    aligns RDS files by GEOID before supplying this vector. Statewide mode
    skips seed search and uses these valid groups for assignment/flow hints
    and its objective floor. With merge_adjacent enabled, imported slots may
    deactivate so consolidation and expansion can occur in the same solve;
    surviving seeded slots retain at least one original tract. Otherwise all
    imported slots remain active. Other strategies resume from the assignments.

    `statewide_graph_cuts` adds root-independent small separators, multi-source
    seed-distance/count rows, and a flow-free connectivity cut pre-pass (up to
    8 rounds, 60 seconds and 15% of the remaining joint budget). Only validated
    connected solutions become incumbents; all cuts carry into the exact flow
    solve. This opt-in does not change other strategies or impose tract caps.

    `skip_flag_path` similarly halts only the in-flight window(s) of the
    current batch -- the flag is consumed on detection -- so the (partial)
    incumbent is committed as its ASU and the loop continues on to build the
    next ASU window normally.

    `progress_out_path`, when given, is updated atomically with JSON snapshots
    of the current tract assignments as the run proceeds. This can drive live
    UI updates while CP-SAT is still running.

    When `use_capacity_sweep` is True, a final pass (after the main loop and
    any combine phase) tries to salvage brand-new standalone ASUs from tracts
    still left in `remaining` -- restricted entirely to the remaining-tract
    subgraph (never touching already-committed tracts, since a leftover tract
    that could improve an existing ASU would already have been captured by
    that ASU's own solves). Each round picks the remaining tract with the
    highest UR-surplus (`den*u - num*E`) as root, grows a window purely from
    other remaining tracts, and solves it with CP-SAT; it stops once no
    remaining tract has positive surplus left. `capacity_sweep_time_limit`
    bounds each individual CP-SAT solve in this pass.
    """
    custom_modes = [
        bool(use_tract_first_search),
        bool(use_flow_first_search),
        bool(use_tract_capacity_search),
        bool(use_flow_capacity_hybrid_search),
    ]
    if sum(custom_modes) > 1:
        raise ValueError("custom fixed-search workers are mutually exclusive")
    if (use_flow_first_search or use_flow_capacity_hybrid_search) and use_arborescence:
        raise ValueError("flow-first and hybrid search require an integer flow formulation")

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
    asu_id = _validate_initial_asu_id(
        initial_asu_id, nb, u, E, P, tau, pop_thresh, max_asus,
        max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu)
    initial_units = [np.flatnonzero(asu_id == label).tolist()
                     for label in np.unique(asu_id[asu_id > 0])]
    remaining = asu_id <= 0
    tried = np.zeros(n, dtype=bool)
    if initial_asu_id is not None and verbose:
        print(f"[STAGE] WARM_START accepted_asus={len(initial_units)} "
              f"assigned_tracts={int(np.sum(asu_id > 0))} "
              f"baseline_unemp={int(u[asu_id > 0].sum())}", flush=True)
    num, den = as_fraction_tau(tau)

    def _sequential_asu_ids(values: np.ndarray) -> np.ndarray:
        """Return a copy whose positive ASU labels are contiguous from 1."""
        compact = np.asarray(values, dtype=int).copy()
        positive_ids = np.unique(compact[compact > 0])
        for sequential_id, original_id in enumerate(positive_ids, start=1):
            if int(original_id) != sequential_id:
                compact[compact == original_id] = sequential_id
        return compact

    progress_write_lock = threading.Lock()

    def _emit_progress(
        phase: str,
        exploring_idx: Optional[Sequence[int]] = None,
        exploring_candidate_idx: Optional[Sequence[int]] = None,
        exploring_added_idx: Optional[Sequence[int]] = None,
        exploring_removed_idx: Optional[Sequence[int]] = None,
    ) -> None:
        if not progress_out_path:
            return
        try:
            published_asu_id = _sequential_asu_ids(asu_id)
            active_mask = published_asu_id > 0
            exploring = sorted({
                int(node)
                for node in (exploring_idx or [])
                if 0 <= int(node) < n
            })
            exploring_candidate = sorted({
                int(node)
                for node in (exploring_candidate_idx or [])
                if 0 <= int(node) < n
            })
            exploring_added = sorted({
                int(node)
                for node in (exploring_added_idx or [])
                if 0 <= int(node) < n
            })
            exploring_removed = sorted({
                int(node)
                for node in (exploring_removed_idx or [])
                if 0 <= int(node) < n
            })
            payload = {
                "phase": str(phase),
                "n_asu": int(np.unique(published_asu_id[active_mask]).size),
                "total_unemp": int(u[np.where(active_mask)[0]].sum()),
                "asu_id": published_asu_id.tolist(),
                "exploring_idx": exploring,
                "exploring_candidate_idx": exploring_candidate,
                "exploring_added_idx": exploring_added,
                "exploring_removed_idx": exploring_removed,
                "updated_at": time.time(),
            }
            with progress_write_lock:
                tmp_path = f"{progress_out_path}.tmp"
                with open(tmp_path, "w", encoding="utf-8") as handle:
                    json.dump(payload, handle)
                # On Windows the dashboard's brief read handle can prevent an
                # otherwise-atomic replacement with WinError 5/32. Preserve
                # atomic publication, but retry long enough to outlast a poll.
                replace_delays = (0.01, 0.02, 0.04, 0.08, 0.12, 0.18, 0.25, 0.30)
                for attempt, delay in enumerate(replace_delays):
                    try:
                        os.replace(tmp_path, progress_out_path)
                        break
                    except OSError as exc:
                        windows_code = getattr(exc, "winerror", None)
                        access_race = windows_code in (5, 32) or (
                            windows_code is None and getattr(exc, "errno", None) == 13
                        )
                        if not access_race or attempt == len(replace_delays) - 1:
                            raise
                        time.sleep(delay)
        except Exception as exc:
            if verbose:
                print(
                    f"  [progress] failed to write snapshot: {exc}",
                    flush=True,
                )

    _emit_progress("INIT")

    # Each concurrent territory keeps its own delta. Serialize aggregation and
    # publication together so a later writer cannot overwrite a newer preview.
    preview_lock = threading.Lock()
    preview_deltas: Dict[object, Tuple[Set[int], Set[int]]] = {}

    def _incumbent_preview(key, local_to_global, baseline_local):
        if not progress_out_path:
            return None
        mapping = tuple(int(node) for node in local_to_global)
        baseline = {mapping[int(node)] for node in baseline_local}

        def report(selected_local, objective):
            selected = {mapping[int(node)] for node in selected_local}
            with preview_lock:
                preview_deltas[key] = (selected - baseline, baseline - selected)
                added, removed = set(), set()
                for delta_added, delta_removed in preview_deltas.values():
                    added.update(delta_added)
                    removed.update(delta_removed)
                _emit_progress(
                    "INCUMBENT_PREVIEW",
                    exploring_added_idx=sorted(added),
                    exploring_removed_idx=sorted(removed - added),
                )
                if verbose:
                    preview_phase = key[0] if isinstance(key, tuple) else key
                    print(
                        f"  [incumbent map] phase={preview_phase} "
                        f"objective={int(objective)} "
                        f"added={len(selected - baseline)} "
                        f"removed={len(baseline - selected)}",
                        flush=True,
                    )
        return report

    def _clear_incumbent_previews():
        with preview_lock:
            preview_deltas.clear()
            _emit_progress("INCUMBENT_CLEAR")

    if statewide_joint:
        statewide_preview = None

        def report_statewide_seeds(units):
            nonlocal statewide_preview
            asu_id[:] = -1
            for label, unit in enumerate(units, 1):
                asu_id[unit] = label
            statewide_preview = _incumbent_preview(
                ("statewide_joint", 0), range(n), [v for unit in units for v in unit])
            _emit_progress("STATEWIDE_JOINT_SEEDS")

        def report_statewide_incumbent(selected, objective):
            if statewide_preview is not None:
                statewide_preview(selected, objective)

        groups, status = _solve_statewide_joint(
            nb, u, E, P, tau, pop_thresh, max_asus,
            time_limit if statewide_joint_time_limit is None else statewide_joint_time_limit,
            workers, seed_seconds=statewide_seed_time_limit,
            use_relaxed_hint=statewide_relaxed_hint, tighten_model=statewide_tighten_model,
            use_graph_cuts=statewide_graph_cuts,
            initial_units=initial_units if initial_asu_id is not None else None,
            max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu,
            merge_adjacent=merge_adjacent, stop_path=stop_flag_path, skip_path=skip_flag_path,
            log=verbose, rel_gap=rel_gap, incumbent_stall_seconds=incumbent_stall_seconds,
            seed_report_callback=report_statewide_seeds,
            incumbent_report_callback=report_statewide_incumbent,
        )
        _clear_incumbent_previews()
        asu_id[:] = -1
        for label, unit in enumerate(groups, 1):
            asu_id[unit] = label
        _emit_progress("DONE")
        return {"asu_id": asu_id.tolist(), "n_asu": len(groups),
                "residual_check": "NOT_RUN_STATEWIDE_JOINT", "joint_status": status}

    if verbose:
        print(
            "[igraph] articulation-point acceleration: "
            + (
                "ENABLED (C-backed via python-igraph)"
                if _ig is not None
                else "DISABLED (pure-Python fallback; pip install python-igraph for a speedup)"
            ),
            flush=True,
        )

    batch_size = max(1, int(parallel_asus))
    k = len(initial_units)
    # `harvest_all_connectivity_free_components` seeds that fail territory
    # expansion get their tracts released back to `remaining` so the normal
    # ball-growing loop can retry them -- but since `remaining`/`tried` end up
    # bit-for-bit identical to before the attempt, the *same* component would
    # otherwise be re-harvested and re-fail forever with zero progress. Once a
    # seed's tracts have been through a failed harvest+expand cycle, never
    # re-admit that exact tract set as a standalone seed again this build.
    poisoned_seeds: Set[Tuple[int, ...]] = set()
    # Graph, economics, and solver settings are immutable within this build.
    optimal_territories: Dict[Tuple, Tuple[int, ...]] = {}
    polish_time_limit = (
        standalone_expansion_time_limit
        if final_asu_polish_time_limit is None
        else float(final_asu_polish_time_limit)
    )
    regional_exchange_state = _RegionalExchangeState(polish_time_limit)
    feasibility_cache: Dict = {}

    def _solve_window(**kwargs):
        return solve_one_asu_cpsat(feasibility_cache=feasibility_cache, **kwargs)

    def _screen_window(u_window, e_window, p_window, neighbors, threshold, population):
        return can_hit_tau(
            u_window, e_window, p_window, neighbors, threshold, population,
            cache=feasibility_cache, workers=workers, stop_path=stop_flag_path,
        )

    touching_attempts = set()

    def _resolve_touching_units(units, source, seconds, protected_nodes=()):
        if not merge_adjacent:
            return units, 0
        if not harvest_connectivity_free_asus:
            return _merge_touching_asu_units(units, nb, max_nodes=max_nodes_per_asu)
        available = set(np.flatnonzero(remaining)) - set(protected_nodes)

        def preview_factory(nodes, seeds):
            baseline = {v for unit in seeds for v in unit}
            return _incumbent_preview(
                ("touching_joint", source), nodes,
                [i for i, v in enumerate(nodes) if v in baseline])

        try:
            return _reoptimize_touching_asu_units(
                units, available, nb, u, E, P, tau, pop_thresh, seconds, workers,
                max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu,
                stop_path=stop_flag_path, skip_path=skip_flag_path,
                attempted=touching_attempts, log=verbose, rel_gap=rel_gap,
                incumbent_stall_seconds=incumbent_stall_seconds,
                source=source, preview_factory=preview_factory,
            )
        finally:
            _clear_incumbent_previews()

    def _reoptimize_committed_touching(source, seconds, protected_nodes=()):
        nonlocal k
        if not harvest_connectivity_free_asus or not merge_adjacent:
            return False
        units = [np.flatnonzero(asu_id == label).tolist()
                 for label in np.unique(asu_id[asu_id > 0])]
        updated, changes = _resolve_touching_units(units, source, seconds, protected_nodes)
        if not changes:
            return False
        previously_selected = asu_id > 0
        asu_id[:] = -1
        for label, unit in enumerate(updated, 1):
            asu_id[unit] = label
        remaining[:] = asu_id <= 0
        tried[previously_selected | (asu_id > 0)] = False
        k = len(updated)
        _emit_progress("PARTITION_TOUCHING_JOINT_COMMIT")
        return True

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

        # A connected component that already meets tau/pop_thresh on its own is a
        # ready-made ASU: rooting there means the solver never needs to reach
        # outside it, so graph-cut's output needs far less repair-stitching.
        components = _connected_components(nb, remaining)
        comp_id = np.full(n, -1, dtype=int)
        comp_qualifies = np.zeros(len(components), dtype=bool)
        for ci, component in enumerate(components):
            comp_id[component] = ci
            comp_qualifies[ci] = _screen_window(u[component], E[component], P[component], [], tau, pop_thresh)
        # Visit eligible seeds in the same capacity order used for every root.
        seed_pool = _capacity_root_order(cand_seeds, u, E, P, tau)

        # ---- Select up to batch_size disjoint feasible windows ----
        reserved = np.zeros(n, dtype=bool)
        windows: List[Dict] = []
        for s in seed_pool:
            if len(windows) >= batch_size:
                break
            s = int(s)
            if tried[s] or reserved[s] or not remaining[s]:
                continue
            if not comp_qualifies[comp_id[s]]:
                # s's entire graph-reachable component (within `remaining`) can't
                # hit tau/pop_thresh even taken as a whole -- no window rooted
                # here can ever succeed, so skip before paying for a (possibly
                # full-graph-sized) window build, harvest, or CP-SAT solve.
                tried[s] = True
                if verbose:
                    print(f"  [seed={s}] skip: own component cannot reach tau/pop_thresh", flush=True)
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
            if not _screen_window(u_g, E_g, P_g, nb_local, tau, pop_thresh):
                if verbose:
                    print(f"  [seed={s}] skip: quick screen fails", flush=True)
                tried[s] = True
                continue

            # If s's own component already qualifies as a standalone ASU, keep the
            # root inside it rather than letting a higher-capacity tract elsewhere
            # in the (much larger, under full_graph_window) window become the root.
            cand_root = cand
            if comp_qualifies[comp_id[s]]:
                same_comp_local = np.fromiter(
                    (local_index[g] for g in components[comp_id[s]] if g in local_index), dtype=int,
                )
                restricted = np.intersect1d(cand, same_comp_local)
                if restricted.size > 0:
                    cand_root = restricted

            root_local = _pick_capacity_root(cand_root, u_g, E_g, P_g, tau)

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
                use_connectivity_free_repair=use_connectivity_free_repair,
                connectivity_free_time_limit=connectivity_free_time_limit,
                harvest_connectivity_free_asus=harvest_connectivity_free_asus,
                harvest_all_connectivity_free_components=harvest_all_connectivity_free_components,
                use_graph_cut_repair=use_graph_cut_repair,
                graph_cut_repair_time_limit=graph_cut_repair_time_limit,
                workers=max(1, int(workers) // len(windows)),
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

        standalone_units: List[List[int]] = []
        for w in windows:
            for component_local in w.get("connectivity_free_standalone_asus", []):
                component_global = sorted(
                    np.array(w["sub"], dtype=int)[
                        np.array(component_local, dtype=int)
                    ].tolist()
                )
                # In the classic (independently-feasible-only) mode this check
                # never fails -- it's already guaranteed by the caller's filter.
                # In `harvest_all_connectivity_free_components` mode most raw
                # components legitimately fail it (that's the point: they still
                # get a shot at the territory-expansion step below), so it must
                # NOT gate seed admission there; the expanded result is instead
                # validated before commit, further down.
                if not harvest_all_connectivity_free_components and not component_ok(
                    component_global, u, E, P, tau, pop_thresh, nb
                ):
                    continue
                if (
                    max_nodes_per_asu is not None
                    and len(component_global) > max_nodes_per_asu
                ):
                    continue
                if tuple(component_global) in poisoned_seeds:
                    continue
                standalone_units.append(component_global)

        if standalone_units:
            # Rank whole components by their signed rate surplus. Negative
            # tract contributions must count too: this is the component's
            # available capacity, not just the sum of its profitable tracts.
            def _partition_seed_key(nodes):
                unemployed = sum(int(u[node]) for node in nodes)
                employed = sum(int(E[node]) for node in nodes)
                surplus = den * unemployed - num * employed
                return (-surplus, -unemployed, tuple(sorted(nodes)))

            standalone_units.sort(
                key=_partition_seed_key
            )
            slots = max_asus - k
            active_units = standalone_units[:slots]
            protected_units = standalone_units[slots:]
            if verbose:
                print(
                    f"\n[HARVEST] expanding {len(active_units)} candidate seed(s) "
                    f"({sum(component_ok(nodes, u, E, P, tau, pop_thresh, nb) for nodes in active_units)} "
                    "independently valid ASU(s)) in disjoint CP-SAT territories; "
                    "seed_priority=q_surplus",
                    flush=True,
                )

            expansion_round = 0
            commit_seeds = active_units
            final_statuses = ["SEED ONLY"] * len(active_units)
            while True:
                expansion_round += 1
                if joint_partition_expansion:
                    active_units = sorted(active_units, key=_partition_seed_key)
                round_seeds = active_units
                territory_sources = round_seeds + protected_units
                all_territories = _partition_standalone_expansion_territories(
                    territory_sources, nb, remaining, u=u
                )
                territories = all_territories[:len(round_seeds)]
                batches = [[i] for i in range(len(round_seeds))]
                if joint_partition_expansion:
                    grouped = _joint_expansion_batches(territories, nb)
                    order = [i for batch in grouped for i in batch]
                    round_seeds = [round_seeds[i] for i in order]
                    territories = [territories[i] for i in order]
                    batches, offset = [], 0
                    for batch in grouped:
                        batches.append(list(range(offset, offset + len(batch))))
                        offset += len(batch)
                # Expansion solves deliberately run one at a time.  Besides
                # making their logs and incumbents easier to attribute, this
                # gives every territory the complete CP-SAT worker portfolio.
                expansion_workers = max(1, int(workers))
                expansion_started = time.monotonic()
                attempted_seed_count = len(round_seeds)
                independently_valid_count = sum(
                    component_ok(nodes, u, E, P, tau, pop_thresh, nb)
                    for nodes in round_seeds
                )
                territory_sizes = [len(nodes) for nodes in territories]
                seed_tract_count = sum(len(nodes) for nodes in round_seeds)
                seed_unemployment = sum(
                    int(u[nodes].sum()) for nodes in round_seeds
                )
                if verbose:
                    print(
                        f"\n[STAGE] PARTITION_EXPANSION "
                        f"round={expansion_round} "
                        f"mode={'joint' if joint_partition_expansion else 'sequential'} "
                        f"solves={len(batches)} "
                        f"workers_per_solve={expansion_workers} "
                        f"time_limit_per_solve={standalone_expansion_time_limit:.1f}s "
                        f"seed_tracts={seed_tract_count} "
                        f"seed_unemp={seed_unemployment} "
                        f"independently_valid={independently_valid_count} "
                        f"territory_tracts_total={sum(territory_sizes)} "
                        f"territory_size_min={min(territory_sizes)} "
                        f"territory_size_median={float(np.median(territory_sizes)):.1f} "
                        f"territory_size_max={max(territory_sizes)} "
                        "incumbent_merge_check=disabled "
                        f"merge_check={'after_solve' if merge_adjacent else 'disabled'}",
                        flush=True,
                    )

                def _expand_standalone(unit_index: int) -> Tuple[List[int], str]:
                    seed_global = round_seeds[unit_index]
                    territory_global = territories[unit_index]
                    seed_feasible = component_ok(
                        seed_global, u, E, P, tau, pop_thresh, nb
                    )
                    if (
                        float(standalone_expansion_time_limit) <= 0
                        or len(territory_global) <= len(seed_global)
                        or _stop_requested(stop_flag_path)
                    ):
                        return seed_global, "SEED ONLY"

                    local_index = {
                        global_node: local_node
                        for local_node, global_node in enumerate(territory_global)
                    }
                    nb_expansion = [
                        sorted(
                            local_index[neighbor]
                            for neighbor in nb[global_node]
                            if neighbor in local_index
                        )
                        for global_node in territory_global
                    ]
                    u_expansion = u[territory_global]
                    E_expansion = E[territory_global]
                    P_expansion = P[territory_global]
                    seed_local = sorted(
                        local_index[node] for node in seed_global
                    )
                    root_expansion = _pick_capacity_root(
                        seed_local, u_expansion, E_expansion, P_expansion, tau
                    )
                    territory_key = (
                        tuple(sorted(territory_global)),
                        int(territory_global[root_expansion]), as_fraction_tau(tau),
                        int(pop_thresh), max_nodes_per_asu, exact_nodes_per_asu,
                    )
                    cached = optimal_territories.get(territory_key)
                    if cached is not None:
                        if not seed_feasible or int(u[list(cached)].sum()) >= int(u[seed_global].sum()):
                            if verbose:
                                print(f"  [EXPAND] reused proven-optimal territory: "
                                      f"{len(territory_global)} tracts", flush=True)
                            return list(cached), "OPTIMAL"
                        # A stronger feasible seed contradicts the certificate.
                        optimal_territories.pop(territory_key)

                    if "geoid" in df.columns:
                        stable_values = [
                            str(df.iloc[global_node]["geoid"])
                            for global_node in territory_global
                        ]
                    else:
                        stable_values = [
                            str(global_node).zfill(12)
                            for global_node in territory_global
                        ]
                    stable_order = sorted(
                        range(len(territory_global)),
                        key=lambda node: (stable_values[node], node),
                    )
                    expansion_tie_rank = [0] * len(territory_global)
                    for rank, local_node in enumerate(stable_order):
                        expansion_tie_rank[local_node] = rank

                    seed_objective = int(u_expansion[seed_local].sum())

                    if verbose:
                        print(
                            f"  [EXPAND round={expansion_round} "
                            f"{unit_index + 1}/{len(round_seeds)}] >>> "
                            f"seed={len(seed_local)} tract(s), "
                            f"territory={len(territory_global)}, "
                            f"workers={expansion_workers}, "
                            f"time_limit={standalone_expansion_time_limit:.1f}s",
                            flush=True,
                        )
                    result = _solve_window(
                        nb_local=nb_expansion,
                        u_g=u_expansion,
                        E_g=E_expansion,
                        P_g=P_expansion,
                        tau=tau,
                        pop_thresh=pop_thresh,
                        root_local=root_expansion,
                        time_limit=standalone_expansion_time_limit,
                        workers=expansion_workers,
                        rel_gap=rel_gap,
                        log=verbose,
                        hint=seed_local if seed_feasible else None,
                        hint_obj=seed_objective if seed_feasible else None,
                        incumbent_report_callback=_incumbent_preview(
                            ("expansion", unit_index), territory_global, seed_local
                        ),
                        incumbent_report_interval_seconds=60.0,
                        # Preview incumbents, but evaluate merges only after
                        # the solve returns its final selection.
                        incumbent_interrupt_callback=None,
                        deterministic_ties=deterministic_ties,
                        tie_break_rank=expansion_tie_rank,
                        objective_shaving=objective_shaving,
                        use_root_articulation_implications=use_root_articulation_implications,
                        use_signed_flow=use_signed_flow,
                        use_arborescence=use_arborescence,
                        configure_subsolvers=configure_subsolvers,
                        use_tract_first_search=use_tract_first_search,
                        use_flow_first_search=use_flow_first_search,
                        use_tract_capacity_search=use_tract_capacity_search,
                        use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                        incumbent_stall_seconds=incumbent_stall_seconds,
                        use_flow_count_envelope=use_flow_count_envelope,
                        use_small_root_separators=use_small_root_separators,
                        root_separator_max_size=root_separator_max_size,
                        root_separator_clause_limit=root_separator_clause_limit,
                        root_separator_target_limit=root_separator_target_limit,
                        use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                        solution_pool_size=solution_pool_size,
                        use_bridge_edge_bounds=use_bridge_edge_bounds,
                        use_articulation_edge_bounds=use_articulation_edge_bounds,
                        use_distance_flow_bounds=use_distance_flow_bounds,
                        use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                        use_bridge_subtree_pruning=use_bridge_subtree_pruning,
                        max_nodes=max_nodes_per_asu,
                        exact_nodes=exact_nodes_per_asu,
                        stop_flag_path=stop_flag_path,
                        skip_flag_path=skip_flag_path,
                    )
                    if result is None:
                        return seed_global, "SEED FALLBACK: NO SOLUTION"

                    expanded_global = sorted(
                        territory_global[local_node]
                        for local_node in result.sel_idx_local
                    )
                    expanded_valid = (
                        component_ok(
                            expanded_global, u, E, P, tau, pop_thresh, nb,
                            max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu,
                        )
                        and territory_global[root_expansion] in expanded_global
                    )
                    if not expanded_valid:
                        return seed_global, "SEED FALLBACK: INVALID RESULT"
                    # Only a feasible seed provides a meaningful objective
                    # floor. Repairing an infeasible seed may require dropping
                    # unemployment to satisfy the rate/population constraints.
                    if seed_feasible and int(u[expanded_global].sum()) < seed_objective:
                        return seed_global, "SEED FALLBACK: OBJECTIVE REGRESSION"
                    if result.status == "OPTIMAL" and (rel_gap is None or rel_gap == 0):
                        optimal_territories[territory_key] = tuple(expanded_global)
                    return expanded_global, result.status

                expanded_results: List[Tuple[List[int], str]] = []
                immediate_merged_units: Optional[List[List[int]]] = None
                immediate_merge_count = 0
                for batch_number, batch in enumerate(batches, 1):
                    if joint_partition_expansion:
                        batch_seeds = [round_seeds[i] for i in batch]
                        batch_nodes = sorted({v for i in batch for v in territories[i]})
                        batch_started = time.monotonic()
                        baseline = sum(int(u[seed].sum()) for seed in batch_seeds
                                       if component_ok(seed, u, E, P, tau, pop_thresh, nb,
                                                       max_nodes=max_nodes_per_asu,
                                                       exact_nodes=exact_nodes_per_asu))
                        if verbose:
                            print(f"[STAGE] PARTITION_JOINT_EXPANSION "
                                  f"round={expansion_round} batch={batch_number}/{len(batches)} "
                                  f"candidates={len(batch)} territory_tracts={len(batch_nodes)} "
                                  f"baseline_unemp={baseline} workers={expansion_workers} "
                                  f"time_limit={standalone_expansion_time_limit:.1f}s "
                                  "incumbent_merge_check=disabled "
                                  f"merge_check={'after_solve' if merge_adjacent else 'disabled'}",
                                  flush=True)
                        batch_index = {v: i for i, v in enumerate(batch_nodes)}
                        candidate_units, status = _solve_regional_exchange(
                            batch_seeds, batch_nodes, nb, u, E, P, tau, pop_thresh,
                            standalone_expansion_time_limit, expansion_workers,
                            max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu,
                            stop_path=stop_flag_path, skip_path=skip_flag_path,
                            allow_inactive_seeds=True, log=verbose, rel_gap=rel_gap,
                            incumbent_stall_seconds=incumbent_stall_seconds,
                            incumbent_report_callback=_incumbent_preview(
                                ("joint_expansion", batch_number), batch_nodes,
                                [batch_index[v] for seed in batch_seeds for v in seed],
                            ),
                        )
                        expanded_results.extend((unit, status) for unit in candidate_units)
                        if verbose:
                            unemployment = sum(int(u[unit].sum()) for unit in candidate_units)
                            active_count = sum(bool(unit) for unit in candidate_units)
                            print(f"[STAGE] PARTITION_JOINT_EXPANSION_COMPLETE "
                                  f"round={expansion_round} batch={batch_number}/{len(batches)} "
                                  f"status={status} active={active_count} "
                                  f"inactive={len(batch) - active_count} "
                                  f"unemp={unemployment} gain={unemployment - baseline} "
                                  f"elapsed={time.monotonic() - batch_started:.3f}s", flush=True)
                    else:
                        expanded_results.append(_expand_standalone(batch[0]))
                    if (
                        not merge_adjacent
                        or _stop_requested(stop_flag_path)
                    ):
                        continue

                    # Check after every expansion. Successfully expanded units
                    # and independently valid pending seeds may reoptimize now;
                    # pending weak seeds remain separate so they still receive
                    # their own repair attempt after the immediate repartition.
                    provisional_valid_units: List[List[int]] = []
                    pending_weak_units: List[List[int]] = []
                    for candidate_index, seed_nodes in enumerate(round_seeds):
                        if candidate_index < len(expanded_results):
                            candidate_nodes = expanded_results[candidate_index][0]
                            candidate_was_attempted = True
                        else:
                            candidate_nodes = seed_nodes
                            candidate_was_attempted = False
                        if component_ok(
                            candidate_nodes, u, E, P, tau, pop_thresh, nb
                        ):
                            provisional_valid_units.append(candidate_nodes)
                        elif not candidate_was_attempted:
                            pending_weak_units.append(seed_nodes)

                    candidate_merged_units, candidate_merge_count = (
                        _resolve_touching_units(
                            provisional_valid_units,
                            "expansion", standalone_expansion_time_limit,
                            protected_nodes=[v for unit in protected_units + pending_weak_units
                                             for v in unit],
                        )
                        if len(provisional_valid_units) > 1
                        else (provisional_valid_units, 0)
                    )
                    if candidate_merge_count == 0:
                        continue
                    if not all(
                        component_ok(nodes, u, E, P, tau, pop_thresh, nb)
                        for nodes in candidate_merged_units
                    ):
                        if verbose:
                            print(
                                "  [HARVEST MERGE] immediate sanity check "
                                "failed; continuing the current expansion round",
                                flush=True,
                            )
                        continue

                    immediate_merged_units = (
                        candidate_merged_units + pending_weak_units
                    )
                    immediate_merged_units.sort(
                        key=_partition_seed_key
                    )
                    immediate_merge_count = candidate_merge_count
                    break

                _clear_incumbent_previews()
                attempted_statuses = [status for _, status in expanded_results]
                if immediate_merged_units is not None:
                    # Retire only attempted seeds whose expansion failed.
                    # Unattempted weak seeds were carried into
                    # immediate_merged_units and will be repaired next round.
                    rejected_attempts = 0
                    for seed_nodes, (nodes, status) in zip(
                        round_seeds, expanded_results
                    ):
                        if component_ok(nodes, u, E, P, tau, pop_thresh, nb):
                            continue
                        rejected_attempts += 1
                        remaining[nodes] = True
                        tried[nodes] = False
                        poisoned_seeds.add(tuple(sorted(seed_nodes)))
                        if verbose:
                            print(
                                f"  [SKIP] candidate seed "
                                f"(seed={len(seed_nodes)} tract(s)) has no "
                                f"valid expansion (status={status}); excluded "
                                "before immediate merge, tracts released to "
                                "remaining pool",
                                flush=True,
                            )

                    if verbose:
                        status_counts: Dict[str, int] = {}
                        for status in attempted_statuses:
                            status_counts[status] = (
                                status_counts.get(status, 0) + 1
                            )
                        status_counts_text = ",".join(
                            f"{status}:{status_counts[status]}"
                            for status in sorted(status_counts)
                        ) or "none"
                        print(
                            f"[STAGE] PARTITION_EXPANSION_COMPLETE "
                            f"round={expansion_round} "
                            f"outcome=immediate_joint_rerun "
                            f"attempted={len(expanded_results)} "
                            f"scheduled={attempted_seed_count} "
                            f"skipped_stale="
                            f"{attempted_seed_count - len(expanded_results)} "
                            f"rejected={rejected_attempts} "
                            f"joint_updates={immediate_merge_count} "
                            f"output_groups={len(immediate_merged_units)} "
                            f"statuses={status_counts_text} "
                            f"elapsed="
                            f"{time.monotonic() - expansion_started:.3f}s",
                            flush=True,
                        )
                        print(
                            f"  [HARVEST TOUCHING JOINT] round {expansion_round}, "
                            f"immediately after expansion "
                            f"{len(expanded_results)}/{attempted_seed_count}: "
                            f"{attempted_seed_count} seed group(s) -> "
                            f"{len(immediate_merged_units)} touching/pending "
                            "group(s); repartitioning before the next solve",
                            flush=True,
                        )
                    active_units = immediate_merged_units
                    continue

                # Filter before merging: a failed seed is not an ASU and must
                # not invalidate unrelated valid merges (or join those groups).
                valid_seeds, valid_results = [], []
                for seed_nodes, (nodes, status) in zip(round_seeds, expanded_results):
                    if not component_ok(nodes, u, E, P, tau, pop_thresh, nb):
                        remaining[nodes] = True
                        tried[nodes] = False
                        poisoned_seeds.add(tuple(sorted(seed_nodes)))
                        if verbose:
                            print(
                                f"  [SKIP] candidate seed (seed={len(seed_nodes)} tract(s)) "
                                f"has no valid expansion (status={status}); "
                                "excluded before merging, tracts released to remaining pool",
                                flush=True,
                            )
                        continue
                    valid_seeds.append(seed_nodes)
                    valid_results.append((nodes, status))
                round_seeds = valid_seeds
                expanded_results = valid_results
                expanded_units = [nodes for nodes, _ in expanded_results]
                expansion_statuses = [status for _, status in expanded_results]
                should_merge = (
                    merge_adjacent
                    and len(expanded_units) > 1
                    and not _stop_requested(stop_flag_path)
                )
                merged_units, merge_count = _resolve_touching_units(
                    expanded_units, "expansion_round", standalone_expansion_time_limit,
                    protected_nodes=[v for unit in protected_units for v in unit],
                ) if should_merge else (expanded_units, 0)
                round_gain = (
                    sum(int(u[nodes].sum()) for nodes in expanded_units)
                    - sum(int(u[nodes].sum()) for nodes in round_seeds
                          if not joint_partition_expansion or component_ok(
                              nodes, u, E, P, tau, pop_thresh, nb,
                              max_nodes=max_nodes_per_asu, exact_nodes=exact_nodes_per_asu))
                )

                def _log_expansion_stage_complete(
                    outcome: str, output_groups: int, merges: int
                ) -> None:
                    if not verbose:
                        return
                    status_counts: Dict[str, int] = {}
                    for status in attempted_statuses:
                        status_counts[status] = status_counts.get(status, 0) + 1
                    status_counts_text = ",".join(
                        f"{status}:{status_counts[status]}"
                        for status in sorted(status_counts)
                    ) or "none"
                    print(
                        f"[STAGE] PARTITION_EXPANSION_COMPLETE "
                        f"round={expansion_round} outcome={outcome} "
                        f"attempted={attempted_seed_count} "
                        f"valid={len(expanded_units)} "
                        f"rejected={attempted_seed_count - len(expanded_units)} "
                        f"joint_updates={merges} output_groups={output_groups} "
                        f"valid_seed_unemp={sum(int(u[nodes].sum()) for nodes in round_seeds)} "
                        f"expanded_unemp={sum(int(u[nodes].sum()) for nodes in expanded_units)} "
                        f"gain={round_gain} statuses={status_counts_text} "
                        f"elapsed={time.monotonic() - expansion_started:.3f}s",
                        flush=True,
                    )

                if merge_count == 0:
                    active_units = expanded_units
                    commit_seeds = round_seeds
                    final_statuses = expansion_statuses
                    if round_gain > 0 and not _stop_requested(stop_flag_path):
                        _log_expansion_stage_complete(
                            "gain_rerun", len(active_units), 0
                        )
                        if verbose:
                            print(f"[PARTITION] round {expansion_round}: unemployment "
                                  f"gain={round_gain}; repartitioning and rerunning expansion",
                                  flush=True)
                        continue
                    _log_expansion_stage_complete(
                        "converged", len(active_units), 0
                    )
                    if verbose:
                        proven = bool(expansion_statuses) and all(
                            status == "OPTIMAL" for status in expansion_statuses)
                        print(f"[PARTITION] round {expansion_round}: no joint update or unemployment gain; "
                               + ("all current expansion models optimal."
                                 if proven else
                                 "stopping with available results; further improvement is not ruled out."),
                              flush=True)
                    break
                if not all(
                    component_ok(nodes, u, E, P, tau, pop_thresh, nb)
                    for nodes in merged_units
                ):
                    _log_expansion_stage_complete(
                        "merge_sanity_fallback", len(expanded_units), 0
                    )
                    if verbose:
                        print(
                            "  [HARVEST MERGE] sanity check failed; "
                            "keeping pre-merge expansions",
                            flush=True,
                        )
                    active_units = expanded_units
                    commit_seeds = round_seeds
                    final_statuses = expansion_statuses
                    break

                merged_units.sort(
                    key=_partition_seed_key
                )
                if verbose:
                    print(
                            f"  [HARVEST TOUCHING JOINT] round {expansion_round}: "
                        f"{len(expanded_units)} expanded ASU(s) -> "
                        f"{len(merged_units)} touching group(s); "
                        "repartitioning and rerunning expansion",
                        flush=True,
                    )
                _log_expansion_stage_complete(
                    "joint_rerun", len(merged_units), merge_count
                )
                active_units = merged_units

            committed_any = False
            for seed_nodes, nodes, status in zip(
                commit_seeds, active_units, final_statuses
            ):
                # Only ever fails in `harvest_all_connectivity_free_components`
                # mode: a weak seed (independently infeasible) whose territory
                # expansion still could not reach population/UR/connectivity.
                # Release its tracts instead of committing a broken ASU; the
                # normal ball-growing loop gets another shot at them below --
                # but only if this harvest round is falling through (see the
                # `committed_any` check after this loop): if every unit here
                # is skipped, `continue`-ing the outer loop would just re-derive
                # the identical stuck window (same top remaining seed, same
                # weak islands) forever without ever trying the real solve.
                if not component_ok(nodes, u, E, P, tau, pop_thresh, nb):
                    remaining[nodes] = True
                    tried[nodes] = False
                    poisoned_seeds.add(tuple(sorted(seed_nodes)))
                    if verbose:
                        print(
                            f"  [SKIP] weak seed (seed={len(seed_nodes)} tract(s)) "
                            f"could not expand into a valid ASU (status={status}); "
                            "tracts released back to remaining pool",
                            flush=True,
                        )
                    continue
                committed_any = True
                k += 1
                asu_id[nodes] = k
                remaining[nodes] = False
                tried[nodes] = False
                _emit_progress("HARVEST_EXPAND")
                if verbose:
                    su = int(u[nodes].sum())
                    sE = int(E[nodes].sum())
                    sP = int(P[nodes].sum())
                    ur_value = 100.0 * (
                        0.0 if su + sE == 0 else su / (su + sE)
                    )
                    seed_set = set(seed_nodes)
                    node_set = set(nodes)
                    print(
                        f"  [OK] ASU {k} expanded: seed={len(seed_nodes)}, "
                        f"tracts={len(nodes)} "
                        f"(+{len(node_set - seed_set)}/-{len(seed_set - node_set)}), "
                        f"pop={sP}, UR={ur_value:.3f}%, unemp={su}",
                        f"status={status}",
                        flush=True,
                    )
            # If nothing was committed this round, every harvested unit was a
            # dead end and the remaining pool is unchanged -- fall through to
            # the normal per-window CP-SAT solve below instead of restarting
            # the outer loop, which would just rebuild this identical window
            # (same top remaining seed under full_graph_window) and re-harvest
            # the same doomed islands again, looping forever.
            if committed_any:
                while _reoptimize_committed_touching(
                    "harvest_commit", standalone_expansion_time_limit,
                    protected_nodes=[v for unit in protected_units for v in unit],
                ):
                    pass
                continue

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
            if verbose:
                committed_mask = asu_id > 0
                print(
                    f"\n[STAGE] PARTITION_BUILD_UNCAPPED_RESCUE "
                    f"asu_target={k + 1} seed={w['seed']} "
                    f"reason={reason.replace(' ', '_')} "
                    f"window_tracts={len(w['nb_local'])} "
                    f"committed_asus={len(np.unique(asu_id[committed_mask]))} "
                    f"committed_unemp={int(u[np.where(committed_mask)[0]].sum())} "
                    f"remaining_tracts={int(remaining.sum())} workers={workers} "
                    f"time_limit={float(time_limit):.1f}s "
                    f"incumbent_merge_check=disabled",
                    flush=True,
                )
            result = _solve_window(
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
                use_flow_first_search=use_flow_first_search,
                use_tract_capacity_search=use_tract_capacity_search,
                use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                incumbent_stall_seconds=incumbent_stall_seconds,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                use_articulation_edge_bounds=use_articulation_edge_bounds,
                use_distance_flow_bounds=use_distance_flow_bounds,
                use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                use_bridge_subtree_pruning=use_bridge_subtree_pruning,
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
            _emit_progress("UNCAPPED_RESCUE")

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
                committed_mask = asu_id > 0
                print(
                    f"\n[STAGE] PARTITION_BUILD "
                    f"asu_target={k + 1} seed={w['seed']} "
                    f"window_tracts={len(w['nb_local'])} "
                    f"committed_asus={len(np.unique(asu_id[committed_mask]))} "
                    f"committed_unemp={int(u[np.where(committed_mask)[0]].sum())} "
                    f"remaining_tracts={int(remaining.sum())} workers={workers_each} "
                    f"time_limit={float(time_limit):.1f}s "
                    "incumbent_merge_check=disabled "
                    f"merge_check={'after_solve' if merge_adjacent else 'disabled'}",
                    flush=True,
                )
                print(f"  [seed={w['seed']}] >>> starting CP-SAT solve (nodes={len(w['nb_local'])}, "
                      f"workers={workers_each}, time_limit={time_limit}s)", flush=True)
            # The greedy warm-start hint is built on the full window (cap-unaware),
            # so an oversized hint's objective would become an infeasible hard
            # lower bound once max_nodes_per_asu is enforced in-model; drop it and
            # let CP-SAT search unassisted rather than hand it a false floor.
            hint_local = w["hint_improved"] if w["hint_valid"] else None
            hint_obj_local = w["hint_obj_val"] if w["hint_valid"] else None
            if (
                max_nodes_per_asu is not None
                and hint_local is not None
                and len(hint_local) > max_nodes_per_asu
            ):
                hint_local, hint_obj_local = None, None
            if (
                exact_nodes_per_asu is not None
                and hint_local is not None
                and len(hint_local) != exact_nodes_per_asu
            ):
                hint_local, hint_obj_local = None, None
            if w.get("connectivity_free_infeasible", False):
                if verbose:
                    print(
                        f"  [seed={w['seed']}] connectivity-free relaxation "
                        "proved this rooted window infeasible; skipping exact solve",
                        flush=True,
                    )
                return None
            if (
                w.get("connectivity_free_proves_optimal", False)
                and hint_local is not None
                and hint_obj_local is not None
                and (not deterministic_ties or rel_gap is not None)
            ):
                if verbose:
                    print(
                        f"  [seed={w['seed']}] connectivity-free upper bound "
                        f"matches connected hint {hint_obj_local}; exact solve skipped",
                        flush=True,
                    )
                return CpsatResult(
                    list(hint_local),
                    int(w["root_local"]),
                    int(hint_obj_local),
                    "RELAXATION_PROVEN_OPTIMAL",
                )

            incumbent_callback = _incumbent_preview(
                ("window", w["seed"]), w["sub"], hint_local or []
            )

            result = _solve_window(
                nb_local=w["nb_local"], u_g=w["u_g"], E_g=w["E_g"], P_g=w["P_g"],
                tau=tau, pop_thresh=pop_thresh, root_local=w["root_local"],
                time_limit=time_limit, workers=workers_each, rel_gap=rel_gap, log=verbose,
                hint=hint_local, hint_obj=hint_obj_local,
                objective_upper_bound=w.get("connectivity_free_upper_bound"),
                initial_connectivity_cuts=w.get("connectivity_free_cuts"),
                initial_components=w.get("best_fixed_components"),
                forced_selected=w["root_component"],
                deterministic_ties=deterministic_ties,
                tie_break_rank=w["tie_break_rank"],
                objective_shaving=objective_shaving,
                use_root_articulation_implications=use_root_articulation_implications,
                use_signed_flow=use_signed_flow,
                use_arborescence=use_arborescence,
                configure_subsolvers=configure_subsolvers,
                use_tract_first_search=use_tract_first_search,
                use_flow_first_search=use_flow_first_search,
                use_tract_capacity_search=use_tract_capacity_search,
                use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                incumbent_stall_seconds=incumbent_stall_seconds,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                use_articulation_edge_bounds=use_articulation_edge_bounds,
                use_distance_flow_bounds=use_distance_flow_bounds,
                use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                use_bridge_subtree_pruning=use_bridge_subtree_pruning,
                max_nodes=max_nodes_per_asu,
                exact_nodes=exact_nodes_per_asu,
                stop_flag_path=stop_flag_path,
                skip_flag_path=skip_flag_path,
                incumbent_report_callback=incumbent_callback,
                incumbent_report_interval_seconds=60.0,
                incumbent_interrupt_callback=None,
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

        # Remove any provisional incumbent overlay before refining or committing
        # the returned solution. The subsequent commit snapshot paints the final
        # ASU through the normal assignment path.
        _clear_incumbent_previews()

        # ---- Refine each window's result within its own reserved territory ----
        candidates: List[List[int]] = []
        stop_no_progress = False
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
                elif full_graph_window:
                    # Under full_graph_window every remaining seed shares this
                    # exact same window (nb_local/u_g/E_g/P_g unchanged), so
                    # marking just this seed tried and looping back to try the
                    # next-highest-capacity seed would re-pay the full cut-pass +
                    # exact-solve cost against an identical graph, only with a
                    # different forced root -- with no cheap way to predict
                    # which root (if any) would succeed. Stop instead of
                    # exhaustively retrying every remaining high-UR tract.
                    tried[w["seed"]] = True
                    if verbose:
                        print(f"  [seed={w['seed']}] uncapped solve found no solution; "
                              f"full_graph_window is on so further seeds would retry "
                              f"the identical window -- stopping ASU creation.", flush=True)
                    stop_no_progress = True
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

        if stop_no_progress:
            break

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
        if (merge_adjacent and not harvest_connectivity_free_asus
                and max_nodes_per_asu is None and len(candidates) > 1):
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

            # A main-build incumbent may have stopped as soon as it touched an
            # already committed ASU. Merge that candidate immediately instead
            # of temporarily committing two touching ASUs and deferring the
            # union until the late combine/polish phases. The union of connected
            # feasible ASUs joined by an edge remains connected, meets the
            # population threshold, and preserves the unemployment-rate bound.
            touching_ids = sorted({
                int(asu_id[neighbor])
                for node in S_final
                for neighbor in nb[node]
                if int(asu_id[neighbor]) > 0
            }) if (merge_adjacent and not harvest_connectivity_free_asus
                   and max_nodes_per_asu is None) else []
            if touching_ids:
                prior_asu_count = len(np.unique(asu_id[asu_id > 0]))
                merged_nodes = sorted(set(S_final) | {
                    int(node)
                    for node in np.where(np.isin(asu_id, touching_ids))[0]
                })
                if component_ok(merged_nodes, u, E, P, tau, pop_thresh, nb):
                    asu_id[merged_nodes] = min(touching_ids)
                    remaining[merged_nodes] = False
                    tried[merged_nodes] = False
                    asu_id[:] = _sequential_asu_ids(asu_id)
                    k = len(np.unique(asu_id[asu_id > 0]))
                    _emit_progress("MAIN_IMMEDIATE_MERGE")
                    if verbose:
                        print(
                            f"[STAGE] PARTITION_BUILD_MERGE "
                            f"candidate_tracts={len(S_final)} "
                            f"touched_asus={','.join(map(str, touching_ids))} "
                            f"merged_tracts={len(merged_nodes)} "
                            f"asus_before={prior_asu_count} asus_after={k} "
                            f"action=restart",
                            flush=True,
                        )
                        print(
                            f"  [MERGE] main-build candidate touched ASU(s) "
                            f"{touching_ids}; merged immediately and restarting "
                            "the partition loop.",
                            flush=True,
                        )
                    continue

            k += 1
            asu_id[S_final] = k
            remaining[S_final] = False
            tried[S_final] = False
            _emit_progress("MAIN_COMMIT")

            if verbose:
                su, sE, sP = int(u[S_final].sum()), int(E[S_final].sum()), int(P[S_final].sum())
                URv = 100.0 * (0.0 if (su + sE) == 0 else su / (su + sE))
                print(f"  [OK] ASU {k} committed: tracts={len(S_final)}, pop={sP}, UR={URv:.3f}%, unemp={su}", flush=True)

            while _reoptimize_committed_touching(
                "main_commit", standalone_expansion_time_limit,
                protected_nodes=np.flatnonzero(pending_mask),
            ):
                pass
            if k >= max_asus:
                break

    # ---- Combine touching ASUs across batches, then expand again via CP-SAT ----
    combine_capped = (
        max_nodes_per_asu is not None
        and combine_capped_asus
        and exact_nodes_per_asu is None
        and not harvest_connectivity_free_asus
    )
    while _reoptimize_committed_touching("cross_batch", standalone_expansion_time_limit):
        pass
    if combine_capped and k > 1:
        combine_solve_time_limit = (
            time_limit if combine_time_limit is None else combine_time_limit
        )
        # Groups that failed the sanity check are skipped on later rounds too,
        # unless a subsequent merge elsewhere changes their member id set.
        failed_signatures: set = set()
        combine_round = 0

        while True:
            if _stop_requested(stop_flag_path):
                if verbose:
                    print("[COMBINE] Stop flag detected; halting combine phase.", flush=True)
                break

            committed_ids = np.unique(asu_id[asu_id > 0]).tolist()
            if len(committed_ids) < 2:
                break

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
            # Re-derived every round: a merged ASU's expanded window can claim
            # previously-unclaimed tracts that newly touch yet other committed
            # ASUs, so groups found on earlier rounds don't capture every merge.
            groups_to_improve = [
                members for members in merge_groups.values()
                if len(members) > 1 and frozenset(members) not in failed_signatures
            ]

            if not groups_to_improve:
                break

            combine_round += 1
            if verbose:
                combine_kind = "capped"
                print(
                    f"\n[STAGE] PARTITION_COMBINE round={combine_round} "
                    f"groups={len(groups_to_improve)} "
                    f"touching_asus={sum(len(m) for m in groups_to_improve)} "
                    f"kind={combine_kind} workers={workers} "
                    f"time_limit_per_solve={float(combine_solve_time_limit):.1f}s\n"
                    f"[COMBINE] round {combine_round}: {len(groups_to_improve)} touching-cluster "
                    f"group(s) found across {sum(len(m) for m in groups_to_improve)} "
                    f"{combine_kind} ASUs; "
                    f"improving via CP-SAT ...",
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
                    failed_signatures.add(frozenset(members))
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
                root_local = _pick_capacity_root(group_local, u_g, E_g, P_g, tau)

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

                result = _solve_window(
                    nb_local=nb_local, u_g=u_g, E_g=E_g, P_g=P_g,
                    tau=tau, pop_thresh=pop_thresh, root_local=root_local,
                    time_limit=combine_solve_time_limit, workers=workers, rel_gap=rel_gap, log=verbose,
                    hint=group_local, hint_obj=hint_obj_val,
                    deterministic_ties=deterministic_ties,
                    tie_break_rank=tie_break_rank,
                    objective_shaving=objective_shaving,
                    use_root_articulation_implications=use_root_articulation_implications,
                    use_signed_flow=use_signed_flow,
                    use_arborescence=use_arborescence,
                    configure_subsolvers=configure_subsolvers,
                    use_tract_first_search=use_tract_first_search,
                    use_flow_first_search=use_flow_first_search,
                    use_tract_capacity_search=use_tract_capacity_search,
                    use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                    incumbent_stall_seconds=incumbent_stall_seconds,
                    use_flow_count_envelope=use_flow_count_envelope,
                    use_small_root_separators=use_small_root_separators,
                    root_separator_max_size=root_separator_max_size,
                    root_separator_clause_limit=root_separator_clause_limit,
                    root_separator_target_limit=root_separator_target_limit,
                    use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                    solution_pool_size=solution_pool_size,
                    use_bridge_edge_bounds=use_bridge_edge_bounds,
                    use_articulation_edge_bounds=use_articulation_edge_bounds,
                    use_distance_flow_bounds=use_distance_flow_bounds,
                    use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                    use_bridge_subtree_pruning=use_bridge_subtree_pruning,
                    # max_nodes intentionally omitted: this phase's purpose is to
                    # lift the per-ASU cap now that touching clusters are combined.
                    stop_flag_path=stop_flag_path,
                    skip_flag_path=skip_flag_path,
                )
                S_local = result.sel_idx_local if result is not None else group_local
                S_global = np.array(sub, dtype=int)[np.array(S_local, dtype=int)].tolist()
                if (
                    int(u[S_global].sum()) < hint_obj_val
                    or not component_ok(S_global, u, E, P, tau, pop_thresh, nb)
                ):
                    S_global = group_tracts.tolist()

                new_id = min(members)
                old_mask = np.isin(asu_id, members)
                asu_id[old_mask] = -1
                remaining[old_mask] = True
                asu_id[S_global] = new_id
                remaining[S_global] = False
                _emit_progress("COMBINE")

                if verbose:
                    su2, sE2, sP2 = int(u[S_global].sum()), int(E[S_global].sum()), int(P[S_global].sum())
                    URv2 = 100.0 * (0.0 if (su2 + sE2) == 0 else su2 / (su2 + sE2))
                    old_tracts = set(group_tracts.tolist())
                    new_tracts = set(S_global)
                    status2 = result.status if result is not None else "GREEDY FALLBACK"
                    print(
                        f"  [OK] Combined ASU {new_id}: tracts={len(S_global)} "
                        f"(+{len(new_tracts - old_tracts)}/-{len(old_tracts - new_tracts)}), "
                        f"pop={sP2}, UR={URv2:.3f}%, unemp={su2} (status={status2})",
                        flush=True,
                    )

    # ---- Capacity sweep: build standalone ASUs from purely-leftover tracts ----
    # The main loop above already seeds new ASUs from remaining tracts whose own
    # unemployment rate clears tau, and stops once none do. Any leftover tract
    # that could usefully join an EXISTING ASU would already have been captured
    # by that ASU's own window/combine solves above, so this pass never looks
    # at already-committed tracts at all -- windows are grown strictly within
    # the remaining-tract subgraph. It only tries to salvage brand-new
    # standalone ASUs from what's left, picking the remaining tract with the
    # highest UR-surplus (q_i = den*u_i - num*E_i, the same exact-integer
    # quantity used for the UR constraint) as root each round, and stops once
    # no remaining tract has positive surplus left.
    if use_capacity_sweep and exact_nodes_per_asu is None and k < max_asus and not _stop_requested(stop_flag_path):
        swept_tried = np.zeros(n, dtype=bool)
        sweep_round = 0
        while k < max_asus:
            if _stop_requested(stop_flag_path):
                if verbose:
                    print("[SWEEP] Stop flag detected; halting capacity sweep.", flush=True)
                break

            cand_idx = np.where(remaining & ~swept_tried)[0]
            if cand_idx.size == 0:
                break
            root_global = _pick_capacity_root(cand_idx, u, E, P, tau)
            if den * int(u[root_global]) - num * int(E[root_global]) <= 0:
                break

            allowed_idx = np.where(remaining)[0]
            r = int(r_start)
            sub = bfs_ball(nb, root_global, r, allowed_idx)
            while P[sub].sum() < min_pop_margin * pop_thresh and r < r_max and len(sub) < hard_cap_nodes:
                r += r_step
                sub = bfs_ball(nb, root_global, r, allowed_idx)
            if len(sub) > hard_cap_nodes:
                while len(sub) > hard_cap_nodes and r > 1:
                    r -= 1
                    sub = bfs_ball(nb, root_global, r, allowed_idx)
                if len(sub) > hard_cap_nodes:
                    sub = sub[:hard_cap_nodes]

            local_index = {g: i for i, g in enumerate(sub)}
            nb_local = [sorted(local_index[h] for h in nb[g] if h in local_index) for g in sub]
            u_g, E_g, P_g = u[sub], E[sub], P[sub]
            root_local = local_index[root_global]

            if not _screen_window(u_g, E_g, P_g, nb_local, tau, pop_thresh):
                swept_tried[root_global] = True
                if verbose:
                    print(f"  [SWEEP] seed={root_global}: quick screen fails; skipping", flush=True)
                continue

            sweep_round += 1
            if "geoid" in df.columns:
                stable_values = [str(df.iloc[int(g)]["geoid"]) for g in sub]
            else:
                stable_values = [str(int(g)).zfill(12) for g in sub]
            stable_order = sorted(range(len(sub)), key=lambda i: (stable_values[i], i))
            tie_break_rank = [0] * len(sub)
            for rank, local_i in enumerate(stable_order):
                tie_break_rank[local_i] = rank

            if verbose:
                print(
                    f"\n[STAGE] CAPACITY_SWEEP round={sweep_round} "
                    f"seed={root_global} window_tracts={len(sub)} "
                    f"remaining_tracts={int(remaining.sum())} "
                    f"committed_asus={len(np.unique(asu_id[asu_id > 0]))} "
                    f"workers={workers} "
                    f"time_limit={float(capacity_sweep_time_limit):.1f}s\n"
                    f"[SWEEP round={sweep_round}] seed={root_global} "
                    f"(q_surplus={int(q_cand[best_pos])}): window r={r}, "
                    f"nodes={len(sub)}, pop={int(P_g.sum())}",
                    flush=True,
                )

            info = _prepare_window_hint(
                nb_local, u_g, E_g, P_g, tau, pop_thresh, root_local,
                verbose=verbose, max_nodes=max_nodes_per_asu,
                use_connectivity_free_repair=use_connectivity_free_repair,
                connectivity_free_time_limit=connectivity_free_time_limit,
                harvest_connectivity_free_asus=False,
                use_graph_cut_repair=use_graph_cut_repair,
                graph_cut_repair_time_limit=graph_cut_repair_time_limit,
                workers=workers,
            )
            hint_local = info["hint_improved"] if info["hint_valid"] else None
            hint_obj_local = info["hint_obj_val"] if info["hint_valid"] else None
            if (
                max_nodes_per_asu is not None
                and hint_local is not None
                and len(hint_local) > max_nodes_per_asu
            ):
                hint_local, hint_obj_local = None, None

            result = _solve_window(
                nb_local=nb_local, u_g=u_g, E_g=E_g, P_g=P_g,
                tau=tau, pop_thresh=pop_thresh, root_local=root_local,
                time_limit=capacity_sweep_time_limit, workers=workers, rel_gap=rel_gap, log=verbose,
                hint=hint_local, hint_obj=hint_obj_local,
                forced_selected=info["root_component"],
                deterministic_ties=deterministic_ties,
                tie_break_rank=tie_break_rank,
                objective_shaving=objective_shaving,
                use_root_articulation_implications=use_root_articulation_implications,
                use_signed_flow=use_signed_flow,
                use_arborescence=use_arborescence,
                configure_subsolvers=configure_subsolvers,
                use_tract_first_search=use_tract_first_search,
                use_flow_first_search=use_flow_first_search,
                use_tract_capacity_search=use_tract_capacity_search,
                use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                incumbent_stall_seconds=incumbent_stall_seconds,
                use_flow_count_envelope=use_flow_count_envelope,
                use_small_root_separators=use_small_root_separators,
                root_separator_max_size=root_separator_max_size,
                root_separator_clause_limit=root_separator_clause_limit,
                root_separator_target_limit=root_separator_target_limit,
                use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                solution_pool_size=solution_pool_size,
                use_bridge_edge_bounds=use_bridge_edge_bounds,
                use_articulation_edge_bounds=use_articulation_edge_bounds,
                use_distance_flow_bounds=use_distance_flow_bounds,
                use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                use_bridge_subtree_pruning=use_bridge_subtree_pruning,
                max_nodes=max_nodes_per_asu,
                stop_flag_path=stop_flag_path,
                skip_flag_path=skip_flag_path,
            )

            if result is not None:
                S_local = result.sel_idx_local
            elif hint_local is not None and (
                max_nodes_per_asu is None or len(hint_local) <= max_nodes_per_asu
            ):
                S_local = hint_local
            else:
                S_local = None

            S_global = (
                sorted(np.array(sub, dtype=int)[np.array(S_local, dtype=int)].tolist())
                if S_local is not None else None
            )
            if S_global is None or not component_ok(S_global, u, E, P, tau, pop_thresh, nb) or (
                max_nodes_per_asu is not None and len(S_global) > max_nodes_per_asu
            ):
                swept_tried[root_global] = True
                if verbose:
                    print(f"  [SWEEP] seed={root_global}: no feasible standalone ASU found; skipping", flush=True)
                continue

            allowed_idx = np.where(remaining)[0]
            S_final = improve_by_trades(S_global, u, E, P, nb, tau, pop_thresh, allowed_idx,
                                         max_iter=200, max_size=max_nodes_per_asu)
            if not component_ok(S_final, u, E, P, tau, pop_thresh, nb):
                S_final = S_global

            k += 1
            asu_id[S_final] = k
            remaining[S_final] = False
            _emit_progress("CAPACITY_SWEEP")
            if verbose:
                su, sE, sP = int(u[S_final].sum()), int(E[S_final].sum()), int(P[S_final].sum())
                ur_value = 100.0 * ur_of(su, sE)
                status_txt = result.status if result is not None else "GREEDY FALLBACK"
                print(
                    f"  [OK] ASU {k} (sweep): tracts={len(S_final)}, pop={sP}, "
                    f"UR={ur_value:.3f}%, unemp={su} (status={status_txt})",
                    flush=True,
                )
            while _reoptimize_committed_touching("capacity_sweep", standalone_expansion_time_limit):
                pass

        if verbose and sweep_round:
            print(f"\n[SWEEP] finished: {sweep_round} window(s) tried, {k} total ASU(s) so far", flush=True)

    polish_enabled = (
        harvest_connectivity_free_asus
        and polish_time_limit > 0
        and not _stop_requested(stop_flag_path)
        and exact_nodes_per_asu is None
    )
    polish_max_nodes = None if combine_capped else max_nodes_per_asu
    # Per-run only: graph, economic data and solve settings are immutable here.
    # Merges elsewhere need not repeat an identical reachable polish problem.
    polish_attempts: set = set()
    polish_last_windows: Dict[int, Set[int]] = {}
    polish_followup_seconds = min(
        180.0, polish_time_limit * max(1, len(np.unique(asu_id[asu_id > 0]))),
    )
    polish_followup_rounds = 0
    polish_skipped_ids: Set[int] = set()

    def _polish_one_asu(
        asu_number: int,
        polish_position: int,
        polish_count: int,
        polish_round: int,
        seconds: Optional[float] = None,
    ) -> bool:
        if _stop_requested(stop_flag_path):
            if verbose:
                print(
                    "[FINAL POLISH] Stop flag detected; halting final pass.",
                    flush=True,
                )
            return False

        current_global = np.where(asu_id == asu_number)[0].astype(int).tolist()
        total_polish_unemp = int(u[np.where(asu_id > 0)[0]].sum())
        if verbose:
            print(
                f"\n[STAGE] FINAL_POLISH round={polish_round} "
                f"checked={polish_position - 1}/{polish_count} "
                f"checking_asu={asu_number} "
                f"position={polish_position}/{polish_count} "
                f"total_unemp={total_polish_unemp} "
                f"tracts={len(current_global)} "
                f"time_limit={polish_time_limit if seconds is None else min(polish_time_limit, seconds):.3f}s "
                "incumbent_merge_check=disabled "
                f"merge_check={'after_solve' if merge_adjacent else 'disabled'}",
                flush=True,
            )
        if not component_ok(
            current_global, u, E, P, tau, pop_thresh, nb
        ):
            if verbose:
                print(
                    f"  [FINAL POLISH round={polish_round} "
                    f"{polish_position}/{polish_count}] ASU {asu_number} "
                    "failed its input sanity check; skipped",
                    flush=True,
                )
            return True

        root_global = _pick_capacity_root(current_global, u, E, P, tau)

        current_set = set(current_global)
        allowed_set = current_set | set(np.where(remaining)[0].astype(int).tolist())
        if len(allowed_set) == len(current_set):
            polish_last_windows[asu_number] = current_set
            if verbose:
                print(
                    f"  [FINAL POLISH round={polish_round} "
                    f"{polish_position}/{polish_count}] ASU {asu_number}: "
                    "no unassigned tracts remain",
                    flush=True,
                )
            return True

        # Restrict polish to tracts reachable from this ASU root.
        sub = _reachable_polish_window(root_global, asu_number, asu_id, nb)
        polish_last_windows[asu_number] = set(sub)
        filtered_unreachable = len(allowed_set) - len(sub)
        attempt_key = _polish_attempt_key(
            root_global, current_global, sub, tau, pop_thresh, polish_max_nodes
        )
        if attempt_key in polish_attempts:
            if verbose:
                print(f"[FINAL POLISH CACHE] ASU {asu_number}: unchanged root, "
                      "incumbent and reachable window; skipping repeated attempt",
                      flush=True)
            return True

        local_index = {
            global_node: local_node
            for local_node, global_node in enumerate(sub)
        }
        nb_local = [
            sorted(
                local_index[neighbor]
                for neighbor in nb[global_node]
                if neighbor in local_index
            )
            for global_node in sub
        ]
        u_g, E_g, P_g = u[sub], E[sub], P[sub]
        current_local = sorted(
            local_index[node] for node in current_global
        )
        root_local = int(local_index[root_global])
        if "geoid" in df.columns:
            stable_values = [
                str(df.iloc[global_node]["geoid"])
                for global_node in sub
            ]
        else:
            stable_values = [
                str(global_node).zfill(12) for global_node in sub
            ]
        stable_order = sorted(
            range(len(sub)), key=lambda node: (stable_values[node], node)
        )
        polish_tie_rank = [0] * len(sub)
        for rank, local_node in enumerate(stable_order):
            polish_tie_rank[local_node] = rank

        current_objective = int(u[current_global].sum())

        if verbose:
            print(
                f"  [FINAL POLISH round={polish_round} "
                f"{polish_position}/{polish_count}] >>> ASU {asu_number}: "
                f"seed={len(current_global)}, window={len(sub)}, "
                f"unassigned={len(sub) - len(current_global)}, "
                f"filtered_unreachable={filtered_unreachable}, "
                f"unemp_floor={current_objective}",
                flush=True,
            )
        result = _solve_window(
            nb_local=nb_local,
            u_g=u_g,
            E_g=E_g,
            P_g=P_g,
            tau=tau,
            pop_thresh=pop_thresh,
            root_local=root_local,
            time_limit=polish_time_limit if seconds is None else min(polish_time_limit, seconds),
            workers=workers,
            rel_gap=rel_gap,
            log=verbose,
            hint=current_local,
            hint_obj=current_objective,
            incumbent_report_callback=_incumbent_preview(
                ("final_polish", polish_round, asu_number), sub, current_local
            ),
            incumbent_report_interval_seconds=60.0,
            incumbent_interrupt_callback=None,
            deterministic_ties=deterministic_ties,
            tie_break_rank=polish_tie_rank,
            objective_shaving=objective_shaving,
            use_root_articulation_implications=use_root_articulation_implications,
            use_signed_flow=use_signed_flow,
            use_arborescence=use_arborescence,
            configure_subsolvers=configure_subsolvers,
            use_tract_first_search=use_tract_first_search,
            use_flow_first_search=use_flow_first_search,
            use_tract_capacity_search=use_tract_capacity_search,
            use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
            incumbent_stall_seconds=incumbent_stall_seconds,
            use_flow_count_envelope=use_flow_count_envelope,
            use_small_root_separators=use_small_root_separators,
            root_separator_max_size=root_separator_max_size,
            root_separator_clause_limit=root_separator_clause_limit,
            root_separator_target_limit=root_separator_target_limit,
            use_separator_cardinality_bounds=use_separator_cardinality_bounds,
            solution_pool_size=solution_pool_size,
            use_bridge_edge_bounds=use_bridge_edge_bounds,
            use_articulation_edge_bounds=use_articulation_edge_bounds,
            use_distance_flow_bounds=use_distance_flow_bounds,
            use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
            use_bridge_subtree_pruning=use_bridge_subtree_pruning,
            max_nodes=polish_max_nodes,
            stop_flag_path=stop_flag_path,
            skip_flag_path=skip_flag_path,
        )
        # Remove the provisional delta before retaining the seed, validating a
        # replacement, or publishing its committed assignment.
        _clear_incumbent_previews()
        if result is None:
            if verbose:
                print(
                    f"  [FINAL POLISH round={polish_round} "
                    f"{polish_position}/{polish_count}] ASU {asu_number}: "
                    "no replacement; seed retained",
                    flush=True,
                )
            return True

        polished_global = sorted(
            sub[local_node] for local_node in result.sel_idx_local
        )
        polished_objective = int(u[polished_global].sum())
        polished_valid = (
            polished_objective >= current_objective
            and component_ok(
                polished_global, u, E, P, tau, pop_thresh, nb
            )
            and (
                polish_max_nodes is None
                or len(polished_global) <= polish_max_nodes
            )
        )
        if not polished_valid:
            if verbose:
                print(
                    f"  [FINAL POLISH round={polish_round} "
                    f"{polish_position}/{polish_count}] ASU {asu_number}: "
                    "invalid replacement; seed retained",
                    flush=True,
                )
            return True

        if (result.status not in ("STOPPED_FEASIBLE", "SKIPPED_FEASIBLE")
                and not _stop_requested(stop_flag_path)):
            polish_attempts.add(attempt_key)
        if result.status == "SKIPPED_FEASIBLE":
            polish_skipped_ids.add(asu_number)
        current_set = set(current_global)
        polished_set = set(polished_global)
        dropped = current_set - polished_set
        added = polished_set - current_set
        asu_id[current_global] = -1
        remaining[current_global] = True
        asu_id[polished_global] = asu_number
        remaining[polished_global] = False
        _emit_progress("FINAL_POLISH")
        if verbose:
            print(
                f"  [OK] ASU {asu_number} polished: "
                f"tracts={len(polished_global)} (+{len(added)}/-{len(dropped)}), "
                f"unemp={polished_objective} "
                f"(+{polished_objective - current_objective}), "
                f"status={result.status}",
                flush=True,
            )
        return True

    def _merge_committed_asus(stage: str, detail: str = "") -> bool:
        """Jointly reoptimize partition groups; retain legacy touching unions."""
        if harvest_connectivity_free_asus:
            return _reoptimize_committed_touching(
                f"{stage} {detail}".strip(),
                polish_time_limit if polish_time_limit > 0 else standalone_expansion_time_limit,
            )
        if (not merge_adjacent or exact_nodes_per_asu is not None
                or _stop_requested(stop_flag_path)):
            return False
        committed_ids = np.unique(asu_id[asu_id > 0]).astype(int).tolist()
        committed_units = [
            np.flatnonzero(asu_id == committed_id).tolist()
            for committed_id in committed_ids
        ]
        merged_units, merge_count = _merge_touching_asu_units(
            committed_units, nb, max_nodes=polish_max_nodes,
        )
        if not merge_count:
            return False
        if not all(component_ok(
            nodes, u, E, P, tau, pop_thresh, nb, max_nodes=polish_max_nodes,
        ) for nodes in merged_units):
            if verbose:
                print(f"[{stage}] sanity check failed; retaining separate ASUs",
                      flush=True)
            return False
        merged_assignments = [
            (int(asu_id[nodes].min()), nodes) for nodes in merged_units
        ]
        for merged_id, nodes in merged_assignments:
            asu_id[nodes] = merged_id
            remaining[nodes] = False
        _emit_progress(stage)
        if verbose:
            print(
                f"[STAGE] {stage} {detail} merges={merge_count} "
                f"asus_before={len(committed_ids)} asus_after={len(merged_units)} "
                f"total_unemp={int(u[asu_id > 0].sum())} "
                f"action={'restart' if polish_enabled else 'merged'}",
                flush=True,
            )
        return True

    polish_round = 0

    def _run_final_polish() -> None:
        nonlocal polish_round, polish_followup_seconds, polish_followup_rounds
        if not polish_enabled:
            return
        pending_ids = None
        seen_states = set()
        while True:
            if _stop_requested(stop_flag_path):
                return
            state = asu_id.tobytes()
            if state in seen_states:
                if verbose:
                    print("[STAGE] FINAL_POLISH_RECHECK_LIMIT reason=assignment_cycle", flush=True)
                return
            seen_states.add(state)
            polish_round += 1
            polish_ids = _polish_asu_order(asu_id, u, E, tau)
            if pending_ids is not None:
                polish_ids = [k for k in polish_ids if k in pending_ids]
            total_polish_unemp = int(u[np.where(asu_id > 0)[0]].sum())
            if verbose and polish_ids:
                print(
                    f"\n[STAGE] FINAL_POLISH round={polish_round} "
                    f"asus={len(polish_ids)} total_unemp={total_polish_unemp} "
                    f"mode={'followup' if pending_ids is not None else 'normal'} "
                    "priority=q_surplus",
                    flush=True,
                )
                print(
                    f"\n[FINAL POLISH] round {polish_round}: "
                    f"{len(polish_ids)} ASU(s), highest q_surplus first, "
                    "each seeing all currently "
                    f"unassigned tracts (up to {polish_time_limit:.1f}s each); "
                    f"total unemployment currently captured={total_polish_unemp}",
                    flush=True,
                )

            polish_completed = True
            restart_after_merge = False
            for polish_position, asu_number in enumerate(polish_ids, start=1):
                if pending_ids is not None and polish_followup_seconds <= 0:
                    if verbose:
                        print("[STAGE] FINAL_POLISH_RECHECK_LIMIT reason=time_budget", flush=True)
                    return
                attempt_started = time.monotonic()
                completed = _polish_one_asu(
                    asu_number,
                    polish_position,
                    len(polish_ids),
                    polish_round,
                    seconds=polish_followup_seconds if pending_ids is not None else None,
                )
                if pending_ids is not None:
                    polish_followup_seconds = max(
                        0.0, polish_followup_seconds - (time.monotonic() - attempt_started),
                    )
                if not completed:
                    polish_completed = False
                    break
                if not merge_adjacent:
                    continue

                # A polish can make this ASU touch another one. Check now,
                # before polishing ASUs whose assignments could become stale.
                # An accepted joint update (or legacy merge) restarts the round
                # with a freshly computed ASU order and territory.
                if _merge_committed_asus(
                    "FINAL_POLISH_MERGE",
                    f"round={polish_round} checked={polish_position}/{len(polish_ids)} "
                    f"after_asu={asu_number}",
                ):
                    restart_after_merge = True
                    break

            if not polish_completed or _stop_requested(stop_flag_path):
                break
            if restart_after_merge:
                pending_ids = None
                continue
            # Revisit only genuinely new opportunities. A smaller window cannot
            # introduce a selection unavailable at the previous attempt.
            pending_ids = set()
            for candidate_id in _polish_asu_order(asu_id, u, E, tau):
                previous = polish_last_windows.get(candidate_id)
                if previous is None or candidate_id in polish_skipped_ids:
                    continue
                current = np.flatnonzero(asu_id == candidate_id).tolist()
                root = _pick_capacity_root(current, u, E, P, tau)
                window = _reachable_polish_window(root, candidate_id, asu_id, nb)
                if set(window) - previous:
                    pending_ids.add(candidate_id)
            if not pending_ids:
                break
            if polish_followup_rounds >= 3 or polish_followup_seconds <= 0:
                if verbose:
                    print(
                        f"[STAGE] FINAL_POLISH_RECHECK_LIMIT pending={len(pending_ids)} "
                        f"rounds={polish_followup_rounds}/3 "
                        f"seconds_remaining={polish_followup_seconds:.3f}",
                        flush=True,
                    )
                break
            polish_followup_rounds += 1
            if verbose:
                print(
                    f"[STAGE] FINAL_POLISH_RECHECK reason=reachable_window_grew "
                    f"queued={len(pending_ids)} followup_round={polish_followup_rounds}/3 "
                    f"seconds_remaining={polish_followup_seconds:.3f} priority=q_surplus",
                    flush=True,
                )

    def _settle_late_merges(stage: str) -> bool:
        if not _merge_committed_asus(f"{stage}_MERGE"):
            return False
        _run_final_polish()
        if harvest_connectivity_free_asus:
            while _merge_committed_asus(f"{stage}_TOUCHING_JOINT"):
                _run_final_polish()
        return True

    _run_final_polish()

    # Joint regional exchanges account for donor feasibility during selection,
    # before the larger takeover's sequential donor-repair attempt.
    while polish_time_limit > 0 and not _stop_requested(stop_flag_path):
        exchanged = _regional_exchange_pass(
            asu_id, nb, u, E, P, tau, pop_thresh, polish_time_limit, workers,
            max_nodes=polish_max_nodes, exact_nodes=exact_nodes_per_asu,
            stop_path=stop_flag_path, skip_path=skip_flag_path, log=verbose,
            exchange_state=regional_exchange_state,
            stop_after_gain=True,
        )
        if np.array_equal(exchanged, asu_id):
            break
        asu_id = exchanged
        remaining = asu_id <= 0
        _emit_progress("REGIONAL_EXCHANGE")
        _settle_late_merges("REGIONAL_EXCHANGE")

    # ---- Single-ASU full-visibility takeover pass ----
    # After polish/merge settles, let the single biggest (by unemployment
    # captured) committed ASU re-solve against every tract in the state --
    # including tracts already claimed by OTHER ASUs, not just unassigned
    # ones. Any donor ASU that loses tracts is re-checked with component_ok;
    # damaged donors are repaired from their remnants and unassigned tracts,
    # with the takeover and all surviving ASUs fixed. The whole takeover is accepted only
    # if it strictly increases total unemployment captured across all
    # surviving ASUs; otherwise every assignment is left untouched.
    if polish_time_limit > 0 and not _stop_requested(stop_flag_path):
        committed_ids_takeover = np.unique(asu_id[asu_id > 0]).astype(int).tolist()
        if committed_ids_takeover:
            big_asu_id = max(
                committed_ids_takeover,
                key=lambda asu_number: (
                    int(u[np.where(asu_id == asu_number)[0]].sum()),
                    asu_number,
                ),
            )
            current_global = np.where(asu_id == big_asu_id)[0].astype(int).tolist()
            total_before = int(u[np.where(asu_id > 0)[0]].sum())
            if component_ok(current_global, u, E, P, tau, pop_thresh, nb):
                current_objective = int(u[current_global].sum())
                root_local = _pick_capacity_root(current_global, u, E, P, tau)
                if "geoid" in df.columns:
                    stable_values = [str(g) for g in df["geoid"].tolist()]
                else:
                    stable_values = [str(node).zfill(12) for node in range(n)]
                stable_order = sorted(
                    range(n), key=lambda node: (stable_values[node], node)
                )
                takeover_tie_rank = [0] * n
                for rank, node in enumerate(stable_order):
                    takeover_tie_rank[node] = rank

                if verbose:
                    print(
                        f"\n[STAGE] SINGLE_ASU_TAKEOVER asu={big_asu_id} "
                        f"total_unemp={total_before} "
                        f"asu_unemp={current_objective}",
                        flush=True,
                    )
                    print(
                        f"\n[SINGLE-ASU TAKEOVER] ASU {big_asu_id} "
                        f"(unemp={current_objective}, tracts={len(current_global)}) "
                        f"now sees all {n} tracts ({polish_time_limit:.1f}s); "
                        f"current total captured={total_before}",
                        flush=True,
                    )

                result = _solve_window(
                    nb_local=nb,
                    u_g=u,
                    E_g=E,
                    P_g=P,
                    tau=tau,
                    pop_thresh=pop_thresh,
                    root_local=root_local,
                    time_limit=polish_time_limit,
                    workers=workers,
                    rel_gap=rel_gap,
                    log=verbose,
                    hint=current_global,
                    hint_obj=current_objective,
                    deterministic_ties=deterministic_ties,
                    tie_break_rank=takeover_tie_rank,
                    objective_shaving=objective_shaving,
                    use_root_articulation_implications=use_root_articulation_implications,
                    use_signed_flow=use_signed_flow,
                    use_arborescence=use_arborescence,
                    configure_subsolvers=configure_subsolvers,
                    use_tract_first_search=use_tract_first_search,
                    use_flow_first_search=use_flow_first_search,
                    use_tract_capacity_search=use_tract_capacity_search,
                    use_flow_capacity_hybrid_search=use_flow_capacity_hybrid_search,
                    incumbent_stall_seconds=incumbent_stall_seconds,
                    use_flow_count_envelope=use_flow_count_envelope,
                    use_small_root_separators=use_small_root_separators,
                    root_separator_max_size=root_separator_max_size,
                    root_separator_clause_limit=root_separator_clause_limit,
                    root_separator_target_limit=root_separator_target_limit,
                    use_separator_cardinality_bounds=use_separator_cardinality_bounds,
                    solution_pool_size=solution_pool_size,
                    use_bridge_edge_bounds=use_bridge_edge_bounds,
                    use_articulation_edge_bounds=use_articulation_edge_bounds,
                    use_distance_flow_bounds=use_distance_flow_bounds,
                    use_global_capacity_cardinality_bound=use_global_capacity_cardinality_bound,
                    use_bridge_subtree_pruning=use_bridge_subtree_pruning,
                    max_nodes=None,
                    stop_flag_path=stop_flag_path,
                    skip_flag_path=skip_flag_path,
                    objective_no_improve_stop=current_objective,
                )

                if result is None:
                    if verbose:
                        print(
                            "  [SINGLE-ASU TAKEOVER] no replacement found",
                            flush=True,
                        )
                else:
                    new_global = sorted(set(int(i) for i in result.sel_idx_local))
                    new_objective = int(u[new_global].sum())
                    bound_stopped_no_gain = (
                        result.status == "BOUND_STOPPED_FEASIBLE"
                        and new_objective <= current_objective
                    )
                    if bound_stopped_no_gain:
                        if verbose:
                            print(
                                "  [SINGLE-ASU TAKEOVER] early stop: objective "
                                "upper bound cannot beat current ASU unemployment",
                                flush=True,
                            )
                    elif new_objective >= current_objective and component_ok(
                        new_global, u, E, P, tau, pop_thresh, nb
                    ):
                        new_set = set(new_global)
                        current_set = set(current_global)
                        added = new_set - current_set

                        donor_ids = sorted(
                            {
                                int(asu_id[node])
                                for node in added
                                if asu_id[node] > 0 and asu_id[node] != big_asu_id
                            }
                        )

                        trial_asu_id = asu_id.copy()
                        trial_asu_id[list(current_set)] = -1
                        dropped_donor_ids: List[int] = []
                        repaired_donor_ids: List[int] = []
                        for donor_id in donor_ids:
                            donor_global = np.where(asu_id == donor_id)[0].astype(int).tolist()
                            donor_remaining = sorted(set(donor_global) - added)
                            trial_asu_id[donor_global] = -1
                            available_for_repair = sorted(
                                set(np.where(trial_asu_id < 0)[0].astype(int).tolist())
                                - new_set
                            )
                            if verbose:
                                print(
                                    f"  [SINGLE-ASU TAKEOVER] repairing donor ASU "
                                    f"{donor_id}: survivors={len(donor_remaining)}, "
                                    f"available={len(available_for_repair)}",
                                    flush=True,
                                )
                            repaired_donor = _repair_takeover_donor(
                                donor_remaining,
                                available_for_repair,
                                nb,
                                u,
                                E,
                                P,
                                tau,
                                pop_thresh,
                                polish_time_limit,
                                workers,
                                stable_values=stable_values,
                                rel_gap=rel_gap,
                                log=verbose,
                                solve_kwargs={
                                    "feasibility_cache": feasibility_cache,
                                    "deterministic_ties": deterministic_ties,
                                    "objective_shaving": objective_shaving,
                                    "use_root_articulation_implications": use_root_articulation_implications,
                                    "use_signed_flow": use_signed_flow,
                                    "use_arborescence": use_arborescence,
                                    "configure_subsolvers": configure_subsolvers,
                                    "use_tract_first_search": use_tract_first_search,
                                    "use_flow_first_search": use_flow_first_search,
                                    "use_tract_capacity_search": use_tract_capacity_search,
                                    "use_flow_capacity_hybrid_search": use_flow_capacity_hybrid_search,
                                    "incumbent_stall_seconds": incumbent_stall_seconds,
                                    "use_flow_count_envelope": use_flow_count_envelope,
                                    "use_small_root_separators": use_small_root_separators,
                                    "root_separator_max_size": root_separator_max_size,
                                    "root_separator_clause_limit": root_separator_clause_limit,
                                    "root_separator_target_limit": root_separator_target_limit,
                                    "use_separator_cardinality_bounds": use_separator_cardinality_bounds,
                                    "solution_pool_size": solution_pool_size,
                                    "use_bridge_edge_bounds": use_bridge_edge_bounds,
                                    "use_articulation_edge_bounds": use_articulation_edge_bounds,
                                    "use_distance_flow_bounds": use_distance_flow_bounds,
                                    "use_global_capacity_cardinality_bound": use_global_capacity_cardinality_bound,
                                    "use_bridge_subtree_pruning": use_bridge_subtree_pruning,
                                    "max_nodes": polish_max_nodes,
                                    "stop_flag_path": stop_flag_path,
                                    "skip_flag_path": skip_flag_path,
                                },
                            )
                            if repaired_donor:
                                trial_asu_id[repaired_donor] = donor_id
                                repaired_donor_ids.append(donor_id)
                            else:
                                dropped_donor_ids.append(donor_id)
                        trial_asu_id[new_global] = big_asu_id

                        for donor_id in list(dropped_donor_ids):
                            if _stop_requested(stop_flag_path):
                                break
                            remnants = np.flatnonzero(
                                (asu_id == donor_id) & (trial_asu_id <= 0)
                            ).tolist()
                            if not remnants:
                                continue
                            if verbose:
                                print(f"[STAGE] TAKEOVER_DONOR_REPAIR asu={donor_id}", flush=True)
                            repaired, repair_status = _search_unassigned_asu(
                                np.flatnonzero(trial_asu_id <= 0).tolist(), nb,
                                u, E, P, tau, pop_thresh, polish_time_limit, workers,
                                stop_flag_path, donor_nodes=remnants,
                                max_nodes=polish_max_nodes, exact_nodes=exact_nodes_per_asu,
                            )
                            if repaired:
                                trial_asu_id[repaired] = donor_id
                                dropped_donor_ids.remove(donor_id)
                            if verbose:
                                print(f"[DONOR REPAIR] ASU {donor_id}: {repair_status}, "
                                      f"retained unemployment={int(u[repaired].sum())}", flush=True)

                        total_after = int(u[np.where(trial_asu_id > 0)[0]].sum())
                        if total_after > total_before:
                            asu_id = trial_asu_id
                            remaining = asu_id < 0
                            _emit_progress("SINGLE_ASU_TAKEOVER")
                            if verbose:
                                print(
                                    f"  [OK] SINGLE-ASU TAKEOVER accepted: ASU "
                                    f"{big_asu_id} tracts={len(new_global)} "
                                    f"(+{len(added)}), unemp={new_objective}, "
                                    f"total unemployment {total_before} -> "
                                    f"{total_after}"
                                    + (
                                        f"; dropped donor ASU(s): {dropped_donor_ids}"
                                        if dropped_donor_ids else ""
                                    ),
                                    flush=True,
                                )
                            _settle_late_merges("SINGLE_ASU_TAKEOVER")
                        elif verbose:
                            print(
                                "  [SINGLE-ASU TAKEOVER] rejected: total "
                                f"unemployment would not increase "
                                f"({total_before} -> {total_after})",
                                flush=True,
                            )
                    elif verbose:
                        print(
                            "  [SINGLE-ASU TAKEOVER] rejected: invalid or "
                            "non-improving replacement for the biggest ASU",
                            flush=True,
                        )

    # Check the final residual graph after takeover/repair, including singleton
    # components. Never equate a time limit or ASU limit with infeasibility.
    residual_check = dict(status="UNRESOLVED", exhausted=False, added_asus=0,
                          components_checked=0, unresolved_components=[])
    pending_components = list(_connected_components(nb, asu_id <= 0))
    while pending_components:
        if _stop_requested(stop_flag_path):
            residual_check["status"] = "STOPPED"
            break
        # Small components first; each solve receives the full worker budget.
        pending_components.sort(key=lambda nodes: (len(nodes), min(nodes)), reverse=True)
        component = pending_components.pop()
        if verbose:
            print(f"[STAGE] FINAL_RESIDUAL_CHECK tracts={len(component)} "
                  f"pending_components={len(pending_components)}", flush=True)
        selected, residual_status = _search_unassigned_asu(
            component, nb, u, E, P, tau, pop_thresh,
            polish_time_limit, workers, stop_flag_path,
            max_nodes=polish_max_nodes, exact_nodes=exact_nodes_per_asu,
        )
        residual_check["components_checked"] += 1
        if residual_status == "INFEASIBLE":
            continue
        if not selected:
            residual_check["unresolved_components"].append(
                dict(nodes=list(map(int, component)), status=residual_status))
            continue
        if len(np.unique(asu_id[asu_id > 0])) >= max_asus:
            residual_check.update(status="MAX_ASUS_REACHED",
                                  feasible_unassigned_tracts=selected)
            break
        new_id = int(asu_id.max(initial=0)) + 1
        asu_id[selected] = new_id
        remaining = asu_id <= 0
        component_mask = np.zeros(n, dtype=bool)
        component_mask[component] = True
        component_mask[selected] = False
        pending_components.extend(_connected_components(nb, component_mask))
        residual_check["added_asus"] += 1
        _emit_progress("FINAL_RESIDUAL_ADDED")
        if verbose:
            print(f"[FINAL RESIDUAL] Added ASU {new_id}: "
                  f"unemployment={int(u[selected].sum())}", flush=True)
        if _settle_late_merges("FINAL_RESIDUAL"):
            # Restart from live assignments: polishing may consume queued tracts
            # or release new ones, invalidating earlier residual certificates.
            pending_components = list(_connected_components(nb, asu_id <= 0))
            residual_check["unresolved_components"] = []
    else:
        unresolved = residual_check["unresolved_components"]
        residual_check.update(status=unresolved[0]["status"] if unresolved else "INFEASIBLE",
                              exhausted=not unresolved)
    residual_check["pending_components"] = len(pending_components)
    residual_check["scope"] = "Current unassigned tracts, with existing ASUs fixed and configured tract limits."
    if verbose:
        print(f"[FINAL RESIDUAL] {residual_check}", flush=True)

    # Merge operations deliberately retain the smallest member ID internally,
    # which can leave gaps. Compact once all solver bookkeeping is finished so
    # the returned result guarantees ASU IDs 1..n with no missing labels.
    asu_id = _sequential_asu_ids(asu_id)
    n_asu_final = int(np.unique(asu_id[asu_id > 0]).size)

    # Final summary if stopped due to no high-UR tracts
    if verbose and k < max_asus:
        rem_idx_final = np.where(remaining)[0]
        if rem_idx_final.size > 0:
            max_ur_remaining = UR[rem_idx_final].max() * 100
            print(f"\nStopped after {n_asu_final} ASUs. Max UR among {rem_idx_final.size} remaining tracts: {max_ur_remaining:.3f}%", flush=True)

    _emit_progress("DONE")

    return {"asu_id": asu_id.tolist(), "n_asu": n_asu_final,
            "residual_check": residual_check}


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
    ap.add_argument(
        "--parallel-asus",
        type=int,
        default=1,
        help=(
            "Maximum concurrent ASU window and standalone expansion solves; "
            "1 runs ASU solves sequentially"
        ),
    )
    ap.add_argument(
        "--no-merge-adjacent",
        action="store_true",
        help=(
            "Disable touching-ASU merges in normal batches and standalone "
            "expansion rounds"
        ),
    )
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
        "--use-flow-first-search",
        action="store_true",
        help=(
            "Enable an experimental partial fixed-search worker that chooses "
            "the widest flow domain, breaks ties by incident tract UR and "
            "unemployment, and selects the minimum value"
        ),
    )
    ap.add_argument(
        "--use-tract-capacity-search",
        action="store_true",
        help=(
            "Enable an experimental partial fixed-search worker that branches "
            "nonnegative tract UR-surplus first, then rejects negative surplus"
        ),
    )
    ap.add_argument(
        "--use-flow-capacity-hybrid-search",
        action="store_true",
        help=(
            "Enable an experimental hybrid worker: flow-first prefix, "
            "tract-capacity prefixes, then far-from-root magnitude minimization"
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
        "--use-articulation-edge-bounds",
        action="store_true",
        help=(
            "Tighten flow domains on root-oriented articulation gateway edges "
            "(reverse direction forced to 0); unproven, opt-in"
        ),
    )
    ap.add_argument(
        "--use-distance-flow-bounds",
        action="store_true",
        help=(
            "Cap each flow direction by max_selected - 1 - BFS root distance "
            "of its tail node; sound on cycles, unproven, opt-in"
        ),
    )
    ap.add_argument(
        "--use-global-capacity-cardinality-bound",
        action="store_true",
        help=(
            "Add a window-wide cap on jointly-selected below-threshold tracts "
            "derived from the UR-surplus knapsack; unproven, opt-in"
        ),
    )
    ap.add_argument(
        "--use-bridge-subtree-pruning",
        action="store_true",
        help=(
            "Hard-fix bridge-gated gateway tracts to 0 when even crediting the "
            "far subtree's full UR-surplus can't offset its own moat cost; "
            "unproven, opt-in"
        ),
    )
    ap.add_argument(
        "--use-connectivity-free-repair",
        action="store_true",
        help=(
            "Solve the UR/population relaxation without connectivity, repair "
            "its selected components, and use an improved valid result as a hint"
        ),
    )
    ap.add_argument(
        "--connectivity-free-time-limit",
        type=float,
        default=10.0,
        help="Time limit in seconds for the connectivity-free hint relaxation",
    )
    ap.add_argument(
        "--use-graph-cut-repair",
        action="store_true",
        help=(
            "Re-optimize the connectivity-free relaxation using lazy "
            "vertex-separator cuts only (no flow phase, no round cap); "
            "independent of --use-connectivity-free-repair"
        ),
    )
    ap.add_argument(
        "--graph-cut-repair-time-limit",
        type=float,
        default=30.0,
        help="Time limit in seconds for the graph-cut-only re-optimization",
    )
    ap.add_argument(
        "--harvest-connectivity-free-asus",
        action="store_true",
        help=(
            "Commit independently valid connected components from the "
            "connectivity-free relaxation as separate ASUs"
        ),
    )
    ap.add_argument(
        "--harvest-all-connectivity-free-components",
        action="store_true",
        help=(
            "With --harvest-connectivity-free-asus, seed the territory "
            "partition/expansion step with every relaxed component, not only "
            "the ones already independently valid; expanded units that still "
            "fail population/UR/connectivity are released, not committed"
        ),
    )
    ap.add_argument(
        "--statewide-joint",
        action="store_true",
        help="Experimental separate strategy: one joint model over all input tracts",
    )
    ap.add_argument("--statewide-joint-time-limit", type=float, default=None,
                    help="Seconds for the statewide joint model; defaults to --time-limit")
    ap.add_argument("--statewide-seed-time-limit", type=float, default=60.0,
                    help="Separate statewide relaxation seed-search budget; 0 skips it")
    ap.add_argument(
        "--no-statewide-relaxed-hint", action="store_false", dest="statewide_relaxed_hint",
        default=True, help="Use feasible-seed hints instead of the relaxed tract selection",
    )
    ap.add_argument(
        "--no-statewide-tightening", action="store_false", dest="statewide_tighten_model",
        default=True, help="Disable additional joint population/component/flow bounds for comparison",
    )
    ap.add_argument(
        "--statewide-graph-cuts", action="store_true",
        help="Experimental statewide separators, seed-distance rows and connectivity cut pre-pass",
    )
    ap.add_argument(
        "--joint-partition-expansion",
        action="store_true",
        help="Experimental: jointly expand up to three neighboring harvested seeds per solve",
    )
    ap.add_argument(
        "--standalone-expansion-time-limit",
        type=float,
        default=30.0,
        help=(
            "CP-SAT seconds per standalone ASU expansion; 0 commits the "
            "relaxed components without expansion"
        ),
    )
    ap.add_argument(
        "--final-asu-polish-time-limit",
        type=float,
        default=None,
        help=(
            "CP-SAT seconds for each final ASU solve against all remaining "
            "tracts; defaults to --standalone-expansion-time-limit"
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
        "--exact-nodes-per-asu",
        type=int,
        default=None,
        help=(
            "Fix every per-ASU CP-SAT solve to exactly this many tracts "
            "instead of leaving size unconstrained or merely capped (takes "
            "precedence over --max-nodes-per-asu); disables the combine, "
            "final-polish, and capacity-sweep phases"
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
    ap.add_argument(
        "--combine-time-limit",
        type=int,
        default=None,
        help=(
            "CP-SAT time limit (seconds) for the uncapped combine/re-solve pass "
            "(defaults to --time-limit when omitted)"
        ),
    )
    ap.add_argument(
        "--use-capacity-sweep",
        action="store_true",
        help=(
            "After the main loop (and combine phase, if any), repeatedly seed a "
            "brand-new standalone ASU from the remaining tract with the highest "
            "UR-surplus, restricted to a window built only from other remaining "
            "tracts, until no remaining tract has positive surplus left"
        ),
    )
    ap.add_argument(
        "--capacity-sweep-time-limit",
        type=float,
        default=30.0,
        help="CP-SAT seconds per standalone ASU solve during the capacity sweep pass",
    )
    ap.add_argument(
        "--incumbent-stall-seconds",
        type=float,
        default=None,
        help=(
            "Finish each CP-SAT solve early, keeping its current incumbent, once "
            "this many seconds pass with no incumbent improvement (default: "
            "disabled)"
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
    custom_modes = [
        ("--use-tract-first-search", args.use_tract_first_search),
        ("--use-flow-first-search", args.use_flow_first_search),
        ("--use-tract-capacity-search", args.use_tract_capacity_search),
        (
            "--use-flow-capacity-hybrid-search",
            args.use_flow_capacity_hybrid_search,
        ),
    ]
    enabled_modes = [name for name, enabled in custom_modes if enabled]
    if len(enabled_modes) > 1:
        ap.error(
            "Custom fixed-search workers are mutually exclusive: "
            + ", ".join(enabled_modes)
        )

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

    # Newer extracts rename the population column for the vintage year (e.g.
    # tract_pop2025); accept it as a drop-in for tract_pop2024.
    if "tract_pop2024" not in df.columns and "tract_pop2025" in df.columns:
        df = df.rename(columns={"tract_pop2025": "tract_pop2024"})

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
        use_flow_first_search=args.use_flow_first_search,
        use_tract_capacity_search=args.use_tract_capacity_search,
        use_flow_capacity_hybrid_search=args.use_flow_capacity_hybrid_search,
        use_flow_count_envelope=not args.no_flow_count_envelope,
        use_small_root_separators=not args.no_small_root_separators,
        root_separator_max_size=args.root_separator_max_size,
        root_separator_clause_limit=args.root_separator_clause_limit,
        root_separator_target_limit=args.root_separator_target_limit,
        use_separator_cardinality_bounds=not args.no_separator_cardinality_bounds,
        solution_pool_size=args.solution_pool_size,
        use_bridge_edge_bounds=args.use_bridge_edge_bounds,
        use_articulation_edge_bounds=args.use_articulation_edge_bounds,
        use_distance_flow_bounds=args.use_distance_flow_bounds,
        use_global_capacity_cardinality_bound=args.use_global_capacity_cardinality_bound,
        use_bridge_subtree_pruning=args.use_bridge_subtree_pruning,
        use_connectivity_free_repair=args.use_connectivity_free_repair,
        connectivity_free_time_limit=args.connectivity_free_time_limit,
        use_graph_cut_repair=args.use_graph_cut_repair,
        graph_cut_repair_time_limit=args.graph_cut_repair_time_limit,
        harvest_connectivity_free_asus=args.harvest_connectivity_free_asus,
        harvest_all_connectivity_free_components=args.harvest_all_connectivity_free_components,
        joint_partition_expansion=args.joint_partition_expansion,
        statewide_joint=args.statewide_joint,
        statewide_joint_time_limit=args.statewide_joint_time_limit,
        statewide_seed_time_limit=args.statewide_seed_time_limit,
        statewide_relaxed_hint=args.statewide_relaxed_hint,
        statewide_tighten_model=args.statewide_tighten_model,
        statewide_graph_cuts=args.statewide_graph_cuts,
        standalone_expansion_time_limit=args.standalone_expansion_time_limit,
        final_asu_polish_time_limit=args.final_asu_polish_time_limit,
        max_nodes_per_asu=args.max_nodes_per_asu,
        exact_nodes_per_asu=args.exact_nodes_per_asu,
        combine_capped_asus=not args.no_combine_capped_asus,
        combine_time_limit=args.combine_time_limit,
        use_capacity_sweep=args.use_capacity_sweep,
        capacity_sweep_time_limit=args.capacity_sweep_time_limit,
        incumbent_stall_seconds=args.incumbent_stall_seconds,
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
