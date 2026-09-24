"""Bounded, disposable neighborhood repairs for component-global search.

Nothing learned here is a bound or cut for the unrestricted global model.
Whole incumbent components can move when they fit in the tract budget. Larger
ones remain selected and become exact aggregate boundary terminals.
"""
from __future__ import annotations

import heapq
import math
import threading
import time

from ortools.sat.python import cp_model


def _groups(graph, selected):
    seen = set()
    for start, chosen in enumerate(selected):
        if not chosen or start in seen:
            continue
        seen.add(start)
        pending, nodes = [start], []
        while pending:
            node = pending.pop()
            nodes.append(node)
            for neighbor in graph[node]:
                if selected[neighbor] and neighbor not in seen:
                    seen.add(neighbor)
                    pending.append(neighbor)
        yield nodes


def repair_selection(
    graph, u, emp, pop, tau, pop_thresh, incumbent_ids, candidate_nodes, *,
    seconds=1.0, workers=1, cancellation=lambda: None, attempt=0, max_nodes=160,
):
    """Return only strictly improved, fully valid selections from a small repair.

    Inputs use the global solver's validated indexing and integer counts.
    candidate_nodes is a flat iterable of promising tracts; attempt rotates seed
    priority. Missing candidates search incumbent boundaries. cancellation
    returns None, STOPPED or SKIPPED without consuming a signal. This helper
    never publishes or returns a global bound, even for local infeasibility.
    """
    from asu_cpsat import as_fraction_tau, _configure_cut_round_params, _new_asu_solver

    started = time.monotonic()
    if not math.isfinite(float(seconds)) or seconds < 0:
        raise ValueError("Repair seconds must be finite and nonnegative")
    if int(max_nodes) != max_nodes or max_nodes < 1:
        raise ValueError("Repair max_nodes must be a positive integer")
    if int(workers) != workers or workers < 1:
        raise ValueError("Repair workers must be a positive integer")
    if any(len(values) != len(graph) for values in (u, emp, pop, incumbent_ids)):
        raise ValueError("Repair arrays must have one value per tract")
    deadline = started + min(5.0, float(seconds))
    selected = [label > 0 for label in incumbent_ids]
    baseline_u = sum(int(u[i]) for i, value in enumerate(selected) if value)
    answer = dict(asu_id=list(incumbent_ids), total_unemp=baseline_u,
                  improved=False, nodes=0, rounds=0, cuts=0, status="NO_IMPROVEMENT")

    def interrupted():
        signal = cancellation()
        if signal:
            return signal if isinstance(signal, str) else "STOPPED"
        if time.monotonic() >= deadline:
            return "TIME_LIMIT"
        return None

    def finish(status):
        answer["status"] = status
        answer["elapsed"] = time.monotonic() - started
        return answer

    signal = interrupted()
    if signal:
        return finish(signal)
    numerator, denominator = as_fraction_tau(float(tau))
    surplus = [denominator * int(a) - numerator * int(b) for a, b in zip(u, emp)]

    def qualifies(nodes, population=pop, q=surplus, unemployed=u, employed=emp):
        return (sum(int(population[i]) for i in nodes) >= pop_thresh
                and sum(int(q[i]) for i in nodes) >= 0
                and (tau <= 0 or any(int(unemployed[i]) + int(employed[i]) > 0
                                     for i in nodes)))

    # Store selected components, not copies of the national adjacency graph.
    # Components are whole units: a terminal never hides a removed connector.
    parent_groups = []
    parent_of = [-1] * len(graph)
    for nodes in _groups(graph, selected):
        signal = interrupted()
        if signal:
            return finish(signal)
        if not qualifies(nodes):
            return finish("INVALID_BASELINE")
        number = len(parent_groups)
        parent_groups.append(nodes)
        for node in nodes:
            parent_of[node] = number

    candidates = set()
    for raw in candidate_nodes:
        node = int(raw)
        if node != raw or not 0 <= node < len(graph):
            raise ValueError("Repair candidates must be zero-based tract indices")
        candidates.add(node)
    if not candidates:
        candidates = {neighbor for node, chosen in enumerate(selected) if chosen
                      for neighbor in graph[node] if not selected[neighbor]}
    if not candidates:
        candidates = set(range(len(graph)))
    signal = interrupted()
    if signal:
        return finish(signal)
    ranked = sorted(candidates, key=lambda node: (-int(u[node]), -surplus[node], node))
    if not ranked:
        return finish("EMPTY_NEIGHBORHOOD")
    offset = int(attempt) % len(ranked)
    ranked = ranked[offset:] + ranked[:offset]
    frontier, queued = [], set()

    def queue(node, distance):
        if node not in queued:
            queued.add(node)
            heapq.heappush(frontier, (distance, -int(u[node]), -surplus[node], node))

    queue(ranked[0], 0)
    movable, visited_parents = set(), set()
    seed_position = 1
    while len(movable) < max_nodes:
        signal = interrupted()
        if signal:
            return finish(signal)
        if not frontier:
            if seed_position >= len(ranked):
                break
            queue(ranked[seed_position], 0)
            seed_position += 1
            if not frontier:
                continue
        distance, _, _, node = heapq.heappop(frontier)
        if node in movable:
            continue
        parent = parent_of[node]
        if parent >= 0:
            if parent in visited_parents:
                continue
            visited_parents.add(parent)
            nodes = parent_groups[parent]
            if len(nodes) <= max_nodes - len(movable):
                movable.update(nodes)
            # A fixed oversized ASU is still an expansion anchor. Only its
            # exterior is explored; its aggregate membership stays fixed.
            for member in nodes:
                for neighbor in graph[member]:
                    if neighbor not in movable and parent_of[neighbor] != parent:
                        queue(neighbor, distance + 1)
        else:
            movable.add(node)
            for neighbor in graph[node]:
                if neighbor not in movable:
                    queue(neighbor, distance + 1)
    if not movable:
        return finish("EMPTY_NEIGHBORHOOD")
    actual = sorted(movable)
    index = {node: i for i, node in enumerate(actual)}
    count = len(actual)
    answer["nodes"] = count
    local_graph = [set() for _ in actual]
    local_u = [int(u[node]) for node in actual]
    local_e = [int(emp[node]) for node in actual]
    local_p = [int(pop[node]) for node in actual]
    local_q = [surplus[node] for node in actual]
    terminals = {}
    for node in actual:
        signal = interrupted()
        if signal:
            return finish(signal)
        for neighbor in graph[node]:
            if neighbor in index:
                other = index[neighbor]
            elif selected[neighbor]:
                parent = parent_of[neighbor]
                if parent not in terminals:
                    terminals[parent] = len(local_graph)
                    local_graph.append(set())
                    nodes = parent_groups[parent]
                    for dest, values in ((local_u, u), (local_e, emp),
                                         (local_p, pop), (local_q, surplus)):
                        dest.append(sum(int(values[i]) for i in nodes))
                other = terminals[parent]
            else:
                continue
            local_graph[index[node]].add(other)
            local_graph[other].add(index[node])

    signal = interrupted()
    if signal:
        return finish(signal)
    model = cp_model.CpModel()
    x = [model.NewBoolVar(f"repair_{node}") for node in actual]
    x.extend(model.NewConstant(1) for _ in terminals)
    objective = sum(local_u[i] * x[i] for i in range(count))
    baseline_local = sum(local_u[i] for i, node in enumerate(actual) if selected[node])
    model.Maximize(objective)
    model.Add(objective >= baseline_local + 1)
    for number, nodes in enumerate(_groups(local_graph, [True] * len(local_graph))):
        active = model.NewBoolVar(f"repair_region_{number}")
        model.AddMaxEquality(active, [x[i] for i in nodes])
        model.Add(sum(local_q[i] * x[i] for i in nodes) >= 0)
        model.Add(sum(local_p[i] * x[i] for i in nodes) >= pop_thresh * active)
        if tau > 0:
            model.Add(sum((local_u[i] + local_e[i]) * x[i] for i in nodes) >= active)
    for i, node in enumerate(actual):
        model.AddHint(x[i], int(selected[node]))

    def scan(chosen):
        return [nodes for nodes in _groups(local_graph, chosen)
                if not qualifies(nodes, local_p, local_q, local_u, local_e)]

    best_local, best_chosen = baseline_local, None
    cut_keys = set()
    final_status = "TIME_LIMIT"
    while True:
        signal = interrupted()
        if signal:
            final_status = signal
            break
        solver = _new_asu_solver()
        _configure_cut_round_params(solver.parameters, deadline - time.monotonic())
        solver.parameters.num_search_workers = int(workers)
        solver.parameters.random_seed = (int(attempt) + answer["rounds"]) % 2147483647
        solver.parameters.relative_gap_limit = 0.0
        solver.parameters.absolute_gap_limit = 0.0
        answer["rounds"] += 1
        finished = threading.Event()
        signals = []

        class ValidRepair(cp_model.CpSolverSolutionCallback):
            def on_solution_callback(self):
                nonlocal best_local, best_chosen
                if interrupted():
                    self.StopSearch()
                    return
                value = int(round(self.ObjectiveValue()))
                if value <= best_local:
                    return
                chosen = [self.BooleanValue(variable) for variable in x]
                if not scan(chosen):
                    best_local, best_chosen = value, chosen

        def watch():
            while not finished.wait(.05):
                reason = interrupted()
                if reason:
                    signals.append(reason)
                    solver.StopSearch()
                    return

        watcher = threading.Thread(target=watch, daemon=True)
        watcher.start()
        try:
            status = solver.Solve(model, ValidRepair())
        finally:
            finished.set()
            watcher.join()
        if status == cp_model.MODEL_INVALID:
            raise ValueError("Invalid neighborhood repair model: " + solver.SolutionInfo())
        if status == cp_model.INFEASIBLE:
            final_status = "LOCAL_EXHAUSTED"
            break
        bad = []
        if status in (cp_model.FEASIBLE, cp_model.OPTIMAL):
            chosen = [solver.BooleanValue(variable) for variable in x]
            bad = scan(chosen)
            value = sum(local_u[i] for i in range(count) if chosen[i])
            if not bad:
                if value > best_local:
                    best_local, best_chosen = value, chosen
                final_status = "IMPROVED" if best_chosen is not None else "NO_IMPROVEMENT"
                break
        if signals:
            final_status = signals[0]
            break
        for nodes in bad:
            key = tuple(sorted(nodes))
            if key in cut_keys:
                continue
            cut_keys.add(key)
            members = set(nodes)
            boundary = {neighbor for node in nodes for neighbor in local_graph[node]
                        if neighbor not in members}
            model.AddBoolOr([x[i].Not() for i in nodes] + [x[i] for i in boundary])
            answer["cuts"] += 1
        if best_chosen is not None:
            model.Add(objective >= best_local + 1)

    if best_chosen is not None:
        # Recheck the complete tract selection before returning an improvement.
        # A local solve's objective/bound is never a global certificate.
        complete = list(selected)
        for i, node in enumerate(actual):
            complete[node] = bool(best_chosen[i])
        groups = list(_groups(graph, complete))
        value = sum(int(u[i]) for i, chosen in enumerate(complete) if chosen)
        if value > baseline_u and all(qualifies(nodes) for nodes in groups):
            ids = [-1] * len(graph)
            for number, nodes in enumerate(groups, 1):
                for node in nodes:
                    ids[node] = number
            answer.update(asu_id=ids, total_unemp=value, improved=True)
    return finish(final_status)
