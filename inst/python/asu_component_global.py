"""Unlabelled ASU selection with exact, dynamically separated component cuts.

The integer model is an optimistic relaxation, not a feasibility certificate.
Only connected components satisfying all ASU rules enter the returned incumbent.
Invalid components are excluded without fixing a root, an ASU count, or an early
assignment. Given sufficient time, the finite cut loop preserves and can prove
the global optimum; a time-limited result includes a conservative upper bound.
"""
from __future__ import annotations

import math
import threading
import time
from typing import Callable

from ortools.sat.python import cp_model


_CUT_POOL_LIMIT = 128
_CUT_POOL_TERMS = 20000
_CALLBACK_BATCH_LIMIT = 8
_REGIONS_PER_ROUND = 12
_REGIONAL_MAX_TRACTS = 256
_REGION_MAX_NODES = 512
_REGIONAL_TERM_LIMIT = 250000
_REPAIR_INTERVAL = 5
_REPAIR_STALL_ROUNDS = 8
_REPAIR_STALL_COOLDOWN = 4
_UNKNOWN_FALLBACK_ROUNDS = 3
_OBJECTIVE_STALL_SECONDS = 180.0


class _ComponentCutPool:
    """Bound memory/callback work; only change the master after Solve returns."""
    def __init__(self, unemployment, seen, limit=_CUT_POOL_LIMIT,
                 max_terms=_CUT_POOL_TERMS):
        self.unemployment = unemployment
        self.seen = seen
        self.limit = limit
        self.max_terms = max_terms
        self.items = {}
        self.terms = 0

    def add(self, groups):
        for nodes in groups:
            if len(self.items) >= self.limit:
                break
            key = tuple(sorted(nodes))
            if not key or key in self.seen or key in self.items:
                continue
            # Allow one oversized component, otherwise bound retained terms.
            if self.items and self.terms + len(key) > self.max_terms:
                continue
            self.items[key] = sum(self.unemployment[i] for i in key)
            self.terms += len(key)

    def batch(self, final_groups, limit=_CALLBACK_BATCH_LIMIT):
        # The final candidate gets first priority so a pool full of older
        # candidates cannot starve the cuts required by the returned selection.
        selected, used = [], set()
        for group in final_groups:
            key = tuple(sorted(group))
            if key and key not in self.seen and key not in used:
                selected.append(key)
                used.add(key)
        if limit <= 0:
            return selected
        extra = 0
        for key in sorted(self.items, key=lambda k: (-self.items[k], len(k), k)):
            if key not in self.seen and key not in used:
                selected.append(key)
                used.add(key)
                extra += 1
                if extra >= limit:
                    break
        return selected


def _repair_schedule(rounds, stalled_rounds, last_repair_round,
                     zero_gain_streak=0):
    """Stalls change the heuristic work allocation, never certify optimality."""
    since = rounds - last_repair_round
    backoff = 1 << min(zero_gain_streak, 3)
    if stalled_rounds >= _REPAIR_STALL_ROUNDS and since >= _REPAIR_STALL_COOLDOWN * backoff:
        return 2.0, "stalled"
    if since >= _REPAIR_INTERVAL * backoff:
        return 1.0, "periodic"
    return 0.0, None


class _SearchPolicy:
    """Keep a mode that returns candidates; only repeated misses change it."""
    def __init__(self):
        self.mode = "standard"
        self.misses = 0

    def observe(self, candidate, skipped=False):
        if skipped:
            return
        if candidate:
            self.misses = 0
            return
        self.misses += 1
        if self.misses >= _UNKNOWN_FALLBACK_ROUNDS:
            self.mode = ("no_presolve" if self.mode == "no_symmetry" else "no_symmetry")
            self.misses = 0


def _integers(values, name, n):
    if len(values) != n:
        raise ValueError(f"{name} must have one value per tract")
    result = []
    for value in values:
        try:
            integer = int(value)
        except (TypeError, ValueError, OverflowError) as exc:
            raise ValueError(f"{name} must contain finite nonnegative integers") from exc
        if integer != value or integer < 0:
            raise ValueError(f"{name} must contain finite nonnegative integers")
        result.append(integer)
    return result


def _components(nb, selected):
    """One O(V+E) pass; never scan the full graph separately per component."""
    seen = bytearray(len(nb))
    for start in range(len(nb)):
        if not selected[start] or seen[start]:
            continue
        seen[start] = 1
        stack, nodes = [start], []
        while stack:
            node = stack.pop()
            nodes.append(node)
            for neighbor in nb[node]:
                if selected[neighbor] and not seen[neighbor]:
                    seen[neighbor] = 1
                    stack.append(neighbor)
        yield nodes


def solve_component_global(
    nb, u, E, P, tau, pop_thresh, *, time_limit=1200, workers=8,
    initial_asu_id=None, stop_path=None, skip_path=None, verbose=False,
    publish: Callable | None = None, rel_gap=None,
):
    """Maximize unemployed people captured by any number of qualifying ASUs.

    ``publish(ids, phase, metadata)`` runs only after a strict validated gain
    above the imported baseline or last published selection. ``time_limit`` is
    an overall wall budget, including model preparation and publishing; every
    CP-SAT invocation also has a quiet five-second limit. Stop retains the best
    valid result. Skip interrupts just the current round and continues.

    Adjacent qualifying groups can be represented by their qualifying union,
    so selected connected components lose no feasible objective value. There
    must be no additional ASU-count, separation, or maximum-size requirement.
    """
    started = time.monotonic()
    # Imported here so asu_cpsat can dispatch into this separate module.
    from asu_cpsat import (
        as_fraction_tau, _configure_cut_round_params, _consume_flag,
        _new_asu_solver, _stage_print, _stop_requested,
    )
    from asu_component_regions import add_region_constraints, iter_regions
    from asu_component_repair import repair_selection

    if not math.isfinite(float(time_limit)) or float(time_limit) < 0:
        raise ValueError("Component-global time_limit must be finite and nonnegative")
    if not math.isfinite(float(tau)) or not 0 <= float(tau) <= 1:
        raise ValueError("tau must be between zero and one")
    if int(pop_thresh) != pop_thresh or int(pop_thresh) < 0:
        raise ValueError("pop_thresh must be a nonnegative integer")
    if int(workers) != workers or workers < 1:
        raise ValueError("workers must be a positive integer")
    if rel_gap is not None and (not math.isfinite(float(rel_gap)) or rel_gap < 0):
        raise ValueError("rel_gap must be nonnegative")
    deadline = started + float(time_limit)
    n = len(nb)
    u, emp, pop = (_integers(v, name, n) for v, name in
                   ((u, "unemployment"), (E, "employment"), (P, "population")))
    num, den = as_fraction_tau(float(tau))
    surplus = [den * unemployed - num * employed
               for unemployed, employed in zip(u, emp)]
    total_u = sum(u)
    # CP-SAT uses signed int64 expressions, but objective bounds are doubles.
    # Keep integer objectives exactly representable and all linear sums safely
    # inside the integer domain (also guards normalizer/presolve arithmetic).
    if total_u > 2**53 - 1:
        raise ValueError("Total unemployment exceeds the exact objective-bound range")
    if max(sum(pop), sum(u) + sum(emp), sum(map(abs, surplus)), int(pop_thresh)) > 2**60:
        raise ValueError("Counts exceed the safe CP-SAT integer coefficient range")

    graph = []
    for node, neighbors in enumerate(nb):
        row = set()
        for value in neighbors:
            neighbor = int(value)
            if neighbor != value or not 0 <= neighbor < n:
                raise ValueError("Neighbor indices must be zero-based tract indices")
            if neighbor != node:
                row.add(neighbor)
        graph.append(row)
    # Input adjacency is undirected. Reject broken input instead of inventing
    # connectivity that could disagree with downstream result validation.
    if any(node not in graph[neighbor] for node, row in enumerate(graph) for neighbor in row):
        raise ValueError("Component-global requires symmetric tract adjacency")
    graph = [sorted(row) for row in graph]

    def scan(selected):
        valid, invalid = [], []
        for nodes in _components(graph, selected):
            population = sum(pop[i] for i in nodes)
            rate_surplus = sum(surplus[i] for i in nodes)
            labor = sum(u[i] + emp[i] for i in nodes)
            good = population >= pop_thresh and rate_surplus >= 0 and (tau <= 0 or labor > 0)
            (valid if good else invalid).append(nodes)
        return valid, invalid

    def assignments(groups):
        ids = [-1] * n
        for label, nodes in enumerate(groups, 1):
            for node in nodes:
                ids[node] = label
        return ids

    baseline = [False] * n
    if initial_asu_id is not None:
        if len(initial_asu_id) != n:
            raise ValueError("initial_asu_id must have one value per tract")
        for i, label in enumerate(initial_asu_id):
            try:
                integer = int(label)
            except (TypeError, ValueError, OverflowError) as exc:
                raise ValueError("initial_asu_id must contain finite integer labels") from exc
            if integer != label or not -1 <= integer <= 2**31 - 1:
                raise ValueError("initial_asu_id needs positive integer labels or 0/-1 for unassigned")
            baseline[i] = label > 0
    initial_groups, invalid = scan(baseline)
    if invalid:
        raise ValueError("The imported component-global baseline contains an invalid ASU component")
    best_ids = assignments(initial_groups)
    best_u = sum(u[i] for i, selected in enumerate(baseline) if selected)
    upper_bound = total_u
    rounds = 0
    cuts = 0
    regional_cuts = 0
    repair_attempts = 0
    repair_gains = 0
    outcome = "TIME_LIMIT"

    def metadata():
        gap = max(0, upper_bound - best_u)
        return dict(total_unemp=best_u, upper_bound=upper_bound, absolute_gap=gap,
                    relative_gap=gap / max(1, abs(best_u)), rounds=rounds, cuts=cuts,
                    regional_cuts=regional_cuts, repair_attempts=repair_attempts,
                    repair_gains=repair_gains)

    def consider(groups, phase):
        nonlocal best_u, best_ids
        value = sum(u[node] for group in groups for node in group)
        if value <= best_u:
            return False
        candidate = assignments(groups)
        # Publication/checkpoint failures must propagate; do not claim a commit
        # that the calling dashboard failed to persist.
        previous_u = best_u
        best_u = value
        details = metadata()
        try:
            if publish is not None:
                publish(candidate, phase, details)
        except Exception:
            best_u = previous_u
            raise
        best_ids = candidate
        if verbose:
            _stage_print(f"[STAGE] COMPONENT_GLOBAL_INCUMBENT phase={phase} "
                         f"valid_unemp={best_u} asus={len(groups)} "
                         f"upper_bound={upper_bound} total_unemp={best_u}", flush=True)
        return True

    def result(status):
        optimal = upper_bound <= best_u
        details = metadata()
        details.update(asu_id=best_ids, n_asu=max(best_ids, default=-1) if any(
            value > 0 for value in best_ids) else 0,
            status="OPTIMAL" if optimal else status, optimal=optimal)
        if verbose:
            _stage_print(f"[STAGE] COMPONENT_GLOBAL_COMPLETE status={details['status']} "
                         f"valid_unemp={best_u} upper_bound={upper_bound} "
                         f"absolute_gap={details['absolute_gap']} rounds={rounds} cuts={cuts}", flush=True)
        return details

    def preparation_status():
        if _stop_requested(stop_path):
            return "STOPPED"
        if time.monotonic() >= deadline:
            return "TIME_LIMIT"
        return None

    if _stop_requested(stop_path):
        return result("STOPPED")
    if _stop_requested(skip_path):
        _consume_flag(skip_path)
    if best_u >= upper_bound:
        return result("OPTIMAL")
    if time.monotonic() >= deadline:
        return result("TIME_LIMIT")

    # Cheap valid seeds are hints only: no tract is frozen by either heuristic.
    graph_groups = list(_components(graph, [True] * n))
    valid, _ = scan([True] * n)
    consider(valid, "component_global_seed")
    if not _stop_requested(stop_path) and time.monotonic() < deadline:
        high_rate = [best_ids[i] > 0 or surplus[i] >= 0 for i in range(n)]
        valid, _ = scan(high_rate)
        consider(valid, "component_global_seed")
    if best_u >= upper_bound:
        return result("OPTIMAL")
    if _stop_requested(stop_path):
        return result("STOPPED")
    if time.monotonic() >= deadline:
        return result("TIME_LIMIT")

    model = cp_model.CpModel()
    x = []
    for i in range(n):
        if i % 256 == 0:
            interrupted = preparation_status()
            if interrupted:
                return result(interrupted)
        x.append(model.NewBoolVar(f"selected_{i}"))
    objective = sum(u[i] * x[i] for i in range(n))
    model.Maximize(objective)
    # Necessary economic rows per *original* graph component prevent unrelated
    # islands from subsidizing one another, without choosing future ASU groups.
    for number, nodes in enumerate(graph_groups):
        interrupted = preparation_status()
        if interrupted:
            return result(interrupted)
        if sum(pop[i] for i in nodes) < pop_thresh or (tau > 0 and not any(
                u[i] + emp[i] for i in nodes)):
            for i in nodes:
                model.Add(x[i] == 0)
            continue
        active = model.NewBoolVar(f"region_active_{number}")
        model.AddMaxEquality(active, [x[i] for i in nodes])
        model.Add(sum(pop[i] * x[i] for i in nodes) >= int(pop_thresh) * active)
        model.Add(sum(surplus[i] * x[i] for i in nodes) >= 0)
        if tau > 0:
            model.Add(sum((u[i] + emp[i]) * x[i] for i in nodes) >= active)

    cut_components = set()
    cut_regions = set()
    regional_terms = 0
    ceiling = total_u
    stalled_rounds = 0
    unproductive_rounds = 0
    last_progress_time = time.monotonic()
    search_policy = _SearchPolicy()
    round_worker_limit = int(workers)
    last_repair_round = 0
    zero_repair_streak = 0
    repair_cache = set()
    cache_incumbent = best_u
    if verbose:
        _stage_print(f"[STAGE] COMPONENT_GLOBAL_START tracts={n} "
                     f"graph_components={len(graph_groups)} time_limit={time_limit}s "
                     f"cut_round_limit=5s regional_strengthening={n <= _REGIONAL_MAX_TRACTS} "
                     f"valid_unemp={best_u} upper_bound={upper_bound}", flush=True)

    while time.monotonic() < deadline:
        previous_best, previous_bound = best_u, upper_bound
        if _stop_requested(stop_path):
            outcome = "STOPPED"
            break
        if _stop_requested(skip_path):
            _consume_flag(skip_path)
        if upper_bound <= best_u:
            break
        if rel_gap is not None and (upper_bound - best_u) / max(1, abs(best_u)) <= rel_gap:
            outcome = "GAP_LIMIT"
            break
        if upper_bound < ceiling:
            # CP-SAT restarts lose search state. Retain the strongest certified
            # global bound as a real model constraint, not just a log number.
            model.Add(objective <= upper_bound)
            ceiling = upper_bound
            if verbose:
                _stage_print(f"[STAGE] COMPONENT_GLOBAL_BOUND upper_bound={ceiling} "
                             "carried_to_model=True", flush=True)
        search_mode = search_policy.mode
        # Keep the growing cut model reusable. The round-specific improvement
        # floor belongs only to a clone, so a fallback can admit the valid
        # incumbent without solver assumptions or losing any permanent cuts.
        round_model = model.Clone()
        round_x = [round_model.GetBoolVarFromProtoIndex(variable.Index())
                   for variable in x]
        if search_mode == "standard":
            round_model.Add(sum(u[i] * round_x[i] for i in range(n)) >= best_u + 1)
        for i, variable in enumerate(round_x):
            if i % 256 == 0:
                interrupted = preparation_status()
                if interrupted:
                    return result(interrupted)
            round_model.AddHint(variable, int(best_ids[i] > 0))
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            break
        solver = _new_asu_solver()
        _configure_cut_round_params(solver.parameters, remaining)
        solver.parameters.num_search_workers = round_worker_limit
        solver.parameters.random_seed = rounds % 2147483647
        if search_mode != "standard":
            solver.parameters.symmetry_level = 0
            solver.parameters.linearization_level = 0
        if search_mode == "no_presolve":
            solver.parameters.cp_model_presolve = False
        # A relaxed incumbent is not a feasible lower bound for this problem.
        # Gap stopping is checked above only against our validated incumbent.
        solver.parameters.relative_gap_limit = 0.0
        solver.parameters.absolute_gap_limit = 0.0
        rounds += 1
        finished = threading.Event()
        signal = []
        cut_pool = _ComponentCutPool(u, cut_components)
        model_vars = len(round_model.Proto().variables)
        model_constraints = len(round_model.Proto().constraints)
        solve_started = time.monotonic()
        solve_perf_started = time.perf_counter()
        round_budget = float(solver.parameters.max_time_in_seconds)
        round_deadline = min(deadline, solve_started + round_budget)
        stop_requested_at = []

        def watch():
            while not finished.wait(0.02):
                if _stop_requested(stop_path):
                    reason = "STOPPED"
                elif _stop_requested(skip_path):
                    reason = "SKIPPED"
                elif time.monotonic() >= deadline:
                    reason = "TIME_LIMIT"
                elif time.monotonic() >= round_deadline:
                    reason = "ROUND_TIME_LIMIT"
                else:
                    continue
                if not signal or signal[-1] != reason:
                    signal.append(reason)
                if not stop_requested_at:
                    stop_requested_at.append(time.perf_counter())
                # A request during native setup can precede the live search.
                # Repeat until Solve returns so that request is not lost.
                solver.StopSearch()

        class ValidIncumbent(cp_model.CpSolverSolutionCallback):
            def __init__(self):
                super().__init__()
                self.last_check = -math.inf
                self.first_candidate_seconds = None
                self.callback_seconds = 0.0
                self.callback_count = 0

            def on_solution_callback(self):
                entered = time.perf_counter()
                self.callback_count += 1
                try:
                    self.inspect_candidate()
                finally:
                    self.callback_seconds += time.perf_counter() - entered

            def inspect_candidate(self):
                now = time.monotonic()
                if self.first_candidate_seconds is None:
                    self.first_candidate_seconds = time.perf_counter() - solve_perf_started
                if _stop_requested(stop_path) or _stop_requested(skip_path) or now >= round_deadline:
                    self.StopSearch()
                    return
                # Large national graph scans are throttled, but the final
                # returned selection is always inspected below.
                if now - self.last_check < 0.1:
                    return
                self.last_check = now
                chosen = [self.BooleanValue(variable) for variable in round_x]
                valid_groups, invalid_groups = scan(chosen)
                cut_pool.add(invalid_groups)
                consider(valid_groups, "component_global")

        watcher = threading.Thread(target=watch, daemon=True)
        watcher.start()
        callback = ValidIncumbent()
        try:
            status = solver.Solve(round_model, callback)
        finally:
            finished.set()
            watcher.join()
        solve_seconds = time.perf_counter() - solve_perf_started
        native_seconds = (solver.WallTime() if callable(getattr(solver, "WallTime", None))
                          else None)
        stop_latency = (time.perf_counter() - stop_requested_at[0] if stop_requested_at else None)
        native_text = f"{native_seconds:.3f}" if native_seconds is not None else "NA"
        stop_text = f"{stop_latency:.3f}" if stop_latency is not None else "NA"
        first_candidate = (f"{callback.first_candidate_seconds:.3f}"
                           if callback.first_candidate_seconds is not None else "NA")
        branches = solver.NumBranches() if callable(getattr(solver, "NumBranches", None)) else "NA"
        conflicts = solver.NumConflicts() if callable(getattr(solver, "NumConflicts", None)) else "NA"
        diagnostics = (f"solve_seconds={solve_seconds:.3f} first_candidate_seconds={first_candidate} "
                       f"branches={branches} conflicts={conflicts} model_vars={model_vars} "
                       f"model_constraints={model_constraints} search_mode={search_mode} "
                       f"workers={round_worker_limit} round_budget_seconds={round_budget:.3f} "
                       f"callback_seconds={callback.callback_seconds:.3f} "
                       f"callbacks={callback.callback_count} "
                       f"native_seconds={native_text} stop_latency_seconds={stop_text} "
                       f"round_end={signal[-1] if signal else 'SOLVER_RETURNED'}")
        # StopSearch is cooperative. Reduce parallel startup/teardown pressure
        # after a material overrun, while preserving all feasible assignments.
        if solve_seconds > round_budget + 0.5 and round_worker_limit > 1:
            round_worker_limit = max(1, round_worker_limit // 2)
            if verbose:
                _stage_print(f"[STAGE] COMPONENT_GLOBAL_ROUND_OVERRUN round={rounds} "
                             f"solve_seconds={solve_seconds:.3f} budget={round_budget:.3f} "
                             f"next_workers={round_worker_limit}", flush=True)

        if status == cp_model.MODEL_INVALID:
            raise ValueError("Invalid component-global CP-SAT model: " + solver.SolutionInfo())
        if status == cp_model.INFEASIBLE:
            if search_mode != "standard":
                raise RuntimeError(
                    "Component-global fallback rejected a previously validated ASU selection")
            upper_bound = best_u
            break
        bad = []
        if status in (cp_model.FEASIBLE, cp_model.OPTIMAL):
            bound = solver.BestObjectiveBound()
            # UNKNOWN before presolve can report a meaningless zero bound.
            # FEASIBLE/OPTIMAL bounds are certified for the improving relaxation.
            if math.isfinite(bound):
                upper_bound = max(best_u, min(upper_bound, math.ceil(bound)))
            chosen = [solver.BooleanValue(variable) for variable in round_x]
            valid, bad = scan(chosen)
            consider(valid, "component_global")
        if _stop_requested(stop_path) or "STOPPED" in signal:
            outcome = "STOPPED"
            break
        skipped = _stop_requested(skip_path) or "SKIPPED" in signal
        if skipped:
            _consume_flag(skip_path)
            if verbose:
                _stage_print(f"[STAGE] COMPONENT_GLOBAL_ROUND_SKIPPED round={rounds}", flush=True)
        search_policy.observe(
            status in (cp_model.FEASIBLE, cp_model.OPTIMAL) or
            callback.first_candidate_seconds is not None, skipped=skipped)
        if upper_bound <= best_u or time.monotonic() >= deadline:
            if verbose:
                _stage_print(f"[STAGE] COMPONENT_GLOBAL_CUT_ROUND round={rounds} "
                             f"status={solver.StatusName(status)} invalid_components={len(bad)} "
                             f"valid_unemp={best_u} upper_bound={upper_bound} cuts={cuts} "
                             f"new_cuts=0 new_regions=0 {diagnostics}", flush=True)
            break

        batch = cut_pool.batch(bad)
        new_cuts, new_regions = 0, 0
        for nodes in batch:
            if _stop_requested(stop_path) or time.monotonic() >= deadline:
                break
            key = tuple(sorted(nodes))
            if key in cut_components:
                continue
            cut_components.add(key)
            member = set(nodes)
            boundary = sorted({neighbor for node in nodes for neighbor in graph[node]
                               if neighbor not in member})
            # Exactly this isolated invalid component is forbidden. A feasible
            # solution must omit a member or select a boundary tract.
            model.AddBoolOr([x[i].Not() for i in nodes] + [x[i] for i in boundary])
            cuts += 1
            new_cuts += 1

            # Without a selected boundary, every selected subset of this region
            # is a union of final ASUs. Its summed surplus must be nonnegative.
            # If the whole region lacks population/labor, no subset can qualify.
            # Reification avoids overflow-prone big-M boundary coefficients.
            boundary_active = None
            if boundary:
                boundary_active = model.NewBoolVar(f"cut_boundary_{cuts}")
                model.AddMaxEquality(boundary_active, [x[i] for i in boundary])
            if sum(pop[i] for i in nodes) < pop_thresh or (
                    tau > 0 and not any(u[i] + emp[i] for i in nodes)):
                stronger = model.Add(sum(x[i] for i in nodes) == 0)
            else:
                stronger = model.Add(sum(surplus[i] * x[i] for i in nodes) >= 0)
            if boundary_active is not None:
                stronger.OnlyEnforceIf(boundary_active.Not())

            # Add conditional eligibility for this component and small buffer
            # regions. These hold for ANY final ASU arrangement, not just the
            # candidate that exposed this region. Caps limit extra work only.
            for region in (() if n > _REGIONAL_MAX_TRACTS else
                           iter_regions(graph, nodes, max_nodes=_REGION_MAX_NODES, rings=2)):
                if preparation_status():
                    break
                if (new_regions >= _REGIONS_PER_ROUND or
                        regional_terms + len(region) > _REGIONAL_TERM_LIMIT):
                    break
                if region in cut_regions:
                    continue
                add_region_constraints(
                    model, x, graph, u, emp, pop, surplus, tau, pop_thresh,
                    region, name=f"global_region_{regional_cuts}")
                cut_regions.add(region)
                regional_terms += len(region)
                regional_cuts += 1
                new_regions += 1

        progressed = best_u > previous_best or upper_bound < previous_bound
        if progressed:
            stalled_rounds = 0
            last_progress_time = time.monotonic()
            if best_u > previous_best:
                zero_repair_streak = 0
        else:
            stalled_rounds += 1
        if progressed or new_cuts:
            unproductive_rounds = 0
        elif not skipped:
            unproductive_rounds += 1
        if verbose:
            _stage_print(f"[STAGE] COMPONENT_GLOBAL_CUT_BATCH round={rounds} "
                         f"callback_components={len(cut_pool.items)} "
                         f"new_cuts={new_cuts} new_regions={new_regions} "
                         f"cuts={cuts} regional_cuts={regional_cuts}", flush=True)
            _stage_print(f"[STAGE] COMPONENT_GLOBAL_CUT_ROUND round={rounds} "
                         f"status={solver.StatusName(status)} invalid_components={len(bad)} "
                         f"valid_unemp={best_u} upper_bound={upper_bound} cuts={cuts} "
                         f"new_cuts={new_cuts} new_regions={new_regions} "
                         f"stalled_rounds={stalled_rounds} unproductive_rounds={unproductive_rounds} "
                         f"objective_stall_seconds={time.monotonic() - last_progress_time:.3f} "
                         f"{diagnostics}", flush=True)

        repair_seconds, repair_reason = _repair_schedule(
            rounds, stalled_rounds, last_repair_round, zero_repair_streak)
        if repair_seconds and not skipped and not preparation_status():
            # On a new incumbent even a previously tried neighborhood changes.
            if best_u != cache_incumbent:
                repair_cache.clear()
                cache_incumbent = best_u
            seeds = sorted(set(batch) | set(tuple(sorted(group)) for group in bad),
                           key=lambda group: (-sum(u[i] for i in group), len(group), group))
            if not seeds:
                # A fully valid but time-limited relaxation may expose no bad
                # groups. Try an unassigned frontier of the saved selection.
                frontier = [i for i in range(n) if best_ids[i] <= 0 and
                            any(best_ids[j] > 0 for j in graph[i])]
                seeds = [(i,) for i in sorted(frontier, key=lambda i: (-u[i], i))]
            seed = next((group for group in seeds if group not in repair_cache), None)
            if seed is not None:
                if len(repair_cache) >= 256:
                    repair_cache.clear()
                repair_cache.add(seed)
                last_repair_round = rounds
                repair_attempts += 1
                before_repair = best_u

                def repair_cancelled():
                    if _stop_requested(stop_path):
                        return "STOPPED"
                    if _stop_requested(skip_path):
                        return "SKIPPED"
                    return None

                budget = min(repair_seconds, max(0.0, deadline - time.monotonic()))
                if verbose:
                    _stage_print(f"[STAGE] COMPONENT_GLOBAL_REPAIR round={rounds} "
                                 f"reason={repair_reason} time_limit={budget:.3f}s "
                                 f"seed_tracts={len(seed)} valid_unemp={best_u}", flush=True)
                repaired = repair_selection(
                    graph, u, emp, pop, tau, pop_thresh, best_ids, seed,
                    seconds=budget, workers=workers, cancellation=repair_cancelled,
                    attempt=repair_attempts - 1, max_nodes=160)
                candidate_ids = repaired.get("asu_id", [])
                # Independently validate the COMPLETE selection. Local bounds,
                # statuses, and claimed gains are not global proof information.
                if len(candidate_ids) == n:
                    valid_repair, invalid_repair = scan([label > 0 for label in candidate_ids])
                    if not invalid_repair:
                        consider(valid_repair, "component_global_repair")
                gain = best_u - before_repair
                repair_gains += gain
                if gain:
                    stalled_rounds = 0
                    unproductive_rounds = 0
                    zero_repair_streak = 0
                    last_progress_time = time.monotonic()
                else:
                    zero_repair_streak += 1
                if verbose:
                    _stage_print(f"[STAGE] COMPONENT_GLOBAL_REPAIR_COMPLETE round={rounds} "
                                 f"status={repaired.get('status', 'UNKNOWN')} "
                                 f"gain={gain} valid_unemp={best_u} upper_bound={upper_bound}", flush=True)
                if _stop_requested(skip_path):
                    _consume_flag(skip_path)

        if (stalled_rounds >= _UNKNOWN_FALLBACK_ROUNDS * 2 and
                time.monotonic() - last_progress_time >= _OBJECTIVE_STALL_SECONDS):
            outcome = "STALLED"
            if verbose:
                _stage_print(f"[STAGE] COMPONENT_GLOBAL_STALLED rounds_without_progress="
                             f"{stalled_rounds} seconds_without_progress="
                             f"{time.monotonic() - last_progress_time:.1f} "
                             f"valid_unemp={best_u} upper_bound={upper_bound}", flush=True)
            break

    if _stop_requested(stop_path):
        outcome = "STOPPED"
    return result(outcome)
