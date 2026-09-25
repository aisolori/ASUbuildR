"""Bound carry, callback batching and local/global proof separation."""
import contextlib
import io
from pathlib import Path
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

from ortools.sat.python import cp_model

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as shared
import asu_component_global as global_solver


class ComponentTighteningTest(unittest.TestCase):
    def test_pool_is_bounded_deduplicated_and_prioritizes_final_candidate(self):
        seen = {(0,)}
        pool = global_solver._ComponentCutPool([1, 2, 3, 4, 5], seen, limit=2, max_terms=3)
        pool.add([[0], [2, 1], [1, 2], [3], [4]])
        self.assertEqual(set(pool.items), {(1, 2), (3,)})
        self.assertEqual(pool.terms, 3)
        self.assertEqual(pool.batch([[4]], limit=2), [(4,), (1, 2), (3,)])
        self.assertEqual(pool.batch([[4], [0], [2]], limit=0), [(4,), (2,)])
        # An oversized component still supplies an exact exclusion, not a
        # permanent failure to separate because of an auxiliary memory cap.
        large = global_solver._ComponentCutPool([1] * 5, set(), max_terms=2)
        large.add([[0, 1, 2, 3], [4]])
        self.assertEqual(list(large.items), [(0, 1, 2, 3)])

    def test_stalls_change_repair_budget_not_search_feasibility(self):
        self.assertEqual(global_solver._repair_schedule(4, 4, 0), (0.0, None))
        self.assertEqual(global_solver._repair_schedule(5, 0, 0), (1.0, "periodic"))
        self.assertEqual(global_solver._repair_schedule(9, 8, 5), (2.0, "stalled"))
        self.assertEqual(global_solver._repair_schedule(10, 9, 9), (0.0, None))
        self.assertEqual(global_solver._repair_schedule(13, 12, 9, 2), (0.0, None))
        self.assertEqual(global_solver._repair_schedule(25, 24, 9, 2), (2.0, "stalled"))

    def test_stalled_rounds_continue_past_old_cutoff_until_user_stop(self):
        settings = []
        clock = [0.0]

        class UnknownSolver:
            def __init__(self):
                self.parameters = cp_model.CpSolver().parameters

            def Solve(self, model, callback):
                clock[0] += 200.0
                settings.append((self.parameters.symmetry_level,
                                 self.parameters.cp_model_presolve,
                                 self.parameters.linearization_level))
                return cp_model.UNKNOWN

            def StatusName(self, status):
                return "UNKNOWN"

            def StopSearch(self):
                pass

        with (patch.object(shared, "_new_asu_solver", side_effect=UnknownSolver),
              patch.object(global_solver, "_repair_schedule", return_value=(0.0, None)),
              patch.object(global_solver.time, "monotonic", side_effect=lambda: clock[0]),
              patch.object(shared, "_stop_requested",
                           side_effect=lambda path: path == "test-stop" and len(settings) >= 8)):
            result = global_solver.solve_component_global(
                [[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5],
                .5, 5, time_limit=5000, workers=1, stop_path="test-stop")
        self.assertEqual(result["status"], "STOPPED")
        self.assertEqual(result["total_unemp"], 5)
        self.assertEqual(result["upper_bound"], 6)
        self.assertEqual(result["rounds"], 8)
        self.assertEqual([row[0] for row in settings], [2, 2, 2, 0, 0, 0, 0, 0])
        self.assertFalse(settings[-1][1])
        self.assertGreater(clock[0], 180)

    def test_candidate_producing_fallback_is_retained(self):
        policy = global_solver._SearchPolicy()
        for _ in range(6):
            policy.observe(False)
        self.assertEqual(policy.mode, "no_presolve")
        for _ in range(20):
            policy.observe(True, progressed=True)
            self.assertEqual(policy.mode, "no_presolve")
        policy.observe(False, skipped=True)
        self.assertEqual(policy.misses, 0)
        for _ in range(3):
            policy.observe(False)
        self.assertEqual(policy.mode, "no_symmetry")

    def test_late_feasible_candidates_and_misses_activate_fallback(self):
        policy = global_solver._SearchPolicy()
        self.assertIsNone(policy.observe(True, first_candidate_seconds=4.9))
        self.assertIsNone(policy.observe(False))
        reason = policy.observe(True, first_candidate_seconds=4.8)
        self.assertEqual(reason, "late_candidates")
        self.assertEqual(policy.mode, "no_symmetry")

    def test_early_feasible_candidates_without_gains_activate_fallback(self):
        policy = global_solver._SearchPolicy()
        for _ in range(11):
            self.assertIsNone(policy.observe(True, first_candidate_seconds=.2))
        policy.observe(True, skipped=True)
        self.assertEqual(policy.stagnant, 11)
        self.assertEqual(policy.observe(True, first_candidate_seconds=.2),
                         "objective_stagnation")
        self.assertEqual(policy.mode, "no_symmetry")
        for _ in range(11):
            policy.observe(True)
        policy.observe(True, progressed=True)
        self.assertEqual(policy.stagnant, 0)
        self.assertEqual(policy.mode, "no_symmetry")

    def test_new_cuts_and_objective_stagnation_do_not_terminate_search(self):
        calls = []
        clock = [0.0]

        class CutSolver:
            def __init__(self):
                self.parameters = cp_model.CpSolver().parameters

            def Solve(self, model, callback):
                clock[0] += 200.0
                calls.append(model)
                return cp_model.FEASIBLE

            def BooleanValue(self, var):
                return var.name in ("selected_0", f"selected_{len(calls)}")

            def BestObjectiveBound(self):
                return 14

            def StatusName(self, status):
                return "FEASIBLE"

            def StopSearch(self):
                pass

        with (patch.object(shared, "_new_asu_solver", side_effect=CutSolver),
              patch.object(global_solver, "_repair_schedule", return_value=(0.0, None)),
              patch.object(global_solver.time, "monotonic", side_effect=lambda: clock[0]),
              patch.object(shared, "_stop_requested",
                           side_effect=lambda path: path == "test-stop" and len(calls) >= 8)):
            result = global_solver.solve_component_global(
                [[] for _ in range(10)], [5] + [1] * 9, [0] + [2] * 9, [5] * 10,
                .5, 5, time_limit=5000, workers=1, stop_path="test-stop")
        self.assertEqual(result["status"], "STOPPED")
        self.assertEqual(result["rounds"], 8)
        self.assertEqual(result["cuts"], 7)
        self.assertEqual(result["total_unemp"], 5)
        self.assertFalse(result["optimal"])

    def test_round_watchdog_stops_search_before_overall_deadline(self):
        engines = []
        configure = shared._configure_cut_round_params

        class WaitingSolver:
            def __init__(self):
                self.parameters = cp_model.CpSolver().parameters
                self.stopped = threading.Event()
                engines.append(self)

            def Solve(self, model, callback):
                if not self.stopped.wait(1):
                    raise AssertionError("Round watchdog did not request a stop")
                return cp_model.UNKNOWN

            def StatusName(self, status):
                return "UNKNOWN"

            def StopSearch(self):
                self.stopped.set()

        def short_round(params, remaining):
            configure(params, remaining)
            params.max_time_in_seconds = .03

        log = io.StringIO()
        with (patch.object(shared, "_new_asu_solver", side_effect=WaitingSolver),
              patch.object(shared, "_configure_cut_round_params", side_effect=short_round),
              patch.object(global_solver, "_repair_schedule", return_value=(0.0, None)),
              contextlib.redirect_stdout(log)):
            started = time.monotonic()
            result = global_solver.solve_component_global(
                [[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5],
                .5, 5, time_limit=.25, workers=1, verbose=True)
        self.assertLess(time.monotonic() - started, 2)
        self.assertEqual(result["status"], "TIME_LIMIT")
        self.assertTrue(all(engine.stopped.is_set() for engine in engines))
        self.assertIn("round_end=ROUND_TIME_LIMIT", log.getvalue())
        self.assertIn("callback_seconds=", log.getvalue())

    def test_slow_shutdown_reduces_workers_and_preserves_incumbent(self):
        observed_workers = []
        configure = shared._configure_cut_round_params
        with tempfile.TemporaryDirectory() as directory:
            stop = Path(directory) / "stop.flag"

            class SlowShutdownSolver:
                def __init__(self):
                    self.parameters = cp_model.CpSolver().parameters
                    self.stopped = threading.Event()

                def Solve(self, model, callback):
                    observed_workers.append(self.parameters.num_search_workers)
                    if not self.stopped.wait(1):
                        raise AssertionError("Round watchdog did not fire")
                    if len(observed_workers) == 1:
                        time.sleep(.55)
                    else:
                        stop.touch()
                    return cp_model.UNKNOWN

                def StatusName(self, status):
                    return "UNKNOWN"

                def StopSearch(self):
                    self.stopped.set()

            def short_round(params, remaining):
                configure(params, remaining)
                params.max_time_in_seconds = .03

            with (patch.object(shared, "_new_asu_solver", side_effect=SlowShutdownSolver),
                  patch.object(shared, "_configure_cut_round_params", side_effect=short_round),
                  patch.object(global_solver, "_repair_schedule", return_value=(0.0, None))):
                result = global_solver.solve_component_global(
                    [[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5],
                    .5, 5, time_limit=5, workers=4, stop_path=str(stop))
        self.assertEqual(observed_workers, [4, 2])
        self.assertEqual(result["total_unemp"], 5)
        self.assertEqual(result["upper_bound"], 6)
        self.assertEqual(result["status"], "STOPPED")
        self.assertFalse(result["optimal"])

    def test_certified_bound_is_installed_in_next_master(self):
        nb = [[1], [0, 2], [1, 3], [2, 4], [3]]
        u = [5, 0, 8, 3, 10]
        snapshots, bounds = [], []
        original = cp_model.CpSolver.Solve

        def solve(engine, model, callback=None):
            proto = model.Proto()
            names = [var.name for var in proto.variables]
            ceilings = []
            for constraint in proto.constraints:
                has_linear = (constraint.has_linear() if hasattr(constraint, "has_linear")
                              else constraint.HasField("linear"))
                if not has_linear or constraint.enforcement_literal:
                    continue
                row = constraint.linear
                coefficients = {names[i]: c for i, c in zip(row.vars, row.coeffs)}
                if coefficients == {"selected_0": 5, "selected_2": 8,
                                    "selected_3": 3, "selected_4": 10}:
                    ceilings.append(list(row.domain)[-1])
            snapshots.append(ceilings)
            status = original(engine, model, callback)
            bounds.append(engine.BestObjectiveBound())
            return status

        with patch.object(cp_model.CpSolver, "Solve", new=solve):
            result = global_solver.solve_component_global(
                nb, u, [0, 100, 10, 5, 100], [5] * 5, .5, 5,
                time_limit=10, workers=1)
        self.assertTrue(result["optimal"])
        self.assertGreaterEqual(len(snapshots), 2)
        self.assertLess(bounds[0], sum(u))
        self.assertIn(int(bounds[0]), snapshots[1])

    def test_intermediate_invalid_components_are_cut_after_solve(self):
        calls = []
        test = self
        stop = None

        class FakeSolver:
            def __init__(self):
                self.parameters = cp_model.CpSolver().parameters

            def Solve(self, model, callback):
                calls.append(model)
                if len(calls) == 2:
                    stop.touch()
                    return cp_model.UNKNOWN
                before = len(model.Proto().constraints)
                callback.BooleanValue = lambda var: var.name in ("selected_0", "selected_3")
                callback.on_solution_callback()
                test.assertEqual(len(model.Proto().constraints), before,
                                 "Callback must not mutate the active model")
                return cp_model.FEASIBLE

            def BooleanValue(self, var):
                return var.name in ("selected_0", "selected_2")

            def BestObjectiveBound(self):
                return 16

            def StatusName(self, status):
                return "FEASIBLE"

            def StopSearch(self):
                pass

        log = io.StringIO()
        with tempfile.TemporaryDirectory() as directory:
            stop = Path(directory) / "stop.flag"
            with (patch.object(shared, "_new_asu_solver", side_effect=FakeSolver),
                  contextlib.redirect_stdout(log)):
                result = global_solver.solve_component_global(
                    [[1], [0, 2], [1, 3], [2, 4], [3]],
                    [5, 0, 8, 3, 10], [0, 100, 10, 5, 100], [5] * 5,
                    .5, 5, time_limit=10, workers=1, stop_path=str(stop), verbose=True)
        self.assertEqual(result["cuts"], 2)
        self.assertIn("callback_components=1 new_cuts=2", log.getvalue())
        self.assertIn("cuts=2 new_cuts=2", log.getvalue())

    def test_local_repair_cannot_supply_global_proof_or_invalid_assignments(self):
        import asu_component_repair as repairs

        class UnknownSolver:
            parameters = cp_model.CpSolver().parameters
            def Solve(self, model, callback):
                return cp_model.UNKNOWN
            def StatusName(self, status):
                return "UNKNOWN"
            def StopSearch(self):
                pass

        for proposed, expected in (([1, -1, 1], 35), ([1, 1, 1], 30)):
            with self.subTest(proposed=proposed), tempfile.TemporaryDirectory() as directory:
                stop = Path(directory) / "stop.flag"
                def repair(*args, **kwargs):
                    stop.touch()
                    return dict(asu_id=proposed, total_unemp=999, status="OPTIMAL",
                                optimal=True, upper_bound=1)
                with (patch.object(shared, "_new_asu_solver", return_value=UnknownSolver()),
                      patch.object(global_solver, "_repair_schedule", return_value=(1.0, "test")),
                      patch.object(repairs, "repair_selection", side_effect=repair)):
                    result = global_solver.solve_component_global(
                        [[1, 2], [0], [0]], [20, 10, 15], [0, 100, 100], [5] * 3,
                        .2, 5, initial_asu_id=[1, 1, -1], time_limit=10,
                        workers=1, stop_path=str(stop))
                self.assertEqual(result["total_unemp"], expected)
                self.assertEqual(result["upper_bound"], 45)
                self.assertFalse(result["optimal"])
                self.assertEqual(result["status"], "STOPPED")
                self.assertEqual(result["repair_attempts"], 1)


if __name__ == "__main__":
    unittest.main()
