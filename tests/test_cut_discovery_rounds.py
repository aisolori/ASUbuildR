"""End split rounds on useful cuts, without mistaking incumbents for bounds."""
import contextlib
import io
import math
from pathlib import Path
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class CutDiscoveryRoundsTest(unittest.TestCase):
    def run_rounds(self, rounds, *, enabled=True, seen=None, max_rounds=25,
                   stall=5, n=3):
        model = solver.cp_model.CpModel()
        row = [model.NewBoolVar(f"x_{i}") for i in range(n)]
        roots = [model.NewBoolVar(f"r_{i}") for i in range(n)]
        for i in range(n):
            model.Add(roots[i] == int(i == 0))
            model.Add(roots[i] <= row[i])
        objective = model.NewIntVar(0, 1000, "objective")
        model.Add(objective == 10 * sum(row))
        model.Maximize(objective)
        nb = [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)]
        initial_rows = len(model.Proto().constraints)
        created, reports, proof, bounds, cancelled = [], [], [], [], [None]
        output = io.StringIO()
        test = self

        def valid(groups):
            return solver.component_ok(groups[0], np.full(n, 10), np.zeros(n, dtype=int),
                                       np.ones(n, dtype=int), .2, 1, nb)

        class FakeSolver:
            def __init__(self):
                self.parameters = SimpleNamespace()
                self.spec = rounds[len(created)]
                self.selection = set(self.spec.get("final", self.spec["candidates"][-1]))
                self.stop_calls = 0
                self.had_callback = False
                created.append(self)

            def Solve(self, supplied_model, callback=None):
                self.had_callback = callback is not None
                before = len(supplied_model.Proto().constraints)
                if callback is not None:
                    def stop():
                        self.stop_calls += 1
                    callback.StopSearch = stop
                    for index, candidate in enumerate(self.spec["candidates"]):
                        if self.spec.get("cancel_at") == index:
                            cancelled[0] = "SKIPPED"
                        members = set(candidate)
                        callback.BooleanValue = lambda v: (
                            int(v.name.split("_")[1]) in (members if v.name.startswith("x_") else {0}))
                        callback.on_solution_callback()
                        test.assertEqual(len(supplied_model.Proto().constraints), before,
                                         "callback changed the live model")
                return self.spec.get("status", solver.cp_model.FEASIBLE)

            def BooleanValue(self, var):
                return int(var.name.split("_")[1]) in (self.selection if var.name.startswith("x_") else {0})

            def BestObjectiveBound(self):
                return self.spec.get("bound", 100.25)

            def StatusName(self, status):
                return "OPTIMAL" if status == solver.cp_model.OPTIMAL else "FEASIBLE"

            def StopSearch(self):
                self.stop_calls += 1

        with patch.object(solver.cp_model, "CpSolver", FakeSolver), \
                patch.object(solver, "_configure_asu_solver_portfolio"), contextlib.redirect_stdout(output):
            result = solver._joint_connectivity_cut_pass(
                model, [row], [roots], nb, np.full(n, 10), [[0]], valid,
                math.inf, 2, lambda: cancelled[0], log=True, objective=objective,
                max_rounds=min(max_rounds, len(rounds)), upper_bound_stall_rounds=stall,
                bound_stall_only=True, seen_cuts=seen, proof_out=proof, bound_out=bounds,
                round_seconds=None, stop_on_new_cuts=enabled,
                report=lambda nodes, value: reports.append((nodes, value)))
        return SimpleNamespace(result=result, model=model, row=row, objective=objective,
                               created=created, proof=proof, bounds=bounds, reports=reports,
                               initial_rows=initial_rows, output=output.getvalue())

    def test_valid_unemployment_stall_counts_equal_and_disconnected_rounds(self):
        for status in (solver.cp_model.FEASIBLE, solver.cp_model.UNKNOWN):
            for candidate in ([0], [0, 2]):
                with self.subTest(status=status, candidate=candidate):
                    rounds = [dict(candidates=[candidate], bound=900-i, status=status)
                              for i in range(60)]
                    result = self.run_rounds(rounds, max_rounds=100, stall=25)
                    self.assertEqual(len(result.created), 50)
                    self.assertEqual(result.result[:2], ([[0]], 10))
                    self.assertEqual(result.proof, [False])
                    self.assertIn('valid_unemp_stall=50/50', result.output)
                    self.assertIn('stop_reason=VALID_UNEMP_STALL', result.output)
                    self.assertEqual(result.bounds, [851])

    def test_valid_callback_gain_resets_stall_even_with_disconnected_final(self):
        for status in (solver.cp_model.FEASIBLE, solver.cp_model.UNKNOWN):
            with self.subTest(status=status):
                rounds = [dict(candidates=[[0, 2]], bound=900-i, status=status)
                          for i in range(90)]
                rounds[24]['candidates'] = [[0, 1], [0, 2]]
                result = self.run_rounds(rounds, max_rounds=100, stall=25)
                self.assertEqual(len(result.created), 75)
                self.assertEqual(result.result[:2], ([[0, 1]], 20))
                self.assertRegex(result.output, r'CUT_ROUND round=25 .*valid_unemp=20 .*valid_unemp_stall=0/50')
                self.assertIn('valid_unemp_stall=50/50', result.output)
                self.assertIn('stop_reason=VALID_UNEMP_STALL', result.output)

    def test_useful_cut_stops_round_and_only_certified_bound_is_carried(self):
        result = self.run_rounds([dict(candidates=[[0, 2]])])
        self.assertEqual(result.created[0].stop_calls, 1)
        self.assertTrue(math.isinf(result.created[0].parameters.max_time_in_seconds))
        self.assertEqual(result.result, ([[0]], 10, "FEASIBLE"))
        self.assertEqual(result.bounds, [101])  # Ceiling of 100.25, not incumbent 20.
        self.assertEqual(result.proof, [False])
        self.assertIn("round_end=NEW_CONNECTIVITY_CUTS", result.output)
        self.assertIn("cuts_added=1", result.output)
        self.assertEqual(len(result.model.Proto().constraints), result.initial_rows + 2)
        # A better connected selection must survive both the new cut and bound.
        for var in result.row:
            result.model.Add(var == 1)
        check = solver.cp_model.CpSolver()
        check.parameters.num_search_workers = 1
        self.assertEqual(check.Solve(result.model), solver.cp_model.OPTIMAL)
        self.assertEqual(check.ObjectiveValue(), 30)

    def test_invalid_response_bounds_never_become_objective_caps(self):
        for bound in (0, float("nan"), float("inf")):
            with self.subTest(bound=bound):
                result = self.run_rounds([dict(candidates=[[0, 2]], bound=bound)])
                self.assertEqual(result.bounds, [None])
                self.assertEqual(len(result.model.Proto().constraints), result.initial_rows + 1)

    def test_known_cut_or_connected_incumbent_does_not_trigger_discovery_stop(self):
        known = self.run_rounds([dict(candidates=[[0, 2]])], seen={(0, 2, (2,))})
        connected = self.run_rounds([dict(candidates=[[0, 1]])])
        for result in (known, connected):
            self.assertEqual(result.created[0].stop_calls, 0)
            self.assertIn("round_end=SOLVER_RETURNED", result.output)
            self.assertIn("cuts_added=0", result.output)
        self.assertEqual(connected.result[:2], ([[0, 1]], 20))

    def test_valid_incumbent_before_discovery_is_retained_not_disconnected_final(self):
        for candidates in ([[0, 1], [0, 2]], [[0, 2], [0, 1]]):
            with self.subTest(candidates=candidates):
                result = self.run_rounds([dict(candidates=candidates, final=[0, 2])])
                self.assertEqual(result.result[:2], ([[0, 1]], 20))
                self.assertEqual(result.reports, [([0, 1], 20)])
                self.assertEqual(result.proof, [False])

    def test_parallel_final_incumbent_cannot_erase_discovered_cuts(self):
        result = self.run_rounds([dict(candidates=[[0, 2]], final=[0, 1, 2])])
        self.assertEqual(result.result[:2], ([[0, 1, 2]], 30))
        self.assertIn("cuts_added=1", result.output)
        self.assertEqual(result.proof, [False])

    def test_skip_preserves_valid_incumbent_and_does_not_publish_disconnected_candidate(self):
        result = self.run_rounds([dict(candidates=[[0, 1], [0, 2]], cancel_at=1)])
        self.assertEqual(result.result, ([[0, 1]], 20, "SKIPPED"))
        self.assertEqual(result.reports, [([0, 1], 20)])
        self.assertIn("cuts_added=0", result.output)

    def test_stall_and_round_limits_still_apply_after_early_cut_discovery(self):
        rounds = [dict(candidates=[[0, i]], bound=200) for i in range(2, 14)]
        stalled = self.run_rounds(rounds, n=15, stall=10)
        self.assertEqual(len(stalled.created), 11)  # Initial bound + ten stalls.
        self.assertTrue(all(instance.stop_calls == 1 for instance in stalled.created))
        self.assertIn("stop_reason=UPPER_BOUND_STALL", stalled.output)
        capped = self.run_rounds(rounds, n=15, stall=10, max_rounds=3)
        self.assertEqual(len(capped.created), 3)
        self.assertIn("stop_reason=ROUND_LIMIT", capped.output)

    def test_opt_in_does_not_change_other_callers_solver_invocation(self):
        result = self.run_rounds([dict(candidates=[[0, 2]])], enabled=False)
        self.assertFalse(result.created[0].had_callback)
        self.assertEqual(result.created[0].stop_calls, 0)
        self.assertIn("cuts_added=1", result.output)

    def test_real_callback_generates_cut_and_then_proves_connected_optimum(self):
        model = solver.cp_model.CpModel()
        row = [model.NewBoolVar(f"x_{i}") for i in range(3)]
        roots = [model.NewConstant(int(i == 0)) for i in range(3)]
        model.Add(row[0] == 1)
        model.Add(row[1] == 0)
        objective = 10 * row[0] + 20 * row[2]
        model.Maximize(objective)
        proof, bounds, output = [], [], io.StringIO()
        configure = solver._configure_asu_solver_portfolio

        def quiet(parameters, workers):
            configure(parameters, workers)
            parameters.log_to_stdout = False

        with contextlib.redirect_stdout(output), \
                patch.object(solver, "_configure_asu_solver_portfolio", side_effect=quiet):
            groups, value, status = solver._joint_connectivity_cut_pass(
                model, [row], [roots], [[1], [0, 2], [1]], np.array([10, 0, 20]), [[0]],
                lambda groups: groups == [[0]], math.inf, 1, lambda: None,
                log=True, objective=objective, bound_stall_only=True,
                stop_on_new_cuts=True, round_seconds=None, max_rounds=25,
                upper_bound_stall_rounds=5, proof_out=proof, bound_out=bounds)
        self.assertEqual((groups, value, status), ([[0]], 10, "OPTIMAL"))
        self.assertEqual(proof, [True])
        self.assertEqual(bounds, [10])
        self.assertIn("round_end=NEW_CONNECTIVITY_CUTS", output.getvalue())


if __name__ == "__main__":
    unittest.main()
