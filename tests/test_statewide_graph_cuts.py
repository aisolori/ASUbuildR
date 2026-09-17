"""Root-safe statewide cuts, pre-pass carryover, budgets and feasible hints."""
import contextlib
import io
import itertools
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import time
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class StatewideGraphCutsTest(unittest.TestCase):
    def solve(self, nb, u, emp, pop, seeds, **options):
        models, reports = [], []
        real_solve = solver.cp_model.CpSolver.Solve
        options.setdefault("deterministic_ties", False)

        def capture(instance, model, *args, **kwargs):
            models.append(model.Clone())
            instance.parameters.log_to_stdout = False
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, "Solve", new=capture):
            groups, status = solver._solve_regional_exchange(
                seeds, list(range(len(nb))), nb, np.array(u), np.array(emp), np.array(pop),
                .2, 10000, 5, 2, allow_inactive_seeds=True, allow_unseeded_groups=True,
                max_groups=None, incumbent_report_callback=lambda x, v: reports.append((x, v)),
                **options)
        self.assertEqual(status, "OPTIMAL")
        return groups, models, reports

    def check_assignment(self, template, groups):
        model = template.Clone()
        model.ClearHints()
        assignments = {f"regional_{k}_{i}": int(i in group)
                       for k, group in enumerate(groups)
                       for i in range(sum(v.name.startswith(f"regional_root_{k}_prefix_")
                                          for v in model.Proto().variables))}
        for i, var in enumerate(model.Proto().variables):
            if var.name in assignments:
                model.Add(model.GetIntVarFromProtoIndex(i) == assignments[var.name])
        check = solver.cp_model.CpSolver()
        check.parameters.max_time_in_seconds = 2
        check.parameters.num_search_workers = 2
        solver._configure_asu_solver_portfolio(check.parameters, 2)
        return check.Solve(model)

    def test_static_separators_preserve_all_connected_subsets_with_either_root_side(self):
        graphs = [
            [[1], [0, 2, 3], [1], [1]],
            [[1, 2], [0, 3], [0, 3], [1, 2]],
            [[1], [0], [3], [2]],
        ]
        sizes = set()
        for nb in graphs:
            cuts = solver._joint_small_separator_cuts(
                nb, np.array([10, 20, 30, 40]), np.array([1]*4), [[0]],
                time.monotonic()+5, lambda: None)
            self.assertEqual(len(cuts), len(set(cuts)))
            for a, b, separator in cuts:
                self.assertNotIn(a, separator)
                self.assertNotIn(b, separator)
                sizes.add(len(separator))
                for mask in itertools.product((False, True), repeat=4):
                    components = solver._connected_components(nb, np.array(mask))
                    if len(components) == 1:
                        self.assertLessEqual(int(mask[a])+int(mask[b]),
                                             1+sum(mask[v] for v in separator))
        self.assertTrue({1, 2}.issubset(sizes))

    def test_seed_distances_use_all_original_seed_tracts(self):
        nb = [[1], [0, 2], [1, 3], [2], []]
        self.assertEqual(solver._joint_seed_distances(nb, [0, 1]), [0, 0, 1, 2, -1])
        for mask in itertools.product((False, True), repeat=5):
            selected = [i for i, yes in enumerate(mask) if yes]
            if not set(selected).intersection([0, 1]):
                continue
            if len(solver._connected_components(nb, np.array(mask))) != 1:
                continue
            for i, d in enumerate(solver._joint_seed_distances(nb, [0, 1])):
                if mask[i]:
                    self.assertGreaterEqual(d, 0)
                    self.assertGreaterEqual(len(selected), d+1)

    def test_prepass_adds_cuts_without_flows_then_retains_them_in_exact_model(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        u, emp, pop = [10, 1, 100, 1], [0, 1000, 0, 0], [10000]*4
        templates = []
        real_pass = solver._joint_connectivity_cut_pass

        def capture_pass(*args, **kwargs):
            result = real_pass(*args, **kwargs)
            templates.append(args[0].Clone())
            return result

        output = io.StringIO()
        with (patch.object(solver, "_joint_small_separator_cuts", return_value=[]),
              patch.object(solver, "_joint_connectivity_cut_pass", side_effect=capture_pass),
              contextlib.redirect_stdout(output)):
            groups, models, reports = self.solve(nb, u, emp, pop, [[]],
                                                 use_joint_cuts=True, log=True)
        self.assertEqual(groups, [[2, 3]])
        self.assertGreaterEqual(len(models), 3)
        self.assertFalse(any(v.name.startswith("regional_flow_") for v in models[0].Proto().variables))
        self.assertTrue(any(v.name.startswith("regional_flow_") for v in models[-1].Proto().variables))
        # Each no-flow round's existing rows remain as a prefix of the final model.
        final_rows = models[-1].Proto().constraints
        for i, row in enumerate(templates[0].Proto().constraints):
            self.assertEqual(str(row), str(final_rows[i]))
        self.assertGreater(len(templates[0].Proto().constraints), len(models[0].Proto().constraints))
        for selected, value in reports:
            self.assertEqual(value, 101)
            self.assertTrue(solver.component_ok(selected, np.array(u), np.array(emp),
                                                np.array(pop), .2, 10000, nb))
        # A cut generated with the root on the RIGHT must still permit a root
        # on the LEFT. This inspects the cut-only model, before its improved floor.
        for group in ([0], [2], [3], [2, 3]):
            self.assertEqual(self.check_assignment(templates[0], [group]), solver.cp_model.OPTIMAL)
        self.assertIn("connected=False", output.getvalue())
        self.assertIn("STATEWIDE_JOINT_FLOW", output.getvalue())
        self.assertIn("bound_carried_to_flow=True", output.getvalue())
        # The final model explicitly retains the pre-pass upper bound, rather
        # than forcing its fresh solver to rediscover it from the graph cuts.
        objective_coeffs = dict(zip(models[-1].Proto().objective.vars,
                                    [-c for c in models[-1].Proto().objective.coeffs]))
        upper_rows = [list(row.linear.domain)[-1] for row in models[-1].Proto().constraints
                      if dict(zip(row.linear.vars, row.linear.coeffs)) == objective_coeffs]
        self.assertIn(101, upper_rows)
        # Improved connected pre-pass incumbent supplies consistent complete hints.
        final = models[-1]
        hints = final.Proto().solution_hint
        self.assertEqual(len(hints.vars), len(set(hints.vars)))
        check = solver.cp_model.CpSolver()
        check.parameters.num_search_workers = 2
        check.parameters.max_time_in_seconds = 2
        check.parameters.fix_variables_to_their_hinted_value = True
        solver._configure_asu_solver_portfolio(check.parameters, 2)
        self.assertEqual(check.Solve(final), solver.cp_model.OPTIMAL)
        self.assertEqual(check.ObjectiveValue(), 101)

    def test_cuts_preserve_exhaustive_optimum_with_optional_seeds_and_tightening(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        u, emp, pop = np.array([10, 1, 10, 1]), np.array([0, 60, 0, 0]), np.array([10000]*4)
        for seeds, consolidate, tighten in itertools.product(
                ([[0], [2]], [[0], []], [[], []]), (False, True), (False, True)):
            baseline = sum(int(u[g].sum()) for g in seeds)
            best = baseline
            for labels in itertools.product(range(3), repeat=4):
                groups = [[i for i, label in enumerate(labels) if label == k+1] for k in range(2)]
                valid = all((not group and (not seed or consolidate)) or (
                    solver.component_ok(group, u, emp, pop, .2, 10000, nb)
                    and (not seed or bool(set(seed) & set(group))))
                    for group, seed in zip(groups, seeds))
                if valid:
                    best = max(best, sum(int(u[g].sum()) for g in groups))
            groups, _, _ = self.solve(nb, u, emp, pop, seeds, use_joint_cuts=True,
                                      allow_seed_consolidation=consolidate, tighten_model=tighten)
            self.assertEqual(sum(int(u[g].sum()) for g in groups), best,
                             msg=f"seeds={seeds}, consolidate={consolidate}, tighten={tighten}")

    def test_every_prepass_and_flow_solve_uses_portfolio_and_shared_budget(self):
        clock = solver.time.monotonic
        cut_pass = solver._joint_connectivity_cut_pass
        elapsed = [0.0]

        def timed_pass(*args, **kwargs):
            result = cut_pass(*args, **kwargs)
            # Tiny models may finish within one Windows clock tick. Simulate
            # measurable pre-pass work so budget subtraction is deterministic.
            elapsed[0] += .25
            return result

        with (patch.object(solver.time, "monotonic", side_effect=lambda: clock() + elapsed[0]),
              patch.object(solver, "_joint_connectivity_cut_pass", side_effect=timed_pass),
              patch.object(solver, "_configure_asu_solver_portfolio",
                           wraps=solver._configure_asu_solver_portfolio) as configure):
            _, models, _ = self.solve([[1], [0]], [10, 1], [0, 0], [10000]*2,
                                      [[0], []], use_joint_cuts=True,
                                      relaxed_selection_hint=[0, 1])
        self.assertEqual(configure.call_count, len(models))
        self.assertTrue(all(call.args[1] == 2 for call in configure.call_args_list))
        limits = [call.args[0].max_time_in_seconds for call in configure.call_args_list]
        self.assertGreater(len(limits), 1)
        self.assertTrue(all(0 < limit <= .75 for limit in limits[:-1]))
        self.assertTrue(0 < limits[-1] <= 4.75)

    def test_connected_prepass_improvement_survives_unknown_flow_solve(self):
        real_solve = solver.cp_model.CpSolver.Solve
        def unknown_flow(instance, model, *args, **kwargs):
            if any(v.name.startswith("regional_flow_") for v in model.Proto().variables):
                return solver.cp_model.UNKNOWN
            return real_solve(instance, model, *args, **kwargs)
        with patch.object(solver.cp_model.CpSolver, "Solve", new=unknown_flow):
            groups, status = solver._solve_statewide_joint(
                [[1], [0]], np.array([10, 5]), np.array([0, 0]), np.array([10000]*2),
                .2, 10000, 1, 5, 2, initial_units=[[0]], use_graph_cuts=True)
        self.assertEqual((groups, status), ([[0, 1]], "UNKNOWN"))

    def test_stop_after_prepass_solve_does_not_launch_flow_solve(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / "stop"
            def stop(instance, model, *args, **kwargs):
                flag.touch()
                return solver.cp_model.UNKNOWN
            with patch.object(solver.cp_model.CpSolver, "Solve", new=stop):
                groups, status = solver._solve_statewide_joint(
                    [[1], [0]], np.array([10, 10]), np.array([0, 0]), np.array([10000]*2),
                    .2, 10000, 2, 5, 2, initial_units=[[0], [1]],
                    use_graph_cuts=True, stop_path=str(flag))
            self.assertEqual((groups, status), ([[0], [1]], "STOPPED"))
            self.assertTrue(flag.exists())

    def test_skip_during_prepass_preserves_fallback_and_skips_flow(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / "skip"
            def skip(instance, model, *args, **kwargs):
                flag.touch()
                return solver.cp_model.UNKNOWN
            with patch.object(solver.cp_model.CpSolver, "Solve", new=skip):
                groups, status = solver._solve_statewide_joint(
                    [[], []], np.array([10, 10]), np.array([0, 0]), np.array([10000]*2),
                    .2, 10000, 2, 5, 2, initial_units=[[0], [1]],
                    use_graph_cuts=True, skip_path=str(flag))
            self.assertEqual(groups, [[0], [1]])
            self.assertEqual(status, "SKIPPED")
            self.assertFalse(flag.exists())

    def test_zero_budget_does_not_solve_and_builder_forwards_toggle(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10], "tract_ASU_emp": [0],
                              "tract_pop2024": [10000]})
        with (patch.object(solver, "_solve_statewide_joint", wraps=solver._solve_statewide_joint) as statewide,
              patch.object(solver.cp_model.CpSolver, "Solve") as solve):
            result = solver.build_many_asus_cpsat(
                frame, [[]], .2, 10000, max_asus=1, initial_asu_id=[1], statewide_joint=True,
                statewide_joint_time_limit=0, statewide_graph_cuts=True, verbose=False)
        self.assertTrue(statewide.call_args.kwargs["use_graph_cuts"])
        solve.assert_not_called()
        self.assertEqual(result["asu_id"], [1])

    def test_toggle_off_runs_only_exact_flow_model(self):
        with patch.object(solver, "_joint_connectivity_cut_pass", side_effect=AssertionError("cut pass ran")):
            _, models, _ = self.solve([[1], [0]], [10, 1], [0, 0], [10000]*2,
                                      [[0], []], use_joint_cuts=False)
        self.assertEqual(len(models), 1)

    def test_prepass_uses_reported_upper_bound_not_incumbent_and_rounds_up(self):
        for reported, expected in ((35.25, 36), (35.0, 35), (29.0, None),
                                   (float("nan"), None), (float("inf"), None),
                                   (float(2**53), None)):
            with self.subTest(reported=reported):
                model = solver.cp_model.CpModel()
                row = [model.NewBoolVar("a"), model.NewBoolVar("b")]
                model.Add(row[0] == 1)
                objective = 10 * row[0] + 20 * row[1]
                model.Maximize(objective)
                with patch.object(solver.cp_model.CpSolver, "BestObjectiveBound", return_value=reported):
                    _, best, _ = solver._joint_connectivity_cut_pass(
                        model, [row], [row], [[1], [0]], np.array([10, 20]),
                        [[0]], lambda groups: True, time.monotonic()+2, 2,
                        lambda: None, objective=objective)
                self.assertEqual(best, 30)
                self.assertEqual(len(model.Proto().constraints), 1 if expected is None else 2)
                if expected is not None:
                    constraints = model.Proto().constraints
                    self.assertEqual(list(constraints[len(constraints)-1].linear.domain)[-1], expected)

    def test_unknown_prepass_does_not_import_default_zero_bound(self):
        model = solver.cp_model.CpModel()
        row = [model.NewBoolVar("a")]
        model.Maximize(10 * row[0])
        with (patch.object(solver.cp_model.CpSolver, "Solve", return_value=solver.cp_model.UNKNOWN),
              patch.object(solver.cp_model.CpSolver, "BestObjectiveBound", return_value=0) as bound):
            groups, best, status = solver._joint_connectivity_cut_pass(
                model, [row], [row], [[]], np.array([10]), [[0]], lambda groups: True,
                time.monotonic()+2, 2, lambda: None, objective=10 * row[0])
        bound.assert_not_called()
        self.assertEqual((groups, best, status), ([[0]], 10, "UNKNOWN"))
        self.assertEqual(len(model.Proto().constraints), 0)

    def test_cut_pass_stops_after_ten_rounds_without_upper_bound_improvement(self):
        self.check_bound_stall([100] * 11, 11)

    def test_cut_pass_resets_stall_when_upper_bound_improves(self):
        self.check_bound_stall([100] * 10 + [90] * 11, 21)

    def test_cut_pass_continues_while_bound_improves_despite_valid_unemp_stall(self):
        self.check_bound_stall(list(range(100, 85, -1)) + [86] * 10, 25)

    def test_cut_pass_counts_unusable_bounds_as_no_improvement(self):
        self.check_bound_stall([float('nan')] * 10, 10)

    def check_bound_stall(self, bounds, expected_rounds):
        n = len(bounds) + 2
        model = solver.cp_model.CpModel()
        row = [model.NewBoolVar(f"x_{i}") for i in range(n)]
        roots = [model.NewBoolVar(f"root_{i}") for i in range(n)]
        selections = [{0, i} for i in range(2, n)]
        created = []

        class FakeSolver:
            def __init__(self):
                self.parameters = type("Parameters", (), {})()
                self.selection = selections[len(created)]
                self.bound = bounds[len(created)]
                created.append(self)

            def Solve(self, unused_model):
                return solver.cp_model.FEASIBLE

            def StatusName(self, unused_status):
                return "FEASIBLE"

            def BooleanValue(self, var):
                prefix, index = var.name.split("_")
                return int(index) in (self.selection if prefix == "x" else {0})

            def BestObjectiveBound(self):
                return self.bound

        output = io.StringIO()
        with (patch.object(solver.cp_model, "CpSolver", FakeSolver),
              patch.object(solver, "_configure_asu_solver_portfolio"),
              contextlib.redirect_stdout(output)):
            groups, best, status = solver._joint_connectivity_cut_pass(
                model, [row], [roots],
                [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)],
                np.ones(n, dtype=int), [[0]], lambda groups: False,
                time.monotonic()+20, 2, lambda: None, log=True, objective=sum(row))

        self.assertEqual((groups, best, status), ([[0]], 1, "FEASIBLE"))
        self.assertEqual(len(created), expected_rounds)
        self.assertIn("upper_bound_stall=10/10", output.getvalue())
        self.assertIn("stop_reason=UPPER_BOUND_STALL", output.getvalue())


if __name__ == "__main__":
    unittest.main()
