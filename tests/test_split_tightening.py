"""Upfront split separators and economic caps preserve every valid split."""
import itertools
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


def path_graph(n):
    return [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)]


class SplitTighteningTest(unittest.TestCase):
    def capture(self, nb, u, emp, pop, parent, tau=.2, threshold=10000):
        captured = {}

        def cut(model, x, *args, **kwargs):
            captured.update(model=model.Clone(), indices=[[v.Index() for v in row] for row in x],
                            options=kwargs)
            return [], 0, "INFEASIBLE"  # Inspection only; skip the exact flow solve.

        with patch.object(solver, "_joint_connectivity_cut_pass", side_effect=cut):
            _, status = solver._solve_asu_split(
                parent, list(range(len(nb))), nb, np.array(u), np.array(emp), np.array(pop),
                tau, threshold, 5, 1, 3)
        return captured, status

    def check_fixed_groups(self, captured, groups, parent):
        model = captured["model"].Clone()
        model.ClearHints()
        model.ClearObjective()
        groups = sorted((set(g) for g in groups if g), key=lambda g: min(g.intersection(parent)))
        self.assertLessEqual(len(groups), len(captured["indices"]))
        for k, row in enumerate(captured["indices"]):
            group = groups[k] if k < len(groups) else set()
            for i, index in enumerate(row):
                model.Add(model.GetIntVarFromProtoIndex(index) == int(i in group))
        check = solver.cp_model.CpSolver()
        check.parameters.num_search_workers = 1
        check.parameters.max_time_in_seconds = 5
        return check.Solve(model)

    def test_upfront_separators_remove_disconnected_assignment(self):
        nb = path_graph(7)
        u = [20, 1, 1, 10, 1, 1, 20]
        parent = [1, 2, 3, 4, 5]
        disconnected = [[0, 1, 5, 6], [3]]
        with patch.object(solver, "_joint_small_separator_cuts", return_value=[]):
            loose, _ = self.capture(nb, u, [0] * 7, [10000] * 7, parent)
        tight, _ = self.capture(nb, u, [0] * 7, [10000] * 7, parent)
        self.assertEqual(self.check_fixed_groups(loose, disconnected, parent), solver.cp_model.OPTIMAL)
        self.assertEqual(self.check_fixed_groups(tight, disconnected, parent), solver.cp_model.INFEASIBLE)
        self.assertEqual(tight["options"]["max_rounds"], 100)
        self.assertEqual(tight["options"]["upper_bound_stall_rounds"], 25)
        self.assertEqual(tight["options"]["round_seconds"], 5.0)
        self.assertFalse(any(v.name.startswith("split_flow_") for v in tight["model"].Proto().variables))

    def test_unassigned_bypass_is_not_cut_by_parent_articulation(self):
        nb = path_graph(7)
        nb[0].append(6)
        nb[6].append(0)
        parent = [1, 2, 3, 4, 5]
        groups = [[0, 1, 5, 6], [3]]
        u = np.array([20, 1, 1, 10, 1, 1, 20])
        self.assertTrue(solver._valid_asu_split(
            groups, parent, range(7), nb, u, np.zeros(7, dtype=int), np.full(7, 10000), .2, 10000))
        real_separator = solver._joint_small_separator_cuts
        with patch.object(solver, "_joint_small_separator_cuts", wraps=real_separator) as separators:
            captured, _ = self.capture(nb, u, [0] * 7, [10000] * 7, parent)
        self.assertIn(6, separators.call_args.args[0][0])
        self.assertEqual(self.check_fixed_groups(captured, groups, parent), solver.cp_model.OPTIMAL)

    def test_population_reservation_reduces_slots_and_child_capacity(self):
        captured, _ = self.capture(path_graph(5), [20, 10, 1, 10, 20], [0] * 5,
                                   [6000, 6000, 1000, 6000, 6000], [1, 2, 3])
        self.assertEqual(len(captured["indices"]), 2)
        variables = {v.name: v for v in captured["model"].Proto().variables}
        self.assertEqual(list(variables["split_count_0"].domain), [0, 3])
        self.assertEqual(self.check_fixed_groups(captured, [[0, 1], [3, 4]], [1, 2, 3]),
                         solver.cp_model.OPTIMAL)

    def test_rate_bounds_exclude_unaffordable_high_unemployment_tract(self):
        captured, _ = self.capture(path_graph(5), [20, 10, 1, 10, 20],
                                   [1000, 0, 0, 0, 0], [10000] * 5, [1, 2, 3])
        model = captured["model"].Clone()
        variables = {v.name: (i, v) for i, v in enumerate(model.Proto().variables)}
        # The fractional rate relaxation has ceiling 44; the integer optimum
        # may be lower. Never replace its bound with an incumbent objective.
        self.assertEqual(list(variables["split_unemployment"][1].domain), [22, 44])
        self.assertEqual(list(variables["split_count_0"][1].domain), [0, 3])
        model.Add(model.GetIntVarFromProtoIndex(variables["split_selected_0"][0]) == 1)
        check = solver.cp_model.CpSolver()
        check.parameters.num_search_workers = 1
        self.assertEqual(check.Solve(model), solver.cp_model.INFEASIBLE)
        self.assertEqual(self.check_fixed_groups(captured, [[1], [3, 4]], [1, 2, 3]),
                         solver.cp_model.OPTIMAL)

    def test_economic_impossibility_skips_cut_and_flow_solves(self):
        with patch.object(solver.cp_model.CpSolver, "Solve") as solve:
            captured, status = self.capture(path_graph(5), [100, 1, 1, 1, 1],
                                            [0, 0, 10000, 10000, 10000], [10000] * 5, [0, 1])
        self.assertEqual(captured, {})
        self.assertEqual(status, "INFEASIBLE")
        solve.assert_not_called()

    def test_all_feasible_small_splits_survive_with_mixed_rates_and_population(self):
        u = np.array([20, 10, 1, 1, 10, 20])
        emp = np.array([110, 2, 3, 8, 4, 90])
        pop = np.array([8, 5, 3, 7, 5, 9])
        parent, checked = [1, 2, 3, 4], 0
        for edges, tau, threshold in (([], .2, 8), ([(0, 5)], .2, 8),
                                       ([], 0, 0), ([], .2, 14)):
            nb = path_graph(6)
            for a, b in edges:
                nb[a].append(b)
                nb[b].append(a)
            captured, _ = self.capture(nb, u, emp, pop, parent, tau, threshold)
            unique, optimum = set(), 0
            for assignment in itertools.product(range(4), repeat=6):
                groups = [[i for i, owner in enumerate(assignment) if owner == k] for k in (1, 2, 3)]
                if not solver._valid_asu_split(groups, parent, range(6), nb, u, emp, pop, tau, threshold):
                    continue
                key = tuple(sorted(tuple(g) for g in groups if g))
                if key in unique:
                    continue
                unique.add(key)
                optimum = max(optimum, sum(int(u[g].sum()) for g in groups if g))
                with self.subTest(edges=edges, tau=tau, threshold=threshold, groups=key):
                    self.assertTrue(captured, "economic screen removed a valid split")
                    self.assertEqual(self.check_fixed_groups(captured, groups, parent), solver.cp_model.OPTIMAL)
                checked += 1
            # Exercise the tightened exact flows as well, not only the cut
            # relaxation, and compare the optimum with exhaustive enumeration.
            with patch.object(solver, "_joint_connectivity_cut_pass", return_value=([], 0, "ROUND_LIMIT")):
                result, _ = solver._solve_asu_split(
                    parent, list(range(6)), nb, u, emp, pop, tau, threshold, 5, 1, 3)
            self.assertEqual(sum(int(u[g].sum()) for g in result), optimum)
        self.assertGreater(checked, 10)

    def test_stop_during_upfront_separator_generation(self):
        stopped = [False]

        def separators(*args, **kwargs):
            stopped[0] = True
            return []

        with patch.object(solver, "_joint_small_separator_cuts", side_effect=separators), \
                patch.object(solver, "_joint_connectivity_cut_pass") as cut:
            groups, status = solver._solve_asu_split(
                [1, 2, 3], list(range(5)), path_graph(5), np.array([20, 10, 1, 10, 20]),
                np.zeros(5, dtype=int), np.full(5, 10000), .2, 10000, 5, 1, 3,
                cancellation=lambda: "STOPPED" if stopped[0] else None)
        self.assertEqual((groups, status), ([], "STOPPED"))
        cut.assert_not_called()


if __name__ == "__main__":
    unittest.main()
