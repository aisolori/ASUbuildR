"""Saved-parent splitting: strict gain, no contacts, bottleneck hints and cuts."""
import ast
import itertools
import json
import math
import re
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


def path_graph(n):
    return [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)]


class AsuSplitTest(unittest.TestCase):
    def arrays(self, u):
        return np.array(u), np.zeros(len(u), dtype=int), np.full(len(u), 10000)

    def solve(self, nb, u, parent, **options):
        unemp, emp, pop = self.arrays(u)
        return solver._solve_asu_split(parent, list(range(len(nb))), nb, unemp, emp,
                                      pop, .2, 10000, 5, 1, 2, **options)

    def test_gain_releases_corridor_and_captures_unassigned(self):
        models, limits = [], []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(instance, model, *args, **kwargs):
            models.append(model.Clone())
            limits.append(instance.parameters.max_time_in_seconds)
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, "Solve", new=capture):
            groups, status = self.solve(path_graph(5), [10, 10, 1, 10, 10], [1, 2, 3])
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual({frozenset(g) for g in groups}, {frozenset([0, 1]), frozenset([3, 4])})
        self.assertEqual(limits[0], 5.0)
        self.assertFalse(any(v.name.startswith("split_flow_") for v in models[0].Proto().variables))

    def test_no_gain_is_not_a_split(self):
        for u in ([1, 10, 100, 10, 1], [1, 10, 2, 10, 1]):
            with self.subTest(u=u):
                groups, status = self.solve(path_graph(5), u, [1, 2, 3])
                self.assertEqual(groups, [])
                self.assertEqual(status, "INFEASIBLE")

    def test_three_children_and_no_recursive_splitting(self):
        nb = [[1, 2, 3], [0, 4], [0, 5], [0, 6], [1], [2], [3]]
        u, emp, pop = self.arrays([1, 10, 10, 10, 10, 10, 10])
        ids, attempts = solver._split_warm_start_asus(
            [1, 1, 1, 1, -1, -1, -1], nb, u, emp, pop, .2, 10000, 5, 5, 1)
        self.assertEqual(len(attempts), 1)
        self.assertTrue(attempts[0]["accepted"])
        self.assertEqual(attempts[0]["gain"], 29)
        self.assertEqual(len(set(ids[ids > 0])), 3)
        self.assertEqual(ids[0], -1)
        for a, b in ((1, 4), (2, 5), (3, 6)):
            self.assertEqual(ids[a], ids[b])

    def test_three_child_cap_applies_to_cut_and_flow_models(self):
        # Four independently profitable arms would yield four children without
        # the cap. Even a direct caller requesting 20 slots gets only three.
        nb = [[1, 2, 3, 4], [0, 5], [0, 6], [0, 7], [0, 8], [1], [2], [3], [4]]
        u, emp, pop = self.arrays([1, 10, 10, 10, 10, 20, 20, 20, 20])
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def cut_pass(model, x, *args, **kwargs):
            self.assertEqual(len(x), 3)
            return [], 0, "ROUND_LIMIT"

        def capture(instance, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver, "_joint_connectivity_cut_pass", side_effect=cut_pass), \
                patch.object(solver.cp_model.CpSolver, "Solve", new=capture):
            groups, status = solver._solve_asu_split(
                [0, 1, 2, 3, 4], list(range(9)), nb, u, emp, pop, .2, 10000, 5, 1, 20)
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(len(groups), 3)
        self.assertEqual(sum(int(u[g].sum()) for g in groups), 90)
        self.assertTrue(any(v.name.startswith("split_flow_") for v in models[0].Proto().variables))
        self.assertFalse(any(v.name.startswith("split_flow_3_") for v in models[0].Proto().variables))

    def test_parent_driver_caps_slots_and_rejects_four_children(self):
        nb = [[1, 2, 3, 4], [0, 5], [0, 6], [0, 7], [0, 8], [1], [2], [3], [4]]
        u, emp, pop = self.arrays([1, 10, 10, 10, 10, 20, 20, 20, 20])
        original = [1, 1, 1, 1, 1, -1, -1, -1, -1]
        for max_asus, expected_slots in ((2, 2), (20, 3)):
            with self.subTest(max_asus=max_asus), patch.object(
                    solver, "_solve_asu_split",
                    return_value=([[1, 5], [2, 6], [3, 7], [4, 8]], "FEASIBLE")) as solve:
                ids, attempts = solver._split_warm_start_asus(
                    original, nb, u, emp, pop, .2, 10000, max_asus, 5, 1)
            self.assertEqual(solve.call_args.args[10], expected_slots)
            self.assertEqual(ids.tolist(), original)
            self.assertFalse(attempts[0]["accepted"])

    def test_runner_template_and_mandatory_upload_wiring(self):
        source = (Path(__file__).resolve().parents[1] / "inst" / "shiny_app" /
                  "ASU_Flexdashboard_mapgl.Rmd").read_text(encoding="utf-8")
        match = re.search(r'runner_code <- sprintf\("(.*?)",\s*(py_mod_path,.*?out_json)\s*\)',
                          source, re.S)
        self.assertIsNotNone(match)
        template, args = match.groups()
        # Count top-level R arguments, ignoring nested calls and quoted text.
        depth, quote, escaped, count = 0, None, False, 1
        for char in args:
            if quote:
                if char == quote and not escaped:
                    quote = None
                escaped = char == "\\" and not escaped
            elif char in "\"'":
                quote = char
            elif char in "([{":
                depth += 1
            elif char in ")]}":
                depth -= 1
            elif char == "," and depth == 0:
                count += 1
        self.assertEqual(count, len(re.findall(r"%[sdf]", template)))
        runner = ast.parse(template % tuple(range(1, count + 1)))
        kwargs = next(n.value for n in ast.walk(runner) if isinstance(n, ast.Assign)
                      and any(isinstance(t, ast.Name) and t.id == "kwargs" for t in n.targets))
        # Persistent runners have an additional checkpoint-directory placeholder.
        self.assertEqual(next(k.value.value for k in kwargs.keywords if k.arg == "split_warm_start"), 5)
        self.assertRegex(args, r'nb_json,\s*if \(legacy_checkpoint\)[^\n]+\s*if \(use_split\) "True" else "False"')
        self.assertIn('if (use_split || isTRUE(input$cpsat_use_warm_start))', source)
        self.assertIn("input.cpsat_use_warm_start === true || input.cpsat_strategy === 'split'", source)

    def test_articulation_and_two_tract_corridor_hints(self):
        parts, cut = solver._split_bottleneck_hint(
            path_graph(5), list(range(5)), np.array([10, 10, 1, 10, 10]), 2, lambda: None)
        self.assertEqual(cut, [2])
        self.assertEqual({frozenset(g) for g in parts}, {frozenset([0, 1]), frozenset([3, 4])})
        # A two-wide ladder has no articulation vertex, but its cheap middle
        # rung is a two-tract corridor separator.
        nb = [[] for _ in range(6)]
        for i, j in [(0, 1), (1, 2), (3, 4), (4, 5), (0, 3), (1, 4), (2, 5)]:
            nb[i].append(j)
            nb[j].append(i)
        self.assertEqual(solver._articulation_points(nb, np.ones(6, dtype=bool)), set())
        parts, cut = solver._split_bottleneck_hint(
            nb, list(range(6)), np.array([10, 1, 10, 10, 1, 10]), 2, lambda: None)
        self.assertEqual(cut, [1, 4])
        self.assertEqual(len(parts), 2)

    def test_flow_extends_cut_model_and_keeps_its_bound(self):
        calls, options, before = [], {}, []
        real_solve = solver.cp_model.CpSolver.Solve

        def cut_pass(model, *args, **kwargs):
            options.update(kwargs)
            self.assertFalse(any(v.name.startswith("split_flow_") for v in model.Proto().variables))
            model.Add(kwargs["objective"] <= 40)
            before.extend(str(c) for c in model.Proto().constraints)
            return [], 0, "ROUND_LIMIT"

        def capture(instance, model, *args, **kwargs):
            calls.append(model.Clone())
            self.assertEqual(instance.parameters.max_time_in_seconds, 5)
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver, "_joint_connectivity_cut_pass", side_effect=cut_pass), \
                patch.object(solver.cp_model.CpSolver, "Solve", new=capture):
            groups, status = self.solve(path_graph(5), [10, 10, 1, 10, 10], [1, 2, 3])
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sum(len(g) for g in groups), 4)
        self.assertEqual(options["max_rounds"], 100)
        self.assertEqual(options["upper_bound_stall_rounds"], 25)
        self.assertEqual(options["round_seconds"], 5.0)
        self.assertEqual(options["objective_floor"], 22)
        self.assertTrue(options["bound_stall_only"])
        self.assertFalse(options["stop_on_new_cuts"])
        self.assertTrue(any(v.name.startswith("split_flow_") for v in calls[0].Proto().variables))
        self.assertEqual(before, [str(c) for c in calls[0].Proto().constraints][:len(before)])

    def test_exact_model_matches_enumeration_even_with_no_hint(self):
        # Two children, including opportunities for disconnected relaxed
        # selections. Explicit enumeration also checks the acceptance gate.
        for extra_edges in ([], [(0, 4)], [(0, 5), (1, 4)]):
            nb = path_graph(6)
            for i, j in extra_edges:
                nb[i].append(j)
                nb[j].append(i)
            u, emp, pop = self.arrays([15, 10, 1, 10, 15, 9])
            parent = [1, 2, 3]
            optimum = 0
            for assignment in itertools.product(range(3), repeat=6):
                groups = [[i for i, label in enumerate(assignment) if label == k] for k in (1, 2)]
                if solver._valid_asu_split(groups, parent, range(6), nb, u, emp, pop, .2, 10000):
                    optimum = max(optimum, sum(int(u[g].sum()) for g in groups))
            with self.subTest(edges=extra_edges), \
                    patch.object(solver, "_split_bottleneck_hint", return_value=([], [])), \
                    patch.object(solver, "_joint_connectivity_cut_pass", return_value=([], 0, "ROUND_LIMIT")):
                groups, status = self.solve(nb, u, parent)
            self.assertEqual(sum(int(u[g].sum()) for g in groups), optimum)
            self.assertIn(status, ("OPTIMAL", "INFEASIBLE"))

    def test_acceptance_rejects_touching_disconnected_unanchored_and_weak_children(self):
        nb = path_graph(6)
        u, emp, pop = self.arrays([10, 10, 1, 10, 10, 100])
        for groups in ([[0, 1], [2, 3, 4]], [[0, 1, 4], [3]], [[0, 1], [5]],
                       [[0, 1], [1, 3, 4]], [[0, 1, 3, 4]], [[0, 1], [3, 4, 5]]):
            with self.subTest(groups=groups):
                self.assertFalse(solver._valid_asu_split(
                    groups, [1, 2, 3], range(5), nb, u, emp, pop, .2, 10000))
        self.assertFalse(solver._valid_asu_split(
            [[0, 1], [3, 4]], [1, 2, 3], range(5), nb, u, emp, pop, .2, 30000))
        emp[3:5] = 1000
        self.assertFalse(solver._valid_asu_split(
            [[0, 1], [3, 4]], [1, 2, 3], range(5), nb, u, emp, pop, .2, 10000))

    def test_build_preserves_other_asu_and_its_neighbor_buffer(self):
        u, emp, pop = self.arrays([10, 10, 1, 10, 10, 100, 5])
        frame = pd.DataFrame(dict(tract_ASU_unemp=u, tract_ASU_emp=emp, tract_pop2024=pop))
        with TemporaryDirectory() as directory, \
                patch.object(solver, "solve_one_asu_cpsat", side_effect=AssertionError("legacy stage")), \
                patch.object(solver, "_search_unassigned_asu", side_effect=AssertionError("residual stage")):
            progress = Path(directory) / "progress.json"
            result = solver.build_many_asus_cpsat(
                frame, path_graph(7), .2, 10000, max_asus=4,
                initial_asu_id=[0, 1, 1, 1, 0, 0, 2], split_warm_start=True,
                time_limit=5, workers=1, verbose=False, progress_out_path=str(progress))
            saved = json.loads(progress.read_text())
        ids = result["asu_id"]
        self.assertEqual(result["n_asu"], 3)
        self.assertEqual(ids[0], ids[1])
        self.assertEqual(ids[3], ids[4])
        self.assertNotEqual(ids[0], ids[3])
        self.assertGreater(ids[6], 0)
        self.assertEqual(ids[2], -1)
        self.assertEqual(ids[5], -1)
        self.assertEqual(saved["asu_id"], ids)
        self.assertEqual(saved["phase"], "DONE")
        self.assertFalse(result["residual_check"]["exhausted"])

    def test_descending_imported_parents_only_and_capacity(self):
        nb = path_graph(8)
        u, emp, pop = self.arrays([10, 1, 10, 20, 20, 10, 1, 10])
        ids = np.array([1, 1, 1, -1, -1, 2, 2, 2])
        seen = []

        def solve(parent, nodes, *args, **kwargs):
            seen.append(list(parent))
            return [], "UNKNOWN"

        u[5] = 20
        with patch.object(solver, "_solve_asu_split", side_effect=solve):
            result, attempts = solver._split_warm_start_asus(ids, nb, u, emp, pop, .2, 10000, 4, 5, 1)
        self.assertEqual(seen, [[5, 6, 7], [0, 1, 2]])
        self.assertEqual(result.tolist(), ids.tolist())
        with patch.object(solver, "_solve_asu_split") as solve_mock:
            result, attempts = solver._split_warm_start_asus(ids, nb, u, emp, pop, .2, 10000, 2, 5, 1)
        solve_mock.assert_not_called()
        self.assertEqual(attempts, [])

    def test_stop_and_skip_leave_valid_parent(self):
        for reason in ("STOPPED", "SKIPPED"):
            groups, status = self.solve(path_graph(5), [10, 10, 1, 10, 10], [1, 2, 3],
                                        cancellation=lambda: reason)
            self.assertEqual(groups, [])
            self.assertEqual(status, reason)

    def test_requires_warm_start(self):
        frame = pd.DataFrame(dict(tract_ASU_unemp=[10], tract_ASU_emp=[0], tract_pop2024=[10000]))
        for ids in (None, [0]):
            with self.subTest(ids=ids), self.assertRaisesRegex(ValueError, "nonempty.*warm start"):
                solver.build_many_asus_cpsat(frame, [[]], .2, 10000, split_warm_start=True,
                                            initial_asu_id=ids, verbose=False)


if __name__ == "__main__":
    unittest.main()
