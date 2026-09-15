"""Statewide strategy: actual joint models, fallbacks, and dashboard plumbing."""
import contextlib
import io
import itertools
import json
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class StatewideJointTest(unittest.TestCase):
    def case(self, slots=2, **kwargs):
        # High-rate islands alone fail population, so there are no valid seeds.
        nb = [[1], [0], [3], [2]]
        u, emp, pop = np.array([10, 1, 10, 1]), np.array([0, 8, 0, 8]), np.array([6000]*4)
        return solver._solve_statewide_joint(
            nb, u, emp, pop, .2, 10000, slots, 5, 2, seed_seconds=0,
            merge_adjacent=False, **kwargs)

    def test_free_slots_can_form_new_asus_without_valid_seeds(self):
        with patch.object(solver, "_solve_regional_exchange",
                          wraps=solver._solve_regional_exchange) as joint:
            groups, status = self.case()
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sorted(groups), [[0, 1], [2, 3]])
        joint.assert_called_once()
        self.assertEqual(joint.call_args.args[:2], ([[], []], [0, 1, 2, 3]))
        self.assertIsNone(joint.call_args.kwargs["max_groups"])
        self.assertTrue(joint.call_args.kwargs["allow_unseeded_groups"])

    def test_max_asus_limits_groups_not_the_geographic_window(self):
        for slots, expected in ((1, 1), (2, 2), (3, 2)):
            with self.subTest(slots=slots):
                groups, status = self.case(slots)
                self.assertEqual(status, "OPTIMAL")
                self.assertEqual(len(groups), expected)

    def test_more_than_three_asus_are_solved_in_one_model(self):
        nb = [[i ^ 1] for i in range(8)]
        u = np.array([10, 1]*4)
        emp, pop = np.array([0, 8]*4), np.array([6000]*8)
        with patch.object(solver, "_configure_asu_solver_portfolio",
                          wraps=solver._configure_asu_solver_portfolio) as configure:
            groups, status = solver._solve_statewide_joint(
                nb, u, emp, pop, .2, 10000, 4, 5, 2, seed_seconds=0,
                merge_adjacent=False)
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sorted(groups), [[0, 1], [2, 3], [4, 5], [6, 7]])
        configure.assert_called_once()
        self.assertEqual(configure.call_args.args[1], 2)

    def test_free_slot_symmetry_preserves_exhaustive_optimum(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        u, emp, pop = np.array([10, 2, 3, 8]), np.array([0, 40, 3, 0]), np.array([6000]*4)
        for seeds, options in (([[], [], []], {}), ([[0, 1], [], []], {}),
                               ([[], [], []], {"exact_nodes": 2}),
                               ([[], [], []], {"max_nodes": 2})):
            with self.subTest(seeds=seeds, options=options):
                valid = lambda group: solver.component_ok(group, u, emp, pop, .2, 10000, nb, **options)
                mandatory = [valid(seed) for seed in seeds]
                best = 0
                for labels in itertools.product(range(4), repeat=4):
                    groups = [[i for i, label in enumerate(labels) if label == k+1] for k in range(3)]
                    if all((not group and not required) or (valid(group) and
                           (not seed or bool(set(seed) & set(group))))
                           for group, required, seed in zip(groups, mandatory, seeds)):
                        best = max(best, sum(int(u[group].sum()) for group in groups))
                groups, status = solver._solve_regional_exchange(
                    seeds, list(range(4)), nb, u, emp, pop, .2, 10000, 5, 2,
                    allow_inactive_seeds=True, allow_unseeded_groups=True,
                    max_groups=None, **options)
                self.assertEqual(status, "OPTIMAL")
                self.assertEqual(sum(int(u[group].sum()) for group in groups), best)

    def test_unknown_and_zero_budget_preserve_valid_baseline(self):
        for seconds in (0, 5):
            with self.subTest(seconds=seconds), patch.object(
                solver.cp_model.CpSolver, "Solve", return_value=solver.cp_model.UNKNOWN):
                groups, status = solver._solve_statewide_joint(
                    [[], []], np.array([10, 20]), np.array([0, 0]), np.array([10000]*2),
                    .2, 10000, 2, seconds, 2, seed_seconds=0)
            self.assertEqual(sorted(groups), [[0], [1]])
            self.assertEqual(status, "DISABLED" if seconds == 0 else "UNKNOWN")

    def test_stop_or_skip_during_seed_stage_prevents_joint_solve(self):
        for status in ("STOPPED", "SKIPPED"):
            with (
                self.subTest(status=status),
                patch.object(solver, "solve_connectivity_free_relaxation",
                             return_value=solver.ConnectivityFreeResult([], None, None, status, .1, [])),
                patch.object(solver, "_solve_regional_exchange") as joint,
            ):
                groups, result_status = solver._solve_statewide_joint(
                    [[], []], np.array([10, 20]), np.array([0, 0]), np.array([10000]*2),
                    .2, 10000, 2, 5, 2, seed_seconds=1)
                self.assertEqual(result_status, status)
                self.assertEqual(sorted(groups), [[0], [1]])
                joint.assert_not_called()

    def test_relaxation_honors_preexisting_stop_and_skip(self):
        with TemporaryDirectory() as folder:
            for name, status in (("stop_path", "STOPPED"), ("skip_path", "SKIPPED")):
                flag = Path(folder) / name
                flag.touch()
                with patch.object(solver.cp_model, "CpSolver") as construct:
                    result = solver.solve_connectivity_free_relaxation(
                        np.array([10]), np.array([0]), np.array([10000]), .2, 10000, 0,
                        **{name: str(flag)})
                self.assertEqual(result.status, status)
                construct.assert_not_called()
                self.assertEqual(flag.exists(), name == "stop_path")

    def test_seed_relaxation_uses_its_own_budget_and_no_bound_transfer(self):
        with (
            patch.object(solver, "solve_connectivity_free_relaxation",
                         return_value=solver.ConnectivityFreeResult([0, 1], 30, 30, "OPTIMAL", .1, [])) as relax,
            patch.object(solver, "_solve_regional_exchange", return_value=([[0], [1]], "FEASIBLE")) as joint,
        ):
            groups, _ = solver._solve_statewide_joint(
                [[], []], np.array([10, 20]), np.array([0, 0]), np.array([10000]*2),
                .2, 10000, 2, 7, 4, seed_seconds=2, merge_adjacent=False)
        self.assertEqual(relax.call_args.kwargs["time_limit"], 2)
        self.assertEqual(joint.call_args.args[8:10], (7, 4))
        self.assertNotIn("objective_upper_bound", joint.call_args.kwargs)
        self.assertEqual(len(groups), 2)

    def test_invalid_joint_output_is_replaced_with_valid_seeds(self):
        with patch.object(solver, "_solve_regional_exchange", return_value=([[0], [0]], "FEASIBLE")):
            groups, status = solver._solve_statewide_joint(
                [[], []], np.array([10, 20]), np.array([0, 0]), np.array([10000]*2),
                .2, 10000, 2, 5, 2, seed_seconds=0)
        self.assertEqual(status, "INVALID_RESULT")
        self.assertEqual(sorted(groups), [[0], [1]])

    def test_strategy_returns_without_running_partition_or_polish_and_writes_map(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10, 1, 10, 1],
                              "tract_ASU_emp": [0, 8, 0, 8], "tract_pop2024": [6000]*4})
        real_joint = solver._solve_regional_exchange
        previews = []
        log = io.StringIO()
        with TemporaryDirectory() as folder:
            progress = Path(folder) / "progress.json"

            def joint(*args, **kwargs):
                original_report = kwargs["incumbent_report_callback"]
                def report(selected, objective):
                    original_report(selected, objective)
                    previews.append(json.loads(progress.read_text()))
                kwargs["log"] = False
                kwargs["incumbent_report_callback"] = report
                return real_joint(*args, **kwargs)

            with (
                patch.object(solver, "_solve_regional_exchange", side_effect=joint),
                patch.object(solver, "_prepare_window_hint", side_effect=AssertionError("partition ran")),
                patch.object(solver, "solve_one_asu_cpsat", side_effect=AssertionError("single solve ran")),
                contextlib.redirect_stdout(log),
            ):
                result = solver.build_many_asus_cpsat(
                    frame, [[1], [0], [3], [2]], .2, 10000, max_asus=2,
                    statewide_joint=True, statewide_joint_time_limit=5,
                    statewide_seed_time_limit=0, workers=2, merge_adjacent=False,
                    progress_out_path=str(progress), verbose=True)
            payload = json.loads(progress.read_text())
        self.assertEqual(result["asu_id"], [1, 1, 2, 2])
        self.assertEqual(result["joint_status"], "OPTIMAL")
        self.assertEqual(payload["phase"], "DONE")
        self.assertEqual(payload["asu_id"], result["asu_id"])
        self.assertTrue(previews)
        self.assertIn("STATEWIDE_JOINT tracts=4 edges=2 slots=2", log.getvalue())
        self.assertIn("assignment_vars=8 flow_vars=4", log.getvalue())
        self.assertIn("STATEWIDE_JOINT_COMPLETE", log.getvalue())
        self.assertNotIn("PARTITION_EXPANSION", log.getvalue())

    def test_empty_input_and_zero_slots_do_not_construct_a_model(self):
        with patch.object(solver.cp_model, "CpSolver") as construct:
            groups, status = solver._solve_statewide_joint(
                [], np.array([]), np.array([]), np.array([]), .2, 10000, 30, 5, 2)
        self.assertEqual((groups, status), ([], "EMPTY"))
        construct.assert_not_called()

    def test_real_seed_search_and_joint_solve_both_use_custom_portfolio(self):
        with patch.object(solver, "_configure_asu_solver_portfolio",
                          wraps=solver._configure_asu_solver_portfolio) as configure:
            groups, status = solver._solve_statewide_joint(
                [[1], [0], [3], [2]], np.array([10, 1, 10, 1]),
                np.array([0, 8, 0, 8]), np.array([6000]*4),
                .2, 10000, 2, 5, 2, seed_seconds=2, merge_adjacent=False)
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sorted(groups), [[0, 1], [2, 3]])
        self.assertEqual(configure.call_count, 2)
        self.assertTrue(all(call.args[1] == 2 for call in configure.call_args_list))

    def test_final_merges_wait_for_returned_assignment(self):
        events = []
        real_merge = solver._merge_touching_asu_units

        def solve(*args, **kwargs):
            kwargs["incumbent_report_callback"]([0, 1, 2, 3], 40)
            events.append("returned")
            return [[0, 1], [2, 3]], "FEASIBLE"

        def merge(*args, **kwargs):
            events.append("merge")
            return real_merge(*args, **kwargs)

        with (
            patch.object(solver, "_solve_regional_exchange", side_effect=solve),
            patch.object(solver, "_merge_touching_asu_units", side_effect=merge),
        ):
            groups, _ = solver._solve_statewide_joint(
                [[1], [0, 2], [1, 3], [2]], np.array([10]*4), np.array([0]*4),
                np.array([6000]*4), .2, 10000, 2, 5, 2, seed_seconds=0,
                incumbent_report_callback=lambda *args: events.append("preview"))
        self.assertEqual(events, ["preview", "returned", "merge"])
        self.assertEqual(groups, [[0, 1, 2, 3]])

    def test_post_solve_merges_respect_exact_tract_count(self):
        groups, status = solver._solve_statewide_joint(
            [[1], [0, 2], [1, 3], [2]], np.array([10]*4), np.array([0]*4),
            np.array([6000]*4), .2, 10000, 2, 5, 2, seed_seconds=0, exact_nodes=2)
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sorted(groups), [[0, 1], [2, 3]])


if __name__ == "__main__":
    unittest.main()
