"""Validated saved assignments seed all CP-SAT strategies without row guesses."""
import contextlib
import io
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


class InitialAsuWarmStartTest(unittest.TestCase):
    def validate(self, ids, **kwargs):
        return solver._validate_initial_asu_id(
            ids, [[1], [0], []], np.array([10, 10, 10]), np.array([0, 0, 0]),
            np.array([10000]*3), .2, 10000, **kwargs)

    def test_compacts_ids_and_preserves_unassigned(self):
        self.assertEqual(self.validate([9, 9, 20], max_asus=2).tolist(), [1, 1, 2])
        self.assertEqual(self.validate([9, 0, -1], max_asus=2).tolist(), [1, -1, -1])

    def test_rejects_malformed_or_disconnected_assignments_and_insufficient_slots(self):
        for ids in ([1], [1, 0, float("nan")], [1.5, 0, 0], [1, -2, 0], [[1, 0, 0]]):
            with self.subTest(ids=ids), self.assertRaises(ValueError):
                self.validate(ids, max_asus=3)
        with self.assertRaisesRegex(ValueError, "connectivity"):
            self.validate([1, 0, 1], max_asus=3)
        with self.assertRaisesRegex(ValueError, "Max ASUs"):
            self.validate([1, 1, 2], max_asus=1)
        with self.assertRaisesRegex(ValueError, "tract-count"):
            self.validate([1, 1, 0], max_asus=2, max_nodes=1)

    def test_statewide_upload_uses_flow_hints_and_skips_automatic_seed_search(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10, 1, 10], "tract_ASU_emp": [0, 0, 0],
                              "tract_pop2024": [10000]*3})
        real_solve = solver.cp_model.CpSolver.Solve
        models = []

        def solve(instance, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(instance, model, *args, **kwargs)

        with (
            patch.object(solver, "solve_connectivity_free_relaxation", side_effect=AssertionError("seed search ran")),
            patch.object(solver.cp_model.CpSolver, "Solve", new=solve),
        ):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0], []], .2, 10000, max_asus=2,
                initial_asu_id=[4, 4, 9], statewide_joint=True,
                statewide_joint_time_limit=5, statewide_relaxed_hint=True,
                merge_adjacent=False, workers=2, verbose=False)
        self.assertEqual(result["asu_id"], [1, 1, 2])
        # The first solve proves the primary unemployment objective; a bounded
        # second solve may improve surplus without changing that objective.
        self.assertGreaterEqual(len(models), 1)
        proto = models[0].Proto()
        hints = {proto.variables[i].name: value for i, value in
                 zip(proto.solution_hint.vars, proto.solution_hint.values)}
        self.assertEqual(hints["regional_0_0"], 1)
        self.assertIn("regional_flow_0_0_1", hints)
        self.assertFalse(any(name.startswith("joint_selected_") for name in hints))

    def test_zero_budget_returns_and_publishes_uploaded_solution(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10, 10], "tract_ASU_emp": [0, 0],
                              "tract_pop2024": [10000, 10000]})
        with TemporaryDirectory() as folder:
            path = Path(folder) / "progress.json"
            result = solver.build_many_asus_cpsat(
                frame, [[], []], .2, 10000, max_asus=2, initial_asu_id=[4, 8],
                statewide_joint=True, statewide_joint_time_limit=0, verbose=False,
                progress_out_path=str(path))
            progress = json.loads(path.read_text())
        self.assertEqual(result["asu_id"], [1, 2])
        self.assertEqual(result["joint_status"], "DISABLED")
        self.assertEqual(progress["asu_id"], [1, 2])
        self.assertEqual(progress["total_unemp"], 20)

    def test_legacy_and_partition_resume_without_recreating_saved_asus(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10, 10], "tract_ASU_emp": [0, 0],
                              "tract_pop2024": [10000, 10000]})
        for partition in (False, True):
            with (
                self.subTest(partition=partition),
                patch.object(solver, "_prepare_window_hint", side_effect=AssertionError("recreated ASU")),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                result = solver.build_many_asus_cpsat(
                    frame, [[], []], .2, 10000, max_asus=2, initial_asu_id=[4, 8],
                    harvest_connectivity_free_asus=partition, final_asu_polish_time_limit=0,
                    time_limit=0, merge_adjacent=False, verbose=False)
            self.assertEqual(result["asu_id"], [1, 2])

    def test_statewide_can_improve_imported_groups_and_form_new_groups(self):
        frame = pd.DataFrame({"tract_ASU_unemp": [10, 5, 20], "tract_ASU_emp": [0, 0, 0],
                              "tract_pop2024": [10000]*3})
        result = solver.build_many_asus_cpsat(
            frame, [[1], [0], []], .2, 10000, max_asus=2, initial_asu_id=[4, 0, 0],
            statewide_joint=True, statewide_joint_time_limit=5, merge_adjacent=False,
            workers=2, verbose=False)
        self.assertEqual(result["asu_id"], [1, 1, 2])


if __name__ == "__main__":
    unittest.main()
