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
    def test_legacy_skips_takeover_and_still_checks_residuals(self):
        frame = pd.DataFrame({
            "tract_ASU_unemp": [10, 0, 20],
            "tract_ASU_emp": [0, 500, 0],
            "tract_pop2024": [10000, 10000, 10000],
        })
        for partition in (False, True):
            for merge in (False, True):
                calls, output = [], io.StringIO()

                def solve(**options):
                    calls.append(options)
                    return None  # Retain the existing valid ASUs.

                with (
                    self.subTest(partition=partition, merge=merge),
                    patch.object(solver, "solve_one_asu_cpsat", side_effect=solve),
                    patch.object(solver, "_solve_supernode_polish", side_effect=solve),
                    patch.object(solver, "_search_unassigned_asu",
                                 return_value=([], "INFEASIBLE")) as residual,
                    patch.object(solver, "_repair_takeover_donor",
                                 side_effect=AssertionError("unexpected donor repair")),
                    contextlib.redirect_stdout(output),
                ):
                    result = solver.build_many_asus_cpsat(
                        frame, [[1], [0, 2], [1]], .2, 10000,
                        max_asus=2, initial_asu_id=[1, -1, 2],
                        harvest_connectivity_free_asus=partition,
                        final_asu_polish_time_limit=1, workers=1,
                        merge_adjacent=merge, final_consolidation=False,
                        verbose=True,
                    )

                takeover = [call for call in calls if "objective_no_improve_stop" in call]
                polish = [call for call in calls if "incumbent_interrupt_callback" in call]
                self.assertEqual(len(takeover), int(partition))
                if partition:
                    self.assertGreaterEqual(len(polish), 2)
                else:
                    self.assertEqual(len(calls), 2)
                    self.assertEqual([call['hint_obj'] for call in calls], [10, 20])
                    self.assertIn('[STAGE] LEGACY_REOPTIMIZE', output.getvalue())
                self.assertEqual("[STAGE] SINGLE_ASU_TAKEOVER" in output.getvalue(), partition)
                self.assertIn("[STAGE] FINAL_RESIDUAL_CHECK", output.getvalue())
                residual.assert_called_once()
                self.assertEqual(result["asu_id"], [1, -1, 2])

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



if __name__ == "__main__":
    unittest.main()
