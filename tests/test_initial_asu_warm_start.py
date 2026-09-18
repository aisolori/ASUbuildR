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
