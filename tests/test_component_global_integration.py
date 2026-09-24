"""Public solver dispatch, durable progress, and automatic ASU-count semantics."""
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class ComponentGlobalIntegrationTest(unittest.TestCase):
    def frame(self):
        return pd.DataFrame({
            "tract_ASU_unemp": [10, 0, 20],
            "tract_ASU_emp": [0, 1000, 0],
            "tract_pop2024": [10000, 10000, 10000],
        })

    def test_dispatch_unlimited_count_and_durable_bound_metadata(self):
        with tempfile.TemporaryDirectory() as directory:
            progress = Path(directory) / "progress.json"
            with patch.object(solver, "_prepare_window_hint",
                              side_effect=AssertionError("legacy seeding used")):
                result = solver.build_many_asus_cpsat(
                    self.frame(), [[1], [0, 2], [1]], .2, 10000,
                    component_global=True, max_asus=1, time_limit=10,
                    workers=1, verbose=False, progress_out_path=str(progress),
                )
            self.assertEqual(result["total_unemp"], 30)
            self.assertEqual(result["n_asu"], 2)
            self.assertTrue(result["optimal"])
            self.assertEqual(result["upper_bound"], 30)
            snapshot = json.loads(progress.read_text())
            self.assertEqual(snapshot["phase"], "DONE")
            self.assertEqual(snapshot["asu_id"], result["asu_id"])
            self.assertTrue(snapshot["optimal"])
            self.assertEqual(snapshot["upper_bound"], 30)

    def test_saved_asus_exempt_from_count_cap_and_retained_on_zero_budget(self):
        result = solver.build_many_asus_cpsat(
            self.frame(), [[1], [0, 2], [1]], .2, 10000,
            component_global=True, max_asus=1, initial_asu_id=[9, -1, 22],
            time_limit=0, workers=1, verbose=False,
        )
        self.assertEqual(result["total_unemp"], 30)
        self.assertEqual(result["n_asu"], 2)

    def test_conflicting_strategies_rejected(self):
        for options in (dict(split_warm_start=True),
                        dict(harvest_connectivity_free_asus=True),
                        dict(partition_seed_strategy="surplus_prune")):
            with self.subTest(options=options), self.assertRaisesRegex(
                    ValueError, "cannot be combined"):
                solver.build_many_asus_cpsat(
                    self.frame(), [[1], [0, 2], [1]], .2, 10000,
                    component_global=True, **options)

    def test_warm_start_can_drop_saved_tracts_for_a_better_global_solution(self):
        frame = pd.DataFrame({
            "tract_ASU_unemp": [20, 10, 15],
            "tract_ASU_emp": [0, 100, 100],
            "tract_pop2024": [10000] * 3,
        })
        result = solver.build_many_asus_cpsat(
            frame, [[1, 2], [0], [0]], .2, 10000,
            component_global=True, initial_asu_id=[1, 1, -1],
            time_limit=10, workers=1, verbose=False,
        )
        self.assertTrue(result["optimal"])
        self.assertEqual(result["total_unemp"], 35)
        self.assertEqual([i for i, label in enumerate(result["asu_id"]) if label > 0],
                         [0, 2])


if __name__ == "__main__":
    unittest.main()
