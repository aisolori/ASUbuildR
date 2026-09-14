"""Regression coverage for immediate merge/restart during final polishing."""

from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class FinalPolishImmediateMergeTest(unittest.TestCase):
    def test_merge_restarts_before_next_original_asu_is_polished(self):
        frame = pd.DataFrame({
            "geoid": [str(node) for node in range(6)],
            "tract_ASU_unemp": [30, 30, 10, 20, 20, 5],
            "tract_ASU_emp": [70, 70, 90, 80, 80, 95],
            "tract_pop2024": [6000, 6000, 1000, 6000, 6000, 1000],
        })
        prepared = {
            "root_component": [0],
            "n_contracted": 6,
            "hint_improved": [],
            "hint_valid": False,
            "hint_obj_val": None,
            "hint_source": "reverse_prune",
            "connectivity_free_standalone_asus": [[0, 1], [3, 4]],
        }
        polish_results = [
            # ASU 1 adds tract 2 and now touches ASU 2 at tract 3.
            solver.CpsatResult([0, 1, 2], 0, 70, "OPTIMAL"),
            # The next solve must be the newly merged five-tract ASU.
            solver.CpsatResult([0, 1, 2, 3, 4, 5], 0, 115, "OPTIMAL"),
            # The later single-ASU takeover pass keeps the polished result.
            None,
        ]

        with (
            TemporaryDirectory() as temp_dir,
            patch.object(solver, "_prepare_window_hint", return_value=prepared),
            patch.object(
                solver,
                "solve_one_asu_cpsat",
                side_effect=polish_results,
            ) as exact_solve,
        ):
            result = solver.build_many_asus_cpsat(
                frame,
                [[1], [0, 2], [1, 3], [2, 4], [3, 5], [4]],
                tau=0.10,
                pop_thresh=10_000,
                max_asus=2,
                time_limit=2,
                workers=1,
                verbose=False,
                full_graph_window=True,
                configure_subsolvers=False,
                deterministic_ties=False,
                harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0.0,
                final_asu_polish_time_limit=2.0,
                progress_out_path=str(Path(temp_dir) / "progress.json"),
            )

        hint_sizes = [
            len(call.kwargs["hint"])
            for call in exact_solve.call_args_list
        ]
        self.assertEqual(hint_sizes[:2], [2, 5])
        for call in exact_solve.call_args_list[:2]:
            self.assertTrue(callable(call.kwargs["incumbent_report_callback"]))
            self.assertEqual(call.kwargs["incumbent_report_interval_seconds"], 60.0)
            self.assertTrue(callable(call.kwargs["incumbent_interrupt_callback"]))
        self.assertEqual(result["asu_id"], [1, 1, 1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)


if __name__ == "__main__":
    unittest.main()
