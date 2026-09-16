"""Merge completed main-build results without interrupting incumbents."""

import contextlib
import io
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class MainImmediateMergeTest(unittest.TestCase):
    def run_build(self, partition=False, consolidate=True):
        frame = pd.DataFrame({
            "geoid": [str(node) for node in range(4)],
            "tract_ASU_unemp": [30, 0, 30, 0],
            "tract_ASU_emp": [70, 100, 70, 100],
            "tract_pop2024": [6_000, 6_000, 6_000, 6_000],
        })
        prepared = {
            "root_component": [0],
            "n_contracted": 2,
            "hint_improved": [0, 1],
            "hint_valid": True,
            "hint_obj_val": 30,
            "hint_source": "test",
            "connectivity_free_standalone_asus": [],
            "connectivity_free_infeasible": False,
            "connectivity_free_proves_optimal": False,
            "connectivity_free_upper_bound": None,
            "connectivity_free_cuts": [],
            "best_fixed_components": [],
        }
        log = io.StringIO()

        def joint(units, *args, **kwargs):
            if not consolidate:
                return units, "OPTIMAL"
            return [sorted(v for unit in units for v in unit)] + [[] for _ in units[1:]], "OPTIMAL"

        with (
            TemporaryDirectory() as progress_dir,
            patch.object(solver, "_prepare_window_hint", return_value=prepared),
            patch.object(
                solver,
                "solve_one_asu_cpsat",
                side_effect=[
                    solver.CpsatResult([0, 1], 0, 30, "OPTIMAL"),
                    solver.CpsatResult(
                        [0, 1], 0, 30, "OPTIMAL"
                    ),
                ],
            ) as exact_solve,
            patch.object(
                solver,
                "improve_by_trades",
                side_effect=lambda selected, *args, **kwargs: list(selected),
            ),
            patch.object(solver, "_solve_regional_exchange", side_effect=joint) as joint_solve,
            contextlib.redirect_stdout(log),
        ):
            result = solver.build_many_asus_cpsat(
                frame,
                [[1], [0, 2], [1, 3], [2]],
                tau=0.10,
                pop_thresh=10_000,
                max_asus=2,
                time_limit=2,
                workers=2,
                verbose=True,
                full_graph_window=True,
                parallel_asus=1,
                deterministic_ties=False,
                configure_subsolvers=False,
                harvest_connectivity_free_asus=partition,
                final_asu_polish_time_limit=0.0,
                merge_adjacent=True,
                progress_out_path=str(Path(progress_dir) / "progress.json"),
            )

        return result, exact_solve, joint_solve, log.getvalue()

    def test_completed_main_result_merges_and_restarts(self):
        result, exact_solve, joint_solve, output = self.run_build()
        joint_solve.assert_not_called()
        self.assertEqual(len(exact_solve.call_args_list), 2)
        self.assertIsNone(
            exact_solve.call_args_list[0].kwargs["incumbent_interrupt_callback"]
        )
        self.assertIsNone(
            exact_solve.call_args_list[1].kwargs["incumbent_interrupt_callback"]
        )
        for call in exact_solve.call_args_list:
            self.assertTrue(callable(call.kwargs["incumbent_report_callback"]))
        self.assertEqual(list(result["asu_id"]), [1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)
        self.assertIn("[STAGE] PARTITION_BUILD asu_target=1", output)
        self.assertIn("incumbent_merge_check=disabled merge_check=after_solve", output)
        self.assertIn("[STAGE] PARTITION_BUILD_MERGE", output)
        self.assertIn("action=restart", output)

    def test_partition_main_commit_uses_safe_union_before_joint(self):
        result, exact, joint, output = self.run_build(partition=True)
        self.assertEqual(result["asu_id"], [1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)
        self.assertEqual(exact.call_count, 2)
        joint.assert_not_called()
        self.assertIn("PARTITION_TOUCHING_SAFE_UNION source=main_commit", output)
        self.assertNotIn("PARTITION_TOUCHING_JOINT source=main_commit", output)
        self.assertNotIn("[STAGE] PARTITION_BUILD_MERGE", output)

    def test_partition_main_commit_safe_union_does_not_depend_on_joint_result(self):
        result, exact, joint, output = self.run_build(partition=True, consolidate=False)
        self.assertEqual(result["asu_id"], [1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)
        self.assertEqual(exact.call_count, 2)
        joint.assert_not_called()
        self.assertIn("PARTITION_TOUCHING_SAFE_UNION source=main_commit", output)


if __name__ == "__main__":
    unittest.main()
