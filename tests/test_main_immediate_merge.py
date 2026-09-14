"""Regression coverage for incumbent-triggered merges in the main build loop."""

import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class MainImmediateMergeTest(unittest.TestCase):
    def test_touching_main_incumbent_merges_and_restarts_immediately(self):
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
        with (
            patch.object(solver, "_prepare_window_hint", return_value=prepared),
            patch.object(
                solver,
                "solve_one_asu_cpsat",
                side_effect=[
                    solver.CpsatResult([0, 1], 0, 30, "OPTIMAL"),
                    solver.CpsatResult(
                        [0, 1], 0, 30, "MERGE_STOPPED_FEASIBLE"
                    ),
                ],
            ) as exact_solve,
            patch.object(
                solver,
                "improve_by_trades",
                side_effect=lambda selected, *args, **kwargs: list(selected),
            ),
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
                harvest_connectivity_free_asus=False,
                final_asu_polish_time_limit=0.0,
                merge_adjacent=True,
            )

        self.assertEqual(len(exact_solve.call_args_list), 2)
        self.assertIsNone(
            exact_solve.call_args_list[0].kwargs["incumbent_interrupt_callback"]
        )
        self.assertTrue(callable(
            exact_solve.call_args_list[1].kwargs["incumbent_interrupt_callback"]
        ))
        self.assertEqual(list(result["asu_id"]), [1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)
        output = log.getvalue()
        self.assertIn("[STAGE] PARTITION_BUILD asu_target=1", output)
        self.assertIn("incumbent_merge_check=enabled", output)
        self.assertIn("[STAGE] PARTITION_BUILD_MERGE", output)
        self.assertIn("action=restart", output)


if __name__ == "__main__":
    unittest.main()
