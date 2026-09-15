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
<<<<<<< HEAD
    def test_distant_merge_skips_unchanged_polish_and_exchange_precedes_takeover(self):
        frame = pd.DataFrame({
            "geoid": [str(node) for node in range(10)],
            "tract_ASU_unemp": [20, 20, 20, 5, 20, 20, 10, 20, 20, 5],
            "tract_ASU_emp": [170, 170, 170] + [70] * 7,
            "tract_pop2024": [6000] * 10,
        })
        # Two disconnected regions. A merge in the second cannot change the
        # first ASU's reachable window, so the restart must skip its old solve.
        nb = [[1], [0, 2], [1, 3], [2],
              [5], [4, 6], [5, 7], [6, 8], [7, 9], [8]]
        prepared = {
            "root_component": [0], "n_contracted": 10,
            "hint_improved": [], "hint_valid": False,
            "hint_obj_val": None, "hint_source": "reverse_prune",
            "connectivity_free_standalone_asus": [[0, 1, 2], [4, 5], [7, 8]],
        }
        events = []
        hints = []

        def exact(**kwargs):
            hint = kwargs["hint"]
            hints.append(len(hint))
            events.append("solve")
            if len(hints) == 1:
                # No gain on the most-unemployment ASU, despite a full attempt.
                return solver.CpsatResult(hint, kwargs["root_local"], 60, "STALLED_FEASIBLE")
            if len(hints) == 2:
                return solver.CpsatResult([0, 1, 2], 0, 50, "OPTIMAL")
            if len(hints) == 3:
                return solver.CpsatResult(list(range(6)), 0, 95, "OPTIMAL")
            return None  # Statewide takeover after the regional pass.

        def regional(assignments, *args, **kwargs):
            events.append("regional")
            return assignments.copy()

        with (
            patch.object(solver, "_prepare_window_hint", return_value=prepared),
            patch.object(solver, "solve_one_asu_cpsat", side_effect=exact),
            patch.object(solver, "_regional_exchange_pass", side_effect=regional),
        ):
            result = solver.build_many_asus_cpsat(
                frame, nb, tau=.1, pop_thresh=10000, max_asus=3,
                time_limit=2, workers=1, verbose=False, full_graph_window=True,
                configure_subsolvers=False, deterministic_ties=False,
                harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0,
                final_asu_polish_time_limit=2,
            )
        self.assertEqual(hints, [3, 2, 5, 6])
        self.assertEqual(events, ["solve", "solve", "solve", "regional", "solve"])
        self.assertEqual(result["n_asu"], 2)

=======
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
    def test_merge_restarts_before_next_original_asu_is_polished(self):
        frame = pd.DataFrame({
            "geoid": [str(node) for node in range(6)],
            "tract_ASU_unemp": [30, 30, 10, 20, 20, 5],
<<<<<<< HEAD
            "tract_ASU_emp": [250, 250, 90, 80, 80, 95],
=======
            "tract_ASU_emp": [70, 70, 90, 80, 80, 95],
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
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
<<<<<<< HEAD
            # ASU 1 has more unemployment despite less surplus capacity.
            # It adds tract 2 from its local window [0, 1, 2], touching ASU 2.
=======
            # ASU 1 adds tract 2 and now touches ASU 2 at tract 3.
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
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
<<<<<<< HEAD
        self.assertEqual(exact_solve.call_args_list[0].kwargs["hint_obj"], 60)
        # ASU 1 has surplus 40 versus ASU 2's 200 (tau=.1 => den=9, num=1).
        first = exact_solve.call_args_list[0].kwargs
        self.assertEqual(9 * int(first["u_g"][first["hint"]].sum())
                         - int(first["E_g"][first["hint"]].sum()), 40)
=======
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
        for call in exact_solve.call_args_list[:2]:
            self.assertTrue(callable(call.kwargs["incumbent_report_callback"]))
            self.assertEqual(call.kwargs["incumbent_report_interval_seconds"], 60.0)
            self.assertTrue(callable(call.kwargs["incumbent_interrupt_callback"]))
        self.assertEqual(result["asu_id"], [1, 1, 1, 1, 1, 1])
        self.assertEqual(result["n_asu"], 1)


if __name__ == "__main__":
    unittest.main()
