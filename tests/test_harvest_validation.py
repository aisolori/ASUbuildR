"""Exercise the real harvest loop with controlled seeds and solve outcomes."""
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


class HarvestValidationTest(unittest.TestCase):
    def run_harvest(
        self, u, emp, seeds, territories, solve_result=None, workers=1,
        max_asus=None,
    ):
        n = len(u)
        df = pd.DataFrame({
            "geoid": [str(i) for i in range(n)], "tract_ASU_unemp": u,
            "tract_ASU_emp": emp, "tract_pop2024": [10000]*n,
        })
        info = dict(
            connectivity_free_standalone_asus=seeds,
            hint_valid=False, hint_improved=[], hint_obj_val=0,
            hint_source="test", n_contracted=3, root_component=[0],
        )
        log = io.StringIO()

        def joint(units, nodes, *args, **kwargs):
            # Controlled feasible consolidation returned by the joint solver.
            # The build must validate it, not perform an automatic union itself.
            return [sorted(v for unit in units for v in unit)] + [[] for _ in units[1:]], "OPTIMAL"

        with (
            TemporaryDirectory() as progress_dir,
            patch.object(solver, "_prepare_window_hint", return_value=info),
            patch.object(solver, "_partition_standalone_expansion_territories",
                         side_effect=territories),
            patch.object(solver, "solve_one_asu_cpsat", return_value=solve_result) as solve,
            patch.object(solver, "_solve_regional_exchange", side_effect=joint) as merge,
            patch.object(solver, "_merge_touching_asu_units",
                         side_effect=AssertionError("partition must not auto-merge")),
            contextlib.redirect_stdout(log),
        ):
            neighbors = [
                [neighbor for neighbor in (i - 1, i + 1) if 0 <= neighbor < n]
                for i in range(n)
            ]
            result = solver.build_many_asus_cpsat(
                df, neighbors, .0645, 10000,
                max_asus=len(seeds) if max_asus is None else max_asus,
                workers=workers, verbose=True,
                full_graph_window=True,
                harvest_connectivity_free_asus=True,
                harvest_all_connectivity_free_components=True,
                standalone_expansion_time_limit=1,
                final_asu_polish_time_limit=0,
                combine_capped_asus=False,
                progress_out_path=str(Path(progress_dir) / "progress.json"),
            )
        return result, log.getvalue(), solve.call_args_list, merge.call_args_list

    def test_seed_slots_prioritize_surplus_over_unemployment(self):
        result, log, solves, _ = self.run_harvest(
            [100, 0, 20], [1400, 10000, 0],
            [[0], [2]],
            lambda seeds, *args, **kwargs: [list(s) for s in seeds],
            max_asus=1,
        )
        self.assertEqual(list(result["asu_id"]), [-1, -1, 1])
        self.assertIn("seed_priority=q_surplus", log)
        self.assertFalse(solves)

    def test_equal_surplus_prefers_unemployment_then_stable_nodes(self):
        for u, emp, expected in (
            ([129, 0, 258], [0, 100000, 1871], [-1, -1, 1]),
            ([20, 0, 20], [0, 10000, 0], [1, -1, -1]),
        ):
            with self.subTest(u=u):
                result, _, _, _ = self.run_harvest(
                    u, emp, [[2], [0]],
                    lambda seeds, *args, **kwargs: [list(s) for s in seeds],
                    max_asus=1,
                )
                self.assertEqual(list(result["asu_id"]), expected)

    def test_invalid_result_is_excluded_before_merging(self):
        result, log, solves, merges = self.run_harvest(
            [10, 10, 1], [0, 0, 100],
            [[0], [1], [2]],
            lambda seeds, *args, **kwargs: [list(s) for s in seeds],
        )
        self.assertEqual(merges[0].args[0], [[0], [1]])
        self.assertEqual(list(result["asu_id"])[:2], [1, 1])
        self.assertLessEqual(result["asu_id"][2], 0)
        self.assertIn("excluded before merging", log)
        self.assertIn("3 candidate seed(s) (2 independently valid ASU(s))", log)
        self.assertNotIn("sanity check failed", log)
        self.assertFalse(solves)

    def test_valid_lower_objective_repair_of_invalid_seed_is_accepted(self):
        result, log, solves, _ = self.run_harvest(
            [10, 10, 0], [0, 300, 10], [[0, 1]],
            lambda seeds, *args, **kwargs: [[0, 1, 2]],
            solver.CpsatResult([0], 0, 10, "OPTIMAL"),
        )
        self.assertEqual(list(result["asu_id"]), [1, -1, -1])
        self.assertIsNone(solves[0].kwargs["hint"])
        self.assertIsNone(solves[0].kwargs["hint_obj"])
        self.assertNotIn("SEED FALLBACK", log)

    def test_feasible_seed_still_cannot_regress(self):
        result, log, solves, _ = self.run_harvest(
            [10, 10, 0], [0, 0, 1000], [[0, 1]],
            lambda seeds, *args, **kwargs: [[0, 1, 2]],
            solver.CpsatResult([0], 0, 10, "FEASIBLE"),
        )
        self.assertEqual(list(result["asu_id"]), [1, 1, -1])
        self.assertEqual(solves[0].kwargs["hint_obj"], 20)
        self.assertIn("OBJECTIVE REGRESSION", log)

    def test_expansions_are_sequential_and_receive_full_worker_budget(self):
        result, log, solves, _ = self.run_harvest(
            [10, 1, 1, 10],
            [0, 0, 0, 0],
            [[0], [3]],
            lambda seeds, *args, **kwargs: [[0, 1], [3, 2]],
            solver.CpsatResult([0], 10, 10, "OPTIMAL"),
            workers=4,
        )

        self.assertEqual(list(result["asu_id"]), [1, -1, -1, 2])
        self.assertEqual(len(solves), 2)
        self.assertEqual([call.kwargs["workers"] for call in solves], [4, 4])
        self.assertIn(
            "[STAGE] PARTITION_EXPANSION round=1 mode=sequential solves=2 "
            "workers_per_solve=4",
            log,
        )
        self.assertIn(
            "[STAGE] PARTITION_EXPANSION_COMPLETE round=1 "
            "outcome=converged attempted=2 valid=2 rejected=0",
            log,
        )
        self.assertIn("statuses=OPTIMAL:2", log)

    def test_expansion_joint_update_restarts_before_next_stale_solve(self):
        def territories(seeds, *args, **kwargs):
            if len(seeds) == 2:
                return [[0, 1, 2], [3]]
            return [list(seeds[0])]

        result, log, solves, _ = self.run_harvest(
            [10, 1, 1, 10],
            [0, 0, 0, 0],
            [[0], [3]],
            territories,
            solver.CpsatResult([0, 1, 2], 12, 12, "OPTIMAL"),
            workers=4,
        )

        self.assertEqual(len(solves), 1)
        self.assertEqual(solves[0].kwargs["workers"], 4)
        self.assertIsNone(
            solves[0].kwargs["incumbent_interrupt_callback"]
        )
        self.assertTrue(callable(solves[0].kwargs["incumbent_report_callback"]))
        self.assertIn("incumbent_merge_check=disabled merge_check=after_solve", log)
        self.assertEqual(list(result["asu_id"]), [1, 1, 1, 1])
        self.assertIn("outcome=immediate_joint_rerun", log)
        self.assertIn("attempted=1 scheduled=2 skipped_stale=1", log)
        self.assertIn("repartitioning before the next solve", log)


if __name__ == "__main__":
    unittest.main()
