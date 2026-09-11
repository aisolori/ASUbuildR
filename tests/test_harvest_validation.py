"""Exercise the real harvest loop with controlled seeds and solve outcomes."""
import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class HarvestValidationTest(unittest.TestCase):
    def run_harvest(self, u, emp, seeds, territories, solve_result=None):
        df = pd.DataFrame({
            "geoid": ["0", "1", "2"], "tract_ASU_unemp": u,
            "tract_ASU_emp": emp, "tract_pop2024": [10000]*3,
        })
        info = dict(
            connectivity_free_standalone_asus=seeds,
            hint_valid=False, hint_improved=[], hint_obj_val=0,
            hint_source="test", n_contracted=3, root_component=[0],
        )
        log = io.StringIO()
        with (
            patch.object(solver, "_prepare_window_hint", return_value=info),
            patch.object(solver, "_partition_standalone_expansion_territories",
                         side_effect=territories),
            patch.object(solver, "solve_one_asu_cpsat", return_value=solve_result) as solve,
            patch.object(solver, "_merge_touching_asu_units",
                         wraps=solver._merge_touching_asu_units) as merge,
            contextlib.redirect_stdout(log),
        ):
            result = solver.build_many_asus_cpsat(
                df, [[1], [0, 2], [1]], .0645, 10000,
                max_asus=3, workers=1, verbose=True, full_graph_window=True,
                harvest_connectivity_free_asus=True,
                harvest_all_connectivity_free_components=True,
                standalone_expansion_time_limit=1,
                final_asu_polish_time_limit=0,
                combine_capped_asus=False,
            )
        return result, log.getvalue(), solve.call_args_list, merge.call_args_list

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


if __name__ == "__main__":
    unittest.main()
