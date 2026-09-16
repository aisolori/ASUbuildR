"""Small exact models and real build-loop coverage for opt-in joint expansion."""
import contextlib
import io
import itertools
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


def chain(n):
    return [[j for j in (i-1, i+1) if 0 <= j < n] for i in range(n)]


class JointModelTest(unittest.TestCase):
    def solve_case(self, u, emp, pop, seeds, nb=None, **kwargs):
        nb = chain(len(u)) if nb is None else nb
        arrays = [np.array(values) for values in (u, emp, pop)]
        return solver._solve_regional_exchange(
            seeds, list(range(len(u))), nb, *arrays, .2, 10000, 5, 2,
            allow_inactive_seeds=True, **kwargs,
        )

    def test_crosses_original_territory_boundary_and_uses_custom_portfolio(self):
        # Territory [2,3] cannot include 2 on its own; ASU 0 can afford it.
        previews = []
        with patch.object(solver, "_configure_asu_solver_portfolio",
                          wraps=solver._configure_asu_solver_portfolio) as configure:
            groups, status = self.solve_case(
                [10, 1, 5, 1], [0, 0, 40, 0], [10000]*4, [[0], [3]],
                incumbent_report_callback=lambda selected, obj: previews.append((selected, obj)),
            )
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(groups, [[0, 1, 2], [3]])
        configure.assert_called_once()
        self.assertEqual(configure.call_args.args[1], 2)
        self.assertTrue(previews)
        self.assertTrue(all(len(nodes) == len(set(nodes)) for nodes, _ in previews))

    def test_weak_seed_can_remain_inactive_without_blocking_valid_seed(self):
        groups, status = self.solve_case([1, 4], [100, 0], [10000]*2, [[0], [1]])
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(groups, [[], [1]])

    def test_optional_groups_do_not_reserve_cardinality(self):
        # Neither island meets population. One group must be allowed all 3 tracts.
        groups, status = self.solve_case([10]*3, [0]*3, [6000]*3, [[0], [2]])
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(sorted(map(len, groups)), [0, 3])

    def test_matches_exhaustive_feasible_assignments(self):
        cases = [
            ([10, 1, 5, 1], [0, 0, 40, 0], [10000]*4, [[0], [3]], chain(4), {}),
            ([10]*4, [0]*4, [6000]*4, [[0], [2], [3]], chain(4), {}),
            ([10]*4, [0]*4, [6000]*4, [[0], [3]], chain(4), {"exact_nodes": 2}),
            ([10]*4, [0]*4, [6000]*4, [[0], [3]], chain(4), {"max_nodes": 2}),
            ([1, 9, 2, 5], [30, 0, 100, 0], [10000]*4, [[0], [3]], chain(4), {}),
            ([10]*4, [0]*4, [6000]*4, [[0], [3]], [[], [2], [1], []], {}),
        ]
        for u, emp, pop, seeds, nb, options in cases:
            with self.subTest(seeds=seeds, options=options, emp=emp, nb=nb):
                arrays = [np.array(values) for values in (u, emp, pop)]
                valid = lambda group: solver.component_ok(group, *arrays, .2, 10000, nb, **options)
                mandatory = [valid(seed) for seed in seeds]
                baseline = sum(sum(u[i] for i in seed) for seed, ok in zip(seeds, mandatory) if ok)
                best = baseline
                for labels in itertools.product(range(len(seeds)+1), repeat=len(u)):
                    groups = [[i for i, label in enumerate(labels) if label == k+1]
                              for k in range(len(seeds))]
                    if all((not group and not required) or
                           (valid(group) and bool(set(group) & set(seed)))
                           for group, seed, required in zip(groups, seeds, mandatory)):
                        best = max(best, sum(u[i] for group in groups for i in group))
                groups, status = self.solve_case(u, emp, pop, seeds, nb, **options)
                self.assertEqual(status, "OPTIMAL")
                self.assertEqual(sum(u[i] for group in groups for i in group), best)

    def test_no_solution_and_zero_budget_keep_only_valid_fallbacks(self):
        args = ([[0], [1]], [0, 1], chain(2), np.array([1, 4]),
                np.array([100, 0]), np.array([10000]*2), .2, 10000)
        groups, status = solver._solve_regional_exchange(
            *args, 0, 2, allow_inactive_seeds=True)
        self.assertEqual((groups, status), ([[], [1]], "DISABLED"))
        with patch.object(solver.cp_model.CpSolver, "Solve", return_value=solver.cp_model.UNKNOWN):
            groups, status = solver._solve_regional_exchange(
                *args, 5, 2, allow_inactive_seeds=True)
        self.assertEqual((groups, status), ([[], [1]], "UNKNOWN"))

    def test_stop_and_skip_are_honored_before_model_construction(self):
        with TemporaryDirectory() as folder:
            for name, expected in (("stop_path", "STOPPED"), ("skip_path", "SKIPPED")):
                flag = Path(folder) / name
                flag.touch()
                groups, status = self.solve_case(
                    [10, 10], [0, 0], [10000]*2, [[0], [1]], **{name: str(flag)})
                self.assertEqual((groups, status), ([[0], [1]], expected))
                self.assertEqual(flag.exists(), name == "stop_path")

    def test_batching_is_bounded_disjoint_and_uses_neighboring_territories(self):
        # Seed priority is 0,1,2,3,4, but territory 1 is isolated.
        territories = [[0], [4], [1], [2], [3]]
        batches = solver._joint_expansion_batches(territories, chain(4) + [[]])
        self.assertEqual(batches, [[0, 2, 3], [1], [4]])
        self.assertEqual(sorted(i for batch in batches for i in batch), list(range(5)))
        self.assertTrue(all(len(batch) <= 3 for batch in batches))


class JointBuildTest(unittest.TestCase):
    def run_build(self, joint, merge, *, weak=False):
        u = [10, 1, 5, 1] if not weak else [1, 0, 0, 4]
        emp = [0, 0, 40, 0] if not weak else [100, 100, 100, 0]
        frame = pd.DataFrame({"geoid": list(map(str, range(4))),
                              "tract_ASU_unemp": u, "tract_ASU_emp": emp,
                              "tract_pop2024": [10000]*4})
        info = dict(connectivity_free_standalone_asus=[[0], [3]],
                    hint_valid=False, hint_improved=[], hint_obj_val=0,
                    hint_source="test", n_contracted=4, root_component=[0])
        events = []
        real_joint = solver._solve_regional_exchange
        real_merge = solver._merge_touching_asu_units

        def solve(units, nodes, *args, **kwargs):
            events.append(("solve", tuple(map(tuple, units))))
            self.assertEqual(args[7], 4)  # full worker budget
            self.assertTrue(kwargs["allow_inactive_seeds"])
            self.assertTrue(callable(kwargs["incumbent_report_callback"]))
            kwargs["log"] = False  # Keep native CP-SAT logs out of test output.
            result = ((units, "OPTIMAL") if kwargs.get("allow_seed_consolidation")
                      else real_joint(units, nodes, *args, **kwargs))
            events.append(("returned", tuple(map(tuple, result[0]))))
            return result

        def combine(*args, **kwargs):
            events.append(("merge", tuple(map(tuple, args[0]))))
            return real_merge(*args, **kwargs)

        log = io.StringIO()
        with (
            TemporaryDirectory() as folder,
            patch.object(solver, "_prepare_window_hint", return_value=info),
            patch.object(solver, "_solve_regional_exchange", side_effect=solve) as joint_solve,
            patch.object(solver, "solve_one_asu_cpsat", return_value=None),
            patch.object(solver, "_merge_touching_asu_units", side_effect=combine),
            patch.object(solver, "_search_unassigned_asu", return_value=([], "INFEASIBLE")),
            contextlib.redirect_stdout(log),
        ):
            result = solver.build_many_asus_cpsat(
                frame, chain(4), .2, 10000, max_asus=2, workers=4, time_limit=1,
                verbose=True, full_graph_window=True, harvest_connectivity_free_asus=True, final_consolidation=False,
                harvest_all_connectivity_free_components=True,
                joint_partition_expansion=joint, standalone_expansion_time_limit=5,
                final_asu_polish_time_limit=0, merge_adjacent=merge,
                combine_capped_asus=False, progress_out_path=str(Path(folder) / "progress.json"),
            )
        return result, events, log.getvalue(), joint_solve.call_count

    def test_enabled_uses_joint_model_then_touching_joint_without_forced_merge(self):
        result, events, log, calls = self.run_build(True, True)
        self.assertEqual(result["asu_id"], [1, 1, 1, 2])
        self.assertGreaterEqual(calls, 2)
        kinds = [event[0] for event in events]
        self.assertNotIn("merge", kinds)
        self.assertEqual(kinds[:3], ["solve", "returned", "solve"])
        self.assertIn("mode=joint solves=1 workers_per_solve=4", log)
        self.assertIn("PARTITION_JOINT_EXPANSION_COMPLETE", log)
        self.assertIn("active=2 inactive=0 unemp=17 gain=6", log)
        self.assertIn("phase=joint_expansion", log)
        self.assertIn("PARTITION_TOUCHING_JOINT_COMPLETE", log)
        self.assertIn("accepted=0 groups_before=2 groups_after=2", log)

    def test_merging_disabled_leaves_two_valid_disjoint_asus(self):
        result, events, _, _ = self.run_build(True, False)
        self.assertEqual(result["asu_id"], [1, 1, 1, 2])
        self.assertFalse(any(event[0] == "merge" for event in events))

    def test_disabled_preserves_sequential_mode(self):
        _, _, log, calls = self.run_build(False, False)
        self.assertEqual(calls, 0)
        self.assertIn("mode=sequential", log)
        self.assertNotIn("PARTITION_JOINT_EXPANSION", log)

    def test_inactive_seed_is_not_committed(self):
        result, _, log, _ = self.run_build(True, False, weak=True)
        self.assertEqual(result["asu_id"], [-1, -1, -1, 1])
        self.assertIn("active=1 inactive=1", log)


if __name__ == "__main__":
    unittest.main()
