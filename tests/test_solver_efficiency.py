"""Regression checks for certificate reuse and release-driven polishing."""
import contextlib
import io
from pathlib import Path
import sys
import time
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class ScreeningEfficiencyTest(unittest.TestCase):
    def solve(self, hint=None):
        return solver.solve_one_asu_cpsat(
            [[1], [0, 2], [1]], np.array([10, 1, 1]), np.array([0, 50, 50]),
            np.array([10000]*3), .1, 10000, 0, time_limit=2, workers=1,
            hint=hint, log=False, deterministic_ties=False,
        )

    def test_valid_hint_skips_screen_invalid_or_absent_hint_screens_once(self):
        for hint, count in (([0], 0), (None, 1), ([1], 1)):
            with self.subTest(hint=hint), patch.object(
                solver, "_connectivity_free_feasibility",
                wraps=solver._connectivity_free_feasibility,
            ) as screen:
                self.assertIsNotNone(self.solve(hint))
                self.assertEqual(screen.call_count, count)

    def test_only_infeasible_rejects(self):
        for status in ("INFEASIBLE", "UNKNOWN", "MODEL_INVALID"):
            with self.subTest(status=status), patch.object(
                solver, "_connectivity_free_feasibility", return_value=status,
            ) as screen:
                result = self.solve()
                self.assertEqual(result is None, status == "INFEASIBLE")
                screen.assert_called_once()

    def test_screen_time_counts_against_connected_solve_budget(self):
        now = [0.0]
        def screen(*args, **kwargs):
            now[0] = 10.0
            return "UNKNOWN"
        with (
            patch.object(solver.time, "monotonic", side_effect=lambda: now[0]),
            patch.object(solver, "_connectivity_free_feasibility", side_effect=screen),
            patch.object(solver.cp_model, "CpSolver") as constructor,
        ):
            self.assertIsNone(self.solve())
            constructor.assert_not_called()

    def test_cache_reuses_certificates_and_distinguishes_constraints(self):
        cache = {}
        def screen(**kwargs):
            return solver._connectivity_free_feasibility(
                np.array([10, 0]), np.array([0, 100]), np.array([10000, 10000]),
                .1, 10000, cache=cache, workers=1, **kwargs,
            )
        with patch.object(solver.cp_model, "CpSolver", wraps=solver.cp_model.CpSolver) as constructor:
            first = screen()
            self.assertIn(first, ("FEASIBLE", "OPTIMAL"))
            self.assertEqual(screen(), first)
            self.assertEqual(constructor.call_count, 1)
            self.assertEqual(screen(required=[1]), "INFEASIBLE")
            self.assertEqual(screen(required=[1]), "INFEASIBLE")
            self.assertEqual(constructor.call_count, 2)
            self.assertEqual(screen(overlap=[]), "INFEASIBLE")
            self.assertEqual(constructor.call_count, 3)
            self.assertEqual(screen(max_nodes=0), "INFEASIBLE")
            self.assertEqual(constructor.call_count, 4)

    def test_unknown_not_cached_and_cache_is_bounded(self):
        cache = {}
        with patch.object(solver.cp_model, "CpSolver") as constructor:
            fake = constructor.return_value
            fake.StatusName.return_value = "UNKNOWN"
            for _ in range(2):
                solver._connectivity_free_feasibility(
                    [10], [0], [10000], .1, 10000, cache=cache,
                )
            self.assertEqual(constructor.call_count, 2)
            self.assertFalse(cache)
            fake.StatusName.return_value = "INFEASIBLE"
            for population in range(40):
                solver._connectivity_free_feasibility(
                    [10], [0], [population], .1, 10000, cache=cache,
                )
            self.assertEqual(len(cache), 32)


class PolishFollowupTest(unittest.TestCase):
    def run_case(self, mode):
        if mode == "released":
            u, emp = [20, 1, 1, 10, 2, 15, 0], [0, 50, 0, 0, 0, 0, 100]
            nb = [[1], [0, 2], [1, 3], [2, 4], [3], [6], [5]]
            seeds = [[0], [2, 3], [5]]
        else:
            u, emp = [100, 90] + [0]*10, [0]*12
            nb = [[j for j in range(12) if j != i] for i in range(12)]
            seeds = [[0, 2, 3, 4, 5], [1, 6, 7, 8, 9]]
        n = len(u)
        frame = pd.DataFrame({
            "geoid": list(map(str, range(n))), "tract_ASU_unemp": u,
            "tract_ASU_emp": emp, "tract_pop2024": [10000+i for i in range(n)],
        })
        info = dict(connectivity_free_standalone_asus=seeds, hint_valid=False,
                    hint_improved=[], hint_obj_val=None, hint_source="test",
                    n_contracted=n, root_component=[0])
        attempts, budgets, caches = [], [], []
        offset = [0.0]
        real_clock = time.monotonic

        def solve(**kwargs):
            if "incumbent_interrupt_callback" not in kwargs:
                return None  # Do not change assignments in the takeover.
            self.assertIsNone(kwargs["incumbent_interrupt_callback"])
            window = [int(value)-10000 for value in kwargs["P_g"]]
            current = [window[i] for i in kwargs["hint"]]
            attempts.append(tuple(current))
            budgets.append(kwargs["time_limit"])
            caches.append(kwargs["feasibility_cache"])
            selected = current
            if mode == "released":
                if current == [2, 3]:
                    selected = [3, 4]
                elif current == [0] and 2 in window:
                    selected = [0, 1, 2]
            else:
                selected = sorted((set(current) - {min(i for i in current if i >= 2)})
                                  | {max(set(window) - set(current))})
                if mode == "budget":
                    offset[0] += 3.0
            return solver.CpsatResult(
                [window.index(i) for i in selected], kwargs["root_local"],
                sum(u[i] for i in selected), "FEASIBLE",
            )

        log = io.StringIO()
        with (
            patch.object(solver, "_prepare_window_hint", return_value=info),
            patch.object(solver, "_partition_standalone_expansion_territories",
                         side_effect=lambda seeds, *a, **kw: [list(s) for s in seeds]),
            patch.object(solver, "solve_one_asu_cpsat", side_effect=solve),
            patch.object(solver, "_regional_exchange_pass", side_effect=lambda a, *args, **kw: a.copy()),
            patch.object(solver, "_search_unassigned_asu", return_value=([], "INFEASIBLE")),
            patch.object(solver.time, "monotonic", side_effect=lambda: real_clock() + offset[0]),
            contextlib.redirect_stdout(log),
        ):
            result = solver.build_many_asus_cpsat(
                frame, nb, .1, 10000, max_asus=len(seeds), workers=1,
                verbose=True, full_graph_window=True, harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=1, final_asu_polish_time_limit=1,
                merge_adjacent=False,
            )
        self.assertTrue(all(cache is caches[0] for cache in caches))
        ids = np.array(result["asu_id"])
        for k in np.unique(ids[ids > 0]):
            self.assertTrue(solver.component_ok(
                np.flatnonzero(ids == k).tolist(), np.array(u), np.array(emp),
                frame["tract_pop2024"].to_numpy(), .1, 10000, nb,
            ))
        return result, attempts, budgets, log.getvalue()

    def test_only_affected_asu_revisited_after_later_release_without_merge(self):
        result, attempts, _, log = self.run_case("released")
        self.assertEqual(attempts, [(0,), (5,), (2, 3), (0,)])
        self.assertEqual(result["asu_id"], [1, 1, 1, 3, 3, 2, -1])
        self.assertIn("FINAL_POLISH_RECHECK reason=reachable_window_grew queued=1", log)

    def test_followup_rounds_are_bounded(self):
        _, attempts, _, log = self.run_case("rounds")
        self.assertEqual(len(attempts), 5)  # Two initial attempts, three follow-ups.
        self.assertIn("FINAL_POLISH_RECHECK_LIMIT", log)
        self.assertIn("rounds=3/3", log)

    def test_followup_time_budget_is_shared(self):
        _, attempts, budgets, log = self.run_case("budget")
        self.assertEqual(len(attempts), 3)
        self.assertTrue(all(0 < seconds <= 1 for seconds in budgets))
        self.assertIn("FINAL_POLISH_RECHECK_LIMIT", log)
        self.assertIn("seconds_remaining=0.000", log)


class PartitionWorkspaceCacheTest(unittest.TestCase):
    def test_repartition_gain_reuses_compiled_territory(self):
        frame = pd.DataFrame({
            "geoid": ["0", "1", "2"],
            "tract_ASU_unemp": [10, 1, 1],
            "tract_ASU_emp": [0, 0, 0],
            "tract_pop2024": [10000, 10000, 10000],
        })
        info = dict(
            connectivity_free_standalone_asus=[[0]], hint_valid=False,
            hint_improved=[], hint_obj_val=None, hint_source="test",
            n_contracted=3, root_component=[0],
        )
        compiled_neighbors = []

        def solve(**kwargs):
            compiled_neighbors.append(kwargs["nb_local"])
            selected = [0, 1] if len(compiled_neighbors) == 1 else kwargs["hint"]
            return solver.CpsatResult(
                list(selected), kwargs["root_local"],
                int(kwargs["u_g"][selected].sum()), "FEASIBLE",
            )

        output = io.StringIO()
        with (
            patch.object(solver, "_prepare_window_hint", return_value=info),
            patch.object(solver, "solve_one_asu_cpsat", side_effect=solve),
            patch.object(solver, "_search_unassigned_asu", return_value=([], "INFEASIBLE")),
            contextlib.redirect_stdout(output),
        ):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0, 2], [1]], .1, 10000,
                max_asus=1, full_graph_window=True,
                harvest_connectivity_free_asus=True, merge_adjacent=False,
                standalone_expansion_time_limit=1,
                final_asu_polish_time_limit=0, verbose=True, workers=1,
            )

        self.assertEqual(result["asu_id"], [1, 1, -1])
        self.assertEqual(len(compiled_neighbors), 2)
        self.assertIs(compiled_neighbors[0], compiled_neighbors[1])
        self.assertIn("reused compiled territory: 3 tracts", output.getvalue())


if __name__ == "__main__":
    unittest.main()
