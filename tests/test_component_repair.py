"""Disposable component-neighborhood repairs must never certify globally."""
import itertools
from pathlib import Path
import random
import sys
import threading
import time
import unittest
from unittest.mock import patch

from ortools.sat.python import cp_model

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as shared
from asu_component_repair import repair_selection, _groups


class ComponentRepairTest(unittest.TestCase):
    def run_case(self, graph, u, emp, pop, initial=None, candidates=None, **kwargs):
        tau, minimum = kwargs.pop("tau", .5), kwargs.pop("pop_thresh", 5)
        if initial is None:
            initial = [-1] * len(graph)
        if candidates is None:
            candidates = list(range(len(graph)))
        result = repair_selection(graph, u, emp, pop, tau, minimum, initial, candidates,
                                  seconds=kwargs.pop("seconds", 3), workers=1, **kwargs)
        baseline = sum(u[i] for i, label in enumerate(initial) if label > 0)
        chosen = [label > 0 for label in result["asu_id"]]
        self.assertEqual(result["total_unemp"], sum(u[i] for i, value in enumerate(chosen) if value))
        self.assertGreaterEqual(result["total_unemp"], baseline)
        self.assertEqual(result["improved"], result["total_unemp"] > baseline)
        self.assertNotIn("upper_bound", result)
        self.assertNotIn("optimal", result)
        for group in _groups(graph, chosen):
            self.assertGreaterEqual(sum(pop[i] for i in group), minimum)
            numerator, denominator = shared.as_fraction_tau(tau)
            self.assertGreaterEqual(sum(denominator * u[i] - numerator * emp[i] for i in group), 0)
            if tau > 0:
                self.assertGreater(sum(u[i] + emp[i] for i in group), 0)
        return result

    def test_connects_population_fragments(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 4], [0, 5, 0], [2, 1, 2])
        self.assertEqual(result["total_unemp"], 9)
        self.assertEqual(result["asu_id"], [1, 1, 1])

    def test_drops_low_rate_connector_and_splits_parent_to_expand(self):
        result = self.run_case([[1, 3], [0, 2], [1], [0]], [5, 0, 5, 4],
                               [0, 8, 0, 8], [3, 1, 3, 1],
                               initial=[1, 1, 1, -1], candidates=[3], pop_thresh=3)
        self.assertEqual(result["total_unemp"], 14)
        self.assertLess(result["asu_id"][1], 0)
        self.assertNotEqual(result["asu_id"][0], result["asu_id"][2])

    def test_two_fixed_neighbors_can_jointly_afford_new_bridge(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 1, 5], [0, 10, 0],
                               [5, 1, 5], initial=[1, -1, 2], candidates=[1], max_nodes=1)
        self.assertEqual(result["total_unemp"], 11)
        self.assertEqual(result["nodes"], 1)
        self.assertEqual(result["asu_id"], [1, 1, 1])

    def test_large_fixed_parent_supplies_exact_economics_and_population(self):
        graph = [[1], [0, 2], [1, 3], [2]]
        result = self.run_case(graph, [2, 2, 2, 1], [1, 1, 1, 3], [1, 1, 1, 1],
                               initial=[1, 1, 1, -1], candidates=[3], max_nodes=1,
                               pop_thresh=3)
        self.assertEqual(result["total_unemp"], 7)
        self.assertEqual(result["nodes"], 1)
        self.assertEqual(result["asu_id"], [1, 1, 1, 1])

    def test_zero_surplus_fixed_parent_cannot_subsidize_deficit(self):
        result = self.run_case([[1], [0, 2], [1, 3], [2]], [2, 2, 2, 1],
                               [2, 2, 2, 2], [1, 1, 1, 1], pop_thresh=3,
                               initial=[1, 1, 1, -1], candidates=[3], max_nodes=1)
        self.assertEqual(result["total_unemp"], 6)
        self.assertEqual(result["status"], "LOCAL_EXHAUSTED")

    def test_invalid_disconnected_relaxation_is_cut_not_returned(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 4], [0, 100, 5], [5, 1, 5],
                               initial=[1, -1, -1], candidates=[2])
        self.assertFalse(result["improved"])
        self.assertEqual(result["asu_id"], [1, -1, -1])
        self.assertGreaterEqual(result["cuts"], 1)

    def test_no_improvement_is_not_a_global_certificate(self):
        result = self.run_case([[], []], [5, 9], [0, 0], [5, 5], initial=[1, -1],
                               candidates=[0], max_nodes=1)
        self.assertEqual(result["total_unemp"], 5)
        self.assertEqual(result["status"], "LOCAL_EXHAUSTED")

    def test_zero_budget_and_immediate_cancellation_preserve_incumbent(self):
        for options, status in (({"seconds": 0}, "TIME_LIMIT"),
                                ({"cancellation": lambda: "STOPPED"}, "STOPPED"),
                                ({"cancellation": lambda: "SKIPPED"}, "SKIPPED")):
            with self.subTest(status=status):
                result = self.run_case([[1], [0]], [5, 3], [0, 0], [5, 5],
                                       initial=[1, -1], candidates=[1], **options)
                self.assertEqual(result["asu_id"], [1, -1])
                self.assertEqual(result["status"], status)
                self.assertEqual(result["rounds"], 0)

    def test_preparation_counts_toward_deadline(self):
        with patch("asu_component_repair.time.monotonic", side_effect=[0, 2, 2]):
            result = repair_selection([[]], [5], [0], [5], .5, 5, [-1], [0], seconds=1)
        self.assertEqual(result["status"], "TIME_LIMIT")
        self.assertEqual(result["rounds"], 0)

    def test_skip_watcher_interrupts_even_without_solution_callbacks(self):
        event = threading.Event()
        solver = cp_model.CpSolver()

        def solve(*args):
            event.set()
            time.sleep(.15)
            return cp_model.UNKNOWN

        with patch.object(shared, "_new_asu_solver", return_value=solver), \
                patch.object(solver, "Solve", side_effect=solve), \
                patch.object(solver, "StopSearch") as stopped:
            result = self.run_case([[]], [5], [0], [5], candidates=[0],
                                   cancellation=lambda: "SKIPPED" if event.is_set() else None)
        self.assertEqual(result["status"], "SKIPPED")
        stopped.assert_called()

    def test_all_repairs_are_quiet_and_capped_at_five_seconds(self):
        solver = cp_model.CpSolver()
        with patch.object(shared, "_new_asu_solver", return_value=solver), \
                patch.object(solver, "Solve", return_value=cp_model.INFEASIBLE):
            self.run_case([[]], [5], [0], [5], candidates=[0], seconds=99)
        self.assertLessEqual(solver.parameters.max_time_in_seconds, 5)
        self.assertFalse(solver.parameters.log_search_progress)
        self.assertFalse(solver.parameters.log_to_stdout)
        self.assertFalse(solver.parameters.log_to_response)

    def test_random_fixed_terminal_neighborhoods_remain_globally_valid(self):
        rng = random.Random(3940)
        for case in range(40):
            count = rng.randint(4, 8)
            graph = [[] for _ in range(count)]
            for a in range(count):
                for b in range(a + 1, count):
                    if rng.random() < .45:
                        graph[a].append(b)
                        graph[b].append(a)
            u = [rng.randrange(1, 8) for _ in graph]
            emp = [rng.randrange(0, 12) for _ in graph]
            pop = [rng.randrange(1, 8) for _ in graph]
            minimum = rng.randrange(3, 9)
            valid = []
            for selected in itertools.product((False, True), repeat=count):
                groups = list(_groups(graph, selected))
                if groups and all(sum(pop[i] for i in group) >= minimum and
                                  sum(u[i] - emp[i] for i in group) >= 0 for group in groups):
                    valid.append(groups)
            if not valid:
                continue
            initial = [-1] * count
            for label, group in enumerate(rng.choice(valid), 1):
                for node in group:
                    initial[node] = label
            with self.subTest(case=case):
                before = [list(row) for row in graph]
                result = self.run_case(graph, u, emp, pop, initial=initial,
                                       max_nodes=rng.randrange(1, 4), pop_thresh=minimum)
                self.assertLessEqual(result["nodes"], 3)
                self.assertEqual(graph, before)

    def test_small_unrestricted_repairs_match_exhaustive_search(self):
        rng = random.Random(1109)
        for case in range(25):
            count = rng.randint(3, 7)
            graph = [[] for _ in range(count)]
            for a in range(count):
                for b in range(a + 1, count):
                    if rng.random() < .4:
                        graph[a].append(b)
                        graph[b].append(a)
            u = [rng.randrange(0, 8) for _ in graph]
            emp = [rng.randrange(0, 14) for _ in graph]
            pop = [rng.randrange(1, 8) for _ in graph]
            minimum = rng.randrange(3, 10)
            optimum = 0
            for selected in itertools.product((False, True), repeat=count):
                groups = list(_groups(graph, selected))
                if all(sum(pop[i] for i in group) >= minimum and
                       sum(u[i] - emp[i] for i in group) >= 0 and
                       any(u[i] + emp[i] > 0 for i in group) for group in groups):
                    optimum = max(optimum, sum(u[i] for i, chosen in enumerate(selected) if chosen))
            with self.subTest(case=case):
                result = self.run_case(graph, u, emp, pop, pop_thresh=minimum)
                self.assertEqual(result["total_unemp"], optimum)


if __name__ == "__main__":
    unittest.main()
