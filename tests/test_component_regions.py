"""Safety and strength of component-global regional constraints."""
import itertools
from pathlib import Path
import random
import sys
import unittest

from ortools.sat.python import cp_model

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
from asu_component_regions import add_region_constraints, iter_regions


class ComponentRegionsTest(unittest.TestCase):
    @staticmethod
    def model_for(graph, u, emp, pop, surplus, minimum, region, tau=.5):
        model = cp_model.CpModel()
        x = [model.NewBoolVar(f'x_{i}') for i in range(len(graph))]
        count = add_region_constraints(model, x, graph, u, emp, pop, surplus,
                                       tau, minimum, region, name='test')
        return model, x, count

    @staticmethod
    def accepts(model, x, selected):
        model.ClearAssumptions()
        model.AddAssumptions([variable if value else variable.Not()
                              for variable, value in zip(x, selected)])
        solver = cp_model.CpSolver()
        solver.parameters.num_search_workers = 1
        solver.parameters.max_time_in_seconds = 2
        return solver.Solve(model) in (cp_model.FEASIBLE, cp_model.OPTIMAL)

    @staticmethod
    def globally_valid(graph, u, emp, pop, surplus, minimum, tau, chosen):
        unseen = {i for i, value in enumerate(chosen) if value}
        while unseen:
            start = unseen.pop()
            stack, nodes = [start], [start]
            while stack:
                for neighbor in graph[stack.pop()]:
                    if neighbor in unseen:
                        unseen.remove(neighbor)
                        nodes.append(neighbor)
                        stack.append(neighbor)
            if sum(pop[i] for i in nodes) < minimum or sum(surplus[i] for i in nodes) < 0:
                return False
            if tau > 0 and sum(u[i] + emp[i] for i in nodes) == 0:
                return False
        return True

    def test_whole_ring_generation_is_bounded_and_canonical(self):
        graph = [[1], [0, 2, 3], [1, 4], [1], [2]]
        self.assertEqual(list(iter_regions(graph, [1, 1], rings=2)),
                         [(1,), (0, 1, 2, 3), (0, 1, 2, 3, 4)])
        self.assertEqual(list(iter_regions(graph, [1], max_nodes=3)), [(1,)])
        self.assertEqual(list(iter_regions(graph, [1, 2], max_nodes=1)), [])
        self.assertEqual(list(iter_regions(graph, [])), [])
        self.assertEqual(list(iter_regions(graph, [1], rings=0)), [(1,)])

    def test_expanded_region_blocks_one_tract_boundary_escape(self):
        graph = [[1], [0, 2], [1]]
        args = (graph, [1, 0, 20], [4, 2, 0], [5, 5, 5], [-3, -2, 20], 5)
        small, x, _ = self.model_for(*args, [0])
        expanded, y, _ = self.model_for(*args, [0, 1])
        self.assertTrue(self.accepts(small, x, [1, 1, 0]))
        self.assertFalse(self.accepts(expanded, y, [1, 1, 0]))
        self.assertTrue(self.accepts(expanded, y, [1, 1, 1]))
        self.assertTrue(self.accepts(expanded, y, [0, 0, 0]))

    def test_population_applies_to_selected_subset_not_whole_region(self):
        graph = [[1], [0, 2], [1]]
        model, x, _ = self.model_for(graph, [1, 1, 1], [0, 0, 0],
                                     [2, 8, 8], [1, 1, 1], 5, [0, 1])
        self.assertFalse(self.accepts(model, x, [1, 0, 0]))
        self.assertTrue(self.accepts(model, x, [0, 1, 0]))
        self.assertTrue(self.accepts(model, x, [1, 1, 0]))
        # An outside selection disables this necessary regional condition. A
        # later component cut remains responsible for any actual invalid ASU.
        self.assertTrue(self.accepts(model, x, [1, 0, 1]))
        self.assertTrue(self.accepts(model, x, [0, 0, 0]))

    def test_zero_labor_and_insufficient_population_are_conditional(self):
        graph = [[1], [0]]
        for population, threshold, tau in (([10, 10], 5, .5), ([2, 10], 5, 0)):
            model, x, _ = self.model_for(graph, [0, 3], [0, 0], population,
                                         [0, 3], threshold, [0], tau=tau)
            self.assertFalse(self.accepts(model, x, [1, 0]))
            self.assertTrue(self.accepts(model, x, [1, 1]))
            self.assertTrue(self.accepts(model, x, [0, 0]))
        model, x, _ = self.model_for(graph, [0, 3], [0, 0], [10, 10],
                                     [0, 3], 5, [0], tau=0)
        self.assertTrue(self.accepts(model, x, [1, 0]))

    def test_labor_constraint_applies_to_selected_subset(self):
        graph = [[1], [0]]
        model, x, _ = self.model_for(graph, [0, 2], [0, 0], [10, 10],
                                     [0, 2], 5, [0, 1])
        self.assertFalse(self.accepts(model, x, [1, 0]))
        self.assertTrue(self.accepts(model, x, [1, 1]))

    def test_no_boundary_and_empty_region_and_constraint_count(self):
        graph = [[], []]
        model, x, count = self.model_for(graph, [2, 1], [0, 3], [5, 5],
                                         [2, -2], 5, [0, 1])
        self.assertEqual(count, len(model.Proto().constraints))
        self.assertFalse(self.accepts(model, x, [0, 1]))
        self.assertTrue(self.accepts(model, x, [1, 0]))
        empty, _, count = self.model_for(graph, [2, 1], [0, 3], [5, 5],
                                         [2, -2], 5, [])
        self.assertEqual(count, 0)
        self.assertEqual(len(empty.Proto().constraints), 0)

    def test_exhaustive_valid_selections_survive_random_regions(self):
        rng = random.Random(240926)
        checked = 0
        for case in range(12):
            n = 6
            graph = [[] for _ in range(n)]
            for a in range(n):
                for b in range(a + 1, n):
                    if rng.random() < .35:
                        graph[a].append(b)
                        graph[b].append(a)
            u = [rng.randrange(6) for _ in range(n)]
            emp = [rng.randrange(8) for _ in range(n)]
            pop = [rng.randrange(8) for _ in range(n)]
            tau = rng.choice((0, .5))
            surplus = [a - b if tau else a for a, b in zip(u, emp)]
            minimum = rng.randrange(0, 10)
            model = cp_model.CpModel()
            x = [model.NewBoolVar(f'x_{i}') for i in range(n)]
            # Include every nonempty region, including disconnected ones.
            for bits in itertools.product((0, 1), repeat=n):
                region = tuple(i for i, value in enumerate(bits) if value)
                add_region_constraints(model, x, graph, u, emp, pop, surplus,
                                       tau, minimum, region, name=f'r_{region}')
            for chosen in itertools.product((False, True), repeat=n):
                if self.globally_valid(graph, u, emp, pop, surplus, minimum, tau, chosen):
                    with self.subTest(case=case, chosen=chosen):
                        self.assertTrue(self.accepts(model, x, chosen))
                        checked += 1
        self.assertGreater(checked, 100)


if __name__ == '__main__':
    unittest.main()
