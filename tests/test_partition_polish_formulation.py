"""Independent exactness and complete-hint checks for partition polishing."""
import itertools
from pathlib import Path
import random
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


def connected(nodes, graph):
    nodes = set(nodes)
    if not nodes:
        return False
    seen = {next(iter(nodes))}
    queue = list(seen)
    for node in queue:
        for neighbor in graph[node]:
            if neighbor in nodes and neighbor not in seen:
                seen.add(neighbor)
                queue.append(neighbor)
    return seen == nodes


def ring(size):
    return [sorted({(i - 1) % size, (i + 1) % size}) for i in range(size)]


def brute_force(graph, unemployed, employed, labels, root, members=None):
    """Enumerate original tracts, enforcing whole donors independently of CP-SAT."""
    donor_ids = sorted(set(labels) - {-1, 0, 1})
    donors = [{i for i, label in enumerate(labels) if label == donor}
              for donor in donor_ids]
    blocks = members or [[i] for i in range(len(graph))]
    best_key, best_nodes = None, None
    for bits in itertools.product((False, True), repeat=len(blocks)):
        nodes = {i for block, selected in zip(blocks, bits) if selected for i in block}
        if root not in nodes or any(nodes & donor and not donor <= nodes for donor in donors):
            continue
        if not connected(nodes, graph):
            continue
        surplus = sum(4 * unemployed[i] - employed[i] for i in nodes)
        if surplus < 0:
            continue
        profit = sum(unemployed[i] for i in nodes if labels[i] <= 1)
        key = (profit, sum(donor <= nodes for donor in donors), surplus,
               -len(nodes), -sum(i + 1 for i in nodes))
        if best_key is None or key > best_key:
            best_key, best_nodes = key, nodes
    return best_key, best_nodes


class PartitionPolishFormulationTest(unittest.TestCase):
    def solve(self, graph, unemployed, employed, labels, root=0, **options):
        return solver._solve_supernode_polish(
            graph, np.asarray(unemployed), np.asarray(employed),
            np.full(len(graph), 10000), .2, 10000, root, 5.0, 1,
            assignments=np.asarray(labels), asu_number=1,
            hint=[i for i, label in enumerate(labels) if label == 1],
            configure_subsolvers=False, use_surplus_path_repair=False,
            **options)

    @staticmethod
    def force_flow(model, x, roots, graph, profit, fallback, valid, deadline,
                   workers, cancellation, **options):
        options['proof_out'].append(False)
        options['bound_out'].append(None)
        return fallback, sum(int(profit[i]) for group in fallback for i in group), 'FEASIBLE'

    def assert_complete_feasible_hint(self, model, real_solve=None):
        self.assertEqual(model.Validate(), '')
        proto = model.Proto()
        indices = list(proto.solution_hint.vars)
        self.assertEqual(len(indices), len(set(indices)), 'Duplicate hint variable')
        self.assertEqual(set(indices), set(range(len(proto.variables))), 'Incomplete hint')
        check = model.Clone()
        for index, value in zip(indices, proto.solution_hint.values):
            check.Add(check.GetIntVarFromProtoIndex(index) == int(value))
        engine = solver.cp_model.CpSolver()
        engine.parameters.max_time_in_seconds = 2
        engine.parameters.num_search_workers = 1
        status = (real_solve or solver.cp_model.CpSolver.Solve)(engine, check)
        self.assertEqual(status, solver.cp_model.OPTIMAL, 'Complete hint is not feasible')

    def test_contraction_keeps_donors_and_zero_profit_corridors_separate(self):
        graph = [[1], [0, 2], [1, 3], [2, 4], [3, 5], [4, 6], [5, 7], [6]]
        members = [[i] for i in range(6)] + [[6, 7]]
        unemployed = [10, 2, 0, 3, 4, 5, 20, 30]
        employed = [0, 8, 0, 0, 0, 30, 0, 0]
        actual = solver._contract_profitable_polish_members(
            members, graph, unemployed, employed, .2, 1)
        self.assertEqual(actual, [[0, 1], [2], [3, 4], [5], [6, 7]])
        self.assertEqual(sorted(i for group in actual for i in group), list(range(8)))
        self.assertEqual(actual[-1], members[-1])

    def test_contraction_preserves_every_small_optimum_and_tie_accounting(self):
        for seed in range(30):
            rng = random.Random(seed)
            graph = ring(8)
            for a, b in itertools.combinations(range(8), 2):
                if b not in graph[a] and rng.random() < .15:
                    graph[a].append(b)
                    graph[b].append(a)
            unemployed = [rng.randrange(0, 9) for _ in graph]
            employed = [rng.randrange(0, 30) for _ in graph]
            unemployed[0], employed[0] = 10, 0
            unemployed[-2:], employed[-2:] = [3, 4], [0, 0]
            labels = [1] + [-1] * 5 + [2, 2]
            original = [[i] for i in range(6)] + [[6, 7]]
            contracted = solver._contract_profitable_polish_members(
                original, graph, unemployed, employed, .2, 1)
            with self.subTest(seed=seed):
                before = brute_force(graph, unemployed, employed, labels, 0)[0]
                after = brute_force(graph, unemployed, employed, labels, 0, contracted)[0]
                self.assertEqual(after, before)

    def test_cyclic_instances_match_brute_force_in_cut_and_forced_flow_paths(self):
        for seed in range(60):
            rng = random.Random(1700 + seed)
            size = rng.randrange(6, 10)
            graph = ring(size)
            for a, b in itertools.combinations(range(size), 2):
                if b not in graph[a] and rng.random() < .2:
                    graph[a].append(b)
                    graph[b].append(a)
            donor_labels = [7, 3, 3] if seed % 3 == 0 else [2, 2]
            free_count = size - len(donor_labels)
            root = rng.randrange(free_count)
            labels = [-1] * free_count + donor_labels
            labels[root] = 1
            unemployed = [rng.randrange(0, 11) for _ in graph]
            employed = [rng.randrange(0, 60) for _ in graph]
            unemployed[root], employed[root] = 12, 0
            for i in range(free_count, size):
                unemployed[i], employed[i] = 3, 4
            donors = [{i for i, label in enumerate(labels) if label == donor}
                      for donor in sorted(set(donor_labels))]
            expected, _ = brute_force(graph, unemployed, employed, labels, root)
            ties = seed % 5 == 0
            with self.subTest(seed=seed, force_flow=bool(seed % 2)):
                if seed % 2:
                    with patch.object(solver, '_joint_connectivity_cut_pass', side_effect=self.force_flow):
                        result = self.solve(graph, unemployed, employed, labels, root,
                                            deterministic_ties=ties)
                else:
                    result = self.solve(graph, unemployed, employed, labels, root,
                                        deterministic_ties=ties)
                self.assertEqual((result.status, result.obj), ('OPTIMAL', expected[0]))
                selected = set(result.sel_idx_local)
                self.assertIn(root, selected)
                self.assertEqual(result.root_local, root)
                self.assertTrue(connected(selected, graph))
                for donor in donors:
                    self.assertTrue(donor <= selected or not selected & donor)
                self.assertGreaterEqual(sum(4 * unemployed[i] - employed[i] for i in selected), 0)
                if ties:
                    actual = (result.obj, sum(donor <= selected for donor in donors),
                              sum(4 * unemployed[i] - employed[i] for i in selected),
                              -len(selected), -sum(i + 1 for i in selected))
                    self.assertEqual(actual, expected)

    def test_large_contraction_matches_uncontracted_original_tract_result(self):
        size, root = 72, 67
        graph = ring(size)
        labels = [-1] * 70 + [2, 2]
        labels[root] = 1
        unemployed, employed = [2] * size, [0] * size
        unemployed[20] = unemployed[50] = 0
        employed[35] = 30
        results, sizes = [], []
        real_cut = solver._joint_connectivity_cut_pass

        def capture(*args, **kwargs):
            sizes.append(len(args[3]))
            return real_cut(*args, **kwargs)

        with patch.object(solver, '_joint_connectivity_cut_pass', side_effect=capture):
            for contraction in (False, True):
                results.append(self.solve(graph, unemployed, employed, labels, root,
                                          use_profitable_contraction=contraction))
        self.assertEqual(sizes[0], 71)
        self.assertLess(sizes[1], sizes[0] // 2)
        self.assertEqual(results[0].obj, sum(unemployed[:70]))
        self.assertEqual(results[1].obj, results[0].obj)
        self.assertEqual(results[1].sel_idx_local, results[0].sel_idx_local)
        for result in results:
            self.assertEqual(result.status, 'OPTIMAL')
            self.assertEqual(result.root_local, root)
            self.assertIn(root, result.sel_idx_local)
            self.assertIn(70, result.sel_idx_local)
            self.assertIn(71, result.sel_idx_local)

    def test_large_profitable_component_can_collapse_to_one_root_node(self):
        size, root = 64, 41
        labels = [-1] * size
        labels[root] = 1
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(engine, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(engine, model, *args, **kwargs)

        with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=self.force_flow),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            result = self.solve(ring(size), [2] * size, [0] * size, labels, root,
                                deterministic_ties=False)
        self.assertEqual((result.status, result.obj), ('OPTIMAL', 128))
        self.assertEqual(result.sel_idx_local, list(range(size)))
        self.assertEqual(result.root_local, root)
        self.assertEqual(sum(v.name.startswith('polish_supernode_')
                             for v in models[0].Proto().variables), 1)
        self.assert_complete_feasible_hint(models[0], real_solve)

    def test_initial_and_refreshed_hints_complete_closure_and_flow(self):
        graph = [[1], [0, 2], [1, 3], [2, 4], [3]]
        unemployed, employed = [10, 0, 5, 5, 20], [0, 10, 0, 0, 0]
        labels = [1, -1, -1, -1, 2]
        cut_models, flow_models = [], []
        real_solve = solver.cp_model.CpSolver.Solve

        def cuts(model, x, roots, graph, profit, fallback, valid, deadline,
                 workers, cancellation, **options):
            cut_models.append(model.Clone())
            # This connected improvement omits profitable neighbor 3. Refresh
            # must close it and fill all auxiliary hints above the new floor.
            improved = [[0, 1, 2]]
            self.assertTrue(valid(improved))
            model.Add(options['objective'] >= 15)
            options['refresh_hint'](model, improved)
            cut_models.append(model.Clone())
            options['proof_out'].append(False)
            options['bound_out'].append(None)
            return improved, 15, 'FEASIBLE'

        def capture(engine, model, *args, **kwargs):
            flow_models.append(model.Clone())
            return real_solve(engine, model, *args, **kwargs)

        with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            result = self.solve(graph, unemployed, employed, labels, deterministic_ties=False)
        self.assertEqual((result.obj, result.status), (20, 'OPTIMAL'))
        self.assertEqual(len(cut_models), 2)
        self.assertEqual(len(flow_models), 1)
        for model in cut_models + flow_models:
            self.assert_complete_feasible_hint(model, real_solve)
        refreshed = cut_models[-1].Proto()
        values = dict(zip(refreshed.solution_hint.vars, refreshed.solution_hint.values))
        hinted = {v.name: values[i] for i, v in enumerate(refreshed.variables)}
        self.assertEqual(hinted['polish_supernode_3'], 1)
        self.assertEqual(hinted['polish_new_capture_objective'], 20)
        self.assertEqual(hinted['polish_selected_count'], 4)

    def test_initial_hint_expands_adjacent_profitable_tracts_before_flow(self):
        captures = []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(engine, model, *args, **kwargs):
            captures.append(model.Clone())
            return real_solve(engine, model, *args, **kwargs)

        with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=self.force_flow),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            result = self.solve(ring(6), [10, 2, 3, 4, 1, 20], [0] * 6,
                                [1, -1, -1, -1, -1, 2], deterministic_ties=False)
        self.assertEqual(result.obj, 20)
        self.assertTrue(captures)
        self.assert_complete_feasible_hint(captures[0], real_solve)

    def test_integer_endpoint_row_matches_original_two_bound_families(self):
        rng = random.Random(827)
        for case in range(500):
            denominator = rng.randrange(1, 13)
            reduced_upper = rng.randrange(0, 180)
            distance = rng.randrange(0, 140)
            baseline = rng.randrange(0, 6)
            original_upper = rng.randrange(baseline, 30)
            conditional = rng.randrange(-8, original_upper + 1)
            upper = min(original_upper, reduced_upper // denominator)
            bound = max(baseline - 1, min(conditional, (reduced_upper - distance) // denominator))
            for selected in (0, 1):
                for objective in range(baseline, original_upper + 1):
                    before = (objective + (original_upper - conditional) * selected <= original_upper
                              and denominator * objective + distance * selected <= reduced_upper)
                    after = (objective <= upper
                             and objective + (upper - bound) * selected <= upper)
                    self.assertEqual(before, after, (case, selected, objective))


if __name__ == '__main__':
    unittest.main()
