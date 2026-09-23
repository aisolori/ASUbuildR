"""Behavioral checks for the standalone round-robin experiment."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'tools'))
import asu_round_robin as rr


class RoundRobinTest(unittest.TestCase):
    def run_growth(self, nb, u, q, **kwargs):
        # At tau=.5, the exact integer surplus is U-E.
        return rr.grow(nb, u, [a-b for a, b in zip(u, q)],
                       [6000]*len(u), '.5', **kwargs)

    def test_roots_take_turns_and_ratio_breaks_size_ties(self):
        nb = [[1], [0, 2], [1], [4], [3, 5], [4]]
        _, _, _, history, roots = self.run_growth(nb, [20, 5, 5, 10, 5, 5],
                                                  [10, -2, -2, 10, -2, -2])
        self.assertEqual(roots, 2)
        self.assertEqual([h['candidate_root_index'] for h in history], [0, 3, 0, 3])
        self.assertEqual([h['sweep'] for h in history], [1, 1, 2, 2])

    def test_positive_neighbor_merges_before_deficit_growth(self):
        owner, groups, _, history, _ = self.run_growth(
            [[1, 2], [0], [0]], [20, 10, 100], [10, 1, -5])
        self.assertEqual(history[0]['action'], 'merge')
        self.assertEqual(history[1]['sweep'], 2)
        self.assertEqual(len(groups), 1)
        self.assertEqual(len(set(owner)), 1)

    def test_deficit_neighbors_rank_by_efficiency_not_total_unemployment(self):
        _, _, _, history, _ = self.run_growth(
            [[1, 2, 3], [0], [0], [0]], [30, 10, 30, 1000], [20, -2, -10, -25])
        self.assertEqual([h['neighbor_index'] for h in history], [1, 2])
        self.assertTrue(all(h['q_after'] >= 0 for h in history))

    def test_blocked_roots_do_not_prevent_others_growing(self):
        _, _, _, history, _ = self.run_growth(
            [[1], [0], [3], [2, 4], [3]], [10, 30, 30, 5, 5], [1, -2, 20, -3, -3])
        self.assertEqual([h['candidate_root_index'] for h in history], [2, 2])

    def test_smallest_candidate_precedes_larger_merged_group(self):
        _, _, _, history, _ = self.run_growth(
            [[1, 2], [0], [0], [4], [3]], [100, 100, 10, 1, 10],
            [1, 1, -1, 1, -1])
        self.assertEqual(history[0]['action'], 'merge')
        self.assertEqual(history[1]['candidate_root_index'], 3)
        self.assertEqual(history[1]['nodes_before'], 1)

    def test_final_merge_can_qualify_individually_small_candidates(self):
        nb = [[1], [0]]
        owner, _, q, _, _ = self.run_growth(nb, [10, 10], [5, 5], merge_during_growth=False)
        labels, summary, rejected = rr.finalize(nb, owner, [10, 10], [5, 5],
                                               [6000, 6000], q, 10000)
        self.assertEqual(labels, [1, 1])
        self.assertEqual(summary[0]['unemployment'], 20)
        self.assertFalse(rejected)
        labels, summary, rejected = rr.finalize(nb, owner, [10, 10], [5, 5],
                                               [6000, 6000], q, 10000, False)
        self.assertEqual(labels, [-1, -1])
        self.assertEqual(len(rejected), 2)

    def test_no_roots_and_zero_surplus_are_handled(self):
        self.assertEqual(self.run_growth([[]], [10], [0])[0], [-1])
        owner, _, q, _, _ = self.run_growth([[1], [0]], [10, 10], [5, 0])
        self.assertEqual(owner, [0, 0])
        self.assertEqual(q, [5, 0])

    def test_geoid_normalization_and_bad_adjacency(self):
        self.assertEqual(rr.normalize_geoid('14000US55001950100'), '55001950100')
        self.assertEqual(rr.normalize_geoid('1001950100'), '01001950100')
        for nb in ([[1], []], [[0]], [[3], []]):
            with self.assertRaises(ValueError):
                rr.validate_neighbors(nb)

    def test_shedding_ignores_population_and_does_not_mutate_incumbent(self):
        nb = [[1], [0]]
        group = rr.Candidate({0, 1}, {0}, 30, 5, 11000)
        retained, history = rr.shed_deficits(nb, {0: group}, [20, 10], [10, -5], 1)
        self.assertEqual(retained, [{0}])  # Can fall below the population minimum.
        self.assertEqual(group.nodes, {0, 1})
        self.assertEqual(history[0]['action'], 'shed')
        self.assertEqual(history[0]['q_after'], 10)

    def test_shedding_recomputes_articulation_after_each_removal(self):
        # Either negative tract can be removed from the cycle, but not both:
        # after removing 1, tract 3 is the only path between positive roots.
        nb = [[1, 3], [0, 2], [1, 3], [0, 2]]
        group = rr.Candidate(set(range(4)), {0, 2}, 51, 12, 24000)
        retained, history = rr.shed_deficits(nb, {0: group}, [20, 1, 20, 10],
                                           [10, -3, 10, -5], 1)
        self.assertEqual(retained, [{0, 2, 3}])
        self.assertEqual([h['neighbor_index'] for h in history], [1])
        self.assertEqual(rr.articulation_points(nb, retained[0]), {3})

    def test_repeated_growth_stops_and_retains_qualified_best(self):
        args = ([[1], [0]], [20, 10], [10, 15], [6000, 6000], '.5', 10000)
        owner, _, _, history, _, refinement = rr.run_experiment(*args)
        self.assertEqual(owner, [0, 0])
        self.assertEqual(refinement['stop_reason'], 'REPEATED_ASSIGNMENT')
        self.assertEqual(refinement['best_cycle'], 0)
        self.assertEqual([row['captured_unemployment'] for row in refinement['cycles']], [30, 30])
        self.assertEqual([event['action'] for event in history], ['add', 'shed', 'add'])

    def test_growth_continues_from_retained_groups(self):
        result = rr.grow([[1], [0, 2], [1]], [10, 5, 5], [0, 7, 7],
                         [6000]*3, '.5', initial_groups=[{0, 1}], cycle=2)
        self.assertEqual(result[0], [0, 0, 0])
        self.assertEqual(len(result[3]), 1)
        self.assertEqual(result[3][0]['cycle'], 2)
        self.assertEqual(result[3][0]['nodes_before'], 2)

    def test_no_removable_deficits_and_disabled_refinement(self):
        args = ([[1], [0, 2], [1]], [20, 10, 20], [10, 15, 10], [6000]*3, '.5', 10000)
        result = rr.run_experiment(*args)
        self.assertEqual(result[-1]['stop_reason'], 'NO_REMOVABLE_DEFICITS')
        first = rr.grow(*args[:5])
        disabled = rr.run_experiment(*args, prune_regrow_cycles=0)
        self.assertEqual(first[0], disabled[0])
        self.assertEqual(disabled[-1]['stop_reason'], 'DISABLED')

    def test_articulation_matches_exhaustive_removal_on_small_graphs(self):
        import itertools
        edges = list(itertools.combinations(range(4), 2))
        for flags in itertools.product((False, True), repeat=len(edges)):
            nb = [[] for _ in range(4)]
            for (a, b), present in zip(edges, flags):
                if present:
                    nb[a].append(b)
                    nb[b].append(a)
            for flags in itertools.product((False, True), repeat=4):
                nodes = {i for i, selected in enumerate(flags) if selected}
                count = len(rr.components(nb, nodes))
                expected = {i for i in nodes if len(rr.components(nb, nodes-{i})) > count}
                self.assertEqual(rr.articulation_points(nb, nodes), expected)

    def path_option(self, nb, u, q, members, hops=6, served=None):
        owner = [-1] * len(nb)
        groups = {}
        for group_id, nodes in members.items():
            for node in nodes:
                owner[node] = group_id
            groups[group_id] = rr.Candidate(set(nodes), {i for i in nodes if q[i] > 0},
                                            sum(u[i] for i in nodes), sum(q[i] for i in nodes), 10000)
        return rr.surplus_path_option(nb, 0, groups, owner, u, q, served or set(), hops)

    def test_path_crosses_prefix_neither_root_can_afford(self):
        nb = [[1], [0, 2], [1]]
        baseline = self.run_growth(nb, [20, 25, 10], [5, -8, 5])
        result = self.run_growth(nb, [20, 25, 10], [5, -8, 5], growth_mode='paths')
        self.assertEqual(baseline[0], [0, -1, 2])
        self.assertEqual(result[0], [0, 0, 0])
        self.assertEqual(len(result[3]), 1)
        self.assertEqual(result[3][0]['action'], 'path_merge')
        self.assertEqual(result[3][0]['q_after'], 2)
        self.assertEqual(result[3][0]['path_indices'], [1])

    def test_path_prices_entire_donor_net_surplus(self):
        self.assertIsNone(self.path_option(
            [[1], [0, 2], [1, 3], [2]], [20, 25, 20, 10], [5, -8, 20, -18],
            {0: {0}, 2: {2, 3}}))

    def test_cheapest_deficit_route_and_hop_limit(self):
        nb = [[1, 2], [0, 4], [0, 3], [2, 4], [1, 3]]
        args = nb, [20]*5, [5, -12, -2, -3, 10], {0: {0}, 4: {4}}
        longer = self.path_option(*args, hops=3)
        self.assertEqual(longer[4:], ([2, 3], 5))
        shorter = self.path_option(*args, hops=2)
        self.assertEqual(shorter[4:], ([1], 12))

    def test_hop_labels_do_not_discard_costlier_short_prefix(self):
        nb = [[1, 3], [0, 2], [1, 4], [0, 4], [2, 3, 5], [4]]
        option = self.path_option(nb, [20]*6, [5, -1, 0, -5, 0, 10],
                                  {0: {0}, 5: {5}}, hops=3)
        self.assertEqual(option[4:], ([3, 4], 5))

    def test_path_does_not_cross_or_absorb_served_group(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        option = self.path_option(nb, [20]*4, [5, -1, 5, 20],
                                  {0: {0}, 2: {2}, 3: {3}}, served={2})
        self.assertIsNone(option)

    def test_path_rank_values_surplus_return_not_just_distance(self):
        nb = [[1, 3], [0, 2], [1], [0, 4], [3]]
        option = self.path_option(nb, [20]*5, [5, -4, 4, -5, 20],
                                  {0: {0}, 2: {2}, 4: {4}})
        self.assertEqual(option[2], 4)


if __name__ == '__main__':
    unittest.main()
