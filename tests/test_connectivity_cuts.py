"""Validity checks for rooted separator cuts used by the ASU solvers."""

import itertools
import random
import sys
import unittest
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
from asu_cpsat import (  # noqa: E402
    _asu_units_touch,
    _minimum_root_vertex_separator,
    _root_graph_distances,
    _small_root_separator_implications,
)


def _is_root_connected(nb, selected, root=0):
    if root not in selected:
        return False
    reached = {root}
    pending = [root]
    while pending:
        node = pending.pop()
        for neighbor in nb[node]:
            if neighbor in selected and neighbor not in reached:
                reached.add(neighbor)
                pending.append(neighbor)
    return reached == selected


class ConnectivityCutValidityTest(unittest.TestCase):
    def test_touch_detection_matches_overlap_and_graph_adjacency(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        self.assertTrue(_asu_units_touch([0], [0], nb))
        self.assertTrue(_asu_units_touch([0, 1], [2], nb))
        self.assertFalse(_asu_units_touch([0], [2, 3], nb))

    def test_distance_cardinality_reachability_for_connected_selections(self):
        rng = random.Random(8317)
        checked = 0
        for n_nodes in range(2, 9):
            for _ in range(20):
                adjacency = [set() for _ in range(n_nodes)]
                for left, right in itertools.combinations(range(n_nodes), 2):
                    if rng.random() < 0.28:
                        adjacency[left].add(right)
                        adjacency[right].add(left)
                nb = [sorted(neighbors) for neighbors in adjacency]
                distances = _root_graph_distances(nb, 0)
                for mask in range(1, 1 << n_nodes, 2):
                    selected = {
                        node for node in range(n_nodes) if mask & (1 << node)
                    }
                    if not _is_root_connected(nb, selected):
                        continue
                    for node in selected:
                        self.assertLess(distances[node], n_nodes)
                        self.assertGreaterEqual(len(selected), distances[node] + 1)
                        checked += 1
        self.assertGreater(checked, 0)

    def test_minimum_separator_disconnects_target_and_avoids_protected_nodes(self):
        # Two internally disjoint root-target paths require separator {1, 2}.
        nb = [[1, 2], [0, 3], [0, 3], [1, 2]]
        separator = _minimum_root_vertex_separator(
            nb, 0, 3, protected_nodes={0, 3}, max_size=3
        )
        self.assertEqual(set(separator or ()), {1, 2})
        remaining = set(range(len(nb))) - set(separator or ())
        self.assertFalse(_is_root_connected(nb, remaining, root=0))

    def test_static_separator_clauses_and_cardinality_bounds_are_valid(self):
        rng = random.Random(20260914)
        checked_clauses = 0
        checked_bounds = 0
        for n_nodes in range(4, 9):
            for _ in range(24):
                adjacency = [set() for _ in range(n_nodes)]
                # Start with a spanning path, then add random chords.
                for node in range(n_nodes - 1):
                    adjacency[node].add(node + 1)
                    adjacency[node + 1].add(node)
                for left, right in itertools.combinations(range(n_nodes), 2):
                    if right != left + 1 and rng.random() < 0.18:
                        adjacency[left].add(right)
                        adjacency[right].add(left)
                nb = [sorted(neighbors) for neighbors in adjacency]
                unemployment = np.array(
                    [rng.randrange(1, 30) for _ in range(n_nodes)], dtype=np.int64
                )
                surplus = np.array(
                    [rng.randrange(-25, 26) for _ in range(n_nodes)], dtype=np.int64
                )
                clauses, bounds = _small_root_separator_implications(
                    nb,
                    0,
                    unemployment,
                    q_surplus=surplus,
                    max_size=3,
                    clause_limit=500,
                    target_limit=n_nodes,
                )

                for mask in range(1 << n_nodes):
                    selected = {
                        node for node in range(n_nodes) if mask & (1 << node)
                    }
                    if (
                        not _is_root_connected(nb, selected)
                        or sum(int(surplus[node]) for node in selected) < 0
                    ):
                        continue
                    for target, separator in clauses:
                        if target in selected:
                            self.assertTrue(selected.intersection(separator))
                        checked_clauses += 1
                    for separator, affected, upper_bound in bounds:
                        affected_count = len(selected.intersection(affected))
                        if affected_count:
                            self.assertTrue(selected.intersection(separator))
                            self.assertLessEqual(affected_count, upper_bound)
                        checked_bounds += 1

        self.assertGreater(checked_clauses, 0)
        self.assertGreater(checked_bounds, 0)


if __name__ == "__main__":
    unittest.main()
