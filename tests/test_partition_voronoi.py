"""Exactness checks for the linear-time partition territory builder."""
import heapq
import random
import sys
import unittest
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
from asu_cpsat import _partition_standalone_expansion_territories


def heap_reference(units, nb, allowed, u):
    """Reference Dijkstra with ascending seed unemployment for distance ties."""
    priority = [int(u[np.array(nodes, dtype=int)].sum()) for nodes in units]
    owner = np.full(len(nb), -1, dtype=int)
    distance = np.full(len(nb), np.iinfo(np.int32).max, dtype=np.int64)
    queue = []
    for unit_index, nodes in enumerate(units):
        for node in nodes:
            owner[node] = unit_index
            distance[node] = 0
            heapq.heappush(queue, (0, priority[unit_index], unit_index, node))
    while queue:
        node_distance, _, unit_index, node = heapq.heappop(queue)
        if distance[node] != node_distance or owner[node] != unit_index:
            continue
        for neighbor in nb[node]:
            if not allowed[neighbor]:
                continue
            candidate = (node_distance + 1, priority[unit_index], unit_index)
            current_owner = int(owner[neighbor])
            current = (
                int(distance[neighbor]),
                priority[current_owner] if current_owner >= 0 else 0,
                current_owner,
            )
            if candidate < current:
                distance[neighbor] = node_distance + 1
                owner[neighbor] = unit_index
                heapq.heappush(
                    queue, (node_distance + 1, priority[unit_index], unit_index, neighbor)
                )
    return [np.flatnonzero(owner == i).tolist() for i in range(len(units))]


class PartitionVoronoiTest(unittest.TestCase):
    def test_equal_distance_prefers_lower_total_seed_unemployment(self):
        result = _partition_standalone_expansion_territories(
            [[0, 1], [4]], [[1], [0, 2], [1, 3], [2, 4], [3]],
            np.ones(5, dtype=bool), np.array([10, 10, 1000, 1000, 15]),
        )
        # Tract 2 is nearer the first seed; tract 3 is nearer the second.
        self.assertEqual(result, [[0, 1, 2], [3, 4]])
        result = _partition_standalone_expansion_territories(
            [[0, 1], [3]], [[1], [0, 2], [1, 3], [2]],
            np.ones(4, dtype=bool), np.array([10, 10, 1000, 15]),
        )
        # Each first-seed tract has fewer unemployed, but its total is higher.
        self.assertEqual(result, [[0, 1], [2, 3]])

    def test_equal_distance_and_unemployment_prefers_earlier_seed(self):
        result = _partition_standalone_expansion_territories(
            [[2], [0]], [[1], [0, 2], [1]], np.ones(3, dtype=bool),
            np.array([10, 1000, 10]),
        )
        self.assertEqual(result, [[1, 2], [0]])

    def test_linear_bfs_matches_heap_reference_on_random_graphs(self):
        rng = random.Random(61204)
        for n in range(2, 50):
            for _ in range(20):
                edges = [{i - 1, i + 1} & set(range(n)) for i in range(n)]
                for i in range(n):
                    for j in range(i + 2, n):
                        if rng.random() < 0.04:
                            edges[i].add(j)
                            edges[j].add(i)
                nb = [sorted(nodes) for nodes in edges]
                seed_nodes = rng.sample(range(n), rng.randint(1, min(10, n)))
                unit_count = rng.randint(1, min(6, len(seed_nodes)))
                units = [seed_nodes[offset::unit_count] for offset in range(unit_count)]
                allowed = np.array([rng.random() < 0.85 for _ in range(n)])
                allowed[seed_nodes] = True
                unemployment = np.array([rng.randrange(100) for _ in range(n)])
                expected = heap_reference(units, nb, allowed, unemployment)
                actual = _partition_standalone_expansion_territories(
                    units, nb, allowed, unemployment
                )
                self.assertEqual(actual, expected)

    def test_duplicate_nodes_within_one_seed_are_harmless(self):
        result = _partition_standalone_expansion_territories(
            [[0, 0], [2]], [[1], [0, 2], [1]], np.ones(3, dtype=bool),
            np.array([10, 0, 1]),
        )
        self.assertEqual(result, [[0], [1, 2]])


if __name__ == "__main__":
    unittest.main()
