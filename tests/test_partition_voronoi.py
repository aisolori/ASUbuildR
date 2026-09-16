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
    """The original lexicographic multi-source Dijkstra implementation."""
    priority = [-int(u[np.array(nodes, dtype=int)].sum()) for nodes in units]
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
        self.assertEqual(result, [[0, 1], [2]])


if __name__ == "__main__":
    unittest.main()
