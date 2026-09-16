"""Independent exhaustive checks for uncapped objective-preserving reductions."""
import itertools
import random
import sys
import unittest
from fractions import Fraction
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
from asu_cpsat import (
    _lagrangian_objective_bound_cached,
    _lagrangian_conditional_bounds,
    _lagrangian_objective_bound,
    _profitable_closure_edges,
    solve_one_asu_cpsat,
)


class UncappedReductionsTest(unittest.TestCase):
    def test_global_bound_against_enumeration_and_cache(self):
        rng = random.Random(8921)
        _lagrangian_objective_bound_cached.cache_clear()
        for n in range(1, 9):
            for _ in range(50):
                u = [rng.randrange(21) for _ in range(n)]
                q = [rng.randrange(-20, 21) for _ in range(n)]
                forced = {i for i in range(n) if rng.random() < 0.2}
                bound = _lagrangian_objective_bound(u, q, forced)
                feasible = []
                for mask in range(1 << n):
                    selected = {i for i in range(n) if mask & (1 << i)}
                    if forced <= selected and sum(q[i] for i in selected) >= 0:
                        feasible.append(selected)
                if feasible:
                    self.assertGreaterEqual(
                        bound, max(sum(u[i] for i in selected) for selected in feasible)
                    )
                else:
                    self.assertEqual(bound, -1)
        misses = _lagrangian_objective_bound_cached.cache_info().misses
        self.assertEqual(
            _lagrangian_objective_bound([10, 1], [1, -1], {0}), 11
        )
        first = _lagrangian_objective_bound_cached.cache_info()
        self.assertEqual(
            _lagrangian_objective_bound([10, 1], [1, -1], {0}), 11
        )
        second = _lagrangian_objective_bound_cached.cache_info()
        self.assertEqual(first.misses, misses + 1)
        self.assertEqual(second.hits, first.hits + 1)

    def test_conditional_bounds_against_enumeration(self):
        rng = random.Random(73419)
        for n in range(1, 9):
            for _ in range(50):
                u = [rng.randrange(21) for _ in range(n)]
                q = [rng.randrange(-20, 21) for _ in range(n)]
                forced = {i for i in range(n) if rng.random() < 0.2}
                bounds = _lagrangian_conditional_bounds(u, q, forced)
                subsets = [
                    set(i for i in range(n) if mask & (1 << i))
                    for mask in range(1 << n)
                ]
                for i, bound in enumerate(bounds):
                    required = forced | {i}
                    feasible = [
                        s for s in subsets if required <= s
                        and sum(q[j] for j in s) >= 0
                    ]
                    if feasible:
                        self.assertGreaterEqual(
                            bound, max(sum(u[j] for j in s) for s in feasible)
                        )
                    # Independent direct evaluation of every dual breakpoint.
                    points = {Fraction(0)} | {
                        Fraction(u[j], -q[j])
                        for j in range(n) if j not in required and q[j] < 0
                    }
                    max_surplus = sum(q[j] for j in required) + sum(
                        max(0, q[j]) for j in range(n) if j not in required
                    )
                    if max_surplus < 0:
                        self.assertEqual(bound, -1)
                    else:
                        exact = min(
                            sum(u[j] + lam*q[j] for j in required)
                            + sum(max(0, u[j] + lam*q[j])
                                  for j in range(n) if j not in required)
                            for lam in points
                        )
                        self.assertEqual(bound, exact.numerator // exact.denominator)

    def test_closure_preserves_all_primary_optima(self):
        rng = random.Random(195)
        for _ in range(100):
            n = 7
            nb = [set() for _ in range(n)]
            for i, j in itertools.combinations(range(n), 2):
                if j == i + 1 or rng.random() < 0.2:
                    nb[i].add(j)
                    nb[j].add(i)
            u = [rng.randrange(11) for _ in range(n)]
            q = [rng.randrange(-8, 9) for _ in range(n)]
            edges = _profitable_closure_edges(nb, u, q)
            feasible = []
            for mask in range(1, 1 << n, 2):
                s = {i for i in range(n) if mask & (1 << i)}
                reached, todo = {0}, [0]
                while todo:
                    for j in nb[todo.pop()] & s - reached:
                        reached.add(j)
                        todo.append(j)
                if reached == s and sum(q[i] for i in s) >= 0:
                    feasible.append(s)
            if feasible:
                optimum = max(sum(u[i] for i in s) for s in feasible)
                for s in feasible:
                    if sum(u[i] for i in s) == optimum:
                        self.assertTrue(all(a not in s or b in s for a, b in edges))

    def test_integer_equality_is_preserved(self):
        # A conditional bound equal to L must not remove a primary tie.
        self.assertEqual(_lagrangian_conditional_bounds([10, 1], [1, -1], {0}),
                         [11, 11])
        huge = 10**20
        self.assertEqual(
            _lagrangian_conditional_bounds([huge, 1], [1, -2], {0}),
            [huge, -1],
        )

    def test_solve_equivalence_and_cap_bypass(self):
        for cap, exact in ((None, None), (2, None), (None, 2)):
            results = []
            for enabled in (False, True):
                result = solve_one_asu_cpsat(
                    [[1], [0, 2], [1, 3], [2]],
                    np.array([10, 1, 1, 1]), np.array([0, 50, 50, 50]),
                    np.array([5000]*4), 0.10, 10000, 0,
                    time_limit=5, workers=1, log=False,
                    configure_subsolvers=False, max_nodes=cap,
                    exact_nodes=exact,
                    use_profitable_component_closure=enabled,
                    use_lagrangian_variable_fixing=enabled,
                )
                self.assertIsNotNone(result)
                results.append((result.obj, result.sel_idx_local))
            self.assertEqual(results[0], results[1])


if __name__ == "__main__":
    unittest.main()
