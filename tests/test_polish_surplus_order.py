"""Polishing prioritizes highest total unemployment, including after a merge."""
from pathlib import Path
import sys
import unittest

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class PolishUnemploymentOrderTest(unittest.TestCase):
    def test_supernodes_use_least_unemployment_and_recalculate_after_merge(self):
        ids = np.array([1, 1, 2, 3, 0, -1])
        u = np.array([10, 10, 15, 10, 999, 999])
        emp = np.zeros(6, dtype=int)
        self.assertEqual(solver._polish_asu_order(
            ids, u, emp, .1, least_unemployment_first=True), [3, 2, 1])
        ids[ids == 3] = 2
        self.assertEqual(solver._polish_asu_order(
            ids, u, emp, .1, least_unemployment_first=True), [1, 2])

    def test_supernode_equal_unemployment_uses_id(self):
        ids = np.array([3, 2, 1])
        u = np.array([20, 20, 10])
        emp = np.array([0, 100, 10])
        self.assertEqual(solver._polish_asu_order(
            ids, u, emp, .1, least_unemployment_first=True), [1, 2, 3])

    def test_total_unemployment_orders_asus_and_recalculates_after_merge(self):
        # At tau=.1, q = 9*u - E. ASU 1 has more unemployed people
        # but its negative-surplus tract reduces total q to 30.
        ids = np.array([1, 1, 2, 3, 0, -1])
        u = np.array([50, 50, 20, 10, 999, 999])
        emp = np.array([100, 770, 0, 10, 0, 0])
        self.assertEqual(solver._polish_asu_order(ids, u, emp, .1), [1, 2, 3])
        # Recalculate from the updated membership after merging 2 and 3.
        ids[ids == 3] = 2
        self.assertEqual(solver._polish_asu_order(ids, u, emp, .1), [1, 2])

    def test_equal_unemployment_uses_id_despite_different_surplus(self):
        ids = np.array([3, 2, 1])
        u = np.array([20, 20, 10])
        emp = np.array([0, 100, 10])
        self.assertEqual(solver._polish_asu_order(ids, u, emp, .1), [2, 3, 1])

    def test_empty_assignments(self):
        ids = np.array([0, -1])
        self.assertEqual(solver._polish_asu_order(ids, ids, ids, .1), [])


if __name__ == "__main__":
    unittest.main()
