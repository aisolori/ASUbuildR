"""Partition polishing prioritizes lowest unemployment, including after merges."""
from pathlib import Path
import sys
import unittest

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class PolishUnemploymentOrderTest(unittest.TestCase):
    def test_unemployment_order_recalculates_after_merge(self):
        ids = np.array([1, 1, 2, 3, 0, -1])
        u = np.array([10, 10, 15, 10, 999, 999])
        self.assertEqual(solver._polish_asu_order(ids, u), [3, 2, 1])
        ids[ids == 3] = 2
        self.assertEqual(solver._polish_asu_order(ids, u), [1, 2])

    def test_equal_unemployment_uses_id(self):
        ids = np.array([3, 2, 1])
        u = np.array([20, 20, 10])
        self.assertEqual(solver._polish_asu_order(ids, u), [1, 2, 3])

    def test_sums_all_member_tracts_not_average_or_largest_tract(self):
        ids = np.array([1, 1, 2, 3, 0, -1])
        u = np.array([10, 10, 15, 25, 0, 0])
        self.assertEqual(solver._polish_asu_order(ids, u), [2, 1, 3])

    def test_recalculates_after_membership_changes(self):
        ids = np.array([1, 2, 0])
        u = np.array([10, 20, 30])
        self.assertEqual(solver._polish_asu_order(ids, u), [1, 2])
        ids[2] = 1
        self.assertEqual(solver._polish_asu_order(ids, u), [2, 1])

    def test_sum_avoids_integer_overflow(self):
        ids = np.array([1, 1, 2])
        self.assertEqual(solver._polish_asu_order(
            ids, np.array([2**62, 2**62, 1], dtype=np.int64)), [2, 1])

    def test_empty_assignments(self):
        ids = np.array([0, -1])
        self.assertEqual(solver._polish_asu_order(ids, ids), [])


if __name__ == "__main__":
    unittest.main()
