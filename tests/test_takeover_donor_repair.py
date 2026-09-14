#!/usr/bin/env python3
"""Regression tests for donor repair during the final takeover pass."""

import os
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(
    0,
    os.path.join(os.path.dirname(__file__), "..", "inst", "python"),
)

from asu_cpsat import _repair_takeover_donor  # noqa: E402


class TakeoverDonorRepairTest(unittest.TestCase):
    def test_adds_available_tract(self):
        neighbors = [[1, 2], [0], [0]]
        unemployed = np.array([20, 20, 500], dtype=np.int64)
        employed = np.array([80, 80, 500], dtype=np.int64)
        population = np.array([6000, 6000, 20_000], dtype=np.int64)

        repaired = _repair_takeover_donor(
            donor_remaining=[0],
            available_nodes=[1],
            nb=neighbors,
            u=unemployed,
            E=employed,
            P=population,
            tau=0.10,
            pop_thresh=10_000,
            time_limit=2.0,
            workers=1,
            stable_values=["0", "1", "2"],
            solve_kwargs={
                "configure_subsolvers": False,
                "deterministic_ties": False,
            },
        )

        self.assertEqual(repaired, [0, 1])
        self.assertNotIn(2, repaired)

    def test_attempted_for_still_valid_donor(self):
        neighbors = [[1], [0]]
        unemployed = np.array([20, 20], dtype=np.int64)
        employed = np.array([80, 80], dtype=np.int64)
        population = np.array([10_000, 1000], dtype=np.int64)

        with patch("asu_cpsat.solve_one_asu_cpsat", return_value=None) as solve:
            repaired = _repair_takeover_donor(
                donor_remaining=[0],
                available_nodes=[1],
                nb=neighbors,
                u=unemployed,
                E=employed,
                P=population,
                tau=0.10,
                pop_thresh=10_000,
                time_limit=2.0,
                workers=1,
            )

        solve.assert_called_once()
        self.assertEqual(repaired, [0])


if __name__ == "__main__":
    unittest.main()
