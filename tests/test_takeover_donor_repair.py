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
    def test_salvages_best_disconnected_piece_without_taking_other_asus(self):
        # Tract 2 is occupied by another ASU and cannot reconnect the donor.
        repaired = _repair_takeover_donor(
            [0, 1], [], [[2], [2], [0, 1]],
            np.array([20, 40, 1000]), np.array([80, 160, 0]),
            np.array([10000, 10000, 10000]), .1, 10000, 2, 1,
            solve_kwargs={"configure_subsolvers": False},
        )
        self.assertEqual(repaired, [1])

    def test_partial_repair_drops_unaffordable_survivor_and_adds_population(self):
        # Keeping tract 1 makes the rate unattainable. Tracts 0+2 qualify.
        repaired = _repair_takeover_donor(
            [0, 1], [2], [[1, 2], [0], [0]],
            np.array([20, 0, 20]), np.array([80, 10000, 80]),
            np.array([6000, 1000, 6000]), .1, 10000, 2, 1,
            solve_kwargs={"configure_subsolvers": False},
        )
        self.assertEqual(repaired, [0, 2])

    def test_partial_repair_requires_donor_overlap(self):
        repaired = _repair_takeover_donor(
            [0], [1], [[], []], np.array([0, 100]), np.array([100, 0]),
            np.array([1000, 10000]), .1, 10000, 2, 1,
            solve_kwargs={"configure_subsolvers": False},
        )
        self.assertEqual(repaired, [])

    def test_timeout_preserves_a_valid_piece(self):
        with patch("asu_cpsat._search_unassigned_asu", return_value=([], "UNKNOWN")):
            repaired = _repair_takeover_donor(
                [0, 1], [], [[], []], np.array([20, 40]), np.array([80, 160]),
                np.array([10000, 10000]), .1, 10000, 2, 1,
            )
        self.assertEqual(repaired, [1])

    def test_partial_repair_respects_exact_count_over_cap(self):
        repaired = _repair_takeover_donor(
            [0, 1], [2], [[1, 2], [0], [0]],
            np.array([20, 0, 20]), np.array([80, 10000, 80]),
            np.array([10000, 1000, 1000]), .1, 10000, 2, 1,
            solve_kwargs={"configure_subsolvers": False, "max_nodes": 1, "exact_nodes": 2},
        )
        self.assertEqual(repaired, [0, 2])

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
