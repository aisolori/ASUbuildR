#!/usr/bin/env python3
"""Regression tests for consistent CP-SAT portfolio configuration."""

import os
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(
    0,
    os.path.join(os.path.dirname(__file__), "..", "inst", "python"),
)

import asu_cpsat  # noqa: E402


class SolverPortfolioTest(unittest.TestCase):
    def test_full_solver_patterns_do_not_repeat_names(self):
        for workers in (1, 2, 6, 16, 32, 64):
            names = asu_cpsat._asu_full_subsolvers(workers)
            self.assertEqual(len(names), len(set(names)))

    def test_feasibility_portfolio_uses_only_unique_applicable_workers(self):
        solver = asu_cpsat.cp_model.CpSolver()
        asu_cpsat._configure_asu_solver_portfolio(
            solver.parameters, 64, has_objective=False
        )
        names = list(solver.parameters.subsolvers)
        self.assertEqual(len(names), len(set(names)))
        self.assertNotIn("lb_tree_search", names)
        self.assertNotIn("objective_lb_search_max_lp", names)
        self.assertNotIn("pseudo_costs", names)
        self.assertEqual(solver.parameters.num_full_subsolvers, len(names))

    def test_one_worker_explicitly_uses_asu_portfolio(self):
        solver = asu_cpsat.cp_model.CpSolver()

        asu_cpsat._configure_asu_solver_portfolio(solver.parameters, 1)

        self.assertEqual(list(solver.parameters.subsolvers), ["portfolio_max_lp"])
        self.assertNotIn("default_lp", solver.parameters.subsolvers)
        self.assertEqual(solver.parameters.num_full_subsolvers, 1)

    def test_connectivity_free_screen_installs_custom_portfolio(self):
        with patch(
            "asu_cpsat._configure_asu_solver_portfolio",
            wraps=asu_cpsat._configure_asu_solver_portfolio,
        ) as configure:
            status = asu_cpsat._connectivity_free_feasibility(
                np.array([20, 20], dtype=np.int64),
                np.array([80, 80], dtype=np.int64),
                np.array([6000, 6000], dtype=np.int64),
                0.10,
                10_000,
                seconds=1.0,
                workers=1,
            )

        configure.assert_called_once()
        self.assertIn(status, {"OPTIMAL", "FEASIBLE"})


if __name__ == "__main__":
    unittest.main()
