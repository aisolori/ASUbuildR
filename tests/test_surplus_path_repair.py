"""Priced repair joins disconnected targets without spending donor profit twice."""
import sys
import time
import unittest
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class SurplusPathRepairTest(unittest.TestCase):
    def repair(self, nb, profit, q, relaxed, *, cap=None, deadline=None, cancel=lambda: None):
        def valid(nodes):
            return (0 in nodes and sum(q[i] for i in nodes) >= 0
                    and (cap is None or len(nodes) <= cap)
                    and len(solver._connected_components(
                        nb, np.array([i in nodes for i in range(len(nb))]))) == 1)
        return solver._surplus_priced_path_repair(
            nb, profit, q, relaxed, [0], valid,
            time.monotonic() + 2 if deadline is None else deadline, cancel)

    def test_crosses_unaffordable_prefix_to_surplus_donor(self):
        # The connector alone is infeasible; the whole donor supplies capacity,
        # but has zero new-capture profit.
        result = self.repair([[1], [0, 2], [1]], [10, 4, 0], [5, -8, 20], [0, 2])
        self.assertEqual(result, [0, 1, 2])

    def test_rejects_infeasible_bundle_and_count_violation(self):
        nb = [[1], [0, 2], [1]]
        self.assertEqual(self.repair(nb, [10, 4, 9], [5, -30, 20], [0, 2]), [0])
        self.assertEqual(self.repair(nb, [10, 4, 9], [5, -8, 20], [0, 2], cap=2), [0])

    def test_shared_connector_counted_once(self):
        result = self.repair([[1], [0, 2, 3], [1], [1]],
                             [10, 4, 8, 9], [5, -8, 5, 5], [0, 2, 3])
        self.assertEqual(result, [0, 1, 2, 3])

    def test_pricing_prefers_affordable_route_to_same_target(self):
        # Two equal-hop routes. A high-value deficit elsewhere makes surplus
        # scarce; the cheap connector is feasible, the other is not.
        nb = [[1, 2], [0, 3], [0, 3], [1, 2], []]
        result = self.repair(nb, [10, 1, 1, 20, 10000],
                             [5, -20, -2, 10, -100], [0, 3])
        self.assertEqual(result, [0, 2, 3])

    def test_cancellation_during_path_search_keeps_valid_incumbent(self):
        calls = [0]
        def cancel():
            calls[0] += 1
            return 'SKIPPED_FEASIBLE' if calls[0] >= 4 else None
        self.assertEqual(self.repair([[1], [0, 2], [1]], [10, 4, 0],
                                    [5, -8, 20], [0, 2], cancel=cancel), [0])

    def test_deadline_cancellation_and_unreachable_target_preserve_incumbent(self):
        nb = [[1], [0], []]
        for options in ({}, {'deadline': 0}, {'cancel': lambda: 'STOPPED_FEASIBLE'}):
            self.assertEqual(self.repair(nb, [10, 1, 20], [5, 0, 10], [0, 2], **options), [0])

    def test_cut_pass_publishes_repair_and_keeps_connectivity_cuts(self):
        nb = [[1], [0, 2], [1]]
        profit = np.array([10, 1, 20])
        model = solver.cp_model.CpModel()
        x = [model.NewBoolVar(f'x{i}') for i in range(3)]
        model.Add(x[0] == 1)
        # The relaxed optimum excludes the expensive connector.
        model.Add(5*x[0] - 8*x[1] + 20*x[2] >= 0)
        objective = model.NewIntVar(10, 31, 'objective')
        model.Add(objective == sum(int(p)*v for p, v in zip(profit, x)))
        model.Maximize(objective)
        reports = []
        from unittest.mock import patch
        real_solve = solver.cp_model.CpSolver.Solve

        def disconnected(engine, model, *args):
            engine.BooleanValue = lambda var: var.Index() in (x[0].Index(), x[2].Index(), roots[0].Index())
            engine.BestObjectiveBound = lambda: 31
            return solver.cp_model.FEASIBLE

        roots = [model.NewConstant(1), model.NewConstant(0), model.NewConstant(0)]
        def valid(groups):
            return groups in ([[0]], [[0, 1, 2]])
        def repair(groups, best):
            return [solver._surplus_priced_path_repair(
                nb, profit, [5, -8, 20], groups[0], best[0],
                lambda nodes: valid([nodes]), time.monotonic()+2, lambda: None)]
        proof = []
        with patch.object(solver.cp_model.CpSolver, 'Solve', new=disconnected):
            best, value, _ = solver._joint_connectivity_cut_pass(
                model, [x], [roots], nb, profit, [[0]], valid,
                time.monotonic()+2, 1, lambda: None, max_rounds=1,
                objective=objective, repair_candidate=repair, proof_out=proof,
                report=lambda nodes, value: reports.append((nodes, value)))
        self.assertEqual((best, value), ([[0, 1, 2]], 31))
        self.assertEqual(reports, [([0, 1, 2], 31)])
        self.assertEqual(proof, [False])  # Heuristic success is not a proof.
        engine = solver.cp_model.CpSolver()
        self.assertEqual(real_solve(engine, model), solver.cp_model.OPTIMAL)
        self.assertEqual(engine.Value(objective), 31)


if __name__ == '__main__':
    unittest.main()
