"""Joint hybrid search is installed only after exact flows are built."""
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class JointFlowHybridTest(unittest.TestCase):
    def solve(self, workers=6, enabled=True, cut_only=False):
        snapshots = []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(instance, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            snapshots.append((model.Clone(), list(instance.parameters.subsolvers)))
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=capture):
            groups, status = solver._solve_regional_exchange(
                [[0], [3]], list(range(4)), [[1], [0, 2], [1, 3], [2]],
                np.array([10, 3, 2, 10]), np.array([0, 30, 30, 0]),
                np.array([10000] * 4), .2, 10000, 10, workers,
                max_nodes=2, tighten_model=True, use_joint_cuts=True,
                deterministic_ties=False, exact_flow_after_cuts=not cut_only,
                use_flow_capacity_hybrid_search=enabled)
        return groups, status, snapshots

    def test_hybrid_is_only_in_flow_phase_and_preserves_optimum(self):
        groups, status, snapshots = self.solve()
        baseline, baseline_status, _ = self.solve(enabled=False)
        self.assertEqual((status, baseline_status), ('OPTIMAL', 'OPTIMAL'))
        unemployment = [10, 3, 2, 10]
        self.assertEqual(sum(unemployment[i] for group in groups for i in group), 25)
        self.assertEqual(sum(unemployment[i] for group in baseline for i in group), 25)
        flow_models = []
        for model, workers in snapshots:
            proto = model.Proto()
            names = [v.name for v in proto.variables]
            if any(name.startswith('regional_flow_') for name in names):
                flow_models.append(model)
                self.assertIn('asu_flow_capacity_hybrid', workers)
                magnitudes = [name for name in names if name.startswith('regional_abs_flow_')]
                flows = [name for name in names if name.startswith('regional_flow_')]
                self.assertEqual(len(magnitudes), len(flows))
                self.assertGreater(len(magnitudes), 0)
                self.assertGreaterEqual(len(proto.search_strategy), 3)
                self.assertEqual(proto.search_strategy[0].variable_selection_strategy,
                                 solver.cp_model.CHOOSE_MAX_DOMAIN_SIZE)
                self.assertEqual(proto.search_strategy[1].domain_reduction_strategy,
                                 solver.cp_model.SELECT_MAX_VALUE)
            else:
                self.assertNotIn('asu_flow_capacity_hybrid', workers)
                self.assertEqual(len(proto.search_strategy), 0)
                self.assertFalse(any(name.startswith('regional_abs_flow_') for name in names))
        self.assertTrue(flow_models)
        self.assertGreater(len(snapshots), len(flow_models))

    def test_disabled_or_small_portfolio_adds_no_hybrid_overhead(self):
        for options in ({'enabled': False}, {'workers': 2}, {'cut_only': True}):
            with self.subTest(options=options):
                _, _, snapshots = self.solve(**options)
                self.assertTrue(snapshots)
                for model, workers in snapshots:
                    self.assertNotIn('asu_flow_capacity_hybrid', workers)
                    self.assertEqual(len(model.Proto().search_strategy), 0)
                    self.assertFalse(any(v.name.startswith('regional_abs_flow_')
                                         for v in model.Proto().variables))


if __name__ == '__main__':
    unittest.main()
