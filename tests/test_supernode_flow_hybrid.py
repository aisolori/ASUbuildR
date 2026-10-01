"""Supernode flow uses hybrid branching with full donor economics."""
import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class SupernodeFlowHybridTest(unittest.TestCase):
    def solve(self, workers=6, enabled=True, configure=True, ties=True,
              prefix_size=None):
        snapshots = []
        real_solve = solver.cp_model.CpSolver.Solve
        real_groups = solver._asu_flow_capacity_hybrid_groups
        output = io.StringIO()

        def capture(engine, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            snapshots.append((model.Clone(), list(engine.parameters.subsolvers), bool(args)))
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        def groups(*args, **kwargs):
            if prefix_size is not None:
                kwargs['max_prefix'] = prefix_size
            return real_groups(*args, **kwargs)

        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=capture),
              patch.object(solver, '_asu_flow_capacity_hybrid_groups', side_effect=groups),
              contextlib.redirect_stdout(output)):
            # A two-tract donor pays for the bridge and far tract; its own
            # unemployment must never inflate the captured-unemployment gain.
            result = solver._solve_supernode_polish(
                [[1], [0, 2], [1, 3], [2, 4], [3]],
                np.array([10, 5, 10, 10, 20]), np.array([0, 80, 0, 0, 120]),
                np.array([10000] * 5), .2, 10000, 0, 10, workers,
                assignments=np.array([1, -1, 2, 2, -1]), asu_number=1, hint=[0],
                configure_subsolvers=configure, deterministic_ties=ties,
                use_flow_capacity_hybrid_search=enabled, log=True)
        return result, snapshots, output.getvalue()

    @staticmethod
    def strategy_names(proto, strategy):
        return [proto.variables[expr.vars[0]].name for expr in strategy.exprs]

    def test_primary_flow_uses_hybrid_and_preserves_donor_accounting(self):
        result, snapshots, log = self.solve()
        baseline, _, _ = self.solve(enabled=False)
        self.assertEqual((result.obj, result.status), (35, 'OPTIMAL'))
        self.assertEqual((baseline.obj, baseline.status), (35, 'OPTIMAL'))
        self.assertEqual(result.sel_idx_local, list(range(5)))
        primary_flows = []
        cut_models = []
        for model, workers, has_callback in snapshots:
            proto = model.Proto()
            names = [v.name for v in proto.variables]
            flows = [name for name in names if name.startswith('polish_flow_')]
            magnitudes = [name for name in names if name.startswith('polish_abs_flow_')]
            if not flows:
                cut_models.append(model)
                self.assertFalse(magnitudes)
                self.assertEqual(len(proto.search_strategy), 0)
                self.assertNotIn('asu_flow_capacity_hybrid', workers)
                continue
            self.assertEqual(len(magnitudes), len(flows))
            self.assertEqual(len(flows), 3)  # Internal donor edge was contracted.
            if has_callback:
                primary_flows.append(model)
                self.assertIn('asu_flow_capacity_hybrid', workers)
                flow, select, reject = proto.search_strategy
                self.assertEqual(flow.variable_selection_strategy, solver.cp_model.CHOOSE_MAX_DOMAIN_SIZE)
                self.assertEqual(flow.domain_reduction_strategy, solver.cp_model.SELECT_MIN_VALUE)
                self.assertEqual(select.domain_reduction_strategy, solver.cp_model.SELECT_MAX_VALUE)
                # Donor surplus is +80; treating donor objective profit (zero)
                # as its unemployment would incorrectly put it after the root.
                self.assertEqual(self.strategy_names(proto, select),
                                 ['polish_supernode_3', 'polish_supernode_0'])
                self.assertEqual(self.strategy_names(proto, reject),
                                 ['polish_supernode_1', 'polish_supernode_2'])
                hints = dict(zip(proto.solution_hint.vars, proto.solution_hint.values))
                for i, var in enumerate(proto.variables):
                    if var.name.startswith('polish_abs_flow_'):
                        signed_index = names.index(var.name.replace('polish_abs_flow_', 'polish_flow_'))
                        self.assertEqual(hints[i], abs(hints[signed_index]))
        self.assertTrue(primary_flows)
        self.assertTrue(cut_models)
        self.assertIn('FINAL_POLISH_SUPERNODES_HYBRID asu=1 cycle=1 worker=asu_flow_capacity_hybrid', log)
        self.assertIn('max_cut_rounds=50 upper_bound_stall_limit=5', log)

    def test_distance_tail_uses_quotient_graph_and_covers_remaining_variables(self):
        result, snapshots, _ = self.solve(prefix_size=1)
        self.assertEqual((result.obj, result.status), (35, 'OPTIMAL'))
        proto = next(model.Proto() for model, workers, callback in snapshots
                     if callback and 'asu_flow_capacity_hybrid' in workers)
        tail = proto.search_strategy[len(proto.search_strategy) - 1]
        names = self.strategy_names(proto, tail)
        # The far tract (node 2) is three quotient edges from the root.
        self.assertEqual(names[0], 'polish_supernode_2')
        all_names = [name for strategy in proto.search_strategy
                     for name in self.strategy_names(proto, strategy)]
        expected = [var.name for var in proto.variables
                    if var.name.startswith(('polish_supernode_', 'polish_abs_flow_'))]
        self.assertCountEqual(all_names, expected)

    def test_disabled_small_or_default_portfolio_has_no_hybrid_overhead(self):
        for options in ({'enabled': False}, {'workers': 5}, {'configure': False}):
            with self.subTest(options=options):
                result, snapshots, log = self.solve(**options)
                self.assertEqual((result.obj, result.status), (35, 'OPTIMAL'))
                for model, workers, _ in snapshots:
                    self.assertNotIn('asu_flow_capacity_hybrid', workers)
                    self.assertEqual(len(model.Proto().search_strategy), 0)
                    self.assertFalse(any(v.name.startswith('polish_abs_flow_')
                                         for v in model.Proto().variables))
                self.assertNotIn('FINAL_POLISH_SUPERNODES_HYBRID ', log)

    def test_cut_proof_can_still_skip_flow(self):
        result, snapshots, log = self.solve(ties=False)
        self.assertEqual((result.obj, result.status), (35, 'OPTIMAL'))
        self.assertTrue(snapshots)
        for model, workers, _ in snapshots:
            self.assertNotIn('asu_flow_capacity_hybrid', workers)
            self.assertEqual(len(model.Proto().search_strategy), 0)
            self.assertFalse(any(v.name.startswith(('polish_flow_', 'polish_abs_flow_'))
                                 for v in model.Proto().variables))
        self.assertNotIn('FINAL_POLISH_SUPERNODES_FLOW ', log)


if __name__ == '__main__':
    unittest.main()
