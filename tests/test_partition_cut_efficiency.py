"""Progress-aware cut handoff, bounded sampling, and consistent warm starts."""
import contextlib
import io
import math
from pathlib import Path
import sys
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class PartitionCutEfficiencyTest(unittest.TestCase):
    def run_rounds(self, specs, **options):
        n = 7
        model = solver.cp_model.CpModel()
        x = [model.NewBoolVar(f'x_{i}') for i in range(n)]
        roots = [model.NewBoolVar(f'r_{i}') for i in range(n)]
        for i, root in enumerate(roots):
            model.Add(root == int(i == 0))
        model.Add(x[0] == 1)
        objective = model.NewIntVar(10, 70, 'objective')
        model.Add(objective == 10 * sum(x))
        model.Maximize(objective)
        count = model.NewIntVar(1, n, 'count')
        model.Add(count == sum(x))
        model.AddHint(objective, 10)
        model.AddHint(count, 1)
        for i, variable in enumerate(x):
            model.AddHint(variable, int(i == 0))
        for i, root in enumerate(roots):
            model.AddHint(root, int(i == 0))
        nb = [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)]
        weights = np.full(n, 10)
        seen, created, proof, bounds, refreshes = set(), [], [], [], []
        cancelled = [None]
        output = io.StringIO()
        test = self
        customized = options.pop('custom_hint', False)

        def valid(groups):
            return (len(groups) == 1 and 0 in groups[0]
                    and solver.component_ok(groups[0], weights, np.zeros(n, dtype=int),
                                            np.ones(n, dtype=int), .2, 1, nb))

        class FakeSolver:
            def __init__(self):
                self.parameters = solver.cp_model.CpSolver().parameters
                self.spec = specs[len(created)]
                self.selected = set(self.spec.get('final', [0]))
                self.running = False
                created.append(self)

            def BooleanValue(self, var):
                prefix, number = var.name.split('_')
                return int(number) in (self.selected if prefix == 'x' else {0})

            def Solve(self, supplied, callback):
                self.running = True
                before = str(supplied.Proto())
                callback.StopSearch = lambda: None
                for candidate in self.spec.get('candidates', []):
                    self.selected = set(candidate)
                    callback.BooleanValue = self.BooleanValue
                    callback.on_solution_callback()
                    test.assertEqual(str(supplied.Proto()), before,
                                     'cut callback must not mutate the live model')
                self.selected = set(self.spec.get('final', [0]))
                if self.spec.get('cancel'):
                    cancelled[0] = self.spec['cancel']
                self.running = False
                return self.spec.get('status', solver.cp_model.FEASIBLE)

            def BestObjectiveBound(self):
                return self.spec.get('bound', 100)

            def StatusName(self, status):
                return {solver.cp_model.UNKNOWN: 'UNKNOWN',
                        solver.cp_model.FEASIBLE: 'FEASIBLE',
                        solver.cp_model.OPTIMAL: 'OPTIMAL',
                        solver.cp_model.MODEL_INVALID: 'MODEL_INVALID'}[status]

            def StopSearch(self):
                pass

        def complete_hint(supplied, groups):
            self.assertFalse(any(engine.running for engine in created))
            refreshes.append([list(group) for group in groups])
            supplied.ClearHints()
            selected = set(groups[0])
            supplied.AddHint(objective, len(selected) * 10)
            supplied.AddHint(count, len(selected))
            for i, variable in enumerate(x):
                supplied.AddHint(variable, int(i in selected))
            for i, root in enumerate(roots):
                supplied.AddHint(root, int(i == 0))

        options.setdefault('max_rounds', len(specs))
        options.setdefault('bound_stall_only', True)
        options.setdefault('upper_bound_stall_rounds', 25)
        with patch.object(solver, '_new_asu_solver', side_effect=FakeSolver), \
                contextlib.redirect_stdout(output):
            result = solver._joint_connectivity_cut_pass(
                model, [x], [roots], nb, weights, [[0]], valid, math.inf, 46,
                lambda: cancelled[0], objective=objective, seen_cuts=seen,
                proof_out=proof, bound_out=bounds, log=True,
                refresh_hint=complete_hint if customized else None, **options)
        return SimpleNamespace(result=result, created=created, proof=proof, bounds=bounds,
                               seen=seen, output=output.getvalue(), model=model,
                               x=x, roots=roots, count=count, objective=objective,
                               refreshes=refreshes)

    def test_unknown_without_any_usable_bound_hands_off_after_three_rounds(self):
        specs = [dict(status=solver.cp_model.UNKNOWN, bound=0)] * 30
        result = self.run_rounds(specs)
        self.assertEqual(len(result.created), 3)
        self.assertEqual(result.result, ([[0]], 10, 'UNKNOWN'))
        self.assertEqual(result.bounds, [None])
        self.assertEqual(result.proof, [False])
        self.assertIn('stop_reason=NO_PROGRESS', result.output)
        self.assertIn('solver_branches=NA', result.output)

    def test_each_kind_of_real_progress_resets_the_handoff_counter(self):
        for progress in ('bound', 'incumbent', 'cut'):
            with self.subTest(progress=progress):
                specs = [dict(final=[0]) for _ in range(10)]
                if progress == 'bound':
                    for spec in specs[3:]:
                        spec['bound'] = 99
                elif progress == 'incumbent':
                    for spec in specs[3:]:
                        spec['final'] = [0, 1]
                else:
                    for i, spec in enumerate(specs):
                        spec['final'] = [0, 2] if i < 3 else [0, 4]
                result = self.run_rounds(specs)
                self.assertEqual(len(result.created), 7)
                self.assertIn('stop_reason=NO_PROGRESS', result.output)

    def test_adaptive_handoff_can_be_disabled_and_does_not_extend_other_modes(self):
        specs = [dict(status=solver.cp_model.UNKNOWN, bound=0)] * 8
        for disabled in (None, 0):
            with self.subTest(disabled=disabled):
                result = self.run_rounds(specs, no_progress_round_limit=disabled)
                self.assertEqual(len(result.created), 8)
                self.assertIn('stop_reason=ROUND_LIMIT', result.output)
        result = self.run_rounds(specs, bound_stall_only=False)
        self.assertEqual(len(result.created), 1)

    def test_callback_samples_and_final_response_all_supply_deduplicated_cuts(self):
        result = self.run_rounds([dict(candidates=[[0, 2], [0, 2], [0, 4]], final=[0, 6])])
        self.assertEqual(result.seen, {(0, 2, (2,)), (0, 4, (4,)), (0, 6, (6,))})
        self.assertIn('cut_samples=2', result.output)
        self.assertIn('cuts_added=3', result.output)
        # Every collected root-aware cut admits a fully connected assignment.
        for variable in result.x:
            result.model.Add(variable == 1)
        check = solver.cp_model.CpSolver()
        self.assertEqual(check.Solve(result.model), solver.cp_model.OPTIMAL)

    def test_cut_sample_and_row_budgets_keep_priority_for_final_response(self):
        spec = dict(candidates=[[0, 2], [0, 4]], final=[0, 6])
        result = self.run_rounds([spec], cut_sample_limit=1, cut_rows_per_round=2)
        self.assertEqual(result.seen, {(0, 2, (2,)), (0, 6, (6,))})
        result = self.run_rounds([spec], cut_rows_per_round=1)
        self.assertEqual(result.seen, {(0, 6, (6,))})
        result = self.run_rounds([spec], cut_limit=1)
        self.assertEqual(result.seen, {(0, 6, (6,))})

    def test_strict_callback_gain_replaces_stale_auxiliary_hints(self):
        result = self.run_rounds([dict(candidates=[[0, 1]], final=[0, 4])])
        self.assertEqual(result.result[:2], ([[0, 1]], 20))
        hint = result.model.Proto().solution_hint
        values = dict(zip(hint.vars, hint.values))
        self.assertNotIn(result.count.index, values)
        self.assertTrue(all(root.index not in values for root in result.roots))
        self.assertEqual(values[result.objective.index], 20)
        self.assertEqual(values[result.x[1].index], 1)
        self.assertEqual(len(hint.vars), len(values))
        check = solver.cp_model.CpSolver()
        check.parameters.fix_variables_to_their_hinted_value = True
        self.assertEqual(check.Solve(result.model), solver.cp_model.OPTIMAL)

    def test_caller_hint_refresh_runs_after_direct_and_repair_gains(self):
        result = self.run_rounds(
            [dict(candidates=[[0, 1]], final=[0, 4])], custom_hint=True,
            repair_candidate=lambda groups, best: [[0, 1, 2]])
        self.assertEqual(result.refreshes, [[[0, 1]], [[0, 1, 2]]])
        self.assertEqual(result.result[:2], ([[0, 1, 2]], 30))
        hint = result.model.Proto().solution_hint
        self.assertEqual(len(hint.vars), len(result.model.Proto().variables))
        check = solver.cp_model.CpSolver()
        check.parameters.fix_variables_to_their_hinted_value = True
        self.assertEqual(check.Solve(result.model), solver.cp_model.OPTIMAL)

    def test_proof_and_cancel_take_precedence_and_keep_valid_incumbents(self):
        result = self.run_rounds([dict(final=[0], bound=10, status=solver.cp_model.OPTIMAL)])
        self.assertEqual(result.proof, [True])
        self.assertNotIn('stop_reason=NO_PROGRESS', result.output)
        result = self.run_rounds([dict(candidates=[[0, 1], [0, 4]], final=[0, 4],
                                      cancel='SKIPPED')])
        self.assertEqual(result.result, ([[0, 1]], 20, 'SKIPPED'))
        self.assertEqual(result.seen, set())
        self.assertEqual(result.proof, [False])

    def test_row_cap_and_no_progress_cannot_mask_terminal_solver_outcomes(self):
        cases = [
            (dict(status=solver.cp_model.MODEL_INVALID, bound=0), 'MODEL_INVALID', False),
            (dict(candidates=[[0, 4]], final=[0, 1], bound=20,
                  status=solver.cp_model.OPTIMAL), 'OPTIMAL', True),
            (dict(candidates=[[0, 1], [0, 4]], final=[0, 4],
                  cancel='STOPPED'), 'STOPPED', False),
        ]
        for spec, status, proof in cases:
            with self.subTest(status=status):
                result = self.run_rounds([spec] * 5, cut_limit=1,
                                         no_progress_round_limit=1)
                self.assertEqual(len(result.created), 1)
                self.assertEqual(result.result[2], status)
                self.assertEqual(result.proof, [proof])
                self.assertLessEqual(len(result.seen), 1)
                self.assertNotIn('stop_reason=NO_PROGRESS', result.output)
        result = self.run_rounds([dict(final=[0, 4])] * 5, cut_limit=1)
        self.assertEqual(len(result.created), 1)
        self.assertEqual(result.result[2], 'CUT_LIMIT')
        self.assertEqual(len(result.seen), 1)


if __name__ == '__main__':
    unittest.main()
