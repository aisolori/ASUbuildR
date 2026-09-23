"""Proof reuse and exact-scout/cut-relaxation sequencing for single-ASU solves."""
import contextlib
import io
import math
from pathlib import Path
import sys
import threading
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class ScoutBeforeCutsTest(unittest.TestCase):
    def run_window(self, *, skip_first_scout=False, economic_scale=1, **options):
        snapshots, output = [], io.StringIO()
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(instance, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            snapshots.append((model.Clone(), instance.parameters.max_time_in_seconds))
            instance.parameters.log_to_stdout = False
            if skip_first_scout and len(snapshots) == 1:
                return solver.cp_model.UNKNOWN
            return real_solve(instance, model, *args, **kwargs)

        defaults = dict(time_limit=10, workers=2, log=True, hint=[0],
                        deterministic_ties=False, configure_subsolvers=False,
                        use_small_root_separators=False,
                        use_profitable_component_closure=False)
        defaults.update(options)
        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=capture),
              contextlib.redirect_stdout(output)):
            result = solver.solve_one_asu_cpsat(
                [[1], [0, 2], [1, 3], [2]], np.array([10, 1, 5, 2]) * economic_scale,
                np.array([0, 50, 0, 30]) * economic_scale, np.array([10000]*4),
                .2, 10000, 0, **defaults)
        return result, snapshots, output.getvalue()

    def has_flow(self, model):
        return any(v.name.startswith('f_') for v in model.Proto().variables)

    def test_scout_proves_optimum_before_any_cut_solve(self):
        result, models, log = self.run_window()
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertEqual(len(models), 1)
        self.assertTrue(self.has_flow(models[0][0]))
        self.assertLessEqual(models[0][1], 1.0)
        self.assertIn('scout: before cut pass', log)
        self.assertIn('skipping cuts and main', log)
        self.assertNotIn('[cut-pass] round', log)

    def test_early_scout_supports_existing_connectivity_formulations(self):
        for options in ({'use_signed_flow': False}, {'use_arborescence': True}):
            with self.subTest(options=options):
                result, _, _ = self.run_window(**options)
                self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))

    def test_nonoptimal_scout_incumbent_is_retained_and_improved_by_cuts(self):
        real_solve = solver.cp_model.CpSolver.Solve
        calls = []

        def feasible_scout(instance, model, *args, **kwargs):
            calls.append(model.Clone())
            if len(calls) == 1:
                # Solve a restricted clone to simulate a weak early incumbent.
                restricted = model.Clone()
                for i, var in enumerate(restricted.Proto().variables):
                    if var.name in ('x_1', 'x_2', 'x_3'):
                        restricted.Add(restricted.GetIntVarFromProtoIndex(i) == 0)
                real_solve(instance, restricted, *args, **kwargs)
                return solver.cp_model.FEASIBLE
            return real_solve(instance, model, *args, **kwargs)

        # The fake scout bound must remain a valid bound on the unrestricted
        # problem, rather than the deliberately restricted incumbent model.
        real_bound = solver.cp_model.CpSolver.BestObjectiveBound
        def bound(instance):
            return 18 if len(calls) == 1 else real_bound(instance)

        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=feasible_scout),
              patch.object(solver.cp_model.CpSolver, 'BestObjectiveBound', new=bound)):
            result, _, log = self.run_window()
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertIn('[cut-pass] round', log)

    def test_unknown_scout_falls_back_to_flow_free_cuts_and_reuses_proof(self):
        result, models, log = self.run_window(skip_first_scout=True, deterministic_ties=True)
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertTrue(self.has_flow(models[0][0]))
        relaxation_indices = [i for i, (model, _) in enumerate(models) if not self.has_flow(model)]
        self.assertTrue(relaxation_indices)
        self.assertEqual(relaxation_indices[0], 1)
        self.assertTrue(all(self.has_flow(model) for model, _ in models[max(relaxation_indices)+1:]))
        self.assertIn('cut-pass: primary optimum proved', log)
        self.assertEqual(log.count('scout: before cut pass'), 1)
        # Exact model keeps its original variables and gains transferred cuts.
        final = models[-1][0]
        self.assertEqual(len(final.Proto().variables), len(models[0][0].Proto().variables))
        self.assertGreater(len(final.Proto().constraints), len(models[0][0].Proto().constraints))

    def test_cut_optimum_with_ties_does_not_run_a_late_scout(self):
        result, models, log = self.run_window(scout_before_cuts=False, deterministic_ties=True)
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertFalse(self.has_flow(models[0][0]))
        self.assertTrue(self.has_flow(models[-1][0]))
        self.assertNotIn('scout:', log)
        self.assertIn('primary optimum proved', log)

    def test_cut_callback_stops_on_new_cuts_and_preserves_connected_incumbents(self):
        real_solve = solver.cp_model.CpSolver.Solve
        for final_selected in ({0, 2, 3}, {0, 1, 2}):
            cuts, requests = [], []

            def discovered(instance, model, *args, **kwargs):
                if self.has_flow(model) or cuts:
                    return real_solve(instance, model, *args, **kwargs)
                cuts.append(model.Clone())
                callback = args[0]
                self.assertTrue(math.isinf(instance.parameters.max_time_in_seconds))
                before = len(model.Proto().constraints)
                callback.StopSearch = lambda: requests.append(True)
                # Preserve connected improvements even when parallel shutdown
                # leaves a different final incumbent than the violating one.
                for selected in ({0, 1, 2}, {0, 2, 3}):
                    callback.BooleanValue = lambda var: int(var.name.split('_')[1]) in selected
                    callback.on_solution_callback()
                    self.assertEqual(len(model.Proto().constraints), before)
                    self.assertEqual(len(requests), int(selected == {0, 2, 3}))
                instance.BooleanValue = lambda var: int(var.name.split('_')[1]) in final_selected
                instance.ObjectiveValue = lambda: 16 if final_selected == {0, 1, 2} else 17
                instance.BestObjectiveBound = lambda: 18
                return solver.cp_model.FEASIBLE

            with self.subTest(final=final_selected), \
                    patch.object(solver.cp_model.CpSolver, 'Solve', new=discovered):
                result, models, log = self.run_window(scout_before_cuts=False, deterministic_ties=True)
            self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
            self.assertEqual(requests, [True])
            self.assertIn('round_end=NEW_CONNECTIVITY_CUTS', log)
            self.assertTrue(any(self.has_flow(model) for model, _ in models))
            self.assertGreater(len(models[1][0].Proto().constraints), len(cuts[0].Proto().constraints))

    def test_cut_callback_preserves_improvement_on_skip(self):
        with TemporaryDirectory() as directory:
            flag = Path(directory) / 'skip'

            def interrupted(instance, model, *args, **kwargs):
                self.assertFalse(self.has_flow(model))
                callback = args[0]
                before = len(model.Proto().constraints)
                callback.BooleanValue = lambda var: var.name in ('x_0', 'x_1', 'x_2')
                callback.StopSearch = lambda: None
                callback.on_solution_callback()
                flag.touch()
                callback.on_solution_callback()
                self.assertEqual(len(model.Proto().constraints), before)
                return solver.cp_model.UNKNOWN

            with patch.object(solver.cp_model.CpSolver, 'Solve', new=interrupted):
                result, models, _ = self.run_window(scout_before_cuts=False, skip_flag_path=str(flag))
            self.assertEqual((result.obj, result.status), (16, 'SKIPPED_FEASIBLE'))
            self.assertEqual(len(models), 1)
            self.assertFalse(flag.exists())

    def test_single_cut_pass_stops_after_25_unchanged_upper_bounds(self):
        real_solve = solver.cp_model.CpSolver.Solve
        cut_models = []

        def repeat_relaxation(instance, model, *args, **kwargs):
            if not self.has_flow(model):
                cut_models.append(model.Clone())
                # Simulate repeated disconnected search responses with a
                # fixed valid upper bound, independently of separator effects.
                instance.BooleanValue = lambda var: var.name in ('x_0', 'x_2', 'x_3')
                instance.BestObjectiveBound = lambda: 18
                return solver.cp_model.FEASIBLE
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=repeat_relaxation):
            result, _, log = self.run_window(scout_before_cuts=False)
        self.assertEqual(len(cut_models), 26)
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertIn('upper_bound_stall=25/25', log)
        self.assertIn('stop_reason=UPPER_BOUND_STALL', log)

    def test_valid_unemployment_stall_stops_at_50_and_resets_on_callback_gain(self):
        real_solve = solver.cp_model.CpSolver.Solve
        for connected, gain_round in ((False, None), (True, None), (False, 25)):
            with self.subTest(connected=connected, gain_round=gain_round):
                rounds = []

                def response(instance, model, *args, **kwargs):
                    if self.has_flow(model):
                        return real_solve(instance, model, *args, **kwargs)
                    rounds.append(model.Clone())
                    if len(rounds) == gain_round:
                        callback = args[0]
                        callback.BooleanValue = lambda var: var.name in ('x_0', 'x_1', 'x_2')
                        callback.on_solution_callback()
                    members = ('x_0',) if connected else ('x_0', 'x_2', 'x_3')
                    instance.BooleanValue = lambda var: var.name in members
                    instance.ObjectiveValue = lambda: 1000 if connected else 1700
                    instance.BestObjectiveBound = lambda: 1800 - len(rounds)
                    return solver.cp_model.FEASIBLE

                with patch.object(solver.cp_model.CpSolver, 'Solve', new=response):
                    result, models, log = self.run_window(scout_before_cuts=False, economic_scale=100)
                self.assertEqual(len(rounds), 50 + (gain_round or 0))
                self.assertEqual((result.obj, result.status), (1600, 'OPTIMAL'))
                self.assertTrue(any(self.has_flow(model) for model, _ in models))
                self.assertIn('valid_unemp_stall=50/50', log)
                self.assertIn('stop_reason=VALID_UNEMP_STALL', log)
                self.assertIn('upper_bound_stall=0/25', log)

    def test_cut_time_is_unlimited_and_does_not_consume_exact_solve_budget(self):
        real_solve = solver.cp_model.CpSolver.Solve
        real_clock = solver.time.monotonic
        elapsed = [0.0]
        cut_limits, exact_limits = [], []

        def long_round(instance, model, *args, **kwargs):
            if not self.has_flow(model):
                cut_limits.append(instance.parameters.max_time_in_seconds)
                elapsed[0] += 120.0  # Every round exceeds the old entire budget.
                instance.BooleanValue = lambda var: var.name in ('x_0', 'x_2', 'x_3')
                instance.BestObjectiveBound = lambda: 18
                return solver.cp_model.FEASIBLE
            exact_limits.append(instance.parameters.max_time_in_seconds)
            return real_solve(instance, model, *args, **kwargs)

        with (patch.object(solver.time, 'monotonic', side_effect=lambda: real_clock() + elapsed[0]),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=long_round)):
            result, _, log = self.run_window(scout_before_cuts=False)
        self.assertEqual(len(cut_limits), 26)
        self.assertTrue(all(math.isinf(limit) for limit in cut_limits))
        self.assertTrue(exact_limits)
        self.assertTrue(all(0 < limit <= 10 for limit in exact_limits))
        self.assertGreater(exact_limits[0], 8)
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertIn('time_limit=none', log)
        self.assertIn('stop_reason=UPPER_BOUND_STALL', log)

    def test_cut_upper_bound_improvement_resets_stall(self):
        real_solve = solver.cp_model.CpSolver.Solve
        bounds = [1700] * 20 + [1650] * 26
        cut_count = [0]

        def improving_round(instance, model, *args, **kwargs):
            if not self.has_flow(model):
                bound = bounds[cut_count[0]]
                cut_count[0] += 1
                instance.BooleanValue = lambda var: var.name in ('x_0', 'x_1')
                instance.BestObjectiveBound = lambda: bound
                return solver.cp_model.FEASIBLE
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=improving_round):
            result, _, log = self.run_window(scout_before_cuts=False, economic_scale=100)
        self.assertEqual(cut_count[0], 46)
        self.assertEqual(result.obj, 1600)
        self.assertIn('upper_bound=1650 upper_bound_stall=0/25', log)
        self.assertIn('stop_reason=UPPER_BOUND_STALL', log)

    def test_untimed_cut_round_honors_stop_and_skip_watchdog(self):
        for kind in ('stop', 'skip'):
            with self.subTest(kind=kind), TemporaryDirectory() as folder:
                flag = Path(folder) / kind
                stopped = threading.Event()

                def interrupt(instance, model, *args, **kwargs):
                    self.assertFalse(self.has_flow(model))
                    self.assertTrue(math.isinf(instance.parameters.max_time_in_seconds))
                    flag.touch()
                    self.assertTrue(stopped.wait(2), 'cut watchdog did not interrupt')
                    return solver.cp_model.UNKNOWN

                with (patch.object(solver.cp_model.CpSolver, 'Solve', new=interrupt),
                      patch.object(solver.cp_model.CpSolver, 'StopSearch', side_effect=stopped.set)):
                    result, models, _ = self.run_window(
                        scout_before_cuts=False, **{kind + '_flag_path': str(flag)})
                self.assertEqual(len(models), 1)
                self.assertEqual(result.obj, 10)
                self.assertEqual(result.status, 'STOPPED_FEASIBLE' if kind == 'stop' else 'SKIPPED_FEASIBLE')
                self.assertEqual(flag.exists(), kind == 'stop')

    def test_connected_feasible_cut_result_waits_for_bound_stall(self):
        real_solve = solver.cp_model.CpSolver.Solve
        cut_count = [0]

        def connected_round(instance, model, *args, **kwargs):
            if not self.has_flow(model):
                cut_count[0] += 1
                instance.BooleanValue = lambda var: var.name == 'x_0'
                instance.ObjectiveValue = lambda: 10
                instance.BestObjectiveBound = lambda: 18
                return solver.cp_model.FEASIBLE
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=connected_round):
            result, _, log = self.run_window(scout_before_cuts=False)
        self.assertEqual(cut_count[0], 26)
        self.assertEqual(result.obj, 16)
        self.assertIn('stop_reason=UPPER_BOUND_STALL', log)

    def test_valid_stall_precedes_total_round_limit_when_only_bound_improves(self):
        real_solve = solver.cp_model.CpSolver.Solve
        cuts = []

        def improving_bound(instance, model, *args, **kwargs):
            if not self.has_flow(model):
                cuts.append(model.Clone())
                instance.BooleanValue = lambda var: var.name in ('x_0', 'x_1')
                instance.BestObjectiveBound = lambda: 1710 - len(cuts)
                return solver.cp_model.FEASIBLE
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=improving_bound):
            result, models, log = self.run_window(scout_before_cuts=False, economic_scale=100)
        self.assertEqual(len(cuts), 50)
        self.assertTrue(self.has_flow(models[-1][0]))
        self.assertEqual((result.obj, result.status), (1600, 'OPTIMAL'))
        self.assertIn('stop_reason=VALID_UNEMP_STALL', log)

    def test_takeover_runs_cuts_before_flow_and_keeps_generated_constraints(self):
        models = []
        in_takeover = [False]
        real_window = solver.solve_one_asu_cpsat
        real_solve = solver.cp_model.CpSolver.Solve

        def window(**kwargs):
            takeover = 'objective_no_improve_stop' in kwargs
            if not takeover:
                return None
            self.assertFalse(kwargs['scout_before_cuts'])
            in_takeover[0] = True
            try:
                return real_window(**kwargs)
            finally:
                in_takeover[0] = False

        def capture(instance, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            if in_takeover[0]:
                models.append(model.Clone())
            instance.parameters.log_to_stdout = False
            return real_solve(instance, model, *args, **kwargs)

        frame = pd.DataFrame({'tract_ASU_unemp': [10, 1, 5, 2],
                              'tract_ASU_emp': [0, 50, 0, 30],
                              'tract_pop2024': [10000]*4})
        with (patch.object(solver, 'solve_one_asu_cpsat', side_effect=window),
              patch.object(solver, '_solve_supernode_polish', return_value=None),
              patch.object(solver, '_search_unassigned_asu', return_value=([], 'INFEASIBLE')),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0, 2], [1, 3], [2]], .2, 10000,
                initial_asu_id=[1, -1, 2, -1], max_asus=2,
                harvest_connectivity_free_asus=True, standalone_expansion_time_limit=0,
                final_asu_polish_time_limit=10, final_consolidation=False,
                deterministic_ties=True, configure_subsolvers=False,
                use_small_root_separators=False, time_limit=0, workers=1, verbose=False)
        self.assertEqual(result['asu_id'], [1, 1, 1, -1])
        self.assertTrue(models)
        self.assertFalse(self.has_flow(models[0]))
        cuts = [model for model in models if not self.has_flow(model)]
        flows = [model for model in models if self.has_flow(model)]
        self.assertTrue(flows)
        final_rows = {str(row) for row in flows[0].Proto().constraints}
        self.assertTrue(all(str(row) in final_rows for row in cuts[-1].Proto().constraints))

    def test_matching_verified_bound_skips_all_primary_solves(self):
        with patch.object(solver.cp_model.CpSolver, 'Solve',
                          side_effect=AssertionError('unnecessary primary solve')):
            result = solver.solve_one_asu_cpsat(
                [[1], [0]], np.array([10, 20]), np.array([0, 0]),
                np.array([10000]*2), .2, 10000, 0, time_limit=10,
                hint=[0, 1], deterministic_ties=False, log=False)
        self.assertEqual((result.obj, result.status), (30, 'OPTIMAL'))

    def test_bound_proof_still_runs_requested_secondary_objectives(self):
        result, models, log = self.run_window(hint=[0, 1, 2], objective_upper_bound=16,
                                              deterministic_ties=True)
        self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
        self.assertIn('bound check: primary optimum proved', log)
        self.assertNotIn('scout: before', log)
        self.assertTrue(models)
        self.assertTrue(all(self.has_flow(model) for model, _ in models))

    def test_scout_skip_retains_verified_incumbent_and_consumes_flag(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / 'skip'
            def interrupt(instance, model, *args, **kwargs):
                flag.touch()
                # The next checkpoint must consume this even if the scout
                # completes before its watchdog's polling interval.
                return solver.cp_model.UNKNOWN
            with patch.object(solver.cp_model.CpSolver, 'Solve', new=interrupt):
                result, _, _ = self.run_window(skip_flag_path=str(flag))
            self.assertEqual(result.obj, 10)
            self.assertEqual(result.status, 'SKIPPED_FEASIBLE')
            self.assertFalse(flag.exists())


if __name__ == '__main__':
    unittest.main()
