"""Whole-ASU contraction preserves feasibility and statewide gain accounting."""
import contextlib
import io
import itertools
import re
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class SupernodePolishTest(unittest.TestCase):
    def test_cut_proof_skips_flow_when_ties_disabled(self):
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(engine, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(engine, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=capture):
            result = self.solve([10, 5, 20], [0, 0, 0], [1, -1, 2], deterministic_ties=False)
        self.assertEqual((result.obj, result.status), (15, 'OPTIMAL'))
        self.assertTrue(models)
        self.assertFalse(any(v.name.startswith('polish_flow_')
                             for model in models for v in model.Proto().variables))

    def test_25_stalled_bound_rounds_reset_on_improvement_even_without_new_cuts(self):
        bounds = [30, 30] + [29] * 26
        cut_models, flow_models, reports = [], [], []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(engine, model, *args, **kwargs):
            if not any(v.name.startswith('polish_flow_') for v in model.Proto().variables):
                cut_models.append(model.Clone())
                bound = bounds[len(cut_models) - 1]
                engine.BooleanValue = lambda var: True
                engine.BestObjectiveBound = lambda: bound
                return solver.cp_model.FEASIBLE
            flow_models.append(model.Clone())
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        output = io.StringIO()
        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=capture),
              contextlib.redirect_stdout(output)):
            result = self.solve([10, 5, 20], [0, 0, 0], [1, -1, 2],
                                deterministic_ties=False, log=True,
                                incumbent_report_callback=lambda selected, value: reports.append((selected, value)))
        self.assertEqual(len(cut_models), 28)
        self.assertEqual(len(flow_models), 1)
        self.assertEqual((result.obj, result.status), (15, 'OPTIMAL'))
        self.assertIn(([0, 1, 2], 15), reports)
        self.assertIn('upper_bound_stall=25/25', output.getvalue())
        self.assertIn('stop_reason=UPPER_BOUND_STALL', output.getvalue())
        self.assertIn('bound_carried_to_flow=True', output.getvalue())

    def test_disconnected_or_unknown_rounds_wait_for_bound_stall(self):
        for unknown in (False, True):
            with self.subTest(unknown=unknown):
                cut_models, flow_models = [], []
                real_solve = solver.cp_model.CpSolver.Solve

                def capture(engine, model, *args, **kwargs):
                    if not any(v.name.startswith('polish_flow_') for v in model.Proto().variables):
                        cut_models.append(model.Clone())
                        engine.BestObjectiveBound = lambda: 20

                        def selected(var):
                            if var.name.startswith('polish_supernode_'):
                                return var.name != 'polish_supernode_1'
                            return model.Proto().variables[var.index].domain[0] == 1

                        engine.BooleanValue = selected
                        return solver.cp_model.UNKNOWN if unknown else solver.cp_model.FEASIBLE
                    flow_models.append(model.Clone())
                    return real_solve(engine, model, *args, **kwargs)

                with patch.object(solver.cp_model.CpSolver, 'Solve', new=capture):
                    result = self.solve([10, 0, 5, 20], [0, 10, 0, 0], [1, -1, -1, 2],
                                        deterministic_ties=False)
                self.assertEqual(len(cut_models), 26)  # Baseline + 25 stalled rounds.
                self.assertEqual(len(flow_models), 1)
                self.assertEqual((result.obj, result.status), (15, 'OPTIMAL'))
                if not unknown:
                    self.assertGreater(len(cut_models[-1].Proto().constraints),
                                       len(cut_models[0].Proto().constraints))
                    # The flow model extends, rather than replaces, all cuts.
                    prefix = flow_models[0].Proto().constraints
                    for index, constraint in enumerate(cut_models[-1].Proto().constraints):
                        self.assertEqual(str(prefix[index]), str(constraint))

    def test_valid_stall_precedes_round_cap_when_every_bound_improves(self):
        cut_models, flow_models = [], []
        real_solve = solver.cp_model.CpSolver.Solve
        real_cuts = solver._joint_connectivity_cut_pass
        configured_limit = []

        def cuts(*args, **kwargs):
            self.assertFalse(kwargs['stop_on_new_cuts'])
            self.assertEqual(kwargs['round_seconds'], 5.0)
            self.assertEqual(kwargs['upper_bound_stall_rounds'], 25)
            configured_limit.append(kwargs['max_rounds'])
            return real_cuts(*args, **kwargs)

        def capture(engine, model, *args, **kwargs):
            if not any(v.name.startswith('polish_flow_') for v in model.Proto().variables):
                cut_models.append(model.Clone())
                self.assertLessEqual(len(cut_models), configured_limit[0])
                engine.BooleanValue = lambda var: True
                bound = configured_limit[0] + 100 - len(cut_models)
                engine.BestObjectiveBound = lambda: bound
                return solver.cp_model.FEASIBLE
            flow_models.append(model.Clone())
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        output = io.StringIO()
        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=capture),
              patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts),
              contextlib.redirect_stdout(output)):
            result = self.solve([10, 5, 20], [0, 0, 0], [1, -1, 2],
                                deterministic_ties=False, log=True)
        self.assertEqual(configured_limit[0], 100)
        self.assertEqual(len(cut_models), 51)  # Initial valid gain + 50 stalled rounds.
        self.assertEqual(len(flow_models), 1)
        self.assertEqual((result.obj, result.status), (15, 'OPTIMAL'))
        self.assertIn('stop_reason=VALID_UNEMP_STALL', output.getvalue())
        self.assertIn('upper_bound_stall=0/25', output.getvalue())

    def solve(self, u, emp, ids, nb=None, **options):
        n = len(u)
        workers = options.pop('workers', 2)
        nb = nb or [[j for j in (i-1, i+1) if 0 <= j < n] for i in range(n)]
        return solver._solve_supernode_polish(
            nb, np.array(u), np.array(emp), np.array([10000] * n),
            .2, 10000, 0, 5, workers, assignments=np.array(ids), asu_number=1,
            hint=[i for i, label in enumerate(ids) if label == 1], **options)

    def test_hybrid_uses_quotient_capacity_without_magnitude_variables(self):
        size = 30
        nb = [[j for j in range(size) if j != i] for i in range(size)]
        unemployment = list(range(1, size + 1))
        assignments = [1] + [-1] * (size - 3) + [2, 2]
        cut_models, flow_models, hybrid_inputs = [], [], []
        real_groups = solver._asu_flow_capacity_hybrid_groups

        def cuts(model, x, roots, graph, profit, fallback, valid, deadline,
                 workers, cancellation, **kwargs):
            cut_models.append(model.Clone())
            self.assertFalse(any(
                variable.name.startswith(('polish_flow_', 'polish_abs_flow_'))
                for variable in model.Proto().variables
            ))
            kwargs['proof_out'].append(False)
            kwargs['bound_out'].append(None)
            value = sum(int(profit[group].sum()) for group in fallback)
            return fallback, value, 'FEASIBLE'

        def groups(*args, **kwargs):
            hybrid_inputs.append((list(args[0]), list(args[1]), list(args[2])))
            return real_groups(*args, **kwargs)

        def capture(engine, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            flow_models.append((model.Clone(), list(engine.parameters.subsolvers)))
            engine.BestObjectiveBound = lambda: float('inf')
            return solver.cp_model.UNKNOWN

        with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts),
              patch.object(solver, '_asu_flow_capacity_hybrid_groups', side_effect=groups),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            self.solve(
                unemployment,
                [0] * size,
                assignments,
                nb=nb,
                workers=6,
                use_flow_capacity_hybrid_search=True,
                deterministic_ties=False,
            )

        self.assertEqual(len(cut_models), 1)
        self.assertEqual(len(flow_models), 1)
        self.assertEqual(len(hybrid_inputs), 1)
        model, subsolvers = flow_models[0]
        flow_names = [
            variable.name for variable in model.Proto().variables
            if variable.name.startswith('polish_flow_')
        ]
        magnitude_names = [
            variable.name for variable in model.Proto().variables
            if variable.name.startswith('polish_abs_flow_')
        ]
        # The two donor tracts are one quotient node with aggregated economics.
        quotient_edges, quotient_u, quotient_e = hybrid_inputs[0]
        self.assertEqual(len(quotient_u), size - 1)
        self.assertEqual(quotient_u[-1], unemployment[-2] + unemployment[-1])
        self.assertEqual(quotient_e[-1], 0)
        self.assertEqual(quotient_edges, [])
        self.assertGreater(len(flow_names), solver._ASU_HYBRID_PREFIX_SIZE)
        self.assertEqual(magnitude_names, [])
        self.assertIn('asu_flow_capacity_hybrid', subsolvers)
        self.assertGreaterEqual(len(model.Proto().search_strategy), 1)

    def test_hybrid_exact_flow_preserves_small_optimum(self):
        def cuts(model, x, roots, graph, profit, fallback, valid, deadline,
                 workers, cancellation, **kwargs):
            kwargs['proof_out'].append(False)
            kwargs['bound_out'].append(None)
            value = sum(int(profit[group].sum()) for group in fallback)
            return fallback, value, 'FEASIBLE'

        with patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts):
            result = self.solve(
                [10, 5, 10, 10, 20],
                [0, 80, 0, 0, 120],
                [1, -1, 2, 2, -1],
                workers=6,
                use_flow_capacity_hybrid_search=True,
                deterministic_ties=False,
            )

        self.assertEqual(result.status, 'OPTIMAL')
        self.assertEqual(result.sel_idx_local, list(range(5)))
        self.assertEqual(result.obj, 35)

    def test_hybrid_is_not_installed_below_six_workers(self):
        flow_models = []

        def cuts(model, x, roots, graph, profit, fallback, valid, deadline,
                 workers, cancellation, **kwargs):
            kwargs['proof_out'].append(False)
            kwargs['bound_out'].append(None)
            value = sum(int(profit[group].sum()) for group in fallback)
            return fallback, value, 'FEASIBLE'

        def capture(engine, model, *args, **kwargs):
            flow_models.append((model.Clone(), list(engine.parameters.subsolvers)))
            engine.BestObjectiveBound = lambda: float('inf')
            return solver.cp_model.UNKNOWN

        with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts),
              patch.object(solver, '_asu_flow_capacity_hybrid_groups') as groups,
              patch.object(solver.cp_model.CpSolver, 'Solve', new=capture)):
            self.solve(
                [10, 5, 20], [0, 0, 0], [1, -1, 2],
                workers=5,
                use_flow_capacity_hybrid_search=True,
                deterministic_ties=False,
            )

        groups.assert_not_called()
        self.assertEqual(len(flow_models), 1)
        model, subsolvers = flow_models[0]
        self.assertFalse(any(
            variable.name.startswith('polish_abs_flow_')
            for variable in model.Proto().variables
        ))
        self.assertNotIn('asu_flow_capacity_hybrid', subsolvers)

    def test_path_tightenings_hold_for_every_connected_feasible_subset(self):
        nb = [[1, 3], [0, 2], [1], [0]]
        profit = [5, 0, 10, 10]
        q = [10, -10, 30, -100]
        positive_supply, surplus_distance = solver._surplus_path_tightening_data(
            nb, q, 0
        )
        denominator, upper, reduced_distance, _ = (
            solver._reduced_cost_path_tightening_data(nb, profit, q, 0)
        )
        self.assertEqual(positive_supply, 40)
        self.assertGreater(reduced_distance[2], 0)
        checked = 0
        for bits in itertools.product((False, True), repeat=len(nb)):
            selected = {i for i, value in enumerate(bits) if value}
            if 0 not in selected or sum(q[i] for i in selected) < 0:
                continue
            reached = {0}
            stack = [0]
            while stack:
                node = stack.pop()
                for neighbor in nb[node]:
                    if neighbor in selected and neighbor not in reached:
                        reached.add(neighbor)
                        stack.append(neighbor)
            if reached != selected:
                continue
            selected_positive = sum(max(0, q[i]) for i in selected)
            objective = sum(profit[i] for i in selected)
            for node in selected:
                self.assertLessEqual(surplus_distance[node], selected_positive)
                self.assertLessEqual(
                    denominator * objective + reduced_distance[node], upper
                )
                checked += 1
        self.assertGreater(checked, 0)

    def test_surplus_path_rows_share_one_sparse_aggregate(self):
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def capture(engine, model, *args, **kwargs):
            models.append(model.Clone())
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, 'Solve', new=capture):
            result = self.solve(
                [10, 5, 10, 5], [0, 30, 10, 20], [1, -1, -1, -1],
                nb=[[1], [0, 2], [1, 3], [2]],
                deterministic_ties=False,
            )
        self.assertEqual(result.status, 'OPTIMAL')
        self.assertTrue(models)
        proto = models[0].Proto()
        aggregate = next(
            i for i, variable in enumerate(proto.variables)
            if variable.name == 'polish_positive_q_selected'
        )
        containing = [
            constraint.linear for constraint in proto.constraints
            if aggregate in constraint.linear.vars
        ]
        self.assertGreater(len(containing), 1)
        self.assertEqual(sum(len(row.vars) > 2 for row in containing), 1)
        self.assertEqual(sum(len(row.vars) == 2 for row in containing), len(containing) - 1)

    def test_unaffordable_corridor_is_removed_before_cut_proof(self):
        output = io.StringIO()
        real_solve = solver.cp_model.CpSolver.Solve

        def quiet(engine, model, *args, **kwargs):
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        with (patch.object(solver.cp_model.CpSolver, 'Solve', new=quiet),
              contextlib.redirect_stdout(output)):
            result = self.solve(
                [10, 0, 20], [0, 50, 100], [1, -1, -1],
                nb=[[1], [0, 2], [1]], log=True,
                deterministic_ties=False,
            )
        self.assertEqual((result.sel_idx_local, result.obj), ([0], 10))
        first_stage = next(
            line for line in output.getvalue().splitlines()
            if '[STAGE] FINAL_POLISH_SUPERNODES ' in line
        )
        self.assertIn('fixed_zero=2', first_stage)
        self.assertIn('max_selected=1', first_stage)

    def test_donor_connects_and_finances_new_tracts_without_counting_its_objective(self):
        # q=4u-e: the target cannot pay the bridge deficit of 60 alone.
        # Absorbing the two-tract donor unlocks the bridge and far tract.
        result = self.solve([10, 5, 10, 10, 20], [0, 80, 0, 0, 120], [1, -1, 2, 2, -1])
        self.assertEqual(result.status, 'OPTIMAL')
        self.assertEqual(result.sel_idx_local, list(range(5)))
        self.assertEqual(result.obj, 35)  # baseline 10 + newly captured 25

    def test_large_donor_cannot_pay_for_losing_already_captured_unemployment(self):
        # Absorbing a large donor must preserve the original captured U.
        # Donor unemployment is discounted in the optimization objective.
        result = self.solve([10, 20, 1000], [0, 0, 0], [1, 1, 2],
                            nb=[[1, 2], [0], [0]])
        self.assertEqual(result.sel_idx_local, [0, 1, 2])
        self.assertEqual(result.obj, 30)


    def test_ties_can_consolidate_without_inflating_gain(self):
        result = self.solve([10, 20, 20], [0, 0, 0], [1, 2, 2])
        self.assertEqual(result.sel_idx_local, [0, 1, 2])
        self.assertEqual(result.obj, 10)

    def test_disconnected_donor_is_not_a_teleporting_supernode(self):
        result = self.solve([10, 10, 10, 5], [0] * 4, [1, 2, 2, -1],
                            nb=[[1], [0], [3], [2]])
        self.assertEqual(result.sel_idx_local, [0])

    def test_optimum_matches_exhaustive_whole_donor_selections(self):
        ids = [1, 1, -1, 2, 2, -1]
        u = [10, 5, 7, 20, 10, 30]
        nb = [[1, 3], [0, 2], [1, 3], [0, 2, 4], [3, 5], [4]]
        for emp in ([0]*6, [0, 10, 100, 0, 0, 150]):
            with self.subTest(emp=emp):
                optimum = 15
                for bits in itertools.product((False, True), repeat=6):
                    if not bits[0] or bits[3] != bits[4]:
                        continue
                    selected = [i for i, value in enumerate(bits) if value]
                    if solver.component_ok(selected, np.array(u), np.array(emp),
                                           np.array([10000]*6), .2, 10000, nb):
                        optimum = max(optimum, sum(u[i] for i in selected if ids[i] != 2))
                result = self.solve(u, emp, ids, nb=nb)
                self.assertEqual(result.status, 'OPTIMAL')
                self.assertEqual(result.obj, optimum)

    def test_stop_and_skip_retain_incumbent(self):
        with TemporaryDirectory() as folder:
            for flag in ('stop', 'skip'):
                path = Path(folder) / flag
                path.touch()
                result = self.solve([10, 20], [0, 0], [1, 2], **{flag + '_flag_path': str(path)})
                self.assertEqual(result.sel_idx_local, [0])
                self.assertEqual(result.status, {'stop': 'STOPPED_FEASIBLE',
                                                 'skip': 'SKIPPED_FEASIBLE'}[flag])
                self.assertEqual(path.exists(), flag == 'stop')

    def test_window_and_cache_include_donor_ownership(self):
        nb = [[1], [0, 2], [1, 3], [2]]
        ids = np.array([1, 2, 2, -1])
        self.assertEqual(solver._reachable_polish_window(0, 1, ids, nb), [0])
        self.assertEqual(solver._reachable_polish_window(0, 1, ids, nb, supernodes=True), [0, 1, 2, 3])
        args = (0, [0], range(4), .2, 10000)
        self.assertNotEqual(solver._polish_attempt_key(*args, ownership=ids),
                            solver._polish_attempt_key(*args, ownership=[1, 2, 3, -1]))

    def test_build_absorbs_donor_and_publishes_only_real_gain(self):
        u, emp = [10, 5, 10, 10, 20], [0, 80, 0, 0, 120]
        frame = pd.DataFrame({'tract_ASU_unemp': u, 'tract_ASU_emp': emp,
                              'tract_pop2024': [10000]*5})
        nb = [[j for j in (i-1, i+1) if 0 <= j < 5] for i in range(5)]
        output = io.StringIO()
        real_solve = solver.cp_model.CpSolver.Solve

        def quiet_solve(instance, model, *args, **kwargs):
            self.assertEqual(model.Validate(), '')
            instance.parameters.log_to_stdout = False
            return real_solve(instance, model, *args, **kwargs)

        with (contextlib.redirect_stdout(output),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=quiet_solve)):
            result = solver.build_many_asus_cpsat(
                frame, nb, .2, 10000, max_asus=2, initial_asu_id=[1, -1, 2, 2, -1],
                harvest_connectivity_free_asus=True, standalone_expansion_time_limit=0,
                final_asu_polish_time_limit=5, final_consolidation=False,
                time_limit=0, workers=2, verbose=True, deterministic_ties=False)
        self.assertEqual(result['n_asu'], 1)
        self.assertEqual(len(set(result['asu_id'])), 1)
        self.assertIn('statewide_gain=25 absorbed_asus=1', output.getvalue())
        # ASU 1 has lower total unemployment and goes first. ASU 2 is a donor.
        self.assertIn('checking_asu=1 position=1/2', output.getvalue())
        # The two-tract ASU 2 is contracted to one donor supernode.
        self.assertIn('model_nodes=4 donors=1', output.getvalue())
        stage_lines = [line for line in output.getvalue().splitlines() if '[STAGE]' in line]
        self.assertTrue(stage_lines)
        for line in stage_lines:
            self.assertRegex(line, r'\btotal_unemp=\d+\b')
            self.assertEqual(len(re.findall(r'\btotal_unemp=', line)), 1)
        self.assertIn('total_unemp=30', stage_lines[0])
        completed = next(line for line in stage_lines if '[STAGE] FINAL_POLISH_COMPLETE ' in line)
        self.assertIn('total_unemp=55', completed)
        self.assertIn('priority=unemployment_ascending', output.getvalue())

    def test_surviving_asu_stage_id_matches_dashboard_after_donor_absorption(self):
        frame = pd.DataFrame({'tract_ASU_unemp': [30, 20, 10],
                              'tract_ASU_emp': [0, 0, 0], 'tract_pop2024': [10000]*3})
        output = io.StringIO()
        real_solve = solver.cp_model.CpSolver.Solve

        def quiet(engine, model, *args, **kwargs):
            engine.parameters.log_to_stdout = False
            return real_solve(engine, model, *args, **kwargs)

        with (contextlib.redirect_stdout(output),
              patch.object(solver.cp_model.CpSolver, 'Solve', new=quiet),
              patch.object(solver, '_merge_touching_asu_units',
                           side_effect=lambda units, *args, **kwargs: (units, 0))):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0], []], .2, 10000, max_asus=3,
                initial_asu_id=[1, 2, 3], harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0, final_asu_polish_time_limit=5,
                final_consolidation=False, time_limit=0, workers=1, verbose=True)
        self.assertEqual(list(result['asu_id']), [1, 1, 2])
        line = next(line for line in output.getvalue().splitlines()
                    if 'internal_checking_asu=3' in line)
        self.assertIn('checking_asu=2 ', line)
        self.assertIn('checking_asus=2 ', line)

    def test_merge_disabled_keeps_other_asus_protected(self):
        frame = pd.DataFrame({'tract_ASU_unemp': [10, 20, 5],
                              'tract_ASU_emp': [0, 0, 0], 'tract_pop2024': [10000]*3})
        output = io.StringIO()
        with (contextlib.redirect_stdout(output),
              patch.object(solver, '_solve_supernode_polish',
                           side_effect=AssertionError('supernodes ran with merging disabled'))):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0, 2], [1]], .2, 10000, max_asus=2,
                initial_asu_id=[1, 2, -1], harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0, final_asu_polish_time_limit=2,
                final_consolidation=False, merge_adjacent=False,
                time_limit=0, workers=2, verbose=True, deterministic_ties=False)
        self.assertEqual(result['n_asu'], 2)
        self.assertNotEqual(result['asu_id'][0], result['asu_id'][1])
        self.assertIn('checking_asu=1 position=1/2', output.getvalue())
        self.assertIn('priority=unemployment_ascending', output.getvalue())


if __name__ == '__main__':
    unittest.main()
