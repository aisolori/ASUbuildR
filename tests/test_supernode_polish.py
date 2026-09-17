"""Whole-ASU contraction preserves feasibility and statewide gain accounting."""
import contextlib
import io
import itertools
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
    def solve(self, u, emp, ids, nb=None, **options):
        n = len(u)
        nb = nb or [[j for j in (i-1, i+1) if 0 <= j < n] for i in range(n)]
        return solver._solve_supernode_polish(
            nb, np.array(u), np.array(emp), np.array([10000] * n),
            .2, 10000, 0, 5, 2, assignments=np.array(ids), asu_number=1,
            hint=[i for i, label in enumerate(ids) if label == 1], **options)

    def test_donor_connects_and_finances_new_tracts_without_counting_its_objective(self):
        # q=4u-e: the target cannot pay the bridge deficit of 60 alone.
        # Absorbing the two-tract donor unlocks the bridge and far tract.
        result = self.solve([10, 5, 10, 10, 20], [0, 80, 0, 0, 120], [1, -1, 2, 2, -1])
        self.assertEqual(result.status, 'OPTIMAL')
        self.assertEqual(result.sel_idx_local, list(range(5)))
        self.assertEqual(result.obj, 35)  # baseline 10 + newly captured 25

    def test_large_donor_cannot_pay_for_losing_already_captured_unemployment(self):
        # Original ASU=[0,1]. A cap allows either that ASU or root+donor,
        # but dropping tract 1 for donor 2 would lose 20 statewide.
        result = self.solve([10, 20, 1000], [0, 0, 0], [1, 1, 2],
                            nb=[[1, 2], [0], [0]], max_nodes=2)
        self.assertEqual(result.sel_idx_local, [0, 1])
        self.assertEqual(result.obj, 30)

    def test_cap_counts_every_donor_tract_and_disallows_partial_absorption(self):
        result = self.solve([10, 10, 10, 5], [0, 0, 0, 0], [1, 2, 2, -1], max_nodes=2)
        self.assertEqual(result.sel_idx_local, [0])
        self.assertEqual(result.obj, 10)

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
        for emp, cap in itertools.product(([0]*6, [0, 10, 100, 0, 0, 150]), (None, 3, 5)):
            with self.subTest(emp=emp, cap=cap):
                optimum = 15
                for bits in itertools.product((False, True), repeat=6):
                    if not bits[0] or bits[3] != bits[4]:
                        continue
                    selected = [i for i, value in enumerate(bits) if value]
                    if solver.component_ok(selected, np.array(u), np.array(emp),
                                           np.array([10000]*6), .2, 10000, nb, max_nodes=cap):
                        optimum = max(optimum, sum(u[i] for i in selected if ids[i] != 2))
                result = self.solve(u, emp, ids, nb=nb, max_nodes=cap)
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
        args = (0, [0], range(4), .2, 10000, None)
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
        # ASU 2 has more surplus, so its two tracts remain individual and
        # the single-tract ASU 1 becomes the optional donor supernode.
        self.assertIn('checking_asu=2 position=1/2', output.getvalue())
        self.assertIn('model_nodes=5 donors=1', output.getvalue())

    def test_merge_disabled_keeps_other_asus_protected(self):
        frame = pd.DataFrame({'tract_ASU_unemp': [10, 20, 5],
                              'tract_ASU_emp': [0, 0, 0], 'tract_pop2024': [10000]*3})
        with patch.object(solver, '_solve_supernode_polish',
                          side_effect=AssertionError('supernodes ran with merging disabled')):
            result = solver.build_many_asus_cpsat(
                frame, [[1], [0, 2], [1]], .2, 10000, max_asus=2,
                initial_asu_id=[1, 2, -1], harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0, final_asu_polish_time_limit=2,
                final_consolidation=False, merge_adjacent=False,
                time_limit=0, workers=2, verbose=False, deterministic_ties=False)
        self.assertEqual(result['n_asu'], 2)
        self.assertNotEqual(result['asu_id'][0], result['asu_id'][1])


if __name__ == '__main__':
    unittest.main()
