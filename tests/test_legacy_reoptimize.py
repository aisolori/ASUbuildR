"""Legacy reopens saved ASUs before residual discovery without rebuilding hints."""
import contextlib
import io
import json
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class LegacyReoptimizeTest(unittest.TestCase):
    def build(self, response=None, *, real=False, ids=(1, 1, -1), nb=None,
              unemp=(10, 0, 30), emp=(0, 10, 0), **options):
        frame = pd.DataFrame({'geoid': [str(i) for i in range(len(ids))],
                              'tract_ASU_unemp': unemp, 'tract_ASU_emp': emp,
                              'tract_pop2024': [10000] * len(ids)})
        defaults = dict(initial_asu_id=ids, max_asus=max(ids), workers=1,
                        time_limit=2, verbose=True, merge_adjacent=False,
                        configure_subsolvers=False, deterministic_ties=False,
                        final_asu_polish_time_limit=0, final_consolidation=False)
        defaults.update(options)
        output, calls = io.StringIO(), []
        original = solver.solve_one_asu_cpsat

        def solve(**kwargs):
            calls.append(kwargs)
            if real:
                return original(**kwargs)
            return response(kwargs) if callable(response) else response

        with (patch.object(solver, 'solve_one_asu_cpsat', side_effect=solve),
              patch.object(solver, '_prepare_window_hint', side_effect=AssertionError('pruning saved ASU')),
              patch.object(solver, '_search_unassigned_asu', return_value=([], 'INFEASIBLE')),
              contextlib.redirect_stdout(output)):
            result = solver.build_many_asus_cpsat(
                frame, nb or [[1, 2], [0], [0]], .2, 10000, **defaults)
        return result, calls, output.getvalue()

    def test_reoptimizes_at_max_asus_can_release_and_add_tracts(self):
        checkpoints = []
        with TemporaryDirectory() as folder:
            progress = Path(folder) / 'progress.json'
            result, calls, log = self.build(
                solver.CpsatResult([0, 2], 0, 40, 'FEASIBLE'),
                progress_out_path=str(progress),
                legacy_checkpoint_callback=lambda phase, ids: checkpoints.append((phase, ids)))
            self.assertEqual(json.loads(progress.read_text())['asu_id'], [1, -1, 1])
        self.assertEqual(result['asu_id'], [1, -1, 1])
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0]['hint'], [0, 1])
        self.assertEqual(calls[0]['hint_obj'], 10)
        self.assertIsNone(calls[0]['forced_selected'])
        self.assertEqual(checkpoints, [('LEGACY_REOPTIMIZE', [1, -1, 1])])
        self.assertIn('reverse_prune=skipped', log)
        self.assertIn('gain=30', log)

    def test_real_solver_expands_using_valid_hint_without_pruning(self):
        result, calls, log = self.build(real=True)
        self.assertEqual(result['asu_id'][2], 1)
        self.assertEqual(result['n_asu'], 1)
        self.assertIn('verified connected hint; auxiliary solve skipped', log)
        self.assertEqual(len(calls), 1)

    def test_invalid_worse_or_missing_replacement_retains_saved_solution(self):
        for selected in (None, [1], [0, 2], [-1], [100], [0, 0]):
            with self.subTest(selected=selected):
                response = None if selected is None else solver.CpsatResult(selected, 0, 99999, 'FEASIBLE')
                result, _, _ = self.build(response, ids=(1, 1, -1),
                                         unemp=(10, 5, 0), emp=(0, 0, 100))
                self.assertEqual(result['asu_id'], [1, 1, -1])

    def test_other_asus_are_excluded_and_newly_released_tracts_are_available(self):
        def replacement(kwargs):
            # ASU 1 drops tract 1; ASU 2 then sees that released tract.
            return solver.CpsatResult([kwargs['root_local']], kwargs['root_local'], 0, 'FEASIBLE')
        result, calls, _ = self.build(replacement, ids=(1, 1, 2),
                                     unemp=(10, 0, 30), emp=(0, 0, 0),
                                     nb=[[1], [0, 2], [1]])
        self.assertEqual(len(calls), 2)
        self.assertEqual(calls[0]['u_g'].tolist(), [10, 0])
        self.assertEqual(calls[1]['u_g'].tolist(), [0, 30])
        self.assertEqual(result['asu_id'], [1, -1, 2])

    def test_stop_before_pass_keeps_saved_solution(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / 'stop.flag'
            flag.touch()
            result, calls, _ = self.build(stop_flag_path=str(flag))
        self.assertEqual(calls, [])
        self.assertEqual(result['asu_id'], [1, 1, -1])

    def test_stop_after_solve_commits_valid_improvement(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / 'stop.flag'
            def stopped(kwargs):
                flag.touch()
                return solver.CpsatResult([0, 2], 0, 40, 'STOPPED_FEASIBLE')
            result, calls, _ = self.build(stopped, stop_flag_path=str(flag))
        self.assertEqual(result['asu_id'], [1, -1, 1])
        self.assertEqual(len(calls), 1)

    def test_skip_before_pass_is_consumed(self):
        with TemporaryDirectory() as folder:
            flag = Path(folder) / 'skip.flag'
            flag.touch()
            result, calls, _ = self.build(skip_flag_path=str(flag))
            self.assertFalse(flag.exists())
        self.assertEqual(calls, [])
        self.assertEqual(result['asu_id'], [1, 1, -1])

    def test_checkpoint_failure_stops_before_next_optimization(self):
        seen = []
        def checkpoint(phase, ids):
            seen.append((phase, ids))
            raise RuntimeError('checkpoint unavailable')
        with self.assertRaisesRegex(RuntimeError, 'checkpoint unavailable'):
            self.build(solver.CpsatResult([0, 2], 0, 40, 'FEASIBLE'),
                       legacy_checkpoint_callback=checkpoint)
        self.assertEqual(seen, [('LEGACY_REOPTIMIZE', [1, -1, 1])])


if __name__ == '__main__':
    unittest.main()
