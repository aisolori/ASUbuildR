"""Statewide stage totals and shared cut-bound stall semantics."""
import contextlib
from contextvars import copy_context
from concurrent.futures import ThreadPoolExecutor
import io
from pathlib import Path
import sys
import unittest

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class StageReportingTest(unittest.TestCase):
    def test_stage_ids_match_dashboard_compaction_and_refresh_after_absorption(self):
        assignments = np.array([1, 3, 5, -1])
        token = solver._stage_assignments.set(lambda: assignments)
        output = io.StringIO()
        try:
            self.assertEqual(solver._asu_display_id_map(assignments), {1: 1, 3: 2, 5: 3})
            with contextlib.redirect_stdout(output), solver._stage_checking([5], 1):
                solver._stage_print('[STAGE] FINAL_POLISH checking_asu=5')
                # ASU 5 absorbs ASU 1. Dashboard IDs are now {3: 1, 5: 2}.
                assignments[assignments == 1] = 5
                solver._stage_print('[STAGE] FINAL_POLISH_COMPLETE asu=5')
        finally:
            solver._stage_assignments.reset(token)
        before, after = output.getvalue().splitlines()
        self.assertIn('checking_asu=3 ', before)
        self.assertIn('checking_asus=3 ', before)
        self.assertIn('internal_checking_asu=5', before)
        self.assertIn('FINAL_POLISH_COMPLETE asu=2 ', after)
        self.assertIn('checking_asus=2 ', after)
        self.assertIn('internal_asu=5', after)
        self.assertEqual(assignments.tolist(), [5, 3, 5, -1])

    def test_joint_ids_are_mapped_once_and_candidate_labels_unchanged(self):
        assignments = np.array([1, 3, 5, -1])
        token = solver._stage_assignments.set(lambda: assignments)
        output = io.StringIO()
        try:
            labels = solver._stage_unit_labels([[1], [2], [3]])
            self.assertEqual(labels, ['3', '5', 'candidate_tract_3'])
            with contextlib.redirect_stdout(output), solver._stage_checking(labels, 0):
                solver._stage_print('[STAGE] JOINT_CUT')
                solver._stage_print('[STAGE] EXPLICIT checking_asus=3,5')
        finally:
            solver._stage_assignments.reset(token)
        self.assertIn('checking_asus=2,3,candidate_tract_3 ', output.getvalue())
        self.assertIn('EXPLICIT checking_asus=2,3 ', output.getvalue())

    def test_live_totals_and_multiline_messages(self):
        total = [100]
        token = solver._stage_total_provider.set(lambda: total[0])
        output = io.StringIO()
        try:
            with contextlib.redirect_stdout(output):
                solver._stage_print('[STAGE] START total_unemp=9')
                total[0] = 120
                solver._stage_print('\n[STAGE] COMPLETE\n[DETAIL] retained=12')
                with ThreadPoolExecutor(max_workers=1) as pool:
                    pool.submit(copy_context().run, solver._stage_print, '[STAGE] WORKER').result()
        finally:
            solver._stage_total_provider.reset(token)
        self.assertIn('[STAGE] START total_unemp=100', output.getvalue())
        self.assertIn('[STAGE] COMPLETE total_unemp=120 checking_asus=none asus_remaining=NA\n[DETAIL] retained=12', output.getvalue())
        self.assertIn('[STAGE] WORKER total_unemp=120', output.getvalue())

    def test_build_scope_restored_even_on_exception(self):
        before = solver._stage_total_provider.get()

        @solver._stage_reporting
        def failing():
            solver._stage_total_provider.set(lambda: 999)
            raise ValueError('test')

        with self.assertRaises(ValueError):
            failing()
        self.assertIs(solver._stage_total_provider.get(), before)

    def test_standalone_stage_does_not_invent_a_statewide_total(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            solver._stage_print('[STAGE] LOCAL objective=999')
        self.assertIn('total_unemp=NA', output.getvalue())

    def test_bound_stall_resets_only_on_new_minimum(self):
        tracker = solver._UpperBoundStall(10)
        self.assertFalse(tracker.observe(100))
        for _ in range(9):
            self.assertFalse(tracker.observe(100))
        self.assertFalse(tracker.observe(90))
        self.assertEqual(tracker.rounds, 0)
        for bound in [95, 90, None, float('nan'), float('inf'), 90, 90, 90, 90]:
            self.assertFalse(tracker.observe(bound))
        self.assertTrue(tracker.observe(90))
        self.assertEqual(tracker.best, 90)

    def test_nested_check_context_restores_queue_and_no_leak(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            with solver._stage_checking([2], 3):
                solver._stage_print('[STAGE] POLISH')
                with solver._stage_checking([2, 5], 1):
                    solver._stage_print('[STAGE] CUT_ROUNDS')
                solver._stage_print('[STAGE] COMPLETE')
            solver._stage_print('[STAGE] DONE')
        lines = output.getvalue().splitlines()
        self.assertIn('checking_asus=2 asus_remaining=3', lines[0])
        self.assertIn('checking_asus=2,5 asus_remaining=1', lines[1])
        self.assertIn('checking_asus=2 asus_remaining=3', lines[2])
        self.assertIn('checking_asus=none asus_remaining=NA', lines[3])


if __name__ == '__main__':
    unittest.main()
