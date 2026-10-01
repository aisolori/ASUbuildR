"""Stalled primary flow retries flow-free cuts within one shared deadline."""
import contextlib
import io
from pathlib import Path
import sys
import threading
import time
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as solver


class SupernodeCutCyclesTest(unittest.TestCase):
    def run_case(self, mode='repeat', stall_seconds=.01):
        cuts_seen, flows_seen, cut_models, seen_ids = [], [], [], []
        original_solve = solver.cp_model.CpSolver.Solve
        real_clock = time.monotonic
        clock_offset = [0.0]
        output = io.StringIO()
        with TemporaryDirectory() as folder:
            stop, skip = Path(folder) / 'stop', Path(folder) / 'skip'

            def cuts(model, x, roots, nb, profit, fallback, valid, deadline,
                     workers, cancellation, **kwargs):
                self.assertFalse(any(v.name.startswith('polish_flow_')
                                     for v in model.Proto().variables))
                cuts_seen.append((kwargs['max_rounds'], kwargs['upper_bound_stall_rounds'],
                                  deadline, [list(group) for group in fallback],
                                  kwargs['initial_upper_bound']))
                self.assertTrue(valid(fallback))
                seen_ids.append(id(kwargs['seen_cuts']))
                if len(cuts_seen) > 1:
                    self.assertIn((0, 0, (0,)), kwargs['seen_cuts'])
                    # Every earlier constraint remains in the flow-free base.
                    for index, constraint in enumerate(cut_models[-1].Proto().constraints):
                        self.assertEqual(str(model.Proto().constraints[index]), str(constraint))
                kwargs['seen_cuts'].add((0, 0, (0,)))
                model.Add(x[0][0] >= 0)
                cut_models.append(model.Clone())
                kwargs['proof_out'].append(False)
                kwargs['bound_out'].append(30)
                if mode in ('cut_merge', 'unknown_merge'):
                    fallback = [[0, 4]]
                    self.assertTrue(valid(fallback))
                value = sum(int(profit[group].sum()) for group in fallback)
                if mode == 'skip_retry' and len(cuts_seen) == 2:
                    return fallback, value, 'SKIPPED_FEASIBLE'
                return fallback, value, 'FEASIBLE'

            def flow(engine, model, *args, **kwargs):
                self.assertTrue(any(v.name.startswith('polish_flow_')
                                    for v in model.Proto().variables))
                flows_seen.append(engine.parameters.max_time_in_seconds)
                if len(flows_seen) == 3:
                    engine.parameters.log_to_stdout = False
                    return original_solve(engine, model, *args, **kwargs)
                selected = {0, 1} if len(flows_seen) == 1 else {0, 1, 2}
                if mode in ('merge', 'cut_merge', 'unknown_merge'):
                    selected = {0, 4} if mode == 'merge' else {0}
                engine.BooleanValue = lambda var: int(var.name.rsplit('_', 1)[1]) in selected
                engine.Value = lambda var: (10 if mode in ('merge', 'cut_merge', 'unknown_merge')
                                            else 15 if len(flows_seen) == 1 else 20)
                engine.BestObjectiveBound = lambda: 30
                if mode != 'ordinary_end':
                    stopped = threading.Event()
                    engine.StopSearch = stopped.set
                    if mode == 'stop':
                        stop.touch()
                    elif mode == 'skip':
                        skip.touch()
                    self.assertTrue(stopped.wait(2), 'stall/cancellation watcher did not stop flow')
                    if mode == 'deadline':
                        clock_offset[0] = 10.0
                if mode in ('unknown_stall', 'unknown_merge') and len(flows_seen) == 1:
                    return solver.cp_model.UNKNOWN
                return solver.cp_model.FEASIBLE

            with (patch.object(solver, '_joint_connectivity_cut_pass', side_effect=cuts),
                  patch.object(solver.cp_model.CpSolver, 'Solve', new=flow),
                  patch.object(solver.time, 'monotonic', side_effect=lambda: real_clock() + clock_offset[0]),
                  contextlib.redirect_stdout(output)):
                result = solver._solve_supernode_polish(
                    ([[1, 4], [0, 2], [1, 3], [2], [0]]
                     if mode in ('merge', 'cut_merge', 'unknown_merge') else
                     [[1], [0, 2], [1, 3], [2, 4], [3]]),
                    np.array([10, 5, 5, 10, 20]), np.zeros(5, dtype=int),
                    np.array([10000]*5), .2, 10000, 0, 5, 1,
                    assignments=np.array([1, -1, -1, -1, 2]), asu_number=1, hint=[0],
                    deterministic_ties=False, incumbent_stall_seconds=stall_seconds,
                    stop_flag_path=str(stop), skip_flag_path=str(skip), log=True)
            if mode == 'skip':
                self.assertFalse(skip.exists())
            if mode == 'stop':
                self.assertTrue(stop.exists())
        return result, cuts_seen, flows_seen, seen_ids, output.getvalue()

    def test_stalls_double_both_limits_and_preserve_state_and_deadline(self):
        result, cuts, flows, seen_ids, log = self.run_case()
        self.assertEqual([(row[0], row[1]) for row in cuts], [(100, 25), (200, 50), (400, 100)])
        self.assertEqual(len({row[2] for row in cuts}), 1)
        self.assertEqual([row[3] for row in cuts], [[[0]], [[0, 1]], [[0, 1, 2]]])
        self.assertEqual([row[4] for row in cuts], [None, 30, 30])
        self.assertEqual(len(set(seen_ids)), 1)
        self.assertGreater(flows[0], flows[1])
        self.assertGreater(flows[1], flows[2])
        self.assertEqual((result.obj, result.status), (30, 'OPTIMAL'))
        self.assertEqual(log.count('[STAGE] FINAL_POLISH_SUPERNODES_RETRY_CUTS '), 2)
        flow_lines = [line for line in log.splitlines()
                      if '[STAGE] FINAL_POLISH_SUPERNODES_FLOW ' in line]
        for line, seconds in zip(flow_lines, [.01, .02, .04]):
            self.assertIn(f'incumbent_stall_seconds={seconds}', line)

    def test_disabled_stall_limit_stays_disabled(self):
        for seconds in (None, 0):
            with self.subTest(seconds=seconds):
                result, cuts, flows, _, log = self.run_case('ordinary_end', stall_seconds=seconds)
                self.assertEqual((len(cuts), len(flows)), (1, 1))
                self.assertEqual(result.status, 'FEASIBLE')
                self.assertIn(f'incumbent_stall_seconds={seconds}', log)

    def test_no_retry_for_ordinary_end_stop_skip_or_exhausted_budget(self):
        for mode, status in [('ordinary_end', 'FEASIBLE'), ('stop', 'STOPPED_FEASIBLE'),
                             ('skip', 'SKIPPED_FEASIBLE'), ('deadline', 'STALLED_FEASIBLE')]:
            with self.subTest(mode=mode):
                result, cuts, flows, _, log = self.run_case(mode)
                self.assertEqual((len(cuts), len(flows)), (1, 1))
                self.assertEqual((result.obj, result.status), (15, status))
                self.assertNotIn('[STAGE] FINAL_POLISH_SUPERNODES_RETRY_CUTS ', log)

    def test_skip_during_retry_cuts_preserves_latest_flow_incumbent(self):
        result, cuts, flows, _, _ = self.run_case('skip_retry')
        self.assertEqual((len(cuts), len(flows)), (2, 1))
        self.assertEqual((result.obj, result.status), (15, 'SKIPPED_FEASIBLE'))
        self.assertEqual(result.sel_idx_local, [0, 1])

    def test_stall_without_a_new_flow_solution_retains_prior_connected_incumbent(self):
        result, cuts, flows, _, _ = self.run_case('unknown_stall')
        self.assertEqual((len(cuts), len(flows)), (3, 3))
        self.assertEqual(cuts[1][3], [[0]])
        self.assertEqual((result.obj, result.status), (30, 'OPTIMAL'))

    def test_stall_returns_available_merge_without_retry_even_at_equal_coverage(self):
        for mode in ('merge', 'cut_merge', 'unknown_merge'):
            with self.subTest(mode=mode):
                result, cuts, flows, _, log = self.run_case(mode)
                self.assertEqual((len(cuts), len(flows)), (1, 1))
                self.assertEqual(result.sel_idx_local, [0, 4])
                self.assertEqual((result.obj, result.status), (10, 'STALLED_FEASIBLE'))
                self.assertIn('[STAGE] FINAL_POLISH_SUPERNODES_STALL_MERGE ', log)
                self.assertIn('statewide_gain=0', log)
                self.assertNotIn('[STAGE] FINAL_POLISH_SUPERNODES_RETRY_CUTS ', log)


if __name__ == '__main__':
    unittest.main()
