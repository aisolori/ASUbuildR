"""Publish expansion assignments before polish; never schedule bridge pairs."""
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


class PartitionPublicationTest(unittest.TestCase):
    def test_expansion_published_before_first_polish_and_no_bridge(self):
        frame = pd.DataFrame({'tract_ASU_unemp': [10, 0, 0, 20],
                              'tract_ASU_emp': [0, 1000, 1000, 0],
                              'tract_pop2024': [10000]*4})
        nb = [[1], [0, 2], [1, 3], [2]]
        prepared = dict(connectivity_free_standalone_asus=[[0], [3]],
                        hint_valid=False, hint_improved=[], hint_obj_val=0,
                        hint_source='test', n_contracted=4, root_component=[0])
        snapshots, output = [], io.StringIO()
        with TemporaryDirectory() as folder:
            progress = Path(folder) / 'progress.json'

            def single(**kwargs):
                snapshots.append(json.loads(progress.read_text()))
                return None  # Valid seeds remain the incumbent.

            with (patch.object(solver, '_prepare_window_hint', return_value=prepared),
                  patch.object(solver, 'solve_one_asu_cpsat', side_effect=single),
                  patch.object(solver, '_solve_regional_exchange',
                               side_effect=AssertionError('unexpected joint/bridge solve')),
                  patch.object(solver, '_search_unassigned_asu', return_value=([], 'INFEASIBLE')),
                  contextlib.redirect_stdout(output),
                  self.assertWarnsRegex(UserWarning, 'bridge-pair phase was removed')):
                result = solver.build_many_asus_cpsat(
                    frame, nb, .2, 10000, max_asus=2, workers=1, time_limit=1,
                    full_graph_window=True, harvest_connectivity_free_asus=True,
                    standalone_expansion_time_limit=1, final_asu_polish_time_limit=1,
                    merge_adjacent=False, final_consolidation=False,
                    verbose=True, bridge_pair=[1, 2],
                    progress_out_path=str(progress))
        before_polish = next(s for s in snapshots if s['phase'] == 'PRE_POLISH')
        self.assertEqual(before_polish['n_asu'], 2)
        self.assertEqual(before_polish['total_unemp'], 30)
        self.assertEqual(sum(v > 0 for v in before_polish['asu_id']), 2)
        self.assertEqual(result['n_asu'], 2)
        self.assertNotIn('[STAGE] BRIDGE', output.getvalue())
        self.assertFalse(hasattr(solver, '_bridge_pass'))
        self.assertIn('checking_asus=candidate_tract_', output.getvalue())
        first_polish = next(line for line in output.getvalue().splitlines()
                            if '[STAGE] FINAL_POLISH ' in line and 'position=1/2' in line)
        self.assertIn('checking_asus=1', first_polish)
        self.assertIn('asus_remaining=1', first_polish)


if __name__ == '__main__':
    unittest.main()
