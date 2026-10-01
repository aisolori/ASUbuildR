"""Finish surviving queued ASUs before restarting after a committed merge."""
import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class FinalPolishMergePriorityTest(unittest.TestCase):
    def run_build(self, replacements, *, status="STALLED_FEASIBLE", reverse=False, unemployment=None):
        # Four saved ASUs, separated by unassigned connector tracts. Population
        # encodes global indices so the mocked solve can inspect real windows.
        u = [10, 0, 60, 0, 20, 0, 30]
        if reverse:
            u = [20, 0, 30, 0, 60, 0, 10]
        if unemployment is not None:
            u = unemployment
        frame = pd.DataFrame({
            "tract_ASU_unemp": u,
            "tract_ASU_emp": [0, 20, 0, 20, 0, 20, 0],
            "tract_pop2024": [10000 + node for node in range(7)],
        })
        nb = [[neighbor for neighbor in (node - 1, node + 1)
               if 0 <= neighbor < 7] for node in range(7)]
        calls = []

        def polish(**kwargs):
            window = [int(pop) - 10000 for pop in kwargs["P_g"]]
            hint = tuple(window[node] for node in kwargs["hint"])
            calls.append((kwargs["asu_number"], hint))
            selected = replacements.get(hint, hint)
            local_selected = [window.index(node) for node in selected]
            ownership = kwargs["assignments"]
            objective = sum(u[window[node]] for node in local_selected
                            if ownership[node] <= 0
                            or ownership[node] == kwargs["asu_number"])
            return solver.CpsatResult(
                local_selected, kwargs["root_local"], objective, status,
            )

        output = io.StringIO()
        with (
            patch.object(solver, "_solve_supernode_polish", side_effect=polish),
            # No statewide takeover change or residual ASU creation in this
            # scheduling test; all individual polish results use the real
            # validation, commit, merge, cache and queue logic.
            patch.object(solver, "solve_one_asu_cpsat", return_value=None),
            patch.object(solver, "_search_unassigned_asu", return_value=([], "INFEASIBLE")),
            contextlib.redirect_stdout(output),
        ):
            result = solver.build_many_asus_cpsat(
                frame, nb, .2, 10000, max_asus=4,
                initial_asu_id=[1, -1, 2, -1, 3, -1, 4],
                harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0, final_asu_polish_time_limit=2,
                final_consolidation=False, time_limit=0, workers=1,
                verbose=True, deterministic_ties=False,
            )
        ids = np.array(result["asu_id"])
        for label in np.unique(ids[ids > 0]):
            self.assertTrue(solver.component_ok(
                np.flatnonzero(ids == label).tolist(), np.array(u),
                frame["tract_ASU_emp"].to_numpy(),
                frame["tract_pop2024"].to_numpy(), .2, 10000, nb,
            ))
        return calls, result, output.getvalue()

    def test_absorbed_donor_waits_behind_lower_unemployment_asus(self):
        for status in ("STALLED_FEASIBLE", "FEASIBLE", "OPTIMAL"):
            with self.subTest(status=status):
                calls, result, log = self.run_build({(0,): (0, 1, 2)}, status=status)
                # Merged U=70 waits behind the remaining ASUs at U=20 and U=30.
                self.assertEqual(calls, [(1, (0,)), (3, (4,)), (4, (6,)),
                                         (1, (0, 1, 2))])
                self.assertEqual(result["n_asu"], 3)
                self.assertIn("merged_first=none", log)

    def test_chained_merges_recalculate_unemployment_order_each_time(self):
        calls, result, _ = self.run_build({
            (0,): (0, 1, 2),
            (0, 1, 2): (0, 1, 2, 3, 4),
        })
        self.assertEqual(calls, [(1, (0,)), (3, (4,)), (4, (6,)),
                                 (1, (0, 1, 2)), (4, (6,)),
                                 (1, (0, 1, 2, 3, 4))])
        self.assertEqual(result["n_asu"], 2)

    def test_touching_union_waits_behind_smaller_asus(self):
        # ASU 4 takes connector 5 and joins ASU 3. The resulting U=70 ASU
        # waits behind the ASUs at U=20 and U=30 after the queue restarts.
        calls, result, log = self.run_build({(6,): (5, 6)}, reverse=True)
        self.assertEqual(calls, [(4, (6,)), (1, (0,)), (2, (2,)),
                                 (3, (4, 5, 6))])
        self.assertEqual(result["n_asu"], 3)
        self.assertIn("merged_first=none", log)

    def test_remaining_queue_finishes_even_when_merged_group_would_sort_ahead(self):
        for replacement in ((0, 1, 2), (0, 1)):
            with self.subTest(replacement=replacement):
                # U=10 merges with U=15. It must wait for BOTH pending U=20
                # and U=90, although an immediate sort would put U=25 first
                # ahead of U=90. Connector-only growth exercises safe union
                # and compaction (pending IDs 3/4 become 2/3).
                calls, result, log = self.run_build(
                    {(0,): replacement}, unemployment=[10, 0, 15, 0, 20, 0, 90])
                self.assertEqual([hint for _, hint in calls[:4]],
                                 [(0,), (4,), (6,), (0, 1, 2)])
                self.assertEqual(result['n_asu'], 3)
                self.assertIn('action=continue_remaining restart=after_sweep', log)
                checks = [line for line in log.splitlines()
                          if '[STAGE] FINAL_POLISH round=1 ' in line and 'checking_asu=' in line]
                self.assertEqual(len(checks), 3)
                self.assertIn('asus_remaining=1', checks[1])
                self.assertIn('asus_remaining=0', checks[2])

    def test_without_merges_normal_unemployment_order_is_preserved(self):
        calls, result, log = self.run_build({})
        self.assertEqual(calls, [(1, (0,)), (3, (4,)), (4, (6,)), (2, (2,))])
        self.assertEqual(result["n_asu"], 4)
        self.assertIn("merged_first=none", log)
        self.assertIn("priority=unemployment_ascending", log)
        self.assertIn("lowest total unemployment first", log)


if __name__ == "__main__":
    unittest.main()
