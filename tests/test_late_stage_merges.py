"""Exercise late-stage commits through the real build loop with controlled solves."""
import contextlib
import io
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class LateStageMergeTest(unittest.TestCase):
    def test_regional_exchange_installs_custom_portfolio(self):
        with patch.object(
            solver, "_configure_asu_solver_portfolio",
            wraps=solver._configure_asu_solver_portfolio,
        ) as configure:
            units, status = solver._solve_regional_exchange(
                [[0], [1]], [0, 1], [[1], [0]],
                np.array([10, 10]), np.array([0, 0]),
                np.array([10000, 10000]), .1, 10000, 2, 2,
            )
        self.assertIn(status, ("OPTIMAL", "FEASIBLE"))
        self.assertEqual(units, [[0], [1]])
        configure.assert_called_once()
        params, workers = configure.call_args.args
        self.assertEqual(workers, 2)
        self.assertEqual(params.num_search_workers, 2)
        self.assertEqual(list(params.subsolvers), solver._asu_full_subsolvers(2))

    def run_build(self, mode, *, merge_adjacent=True, max_nodes=None, joint_improve=True):
        u = [20, 10 if mode == "residual" else 1, 1, 10, 1, 1]
        emp = [0, 0 if mode == "residual" else 50, 50, 0, 50, 50]
        nb = [[j for j in (i-1, i+1) if 0 <= j < 6] for i in range(6)]
        frame = pd.DataFrame({
            "geoid": list(map(str, range(6))),
            "tract_ASU_unemp": u, "tract_ASU_emp": emp,
            # Encode global IDs so mock solves can translate local windows.
            "tract_pop2024": [10000+i for i in range(6)],
        })
        prepared = dict(
            connectivity_free_standalone_asus=[[0], [3]],
            hint_valid=False, hint_improved=[], hint_obj_val=0,
            hint_source="test", n_contracted=6, root_component=[0],
        )
        events, callbacks = [], []
        exchanged_once = False
        screened = 0

        def screen(*args, **kwargs):
            nonlocal screened
            screened += 1
            # Seed once; leave the test's remaining opportunity for residual search.
            return screened <= 2

        def solve(**kwargs):
            window = [int(p)-10000 for p in kwargs["P_g"]]
            hint = [window[i] for i in kwargs["hint"]]
            if "incumbent_interrupt_callback" in kwargs:
                self.assertIsNone(kwargs["incumbent_interrupt_callback"])
                self.assertTrue(callable(kwargs["incumbent_report_callback"]))
                events.append(("polish", tuple(hint)))
                callbacks.append(kwargs["incumbent_interrupt_callback"])
                selected = hint
                if mode == "preview" and hint == [0]:
                    # This temporary selection touches ASU 3, but the solve's
                    # returned selection does not. Only that return may merge.
                    kwargs["incumbent_report_callback"](
                        [window.index(i) for i in [0, 1, 2]], 22,
                    )
                    events.append(("touching_preview", (0, 1, 2)))
                if mode == "residual":
                    if hint == [0, 1]:
                        selected = [0, 1, 2]  # Touch ASU 3 and restart again.
                    elif hint == [0, 1, 2, 3]:
                        selected = [0, 1, 2, 3, 4]  # Consume a queued residual tract.
                return solver.CpsatResult(
                    [window.index(i) for i in selected], kwargs["root_local"],
                    sum(u[i] for i in selected), "FEASIBLE",
                )
            events.append(("takeover", tuple(hint)))
            if mode == "takeover":
                return solver.CpsatResult([0, 1, 2], 0, 22, "FEASIBLE")
            return None

        def exchange(assignments, *args, **kwargs):
            nonlocal exchanged_once
            self.assertTrue(kwargs["stop_after_gain"])
            events.append(("exchange", tuple(assignments)))
            changed = assignments.copy()
            if mode == "exchange" and not exchanged_once:
                changed[1:3] = changed[0]
                exchanged_once = True
            return changed

        def residual(nodes, *args, **kwargs):
            events.append(("residual", tuple(nodes)))
            if mode == "residual" and 1 in nodes:
                return [1], "OPTIMAL"
            return [], "INFEASIBLE"

        def joint(units, nodes, *args, **kwargs):
            events.append(("touching_joint", tuple(map(tuple, units))))
            self.assertTrue(kwargs["allow_seed_consolidation"])
            self.assertIsNone(kwargs["max_groups"])
            combined = sorted(v for unit in units for v in unit)
            if not joint_improve or (max_nodes is not None and len(combined) > max_nodes):
                return units, "OPTIMAL"
            return [combined] + [[] for _ in units[1:]], "FEASIBLE"

        log = io.StringIO()
        with (
            TemporaryDirectory() as progress_dir,
            patch.object(solver, "can_hit_tau", side_effect=screen),
            patch.object(solver, "_prepare_window_hint", return_value=prepared),
            patch.object(solver, "_partition_standalone_expansion_territories",
                         side_effect=lambda seeds, *a, **kw: [list(s) for s in seeds]),
            patch.object(solver, "solve_one_asu_cpsat", side_effect=solve),
            patch.object(solver, "_regional_exchange_pass", side_effect=exchange),
            patch.object(solver, "_solve_regional_exchange", side_effect=joint),
            patch.object(solver, "_merge_touching_asu_units",
                         side_effect=AssertionError("partition must not auto-merge")),
            patch.object(solver, "_search_unassigned_asu", side_effect=residual),
            contextlib.redirect_stdout(log),
        ):
            result = solver.build_many_asus_cpsat(
                frame, nb, .1, 10000, max_asus=3 if mode == "residual" else 2,
                workers=1, verbose=True, full_graph_window=True,
                harvest_connectivity_free_asus=True, final_consolidation=False,
                standalone_expansion_time_limit=1, final_asu_polish_time_limit=1,
                merge_adjacent=merge_adjacent, max_nodes_per_asu=max_nodes,
                combine_capped_asus=False,
                progress_out_path=str(Path(progress_dir) / "progress.json"),
            )
        ids = np.array(result["asu_id"])
        for k in np.unique(ids[ids > 0]):
            self.assertTrue(solver.component_ok(
                np.flatnonzero(ids == k).tolist(), np.array(u), np.array(emp),
                frame["tract_pop2024"].to_numpy(), .1, 10000, nb,
                max_nodes=max_nodes,
            ))
        return result, events, callbacks, log.getvalue()

    def test_merging_disabled_does_not_interrupt_polishing(self):
        result, events, callbacks, log = self.run_build("takeover", merge_adjacent=False)
        self.assertTrue(callbacks)
        self.assertTrue(all(callback is None for callback in callbacks))
        self.assertEqual(result["asu_id"], [1, 1, 1, 2, -1, -1])
        self.assertNotIn(("polish", (0, 1, 2, 3)), events)
        self.assertIn("incumbent_merge_check=disabled", log)
        self.assertNotIn("SINGLE_ASU_TAKEOVER_MERGE", log)

    def test_takeover_merges_and_restarts_polish(self):
        result, events, callbacks, log = self.run_build("takeover")
        self.assertEqual(result["asu_id"], [1, 1, 1, 1, -1, -1])
        self.assertTrue(callbacks)
        self.assertTrue(all(callback is None for callback in callbacks))
        self.assertIn("incumbent_merge_check=disabled merge_check=after_solve", log)
        self.assertLess(events.index(("takeover", (0,))),
                        events.index(("polish", (0, 1, 2, 3))))
        self.assertIn("PARTITION_TOUCHING_JOINT source=SINGLE_ASU_TAKEOVER_MERGE", log)
        self.assertIn("groups_before=2 groups_after=1", log)

    def test_exchange_settles_merge_before_next_exchange(self):
        result, events, _, log = self.run_build("exchange")
        self.assertEqual(result["asu_id"], [1, 1, 1, 1, -1, -1])
        exchanges = [i for i, event in enumerate(events) if event[0] == "exchange"]
        restarted = events.index(("polish", (0, 1, 2, 3)))
        self.assertLess(exchanges[0], restarted)
        self.assertLess(restarted, exchanges[1])
        self.assertIn("PARTITION_TOUCHING_JOINT source=REGIONAL_EXCHANGE_MERGE", log)

    def test_residual_merge_rebuilds_queue_after_polishing(self):
        result, events, _, log = self.run_build("residual")
        self.assertEqual(result["asu_id"], [1, 1, 1, 1, 1, -1])
        self.assertEqual([event[1] for event in events if event[0] == "residual"],
                         [(1, 2), (5,)])
        self.assertIn(("polish", (0, 1)), events)
        self.assertIn(("polish", (0, 1, 2, 3)), events)
        self.assertIn("PARTITION_TOUCHING_JOINT source=FINAL_RESIDUAL_MERGE", log)
        self.assertIn("PARTITION_TOUCHING_JOINT source=FINAL_POLISH_MERGE", log)
        self.assertTrue(result["residual_check"]["exhausted"])

    def test_late_merge_respects_tract_limit(self):
        result, events, _, log = self.run_build("takeover", max_nodes=3)
        self.assertEqual(result["asu_id"], [1, 1, 1, 2, -1, -1])
        self.assertNotIn(("polish", (0, 1, 2, 3)), events)
        self.assertIn("accepted=0 groups_before=2 groups_after=2", log)

    def test_nonimproving_late_joint_solve_keeps_touching_asus_separate(self):
        result, events, _, log = self.run_build("takeover", joint_improve=False)
        self.assertEqual(result["asu_id"], [1, 1, 1, 2, -1, -1])
        self.assertIn(("touching_joint", ((0, 1, 2), (3,))), events)
        self.assertNotIn(("polish", (0, 1, 2, 3)), events)
        self.assertIn("accepted=0 groups_before=2 groups_after=2", log)

    def test_unchanged_late_stages_do_not_repeat_polish(self):
        result, events, _, log = self.run_build("unchanged")
        self.assertEqual([event[1] for event in events if event[0] == "polish"],
                         [(0,), (3,)])
        self.assertEqual(result["n_asu"], 2)
        self.assertNotIn("REGIONAL_EXCHANGE_MERGE", log)

    def test_touching_preview_does_not_merge_before_final_result(self):
        result, events, _, log = self.run_build("preview")
        self.assertIn(("touching_preview", (0, 1, 2)), events)
        self.assertEqual(result["asu_id"], [1, -1, -1, 2, -1, -1])
        self.assertNotIn("[STAGE] FINAL_POLISH_MERGE", log)


if __name__ == "__main__":
    unittest.main()
