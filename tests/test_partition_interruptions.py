"""Exercise Skip fairness and Stop cleanup in the real partition build loop."""
import contextlib
import io
from pathlib import Path
import sys
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class PartitionInterruptionsTest(unittest.TestCase):
    def run_build(self, action):
        frame = pd.DataFrame({
            "geoid": list(map(str, range(8))), "tract_ASU_unemp": [30, 20, 5, 0, 0, 10, 0, 1],
            "tract_ASU_emp": [0, 0, 0, 10000, 10000, 0, 10000, 1000],
            "tract_pop2024": [10000]*8,
        })
        nb = [[j for j in (i-1, i+1) if 0 <= j < 8] for i in range(8)]
        if action == "stop_single":
            nb[:3] = [[1, 2], [0], [0, 3]]
        seeds = [[0], [1], [5], [7]]
        if action == "stop_no_valid":
            seeds = [[7]]
        prepared = dict(connectivity_free_standalone_asus=seeds, hint_valid=False,
                        hint_improved=[], hint_obj_val=0, hint_source="test",
                        n_contracted=8, root_component=[0])
        events, output = [], io.StringIO()
        original_note_turn = solver._TouchingJointDeferrals.note_turn

        def note_turn(state, units):
            events.append(("turn", tuple(map(tuple, units))))
            original_note_turn(state, units)

        with TemporaryDirectory() as folder:
            stop = Path(folder) / "stop"
            def joint(units, nodes, *args, **kwargs):
                events.append(("joint", tuple(map(tuple, units))))
                if action == "skip" and sum(event[0] == "joint" for event in events) == 1:
                    return [[0], [1, 2]], "SKIPPED"
                stop.touch()
                return ([[0], [1, 2]] if action == "stop_joint" else units), "STOPPED"

            def single(**kwargs):
                self.assertEqual(action, "stop_single")
                events.append(("single", ()))
                stop.touch()
                return solver.CpsatResult([0, 1], 0, 35, "STOPPED")

            def territories(units, *args, **kwargs):
                if action == "stop_no_valid":
                    stop.touch()
                return [[0, 2] if action == "stop_single" and unit == [0] else list(unit)
                        for unit in units]

            with (patch.object(solver, "_prepare_window_hint", return_value=prepared),
                  patch.object(solver, "_partition_standalone_expansion_territories", side_effect=territories),
                  patch.object(solver, "solve_one_asu_cpsat", side_effect=single),
                  patch.object(solver, "_solve_regional_exchange", side_effect=joint),
                  patch.object(solver._TouchingJointDeferrals, "note_turn", new=note_turn),
                  contextlib.redirect_stdout(output)):
                result = solver.build_many_asus_cpsat(
                    frame, nb, .2, 10000, max_asus=4, workers=2, verbose=True,
                    full_graph_window=True, harvest_connectivity_free_asus=True,
                    harvest_all_connectivity_free_components=True,
                    standalone_expansion_time_limit=5, final_asu_polish_time_limit=0,
                    combine_capped_asus=False, stop_flag_path=str(stop))
        self.assertEqual(result["n_asu"], 0 if action == "stop_no_valid" else 3)
        self.assertEqual(result["asu_id"][7], -1)
        self.assertEqual(result["residual_check"]["status"], "STOPPED")
        return result, events, output.getvalue()

    def test_skip_waits_for_valid_and_weak_peers_before_retry(self):
        _, events, log = self.run_build("skip")
        joints = [i for i, event in enumerate(events) if event[0] == "joint"]
        self.assertEqual(len(joints), 2)
        turns = events[joints[0]+1:joints[1]]
        self.assertIn(("turn", ((5,),)), turns)
        self.assertIn(("turn", ((7,),)), turns)
        self.assertIn("status=DEFERRED_SKIP", log)
        self.assertNotIn("PARTITION_EXPANSION round=3", log)

    def test_stop_after_joint_gain_keeps_result_without_restart_or_false_rejections(self):
        result, events, log = self.run_build("stop_joint")
        self.assertEqual(result["asu_id"][1], result["asu_id"][2])
        self.assertEqual(sum(event[0] == "joint" for event in events), 1)
        self.assertNotIn("PARTITION_EXPANSION round=2", log)
        self.assertIn("outcome=stopped scheduled=4 attempted=0 unattempted=4 valid=3", log)
        self.assertIn("unresolved_seeds=1 joint_updates=1", log)
        self.assertNotIn("outcome=converged", log)
        self.assertNotIn("excluded before merging", log)
        self.assertNotIn("rejected=", log)

    def test_stop_during_individual_expansion_counts_only_attempted_window(self):
        result, events, log = self.run_build("stop_single")
        self.assertEqual(result["asu_id"][0], result["asu_id"][2])
        self.assertEqual(sum(event[0] == "single" for event in events), 1)
        self.assertFalse(any(event[0] == "joint" for event in events))
        self.assertIn("outcome=stopped scheduled=4 attempted=1 unattempted=3 valid=3", log)
        self.assertNotIn("PARTITION_EXPANSION round=2", log)
        self.assertNotIn("outcome=converged", log)

    def test_stop_without_valid_seed_does_not_fall_through_to_main_solve(self):
        _, events, log = self.run_build("stop_no_valid")
        self.assertFalse(any(event[0] in ("single", "joint") for event in events))
        self.assertIn("outcome=stopped scheduled=1 attempted=0 unattempted=1 valid=0", log)
        self.assertIn("unresolved_seeds=1", log)
        self.assertNotIn("outcome=converged", log)


if __name__ == "__main__":
    unittest.main()
