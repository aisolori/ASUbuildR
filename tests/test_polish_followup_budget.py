"""Exercise the real nested scheduler with simulated long polish attempts."""
import ast
import contextlib
import io
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest.mock import Mock

import numpy as np


class PolishFollowupBudgetTest(unittest.TestCase):
    def test_long_followups_finish_queue_and_keep_three_round_limit(self):
        # Isolate scheduling from expensive solver/model construction, while
        # executing the actual production function, not a copied implementation.
        path = Path(__file__).resolve().parents[1] / "inst/python/asu_cpsat.py"
        tree = ast.parse(path.read_text(encoding="utf-8"))
        scheduler = next(node for node in ast.walk(tree)
                         if isinstance(node, ast.FunctionDef)
                         and node.name == "_run_final_polish")
        scheduler.body = [
            ast.copy_location(ast.Global(names=node.names), node)
            if isinstance(node, ast.Nonlocal) else node
            for node in scheduler.body
        ]
        module = ast.fix_missing_locations(ast.Module(
            body=[scheduler], type_ignores=[]))
        assignments = np.array([1, 2, -1])
        calls = []
        elapsed = [0.0]

        def polish(label, position, count, round_number, seconds=None):
            calls.append((label, round_number, seconds))
            elapsed[0] += 301.0  # Every attempt exceeds the former shared cap.
            # Simulate changing territory to avoid the independent cycle guard.
            assignments[2] -= 1
            return True

        env = dict(
            np=np, time=SimpleNamespace(monotonic=lambda: elapsed[0]),
            polish_round=0, polish_followup_rounds=0,
            polish_followup_seconds=180.0,  # Detect regressions to old accounting.
            polish_enabled=True, polish_time_limit=600.0,
            stop_flag_path=None, _stop_requested=lambda _: False,
            asu_id=assignments, u=np.array([10, 20, 0]),
            E=np.zeros(3), P=np.ones(3), tau=.2, nb=[[], [], []],
            verbose=True, merge_adjacent=False,
            _polish_asu_order=lambda ids, u: [1, 2],
            touching_sweep=SimpleNamespace(begin=Mock(), pending=False),
            touching_deferrals=SimpleNamespace(note_turn=Mock()),
            _remaining_polish_queue=lambda units, checked: [
                int(assignments[unit[0]]) for unit in units
                if unit[0] not in checked],
            _stage_checking=lambda *args: contextlib.nullcontext(),
            _stage_print=print, _polish_one_asu=polish,
            polish_last_windows={1: set(), 2: set()},
            polish_last_ownership={}, polish_skipped_ids=set(),
            _pick_capacity_root=lambda current, *args: current[0],
            _reachable_polish_window=lambda *args, **kwargs: [0, 1],
        )
        exec(compile(module, str(path), "exec"), env)
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            env["_run_final_polish"]()

        self.assertEqual(calls, [
            (label, round_number, None)
            for round_number in range(1, 5) for label in (1, 2)
        ])
        self.assertEqual(env["polish_followup_rounds"], 3)
        self.assertIn("reason=round_limit time_budget=unlimited", output.getvalue())
        self.assertNotIn("reason=time_budget", output.getvalue())


if __name__ == "__main__":
    unittest.main()
