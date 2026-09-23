"""Reverse-prune progress is throttled and does not change tract selection."""
import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class ReversePruneProgressTest(unittest.TestCase):
    def test_periodic_progress_preserves_result(self):
        nb = [[1, 2, 3, 4], [0], [0], [0], [0]]
        args = (nb, np.array([10, 0, 0, 0, 0]), np.array([0, 100, 100, 100, 100]),
                np.full(5, 10000), .5, 10000, 0)
        quiet = io.StringIO()
        with contextlib.redirect_stdout(quiet):
            expected = solver.reverse_prune_hint(*args)
        self.assertEqual(quiet.getvalue(), "")
        clock = [0.0]
        articulation = solver._articulation_points

        def step(*values):
            clock[0] += 4.0
            return articulation(*values)

        output = io.StringIO()
        with (patch.object(solver.time, "monotonic", side_effect=lambda: clock[0]),
              patch.object(solver, "_articulation_points", side_effect=step),
              contextlib.redirect_stdout(output)):
            actual = solver.reverse_prune_hint(*args, log=True)
        self.assertEqual(actual, expected)
        self.assertEqual(actual, [0])
        lines = output.getvalue().splitlines()
        self.assertEqual(len(lines), 3)
        self.assertIn("phase=start", lines[0])
        self.assertIn("phase=progress root=0 elapsed=12.0s removed=3 remaining=2", lines[1])
        self.assertIn("UR=9.0909% target=50.0000%", lines[1])
        self.assertIn("phase=complete root=0 elapsed=16.0s removed=4 remaining=1", lines[2])
        self.assertIn("reason=threshold_reached", lines[2])

    def test_blocked_prune_reports_reason_without_claiming_feasibility(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            result = solver.reverse_prune_hint(
                [[1], [0]], np.array([0, 10]), np.array([100, 0]),
                np.array([10000, 10000]), .5, 10000, 0, log=True)
        self.assertEqual(result, [0, 1])
        lines = output.getvalue().splitlines()
        self.assertEqual(len(lines), 2)
        self.assertIn("removed=0 remaining=2", lines[-1])
        self.assertIn("reason=no_valid_removal", lines[-1])

    def test_already_at_threshold_reports_immediate_completion(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            result = solver.reverse_prune_hint(
                [[]], np.array([10]), np.array([0]), np.array([10000]),
                .5, 10000, 0, log=True)
        self.assertEqual(result, [0])
        self.assertEqual(len(output.getvalue().splitlines()), 2)
        self.assertIn("reason=threshold_reached", output.getvalue())


if __name__ == "__main__":
    unittest.main()
