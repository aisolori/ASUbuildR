"""Legacy single-ASU graph-cut hint generation and bound-stall behavior."""
import contextlib
import io
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class LegacyGraphCutHintTest(unittest.TestCase):
    def test_graph_cut_mode_supersedes_connectivity_free_repair(self):
        relaxed_candidate = solver.ConnectivityFreeCandidate([0, 2], 30)
        relaxed = solver.ConnectivityFreeResult(
            selected=[0, 2],
            objective=30,
            best_bound=30.0,
            status="OPTIMAL",
            solve_seconds=0.0,
            candidates=[relaxed_candidate],
        )
        graph_result = solver.CpsatResult([0], 0, 10, "FEASIBLE")

        with (
            patch.object(
                solver, "solve_connectivity_free_relaxation", return_value=relaxed
            ),
            patch.object(
                solver,
                "repair_connectivity_free_selection",
                side_effect=AssertionError("heuristic repair ran"),
            ) as repair,
            patch.object(
                solver, "solve_asu_graph_cut_only", return_value=graph_result
            ) as graph_cut,
        ):
            result = solver._prepare_window_hint(
                [[1], [0, 2], [1]],
                np.array([10, 1, 20]),
                np.array([0, 100, 0]),
                np.array([10, 10, 10]),
                0.05,
                10,
                0,
                use_connectivity_free_repair=True,
                use_graph_cut_repair=True,
            )

        repair.assert_not_called()
        graph_cut.assert_called_once()
        self.assertTrue(result["hint_valid"])

    def test_cut_loop_stalls_on_bound_not_components_and_never_repairs(self):
        result, created, log = self.run_cut_loop([100, 100, 90, 90, 90, 90], 3)

        self.assertEqual(len(created), 6)
        self.assertIn(0, result.sel_idx_local)
        self.assertTrue(
            solver.component_ok(
                result.sel_idx_local,
                np.array([10, 1, 20]),
                np.array([0, 100, 0]),
                np.array([10, 10, 10]),
                0.05,
                10,
                [[1], [0, 2], [1]],
            )
        )
        self.assertIn("upper_bound=90", log)
        self.assertIn("upper_bound_stall=3/3", log)
        self.assertIn("stop_reason=UPPER_BOUND_STALL", log)
        self.assertNotIn("[repair]", log)
        self.assertTrue(all(0 < item.parameters.max_time_in_seconds <= 10 for item in created))

    def run_cut_loop(self, bounds, stall_rounds):
        created = []

        class FakeSolver:
            def __init__(self):
                self.parameters = type("Parameters", (), {})()
                self.bound = bounds[len(created)]
                created.append(self)

            def Solve(self, unused_model, unused_callback=None):
                return solver.cp_model.FEASIBLE

            def StatusName(self, unused_status):
                return "FEASIBLE"

            def BooleanValue(self, var):
                return var.name in ("x_0", "x_2")

            def BestObjectiveBound(self):
                return self.bound

        output = io.StringIO()
        with (
            patch.object(solver.cp_model, "CpSolver", FakeSolver),
            patch.object(solver, "_configure_asu_solver_portfolio"),
            patch.object(
                solver,
                "repair_connectivity_free_selection",
                side_effect=AssertionError("heuristic repair ran"),
            ),
            contextlib.redirect_stdout(output),
        ):
            result = solver.solve_asu_graph_cut_only(
                [[1], [0, 2], [1]],
                np.array([10, 1, 20]),
                np.array([0, 100, 0]),
                np.array([10, 10, 10]),
                0.05,
                10,
                0,
                time_limit=120,
                workers=2,
                log=True,
                hint=[0],
                stall_rounds=stall_rounds,
            )
        return result, created, output.getvalue()


if __name__ == "__main__":
    unittest.main()
