"""Symmetry settings reach real solver phases with or without custom workers."""
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as s


class SolverSymmetryTest(unittest.TestCase):
    def capture(self, engine, model, *args, **kwargs):
        self.assertEqual(engine.parameters.symmetry_level, 3)
        self.assertEqual(engine.parameters.symmetry_detection_deterministic_time_limit, 1.0)
        self.phases.append(any(v.name.startswith('f_') for v in model.Proto().variables))
        engine.parameters.log_to_stdout = False
        return self.original(engine, model, *args, **kwargs)

    def setUp(self):
        self.original = s.cp_model.CpSolver.Solve
        self.phases = []
        self.capture_solve = lambda engine, model, *args, **kwargs: self.capture(engine, model, *args, **kwargs)

    def test_cut_and_exact_phases_keep_symmetry_with_portfolio_on_or_off(self):
        for custom in (False, True):
            with self.subTest(custom=custom), patch.object(s.cp_model.CpSolver, 'Solve', new=self.capture_solve):
                self.phases.clear()
                result = s.solve_one_asu_cpsat(
                    [[1], [0, 2], [1, 3], [2]], np.array([10, 1, 5, 2]),
                    np.array([0, 50, 0, 30]), np.array([10000]*4), .2, 10000, 0,
                    time_limit=10, workers=2, hint=[0], scout_before_cuts=False,
                    configure_subsolvers=custom, deterministic_ties=True,
                    use_small_root_separators=False, use_profitable_component_closure=False)
                self.assertEqual((result.obj, result.status), (16, 'OPTIMAL'))
                self.assertIn(False, self.phases)
                self.assertIn(True, self.phases)

    def test_feasibility_screen_uses_bounded_symmetry(self):
        with patch.object(s.cp_model.CpSolver, 'Solve', new=self.capture_solve):
            status = s._connectivity_free_feasibility(
                np.array([20, 20]), np.array([80, 80]), np.array([6000, 6000]),
                .1, 10000, seconds=1, workers=1)
        self.assertIn(status, ('OPTIMAL', 'FEASIBLE'))
        self.assertEqual(len(self.phases), 1)


if __name__ == '__main__':
    unittest.main()
