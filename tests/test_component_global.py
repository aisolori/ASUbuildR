"""Component-global validity, exactness and bounded-run regression tests."""
import contextlib
import io
import itertools
from pathlib import Path
import random
import sys
import tempfile
import unittest
from unittest.mock import patch

import numpy as np
from ortools.sat.python import cp_model

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'inst' / 'python'))
import asu_cpsat as shared
from asu_component_global import solve_component_global, _components


class ComponentGlobalTest(unittest.TestCase):
    def run_case(self, nb, u, emp, pop, tau=.5, pop_thresh=5, **kwargs):
        result = solve_component_global(
            nb, u, emp, pop, tau, pop_thresh,
            time_limit=kwargs.pop('time_limit', 15), workers=1, **kwargs)
        ids = np.asarray(result['asu_id'])
        self.assertEqual(len(ids), len(nb))
        self.assertEqual(result['n_asu'], len(set(ids[ids > 0])))
        self.assertEqual(result['total_unemp'], sum(u[i] for i in range(len(nb)) if ids[i] > 0))
        for label in set(ids[ids > 0]):
            self.assertTrue(shared.component_ok(
                np.flatnonzero(ids == label).tolist(), np.asarray(u),
                np.asarray(emp), np.asarray(pop), tau, pop_thresh, nb))
        self.assertGreaterEqual(result['upper_bound'], result['total_unemp'])
        self.assertEqual(result['absolute_gap'], result['upper_bound'] - result['total_unemp'])
        self.assertEqual(result['optimal'], result['absolute_gap'] == 0)
        return result

    @staticmethod
    def brute_force(nb, u, emp, pop, tau, pop_thresh):
        best = 0
        for chosen in itertools.product((False, True), repeat=len(nb)):
            if all(shared.component_ok(nodes, np.asarray(u), np.asarray(emp),
                                       np.asarray(pop), tau, pop_thresh, nb)
                   for nodes in _components(nb, chosen)):
                best = max(best, sum(u[i] for i, value in enumerate(chosen) if value))
        return best

    def test_component_repair_cuts_exclude_economic_subsidy(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5])
        self.assertEqual(result['total_unemp'], 5)
        self.assertTrue(result['optimal'])
        self.assertGreaterEqual(result['cuts'], 1)
        self.assertGreaterEqual(result['rounds'], 2)

    def test_two_valid_components_capture_more_than_a_single_parent(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 8], [0, 100, 0], [5, 1, 5])
        self.assertEqual(result['total_unemp'], 13)
        self.assertEqual(result['n_asu'], 2)
        self.assertTrue(result['optimal'])

    def test_brute_force_tiny_graphs(self):
        rng = random.Random(9426)
        for case in range(35):
            n = rng.randrange(3, 8)
            nb = [[] for _ in range(n)]
            for left in range(n):
                for right in range(left + 1, n):
                    if rng.random() < .4:
                        nb[left].append(right)
                        nb[right].append(left)
            u = [rng.randrange(0, 8) for _ in range(n)]
            emp = [rng.randrange(0, 20) for _ in range(n)]
            pop = [rng.randrange(1, 9) for _ in range(n)]
            tau, minimum = rng.choice((.2, .5, .645)), rng.randrange(3, 15)
            with self.subTest(case=case):
                optimum = self.brute_force(nb, u, emp, pop, tau, minimum)
                result = self.run_case(nb, u, emp, pop, tau, minimum)
                self.assertTrue(result['optimal'])
                self.assertEqual(result['total_unemp'], optimum)
                self.assertEqual(result['upper_bound'], optimum)

    def test_population_and_zero_labor_components_cannot_sneak_through(self):
        for tau in (.2, 0.0):
            result = self.run_case([[], [], []], [4, 0, 2], [0, 0, 0], [1, 100, 6], tau, 5)
            self.assertEqual(result['total_unemp'], 2)
            self.assertTrue(result['optimal'])
            self.assertLess(result['asu_id'][0], 0)
            # Zero-objective qualifying groups need not be returned at tau=0.
            if tau > 0:
                self.assertLess(result['asu_id'][1], 0)

    def test_rate_threshold_uses_shared_exact_integer_rule(self):
        for employed, expected in ((1871, 129), (1872, 0)):
            result = self.run_case([[]], [129], [employed], [10000], .0645, 10000)
            self.assertEqual(result['total_unemp'], expected)
            self.assertTrue(result['optimal'])

    def test_warm_start_is_retained_at_zero_budget(self):
        publish = []
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 8], [0, 100, 0], [5, 1, 5],
                               initial_asu_id=[7, -1, -1], time_limit=0,
                               publish=lambda *args: publish.append(args))
        self.assertEqual(result['total_unemp'], 5)
        self.assertEqual(result['status'], 'TIME_LIMIT')
        self.assertFalse(result['optimal'])
        self.assertEqual(result['upper_bound'], 13)
        self.assertEqual(publish, [])

    def test_warm_start_does_not_freeze_parent(self):
        # Releasing the central low-rate tract permits all U at the two ends.
        nb = [[1], [0, 2], [1, 3], [2]]
        u, emp, pop = [10, 0, 4, 8], [0, 8, 8, 0], [5, 1, 5, 5]
        result = self.run_case(nb, u, emp, pop, initial_asu_id=[1, 1, -1, -1])
        self.assertTrue(result['optimal'])
        self.assertEqual(result['total_unemp'], self.brute_force(nb, u, emp, pop, .5, 5))
        self.assertGreater(result['total_unemp'], 10)

    def test_preexisting_stop_retains_valid_baseline(self):
        with tempfile.TemporaryDirectory() as folder:
            flag = Path(folder) / 'stop'
            flag.touch()
            result = self.run_case([[1], [0, 2], [1]], [5, 0, 8], [0, 100, 0], [5, 1, 5],
                                   initial_asu_id=[1, -1, -1], stop_path=str(flag))
        self.assertEqual(result['status'], 'STOPPED')
        self.assertEqual(result['total_unemp'], 5)
        self.assertEqual(result['rounds'], 0)

    def test_skip_interrupts_only_current_round(self):
        with tempfile.TemporaryDirectory() as folder:
            flag = Path(folder) / 'skip'
            factory = shared._new_asu_solver
            calls = []

            def make_solver():
                result = factory()
                if not calls:
                    flag.touch()
                calls.append(result)
                return result

            with patch.object(shared, '_new_asu_solver', side_effect=make_solver):
                result = self.run_case([[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5],
                                       skip_path=str(flag))
            self.assertFalse(flag.exists())
        self.assertTrue(result['optimal'])
        self.assertGreaterEqual(len(calls), 2)

    def test_previews_are_valid_strict_gains_above_imported_baseline(self):
        nb, u, emp, pop = [[1], [0, 2], [1]], [5, 0, 8], [0, 100, 0], [5, 1, 5]
        seen = []

        def publish(ids, phase, metadata):
            chosen = [label > 0 for label in ids]
            for nodes in _components(nb, chosen):
                self.assertTrue(shared.component_ok(nodes, np.asarray(u), np.asarray(emp),
                                                    np.asarray(pop), .5, 5, nb))
            value = sum(u[i] for i, selected in enumerate(chosen) if selected)
            self.assertGreater(value, seen[-1] if seen else 5)
            self.assertEqual(value, metadata['total_unemp'])
            self.assertTrue(phase.startswith('component_global'))
            seen.append(value)

        result = self.run_case(nb, u, emp, pop, initial_asu_id=[1, -1, -1], publish=publish)
        self.assertEqual(seen[-1], result['total_unemp'])

    def test_publish_failure_is_not_silently_committed(self):
        def failure(*args):
            raise OSError('checkpoint unavailable')
        with self.assertRaisesRegex(OSError, 'checkpoint unavailable'):
            self.run_case([[]], [5], [0], [5], publish=failure)

    def test_relative_gap_is_against_validated_incumbent(self):
        result = self.run_case([[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5], rel_gap=.25)
        self.assertEqual(result['status'], 'GAP_LIMIT')
        self.assertFalse(result['optimal'])
        self.assertEqual(result['total_unemp'], 5)
        self.assertEqual(result['upper_bound'], 6)
        self.assertEqual(result['rounds'], 0)

    def test_rounds_are_capped_and_quiet(self):
        factory = shared._new_asu_solver
        engines = []

        def make_solver():
            result = factory()
            engines.append(result)
            return result

        output = io.StringIO()
        with patch.object(shared, '_new_asu_solver', side_effect=make_solver), contextlib.redirect_stdout(output):
            self.run_case([[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5], verbose=True)
        self.assertTrue(engines)
        for engine in engines:
            params = engine.parameters
            self.assertLessEqual(params.max_time_in_seconds, 5)
            self.assertFalse(params.log_search_progress)
            self.assertFalse(params.log_to_stdout)
            self.assertFalse(params.log_to_response)
            self.assertEqual(params.relative_gap_limit, 0)
        self.assertIn('COMPONENT_GLOBAL_CUT_ROUND', output.getvalue())
        self.assertNotIn('Starting CP-SAT solver', output.getvalue())

    def test_unknown_zero_bound_is_not_a_false_proof(self):
        class UnknownSolver:
            parameters = cp_model.CpSolver().parameters

            def Solve(self, model, callback):
                return cp_model.UNKNOWN

            def BestObjectiveBound(self):
                raise AssertionError('Do not use UNKNOWN bounds')

            def StatusName(self, status):
                return 'UNKNOWN'

            def StopSearch(self):
                pass

        with patch.object(shared, '_new_asu_solver', return_value=UnknownSolver()):
            result = self.run_case([[1], [0, 2], [1]], [5, 0, 1], [0, 100, 2], [5, 1, 5], time_limit=.05)
        self.assertFalse(result['optimal'])
        self.assertEqual(result['upper_bound'], 6)
        self.assertEqual(result['total_unemp'], 5)

    def test_empty_and_no_positive_unemployment_are_optimal(self):
        for nb, u, emp, pop in (([], [], [], []), ([[]], [0], [100], [10])):
            result = self.run_case(nb, u, emp, pop)
            self.assertTrue(result['optimal'])
            self.assertEqual(result['n_asu'], 0)
            self.assertEqual(result['total_unemp'], 0)

    def test_rejects_invalid_inputs_and_baselines(self):
        cases = (
            dict(nb=[[]], u=[-1], E=[0], P=[5]),
            dict(nb=[[]], u=[1.5], E=[0], P=[5]),
            dict(nb=[[]], u=[2**53], E=[0], P=[5]),
            dict(nb=[[]], u=[1], E=[2**61], P=[5]),
            dict(nb=[[1], []], u=[1, 1], E=[0, 0], P=[5, 5]),
            dict(nb=[[]], u=[1], E=[10], P=[5], initial_asu_id=[1]),
            dict(nb=[[]], u=[1], E=[0], P=[5], initial_asu_id=[float('inf')]),
            dict(nb=[[]], u=[1], E=[0], P=[5], initial_asu_id=[-2]),
        )
        for kwargs in cases:
            with self.subTest(kwargs=kwargs), self.assertRaises(ValueError):
                solve_component_global(tau=.5, pop_thresh=5, workers=1, **kwargs)


if __name__ == '__main__':
    unittest.main()
