"""Short partition cuts use a portable portfolio without changing exact flow."""
from itertools import product
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class PartitionCutProfileTest(unittest.TestCase):
    def test_profile_bounds_startup_work_and_keeps_search_diversity(self):
        engine = solver._new_asu_solver()
        params = engine.parameters
        params.max_time_in_seconds = 3.25
        inprocessing = params.use_sat_inprocessing
        solver._configure_partition_cut_solver(params, 46)

        self.assertEqual(params.num_search_workers, 16)
        self.assertTrue(params.cp_model_presolve)
        self.assertEqual(params.linearization_level, 2)
        self.assertEqual(params.symmetry_level, 1)
        self.assertEqual(params.symmetry_detection_deterministic_time_limit, .05)
        self.assertEqual(params.cp_model_probing_level, 1)
        self.assertEqual(params.probing_deterministic_time_limit, .05)
        if hasattr(params, "presolve_probing_deterministic_time_limit"):
            self.assertEqual(params.presolve_probing_deterministic_time_limit, .05)
        self.assertEqual(params.use_sat_inprocessing, inprocessing)
        self.assertEqual(params.max_time_in_seconds, 3.25)
        self.assertEqual(list(params.subsolvers),
                         ["max_lp", "no_lp", "objective_lb_search", "probing"])
        self.assertEqual(params.num_full_subsolvers, 4)
        self.assertFalse(params.subsolver_params)

    def test_profile_discards_long_solve_overrides_without_mutating_other_solver(self):
        flow = solver._new_asu_solver()
        flow.parameters.num_search_workers = 46
        solver._configure_asu_solver_portfolio(flow.parameters, 46)
        before = str(flow.parameters)
        cut = solver._new_asu_solver()
        solver._configure_asu_solver_portfolio(cut.parameters, 46)
        self.assertTrue(cut.parameters.subsolver_params)

        solver._configure_partition_cut_solver(cut.parameters, 8)

        self.assertFalse(cut.parameters.subsolver_params)
        self.assertFalse(cut.parameters.extra_subsolvers)
        self.assertFalse(cut.parameters.filter_subsolvers)
        self.assertEqual(str(flow.parameters), before)
        self.assertEqual(flow.parameters.num_search_workers, 46)
        self.assertEqual(flow.parameters.symmetry_level, 3)
        self.assertEqual(flow.parameters.symmetry_detection_deterministic_time_limit, 1.0)

    def test_small_worker_requests_are_respected(self):
        for requested, effective in ((0, 1), (1, 1), (2, 2), (3, 3), (8, 8)):
            with self.subTest(workers=requested):
                params = solver._new_asu_solver().parameters
                solver._configure_partition_cut_solver(params, requested)
                self.assertEqual(params.num_search_workers, effective)
                self.assertEqual(params.num_full_subsolvers, min(4, effective))

    def test_rebuilding_profile_clears_ignored_workers_and_is_idempotent(self):
        params = solver._new_asu_solver().parameters
        solver._configure_asu_solver_portfolio(params, 46)
        params.ignore_subsolvers.extend(["max_lp", "no_lp", "probing"])

        solver._configure_partition_cut_solver(params, 16)

        self.assertFalse(params.ignore_subsolvers)
        self.assertEqual(list(params.subsolvers),
                         ["max_lp", "no_lp", "objective_lb_search", "probing"])
        first_profile = str(params)
        solver._configure_partition_cut_solver(params, 16)
        self.assertEqual(str(params), first_profile)

    def test_stock_profiles_solve_to_known_optimum_at_all_requested_worker_counts(self):
        profit = [11, 7, 14, 4, 9, 16, 6, 12, 5, 13]
        cost = [4, 3, 5, 2, 4, 6, 2, 5, 3, 5]
        n = len(profit)
        feasible = [
            bits for bits in product((0, 1), repeat=n)
            if bits[0] and sum(a * b for a, b in zip(cost, bits)) <= 17
            and all(bits[i] + bits[(i + 1) % n] <= 1 for i in range(n))
        ]
        optimum = max(sum(a * b for a, b in zip(profit, bits)) for bits in feasible)
        model = solver.cp_model.CpModel()
        x = [model.NewBoolVar(f"x_{i}") for i in range(n)]
        model.Add(x[0] == 1)
        model.Add(sum(a * b for a, b in zip(cost, x)) <= 17)
        for i in range(n):
            model.Add(x[i] + x[(i + 1) % n] <= 1)
        model.Maximize(sum(a * b for a, b in zip(profit, x)))
        for i, var in enumerate(x):
            model.AddHint(var, int(i == 0))

        for requested in (1, 8, 16, 46):
            with self.subTest(workers=requested):
                engine = solver._new_asu_solver()
                solver._configure_partition_cut_solver(engine.parameters, requested)
                solver._configure_cut_round_params(engine.parameters)
                status = engine.Solve(model)
                self.assertEqual(status, solver.cp_model.OPTIMAL, engine.ResponseStats())
                self.assertEqual(round(engine.ObjectiveValue()), optimum)
                self.assertEqual(engine.parameters.num_search_workers, min(16, requested))


if __name__ == "__main__":
    unittest.main()
