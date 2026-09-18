"""Touching neighborhoods prefer safe union, then bounded joint optimization."""
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


def chain(n):
    return [[j for j in (i - 1, i + 1) if 0 <= j < n] for i in range(n)]


class TouchingJointTest(unittest.TestCase):
    def test_attempt_cache_retries_stronger_search_and_reuses_proofs(self):
        cache = solver._TouchingJointAttemptCache()
        key = (((0,), (1,)), (0, 1, 2), None, None, .2, 10000, None)
        cache.remember(key, 5, False, "CUT_DEFERRED")
        self.assertEqual(cache.should_skip(key, 5, False), (True, "budget"))
        self.assertEqual(cache.should_skip(key, 10, False), (False, None))
        self.assertEqual(cache.should_skip(key, 5, True), (False, None))
        cache.remember(key, 10, True, "UNKNOWN")
        self.assertEqual(cache.should_skip(key, 5, True), (True, "budget"))

        proved_key = (((3,), (4,)), (3, 4), None, None, .2, 10000, None)
        cache.remember(proved_key, 1, False, "OPTIMAL")
        self.assertEqual(cache.should_skip(proved_key, 100, True), (True, "proved"))

    def test_cut_only_policy_returns_valid_incumbent_without_flow_solve(self):
        def cut_pass(model, x, roots, nb, u, fallback, *args, **kwargs):
            return fallback, sum(int(u[unit].sum()) for unit in fallback), "FEASIBLE"

        output = io.StringIO()
        with (patch.object(solver, "_joint_connectivity_cut_pass", side_effect=cut_pass),
              patch.object(solver.cp_model.CpSolver, "Solve",
                           side_effect=AssertionError("flow solve must be deferred")),
              contextlib.redirect_stdout(output)):
            groups, status = solver._solve_regional_exchange(
                [[0], [1]], [0, 1, 2], chain(3), np.full(3, 10),
                np.zeros(3, dtype=int), np.full(3, 10000), .2, 10000, 5, 1,
                use_joint_cuts=True, exact_flow_after_cuts=False, log=True,
            )
        self.assertEqual(groups, [[0], [1, 2]])
        self.assertTrue(all(solver.component_ok(
            group, np.full(3, 10), np.zeros(3, dtype=int),
            np.full(3, 10000), .2, 10000, chain(3),
        ) for group in groups))
        self.assertEqual(status, "CUT_DEFERRED")
        self.assertIn("FLOW_DEFERRED", output.getvalue())

    def test_large_touching_component_tries_only_one_pair(self):
        units = [[0], [1], [2], [3]]
        _, solve = self.run_case(
            units, [4, 5, 6], max_cluster_groups=2, max_cluster_attempts=1,
        )
        solve.assert_called_once()
        self.assertEqual(len(solve.call_args.args[0]), 2)

    def test_build_safe_union_avoids_joint_model(self):
        frame = pd.DataFrame(dict(
            geoid=["0", "1"], tract_ASU_unemp=[10, 10],
            tract_ASU_emp=[0, 0], tract_pop2024=[10000, 10000],
        ))
        with patch.object(
            solver, "_solve_regional_exchange",
            side_effect=AssertionError("safe union must precede the joint model"),
        ):
            result = solver.build_many_asus_cpsat(
                frame, chain(2), .1, 10000, max_asus=2,
                initial_asu_id=[1, 2], harvest_connectivity_free_asus=True,
                standalone_expansion_time_limit=0, final_asu_polish_time_limit=0,
                final_consolidation=False, workers=1, verbose=False,
            )
        self.assertEqual(result["n_asu"], 1)
        self.assertEqual(result["asu_id"], [1, 1])

    def test_polish_peers_finish_before_changed_cluster_retries(self):
        frame = pd.DataFrame(dict(geoid=list(map(str, range(6))),
                                  tract_ASU_unemp=[20, 10, 1, 0, 5, 0],
                                  tract_ASU_emp=[0, 0, 0, 1000, 0, 1000],
                                  tract_pop2024=list(range(10000, 10006))))
        events = []
        original = solver._reoptimize_touching_asu_units
        def touching(units, *args, **kw):
            if kw['source'] == 'cross_batch':
                return units, 0  # Focus this regression on the polish sweep.
            return original(units, *args, **kw)
        def joint(units, *args, **kw):
            events.append(('joint', tuple(map(tuple, units))))
            return [[0], [1, 2]], 'FEASIBLE'
        def single(**kw):
            events.append(('single', int(kw['P_g'][kw['root_local']])-10000))
            hint = kw.get('hint')
            return (solver.CpsatResult(hint, kw['root_local'], int(kw['u_g'][hint].sum()), 'FEASIBLE')
                    if hint else None)
        with (patch.object(solver, '_reoptimize_touching_asu_units', side_effect=touching),
              patch.object(solver, '_solve_regional_exchange', side_effect=joint),
              patch.object(solver, '_merge_touching_asu_units',
                           side_effect=lambda units, *args, **kwargs: (units, 0)),
              patch.object(solver, 'solve_one_asu_cpsat', side_effect=single),
              patch.object(solver, '_solve_supernode_polish', side_effect=single),
              patch.object(solver, '_regional_exchange_pass', side_effect=lambda a, *args, **kw: a.copy()),
              patch.object(solver, '_search_unassigned_asu', return_value=([], 'INFEASIBLE'))):
            solver.build_many_asus_cpsat(
                frame, chain(6), .1, 10000, initial_asu_id=[1, 2, -1, -1, 3, -1],
                max_asus=3, harvest_connectivity_free_asus=True, final_consolidation=False,
                standalone_expansion_time_limit=1, final_asu_polish_time_limit=1,
                workers=1, verbose=False)
        joints = [i for i, event in enumerate(events) if event[0] == 'joint']
        self.assertEqual(len(joints), 2, events)
        self.assertIn(('single', 4), events[joints[0]+1:joints[1]], events)

    def test_changed_neighborhood_waits_for_next_sweep(self):
        sweep = solver._TouchingJointSweep()
        sweep.begin()
        cache = set()
        self.run_case([[0], [1]], [2], ([[0], [1, 2]], 'FEASIBLE'),
                      sweep=sweep, attempted=cache)
        _, solve = self.run_case([[0], [1, 2]], [3], sweep=sweep, attempted=cache)
        solve.assert_not_called()
        self.assertTrue(sweep.pending)
        # Unrelated clusters still receive their turn, even if free windows overlap.
        _, solve = self.run_case([[4], [5]], [2, 3, 6], sweep=sweep, attempted=cache)
        solve.assert_called_once()
        sweep.begin()
        _, solve = self.run_case([[0], [1, 2]], [3], sweep=sweep, attempted=cache)
        solve.assert_called_once()

    def test_sweep_follows_absorbed_members_and_preserves_exact_cache(self):
        sweep = solver._TouchingJointSweep()
        sweep.begin()
        sweep.record({0, 1})
        self.assertTrue(sweep.blocks({1, 2}))
        self.assertTrue(sweep.blocks({2, 3}))
        sweep.begin()
        cache = set()
        self.run_case([[0], [1]], [2], sweep=sweep, attempted=cache)
        _, solve = self.run_case([[0], [1]], [2], sweep=sweep, attempted=cache)
        solve.assert_not_called()
        self.assertFalse(sweep.pending)  # Exact hits do not schedule another sweep.

    def test_cross_batch_uses_expansion_stall_and_polish_keeps_general_limit(self):
        frame = pd.DataFrame(dict(geoid=["0", "1"], tract_ASU_unemp=[10, 10],
                                  tract_ASU_emp=[0, 0], tract_pop2024=[10000, 10000]))
        calls = []
        def touching(units, *args, **kwargs):
            calls.append((kwargs["source"], kwargs["incumbent_stall_seconds"]))
            return units, 0
        with (patch.object(solver, "_reoptimize_touching_asu_units", side_effect=touching),
              patch.object(solver, "_merge_touching_asu_units",
                           side_effect=lambda units, *args, **kwargs: (units, 0))):
            solver.build_many_asus_cpsat(
                frame, chain(2), .1, 10000, max_asus=2, initial_asu_id=[1, 2],
                harvest_connectivity_free_asus=True, workers=1, verbose=False,
                standalone_expansion_time_limit=1, final_asu_polish_time_limit=1,
                expansion_incumbent_stall_seconds=7, incumbent_stall_seconds=37,
                configure_subsolvers=False, deterministic_ties=False,
            )
        self.assertIn(("cross_batch", 7), calls)
        polish_calls = [(source, limit) for source, limit in calls if source != "cross_batch"]
        self.assertTrue(polish_calls)
        self.assertTrue(all(limit == 37 for _, limit in polish_calls), calls)

    def run_case(self, units, available, result=None, *, n=7, nb=None, **kwargs):
        args = (units, available, chain(n) if nb is None else nb,
                np.full(n, 10), np.zeros(n, dtype=int), np.full(n, 10000),
                .2, 10000, kwargs.pop("seconds", 5), 4)
        with patch.object(solver, "_solve_regional_exchange",
                          return_value=(units, "UNKNOWN") if result is None else result) as solve:
            answer = solver._reoptimize_touching_asu_units(*args, **kwargs)
        return answer, solve

    def test_window_is_all_reachable_free_tracts_but_excludes_other_asus(self):
        # ASU [5] shields tract 6; the free window reaches > two hops from [1].
        (_, updates), solve = self.run_case([[0], [1], [5]], range(7))
        self.assertEqual(updates, 0)
        self.assertEqual(solve.call_args.args[0], [[0], [1]])
        self.assertEqual(solve.call_args.args[1], [0, 1, 2, 3, 4])
        self.assertEqual(solve.call_args.args[9], 4)
        self.assertTrue(solve.call_args.kwargs["allow_seed_consolidation"])
        self.assertTrue(solve.call_args.kwargs["allow_inactive_seeds"])
        self.assertIsNone(solve.call_args.kwargs["max_groups"])
        self.assertTrue(solve.call_args.kwargs["tighten_model"])
        self.assertTrue(solve.call_args.kwargs["use_joint_cuts"])
        self.assertEqual(solve.call_args.kwargs["stage_prefix"], "PARTITION_TOUCHING_JOINT")

    def test_non_touching_asus_do_not_launch_joint_solve(self):
        answer, solve = self.run_case([[0], [2]], [1, 3, 4])
        self.assertEqual(answer, ([[0], [2]], 0))
        solve.assert_not_called()

    def test_gain_can_keep_touching_asus_separate(self):
        answer, _ = self.run_case([[0], [1]], [2], ([[0], [1, 2]], "OPTIMAL"))
        self.assertEqual(answer, ([[0], [1, 2]], 1))

    def test_equal_coverage_consolidation_is_accepted(self):
        answer, _ = self.run_case([[0], [1]], [], ([[0, 1], []], "FEASIBLE"))
        self.assertEqual(answer, ([[0, 1]], 1))

    def test_no_improvement_or_timeout_does_not_force_union(self):
        for status in ("OPTIMAL", "FEASIBLE", "UNKNOWN", "SKIPPED", "STOPPED"):
            with self.subTest(status=status):
                answer, _ = self.run_case([[0], [1]], [], ([[0], [1]], status))
                self.assertEqual(answer, ([[0], [1]], 0))

    def test_invalid_and_regressing_outputs_are_rejected(self):
        cases = [([[0], []], {}), ([[0, 1], [1, 2]], {}),
                 ([[0, 2], [1]], {}), ([[0], [1, 5]], {}),
                 ([[0], [1], [2]], {})]
        for candidate, options in cases:
            with self.subTest(candidate=candidate, options=options):
                answer, _ = self.run_case([[0], [1]], [2], (candidate, "FEASIBLE"), **options)
                self.assertEqual(answer, ([[0], [1]], 0))

    def test_equal_value_boundary_churn_is_rejected(self):
        answer, _ = self.run_case([[0, 1], [2]], [3], ([[1], [2, 3]], "FEASIBLE"))
        self.assertEqual(answer, ([[0, 1], [2]], 0))

    def test_cache_ignores_labels_but_not_changed_window(self):
        cache = set()
        self.run_case([[0], [1]], [2], attempted=cache)
        _, solve = self.run_case([[1], [0]], [2], attempted=cache)
        solve.assert_not_called()
        _, solve = self.run_case([[1], [0]], [2, 3], attempted=cache)
        solve.assert_called_once()
        _, solve = self.run_case([[1], [0]], [2, 3], attempted=cache, seconds=10)
        solve.assert_called_once()

    def test_cancelled_attempts_are_not_cached_and_zero_budget_is_disabled(self):
        for status in ("STOPPED", "SKIPPED"):
            cache = set()
            self.run_case([[0], [1]], [], ([[0], [1]], status), attempted=cache)
            self.assertFalse(cache)
        answer, solve = self.run_case([[0], [1]], [], seconds=0)
        self.assertEqual(answer, ([[0], [1]], 0))
        solve.assert_not_called()

    def test_protected_pending_tract_is_not_available(self):
        _, solve = self.run_case([[0], [1]], [3, 4, 5, 6])
        self.assertEqual(solve.call_args.args[1], [0, 1])

    def test_skipped_improvement_defers_changed_cluster_until_all_peers_get_turn(self):
        state = solver._TouchingJointDeferrals()
        units = [[0], [1], [4], [6]]
        (updated, changes), _ = self.run_case(
            units, [2], ([[0], [1, 2]], "SKIPPED"), deferrals=state,
            peer_units=units + [[8]], n=9)
        self.assertEqual(changes, 1)
        self.assertEqual(updated, [[0], [1, 2], [4], [6]])
        for turns in ([[0], [1, 2]], [[4]], [[6]]):
            state.note_turn(turns)
            _, solve = self.run_case(updated, [3, 5], deferrals=state, n=9)
            solve.assert_not_called()
        state.note_turn([[8]])  # Includes weak/pending seeds, not only valid ASUs.
        _, solve = self.run_case(updated, [3, 5], deferrals=state, n=9)
        solve.assert_called_once()

    def test_skip_without_gain_or_peers_does_not_immediately_retry(self):
        state = solver._TouchingJointDeferrals()
        self.run_case([[0], [1]], [2], ([[0], [1]], "SKIPPED"), deferrals=state)
        state.note_turn([[0], [1]])
        _, solve = self.run_case([[0], [1]], [2, 3], deferrals=state)
        solve.assert_not_called()
        state.note_turn([[5]])
        _, solve = self.run_case([[0], [1]], [2, 3], deferrals=state)
        solve.assert_called_once()

    def test_joint_peer_batch_counts_other_groups_even_with_cluster_member(self):
        state = solver._TouchingJointDeferrals()
        state.defer({0, 1}, [[0], [1], [4], [6]])
        state.note_turn([[0], [4], [6]])
        self.assertFalse(state.blocks({0, 1}))

    def test_more_than_three_touching_groups_use_one_model(self):
        units = [[0], [1], [2], [3]]
        _, solve = self.run_case(units, [4, 5, 6])
        self.assertEqual(solve.call_args.args[0], units)
        solve.assert_called_once()

    def test_stage_stats_and_preview_mapping(self):
        previews, log = [], io.StringIO()
        with contextlib.redirect_stdout(log):
            self.run_case([[1], [2]], [3], ([[1], [2, 3]], "OPTIMAL"),
                          log=True, source="expansion",
                          preview_factory=lambda nodes, units: previews.append((nodes, units)))
        self.assertEqual(previews, [([1, 2, 3], [[1], [2]])])
        self.assertIn("PARTITION_TOUCHING_JOINT source=expansion", log.getvalue())
        self.assertIn("roots=movable", log.getvalue())
        self.assertIn("accepted=1 groups_before=2 groups_after=2", log.getvalue())
        self.assertIn("baseline_unemp=20 unemp=30 gain=10", log.getvalue())

    def test_real_model_expands_touching_groups_and_keeps_custom_configuration(self):
        u, emp, pop = np.array([1, 5, 5, 10, 20]), np.array([0, 20, 0, 0, 0]), np.full(5, 10000)
        self.assertEqual(solver._pick_capacity_root([0, 1], u, emp, pop, .2), 0)
        with patch.object(solver, "_configure_asu_solver_portfolio",
                          wraps=solver._configure_asu_solver_portfolio) as configure:
            groups, updates = solver._reoptimize_touching_asu_units(
                [[0, 1], [2, 3]], [4], chain(5), u, emp, pop, .2, 10000, 5, 2)
        self.assertEqual(updates, 1)
        self.assertEqual(sorted(v for group in groups for v in group), [0, 1, 2, 3, 4])
        self.assertEqual(sum(int(u[group].sum()) for group in groups), 41)
        self.assertTrue(all(solver.component_ok(group, u, emp, pop, .2, 10000,
                                               chain(5)) for group in groups))
        # A connected optimum of the cut model now proves the exact model, so
        # this case needs only the configured cut-pass solver and no flow solve.
        self.assertGreaterEqual(configure.call_count, 1)
        self.assertTrue(all(call.args[1] == 2 for call in configure.call_args_list))

    def test_touching_connected_cut_proof_skips_redundant_flow_solve(self):
        models, limits, cut_models = [], [], []
        real_solve = solver.cp_model.CpSolver.Solve
        real_pass = solver._joint_connectivity_cut_pass

        def capture(instance, model, *args, **kwargs):
            models.append(model.Clone())
            limits.append(instance.parameters.max_time_in_seconds)
            instance.parameters.log_to_stdout = False
            return real_solve(instance, model, *args, **kwargs)

        def capture_pass(*args, **kwargs):
            result = real_pass(*args, **kwargs)
            cut_models.append(args[0].Clone())
            return result

        output = io.StringIO()
        with (patch.object(solver.cp_model.CpSolver, "Solve", new=capture),
              patch.object(solver, "_joint_connectivity_cut_pass", side_effect=capture_pass),
              contextlib.redirect_stdout(output)):
            groups, updates = solver._reoptimize_touching_asu_units(
                [[0], [1]], [2, 3], chain(4), np.full(4, 10), np.zeros(4, dtype=int),
                np.full(4, 10000), .2, 10000, 5, 2, log=True)
        self.assertEqual(updates, 1)
        self.assertEqual(sorted(v for unit in groups for v in unit), [0, 1, 2, 3])
        self.assertEqual(len(models), 1)
        has_flow = lambda m: any(v.name.startswith("regional_flow_") for v in m.Proto().variables)
        self.assertFalse(has_flow(models[0]))
        self.assertTrue(all(0 < limit <= .75 for limit in limits))
        self.assertEqual(len(cut_models), 1)
        log = output.getvalue()
        for suffix in ("MODEL", "CUT_PASS", "CUT_ROUND", "CUT_COMPLETE", "CUT_PROOF"):
            self.assertIn(f"[STAGE] PARTITION_TOUCHING_JOINT_{suffix} ", log)
        self.assertNotIn("[STAGE] PARTITION_TOUCHING_JOINT_FLOW ", log)
        self.assertIn("exact_flow_skipped=1", log)
        self.assertIn("graph_cuts=True", log)
        self.assertRegex(log, r"separator_cuts=[1-9][0-9]*")
        self.assertRegex(log, r"seed_distance_rows=[1-9][0-9]*")
        self.assertNotIn("[STAGE] STATEWIDE_JOINT", log)
        self.assertNotIn("[STAGE] JOINT_EXPANSION", log)

    def test_connected_prepass_gain_survives_unknown_touching_flow_solve(self):
        real_solve = solver.cp_model.CpSolver.Solve

        def unknown_flow(instance, model, *args, **kwargs):
            if any(v.name.startswith("regional_flow_") for v in model.Proto().variables):
                return solver.cp_model.UNKNOWN
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, "Solve", new=unknown_flow):
            groups, updates = solver._reoptimize_touching_asu_units(
                [[0], [1]], [2], chain(3), np.full(3, 10), np.zeros(3, dtype=int),
                np.full(3, 10000), .2, 10000, 5, 2)
        self.assertEqual(updates, 1)
        self.assertEqual(sorted(v for unit in groups for v in unit), [0, 1, 2])

    def test_touching_stop_or_skip_during_prepass_preserves_originals(self):
        with TemporaryDirectory() as folder:
            for option in ("stop_path", "skip_path"):
                flag = Path(folder) / option
                cache = set()

                def cancel(instance, model, *args, **kwargs):
                    flag.touch()
                    return solver.cp_model.UNKNOWN

                with patch.object(solver.cp_model.CpSolver, "Solve", autospec=True,
                                  side_effect=cancel) as solve:
                    groups, updates = solver._reoptimize_touching_asu_units(
                        [[0], [1]], [2], chain(3), np.full(3, 10), np.zeros(3, dtype=int),
                        np.full(3, 10000), .2, 10000, 5, 2, attempted=cache,
                        **{option: str(flag)})
                self.assertEqual((groups, updates), ([[0], [1]], 0))
                solve.assert_called_once()
                self.assertFalse(cache)
                self.assertEqual(flag.exists(), option == "stop_path")


if __name__ == "__main__":
    unittest.main()
