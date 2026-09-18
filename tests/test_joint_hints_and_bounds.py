"""Check partial relaxed hints and validity of joint-model strengthening."""
import itertools
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class JointHintsAndBoundsTest(unittest.TestCase):
    def capture(self, nb, u, emp, pop, seeds, **options):
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def solve(instance, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, "Solve", new=solve):
            groups, status = solver._solve_regional_exchange(
                seeds, list(range(len(u))), nb, np.array(u), np.array(emp), np.array(pop),
                .2, 10000, 5, 2, allow_inactive_seeds=True, allow_unseeded_groups=True,
                max_groups=None, **options)
        self.assertEqual(status, "OPTIMAL")
        return groups, models[0]

    def test_partial_hint_covers_invalid_relaxed_components_without_conflicting_hints(self):
        # Collective rate is valid, but isolated tract 1 cannot be a valid ASU.
        groups, model = self.capture(
            [[], [], []], [100, 1, 1], [0, 200, 0], [10000]*3, [[0], []],
            relaxed_selection_hint=[0, 1, 2], tighten_model=True)
        self.assertEqual(sorted(unit for unit in groups if unit), [[0], [2]])
        proto = model.Proto()
        hints = {proto.variables[i].name: v for i, v in
                 zip(proto.solution_hint.vars, proto.solution_hint.values)}
        self.assertEqual(hints, {f"joint_selected_{i}": 1 for i in range(3)})
        self.assertEqual(len(proto.solution_hint.vars), len(set(proto.solution_hint.vars)))
        # The valid fallback remains feasible: the relaxed objective is NOT a floor.
        model.ClearHints()
        for i, variable in enumerate(proto.variables):
            if variable.name in {f"regional_{k}_{j}" for k in range(2) for j in range(3)}:
                model.Add(model.GetIntVarFromProtoIndex(i) == int(variable.name == "regional_0_0"))
        check = solver.cp_model.CpSolver()
        check.parameters.max_time_in_seconds = 2
        check.parameters.num_search_workers = 2
        solver._configure_asu_solver_portfolio(check.parameters, 2)
        self.assertIn(check.Solve(model), (solver.cp_model.OPTIMAL, solver.cp_model.FEASIBLE))
        self.assertEqual(check.ObjectiveValue(), 100)

    def test_no_relaxed_selection_retains_complete_feasible_seed_hints(self):
        _, model = self.capture([[1], [0]], [10, 10], [0, 0], [10000]*2,
                                [[0], []], relaxed_selection_hint=[], tighten_model=True)
        proto = model.Proto()
        names = {proto.variables[i].name for i in proto.solution_hint.vars}
        self.assertIn("regional_0_0", names)
        self.assertIn("regional_flow_0_0_1", names)
        self.assertFalse(any(name.startswith("joint_selected_") for name in names))

    def test_derived_population_count_is_optimistic_for_every_subset(self):
        self.assertEqual(solver._joint_minimum_tract_count([10000], 10000.5), 1)
        for pop, threshold in (([6000]*5, 10000), ([0, 5, 10], 20),
                               ([-10, 6, 6], 10), ([0, 0], 0), ([12000], 10000)):
            minimum = solver._joint_minimum_tract_count(pop, threshold)
            for mask in itertools.product((False, True), repeat=len(pop)):
                chosen = [i for i, flag in enumerate(mask) if flag]
                if chosen and sum(pop[i] for i in chosen) >= threshold:
                    self.assertGreaterEqual(len(chosen), minimum)

    def test_component_domains_and_edge_flows_are_tightened(self):
        nb = [[1], [0], [3], [2], []]
        groups, model = self.capture(nb, [10]*5, [0]*5, [6000]*5, [[0, 1], []],
                                     tighten_model=True)
        self.assertEqual(groups, [[0, 1], [2, 3]])
        variables = {v.name: list(v.domain) for v in model.Proto().variables}
        self.assertEqual(variables["regional_0_2"], [0, 0])  # Cannot reach seed.
        self.assertEqual(variables["regional_1_4"], [0, 0])  # Too little population.
        self.assertEqual(variables["regional_flow_0_0_1"], [-1, 1])
        # Dead-component flow variables are omitted, rather than constructed
        # with a zero domain and left for presolve to remove.
        self.assertNotIn("regional_flow_0_2_3", variables)

    def test_joint_floor_fixes_conditionally_unaffordable_tracts_and_flows(self):
        groups, model = self.capture(
            [[1], [0, 2], [1]], [100, 1, 100], [0, 1000, 0], [10000] * 3,
            [[0], [2]], tighten_model=True,
        )
        self.assertEqual(groups, [[0], [2]])
        variables = {v.name: list(v.domain) for v in model.Proto().variables}
        self.assertEqual(variables["regional_0_1"], [0, 0])
        self.assertEqual(variables["regional_1_1"], [0, 0])
        self.assertNotIn("regional_root_0_prefix_1", variables)
        self.assertNotIn("regional_root_1_prefix_1", variables)
        self.assertFalse(any(name.startswith("regional_flow_") for name in variables))
        self.assertFalse(any(name.endswith("_1") and "regional_injected" in name
                             for name in variables))

    def test_joint_objective_has_aggregate_lagrangian_rate_bound(self):
        u, emp = [10, 1, 100, 1], [0, 1000, 0, 0]
        groups, model = self.capture(
            [[], [], [], []], u, emp, [10000] * 4, [[], []],
            tighten_model=True,
        )
        num, den = solver.as_fraction_tau(.2)
        q = den * np.array(u, dtype=np.int64) - num * np.array(emp, dtype=np.int64)
        expected = solver._lagrangian_objective_bound(u, q, set())
        variables = {v.name: list(v.domain) for v in model.Proto().variables}
        self.assertEqual(expected, 111)
        self.assertEqual(variables['regional_objective_unemployment'], [0, expected])
        self.assertEqual(sum(u[i] for group in groups for i in group), 110)


    def test_joint_hint_banks_safe_surplus_then_spends_it_on_deficit_tract(self):
        # At tau=.1, q=9u-e. Tract 2 cannot join tract 0 directly
        # (q=-40), but tract 1 first contributes q=45.
        groups, model = self.capture(
            [[1], [0, 2], [1], []],
            [10, 5, 20, 10], [40, 0, 100, 40], [10000] * 4,
            [[0], [3]], tighten_model=True,
        )
        self.assertEqual(groups, [[0, 1, 2], [3]])
        proto = model.Proto()
        hints = {proto.variables[i].name: value for i, value in
                 zip(proto.solution_hint.vars, proto.solution_hint.values)}
        self.assertEqual(hints['regional_0_1'], 1)
        self.assertEqual(hints['regional_0_2'], 1)
        variables = {v.name: list(v.domain) for v in proto.variables}
        self.assertEqual(variables['regional_objective_unemployment'], [45, 45])

    def test_joint_hint_uses_objective_when_deficit_is_already_affordable(self):
        # At tau=.1, q=9u-e. The seed can already afford tract 2 (q=-1),
        # so its 100 unemployed should beat the safe but low-value tract 1.
        groups, safe_added, deficit_added = solver._joint_capacity_grow_hint(
            [[0]], [[0]], [[1, 2], [0], [0]],
            np.array([10, 1, 100]), np.array([0, 0, 901]),
            np.array([10000] * 3), .1, 10000, total_bound=2, group_bound=2,
        )
        self.assertEqual(groups, [[0, 2]])
        self.assertEqual((safe_added, deficit_added), (0, 1))

    def test_equal_surplus_root_prefers_more_unemployment_before_population(self):
        # At tau=.2 both q values are 40; population favors tract 0, but the
        # primary objective correctly favors tract 1.
        root = solver._pick_capacity_root(
            [0, 1], np.array([10, 20]), np.array([0, 40]),
            np.array([20000, 10000]), .2,
        )
        self.assertEqual(root, 1)

    def test_joint_profitable_closure_is_union_level(self):
        nb = [[1], [0, 2], [1]]

        def closure_rows(model):
            proto = model.Proto()
            positions = {v.name: i for i, v in enumerate(proto.variables)}
            if not all(f"joint_selected_{i}" in positions for i in range(3)):
                return []
            selected = {positions[f"joint_selected_{i}"] for i in range(3)}
            return [
                row for row in proto.constraints
                if len(row.linear.vars) == 2
                and set(row.linear.vars).issubset(selected)
                and set(row.linear.coeffs) == {-1, 1}
                and list(row.linear.domain)[0] == 0
            ]

        _, uncapped = self.capture(
            nb, [10] * 3, [0] * 3, [10000] * 3, [[0], []],
            tighten_model=True,
        )
        self.assertEqual(len(closure_rows(uncapped)), 4)

    def test_group_objective_bounds_use_reachable_seed_component(self):
        _, model = self.capture(
            [[1], [0], [3], [2]], [10, 1, 100, 1], [0] * 4,
            [10000] * 4, [[0], [2]], tighten_model=True,
        )
        proto = model.Proto()
        positions = {v.name: i for i, v in enumerate(proto.variables)}

        def has_group_bound(group, bound):
            active = positions[f"regional_active_{group}"]
            for constraint in proto.constraints:
                coeffs = dict(zip(constraint.linear.vars, constraint.linear.coeffs))
                if coeffs.get(active) != -bound:
                    continue
                if all(coeffs.get(positions[f"regional_{group}_{i}"]) == value
                       for i, value in enumerate([10, 1, 100, 1])):
                    return True
            return False

        self.assertTrue(has_group_bound(0, 11))
        self.assertTrue(has_group_bound(1, 101))

    def test_cached_rate_count_bounds_dominate_exhaustive_optima(self):
        cases = [
            ([10, 8, 5], [4, -3, -7], 2),
            ([20, 1, 30, 4], [-10, 8, -3, 0], 3),
            ([5, 7, 9], [-6, -2, 10], 1),
        ]
        for u, q, limit in cases:
            with self.subTest(u=u, q=q, limit=limit):
                total, conditional = solver._rate_count_objective_bounds(u, q, limit)
                feasible = []
                for mask in itertools.product((False, True), repeat=len(u)):
                    chosen = [i for i, selected in enumerate(mask) if selected]
                    if len(chosen) <= limit and sum(q[i] for i in chosen) >= 0:
                        feasible.append((set(chosen), sum(u[i] for i in chosen)))
                self.assertGreaterEqual(total, max(value for _, value in feasible))
                for i in range(len(u)):
                    forced_values = [value for chosen, value in feasible if i in chosen]
                    expected = max(forced_values, default=-1)
                    self.assertGreaterEqual(conditional[i], expected)

    def test_joint_tie_prefers_surplus_after_primary_objective(self):
        solve_calls = 0
        real_solve = solver.cp_model.CpSolver.Solve

        def counted_solve(instance, model, *args, **kwargs):
            nonlocal solve_calls
            solve_calls += 1
            return real_solve(instance, model, *args, **kwargs)

        with patch.object(solver.cp_model.CpSolver, "Solve", new=counted_solve):
            groups, status = solver._solve_regional_exchange(
                [[0], []], [0, 1, 2], [[], [], []],
                np.array([5, 10, 10]), np.array([0, 0, 20]),
                np.array([10000] * 3), .2, 10000, 5, 2,
                allow_inactive_seeds=True,
                allow_unseeded_groups=True, max_groups=None,
                tighten_model=True,
            )
        self.assertEqual(status, "OPTIMAL")
        self.assertEqual(groups, [[0], [1]])
        self.assertGreaterEqual(solve_calls, 2)

    def test_joint_model_caps_globally_affordable_deficit_tract_count(self):
        # At tau=.2, q=4u-e: the q=40 supply tract can fund at most one
        # of the two q=-30 deficit tracts, even across separate group slots.
        _, model = self.capture(
            [[], [], []], [10, 10, 10], [0, 70, 70], [10000] * 3,
            [[], []], tighten_model=True,
        )
        proto = model.Proto()
        positions = {v.name: i for i, v in enumerate(proto.variables)}
        deficit_vars = {
            positions[f'regional_{k}_{i}'] for k in range(2) for i in (1, 2)
        }
        global_rows = [
            row for row in proto.constraints
            if set(row.linear.vars) == deficit_vars
            and set(row.linear.coeffs) == {1}
            and list(row.linear.domain)[-1] == 1
        ]
        self.assertEqual(len(global_rows), 1)

    def test_closed_objective_gap_turns_consolidation_into_feasibility_proof(self):
        groups, model = self.capture(
            [[1], [0]], [10, 10], [0, 0], [10000, 10000], [[0], [1]],
            tighten_model=True, allow_seed_consolidation=True,
        )
        self.assertEqual(sum(bool(group) for group in groups), 1)
        self.assertEqual(sum(10 for group in groups for _ in group), 20)
        positions = {v.name: i for i, v in enumerate(model.Proto().variables)}
        active_vars = {positions["regional_active_0"], positions["regional_active_1"]}
        active_caps = [
            list(row.linear.domain)[-1]
            for row in model.Proto().constraints
            if set(row.linear.vars) == active_vars
            and list(row.linear.coeffs) == [1, 1]
        ]
        self.assertIn(1, active_caps)

    def test_mandatory_groups_reserve_population_derived_minimum_counts(self):
        nb = [[j for j in (i-1, i+1) if 0 <= j < 6] for i in range(6)]
        _, model = self.capture(nb, [10]*6, [0]*6, [6000]*6, [[0, 1], [4, 5]],
                                tighten_model=True)
        variables = {v.name: list(v.domain) for v in model.Proto().variables}
        self.assertEqual(variables["regional_count_0"], [0, 4])
        self.assertEqual(variables["regional_flow_0_0_1"], [-3, 3])

    def test_tightening_preserves_every_feasible_partition_up_to_free_label_symmetry(self):
        cases = [
            ([[1], [0, 2], [1, 3], [2]], [10]*4, [0]*4, [6000]*4, [[], []], {}),
            ([[1], [0, 2], [1, 3], [2]], [10, 2, 3, 8], [0, 40, 3, 0], [6000]*4, [[], []], {}),
            ([[1], [0], [3], [2]], [10]*4, [0]*4, [6000]*4, [[], []], {}),
            ([[1], [0, 2], [1, 3], [2]], [10]*4, [0]*4, [6000]*4, [[0, 1], []], {}),
        ]
        tested = 0
        for nb, u, emp, pop, seeds, options in cases:
            arrays = [np.array(values) for values in (u, emp, pop)]
            _, template = self.capture(
                nb, u, emp, pop, seeds, tighten_model=True,
                use_joint_profitable_closure=False, **options,
            )
            template.ClearHints()
            positions = {v.name: i for i, v in enumerate(template.Proto().variables)}
            objective_floor = template.Proto().variables[
                positions["regional_objective_unemployment"]
            ].domain[0]
            valid = lambda group: solver.component_ok(group, *arrays, .2, 10000, nb, **options)
            mandatory = [valid(seed) for seed in seeds]
            baseline = sum(sum(u[i] for i in seed) for seed, required in zip(seeds, mandatory) if required)
            seen = set()
            for labels in itertools.product(range(3), repeat=4):
                groups = [[i for i, label in enumerate(labels) if label == k+1] for k in range(2)]
                if not all((not group and not required) or (valid(group) and
                           (not seed or bool(set(seed) & set(group))))
                           for group, required, seed in zip(groups, mandatory, seeds)):
                    continue
                if sum(u[i] for group in groups for i in group) < max(baseline, objective_floor):
                    continue
                # Empty seed slots are interchangeable; canonicalize their roots.
                free = sorted((groups[k] for k, seed in enumerate(seeds) if not seed),
                              key=lambda g: solver._pick_capacity_root(g, *arrays, .2) if g else 5)
                free_iter = iter(free)
                groups = [group if seed else next(free_iter) for group, seed in zip(groups, seeds)]
                key = tuple(map(tuple, groups))
                if key in seen:
                    continue
                seen.add(key)
                model = template.Clone()
                for k, group in enumerate(groups):
                    for i in range(4):
                        model.Add(model.GetIntVarFromProtoIndex(positions[f"regional_{k}_{i}"]) == int(i in group))
                check = solver.cp_model.CpSolver()
                check.parameters.max_time_in_seconds = 2
                check.parameters.num_search_workers = 2
                solver._configure_asu_solver_portfolio(check.parameters, 2)
                status = check.Solve(model)
                self.assertIn(status, (solver.cp_model.OPTIMAL, solver.cp_model.FEASIBLE),
                              msg=f"Excluded feasible partition {groups}, options={options}, nb={nb}")
                tested += 1
        self.assertGreater(tested, 15)



if __name__ == "__main__":
    unittest.main()
