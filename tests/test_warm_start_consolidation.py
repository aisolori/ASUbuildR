"""Imported regional ASUs can consolidate without losing their objective floor."""
import contextlib
import io
import itertools
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class WarmStartConsolidationTest(unittest.TestCase):
    # Neither seed can afford the bridge alone; their union can.
    nb = [[1], [0, 2], [1]]
    u = np.array([10, 1, 10])
    emp = np.array([0, 60, 0])
    pop = np.array([10000]*3)
    seeds = [[0], [2]]


    def capture(self, nb=None, u=None, emp=None, pop=None, seeds=None, **options):
        models = []
        real_solve = solver.cp_model.CpSolver.Solve

        def solve(instance, model, *args, **kwargs):
            models.append(model.Clone())
            return real_solve(instance, model, *args, **kwargs)

        nb = self.nb if nb is None else nb
        arrays = [default if value is None else np.array(value)
                  for value, default in ((u, self.u), (emp, self.emp), (pop, self.pop))]
        with patch.object(solver.cp_model.CpSolver, "Solve", new=solve):
            groups, status = solver._solve_regional_exchange(
                self.seeds if seeds is None else seeds, list(range(len(nb))), nb,
                *arrays, .2, 10000, 5, 2, allow_inactive_seeds=True,
                allow_unseeded_groups=True, max_groups=None,
                allow_seed_consolidation=True, **options)
        self.assertEqual(status, "OPTIMAL")
        return groups, models[0]


    def test_count_and_flow_bounds_do_not_reserve_space_for_deactivated_seeds(self):
        # Each seed needs two tracts for population. The full five-tract union
        # needs a count domain of five and four units of root flow.
        nb = [[j for j in (i-1, i+1) if 0 <= j < 5] for i in range(5)]
        groups, model = self.capture(nb, [5, 5, 1, 5, 5], [0, 0, 60, 0, 0],
                                     [6000]*5, [[0, 1], [3, 4]], tighten_model=True)
        self.assertEqual(sorted(unit for unit in groups if unit), [list(range(5))])
        variables = {v.name: list(v.domain) for v in model.Proto().variables}
        self.assertEqual(variables["regional_count_0"], [0, 5])
        self.assertEqual(variables["regional_flow_0_0_1"], [-4, 4])

    def test_every_small_assignment_obeys_feasibility_and_combined_floor(self):
        for tighten in (False, True):
            _, template = self.capture(tighten_model=tighten)
            template.ClearHints()
            positions = {v.name: i for i, v in enumerate(template.Proto().variables)}
            for labels in itertools.product(range(3), repeat=3):
                groups = [[i for i, label in enumerate(labels) if label == k+1] for k in range(2)]
                valid = all(not group or (
                    solver.component_ok(group, self.u, self.emp, self.pop, .2, 10000, self.nb)
                    and bool(set(group) & set(seed))) for group, seed in zip(groups, self.seeds))
                valid = valid and sum(int(self.u[group].sum()) for group in groups) >= 20
                model = template.Clone()
                for k, group in enumerate(groups):
                    for i in range(3):
                        model.Add(model.GetIntVarFromProtoIndex(positions[f"regional_{k}_{i}"]) == int(i in group))
                check = solver.cp_model.CpSolver()
                check.parameters.max_time_in_seconds = 2
                check.parameters.num_search_workers = 2
                solver._configure_asu_solver_portfolio(check.parameters, 2)
                status = check.Solve(model)
                self.assertEqual(status, solver.cp_model.OPTIMAL if valid else solver.cp_model.INFEASIBLE,
                                 msg=f"tighten={tighten}, groups={groups}")

    def test_original_assignment_and_flow_hints_remain_feasible(self):
        _, model = self.capture(tighten_model=True)
        proto = model.Proto()
        hints = {proto.variables[i].name: value for i, value in
                 zip(proto.solution_hint.vars, proto.solution_hint.values)}
        self.assertEqual(hints["regional_active_0"], 1)
        self.assertEqual(hints["regional_active_1"], 1)
        self.assertIn("regional_flow_0_0_1", hints)
        check = solver.cp_model.CpSolver()
        check.parameters.max_time_in_seconds = 2
        check.parameters.num_search_workers = 2
        solver._configure_asu_solver_portfolio(check.parameters, 2)
        check.parameters.fix_variables_to_their_hinted_value = True
        self.assertEqual(check.Solve(model), solver.cp_model.OPTIMAL)
        self.assertEqual(check.ObjectiveValue(), 20)


    def test_consolidation_respects_connectivity(self):
        groups, _ = self.capture(nb=[[], [], []], tighten_model=True)
        self.assertEqual(groups, self.seeds)



if __name__ == "__main__":
    unittest.main()
