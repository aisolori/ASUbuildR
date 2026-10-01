"""Bounded, paired final-polish benchmark; does not modify input or assignments.

Example (run from the repository root):
    python tests/benchmark_partition_polish.py --seconds 20 --repeats 2
    python tests/benchmark_partition_polish.py --baseline /path/to/asu_cpsat.py

Uses local Colorado tract data with three reproducibly constructed valid ASUs.
This exercises whole-donor final polish, not a complete Partition Strategy run.
"""
import argparse
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import time

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'inst' / 'python'))
import asu_cpsat


def load_module(path):
    spec = importlib.util.spec_from_file_location('asu_cpsat_benchmark_baseline', path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def fixture(workbook, neighbors):
    frame = pd.read_excel(workbook)
    with open(neighbors, encoding='utf-8') as stream:
        nb = json.load(stream)
    u = frame['tract_ASU_unemp'].to_numpy(dtype=np.int64)
    employed = frame['tract_ASU_emp'].to_numpy(dtype=np.int64)
    population = frame['tract_pop2025'].to_numpy(dtype=np.int64)
    if len(nb) != len(frame):
        raise ValueError('Neighbor rows must match workbook rows in the same order')
    tau, pop_thresh = .0645, 10000
    num, den = asu_cpsat.as_fraction_tau(tau)
    positive = (den * u - num * employed >= 0) & (u > 0)
    groups = [sorted(group) for group in asu_cpsat._connected_components(nb, positive)
              if int(population[group].sum()) >= pop_thresh]
    groups.sort(key=lambda group: (-int(u[group].sum()), tuple(group)))
    if len(groups) < 3:
        raise ValueError('Benchmark needs at least three valid positive-surplus components')
    assignments = np.full(len(nb), -1, dtype=int)
    for label, group in enumerate(groups[:3], start=1):
        assignments[group] = label
    hint = groups[0]
    assert all(asu_cpsat.component_ok(group, u, employed, population, tau, pop_thresh, nb)
               for group in groups[:3])
    return nb, u, employed, population, tau, pop_thresh, assignments, hint


def run(module, label, data, seconds, workers, seed):
    nb, u, employed, population, tau, threshold, assignments, hint = data
    original_factory = module._new_asu_solver

    def factory():
        engine = original_factory()
        engine.parameters.random_seed = seed
        engine.parameters.log_to_stdout = False
        engine.parameters.log_to_response = False
        return engine

    output = io.StringIO()
    module._new_asu_solver = factory
    started = time.monotonic()
    try:
        with contextlib.redirect_stdout(output):
            result = module._solve_supernode_polish(
                nb, u, employed, population, tau, threshold, hint[0], seconds, workers,
                assignments=assignments.copy(), asu_number=1, hint=list(hint),
                deterministic_ties=False, log=True)
    finally:
        module._new_asu_solver = original_factory
    elapsed = time.monotonic() - started
    selected = set(result.sel_idx_local)
    valid = module.component_ok(sorted(selected), u, employed, population, tau, threshold, nb)
    valid = valid and hint[0] in selected
    for donor in (2, 3):
        group = set(np.flatnonzero(assignments == donor).tolist())
        valid = valid and (not (group & selected) or group <= selected)
    captured = sum(int(u[i]) for i in selected if assignments[i] <= 1)
    baseline = int(u[hint].sum())
    if not valid or captured != result.obj or captured < baseline:
        raise AssertionError('Invalid incumbent, donor accounting, or decreasing coverage')
    lines = output.getvalue().splitlines()
    construction = next((line for line in lines if 'FINAL_POLISH_SUPERNODES ' in line), '')
    stats = dict(part.split('=', 1) for part in construction.split() if '=' in part)
    row = dict(variant=label, seed=seed, budget_seconds=seconds, workers=workers,
               elapsed_seconds=round(elapsed, 3), captured_unemployed=captured,
               gain=captured-baseline, status=result.status, valid=bool(valid),
               model_nodes=stats.get('model_nodes'), contracted_nodes=stats.get('contracted_nodes', '0'),
               cut_rounds=sum('_CUT_ROUND ' in line for line in lines),
               unknown_rounds=sum('_CUT_ROUND ' in line and 'status=UNKNOWN' in line for line in lines),
               reached_flow=any('FINAL_POLISH_SUPERNODES_FLOW ' in line for line in lines))
    print(json.dumps(row), flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', type=Path)
    parser.add_argument('--seconds', type=float, default=20)
    parser.add_argument('--workers', type=int, default=16)
    parser.add_argument('--repeats', type=int, default=2)
    parser.add_argument('--seed-start', type=int, default=1)
    parser.add_argument('--workbook', type=Path, default=ROOT / 'test folder' / 'CO_asu27.xlsx')
    parser.add_argument('--neighbors', type=Path, default=ROOT / 'test folder' / 'CO_neighbors_2025.json')
    args = parser.parse_args()
    if args.seconds <= 0 or args.workers < 1 or args.repeats < 1:
        parser.error('seconds, workers, and repeats must be positive')
    data = fixture(args.workbook, args.neighbors)
    variants = [('updated', asu_cpsat)]
    if args.baseline:
        variants.insert(0, ('baseline', load_module(args.baseline)))
    for repeat in range(args.repeats):
        # Alternate order to reduce consistent first-run/cache bias.
        for label, module in variants[::1 if repeat % 2 == 0 else -1]:
            run(module, label, data, args.seconds, args.workers, args.seed_start + repeat)


if __name__ == '__main__':
    main()
