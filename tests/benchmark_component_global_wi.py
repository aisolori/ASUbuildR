"""Compare component-global cut policies on the saved Wisconsin tract geometry.

Run, for example:
  python tests/benchmark_component_global_wi.py --seconds 30 --workers 4

This is a performance probe, not an assertion about global optimality.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "inst" / "python"))
import asu_component_global as global_solver


def load_wisconsin(rscript, rds):
    with tempfile.TemporaryDirectory() as folder:
        output = Path(folder) / "wi.json"
        subprocess.run(
            [rscript, "--vanilla", str(ROOT / "tests/benchmark_wi_prepare.R"),
             str(rds), str(output)],
            check=True, stdout=subprocess.DEVNULL,
        )
        data = json.loads(output.read_text(encoding="utf-8"))
    graph = [sorted(set(row if isinstance(row, list) else [row]))
             for row in data["nb"]]
    groups = list(global_solver._components(graph, [True] * len(graph)))
    if len(groups) > 1:
        main = max(groups, key=len)
        for island in groups:
            if island is main:
                continue
            source, target = min(
                ((i, j) for i in island for j in main),
                key=lambda pair: ((data["lat"][pair[0]] - data["lat"][pair[1]]) ** 2
                                  + (data["lon"][pair[0]] - data["lon"][pair[1]]) ** 2,
                                  pair),
            )
            graph[source].append(target)
            graph[target].append(source)
        graph = [sorted(set(row)) for row in graph]
    return graph, data


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--rds", type=Path, default=Path("inst/shiny_app/saved_data.rds"))
    parser.add_argument("--rscript", default=shutil.which("Rscript"))
    parser.add_argument("--seconds", type=float, default=30)
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--variant", choices=("both", "regional_off", "regional_on"), default="both")
    parser.add_argument("--search-mode", choices=("auto", "standard", "no_symmetry", "no_presolve"),
                        default="auto")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    if not args.rscript:
        parser.error("Rscript is required to read the saved Wisconsin geometry")
    if args.search_mode != "auto":
        policy_type = global_solver._SearchPolicy
        def initial_policy():
            policy = policy_type()
            policy.mode = args.search_mode
            return policy
        global_solver._SearchPolicy = initial_policy
    graph, data = load_wisconsin(args.rscript, args.rds)
    for label, max_tracts in (("regional_off", 256), ("regional_on", len(graph))):
        if args.variant not in ("both", label):
            continue
        global_solver._REGIONAL_MAX_TRACTS = max_tracts
        started = time.monotonic()
        result = global_solver.solve_component_global(
            graph, data["u"], data["E"], data["P"], .0645, 10000,
            time_limit=args.seconds, workers=args.workers, verbose=args.verbose,
        )
        print(json.dumps(dict(
            variant=label, tracts=len(graph), workers=args.workers,
            requested_seconds=args.seconds, elapsed_seconds=round(time.monotonic() - started, 3),
            status=result["status"], valid_unemp=result["total_unemp"],
            upper_bound=result["upper_bound"], rounds=result["rounds"],
            cuts=result["cuts"], regional_cuts=result["regional_cuts"],
            repair_attempts=result["repair_attempts"], repair_gains=result["repair_gains"],
        )), flush=True)


if __name__ == "__main__":
    main()
