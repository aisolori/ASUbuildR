# Partition polish performance changes

Implemented on 2026-09-28, with OR-Tools 9.15.6755 in both the development and
R application's Python environments. Existing user changes to linearization
settings were preserved.

## Behavior

- Three consecutive cut rounds with no new cut, strict connected-incumbent
  improvement, or certified bound improvement hand off to exact flow. Any of
  those improvements resets the counter. The existing cut, proof, cancellation,
  and deadline limits remain in effect.
- Short partition cut solves use at most 16 workers, a stock LP/no-LP/bound/probing
  portfolio, and bounded symmetry/probing startup work. Long exact solves keep
  their existing worker count and custom portfolio; linearization remains 2 in
  the short-cut profile.
- Strict connected improvements refresh hints outside the active solve.
  Supernode hints include profitable closure, objective, counts, positive
  surplus, separator activations, constants, and exact-flow values.
- For quotient graphs with at least 64 nodes, mutually implied profitable
  neighbors are explicitly contracted. Whole donors keep their identities;
  zero-profit corridors remain available; original-tract accounting and
  tie-breaking are preserved. `use_profitable_contraction=False` disables this
  private polish-solver option for comparisons.
- Conditional and reduced-cost-path objective bounds are combined into one
  integer-equivalent row per affected node, without large denominator
  coefficients.
- Each cut solve samples up to eight distinct disconnected callback solutions
  and checks the final solution. At most 128 new, deduplicated rows are added
  per round, only after the solve returns.

The exact connectivity fallback and final ASU validation are unchanged. No
optimality-gap relaxation or geographic-window restriction was introduced.

## Correctness checks

The new tests cover no-progress resets and terminal-condition precedence,
bounded cut collection, profile compatibility, complete feasible hints, donor
accounting, original-tract expansion, and integer-bound equivalence. They include
60 small cyclic graphs checked against exhaustive optima (half forcing exact
flow), 30 exhaustive contraction comparisons, and 500 integer-row cases.

## Bounded Colorado comparison

`benchmark_partition_polish.py` constructs three valid initial ASUs from the
local Colorado fixture and benchmarks a single whole-donor polish call. This
is not a full Partition Strategy run or a reproduction of the user's original
2,796-tract run. The baseline was the working solver immediately before these
changes, including its pre-existing linearization edits.

Five paired runs, 20 seconds per call, 46 requested workers, alternating run
order. Captured unemployment excludes already captured donor unemployment.

| Seed | Baseline | Updated |
| --- | ---: | ---: |
| 1 | 69,020 | 69,601 |
| 2 | 69,716 | 67,282 |
| 3 | 69,157 | 70,165 |
| 4 | 68,931 | 69,397 |
| 5 | 68,345 | 67,341 |
| Mean | 69,033.8 | 68,757.2 |

All results passed connectivity, population, rate, whole-donor, and objective
accounting checks. Quotient nodes decreased from 1,433 to 1,334 (6.9%). Baseline
cut rounds totaled 118, including three UNKNOWN responses; updated rounds
totaled 65 with no UNKNOWN responses. Both versions used approximately their
entire 20-second budgets, and neither reached flow in this productive-cut case.

Quality was mixed: three improved runs, two worse runs, and a 0.4% lower mean.
This small single-fixture experiment does **not** establish an end-to-end speedup
or a generally better portfolio. The no-progress handoff is verified separately
with deterministic regression tests. Larger representative runs are needed to
evaluate solution quality and time to a target objective.

Run the benchmark from the repository root, optionally supplying a preserved
baseline solver file:

```powershell
.\.venv\Scripts\python.exe tests/benchmark_partition_polish.py --baseline PATH_TO_BASELINE --seconds 20 --workers 46 --repeats 5
```
