# Dashboard user guide

ASUbuildR helps you build, inspect, edit, and export Areas of Substantial
Unemployment (ASUs). An ASU number groups tracts together. **0 means unassigned**
on the dashboard. Selecting or highlighting a tract does not change its ASU.

## Quick start

1. Launch the dashboard from R with `ASUbuildR::launch_ASUbuildR()`.
2. In **Data Initialization**, choose your BLS workbook and check the state,
   population year, preview, and Census boundary year.
3. In **Load Initial ASU**, choose an algorithm and click **Load tracts & initialise**.
4. Wait for completion, then use **Save and Load Data** to save a checkpoint.
5. Inspect and edit assignments in **Modify ASU Selections**.
6. Set the review thresholds and click **Generate ASU Summary** in
   **ASU Review and Finalization**. Once generated, it updates after edits.
7. Save again and generate the review CSV and LSS text file.

Tract Hunter is initially selected and needs no Python setup. To use CP-SAT,
run `ASUbuildR::setup_asu_python()` in R, then restart the dashboard.
`ASUbuildR::check_asu_python()` checks that installation.

## Data Initialization

| Control | Meaning |
| --- | --- |
| Choose Excel File / Browse | Loads the BLS workbook (`.xls` or `.xlsx`, typically `ST_asuYY.xlsx`). The importer expects the original column order in A through AB, with headers in row 1. |
| Display area | Selects one state or **All states (nationwide)** for the preview and both maps. The selectors on Data Initialization, Load Initial ASU, and Modify ASU Selections stay synchronized. This changes the display, not the solver's uploaded-state scope. |
| Census Tract Year | Boundary vintage used for the map and tract matching. Separate from the population year read from the workbook. The current selector accepts 2010–2025 and initially shows 2024. |
| Preview and detected state/year | Check that the state and tract values match the intended workbook before building. |
| New England Year Override (on Load Initial ASU) | Choose **Use selected year from Data Initialization** or **Use 2021 for New England states**. Connecticut legacy county GEOIDs automatically use 2021 geometry. Other states retain their selected vintage. This does not convert the workbook's geography. |

Keep GEOIDs (tract identifiers) intact, including leading zeros. The importer
expects county-based tract data. The first boundary download needs internet
access; later runs can reuse cached boundaries.

Nationwide workbooks are read in full and retained on the server. The upload
validates tract IDs, duplicate records, state codes and numeric counts before
replacing the current data. The preview uses server-side pagination. No tract
geometry is built by the upload handler; initialization starts with the explicit
**Load tracts & initialise** action.

Maps use PMTiles by default when `freestiler` and PMTiles support in `mapgl` are
installed. PMTiles archives are cached on disk and served as byte ranges through
the authenticated Shiny session, so no separate tile-server port is needed.
`options(ASUbuildR.use_pmtiles = FALSE)` selects the HTTP GeoJSON fallback.
Choose a state or **All states (nationwide)** from **Display area**. All states
shows every uploaded state and requires PMTiles, fetching tiles on demand through
the Shiny session HTTP endpoint. Its first load can take several minutes to
simplify geometry and build the cached archive; a progress indicator appears.
The last two display geometries are retained during the session for faster switching.
The GeoJSON fallback is available for single-state views. All layers share one
source. Display geometry is simplified in
metres; original geometry, neighbor lists and solver inputs stay on the server.
Changing an ASU assignment updates colors and tooltips without resending polygons.
Switching the display area clears the tract selection but retains all assignments.
The selector affects maps and paginated detail tables, not the nationwide solver scope.

For diagnostics, `[payload]` messages record serialized byte counts and output
IDs without data contents. `transport=websocket` records the complete logical
message before httpuv sends it; individual outputs in the same update share its
message number. `transport=http-geojson` records the separate geometry response;
`transport=http-pmtiles-range` records bytes served for a tile-range request.
Set `options(asu.payload_log = "path/to/payload.log")` to retain these records,
or `options(asu.payload_logging = FALSE)` to disable the WebSocket hook. The hook
is guarded for Shiny-version compatibility; it does not change network limits.

## Load Initial ASU: algorithms and buttons

| Control | Meaning |
| --- | --- |
| Choose algorithm | Selects Tract Hunter, Simple Snake, or CP-SAT and shows its controls. |
| Load tracts & initialise | Loads boundaries, joins the upload, and starts the builder. Running again creates a new initial result; save edits you want to keep first. |
| Run Hunter | Runs another Tract Hunter improvement pass after initialization. |
| Combine Groups | Attempts to combine Tract Hunter groups. Inspect the resulting map and totals. |

**Tract Hunter** builds candidate groups and supports repeated Hunter and
Combine Groups passes. **Simple Snake** grows groups from high-unemployment
seeds; it does not enforce population qualification while building, so inspect
small groups carefully. **CP-SAT (OR-Tools)** searches for connected groups
meeting the configured population and rate thresholds while increasing the
unemployment captured.

### Component-first global strategy (Python/CLI and existing jobs)

Component-first global is no longer offered in the dashboard strategy selector.
Its Python/CLI implementation and support for existing jobs remain available.
The following reference describes its global unemployment objective without
choosing a number of ASUs.
The model selects tracts; each connected selected component becomes an ASU
only if it independently meets both the population and unemployment-rate
thresholds. Touching valid groups may become one component without losing
captured unemployment. This requires the same minimum thresholds throughout
the solve, with no maximum ASU size or separation requirement.

The search begins with an optimistic model and adds valid component cuts:
an invalid isolated component must lose at least one selected tract or connect
to additional territory. Qualifying components from intermediate solutions
provide valid incumbents, but are never locked into the global model. Future
rounds can expand, split, merge, or replace them. The strategy keeps the best
validated total unemployment found, not the relaxed objective when some
components still fail eligibility.

The global model carries its tightest certified unemployment upper bound into
later rounds as an objective ceiling. After each solve, it cuts every invalid
component in the returned candidate and at most eight extra components
collected from intermediate candidates. Small input graphs (up to 256 tracts)
also receive conditional population, rate, and positive-labor-force constraints
around invalid components. These extra regional constraints are disabled for
larger inputs because their cost outweighed their benefit in the Wisconsin run.
All enabled cuts are valid restrictions, not permanent search boundaries.

Short, targeted repair solves periodically revisit invalid components together
with nearby valid ASUs and unassigned tracts. Repairs may reshape or replace
the affected selections; they do not permanently freeze any ASU. When neither
validated unemployment nor the upper bound improves, a stronger repair attempt
is scheduled, then global search continues. By default, eligible repairs
receive up to one second every five cut rounds; after eight stalled rounds they
receive up to two seconds, at least four rounds apart. Repeated repairs with
zero gain are spaced progressively farther apart. A repair
uses at most 160 movable tracts; larger neighboring ASUs can participate as
fixed aggregate components in that local model only. Only independently
validated improvements become incumbents.
Repair bounds describe restricted neighborhoods and are **never** reported as
global upper bounds or used to declare global optimality.

**Solve time limit (sec)** is a whole-search budget in this strategy, not a
per-ASU limit, and includes model preparation, cut generation, and local repair.
Input validation, shutdown, and final result writing can add overhead.
Each cut solve receives at most five solver seconds or the remaining budget,
whichever is smaller, with detailed solver logs suppressed. There is no Max
ASUs cap or per-ASU concurrency setting. The configured worker count applies to
each global solve. Stop ends the search and returns the best valid selection;
**Skip Cut Round** ends the current round and lets global search continue.

The log reports validated captured unemployment, a global upper bound, and
the remaining gap. A bound on the relaxed model also bounds the valid problem;
only closing that gap proves the valid solution optimal. A requested positive
relative-gap tolerance may stop with a nonzero gap and is not exact optimality.
A timeout without proof is a valid best-so-far result, not a failed search.
This is an experimental strategy: it preserves feasible alternatives but does
not guarantee faster solves or a proof within the time budget.
Each cut round starts a new CP-SAT search. The solver tries another configuration
after three consecutive rounds that either return no candidate or first produce
one after 80% of the round budget (four seconds in a five-second round). It also
switches after twelve rounds in the same configuration without improving
captured unemployment or the upper bound, even when rounds return `FEASIBLE`.
New cuts alone do not reset this counter. Improvements from local repairs do.
The log reports each change as `COMPONENT_GLOBAL_SEARCH_MODE`, including its reason.

There is no component-global stall cutoff. Stagnation changes the search settings;
the run continues until its overall time budget, requested gap, optimality proof,
or the user's Stop action. Round logs retain objective stagnation diagnostics.

A separate watchdog requests a stop at each round's five-second deadline.
CP-SAT stops asynchronously, so native shutdown or a running Python callback
can still extend elapsed time. A material overrun reduces the worker count for
subsequent rounds. Compact diagnostics include configured budget, actual elapsed
time, native solver time, callback time, time from stop request to return,
active worker count, branches, conflicts, and model size.

Python callers can select it with `component_global=True` in
`build_many_asus_cpsat`; the CLI equivalent is `--component-global`.

Optional saved ASUs are validated using the current data and graph, then used
as a starting incumbent and a soft hint over the full graph. They impose no
fixed boundaries or ASU-count limit. Imported assigned tracts must still match
the current input; this option does not automatically clip a national RDS to
a selected state. Strict valid-unemployment improvements update the map and
durable progress JSON. The detached job exports a final RDS and can recover
the last published assignments independently of the browser.

### CP-SAT parameters

The shared CP-SAT solver factory sets `symmetry_level=3`, including for
large models that CP-SAT would otherwise skip. Symmetry detection retains a
`symmetry_detection_deterministic_time_limit` of 1.0. This is deterministic
solver time, not a one-second wall-clock guarantee. The setting applies even
when the custom worker portfolio is disabled. It is an experiment: extra
presolve work may or may not improve the incumbent or proof within the total
solve budget. The retained component-global backend can override this in its
fallback modes; it is not a dashboard strategy choice.

Partition polishing starts each pass with the lowest total unemployment first,
breaking ties by ASU number. This is the sum of unemployed-person counts across
all member tracts, not the unemployment rate or `q_surplus`.
A merge is committed immediately, but surviving
ASUs still waiting in that pass keep their turns. Absorbed ASUs are removed;
groups containing an ASU already checked wait for the next pass. Once the
remaining queue finishes, a new pass recalculates unemployment order from the
updated assignments. This ordering also applies with merging disabled and to
follow-up polishing passes. Logs report `priority=unemployment_ascending`.

Partitioning's **Initial ASU seed method** defaults to connected components of
the connectivity-free solution, with a 120-second relaxation budget in the dashboard.
The dashboard admits all relaxed components as candidate seeds, including ones
that do not yet meet rate or population requirements; only qualifying expansion
results are committed. The optional **Surplus pruning** method starts
with all available tracts and removes negative-surplus tracts in ascending
`q_surplus` order, preserving the population threshold. Articulation tracts can
be removed when every resulting component meets the population threshold,
even if some do not yet meet the unemployment-rate threshold. Each resulting
component is then pruned independently and may split again. Pruning stops for a component once it qualifies or no eligible
removal remains. Stalled components are first repaired using only their retained
tracts; pruned tracts are excluded from these solves. Already-valid components
need no restricted solve because all their retained tracts are selected. Only
valid seeds enter outward territory expansion and final polishing, where removed
tracts become available again. If no valid seeds survive repair, the strategy
returns no new ASUs rather than falling back to an unrestricted full-graph solve.
This is not a proof that no ASUs exist.

**Expansion incumbent stall limit (sec)** controls how long retained-component
repair, individual ASU expansion, and expansion touching-group solves may run
without improving an incumbent. The dashboard default is 300 seconds; 0 disables
this limit. The solve time budget still applies. Final polishing uses the general
incumbent stall limit. Python/CLI callers can set
`expansion_incumbent_stall_seconds` / `--expansion-incumbent-stall-seconds`;
omitting it inherits the general stall setting.

Dashboard runs disable the standalone `[graph-cut]` hint stage and corridor
`[repair]`. Legacy single-ASU windows proceed from their other starting-solution
heuristics to the main solver, which retains its internal `[cut-pass]`.
That pass carries its generated cuts and tighter bounds into the exact solve.
The single-ASU `[cut-pass]` uses a five-second solver limit for each round.
It stops after 100 rounds, 25 consecutive rounds without a lower proven
upper bound, or 50 consecutive rounds without higher valid unemployment,
whichever happens first. Each improvement resets only its own stall counter.
Equal-value solutions and disconnected candidates do not reset the valid
unemployment counter. Logs show `valid_unemp_stall=N/50` and
`stop_reason=VALID_UNEMP_STALL` when that limit ends the cut pass. The shared
joint, split, and supernode cut passes use this same 50-round valid unemployment
limit; it stays at 50 even when supernode retries increase other limits.
Proof, Stop/Skip, and
solver failure can end it earlier. Cut-pass time is additional to the configured
solve budget, which still limits the other solve phases.
Within each round, a callback retains useful disconnected candidates and valid
connected improvements without stopping search. Separator rows are added only
after the solve returns. Valid connected improvements survive restarts and Stop/Skip.
`round_end=SOLVER_RETURNED` identifies normal returns; round summaries remain
visible, but detailed CP-SAT search logs are suppressed for cut solves.
This applies to Legacy and to Partitioning's individual single-ASU solves.
Partitioning still uses the connectivity-free relaxation to generate seeds.

**Split saved ASUs (joint model)** requires a warm-start RDS, even if the
warm-start checkbox is off. It tries each imported ASU once, in descending
captured unemployment; there is no size threshold. Newly created children
are not split again during that run. Increase **Max ASUs** above the imported
count to leave room for splits. A parent can produce **two or three children**,
limited further by the remaining ASU slots and available qualifying population.
Both the cut pass and exact flow model allocate at most three child slots.

Each child must contain at least one tract from its parent and independently
meet connectivity, population, and unemployment-rate requirements. Parent
tracts may be released and currently unassigned tracts may be added. Other
ASUs remain fixed during the attempt; their tracts and all their neighbors
are excluded. Children cannot touch one another or any other ASU, including
corner contacts in the queen-contiguity graph. Only a strictly greater combined
unemployment total replaces the parent; otherwise it stays unchanged.

The solver uses articulation points and a bounded search of low-degree,
two-tract corridor separators to suggest initial child lobes. These are graph
bottleneck hints, not geometric-width measurements or forced split boundaries.
The full joint model can choose a different separation or find a split with no
such hint. This does not enable the standalone `[graph-cut]` repair stage.

Before the first cut round, the split model also adds a bounded set of valid
articulation/small-separator constraints to every child slot. These use the
**full eligible graph**, including unassigned tracts, so a bypass outside the
parent is not incorrectly cut off. These constraints enforce necessary
connectivity conditions; they do not force a particular split boundary.

Population-derived minimum child sizes and rate-surplus bounds tighten the
maximum selected-tract and deficit-tract counts. Each child reserves enough
tracts for the other active children. Optimistic unemployment upper bounds
apply both to each child and to their combined selection; conditional bounds
can exclude a tract only when selecting it cannot meet the rate requirement
or cannot improve on the parent. The same constraints and derived flow
capacities carry into the exact model. `ASU_SPLIT_TIGHTENING` reports separator
rows, count bounds, objective bounds, and gain-based exclusions. A proven
economic impossibility returns the unchanged parent without running cut or
flow solves and is logged as `ASU_SPLIT_ECONOMIC_SCREEN`.

Each parent starts with a flow-free connectivity cut pass with five-second rounds:
at most **100 rounds, 25 consecutive stalled-upper-bound rounds, or 50 consecutive
rounds without higher valid unemployment**, whichever comes first. Proof, cancellation,
or solver failure may end it earlier. Within each round, the solver checks
incumbents for disconnected children but continues after discovering cuts.
It adds those cuts after the solver returns, then restarts unless the pass
has reached its proof, round, or stall limit. The same behavior applies to
Legacy's main cut pass and Partitioning's individual, touching-ASU joint,
and supernode-polishing cut passes. Existing overall phase budgets still apply.

`ASU_SPLIT_CUT_ROUND` reports `round_end=SOLVER_RETURNED` for normal returns.
On a time limit, the solver status may be `FEASIBLE` or `UNKNOWN`, not `OPTIMAL`. Only certified solver
upper bounds carry forward, never a disconnected incumbent's objective as an
upper bound. Valid connected split incumbents found during a round are retained.
Five-second rounds may finish without a useful candidate; presolve still runs
each round and consumes part of that limit. Cuts and tighter bounds remain in the
same model when exact connectivity flows are added. A valid connected optimum
of the cut model needs no further flow solve. **Time limit per window** and
**Incumbent stall limit** apply only to that subsequent exact solve per parent;
the cut pass has no separate total-time cap, but each cut solve has a five-second
solver limit and the pass has the round/stall limits above. Use a positive time
limit to enable split attempts. Solves run sequentially using all workers.
Stop/Skip preserve the best validated improving split, if any; otherwise the
original parent is retained. No merging, takeover, polishing, or residual-ASU
creation follows this strategy. An unsuccessful limited solve is not a proof
that the parent cannot be improved. `[STAGE] ASU_SPLIT_*` logs report attempts,
bottleneck hints, cut rounds, and accepted gains.

`SURPLUS_PRUNE_COMPONENT` logs each retained component's population, unemployment
rate, threshold, and exact `q_surplus`. Stalled components also report counts of
candidate removals blocked by population, nonnegative surplus, or resulting
components with insufficient population. `SURPLUS_PRUNE_SPLIT` identifies
accepted splits; `split_pending_prune` reports each child before independent pruning.
`SURPLUS_PRUNE_REPAIR` and `SURPLUS_PRUNE_REPAIR_COMPLETE` show the restricted
repair size and whether it produced a valid seed.

With either method, components with the highest total `q_surplus` are selected first:
this is their signed surplus above the unemployment-rate threshold, summed
over all their tracts. Ties favor higher total unemployment, then stable tract
order. The available Max ASUs slots limit how many seeds are expanded at once.
Expansion still maximizes captured unemployment, and candidates must meet the
rate, population, and connectivity requirements before being committed.

**Consolidate touching ASUs at the end** is enabled by default for partitioning.
After the search stages, it deterministically unions eligible touching groups,
preserving every selected tract and the total captured unemployment. This quick
cleanup also runs after Stop. It does not depend on a joint solve finding a merge.

Touching-ASU joint solves reuse generated connectivity cuts within the current
run when the tract window and adjacency match exactly. Cuts use global tract IDs
and are applied to the current group slots with their current root variables,
so changes in seed boundaries or group order do not discard reusable graph cuts.
Changed windows or adjacency do not reuse cuts. Objective bounds, seed-distance
constraints, and the solver's internal learned clauses are not cached.
`PARTITION_TOUCHING_JOINT_CUT_CACHE` reports cached cuts and reused model rows;
`CUT_CACHE_COMPLETE` reports newly stored cuts. Storage is limited to eight
recent windows and 256 distinct cuts per window; replay adds at most 1,024 rows.

Each touching neighborhood gets at most one joint attempt per complete expansion
or polish sweep. Boundary changes, relabeling, or a merge do not immediately reset
its turn. Independent neighborhoods can still run. Mid-sweep restarts retain this
scheduling state so other ASUs finish before a changed neighborhood is retried.
`DEFERRED_SWEEP` identifies these postponed attempts. A changed deferred problem
can trigger a new sweep after peers finish; an exact `CACHED` hit does not.
This scheduling rule is separate from cut reuse and is not an optimality claim.

**Polish consolidated groups** is optional and on by default in the dashboard
(the Python API default is off). It only applies when final consolidation is enabled
and creates merged groups. It gives those merged
groups one additional polish attempt using the final-polish budget and general
incumbent stall limit. Replacements that release selected tracts are discarded.
Any new contacts are consolidated afterward without starting another polish loop.
Stop skips additional solves. Logs report `FINAL_CONSOLIDATION` with group counts
before/after, selected tract count, and captured unemployment.

Set these before starting. Changing a control does not reconfigure a running solve.

| Parameter | Meaning and effect |
| --- | --- |
| CP-SAT strategy | **Partitioning strategy (potential multiple ASUs)** creates seeds, expands territories, merges touching valid groups, and polishes ASUs. Joint touching optimization is a fallback. **Legacy single-ASU solve** reoptimizes imported groups, then builds candidates from remaining tracts; it can produce multiple ASUs. **Split saved ASUs (joint model)** splits imported parents only when separated children capture strictly more combined unemployment. Legacy is currently the selected default. |
| UR threshold (τ) | Minimum aggregate rate as a **fraction**. Default `0.0645` means **6.45%**. Do not enter `6.45` here. |
| Population threshold | Minimum combined population of a candidate. Default `10000`. |
| Use saved ASUs as a warm start / Warm-start RDS | Upload a saved dashboard `.rds` (`sf` or data frame) containing `GEOID`/`geoid` and `asunum`/`asu_id`. Groups are matched to the current data by GEOID and validated before starting. Optional and off by default for Legacy and Partitioning; **mandatory for Split saved ASUs regardless of the checkbox**. |
| Max ASUs | Limit used by the ASU creation loop. Default `30`. It is not a target count; fewer groups may be feasible and merges reduce the count. |
| Solve time limit (sec) | Default `18000` is five hours per main candidate window, **not a whole-run limit**. Use positive seconds. Hint preparation and single-ASU cut passes add time. Partition expansion and final polishing use separate built-in 1800-second budgets per ASU; single-ASU expansion cut passes add time, while supernode polish cuts share the polish budget. |
| Incumbent stall limit (sec) | Stops the applicable search after this long without improving its best solution. Default `300` is five minutes; `0` disables it. Bound improvements alone do not reset this timer. |
| Total CP-SAT workers (detected cores - 2) | Threads available to CP-SAT; initialized from detected physical cores with a minimum of one. More workers use more CPU and may use more memory; they do not guarantee better results. |
| Concurrent ASU solves | Maximum simultaneous main candidate solves, not a promised count. Default `1`. Disjoint main windows share the worker budget; the dashboard's all-remaining-tract window can leave only one window available. Expansion, polishing, imported Legacy reoptimization, and splits are sequential and use all workers. |
| Relative gap (optional) | Allows earlier termination when the solution is close to its bound. `0.01` means approximately 1%. Blank leaves this optional tolerance unset. The gap concerns the current model, not proof of the best overall arrangement. |

**Rate units differ between tabs:**

| Threshold | CP-SAT entry (fraction) | Review entry (percent) |
| --- | --- | --- |
| 6.45% | `0.0645` | `6.45` |

Build and review thresholds are independent. Changing review thresholds does
not rebuild groups. Set both to the values intended for your analysis.

### Run stages, progress, and map colors

An **incumbent** is the best solution found so far. During unemployment
maximization, **best** reports its objective for the current solve. **#Bound**
reports a tightened estimate of how good an unseen solution could be; it need
not change the map. **FEASIBLE** means a solution was found without exact
optimality proven. **OPTIMAL** is a status for the current model, subject to
configured gap tolerances. **UNKNOWN / NO SOLUTION** alone does not prove
infeasibility.

| Stage | What is happening |
| --- | --- |
| COMPONENT_GLOBAL_* | Global component-cut search. Concise round and completion reports distinguish validated unemployment from the optimistic bound and report the global gap/status. A relaxed round's OPTIMAL status alone does not prove the valid ASU problem optimal. |
| COMPONENT_GLOBAL_CUT_BATCH | Adds all previously unseen invalid components from the returned candidate plus up to eight from intermediate candidates. Reports new component cuts and any optional regional groups added on small inputs. |
| COMPONENT_GLOBAL_REPAIR / COMPONENT_GLOBAL_REPAIR_COMPLETE | Runs a short, targeted neighborhood search periodically or after stalled improvement, then resumes unrestricted global search. Only validated gains update the incumbent; local bounds are not global proof. |
| PARTITION_BUILD | Searching a main candidate window using remaining tracts. |
| LEGACY_REOPTIMIZE / LEGACY_REOPTIMIZE_COMPLETE | Reopens a saved Legacy ASU with an unemployment floor and protected peers before searching for new groups. |
| PARTITION_EXPANSION | Expanding seeds in assigned territories, one solve at a time. |
| PARTITION_EXPANSION_COMPLETE | Summarizing that round. Rejected seeds did not produce valid ASUs; the whole run can continue. |
| PARTITION_TOUCHING_SAFE_UNION | Merges touching valid groups directly, preserving selected tracts and captured unemployment without a joint solve. |
| PARTITION_TOUCHING_JOINT / PARTITION_TOUCHING_JOINT_COMPLETE | Fallback joint reoptimization with reachable unassigned tracts. `exact_flow` distinguishes cut-only from exact-flow-enabled attempts. `CACHED` skips a previously attempted neighborhood, not a proof of optimality. |
| PARTITION_BUILD_MERGE / PARTITION_COMBINE | Touching-group bookkeeping; partitioning normally uses safe union before any fallback joint solve. |
| FINAL_POLISH | Processes ASUs from lowest to highest total unemployment. After a merge, pending surviving ASUs finish their turns; the next pass recalculates unemployment from updated memberships. |
| FINAL_POLISH_MERGE | Committing a merge and finishing remaining queued ASUs before restarting polishing. |
| SINGLE_ASU_TAKEOVER / TAKEOVER_DONOR_REPAIR | Partitioning only. Runs flow-free graph cuts before the takeover flow solve, then repairs affected groups before accepting or rejecting the attempt. |
| FINAL_RESIDUAL_CHECK | Checking remaining tract components near the end. |

Stages may repeat or be skipped depending on the strategy and results.
`COMPONENT_GLOBAL_*` entries describe retained backend/existing-job support,
not a currently selectable dashboard strategy.

Legacy single-ASU runs proceed to the final residual check after their ASU
search passes; they do not run `SINGLE_ASU_TAKEOVER` or its donor repairs.

Across all strategies, each connectivity-cut solve has a **five-second limit**
(including presolve), or less when the enclosing phase has less time remaining.
Finding a cut does not interrupt the solve: it continues until optimality or
the time limit, unless Stop/Skip, infeasibility, or a solver error ends it sooner.
Cuts are added between solves, not during a live solve. Connected incumbents are
retained. Cut rounds show concise summaries instead of the full CP-SAT log.
Model setup, cut generation, and solver shutdown can add wall-clock overhead
outside the solver's five-second search limit. Exact-flow solve logging is unchanged.

In Partitioning, the single-ASU takeover uses the same cut-pass limits as Legacy:
100 rounds maximum, 25 rounds without an improved upper bound, or 50 rounds
without improved valid unemployment, whichever happens first.
All generated cuts strengthen the subsequent exact model.
A time-limited round need not prove a disconnected assignment optimal.
A proven optimum can skip further primary optimization; Stop and Skip remain active.

The post-polish bridge-pair pass has been removed (both cuts and flow).
Supernode polishing starts with connectivity cuts on the contracted graph.
Its rounds check ASUs from lowest to highest total unemployment, breaking ties by
ASU ID. After a merge, remaining surviving ASUs finish before the queue restarts
in this same unemployment order. Ordinary polishing with merging
disabled also uses lowest-unemployment-first order.
`FINAL_POLISH_SUPERNODES_CUT_ROUND` reports the upper bound and its stall count.
The first cut pass stops after 100 cut rounds, 25 consecutive rounds without a
better upper bound, or 50 consecutive rounds without higher valid unemployment,
whichever happens first. A better bound resets its stall
count, not the total round count. There is no individual-cut-count cap.
Proof, cancellation, and the overall polish time limit can stop it sooner.
Individual supernode and touching-ASU joint cut rounds use the same five-second
ceiling and continue after cut discovery. Their shared overall phase budgets
still apply.
Exact flow retains the cuts and best connected solution and uses the remaining
time. If primary flow stalls with a valid incumbent that absorbs another ASU,
that solution returns immediately for merge validation/commit and the polish
queue continues with remaining surviving ASUs before restarting in lowest
total unemployment order. Equal statewide unemployment is sufficient; coverage cannot
decrease. `FINAL_POLISH_SUPERNODES_STALL_MERGE` reports this handoff.
Otherwise, if the primary flow solve reaches its incumbent stall limit without a
proof, another flow-free cut pass runs with the total-round and upper-bound-stall
limits doubled: 200/50, 400/100, 800/200, and so on. The valid unemployment stall
limit remains 50 per cut pass. Each flow solve is rebuilt from the accumulated cuts.
The primary flow incumbent-stall allowance also doubles on each retry:
the configured limit, then 2x, 4x, and so on. A disabled stall limit stays
disabled. Cycle/flow stage logs report the active allowance. Valid
incumbents and certified bounds carry forward. All cycles share the original
per-ASU time budget, and Stop/Skip ends the cycle sequence. Ordinary time-limit
termination and stalls during post-proof tie-breaking do not trigger retries.
`FINAL_POLISH_SUPERNODES_CYCLE` and `FINAL_POLISH_SUPERNODES_RETRY_CUTS` report
the active cycle and cut limits. Other cut passes retain their existing rules.

Committed expansion assignments update the live ASU data, summary table, and
map before polishing starts.

Enriched stage messages include `total_unemp` for committed coverage over the input,
`checking_asus`, and `asus_remaining`. During polishing these are ASU IDs
and the number still waiting in the current pass, excluding the current ASU.
Joint checks list all participating IDs. Stage fields `asu`, `checking_asu`,
and `checking_asus` use the same compact IDs as the current dashboard snapshot,
including after absorption. When different, `internal_*` fields retain the
solver IDs for tracing earlier messages. Before commitment, expansion groups
use `candidate_tract_N` labels, where N is a zero-based member tract index,
not an ASU ID. Queues are recomputed after repartitioning or merges; counts
are not a promise of how many future solves remain. Stages with no applicable
ASU queue report `checking_asus=none asus_remaining=NA`.

### Starting from a saved RDS solution

Load your current input data and choose its states, tract year, and thresholds
as usual. In the CP-SAT controls, enable **Use saved ASUs as a warm start**,
upload the RDS file, and set **Max ASUs** to at least the saved group count.
Split requires this upload regardless of the checkbox and needs extra slots
to create children.
The RDS is an assignment warm start, not a replacement for the current input
data. Its geometry and economic columns are ignored; current population,
employment, and the current neighbor graph determine validity.

Rows are matched by GEOID, never by file order. Positive `asunum` or `asu_id`
values identify groups; `0`, `-1`, or missing assignments mean unassigned.
Gaps in group numbers are compacted. New current tracts absent from the file
remain unassigned. Extra unassigned file rows may be ignored, but an assigned
tract missing from the current data is an error. Duplicate GEOIDs, conflicting
ID columns, invalid groups, or too many groups for an applicable Max ASUs cap stop the run with
an explanation; groups are not silently truncated or discarded.

In **Legacy single-ASU**, each saved ASU is reoptimized once before looking for
new ASUs. Its saved selection is a hint and its unemployment total is a minimum
objective, not a fixed membership. The solver can add, drop, or exchange tracts
within its current group plus reachable unassigned territory. Other saved ASUs
remain protected; one anchor tract chosen from the target ASU remains selected.
Reverse-pruning and hint-refinement are skipped for these already-valid seeds.
`LEGACY_REOPTIMIZE` reports the window and objective floor; completed replacements
are validated, cannot lose captured unemployment, and are checkpointed before
later solves. No replacement leaves the saved ASU intact. Stop/Skip remain active.
This pass uses the normal per-ASU solve time and all workers, even when Max ASUs
already equals the imported count; a zero solve time skips it. Subsequent new-ASU
searches may still use pruning because they have no saved candidate.

In **Partitioning**, saved groups initialize existing assignments and remain
subject to that strategy's later polishing, exchange, and merge rules.
Max ASUs counts both imported and newly created groups in those strategies.
In the retained **Component-first global** backend, imported groups instead provide an incumbent
and a hint; all tracts remain globally reconsiderable and Max ASUs is ignored.

The `WARM_START` stage reports imported ASUs, assigned tracts, and baseline
unemployment. The original RDS is never modified. The edit tab's existing
**Load Data** control remains separate from this solver warm-start upload.

Partitioning expands ASUs individually in sequence, using all workers and a
1800-second budget per ASU. The joint multi-ASU expansion option has been
removed from the dashboard. Touching groups are checked after individual
expansions; a safe union or accepted fallback update restarts expansion before
the next ASU. Single-ASU cut passes add time to the expansion budget.

### Touching ASUs in partitioning

With touching resolution enabled (`merge_adjacent=True`, the dashboard default),
partitioning first uses **safe union**: touching valid ASUs are merged directly.
Their union stays connected, preserves population and the minimum aggregate
rate, and retains every selected tract and the same total unemployment.
`PARTITION_TOUCHING_SAFE_UNION` reports this operation. It avoids paying for a
joint solve just to rediscover a legal merge. Disabling the optional final
consolidation checkbox does not disable these earlier merges.

Joint optimization remains a fallback path, not a required step for every
contact. Its window includes the participating groups and reachable unassigned
tracts; other ASUs and pending seeds are protected. It may exchange or drop
tracts, move roots, or deactivate slots. Surviving groups must qualify and
retain a tract from their seed. Updates require higher combined unemployment,
or equal unemployment with fewer groups. That is not a proof of the best
overall partition. An unsuccessful fallback leaves the input groups unchanged.

Touching-ASU solves automatically enable small-separator cuts, seed-distance/
cardinality constraints, and the connectivity cut pre-pass.
The pre-pass uses up to 50 rounds, ending earlier after 10 rounds without a
better upper bound, 50 without higher valid unemployment, or another stopping
condition. Its deadline is the smaller of 180 seconds and 15% of the remaining
joint budget, with at most five solver seconds per round; it does not add time
to the joint budget. Only validated connected solutions are retained. Cuts
carry into the exact flow model when that follow-up is enabled.
Finite usable upper bounds reported by the pre-pass are also carried forward
as explicit objective constraints (conservatively rounded upward). The bound
comes from the solver's bound, never a disconnected incumbent's objective.
The log uses `PARTITION_TOUCHING_JOINT_MODEL` (`graph_cuts=True`),
`PARTITION_TOUCHING_JOINT_CUT_PASS`, `_CUT_ROUND`, `_CUT_COMPLETE`, and `_FLOW`.

Fallback touching checks during expansion use the cut/capacity phase only,
without an exact-flow follow-up. Exact-flow-enabled touching fallbacks share
at most 180 seconds across the entire run (or the polish budget, if smaller),
rather than receiving 1800 seconds per cluster. That allowance includes their
pre-pass and preparation; it does not cap ordinary individual expansion or
polishing. Each invocation attempts at most one neighborhood. Expansion uses
at most two groups; later checks use at most three, with larger clusters broken
into touching-pair neighborhoods. All workers and Stop/Skip remain available.
Attempt caching and sweep deferral avoid repeated unchanged work; neither is
an optimality certificate. Roots stay fixed in individual expansion/polish
models but can move in the fallback joint model.

**Skip during a touching solve** retains any eligible improvement, then defers
that cluster until the other queued ASUs/candidate seeds have had a turn.
The deferral survives boundary changes and round restarts; the log reports
`DEFERRED_SKIP`. If there are no other groups, it stays deferred until a
different ASU gets a turn. It does not immediately retry the same cluster.

**Stop during expansion** retains valid current results and ends expansion
without another optimization round. The completion log reports `outcome=stopped`,
attempted and unattempted seed counts, and unresolved seeds. Unprocessed weak
seeds are not reported as failed repairs or proven infeasible. The final map
is a partial result, not a converged run.

Live incumbent previews do not interrupt a solve to merge ASUs. Expansion,
main-build, and polishing solves finish under their normal stopping conditions
before touching checks use the returned selection. Stop/Skip and
configured time, gap, and stall limits still apply. When a completed expansion
leads to a safe union or accepted touching-joint update, expansion restarts before the next
stale solve. During polishing, pending ASUs are tracked by tract membership
across merges and renumbering; the remaining queue finishes before a restart.

If a later polish releases tracts that enlarge an earlier ASU's reachable
window, or changes ownership within its supernode model, that ASU can receive
a follow-up solve even without a merge. Follow-ups use the same polish ordering;
unchanged or merely smaller windows do not trigger a retry unless ownership changes.
The whole run allows at most three such follow-up rounds, with no shared
time cap. Each ASU receives its normal configured per-ASU polish time limit;
Stop/Skip and the five-second cut-round limit still apply.
The follow-up round limit does not replace ordinary merge restarts. The log uses
`FINAL_POLISH_RECHECK` and `FINAL_POLISH_RECHECK_LIMIT`; reaching a limit does
not prove that further improvement is impossible.

A validated connected hint skips the auxiliary feasibility screen. Otherwise,
the connected solve screens once and can reuse a matching certificate from a
bounded, run-local cache. Timeouts are not cached as infeasibility. Screening
and model preparation count toward the connected solve's time budget.

Pastel fills identify committed ASUs; colors can repeat, so check the tooltip's
ASU number. Grey indicates unassigned tracts. During supported searches,
translucent green/red fills show proposed additions/removals relative to the
starting selection. These are provisional previews, normally checked about
every 60 seconds during longer solves; cut passes can report at round boundaries.
Identical selections and candidates without a strict gain in valid unemployment
are suppressed. Bound improvements alone do not repaint the map. Large-state
maps may load after the log.

| Run button | Effect |
| --- | --- |
| Stop Solve | Requests an orderly stop with the available incumbent. Wait for completion and the final map update before saving. |
| Skip to Next ASU | Requests that the current search finish with its available incumbent so building can continue; it does not delete an ASU. |
| Close | Hides the log panel; does not stop the Python process. |
| Save log as... | Opens a save dialog for the complete log on your computer, during or after a run. |
| Open log / Open folder | Opens the automatic log or folder on the computer running R. |

CP-SAT jobs survive browser closes and disconnects; closing the log only hides it.
Use **Stop Solve** to request termination and wait for the final result before
manual editing or loading another file. A mid-run RDS save contains the currently
committed assignments, not the search tree or temporary preview.

## Modify ASU Selections

Hover for tract details; click to toggle selection. Red outlines show selected
tracts. The Selection Summary updates automatically.

| Control | Meaning |
| --- | --- |
| Select ASU Number | Selects that group's tracts and zooms to them. None clears selection. |
| Change ASU Value | Destination for the next update. Use a positive whole number, or `0` to unassign. Typing alone does not change assignments. |
| Update Selected Tracts | Assigns **every selected tract** to that number and clears selection. It does not repair disconnected or under-threshold groups. |
| Clear Selected Tracts | Clears selection without changing assignments. |
| Low/High unemployment filter and Percentile Threshold | Chooses the bottom/top percentage of rates within the current selection. Low and `5` targets the bottom 5%; ties can include more tracts. Tracts without positive population and labor force are excluded. |
| Highlight Tracts | Applies the filter. Highlights do not narrow the selection that Update Selected Tracts will edit. |
| Clear Highlights | Removes inspection highlights without changing assignments. |
| Enter a GEOID / Select a Single Tract | Locates a tract by its 11-character GEOID, including leading zeros. |
| Selection Summary table rows | Highlights those tracts for inspection. Table highlights are separate from the assignment selection. |
| Reset to Initial Data | Restores the latest stored baseline and discards subsequent edits. Not a one-step Undo. Loading an RDS sets that file as the new reset baseline; initialization and Hunter passes also update the baseline. |

**Add a tract to ASU 1:** clear selection, click the tract, enter `1`, then
Update Selected Tracts. Select ASU 1 to inspect totals and regenerate the review.

**Merge ASU 2 into ASU 1:** select ASU 2, enter `1`, and update. Check the
combined group's connectivity and thresholds afterward. To remove a tract,
select it and update its value to `0`.

## Save and Load Data

| Control | Meaning |
| --- | --- |
| LSS / CSV Export Directory | Existing folder on the machine running R, used for LSS and summary CSV exports. RDS and log saves choose their own location. |
| Save Data As... | Opens a save dialog to choose a folder and filename for current map data and assignments. The suggested filename is `saved_data.rds`. Cancel leaves the data unchanged. |
| Load Data / Browse | Restores a saved dashboard `.rds` sf map for editing and sets the Reset baseline. It does not start optimization, resume a search tree, or restore all settings. Use Warm-start RDS when launching CP-SAT to reoptimize saved ASUs. |

The native save dialog is available in supporting browsers such as Chrome and
Edge over HTTPS or localhost. Other browsers and insecure remote HTTP sessions
use ordinary downloads; enable **Ask where to save each file** in the browser
to choose a destination each time. Saves go to your computer even when R runs
in an EC2 container. Automatic run logs remain on the R server.

Save after initialization and at editing milestones. After restoring an RDS,
continue editing directly. For LSS export from a saved single-state result,
upload the matching single-state workbook **before** loading the RDS: workbook
upload clears the current map, and LSS labels use its state setting. Do not
initialize again unless you intend to build a new result. The current LSS
exporter is not a nationwide per-state export workflow; a display-state filter
does not filter its output. Retain nationwide results as RDS/CSV and prepare
and verify state-specific exports separately.

## ASU Review and Finalization

| Control | Meaning |
| --- | --- |
| ASU Unemployment Rate Threshold | Review threshold in **percent**, initially `6.45`. |
| ASU Population Threshold | Review population threshold, initially `10000`. |
| Generate ASU Summary | Enables the review table. Once generated, it reacts to assignment edits, newly loaded data, and review-threshold changes. |
| Generate LSS .txt File | Writes assigned tracts to `lss_batch_file.txt`. Does not filter out failed groups or submit the file. |
| Create Summary CSV | Writes `ASU_Review_File.csv`, including unassigned tracts, for tract-level review. |

The table's `ur_qualified`, `pop_qualified`, and `contiguous` flags test rate,
population, and connectivity. `asu_qualified` requires all three. TRUE passes;
FALSE needs attention. Contiguity uses the dashboard neighbor graph, including
its island-connection handling; inspect unusual geographic connections.

For borderline rates, inspect aggregate unemployment and labor force: review
uses the unrounded aggregate rate for qualification and displays three decimal
places. Use thresholds appropriate to your analysis and program
guidance. Export buttons write current assignments even without a refreshed
summary. Review first and retain an RDS checkpoint. Repeated exports overwrite
the same filenames in the active save directory.

## Troubleshooting

Legacy single-ASU solves automatically save a recovery RDS immediately after a
valid exact result returns, before trade refinement, merging, or another solve.
A second checkpoint saves the final run result. Python waits for each save to
finish; a failed save stops further solving and reports the error. Check the
`[checkpoint] Saved ...` log line for the full filename. These files retain the
original geometry, GEOIDs, population/economic data and ASU assignments, and can
be opened with **Load Data** or used as a warm start.

### Persistent solver jobs and browser disconnects

All newly launched dashboard CP-SAT strategies (Legacy, Partitioning, and Split) run
as detached jobs under `~/ASUbuildR/jobs/` on the machine running R. Set
`ASU_JOB_DIR` before launching the dashboard to choose another writable,
persistent directory. The old `ASU_CHECKPOINT_DIR` location is not used for new
dashboard jobs; existing checkpoints there remain untouched.

Closing/navigating away from the browser, losing its WebSocket, or ending the
launching R process does not request solver termination. Explicit **Stop** and
**Skip** still send their usual flags. To avoid an accidental duplicate solve,
the dashboard asks you to attach to an existing active job before starting another.

On a fresh dashboard, use **Persistent solver jobs** on the Data Initialization
tab: **Refresh jobs**, choose the run, then **Attach to job**. No workbook upload
is required to view that job's saved data. A successful attachment opens
**Load Initial ASU** to show the map, summary, and live log. Attaching never
reruns the solver.
Only active jobs are available for attachment; the list refreshes every five
seconds and restores live progress and Stop/Skip controls. Completed, stopped,
failed, or interrupted jobs are hidden. On job-list refresh, completed job
folders are permanently deleted once completion is more than 24 hours old,
including inputs, logs, checkpoints, and results. Copy results you want to keep
outside the job folder. Interrupted jobs are also deleted after 24 hours
without recorded activity (using the latest timestamps in job files and status).
Live supervisor/solver processes protect their job folders from deletion.
Running, stopped, and failed jobs are preserved.
Cleanup runs while a dashboard is open, not as a background timer
when all dashboards are closed.
A failed job is not automatically retried. Reopening a dashboard after restarting R must
use the same `ASU_JOB_DIR` and OS user to discover those jobs.

Each unique job directory contains:

- `solver.log`: solver stdout/stderr with a UTC capture timestamp on every
  nonempty line, independent of Shiny. Millisecond timestamps are added as
  output is collected (normally polled every 0.1 seconds), not when a viewer
  attaches. CP-SAT's own elapsed-time fields are preserved. Quiet cut rounds
  still suppress detailed search logs.
- `solver.raw.log`: original output spool, retained if the supervisor fails.
  Native solver output and stderr use this regular file, not a browser-owned
  pipe. The timestamped copy also includes recovery-process output.
- `events.log`: viewer attach/disconnect and explicit user-control events.
- `supervisor.log`: detached supervisor startup/errors.
- `job.json`, `owner.json`, `status.json`: configuration, process identity,
  running/terminal state, timestamps and solver exit code. A failed RDS export
  has a separate `recovery_exit_code`; JSON recovery remains available.
- `input.rds`, `df.csv`, `nb.json`: immutable source geometry, ordered data and graph.
- `python/`, `runner.py`, `recover.R`: snapshotted job code, so later source edits
  cannot silently change a running job's checkpoint code or solver module.
- `progress.json`: atomically replaced committed assignments for all strategies.
- `out.json`, `result.rds`: final assignments and dashboard-loadable output when
  the solve returns successfully, including an explicit Stop that returns results.
- `recovered.rds`: latest published progress exported if the solver fails and
  a valid progress snapshot exists. The unfinished candidate may not be committed.
  Repeated manual exports use a unique filename instead of overwriting earlier saves.
- Legacy `*.request.json`, `*.ack.json`, and `*.rds` checkpoints: independent R
  subprocesses now service these; they never wait for a browser/Shiny observer.

If the machine or supervisor dies before exporting an RDS, the saved input and
progress JSON can still be converted from an R session with the updated package:

```r
ASUbuildR:::asu_job_recover('FULL/PATH/TO/job_...')
```

This recovers assignments, not CP-SAT's search tree. Load the RDS or use it as a
warm start for a new solve. Browser independence does not mean jobs survive a
machine reboot, OS-enforced logout termination, disk failure, or out-of-memory
kill. Jobs run as your OS user, not as a system service. Completed and interrupted
jobs are subject to the 24-hour cleanup described above; save wanted results
elsewhere. Monitor disk space, particularly for national geometry and retained
job checkpoints. The job list covers the configured folder on
this machine; it does not attach to jobs on another server.

Timestamp changes apply to newly launched jobs after updating the package.
Existing jobs use their snapshotted code; reattaching does not upgrade their
logger or reconstruct missing timestamps in older output.

- **Python not detected:** run `ASUbuildR::setup_asu_python()` and
  `ASUbuildR::check_asu_python()`, then restart the dashboard.
- **Upload/matching fails:** check the workbook layout, state, GEOIDs, boundary
  vintage, and New England override.
- **Log changes but map does not:** bounds can improve without new solutions;
  allow time for periodic previews and initial map loading.
- **Run exceeds the time limit:** the limit is per window; later stages have
  additional solves. Stop Solve requests completion with available results.
- **An edit affects too many tracts:** updates use the whole selection, not just
  percentile or table highlights.
- **Files are missing:** RDS and Save log as downloads go to the browser's chosen
  destination. LSS/CSV exports use Current save directory on the R server;
  automatic job logs/checkpoints stay in the job folder. Check the success dialog.
- **Reporting an issue:** retain the log and note the algorithm, strategy,
  state, boundary year, settings, and last stage.


The experimental statewide joint strategy and user-imposed ASU tract limits
have been removed from the dashboard, R wrapper, Python API and CLI.
Connectivity cut passes and touching-ASU joint checks remain available.

### Persistent neighbor cache

The dashboard automatically caches tract adjacency lists as JSON in
`rappdirs::user_cache_dir("ASUbuildR", "neighbors")`. CP-SAT reuses the completed
island-connected graph on subsequent loads, skipping contiguity construction
and island repair. The other initialization algorithms and saved-data summary
checks also reuse cached graphs. Logs report when neighbors are built or reused.

Cache keys include ordered GEOIDs, geometry and CRS, island-repair coordinates,
spatial-library versions/settings, and the neighbor algorithm version. Changes
to unemployment or population attributes do not invalidate the graph. Tract
geometry still loads from the existing TIGER cache. Each solver run retains its
own zero-based `nb.json`, so saved scripts remain self-contained; the persistent
cache stores one-based R adjacency lists with validation metadata.

Set `options(asu.neighbor_cache_dir = "path/to/cache")` before launching the
dashboard to choose a location, or `options(asu.neighbor_cache_dir = FALSE)` to
disable caching. Delete the cache directory to force rebuilding. Invalid cache
files are rebuilt automatically; inability to save a cache does not block a run.

Nationwide solver initialization matches every uploaded GEOID before building the neighbor graph. Connecticut workbooks with legacy county GEOIDs use TIGER 2021 for Connecticut only; other states retain the selected geography year. The optional New England year override affects only New England states. A population year such as 2025 does not imply a 2025 geography vintage. Missing tract matches stop initialization with a diagnostic instead of creating blank counts.

During reverse pruning, the live CP-SAT log and stage display report elapsed time, removed and remaining tracts, current and target unemployment rates, retained population and unemployment, and average removals per second. Updates appear approximately every 10 seconds, after a pruning step finishes, with immediate start and completion messages. Completion states whether the rate threshold was reached or no valid removal remained. These are preprocessing progress reports, not a percentage-complete estimate.

After reverse pruning, warm-start trade refinement has a 30-second budget and reports HINT_TRADES progress every 10 seconds. The following articulation reroute also has a 30-second budget, checked inside its graph searches, and reports HINT_REROUTE progress. These cooperative budgets keep the best completed feasible hint; a single in-progress graph operation can briefly overrun a deadline. Stop/Skip is checked during reverse pruning and these refinements. Stop during hint preparation retains validated, nonoverlapping feasible hints as ASUs before returning. Exact CP-SAT time and cut limits are separate.
