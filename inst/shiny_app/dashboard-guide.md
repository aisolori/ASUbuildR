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
   **ASU Review and Finalization**. Repeat after edits.
7. Save again and generate the review CSV and LSS text file.

Tract Hunter is initially selected and needs no Python setup. To use CP-SAT,
run `ASUbuildR::setup_asu_python()` in R, then restart the dashboard.
`ASUbuildR::check_asu_python()` checks that installation.

## Data Initialization

| Control | Meaning |
| --- | --- |
| Choose Excel File / Browse | Loads the BLS workbook (`.xls` or `.xlsx`, typically `ST_asuYY.xlsx`). The importer expects the original column order in A through AB, with headers in row 1. |
| Census Tract Year | Boundary vintage used for the map and tract matching. Separate from the population year read from the workbook. The current selector accepts 2010–2025 and initially shows 2024. |
| Preview and detected state/year | Check that the state and tract values match the intended workbook before building. |
| New England Year Override (on Load Initial ASU) | Uses the selected boundary year or **Force 2021 (last NECTA year)**. Choose a vintage matching the workbook's GEOIDs. This changes the boundary download; it does not convert the workbook's geography. |

Keep GEOIDs (tract identifiers) intact, including leading zeros. The importer
expects county-based tract data. The first boundary download needs internet
access; later runs can reuse cached boundaries.

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

### CP-SAT parameters

During polishing, ASUs with the highest total signed `q_surplus` get first
access to the unassigned tracts. Ties favor higher total unemployment, then
the lower ASU number. A merge restarts the round and recalculates this order
from the updated assignments.

Partitioning's **Initial ASU seed method** defaults to connected components of
the connectivity-free solution. The optional **Surplus pruning** method starts
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
repair, sequential/joint expansion, and expansion touching-group solves may run
without improving an incumbent. The dashboard default is 300 seconds; 0 disables
this limit. The solve time budget still applies. Final polishing uses the general
incumbent stall limit. Python/CLI callers can set
`expansion_incumbent_stall_seconds` / `--expansion-incumbent-stall-seconds`;
omitting it inherits the general stall setting.

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

**Polish consolidated groups** is optional and off by default. It gives merged
groups one additional polish attempt using the final-polish budget and general
incumbent stall limit. Replacements that release selected tracts are discarded.
Any new contacts are consolidated afterward without starting another polish loop.
Stop skips additional solves. Logs report `FINAL_CONSOLIDATION` with group counts
before/after, selected tract count, and captured unemployment.

Set these before starting. Changing a control does not reconfigure a running solve.

| Parameter | Meaning and effect |
| --- | --- |
| CP-SAT strategy | **Partitioning strategy (potential multiple ASUs)** creates seeds, expands territories, jointly reoptimizes touching ASUs, and polishes groups. **Legacy single-ASU solve** builds candidates from remaining tracts one at a time; it can still produce multiple ASUs. Legacy is currently the selected default. |
| UR threshold (τ) | Minimum aggregate rate as a **fraction**. Default `0.0645` means **6.45%**. Do not enter `6.45` here. |
| Population threshold | Minimum combined population of a candidate. Default `10000`. |
| Use saved ASUs as a warm start / Warm-start RDS | Available for CP-SAT strategies, off by default. Upload a saved dashboard `.rds` (`sf` or data frame) containing `GEOID`/`geoid` and `asunum`/`asu_id`. Groups are matched to the current data by GEOID and validated before starting. Disable the checkbox to run without the uploaded file. |
| Joint multi-ASU expansion (experimental) | Only shown for the partitioning strategy; off by default. Pools up to three neighboring seed territories and jointly assigns their tracts. Each active ASU must independently satisfy connectivity, population, and rate requirements. Batches run sequentially with all workers and 1800 seconds per batch, not per ASU. May use more memory. |
| Max ASUs | Limit used by the ASU creation loop. Default `30`. It is not a target count; fewer groups may be feasible and merges reduce the count. |
| Time limit (sec) per window | Budget for an individual main candidate window (the tracts considered in that solve). Default `18000` is five hours. **Not a whole-run limit.** Use positive seconds. Partition expansion has a separate built-in 1800-second budget per solve, also used by default for final polishing. |
| Incumbent stall limit (sec) | Stops the applicable search after this long without improving its best solution. Default `300` is five minutes; `0` disables it. Bound improvements alone do not reset this timer. |
| Total CP-SAT workers (detected cores - 2) | Threads available to CP-SAT; initialized from detected physical cores with a minimum of one. More workers use more CPU and may use more memory; they do not guarantee better results. |
| Concurrent ASU solves | Maximum simultaneous main candidate solves. Default `1`. Main windows share the worker budget. Partition expansions are sequential and receive the full worker budget. |
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
| PARTITION_BUILD | Searching a main candidate window using remaining tracts. |
| STATEWIDE_JOINT_SEED / STATEWIDE_JOINT_SEED_COMPLETE | Preparing valid component seeds. Reports seed budget, status, valid seeds, free slots, and baseline unemployment. |
| STATEWIDE_JOINT / STATEWIDE_JOINT_COMPLETE | One all-tract joint model. Reports group slots, assignment/flow variable estimates, workers, time limit, and final status, active groups, merges, unemployment, gain, and elapsed time. |
| STATEWIDE_JOINT_MODEL | Reports the actual hint mode, hinted tract count, tightening setting, derived count bounds, graph components, and assignments fixed to zero. |
| PARTITION_EXPANSION | Expanding seeds in assigned territories, one solve at a time. |
| PARTITION_JOINT_EXPANSION / PARTITION_JOINT_EXPANSION_COMPLETE | Experimental pooled expansion. Reports batch number, candidate count, territory size, workers, time budget, and then status, active/inactive groups, unemployment gain, and elapsed time. |
| PARTITION_EXPANSION_COMPLETE | Summarizing that round. Rejected seeds did not produce valid ASUs; the whole run can continue. |
| PARTITION_TOUCHING_JOINT / PARTITION_TOUCHING_JOINT_COMPLETE | Jointly reoptimizing a touching partition cluster with reachable unassigned tracts. Reports source stage, group/window size, budget/workers, movable roots, baseline unemployment, gain, deactivated slots, acceptance, and elapsed time. `CACHED` skips an unchanged attempted neighborhood, not a proof of optimality. |
| PARTITION_BUILD_MERGE / PARTITION_COMBINE | Legacy touching-group combining; partitioning uses the joint check instead. |
| FINAL_POLISH | With merging enabled, processes ASUs from least to most total unemployment. After a merge, the newly merged ASU gets the next solve, then normal ordering resumes. |
| FINAL_POLISH_MERGE | Restarting polishing after a merge. |
| SINGLE_ASU_TAKEOVER / TAKEOVER_DONOR_REPAIR | Runs flow-free graph cuts before the takeover flow solve, then repairs affected groups before accepting or rejecting the attempt. |
| FINAL_RESIDUAL_CHECK | Checking remaining tract components near the end. |

Stages may repeat or be skipped depending on the strategy and results.

The single-ASU takeover cut pass keeps its 100-round maximum and stops after
ten rounds without an improved upper bound. It shares the solve time budget
(up to 60 seconds and 15% of that budget for cuts, with a two-second minimum
when time remains). All generated cuts strengthen the subsequent exact model.
A proven optimum can skip further primary optimization; Stop and Skip remain active.

The post-polish bridge-pair pass has been removed (both cuts and flow).
Supernode polishing starts with connectivity cuts on the contracted graph.
Its rounds check ASUs from least to most total unemployment, breaking ties by
ASU ID. After a merge, the newly merged ASU runs next before returning to this order. Ordinary polishing with merging
disabled retains highest-unemployment-first order.
`FINAL_POLISH_SUPERNODES_CUT_ROUND` reports the upper bound and its stall count.
The first cut pass stops after 50 cut rounds or five consecutive rounds without a
better upper bound, whichever happens first. A better bound resets the stall
count, not the total round count. There is no individual-cut-count cap.
Proof, cancellation, and the overall polish time limit can stop it sooner.
Exact flow retains the cuts and best connected solution and uses the remaining
time. If primary flow stalls with a valid incumbent that absorbs another ASU,
that solution returns immediately for merge validation/commit and the polish
queue restarts with the merged ASU first, then resumes lowest unemployment first. Equal statewide unemployment is sufficient; coverage cannot
decrease. `FINAL_POLISH_SUPERNODES_STALL_MERGE` reports this handoff.
Otherwise, if the primary flow solve reaches its incumbent stall limit without a
proof, another flow-free cut pass runs with both limits doubled: 100/10, 200/20,
400/40, and so on. Each flow solve is rebuilt from the accumulated cuts.
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

Every stage includes `total_unemp` for committed statewide coverage,
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
The RDS is an assignment warm start, not a replacement for the current input
data. Its geometry and economic columns are ignored; current population,
employment, and the current neighbor graph determine validity.

Rows are matched by GEOID, never by file order. Positive `asunum` or `asu_id`
values identify groups; `0`, `-1`, or missing assignments mean unassigned.
Gaps in group numbers are compacted. New current tracts absent from the file
remain unassigned. Extra unassigned file rows may be ignored, but an assigned
tract missing from the current data is an error. Duplicate GEOIDs, conflicting
ID columns, invalid groups, or too many groups for Max ASUs stop the run with
an explanation; groups are not silently truncated or discarded.

In **Legacy single-ASU** and **Partitioning**, saved groups initialize the
existing assignments. New ASUs are sought in remaining tracts; the normal
later polishing, exchange, and merge rules can reconsider saved groups.
Max ASUs counts both imported and newly created groups.

The `WARM_START` stage reports imported ASUs, assigned tracts, and baseline
unemployment. The original RDS is never modified. The edit tab's existing
**Load Data** control remains separate from this solver warm-start upload.
To compare expansion strategies, choose **Partitioning strategy**, then run
with **Joint multi-ASU expansion (experimental)** off and on using the same
data and thresholds. Compare total unemployment, valid ASUs, elapsed time,
and memory use; a joint batch is not a statewide optimum. Sequential mode
gets 1800 seconds per seed, while joint mode shares 1800 seconds per batch,
so these are not equal-total-budget comparisons.

Joint expansion may exchange tracts across the pooled territory boundaries.
Valid seed groups remain active and their combined unemployment cannot fall;
individual groups can shrink. A weak relaxed component may be repaired or
left inactive, and is never committed as an invalid ASU. Every active group
retains at least one tract from its seed. Other batches and already committed
ASUs stay outside the solve. Valid seeds are retained if no solution is found.
Stop ends the search; Skip skips the current batch. Time, relative-gap and
incumbent-stall controls apply. Previews show the combined selection's added
and removed tracts, not provisional group colors. Touching joint checks run only
after the batch returns. An accepted update restarts expansion before the next batch.

### Touching ASUs in partitioning

Partitioning no longer automatically unions touching ASUs. With touching
resolution enabled (`merge_adjacent=True`, the dashboard default), the whole
connected cluster of touching ASUs is reoptimized jointly with **all reachable
unassigned tracts**. Other ASUs and pending seeds are protected. This applies
even when the experimental pooled-expansion checkbox is off; that checkbox
controls initial territory batches, not the touching-cluster step.

The joint solver may exchange or drop tracts, move roots, and deactivate seed
slots. Every surviving group must be connected, meet the population/rate
requirements, respect any enabled tract limit, and retain at least one of its
own seed tracts. The number of slots cannot increase. Combined unemployment
is protected, but an individual ASU may shrink.

An update is accepted if it captures more unemployment, or captures the same
unemployment with fewer ASUs. This is an acceptance rule, not a guarantee the
solver finds the fewest groups among equal-value solutions. Unchanged,
invalid, or non-improving results keep the original ASUs separate; there is no
automatic merge fallback. Accepted changes restart expansion or polishing
with fresh territories/order. Late-stage changes receive the same check.

Touching-ASU solves automatically enable small-separator cuts, seed-distance/
cardinality constraints, and the connectivity cut pre-pass. These do not depend
on the optional joint-expansion checkbox.
The pre-pass uses at most eight rounds, 60 seconds, and 15% of the remaining
joint budget; it does not add time to that budget. Only validated connected
solutions are retained. Cuts carry into the final exact flow model.
Finite usable upper bounds reported by the pre-pass are also carried forward
as explicit objective constraints (conservatively rounded upward). The bound
comes from the solver's bound, never a disconnected incumbent's objective.
The log uses `PARTITION_TOUCHING_JOINT_MODEL` (`graph_cuts=True`),
`PARTITION_TOUCHING_JOINT_CUT_PASS`, `_CUT_ROUND`, `_CUT_COMPLETE`, and `_FLOW`.

Each cluster gets the expansion time limit during building and the polish time
limit during polishing/late checks (falling back to expansion when polishing
is disabled). Both are 1800 seconds in the dashboard. All configured workers,
the custom portfolio, Stop/Skip, gap/stall settings, and incumbent previews
apply. Identical attempted neighborhoods are not repeatedly solved; changed
memberships, reachable tracts, or budgets allow another attempt. A zero budget
disables that check. Large touching clusters can cost more than single-ASU
expansions. Ordinary individual expansion/polish roots remain fixed within
each solve; **roots move in the touching joint solve**.

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
or polish leads to an accepted touching-joint update, its round restarts before
the next stale solve. Legacy merging behavior is unchanged.

If a later polish releases tracts that enlarge an earlier ASU's reachable
window, that ASU can receive a follow-up solve even without a merge. Follow-ups
run in highest-`q_surplus` order and skip unchanged or merely smaller windows.
The whole run allows at most three such follow-up rounds, sharing up to 180
seconds (or the initial ASU count times the per-ASU polish limit, if smaller).
These extra-work limits do not replace ordinary merge restarts. The log uses
`FINAL_POLISH_RECHECK` and `FINAL_POLISH_RECHECK_LIMIT`; reaching a limit does
not prove that further improvement is impossible.

A validated connected hint skips the auxiliary feasibility screen. Otherwise,
the connected solve screens once and can reuse a matching certificate from a
bounded, run-local cache. Timeouts are not cached as infeasibility. Screening
and model preparation count toward the connected solve's time budget.

Pastel fills identify committed ASUs; colors can repeat, so check the tooltip's
ASU number. Grey indicates unassigned tracts. During supported searches,
translucent green/red fills show proposed additions/removals relative to the
starting selection. These are provisional previews, normally updated about
every 60 seconds. Unchanged incumbents and bound updates need not repaint the
map. Large-state maps may load after the log.

| Run button | Effect |
| --- | --- |
| Stop Solve | Requests an orderly stop with the available incumbent. Wait for completion and the final map update before saving. |
| Skip to Next ASU | Requests that the current search finish with its available incumbent so building can continue; it does not delete an ASU. |
| Close | Hides the log panel; does not stop the Python process. |
| Save log as... | Opens a save dialog for the complete log on your computer, during or after a run. |
| Open log / Open folder | Opens the automatic log or folder on the computer running R. |

Keep the browser session open while solving; ending the session terminates its
solver process. Finish or stop the run before manual editing or saving results.

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
| Reset to Initial Data | Restores the stored initialization/algorithm baseline and discards subsequent edits. Not a one-step Undo. Loading an RDS does not establish a new reset baseline. |

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
| Load Data / Browse | Restores a saved `.rds` map for editing. It does not restore a running search or all dashboard settings. |

The native save dialog is available in supporting browsers such as Chrome and
Edge over HTTPS or localhost. Other browsers and insecure remote HTTP sessions
use ordinary downloads; enable **Ask where to save each file** in the browser
to choose a destination each time. Saves go to your computer even when R runs
in an EC2 container. Automatic run logs remain on the R server.

Save after initialization and at editing milestones. After restoring an RDS,
continue editing directly. Before LSS export, upload the matching workbook in
Data Initialization to establish the correct state, which the export uses.
Do not initialize again unless you intend to build a new result.

## ASU Review and Finalization

| Control | Meaning |
| --- | --- |
| ASU Unemployment Rate Threshold | Review threshold in **percent**, initially `6.45`. |
| ASU Population Threshold | Review population threshold, initially `10000`. |
| Generate ASU Summary | Recalculates the table. Run again after edits or threshold changes. |
| Generate LSS .txt File | Writes assigned tracts to `lss_batch_file.txt`. Does not filter out failed groups or submit the file. |
| Create Summary CSV | Writes `ASU_Review_File.csv`, including unassigned tracts, for tract-level review. |

The table's `ur_qualified`, `pop_qualified`, and `contiguous` flags test rate,
population, and connectivity. `asu_qualified` requires all three. TRUE passes;
FALSE needs attention. Contiguity uses the dashboard neighbor graph, including
its island-connection handling; inspect unusual geographic connections.

For borderline rates, inspect aggregate unemployment and labor force: the
current review calculation rounds to five decimal places before comparison
and displays three. Use thresholds appropriate to your analysis and program
guidance. Export buttons write current assignments even without a refreshed
summary. Review first and retain an RDS checkpoint. Repeated exports overwrite
the same filenames in the active save directory.

## Troubleshooting

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
- **Files are missing:** check Current save directory and the success dialog.
  Files are saved where R runs, not automatically in browser Downloads.
- **Reporting an issue:** retain the log and note the algorithm, strategy,
  state, boundary year, settings, and last stage.


The experimental statewide joint strategy and user-imposed ASU tract limits
have been removed from the dashboard, R wrapper, Python API and CLI. Partition
joint expansion and connectivity cut passes remain available.
