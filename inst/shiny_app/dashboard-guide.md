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

Partitioning seeds come from connected components of the connectivity-free
solution. Components with the highest total `q_surplus` are selected first:
this is their signed surplus above the unemployment-rate threshold, summed
over all their tracts. Ties favor higher total unemployment, then stable tract
order. The available Max ASUs slots limit how many seeds are expanded at once.
Expansion still maximizes captured unemployment, and candidates must meet the
rate, population, and connectivity requirements before being committed.

Set these before starting. Changing a control does not reconfigure a running solve.

| Parameter | Meaning and effect |
| --- | --- |
| CP-SAT strategy | **Partitioning strategy (potential multiple ASUs)** creates seeds, expands territories, jointly reoptimizes touching ASUs, and polishes groups. **Legacy single-ASU solve** builds candidates from remaining tracts one at a time; it can still produce multiple ASUs. Legacy is currently the selected default. |
| UR threshold (τ) | Minimum aggregate rate as a **fraction**. Default `0.0645` means **6.45%**. Do not enter `6.45` here. |
| Population threshold | Minimum combined population of a candidate. Default `10000`. |
| Use saved ASUs as a warm start / Warm-start RDS | Available for CP-SAT strategies, off by default. Upload a saved dashboard `.rds` (`sf` or data frame) containing `GEOID`/`geoid` and `asunum`/`asu_id`. Groups are matched to the current data by GEOID and validated before starting. Disable the checkbox to run without the uploaded file. |
| Statewide joint multi-ASU (experimental) | Separate CP-SAT strategy, not the partitioning checkbox. Builds one joint model over all loaded tracts with up to Max ASUs group slots. Valid component seeds provide a fallback; unused slots can form new ASUs anywhere. Large states and high Max ASUs values can require substantial memory. |
| Statewide seed search (sec) | Shown only for statewide joint. Default `60`; separate connectivity-free initialization budget. `0` skips the relaxation but still uses cheap valid high-rate components as seeds. |
| Statewide joint solve (sec) | Shown only for statewide joint. Default `1800`; one shared budget for model construction and the joint solve, not a per-ASU limit. `0` returns the valid seed fallback. Seed search and data loading are separate. |
| Hint all relaxation-selected tracts | Statewide only; on by default. Supplies partial hints that relaxation-selected tracts belong to some ASU, without choosing ownership or flows. A hint is not a requirement. If the relaxation returns no selection, feasible-seed hints are used instead. Turn off to compare against feasible-seed hints. |
| Tighten statewide joint model | Statewide only; on by default. Adds population-derived minimum tract counts, disconnected-region restrictions, and tighter flow/count bounds. These are necessary conditions, not an artificial tract cap. Turn off for comparison. |
| Graph cuts + connectivity pre-pass (experimental) | Statewide only; off by default for comparison. Adds small-separator cuts and seed-distance/count constraints, followed by a bounded cut-only pre-pass. All cuts carry into the final exact flow solve. Works independently of the tightening and relaxed-hint toggles. |
| Joint multi-ASU expansion (experimental) | Only shown for the partitioning strategy; off by default. Pools up to three neighboring seed territories and jointly assigns their tracts. Each active ASU must independently satisfy connectivity, population, and rate requirements. Batches run sequentially with all workers and 1800 seconds per batch, not per ASU. May use more memory. |
| Max ASUs | Limit used by the ASU creation loop. Default `30`. It is not a target count; fewer groups may be feasible and merges reduce the count. |
| Time limit (sec) per window | Budget for an individual main candidate window (the tracts considered in that solve). Default `18000` is five hours. **Not a whole-run limit.** Use positive seconds. Partition expansion has a separate built-in 1800-second budget per solve, also used by default for final polishing. |
| Incumbent stall limit (sec) | Stops the applicable search after this long without improving its best solution. Default `300` is five minutes; `0` disables it. Bound improvements alone do not reset this timer. |
| Total CP-SAT workers (detected cores - 2) | Threads available to CP-SAT; initialized from detected physical cores with a minimum of one. More workers use more CPU and may use more memory; they do not guarantee better results. |
| Concurrent ASU solves | Maximum simultaneous main candidate solves. Default `1`. Main windows share the worker budget. Partition expansions are sequential and receive the full worker budget. |
| Relative gap (optional) | Allows earlier termination when the solution is close to its bound. `0.01` means approximately 1%. Blank leaves this optional tolerance unset. The gap concerns the current model, not proof of the best statewide arrangement. |
| Limit tracts per ASU (cap + combine) | Optional restriction, **off by default**. Leave off for unrestricted sizes. Legacy combination can exceed the initial cap; partition touching-joint and statewide joint solves keep it. |
| Max tracts per ASU | Appears with the cap enabled. Default `500`. Caps groups except during legacy uncapped combination. |
| Combine/re-solve time limit (sec, optional) | Appears with the cap enabled. Legacy-only budget for combining capped groups; blank uses the main per-window budget. Partition touching checks instead use the expansion/polish budget. |

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
<<<<<<< HEAD
| FINAL_POLISH | Reconsidering ASUs from most to least unemployment captured, with ASU number breaking ties. Each can reconsider its own and unassigned tracts; the order is recomputed after a merge. |
| FINAL_POLISH_MERGE | Restarting polishing after a merge. |
| REGIONAL_EXCHANGE | Letting two or three nearby ASUs exchange tracts together, while keeping every affected ASU valid. Only increases in combined unemployment are accepted. |
=======
| FINAL_POLISH | Reconsidering each ASU with unassigned tracts; additions and removals are possible. |
| FINAL_POLISH_MERGE | Restarting polishing after a merge. |
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
| SINGLE_ASU_TAKEOVER / TAKEOVER_DONOR_REPAIR | Trying a larger replacement and repairing affected groups before accepting or rejecting the attempt. |
| FINAL_RESIDUAL_CHECK | Checking remaining tract components near the end. |

Stages may repeat or be skipped depending on the strategy and results.

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

In **Statewide joint**, imported groups replace automatic seed search and the
relaxed-selection hint. Valid assignment and spanning-tree flow hints are
supplied instead. The saved groups provide the unemployment baseline and
their boundaries may change. When merging is enabled, imported group slots
may deactivate, allowing consolidation and expansion in the same solve. Every
remaining ASU must still meet connectivity, population, and rate requirements;
total unemployment coverage cannot fall below the imported baseline. An active
seeded slot retains at least one of its original tracts. This protects total
coverage, not individual tracts or ASU identities: some saved tracts may be
replaced with others. With merging disabled, all imported slots stay active.
Remaining unseeded slots can form new groups. A timeout with no new solution
retains the validated starting solution (with the usual final touching-merge
check when enabled). The statewide time limit still applies. Invalid imported
groups are still rejected before solving, not repaired by consolidation.

In **Legacy single-ASU** and **Partitioning**, saved groups initialize the
existing assignments. New ASUs are sought in remaining tracts; the normal
later polishing, exchange, and merge rules can reconsider saved groups.
Max ASUs counts both imported and newly created groups.

The `WARM_START` stage reports imported ASUs, assigned tracts, and baseline
unemployment. The original RDS is never modified. The edit tab's existing
**Load Data** control remains separate from this solver warm-start upload.
The statewide model stage reports `seed_consolidation` and `mandatory_groups`;
the completion stage reports `deactivated_seed_slots` separately from post-solve
touching `merges`. Deactivated slots count emptied seed labels, not necessarily
one-to-one pairwise mergers.

For **Statewide joint multi-ASU (experimental)**, Max ASUs limits the number
of simultaneous group slots (also bounded by the number of input tracts),
not the number of batches or a required output count. All loaded tracts are
available to every group; when multiple states are loaded, the entire loaded
graph is considered, with connectivity still enforced for each ASU.

**Optional statewide graph cuts.** Enable **Graph cuts + connectivity pre-pass
(experimental)** to use all three additions:

- Small separators: selecting tracts on both sides of a bottleneck requires
  selecting at least one connector. These cuts do not fix the ASU's root.
- Seed-distance/count rows: selecting a tract at least `d` adjacency steps
  from every original seed tract requires at least `d + 1` selected tracts.
  Unseeded slots have no seed-distance restriction. These are derived bounds,
  not user-imposed tract caps, and allow imported seed consolidation.
- Connectivity cut pre-pass: jointly optimize without flow variables, inspect
  disconnected groups, add root-aware boundary cuts, and repeat for at most
  eight rounds. The pre-pass uses at most 60 seconds and 15% of the remaining
  joint budget, not additional time. Its disconnected trial solutions are not
  published or accepted. Valid improvements strengthen the fallback and hints.

The final solve still enforces exact flow connectivity and preserves the best
validated combined unemployment baseline. Stop/Skip work during the pre-pass;
each solve uses the custom portfolio and configured workers. The stage log
reports separator/distance row counts, `STATEWIDE_JOINT_CUT_ROUND` statistics
(relaxed vs. valid unemployment, disconnected components, and cuts added), and
`STATEWIDE_JOINT_FLOW` when the final flow phase begins. Cuts are capped and
deduplicated; compare runtime, objective, and bounds with the toggle off, since
more constraints are not guaranteed to improve performance.

Valid components from the relaxation and cheap high-rate components are
ranked by signed `q_surplus`, then unemployment. Nonoverlapping valid seeds
are retained up to Max ASUs, and remaining slots are optional, unseeded groups.
Automatically selected valid seeds stay active and retain at least one original tract. Their combined
unemployment is the objective floor, but individual groups may shrink. This
is a seed-constrained optimization, not a guarantee of the unrestricted
statewide optimum. Invalid relaxed components do not become committed ASUs;
their tracts remain available to the joint model. If no valid seeds are found,
all slots are optional and the fallback is an empty assignment.

The relaxed-selection hint includes tracts from components that are not valid
ASUs on their own. It suggests membership in any ASU, leaving ownership, roots,
flows, and unmentioned selections open. The solver may change the hint to make
the result feasible. In this mode, the previous assignment/flow hints are not
also installed, avoiding contradictory hints. Valid seed groups remain the
fallback and still supply the objective floor; the relaxed objective is never
used as that floor.

The tightening option uses these necessary conditions:

- If even the largest `m-1` tract populations cannot reach the population
  threshold, each active ASU needs at least `m` tracts. Other active groups must
  also have room for their minimum counts.
- A connected ASU stays within one connected component of the full tract
  graph. A seeded ASU must use a component containing part of its seed.
  Components with insufficient population cannot host an ASU.
- A spanning-tree flow for an ASU with `c` tracts needs at most `c-1` units on
  any edge. Flow domains also use the size of the graph component, and root
  injection is linked directly to the ASU's tract count.

These bounds preserve feasible tract assignments; they may discard unnecessary
circulating-flow representations. They do not introduce a user tract cap.
For comparisons, keep data, Max ASUs, workers, and both time budgets fixed and
toggle the hint and tightening separately. Improvements in runtime or final
unemployment are not guaranteed.

Statewide joint uses all configured workers and the custom joint portfolio.
Relative gap, incumbent stall, Stop, and Skip apply to the joint search.
Stop or Skip during initialization returns valid seeds without launching the
joint model; Skip during the joint solve ends that solve and finishes this
strategy. Incumbent previews do not trigger early merging. Touching groups
are checked for merging only after the solve, respecting enabled tract limits.
There are no subsequent partition, polishing, takeover, residual, or uncapped
combine solves. The normal per-window and concurrent-solve controls do not
apply. This keeps the statewide experiment's work separate and predictable.

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
on the optional joint-expansion checkbox or the statewide graph-cuts checkbox.
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
the next stale solve. Legacy and statewide merging behavior is unchanged.

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

<<<<<<< HEAD
Regional exchanges run after final polishing, before statewide takeover.
The exchange pass has up to four neighborhood attempts per
run, with at most 60 seconds per attempt and 180 seconds total (or the
final-polish budget, if smaller). Time spent on ordinary expansion and polish
does not use this exchange budget. Unchanged neighborhoods already attempted
are skipped even if their ASU labels have changed. Other ASUs stay fixed.
Stop and Skip also apply to these passes. If no improvement is found, existing
assignments are retained and the build continues to its next stage.
Attempts rotate through nearby anchor ASUs before trying their alternate
pairs. They start with a one-hop halo of unassigned tracts, then widen to
two hops after the smaller neighborhoods have been attempted. Regional roots
can move between ASUs; each resulting ASU must retain at least one of its
original tracts and independently meet the population and rate requirements.

During a run, final polishing skips a previously attempted problem when its
root, incumbent tracts, reachable window, and constraints are unchanged.
The log marks these skips with `FINAL POLISH CACHE`. Changed inputs allow a
new attempt; a cache hit does not mean optimality was proved.
Partition expansion separately reuses proven-optimal territory results when
the territory, root, and constraints match. Partial or gap-limited results
are not cached as optimal territory solutions.

Automatic CP-SAT roots use the eligible tract with the highest rate capacity:
`den × unemployed − num × employed`, the exact scaled form of
`unemployed − threshold × labor force`. Equal capacities favor higher
population, then lower tract index. Individual polish solves keep their root.
Regional exchanges and residual searches choose the highest-capacity tract
within each selection they find, allowing them to replace the original root.

=======
>>>>>>> fe2b02e74c641ae8259311ba9ad97e23ed77101c
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
| Open log / Open folder | Opens the saved log or folder on the computer running R. |

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
| Save Directory Path | Existing folder on the computer running R. Check **Current save directory**: a nonexistent path does not switch the active folder. Exports use this folder too. |
| Save Data | Writes current map data and assignments to `saved_data.rds`. Saving again overwrites it. Rename/copy checkpoints or use separate folders. |
| Load Data / Browse | Restores a saved `.rds` map for editing. It does not restore a running search or all dashboard settings. |

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

