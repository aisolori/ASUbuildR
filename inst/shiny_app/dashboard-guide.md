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

Set these before starting. Changing a control does not reconfigure a running solve.

| Parameter | Meaning and effect |
| --- | --- |
| CP-SAT strategy | **Partitioning strategy (potential multiple ASUs)** creates seeds, expands territories, checks merges, and polishes groups. **Legacy single-ASU solve** builds candidates from remaining tracts one at a time; it can still produce multiple ASUs. Legacy is currently the selected default. |
| UR threshold (τ) | Minimum aggregate rate as a **fraction**. Default `0.0645` means **6.45%**. Do not enter `6.45` here. |
| Population threshold | Minimum combined population of a candidate. Default `10000`. |
| Max ASUs | Limit used by the ASU creation loop. Default `30`. It is not a target count; fewer groups may be feasible and merges reduce the count. |
| Time limit (sec) per window | Budget for an individual main candidate window (the tracts considered in that solve). Default `18000` is five hours. **Not a whole-run limit.** Use positive seconds. Partition expansion has a separate built-in 1800-second budget per solve, also used by default for final polishing. |
| Incumbent stall limit (sec) | Stops the applicable search after this long without improving its best solution. Default `300` is five minutes; `0` disables it. Bound improvements alone do not reset this timer. |
| Total CP-SAT workers (detected cores - 2) | Threads available to CP-SAT; initialized from detected physical cores with a minimum of one. More workers use more CPU and may use more memory; they do not guarantee better results. |
| Concurrent ASU solves | Maximum simultaneous main candidate solves. Default `1`. Main windows share the worker budget. Partition expansions are sequential and receive the full worker budget. |
| Relative gap (optional) | Allows earlier termination when the solution is close to its bound. `0.01` means approximately 1%. Blank leaves this optional tolerance unset. The gap concerns the current model, not proof of the best statewide arrangement. |
| Limit tracts per ASU (cap + combine) | Optional restriction, **off by default**. Leave off for unrestricted sizes. Later combination can exceed the initial cap. |
| Max tracts per ASU | Appears with the cap enabled. Default `500`. Restricts the initial capped search, not every final group. |
| Combine/re-solve time limit (sec, optional) | Appears with the cap enabled. Separate budget for combining capped groups; blank uses the main per-window budget. |

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
| PARTITION_EXPANSION | Expanding seeds in assigned territories, one solve at a time. |
| PARTITION_EXPANSION_COMPLETE | Summarizing that round. Rejected seeds did not produce valid ASUs; the whole run can continue. |
| PARTITION_BUILD_MERGE / PARTITION_COMBINE | Joining touching candidates or groups. A merge may start another pass. |
| FINAL_POLISH | Reconsidering each ASU with unassigned tracts; additions and removals are possible. |
| FINAL_POLISH_MERGE | Restarting polishing after a merge. |
| SINGLE_ASU_TAKEOVER / TAKEOVER_DONOR_REPAIR | Trying a larger replacement and repairing affected groups before accepting or rejecting the attempt. |
| FINAL_RESIDUAL_CHECK | Checking remaining tract components near the end. |

Stages may repeat or be skipped depending on the strategy and results.

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

