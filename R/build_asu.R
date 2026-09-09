#' Build ASUs using OR-Tools CP-SAT (Python) from an R data frame
#'
#' Requires the managed Python environment created by
#' \code{setup_asu_python()}, including OR-Tools \code{>= 9.15}.
#'
#' @param df data.frame with columns: geoid, tract_ASU_unemp, tract_ASU_emp, tract_pop2024
#' @param neighbors list of integer vectors (0-based or 1-based ok). If NULL, you must use CLI with --geometry.
#' @param tau numeric unemployment rate threshold (e.g. 0.0645)
#' @param pop_thresh integer population threshold
#' @param max_asus max number of ASUs to carve
#' @param time_limit seconds per window
#' @param workers CP-SAT threads
#' @param parallel_asus maximum number of ASU windows or standalone expansions
#'   solved concurrently. Use `1` (the default) to solve ASUs sequentially;
#'   the configured `workers` are then available to each solve
#' @param rel_gap optional relative MIP gap (e.g. 0.01)
#' @param configure_subsolvers logical; if `FALSE`, use OR-Tools' default
#'   subsolver portfolio instead of the ASU-specific portfolio
#' @param use_tract_first_search logical; enable the experimental
#'   incumbent-boundary worker, trying safe exclusions before frontier
#'   additions, within the ASU-specific portfolio
#' @param use_flow_first_search logical; enable the experimental flow-first
#'   worker, choosing the widest flow domain, breaking ties by incident tract
#'   unemployment rate and count, and selecting the minimum value. Requires
#'   `configure_subsolvers = TRUE` and cannot be combined with another custom
#'   fixed-search worker
#' @param use_tract_capacity_search logical; enable the experimental
#'   tract-capacity worker, selecting nonnegative exact unemployment-rate
#'   capacity from highest to lowest, then rejecting negative capacity from
#'   lowest to highest. Requires `configure_subsolvers = TRUE` and cannot be
#'   combined with another custom fixed-search worker
#' @param use_flow_capacity_hybrid_search logical; enable the experimental
#'   hybrid worker: minimize a flow-first prefix, apply tract-capacity
#'   branching, then minimize remaining tract and absolute-flow variables from
#'   farthest to nearest root distance. Requires `configure_subsolvers = TRUE`,
#'   an integer-flow formulation, and cannot be combined with another custom
#'   fixed-search worker
#' @param use_flow_count_envelope logical; dynamically bound signed-flow values
#'   by the number of selected nodes
#' @param use_small_root_separators logical; add valid size-2/3 rooted
#'   vertex-separator clauses
#' @param root_separator_max_size maximum rooted vertex-separator size to add
#' @param root_separator_clause_limit maximum number of rooted separator clauses
#' @param root_separator_target_limit number of highest-value candidate targets
#'   searched for a rooted vertex separator
#' @param solution_pool_size number of feasible solutions retained for LNS
#' @param use_bridge_edge_bounds logical; tighten flow variable domains on
#'   graph bridges using a root-rooted directional bound (the reverse
#'   direction across a bridge is forced to 0). Sound but unproven on real
#'   data -- opt-in, default FALSE
#' @param use_articulation_edge_bounds logical; tighten flow domains on edges
#'   from root-separating articulation vertices into far-side components. The
#'   reverse direction is forced to 0 and capacity is bounded by component size.
#'   Sound but unproven on real data -- opt-in, default FALSE
#' @param use_distance_flow_bounds logical; cap each flow direction by
#'   `max_selected - 1 - dist(root, tail)` using BFS distance from the root.
#'   Sound on cycles (any connected selection admits a rooted spanning-tree
#'   flow within these caps) but unproven on real data -- opt-in, default FALSE
#' @param use_distance_flow_count_envelope logical; same distance-from-root
#'   fact as `use_distance_flow_bounds`, but expressed against the live
#'   `selected_count` model variable (as `use_flow_count_envelope` already
#'   does with the uniform bound) instead of the static `max_selected`
#'   constant, so the bound keeps tightening as CP-SAT narrows
#'   `selected_count` anywhere during the main solve. Sound but unproven on
#'   real data -- opt-in, default FALSE
#' @param use_direct_endpoint_gating logical; add explicit `L*x <= f <= U*x`
#'   linear rows per signed flow endpoint, alongside the existing reified
#'   zero-flow constraints, to expose the same bound directly to the LP
#'   relaxation. Sound but unproven on real data -- opt-in, default FALSE
#' @param use_capacity_radius_pruning logical; vertex-weighted-Dijkstra
#'   generalization of the bridge-subtree fix: hard-fixes any tract whose
#'   cheapest root path already consumes more UR-surplus budget than the
#'   whole window can supply. Sound but unproven on real data -- opt-in,
#'   default FALSE
#' @param use_capacity_cover_cuts logical; disaggregated generalization of
#'   the bridge-subtree/capacity-radius fixes into genuine size-2/3 knapsack
#'   cover cuts (`sum(x_i for i in cover) <= |cover| - 1`) over the same
#'   UR-surplus budget, instead of only fixing individual tracts to 0.
#'   Sound (each cover is provably minimal and jointly infeasible under the
#'   same best-case budget argument) but unproven on real data -- opt-in,
#'   default FALSE
#' @param use_lobe_capacity_cardinality_bounds logical; generalizes the
#'   window-wide capacity cardinality bound into one cut per root-separating
#'   articulation lobe, so the LP can no longer concentrate fractional
#'   below-threshold weight inside a single component the way one pooled
#'   cut allows. Reuses the same global budget as the pooled bound (sound
#'   disaggregation), but real-data solve-time benefit is unproven -- opt-in,
#'   default FALSE
#' @param use_bisection_max_selected logical; tighten the connectivity-free
#'   max-selected-tracts bound via a feasibility bisection on cardinality
#'   ("does any selection of size >= K satisfy pop/UR?") instead of relying
#'   solely on one Maximize call, taking the tighter of the two. Sound (a
#'   query only lowers the bound on a proven CP-SAT INFEASIBLE result; a
#'   timeout leaves it unchanged) but real-data benefit is unproven --
#'   opt-in, default FALSE
#' @param use_connectivity_free_repair logical; solve the unemployment objective
#'   subject to population and UR requirements without connectivity, repair the
#'   selected components, and use the result only when it is a better valid hint
#' @param connectivity_free_time_limit seconds allowed for the connectivity-free
#'   relaxation in each ASU window
#' @param harvest_connectivity_free_asus logical; commit each connected relaxed
#'   component that independently meets the population and UR requirements as a
#'   separate ASU, instead of connecting all valid components to one root
#' @param standalone_expansion_time_limit seconds allowed for each exact CP-SAT
#'   expansion of a standalone relaxed component within its assigned disjoint
#'   territory. The current ASU is a hint and objective floor, so tracts may be
#'   replaced. Touching expanded ASUs are merged and expansion is rerun until
#'   stable; zero commits standalone components without initial expansion
#' @param use_buffer_asu_merge logical; after the touching-ASU combine phase
#'   (independent of `max_nodes_per_asu`/`combine_capped_asus` and
#'   `harvest_connectivity_free_asus`), also group and pool ASUs that touch
#'   directly or share a common "buffer" ASU neighbor (at most one other ASU
#'   apart) -- including that buffer -- into one CP-SAT combine solve per
#'   group, mirroring the touching-ASU combine phase's window-expansion and
#'   fallback-to-original-tracts behavior. Repeats to a fixed point since a
#'   merge can create fresh touches/shared buffers
#' @param buffer_asu_merge_time_limit optional seconds; CP-SAT time limit for
#'   each buffer-ASU-merge combine solve (`NA_real_` falls back to
#'   `combine_time_limit`, then `time_limit`, the default)
#' @param final_asu_polish_time_limit optional seconds for each final sequential
#'   CP-SAT solve over one committed ASU plus every currently unassigned tract,
#'   plus any other ASU's tract reachable through at most one unassigned tract
#'   (so a thin unassigned sliver between two ASUs no longer hides a
#'   worthwhile neighboring tract). A donor ASU is never left below
#'   `pop_thresh` or disconnected by such a steal -- the polish is rejected
#'   and the seed retained if it would be. Current tracts are a hint and
#'   objective floor rather than forced membership; dropped tracts become
#'   available to later ASUs. New touching ASUs are merged transitively and
#'   the polish repeats until no new merge occurs. `NA_real_` uses
#'   `standalone_expansion_time_limit`, and zero disables the final polish
#' @param max_nodes_per_asu optional integer cap on the number of tracts per
#'   ASU (`NA_integer_` disables the cap, the default). When set, ASUs are
#'   built up to this size, then touching capped ASUs are combined and
#'   re-solved (uncapped) via CP-SAT in a final improvement pass -- see
#'   `combine_capped_asus`
#' @param exact_nodes_per_asu optional integer; when set, forces every ASU
#'   built during the main loop to select exactly this many tracts instead of
#'   at most `max_nodes_per_asu` (`NA_integer_` disables it, the default;
#'   takes precedence over `max_nodes_per_asu` when both are set). Intended
#'   for the legacy single-ASU-at-a-time build to test whether fixing the
#'   tract count speeds up the search; the combine and buffer-merge phases are
#'   force-disabled whenever it is set
#' @param combine_capped_asus logical; when `max_nodes_per_asu` is set,
#'   combine touching capped ASUs and improve them via an uncapped CP-SAT
#'   re-solve after the main build loop finishes. Ignored if
#'   `max_nodes_per_asu` is `NA`
#' @param combine_time_limit optional seconds; CP-SAT time limit used
#'   specifically for the uncapped combine/re-solve pass (`NA_integer_` uses
#'   `time_limit`, the default)
#' @param use_capacity_sweep logical; after the main loop and combine phase,
#'   repeatedly seed a brand-new standalone ASU from the remaining tract with
#'   the highest UR-surplus, building its window strictly from other
#'   remaining tracts (never touching already-committed ASUs), until no
#'   remaining tract has positive surplus left. Salvages leftover tracts that
#'   the main loop's own seed selection can never reach on its own (e.g. an
#'   isolated tract that already meets the UR/population thresholds by
#'   itself but has no remaining neighbor)
#' @param capacity_sweep_time_limit seconds allowed for each standalone CP-SAT
#'   solve during the capacity sweep pass
#' @param incumbent_stall_seconds optional seconds; finish each CP-SAT solve
#'   early, keeping its current incumbent, once this many seconds pass with
#'   no incumbent improvement. `NA_real_` (the default) disables this
#'   early-finish check
#' @param verbose logical; print CP-SAT logs
#' @return df with added `asu_id` column (integer; -1 means unassigned)
#' @export
build_asu <- function(
    df,
    neighbors,
    tau = 0.0645,
    pop_thresh = 10000,
    max_asus = 30,
    time_limit = 1200,
    workers = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    rel_gap = NA_real_,
    configure_subsolvers = FALSE,
    use_tract_first_search = FALSE,
    use_flow_count_envelope = TRUE,
    use_small_root_separators = TRUE,
    root_separator_max_size = 3L,
    root_separator_clause_limit = 200L,
    root_separator_target_limit = 128L,
    solution_pool_size = 32L,
    use_bridge_edge_bounds = FALSE,
    use_articulation_edge_bounds = FALSE,
    use_distance_flow_bounds = FALSE,
    use_distance_flow_count_envelope = FALSE,
    use_direct_endpoint_gating = FALSE,
    use_capacity_radius_pruning = FALSE,
    use_capacity_cover_cuts = FALSE,
    use_lobe_capacity_cardinality_bounds = FALSE,
    use_bisection_max_selected = FALSE,
    use_connectivity_free_repair = FALSE,
    connectivity_free_time_limit = 10,
    harvest_connectivity_free_asus = FALSE,
    standalone_expansion_time_limit = 30,
    use_buffer_asu_merge = FALSE,
    buffer_asu_merge_time_limit = NA_real_,
    final_asu_polish_time_limit = NA_real_,
    max_nodes_per_asu = NA_integer_,
    exact_nodes_per_asu = NA_integer_,
    combine_capped_asus = TRUE,
    combine_time_limit = NA_integer_,
    use_capacity_sweep = FALSE,
    capacity_sweep_time_limit = 30,
    incumbent_stall_seconds = NA_real_,
    verbose = interactive(),
    parallel_asus = 1L,
    use_flow_first_search = FALSE,
    use_tract_capacity_search = FALSE,
    use_flow_capacity_hybrid_search = FALSE
) {
  asu_use_python(required = TRUE)
  asu_assert_ortools_version(required = TRUE)

  # Normalize neighbor indexing to 0-based
  if (is.null(neighbors)) stop("Provide `neighbors` as a list of integer vectors (contiguity).")
  n <- nrow(df)
  nb <- lapply(neighbors, as.integer)
  all_indices <- unlist(nb, use.names = FALSE)
  one_based <- length(all_indices) > 0L && !any(all_indices == 0L) &&
    all(all_indices >= 1L & all_indices <= n)
  if (one_based) nb <- lapply(nb, function(v) v - 1L)
  nb <- lapply(nb, function(v) {
    as.list(v[v >= 0L & v < n])
  })

  # Load python module and call
  mod <- asu_load_py()
  # reticulate converts data.frame -> pandas.DataFrame and list(list(int)) -> Python list of lists
  out <- mod$build_many_asus_cpsat(
    df = df,
    nb = nb,
    tau = tau,
    pop_thresh = as.integer(pop_thresh),
    max_asus = as.integer(max_asus),
    time_limit = as.integer(time_limit),
    workers = as.integer(workers),
    parallel_asus = max(1L, as.integer(parallel_asus)),
    rel_gap = if (is.na(rel_gap)) NULL else as.numeric(rel_gap),
    verbose = isTRUE(verbose),
    configure_subsolvers = isTRUE(configure_subsolvers),
    use_tract_first_search = isTRUE(use_tract_first_search),
    use_flow_first_search = isTRUE(use_flow_first_search),
    use_tract_capacity_search = isTRUE(use_tract_capacity_search),
    use_flow_capacity_hybrid_search = isTRUE(use_flow_capacity_hybrid_search),
    use_flow_count_envelope = isTRUE(use_flow_count_envelope),
    use_small_root_separators = isTRUE(use_small_root_separators),
    root_separator_max_size = as.integer(root_separator_max_size),
    root_separator_clause_limit = as.integer(root_separator_clause_limit),
    root_separator_target_limit = as.integer(root_separator_target_limit),
    solution_pool_size = as.integer(solution_pool_size),
    use_bridge_edge_bounds = isTRUE(use_bridge_edge_bounds),
    max_nodes_per_asu = if (is.na(max_nodes_per_asu)) NULL else as.integer(max_nodes_per_asu),
    exact_nodes_per_asu = if (is.na(exact_nodes_per_asu)) NULL else as.integer(exact_nodes_per_asu),
    combine_capped_asus = isTRUE(combine_capped_asus),
    combine_time_limit = if (is.na(combine_time_limit)) NULL else as.integer(combine_time_limit)
  )

  df$asu_id <- as.integer(reticulate::py_to_r(out[["asu_id"]]))
  attr(df, "n_asu") <- as.integer(out[["n_asu"]])
  df
}
