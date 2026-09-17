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
#' @param parallel_asus maximum number of main ASU windows solved concurrently.
#'   Use `1` (the default) to solve main windows sequentially. Partitioned
#'   standalone expansions always run sequentially and each receives all
#'   configured `workers`
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
#' @param use_distance_flow_count_envelope,use_direct_endpoint_gating,use_capacity_radius_pruning,use_capacity_cover_cuts,use_lobe_capacity_cardinality_bounds,use_bisection_max_selected
#'   deprecated compatibility arguments. These experiments are no longer
#'   implemented. Their default `FALSE` is ignored; setting any to `TRUE`
#'   produces a warning instead of silently doing nothing.
#' @param use_connectivity_free_repair logical; solve the unemployment objective
#'   subject to population and UR requirements without connectivity, repair the
#'   selected components, and use the result only when it is a better valid hint
#' @param connectivity_free_time_limit seconds allowed for the connectivity-free
#'   relaxation in each ASU window
#' @param harvest_connectivity_free_asus logical; commit each connected relaxed
#'   component that independently meets the population and UR requirements as a
#'   separate ASU, instead of connecting all valid components to one root
#' @param harvest_all_connectivity_free_components logical; when
#'   `harvest_connectivity_free_asus` is TRUE, seed the territory
#'   partition/expansion step with every relaxed component instead of only the
#'   ones already independently valid. Expanded units that still fail
#'   population/UR/connectivity are released rather than committed, so
#'   correctness is unaffected, but this is unproven for real-data solve speed
#'   (default FALSE)
#' @param partition_seed_strategy seed method: `"connectivity_free"` (default)
#'   or `"surplus_prune"`. Surplus pruning enables full-graph partitioning,
#'   removes lowest-surplus tracts while preserving population, and allows splits
#'   only when all resulting components meet the population threshold. Each is then pruned independently toward the rate threshold. Retained components
#'   are repaired with pruned tracts excluded; only valid seeds enter outward
#'   expansion and polishing. No unrestricted fallback runs if repair yields none.
#' @param expansion_incumbent_stall_seconds seconds without incumbent improvement
#'   before finishing partition repair/expansion. NA inherits the solver default;
#'   zero disables the limit. Does not change final polishing.
#' @param final_consolidation merge touching partition ASUs after all search,
#'   preserving selected tracts and respecting tract-count limits (default TRUE).
#' @param polish_consolidated_asus optionally polish consolidated groups without
#'   releasing selected tracts, then consolidate new contacts (default FALSE).
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
#'   phase. The former post-polish bridge-pair pass is no longer run.
#' @param bridge_pair deprecated compatibility argument; ignored because the
#'   bridge-pair phase has been removed.
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
    configure_subsolvers = TRUE,
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
    harvest_all_connectivity_free_components = FALSE,
    partition_seed_strategy = "connectivity_free",
    standalone_expansion_time_limit = 30,
    use_buffer_asu_merge = FALSE,
    buffer_asu_merge_time_limit = NA_real_,
    final_asu_polish_time_limit = NA_real_,
    bridge_pair = NULL,
    max_nodes_per_asu = NA_integer_,
    exact_nodes_per_asu = NA_integer_,
    combine_capped_asus = TRUE,
    combine_time_limit = NA_integer_,
    verbose = interactive(),
    parallel_asus = 1L,
    use_flow_first_search = FALSE,
    use_tract_capacity_search = FALSE,
    use_flow_capacity_hybrid_search = FALSE,
    expansion_incumbent_stall_seconds = NA_real_,
    final_consolidation = TRUE,
    polish_consolidated_asus = FALSE
) {
  asu_use_python(required = TRUE)
  asu_assert_ortools_version(required = TRUE)

  removed_cut_options <- c(
    use_distance_flow_count_envelope = isTRUE(use_distance_flow_count_envelope),
    use_direct_endpoint_gating = isTRUE(use_direct_endpoint_gating),
    use_capacity_radius_pruning = isTRUE(use_capacity_radius_pruning),
    use_capacity_cover_cuts = isTRUE(use_capacity_cover_cuts),
    use_lobe_capacity_cardinality_bounds = isTRUE(use_lobe_capacity_cardinality_bounds),
    use_bisection_max_selected = isTRUE(use_bisection_max_selected)
  )
  enabled_removed_options <- names(removed_cut_options)[removed_cut_options]
  if (length(enabled_removed_options)) {
    warning(
      "Deprecated cut option(s) are no longer implemented and were ignored: ",
      paste(enabled_removed_options, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(bridge_pair)) {
    warning("`bridge_pair` is ignored: the bridge-pair phase was removed.", call. = FALSE)
    bridge_pair <- NULL
  }

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
    use_articulation_edge_bounds = isTRUE(use_articulation_edge_bounds),
    use_distance_flow_bounds = isTRUE(use_distance_flow_bounds),
    partition_seed_strategy = match.arg(partition_seed_strategy, c("connectivity_free", "surplus_prune")),
    final_consolidation = isTRUE(final_consolidation),
    polish_consolidated_asus = isTRUE(polish_consolidated_asus),
    expansion_incumbent_stall_seconds = if (is.na(expansion_incumbent_stall_seconds)) NULL else as.numeric(expansion_incumbent_stall_seconds),
    harvest_connectivity_free_asus = isTRUE(harvest_connectivity_free_asus),
    harvest_all_connectivity_free_components = isTRUE(harvest_all_connectivity_free_components),
    standalone_expansion_time_limit = as.numeric(standalone_expansion_time_limit),
    final_asu_polish_time_limit = if (is.na(final_asu_polish_time_limit)) NULL else as.numeric(final_asu_polish_time_limit),
    bridge_pair = bridge_pair,
    max_nodes_per_asu = if (is.na(max_nodes_per_asu)) NULL else as.integer(max_nodes_per_asu),
    exact_nodes_per_asu = if (is.na(exact_nodes_per_asu)) NULL else as.integer(exact_nodes_per_asu),
    combine_capped_asus = isTRUE(combine_capped_asus),
    combine_time_limit = if (is.na(combine_time_limit)) NULL else as.integer(combine_time_limit)
  )

  df$asu_id <- as.integer(reticulate::py_to_r(out[["asu_id"]]))
  attr(df, "n_asu") <- as.integer(out[["n_asu"]])
  df
}
