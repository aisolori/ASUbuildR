#' Build ASUs using OR-Tools CP-SAT (Python) from an R data frame
#'
#' @param df data.frame with columns: geoid, tract_ASU_unemp, tract_ASU_emp, tract_pop2024
#' @param neighbors list of integer vectors (0-based or 1-based ok). If NULL, you must use CLI with --geometry.
#' @param tau numeric unemployment rate threshold (e.g. 0.0645)
#' @param pop_thresh integer population threshold
#' @param max_asus max number of ASUs to carve
#' @param time_limit seconds per window
#' @param workers CP-SAT threads
#' @param rel_gap optional relative MIP gap (e.g. 0.01)
#' @param configure_subsolvers logical; if `FALSE`, use OR-Tools' default
#'   subsolver portfolio instead of the ASU-specific portfolio
#' @param use_tract_first_search logical; enable the experimental
#'   incumbent-boundary worker, trying safe exclusions before frontier
#'   additions, within the ASU-specific portfolio
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
#' @param max_nodes_per_asu optional integer cap on the number of tracts per
#'   ASU (`NA_integer_` disables the cap, the default). When set, ASUs are
#'   built up to this size, then touching capped ASUs are combined and
#'   re-solved (uncapped) via CP-SAT in a final improvement pass -- see
#'   `combine_capped_asus`
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
    configure_subsolvers = FALSE,
    use_tract_first_search = FALSE,
    use_flow_count_envelope = TRUE,
    use_small_root_separators = TRUE,
    root_separator_max_size = 3L,
    root_separator_clause_limit = 200L,
    root_separator_target_limit = 128L,
    solution_pool_size = 32L,
    use_bridge_edge_bounds = FALSE,
    max_nodes_per_asu = NA_integer_,
    combine_capped_asus = TRUE,
    combine_time_limit = NA_integer_,
    verbose = interactive()
) {
  asu_use_python(required = TRUE)
  if (!reticulate::py_module_available("ortools"))
    stop("Python env missing 'ortools'. Run ASUbuildR::setup_asu_python() first.")

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
    rel_gap = if (is.na(rel_gap)) NULL else as.numeric(rel_gap),
    verbose = isTRUE(verbose),
    configure_subsolvers = isTRUE(configure_subsolvers),
    use_tract_first_search = isTRUE(use_tract_first_search),
    use_flow_count_envelope = isTRUE(use_flow_count_envelope),
    use_small_root_separators = isTRUE(use_small_root_separators),
    root_separator_max_size = as.integer(root_separator_max_size),
    root_separator_clause_limit = as.integer(root_separator_clause_limit),
    root_separator_target_limit = as.integer(root_separator_target_limit),
    solution_pool_size = as.integer(solution_pool_size),
    use_bridge_edge_bounds = isTRUE(use_bridge_edge_bounds),
    max_nodes_per_asu = if (is.na(max_nodes_per_asu)) NULL else as.integer(max_nodes_per_asu),
    combine_capped_asus = isTRUE(combine_capped_asus),
    combine_time_limit = if (is.na(combine_time_limit)) NULL else as.integer(combine_time_limit)
  )

  df$asu_id <- as.integer(reticulate::py_to_r(out[["asu_id"]]))
  attr(df, "n_asu") <- as.integer(out[["n_asu"]])
  df
}
