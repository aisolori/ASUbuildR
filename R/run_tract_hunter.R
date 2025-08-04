# -----------------------------------------------------------------------
# run_tract_hunter.R  –  "as‑is" port of your original script
# -----------------------------------------------------------------------

# The functions below break the Tract Hunter algorithm into
# discrete stages so the Shiny dashboard can run each step
# individually.  `run_tract_hunter()` still executes the full
# pipeline for backwards compatibility.

#' Initialize Tract Hunter state
#'
#' Sets up data structures required by the Tract Hunter algorithm.
#'
#' @param tract_list List of tract data with contiguity information
#' @param bls_df BLS data frame
#' @param ur_thresh Unemployment rate threshold
#' @param pop_thresh Population threshold
#' @param verbose Logical, print progress messages
#' @return A list representing the algorithm state
#' @keywords internal
tract_hunter_seed <- function(tract_list,
                              bls_df,
                              ur_thresh  = 0.0645,
                              pop_thresh = 10000,
                              verbose    = TRUE) {

  update_status <- function(msg) {
    if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
      shiny::incProgress(amount = 0, detail = msg)
    } else if (isTRUE(verbose)) {
      cat("\r", msg); utils::flush.console()
    }
  }

  # ---- 0 · PREP ----------------------------------------------------
  data_merge <- tract_list |>
    dplyr::left_join(bls_df, by = "GEOID") |>
    dplyr::mutate(
      ur      = ifelse(tract_ASU_unemp + tract_ASU_emp == 0,
                       0, tract_ASU_unemp/(tract_ASU_unemp + tract_ASU_emp)),
      row_num = dplyr::if_else(!is.na(row_num), row_num, dplyr::row_number())
    )

  data_merge$continuous <- lapply(data_merge$continuous, function(x) {
    # If it's scalar 0, replace with integer(0)
    if (length(x) == 1 && x == 0) return(integer(0))
    as.integer(x)
  })

  nb <- data_merge$continuous
  g  <- igraph::graph_from_adj_list(nb)

  comps <- igraph::components(g)
  main_component_id <- which.max(comps$csize)
  main_indices <- which(comps$membership == main_component_id)
  island_indices <- which(comps$membership != main_component_id)

  coords <- data.frame(
    INTPTLAT = as.numeric(data_merge$INTPTLAT),
    INTPTLON = as.numeric(data_merge$INTPTLON)
  )

  island_component_ids <- setdiff(unique(comps$membership), main_component_id)

  for (island_id in island_component_ids) {
    island_members <- which(comps$membership == island_id)
    # For all members of this island component, get distances to all in main
    combis <- expand.grid(island= island_members, main= main_indices)
    combis$dist <- sqrt(
      (coords$INTPTLAT[combis$island] - coords$INTPTLAT[combis$main])^2 +
        (coords$INTPTLON[combis$island] - coords$INTPTLON[combis$main])^2
    )
    closest <- combis[which.min(combis$dist), ]
    g <- igraph::add_edges(g, c(closest$island, closest$main))
  }

  emp_vec        <- data_merge$tract_ASU_emp
  unemp_vec      <- data_merge$tract_ASU_unemp
  population_vec <- data_merge$tract_pop_cur
  ur_vec         <- data_merge$ur

  used_indexes           <- integer(0)
  tried_starting_indexes <- integer(0)
  asu_groups             <- list()

  # ---- 1 · SEED‑AND‑EXPAND ---------------------------------------
  repeat {
    all_unused   <- setdiff(seq_along(ur_vec), used_indexes)
    valid_unused <- all_unused[ ur_vec[all_unused] >= ur_thresh ]

    possible_starts <- setdiff(valid_unused, tried_starting_indexes)
    if (length(possible_starts) == 0) break

    starting_index <- possible_starts[which.max(ur_vec[possible_starts])]

    asu_emp   <- emp_vec[starting_index]
    asu_unemp <- unemp_vec[starting_index]
    asu_pop   <- population_vec[starting_index]
    asu_ur    <- ifelse(asu_emp + asu_unemp == 0,
                        0, asu_unemp / (asu_emp + asu_unemp))

    asu_list <- c(starting_index)

    repeat {
      boundary_tracts <- unique(unlist(nb[asu_list]))
      boundary_tracts <- setdiff(boundary_tracts, c(asu_list, used_indexes))
      if (length(boundary_tracts) == 0) break

      res <- choose_best_neighbor(boundary_tracts,
                                  emp_vec, unemp_vec, population_vec,
                                  asu_emp, asu_unemp, asu_pop,
                                  ur_thresh)

      best_index <- res$best_index
      if (is.na(best_index)) break

      best_path  <- best_index
      best_ur    <- res$best_ur
      best_emp   <- res$best_emp
      best_unemp <- res$best_unemp
      best_pop   <- res$best_pop

      new_tracts <- setdiff(best_path, asu_list)
      asu_list   <- c(asu_list, new_tracts)

      asu_emp    <- best_emp
      asu_unemp  <- best_unemp
      asu_pop    <- best_pop
      asu_ur     <- best_ur

      cat(glue::glue("UR: {round(asu_ur, 4)} | Unemp: {round(asu_unemp)} | Emp: {round(asu_emp)} | Pop: {round(asu_pop)}   \r"))
      utils::flush.console()
    }

    if (asu_pop >= pop_thresh && asu_ur >= ur_thresh) {
      asu_groups[[length(asu_groups) + 1]] <- asu_list
      used_indexes <- c(used_indexes, asu_list)
    } else {
      tried_starting_indexes <- c(tried_starting_indexes, starting_index)
    }
  }

  data_merge$asunum <- NA
  for (i in seq_along(asu_groups)) {
    group_rows <- asu_groups[[i]]
    data_merge$asunum[group_rows] <- i
  }

  list(
    data_merge      = data_merge,
    nb              = nb,
    g               = g,
    emp_vec         = emp_vec,
    unemp_vec       = unemp_vec,
    population_vec  = population_vec,
    ur_vec          = ur_vec,
    ur_thresh       = ur_thresh,
    pop_thresh      = pop_thresh
  )
}
#' Combine touching ASU groups
#'
#' Internal helper that merges adjacent ASU groups into one.
#'
#' @param tract_data Tract data with asunum assignments
#' @param nb Neighbor list of contiguity
#' @return Updated tract data with merged groups
#' @keywords internal
combine_asu_groups_internal <- function(tract_data, nb) {
  assigned <- tract_data |> dplyr::filter(!is.na(asunum))
  asu_vec  <- as.integer(tract_data$asunum)
  edges_mat <- build_asu_edges(nb, asu_vec)

  if (nrow(edges_mat) == 0) {
    message("No ASU groups are touching; no merging required.")
    return(tract_data)
  } else {
    edges_unique <- unique(t(apply(edges_mat, 1, sort)))

    asunums <- unique(assigned$asunum)
    vertices_df <- data.frame(name = asunums, stringsAsFactors = FALSE)

    asu_graph <- igraph::graph_from_data_frame(
      d = as.data.frame(edges_unique, stringsAsFactors = FALSE),
      vertices = vertices_df,
      directed = FALSE
    )

    comps <- igraph::components(asu_graph)

    lookup <- data.frame(
      original_asu = names(comps$membership),
      comp = comps$membership,
      stringsAsFactors = FALSE
    )

    new_ids <- lookup |>
      dplyr::group_by(comp) |>
      dplyr::summarize(new_asu = min(original_asu)) |>
      dplyr::ungroup()

    lookup <- lookup |> dplyr::left_join(new_ids, by = "comp")

    tract_data <- tract_data |>
      dplyr::mutate(asunum = ifelse(!is.na(asunum),
                                    lookup$new_asu[match(as.character(asunum), lookup$original_asu)],
                                    asunum))

    message(crayon::yellow("Combined ASU groups based on touching tracts."))
    return(tract_data)
  }
}

#' Execute a single ASU building pass
#'
#' Internal helper that grows ASUs until no high unemployment tracts remain.
#'
#' @param state Internal state list from `tract_hunter_seed`
#' @param verbose Logical, print progress messages
#' @return Updated state list
#' @keywords internal
tract_hunter_asu_pass <- function(state, verbose = TRUE) {

  data_merge <- state$data_merge
  nb         <- state$nb
  g          <- state$g
  emp_vec    <- state$emp_vec
  unemp_vec  <- state$unemp_vec
  ur_thresh  <- state$ur_thresh

  update_status <- function(msg) {
    if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
      shiny::incProgress(amount = 0.00001, detail = msg)
    } else if (isTRUE(verbose)) {
      cat("\r", msg); utils::flush.console()
    }
  }

  unemployment_rate <- function(indexes){
    sum(unemp_vec[indexes])/(sum(emp_vec[indexes])+sum(unemp_vec[indexes]))
  }

  find_boundary_path <- function(target_index, asu_indexes) {
    current_neighbors <- unlist(nb[target_index])
    found_neighbors <- current_neighbors[current_neighbors %in% asu_indexes]

    if (length(found_neighbors) == 0) {
      visited <- target_index
      current_level <- current_neighbors
      while (length(found_neighbors) == 0 && length(current_level) > 0) {
        visited <- c(visited, current_level)
        next_level <- unique(unlist(nb[current_level]))
        next_level <- setdiff(next_level, visited)
        found_neighbors <- next_level[next_level %in% asu_indexes]
        current_level <- next_level
      }
    }

    asu_emp   <- sum(emp_vec[asu_indexes])
    asu_unemp <- sum(unemp_vec[asu_indexes])

    all_paths <- list()
    for (nbr in found_neighbors) {
      paths_temp <- igraph::k_shortest_paths(g, from = nbr, to = target_index, mode = "out", k = 5)
      # Defensive: add only non-empty paths
      all_paths <- c(all_paths, Filter(function(x) length(x) > 0, paths_temp$vpath))
    }

    if (length(all_paths) == 0) {
      return(NULL)
    }

    cands_ur <- c()
    for (path in all_paths) {
      path_ids <- as.numeric(path)
      new_tracts <- setdiff(path_ids, asu_indexes)
      # Defensive: skip empty new_tracts to avoid sum(numeric(0)) issues
      if (length(new_tracts) == 0) next
      path_ur <- (sum(unemp_vec[new_tracts]) + asu_unemp) /
        (sum(unemp_vec[new_tracts]) + sum(emp_vec[new_tracts]) + asu_emp + asu_unemp)
      cands_ur <- c(cands_ur, path_ur)
    }

    # Defensive: after filtering, cands_ur may be empty
    if (length(cands_ur) == 0) {
      return(NULL)
    }

    # Select best candidate
    best_idx <- which.max(cands_ur)
    full_path_to_target <- all_paths[[best_idx]]

    list(
      new_tracts = setdiff(as.numeric(full_path_to_target), asu_indexes),
      neighbors  = found_neighbors,
      full_path  = full_path_to_target
    )
  }

  update_tract_data <- function(target_index) {
    all_asu_indexes <- which(!is.na(data_merge$asunum))

    path_finder <- find_boundary_path(target_index, asu_indexes = data_merge$row_num[all_asu_indexes])
    # 1) If no path was found, bail out:
    if (is.null(path_finder) || length(path_finder$new_tracts) == 0) {
      return(FALSE)
    }
    new_indexes  <- path_finder$new_tracts

    asu_being_processed <- data_merge$asunum[as.numeric(path_finder$full_path[[1]])]
    asu_indexes <- which(data_merge$asunum == asu_being_processed)

    union_indexes <- sort(c(asu_indexes, new_indexes))
    sub_g <- igraph::induced_subgraph(g, vids = union_indexes)
    cp <- igraph::articulation_points(sub_g)
    cut_verts <- if (length(cp) > 0) union_indexes[cp] else integer(0)
    invalid_drop_ids <- c(cut_verts, new_indexes)

    drop_candidates <- setdiff(asu_indexes, invalid_drop_ids)

    remaining_indexes <- asu_indexes
    total_new_unemp <- sum(unemp_vec[new_indexes], na.rm = TRUE)
    total_new_emp   <- sum(emp_vec[new_indexes],   na.rm = TRUE)
    remaining_unemp <- sum(unemp_vec[remaining_indexes], na.rm = TRUE)
    remaining_emp   <- sum(emp_vec[remaining_indexes],   na.rm = TRUE)

    denom <- remaining_unemp + total_new_unemp + remaining_emp + total_new_emp
    if (denom <= 0) {
      # no population/labor in either group — can't improve UR
      return(FALSE)
    }

    new_ur <- (remaining_unemp + total_new_unemp) / denom

    if (new_ur >= ur_thresh) {
      utils::flush.console()
      data_merge[new_indexes, "asunum"] <<- asu_being_processed
      return(TRUE)
    }

    dropped_indexes <- integer(0)
    trade_complete  <- FALSE
    unemp_buffer    <- total_new_unemp

    while (new_ur < ur_thresh) {
      if (length(drop_candidates) == 0) return(FALSE)

      drop_res <- choose_best_drop_candidate(drop_candidates,
                                             unemp_vec, emp_vec,
                                             remaining_unemp, remaining_emp,
                                             total_new_unemp, total_new_emp,
                                             unemp_buffer)
      new_drop_index <- drop_res$best_index
      if (is.na(new_drop_index)) return(FALSE)

      dropped_indexes  <- c(dropped_indexes, new_drop_index)
      remaining_indexes <- setdiff(remaining_indexes, new_drop_index)
      remaining_unemp   <- remaining_unemp - unemp_vec[new_drop_index]
      remaining_emp     <- remaining_emp   - emp_vec[new_drop_index]

      if (sum(unemp_vec[dropped_indexes], na.rm = TRUE) > unemp_buffer) return(FALSE)

      denom <- remaining_unemp + total_new_unemp + remaining_emp + total_new_emp
      if (denom <= 0) {
        # no population/labor in either group — can't improve UR
        return(FALSE)
      }

      new_ur <- (remaining_unemp + total_new_unemp) /denom

      if (new_ur >= ur_thresh) {
        trade_complete <- TRUE
        break
      }

      union_indexes <- sort(c(remaining_indexes, new_indexes))
      sub_g <- igraph::induced_subgraph(g, vids = union_indexes)
      cp <- igraph::articulation_points(sub_g)
      cut_verts <- if (length(cp) > 0) union_indexes[cp] else integer(0)
      invalid_drop_ids <- c(cut_verts, new_indexes)
      drop_candidates <- setdiff(remaining_indexes, invalid_drop_ids)
      if (length(drop_candidates) == 0) return(FALSE)
    }

    if (trade_complete) {
      data_merge[dropped_indexes, "asunum"] <<- NA_character_
      data_merge[new_indexes, "asunum"] <<- asu_being_processed
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  repeat {
    data_merge_local <- data_merge
    tracts_not_in_asu <- data_merge_local |>
      dplyr::filter(is.na(asunum)) |>
      dplyr::arrange(-ur)

    if (nrow(tracts_not_in_asu) == 0L) {
      if (verbose) cat("\nNo more tracts to process.\n")
      break
    }

    successful_update <- FALSE
    n_can <- nrow(tracts_not_in_asu)

    for (i in seq_len(n_can)) {
      target_index <- tracts_not_in_asu$row_num[i]

      ok <- update_tract_data(target_index)

      data_merge_local <- data_merge
      tracts_in_asu    <- data_merge_local$row_num[!is.na(data_merge_local$asunum)]
      unemp_tot        <- sum(unemp_vec[tracts_in_asu])

      if (verbose && (i %% 500L == 1L || i == n_can)) {
        update_status(
          glue::glue("Remaining: {n_can - i} | Unemployed: {unemp_tot}")
        )
      }

      if (isTRUE(ok)) {
        successful_update <- TRUE
        break
      }
    }

    if (!successful_update) {
      if (verbose) cat("\nNone of the target indexes produced an update. Exiting loop.\n")
      break
    }
  }

  state$data_merge <- data_merge
  state
}

#' Merge touching ASU groups and update state
#'
#' @param state Internal state list from `tract_hunter_seed`
#' @return Updated state list
#' @keywords internal
tract_hunter_combine_groups <- function(state) {
  state$data_merge <- combine_asu_groups_internal(state$data_merge, state$nb)
  state
}

#' Finalize Tract Hunter results
#'
#' @param state Internal state list from `tract_hunter_seed`
#' @return List of objects for the dashboard
#' @keywords internal
tract_hunter_finalize <- function(state) {
  data_merge <- state$data_merge
  unemp_vec  <- state$unemp_vec

  data_merge$asunum[is.na(data_merge$asunum)] <- 0

  full_data <- data_merge |>
    dplyr::select(-continuous) |>
    sf::st_as_sf() |>
    sf::st_cast("MULTIPOLYGON", warn = FALSE) |>
    dplyr::mutate(ur = ur * 100,
                  asunum = as.integer(asunum))

  asu_tracts <- full_data |> dplyr::filter(asunum > 0)

  asu_summary <- asu_tracts |>
    sf::st_drop_geometry() |>
    dplyr::group_by(asunum) |>
    dplyr::summarise(
      Tracts     = dplyr::n(),
      Population          = sum(tract_pop_cur,   na.rm = TRUE),
      Unemployment        = sum(tract_ASU_unemp, na.rm = TRUE),
      Employed            = sum(tract_ASU_emp,   na.rm = TRUE),
      `Unemployment Rate` = round(Unemployment/(Unemployment+Employed)*100, 5),
      .groups = "drop"
    )

  list(
    full_data       = full_data,
    asu_tracts      = asu_tracts,
    asu_summary     = asu_summary,
    full_data_reset = full_data,
    asu_data        = asu_tracts
  )
}

#' Execute the Tract Hunter algorithm
#'
#' This is a wrapper that runs all stages of the Tract Hunter algorithm
#' and returns the final results used by the dashboard.
#'
#' @param tract_list List of tract data with contiguity information
#' @param bls_df BLS data frame
#' @param ur_thresh Unemployment rate threshold
#' @param pop_thresh Population threshold
#' @param join_touching Logical, join touching ASUs after first pass
#' @param verbose Logical, print progress messages
#' @return A list of results similar to `run_asu_original()`
#' @keywords internal
run_tract_hunter <- function(tract_list,
                             bls_df,
                             ur_thresh  = 0.0645,
                             pop_thresh = 10000,
                             join_touching = TRUE,
                             verbose    = TRUE) {
  state <- tract_hunter_seed(tract_list, bls_df,
                             ur_thresh = ur_thresh,
                             pop_thresh = pop_thresh,
                             verbose = verbose)

  state <- tract_hunter_asu_pass(state, verbose = verbose)

  if (isTRUE(join_touching)) {
    state <- tract_hunter_combine_groups(state)
    state <- tract_hunter_asu_pass(state, verbose = verbose)
  }

  tract_hunter_finalize(state)
}
