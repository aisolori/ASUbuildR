# -----------------------------------------------------------------------
# run_tract_hunter.R  –  “as‑is” port of your original script
# -----------------------------------------------------------------------

# The functions below break the Tract Hunter algorithm into
# discrete stages so the Shiny dashboard can run each step
# individually.  `run_tract_hunter()` still executes the full
# pipeline for backwards compatibility.

tract_hunter_seed <- function(tract_list,
                              bls_df,
                              ur_thresh  = 0.0645,
                              pop_thresh = 10000,
                              verbose    = TRUE) {
  # Progress updater
  update_status <- function(msg) {
    if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
      shiny::incProgress(amount = 0, detail = msg)
    } else if (isTRUE(verbose)) {
      cat("\r", msg); flush.console()
    }
  }

  # ---- 0 · PREP: join and compute UR --------------------------------
  data_merge <- tract_list %>%
    dplyr::left_join(bls_df, by = "GEOID") %>%
    dplyr::mutate(
      ur      = ifelse(tract_ASU_unemp + tract_ASU_emp == 0,
                       0,
                       tract_ASU_unemp / (tract_ASU_unemp + tract_ASU_emp)
      ),
      row_num = dplyr::row_number()
    )

  # ensure neighbor list is integer vectors
  data_merge$continuous <- lapply(data_merge$continuous, function(x) {
    if (length(x) == 1 && x == 0) return(integer(0))
    as.integer(x)
  })

  # ---- 1 · neighbor statistics --------------------------------------
  emp0 <- data_merge$tract_ASU_emp
  une0 <- data_merge$tract_ASU_unemp
  ur0  <- data_merge$ur
  nb   <- data_merge$continuous

  neigh_emp_tot   <- purrr::map_dbl(nb, ~ sum(emp0[.x], na.rm = TRUE))
  neigh_unemp_tot <- purrr::map_dbl(nb, ~ sum(une0[.x], na.rm = TRUE))
  neigh_emp_avg   <- purrr::map_dbl(nb, ~ mean(emp0[.x], na.rm = TRUE))
  neigh_unemp_avg <- purrr::map_dbl(nb, ~ mean(une0[.x], na.rm = TRUE))
  neigh_ur_tot    <- neigh_unemp_tot / (neigh_unemp_tot + neigh_emp_tot)
  neigh_ur_avg    <- purrr::map_dbl(nb, ~ mean(ur0[.x],   na.rm = TRUE))

  data_merge <- data_merge %>%
    dplyr::mutate(
      neigh_emp_tot   = neigh_emp_tot,
      neigh_unemp_tot = neigh_unemp_tot,
      neigh_emp_avg   = neigh_emp_avg,
      neigh_unemp_avg = neigh_unemp_avg,
      neigh_ur_tot    = neigh_ur_tot,
      neigh_ur_avg    = neigh_ur_avg
    )

  # ---- 2 · scale predictors & score ---------------------------------
  # extract predictors
  pred_df <- data.frame(
    tract_ASU_emp   = emp0,
    tract_ASU_unemp = une0,
    ur              = ur0,
    neigh_emp_tot   = neigh_emp_tot,
    neigh_unemp_tot = neigh_unemp_tot,
    neigh_emp_avg   = neigh_emp_avg,
    neigh_unemp_avg = neigh_unemp_avg,
    neigh_ur_tot    = neigh_ur_tot,
    neigh_ur_avg    = neigh_ur_avg
  )
  # z-score
  pred_scaled <- as.data.frame(
    lapply(pred_df, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  )
  # coefficients
  coefs <- c(
    `(Intercept)`    = 15.5542,
    tract_ASU_emp    = -13.5182,
    tract_ASU_unemp  =  31.2327,
    ur               =  -0.3193,
    neigh_emp_tot    =   1.9780,
    neigh_unemp_tot  =  -1.7317,
    neigh_ur_tot     =  -1.4887,
    neigh_emp_avg    =  -1.6452,
    neigh_unemp_avg  =   1.8233,
    neigh_ur_avg     =   1.0896
  )
  data_merge$th_score <- coefs["(Intercept)"] +
    pred_scaled$tract_ASU_emp   * coefs["tract_ASU_emp"] +
    pred_scaled$tract_ASU_unemp * coefs["tract_ASU_unemp"] +
    pred_scaled$ur              * coefs["ur"] +
    pred_scaled$neigh_emp_tot   * coefs["neigh_emp_tot"] +
    pred_scaled$neigh_unemp_tot * coefs["neigh_unemp_tot"] +
    pred_scaled$neigh_ur_tot    * coefs["neigh_ur_tot"] +
    pred_scaled$neigh_emp_avg   * coefs["neigh_emp_avg"] +
    pred_scaled$neigh_unemp_avg * coefs["neigh_unemp_avg"] +
    pred_scaled$neigh_ur_avg    * coefs["neigh_ur_avg"]

  # ---- 3 · build graph & connect islands ---------------------------
  g    <- igraph::graph_from_adj_list(nb, mode = "all")
  comps<- igraph::components(g)
  main <- which.max(comps$csize)
  iso  <- setdiff(unique(comps$membership), main)
  coords <- data.frame(lat = as.numeric(data_merge$INTPTLAT),
                       lon = as.numeric(data_merge$INTPTLON))
  for (i in iso) {
    mem <- which(comps$membership == i)
    d   <- expand.grid(island = mem, main = which(comps$membership == main))
    d$dist <- sqrt((coords$lat[d$island] - coords$lat[d$main])^2 +
                     (coords$lon[d$island] - coords$lon[d$main])^2)
    nn  <- d[which.min(d$dist),]
    g   <- igraph::add_edges(g, c(nn$island, nn$main))
  }

  # ---- 4 · init build state ----------------------------------------
  emp_vec   <- emp0
  une_vec   <- une0
  pop_vec   <- data_merge$tract_pop_cur
  ur_vec    <- ur0
  score_vec <- data_merge$th_score
  used      <- integer(0)
  tried     <- integer(0)
  asu_list  <- list()

  # ---- 5 · SEED & EXPAND using score -------------------------------
  repeat {
    all_unused   <- setdiff(seq_along(ur_vec), used)
    valid_unused <- all_unused[ur_vec[all_unused] >= ur_thresh]
    starts       <- setdiff(valid_unused, tried)
    if (length(starts) == 0) break
    seed         <- starts[which.max(score_vec[starts])]
    asu          <- seed
    ae <- emp_vec[seed]; au <- une_vec[seed]; ap <- pop_vec[seed]; ar <- ur_vec[seed]
    repeat {
      boundary <- setdiff(unlist(nb[asu]), c(asu, used))
      if (!length(boundary)) break
      next_nb  <- boundary[which.max(score_vec[boundary])]
      ne <- ae + emp_vec[next_nb]; nu <- au + une_vec[next_nb]; np <- ap + pop_vec[next_nb]
      nr <- if ((ne+nu)==0) 0 else nu/(ne+nu)
      if (np < pop_thresh || nr < ur_thresh) break
      asu <- c(asu, next_nb); ae<-ne; au<-nu; ap<-np; ar<-nr
    }
    if (ap >= pop_thresh && ar >= ur_thresh) {
      asu_list[[length(asu_list)+1]] <- asu
      used <- c(used, asu)
    } else {
      tried <- c(tried, seed)
    }
  }

  # ---- 6 · assign ASU numbers --------------------------------------
  data_merge$asunum <- NA_integer_
  for (i in seq_along(asu_list)) {
    data_merge$asunum[asu_list[[i]]] <- i
  }

  # ---- 7 · return state --------------------------------------------
  list(data_merge      = data_merge,
       nb              = nb,
       g               = g,
       emp_vec         = emp0,
       unemp_vec       = une0,
       population_vec  = pop_vec,
       ur_vec          = ur0,
       score_vec       = score_vec,
       ur_thresh       = ur_thresh,
       pop_thresh      = pop_thresh)
}




combine_asu_groups_internal <- function(tract_data, nb) {
  assigned <- tract_data %>% filter(!is.na(asunum))
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

    new_ids <- lookup %>%
      group_by(comp) %>%
      summarize(new_asu = min(original_asu)) %>%
      ungroup()

    lookup <- lookup %>% left_join(new_ids, by = "comp")

    tract_data <- tract_data %>%
      mutate(asunum = ifelse(!is.na(asunum),
                             lookup$new_asu[match(as.character(asunum), lookup$original_asu)],
                             asunum))

    message(crayon::yellow("Combined ASU groups based on touching tracts."))
    return(tract_data)
  }
}

tract_hunter_asu_pass <- function(state, verbose = TRUE) {

  data_merge <- state$data_merge
  nb         <- state$nb
  g          <- state$g
  emp_vec    <- state$emp_vec
  unemp_vec  <- state$unemp_vec
  ur_thresh  <- state$ur_thresh

  update_status <- function(msg) {
    if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
      shiny::incProgress(amount = 0, detail = msg)
    } else if (isTRUE(verbose)) {
      cat("\r", msg); flush.console()
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
      flush.console()
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
    tracts_not_in_asu <- data_merge_local %>%
      filter(is.na(asunum)) %>%
      arrange(-ur)

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

tract_hunter_combine_groups <- function(state) {
  state$data_merge <- combine_asu_groups_internal(state$data_merge, state$nb)
  state
}

tract_hunter_finalize <- function(state) {
  data_merge <- state$data_merge
  unemp_vec  <- state$unemp_vec

  data_merge$asunum[is.na(data_merge$asunum)] <- 0

  full_data <- data_merge %>%
    select(-continuous) %>%
    st_as_sf() %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    mutate(ur = ur * 100,
           asunum = as.integer(asunum))

  asu_tracts <- full_data %>% filter(asunum > 0)

  asu_summary <- asu_tracts %>%
    st_drop_geometry() %>%
    group_by(asunum) %>%
    summarise(
      Tracts     = n(),
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
