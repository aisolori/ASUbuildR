# -----------------------------------------------------------------------
# run_tract_hunter.R  –  “as‑is” port of your original script
# -----------------------------------------------------------------------

# The functions below break the Tract Hunter algorithm into
# discrete stages so the Shiny dashboard can run each step
# individually.  `run_tract_hunter()` still executes the full
# pipeline for backwards compatibility.

tract_hunter_seed <- function(tract_list,
                              bls_df,
                              ur_thresh   = 0.0645,
                              pop_thresh  = 10000,
                              verbose     = TRUE,
                              model_path  = "C:/Users/anton/Desktop/Code Projects/ASUs/asu_xgb_model.bin"  # ← NEW
) {
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

  # ---- ensure county + pop features exist (and are numeric) ----------
  data_merge <- data_merge |>
    dplyr::mutate(cnty_fips = substr(GEOID, 1, 5))

  if (!all(c("cnty_emp","cnty_unemp","cnty_urate") %in% names(data_merge))) {
    data_merge <- data_merge |>
      dplyr::group_by(cnty_fips) |>
      dplyr::mutate(
        cnty_emp   = sum(tract_ASU_emp,   na.rm = TRUE),
        cnty_unemp = sum(tract_ASU_unemp, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        cnty_urate = dplyr::if_else(cnty_emp + cnty_unemp == 0,
                                    0, cnty_unemp / (cnty_emp + cnty_unemp))
      )
  }

  # If tract_pop2023 missing, fall back to tract_pop_cur only
  if (!"tract_pop2023" %in% names(data_merge) || all(is.na(data_merge$tract_pop2023))) {
    data_merge$tract_pop2023 <- data_merge$tract_pop_cur
  }

  # ensure numeric
  data_merge <- data_merge |>
    dplyr::mutate(
      cnty_emp      = as.numeric(cnty_emp),
      cnty_unemp    = as.numeric(cnty_unemp),
      cnty_urate    = as.numeric(cnty_urate),
      tract_pop2023 = as.numeric(tract_pop2023)
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

  # ensure neighbor list is integer vectors
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

  # ---- 2 · XGBoost score (prob_asu) ---------------------------------
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required. install.packages('xgboost')")
  }

  # ---- build the feature frame in exact order ----
  req_feats <- c("tract_ASU_unemp","tract_ASU_emp","tract_ASU_urate",
                 "cnty_urate","cnty_emp","cnty_unemp","tract_pop2023")

  missing <- setdiff(req_feats, names(data_merge))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))

  feat_df <- as.data.frame(data_merge[req_feats], stringsAsFactors = FALSE)

  # Repair tract_ASU_urate if NA
  bad_urate <- !is.finite(feat_df$tract_ASU_urate)
  if (any(bad_urate)) {
    num <- data_merge$tract_ASU_unemp
    den <- data_merge$tract_ASU_unemp + data_merge$tract_ASU_emp
    feat_df$tract_ASU_urate[bad_urate] <- ifelse(den[bad_urate] == 0, 0, num[bad_urate] / den[bad_urate])
  }

  # ---- robust scalar coercion for any weird list/nested cells ----
  safe_scalar_numeric <- function(x) {
    # returns a numeric vector of length nrow(feat_df)
    if (is.list(x)) {
      vapply(x, function(el) {
        # empty/NULL -> NA
        if (is.null(el) || length(el) == 0) return(NA_real_)
        val <- el
        # unwrap one level if it’s a list
        if (is.list(val)) val <- val[[1]]
        # if it's a matrix/data.frame, take first element
        if (is.matrix(val)) val <- val[1]
        if (is.data.frame(val)) val <- val[[1]][1]
        # if it's a vector length>1, take the first scalar
        val <- val[1]
        suppressWarnings(as.numeric(val))
      }, numeric(1))
    } else if (is.factor(x)) {
      suppressWarnings(as.numeric(as.character(x)))
    } else {
      suppressWarnings(as.numeric(x))
    }
  }

  # Coerce all features to safe numeric scalars
  feat_df[] <- lapply(feat_df, safe_scalar_numeric)

  # Force exact column order (prevents mismatch error)
  feat_df <- feat_df[req_feats]

  # sanity checks
  if (!identical(names(feat_df), req_feats)) stop("Feature order mismatch.")
  bad_len <- vapply(feat_df, length, integer(1)) != nrow(feat_df)
  if (any(bad_len)) stop("Feature length mismatch in: ", paste(names(feat_df)[bad_len], collapse=", "))

  # last guard: no NA-only columns
  all_na <- vapply(feat_df, function(v) all(is.na(v)), logical(1))
  if (any(all_na)) stop("Feature(s) all NA after coercion: ", paste(names(feat_df)[all_na], collapse=", "))

  # ---- score with the saved model ----
  mp  <- normalizePath(model_path, winslash = "/", mustWork = TRUE)
  bst <- xgboost::xgb.load(mp)
  dm  <- xgboost::xgb.DMatrix(data = data.matrix(feat_df))
  data_merge$prob_asu <- as.numeric(predict(bst, dm))
  score_vec <- data_merge$prob_asu



  # ---- 3 · build graph & connect islands ---------------------------
  g    <- igraph::graph_from_adj_list(nb, mode = "all")
  # ... (unchanged) ...

  # ---- 4 · init build state ----------------------------------------
  emp_vec <- emp0
  une_vec <- une0
  pop_vec <- dplyr::coalesce(data_merge$tract_pop_cur, data_merge$tract_pop2023)
  ur_vec  <- ur0

  used     <- integer(0)  # NEW
  tried    <- integer(0)  # NEW
  asu_list <- list()      # NEW


  # score_vec already set to prob_asu above

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

  if (length(asu_list)) {
    for (i in seq_along(asu_list)) {
      data_merge$asunum[asu_list[[i]]] <- i
    }
  } # else: leave all NA if no groups formed


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
      original_asu = as.numeric(names(comps$membership)),
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
  score_vec  <- state$score_vec  # xgb probabilities we saved earlier

  # Ensure asunum exists and is integer
  if (!"asunum" %in% names(data_merge)) {
    data_merge$asunum <- NA_integer_
  } else {
    # coerce any stray characters/factors to integer (preserve NAs)
    data_merge$asunum <- suppressWarnings(as.integer(data_merge$asunum))
  }

  # ensure a column exists on the data for dplyr verbs
  if (!"prob_asu" %in% names(data_merge)) data_merge$prob_asu <- score_vec
  # keep legacy name so old code doesn't break
  data_merge$th_score <- data_merge$prob_asu


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
    found_neighbors   <- current_neighbors[current_neighbors %in% asu_indexes]

    if (length(found_neighbors) == 0) {
      visited       <- target_index
      current_level <- current_neighbors
      while (length(found_neighbors) == 0 && length(current_level) > 0) {
        visited        <- c(visited, current_level)
        next_level     <- unique(unlist(nb[current_level]))
        next_level     <- setdiff(next_level, visited)
        found_neighbors <- next_level[next_level %in% asu_indexes]
        current_level  <- next_level
      }
    }

    # Gather all candidate paths
    all_paths <- list()
    for (nbr in found_neighbors) {
      tmp <- igraph::k_shortest_paths(g, from = nbr, to = target_index, mode = "all", k = 20)

      all_paths <- c(all_paths,
                     Filter(function(x) length(x) > 0, tmp$vpath))
    }
    if (length(all_paths) == 0) return(NULL)

    # Score each path by sum of th_score over the new tracts
    path_scores <- vapply(all_paths, function(path) {
      ids       <- as.integer(path)
      new_tr    <- setdiff(ids, asu_indexes)
      if (length(new_tr) == 0) return(NA_real_)
      sum(score_vec[new_tr], na.rm = TRUE)
    }, numeric(1))

    # Keep only paths that actually add at least one tract
    valid <- !is.na(path_scores)
    if (!any(valid)) return(NULL)

    best_idx <- which.max(path_scores[valid])
    chosen   <- all_paths[[ which(valid)[best_idx] ]]

    list(
      new_tracts = setdiff(as.integer(chosen), asu_indexes),
      neighbors  = found_neighbors,
      full_path  = chosen
    )
  }


  update_tract_data <- function(target_index) {
    # which tracts currently in an ASU
    all_asu_indexes <- which(!is.na(data_merge$asunum))

    # Path to the closest ASU, scored by your model probs (already in score_vec)
    path_finder <- find_boundary_path(target_index,
                                      asu_indexes = data_merge$row_num[all_asu_indexes])

    if (is.null(path_finder) || length(path_finder$new_tracts) == 0) return(FALSE)

    new_indexes <- path_finder$new_tracts

    # Which ASU are we attaching to?
    asu_being_processed <- data_merge$asunum[as.numeric(path_finder$full_path[[1]])]
    asu_indexes <- which(data_merge$asunum == asu_being_processed)

    # Articulation points: never drop these (or the just-added ones)
    union_indexes <- sort(c(asu_indexes, new_indexes))
    sub_g <- igraph::induced_subgraph(g, vids = union_indexes)
    cp <- igraph::articulation_points(sub_g)
    cut_verts <- if (length(cp) > 0) union_indexes[cp] else integer(0)
    invalid_drop_ids <- c(cut_verts, new_indexes)
    drop_candidates <- setdiff(asu_indexes, invalid_drop_ids)

    # Current & proposed sums
    remaining_indexes <- asu_indexes
    total_new_unemp <- sum(unemp_vec[new_indexes], na.rm = TRUE)
    total_new_emp   <- sum(emp_vec[new_indexes],   na.rm = TRUE)
    remaining_unemp <- sum(unemp_vec[remaining_indexes], na.rm = TRUE)
    remaining_emp   <- sum(emp_vec[remaining_indexes],   na.rm = TRUE)

    denom <- remaining_unemp + total_new_unemp + remaining_emp + total_new_emp
    if (denom <= 0) return(FALSE)

    new_ur <- (remaining_unemp + total_new_unemp) / denom

    # If adding the path already clears the threshold, just add it
    if (new_ur >= ur_thresh) {
      data_merge[new_indexes, "asunum"] <<- as.integer(asu_being_processed)
      return(TRUE)
    }

    # Otherwise try greedy drops scored by the new C++ function
    dropped_indexes <- integer(0)
    trade_complete  <- FALSE
    unemp_buffer    <- total_new_unemp
    gamma_penalty   <- 100  # tune this (0 = ignore model prob, larger = protect high-prob tracts)

    while (new_ur < ur_thresh) {
      if (length(drop_candidates) == 0) return(FALSE)

      # Optional: a cheap per-tract “shape penalty” (e.g., boundary length share)
      # Start with zeros if you don't have one yet:
      if (!exists("border_cost")) border_cost <- rep(0, nrow(data_merge))

      # BEFORE calling the C++:
      # 1) Normalize unemployment counts to [0,1] over the current ASU
      u_scale <- max(1, sum(unemp_vec[asu_indexes], na.rm = TRUE))
      e_scale <- max(1, sum(emp_vec[asu_indexes],   na.rm = TRUE))

      unemp_vec_n <- unemp_vec / u_scale
      emp_vec_n   <- emp_vec   / e_scale

      # 2) Ensure score_vec is a probability in [0,1]
      score_vec_n <- pmin(pmax(score_vec, 0), 1)

      # 3) border_cost in [0,1] (0 OK if you don’t use it)
      border_cost_n <- if (exists("border_cost")) {
        rng <- range(border_cost, na.rm = TRUE)
        if (diff(rng) > 0) (border_cost - rng[1]) / diff(rng) else border_cost*0
      } else {
        rep(0, length(unemp_vec))
      }

      # Then pass the *_n vectors into the drop chooser
      drop_res <- choose_best_drop_candidate_xgb2(
        drop_candidates,
        unemp_vec_n, emp_vec_n,
        score_vec_n,
        border_cost_n,
        remaining_unemp / u_scale, remaining_emp / e_scale,
        total_new_unemp / u_scale, total_new_emp / e_scale,
        unemp_buffer / u_scale,
        alpha = .24, beta = 1, gamma = 0.0, delta = 0, p = 0
      )


      new_drop_index <- drop_res$best_index
      if (is.na(new_drop_index)) return(FALSE)

      dropped_indexes   <- c(dropped_indexes, new_drop_index)
      remaining_indexes <- setdiff(remaining_indexes, new_drop_index)
      remaining_unemp   <- remaining_unemp - unemp_vec[new_drop_index]
      remaining_emp     <- remaining_emp   - emp_vec[new_drop_index]

      # Don't let drops exceed the unemployment you're trying to bring in
      if (sum(unemp_vec[dropped_indexes], na.rm = TRUE) > unemp_buffer) return(FALSE)

      denom <- remaining_unemp + total_new_unemp + remaining_emp + total_new_emp
      if (denom <= 0) return(FALSE)

      new_ur <- (remaining_unemp + total_new_unemp) / denom
      if (new_ur >= ur_thresh) {
        trade_complete <- TRUE
        break
      }

      # Recompute valid drop set (protect articulation points & new path)
      union_indexes <- sort(c(remaining_indexes, new_indexes))
      sub_g <- igraph::induced_subgraph(g, vids = union_indexes)
      cp <- igraph::articulation_points(sub_g)
      cut_verts <- if (length(cp) > 0) union_indexes[cp] else integer(0)
      invalid_drop_ids <- c(cut_verts, new_indexes)
      drop_candidates <- setdiff(remaining_indexes, invalid_drop_ids)
      if (length(drop_candidates) == 0) return(FALSE)
    }

    if (trade_complete) {
      data_merge[dropped_indexes, "asunum"] <<- NA_integer_
      data_merge[new_indexes, "asunum"]    <<- as.integer(asu_being_processed)
      return(TRUE)
    } else {
      return(FALSE)
    }
  }


  repeat {
    data_merge_local <- data_merge

    tracts_not_in_asu <- data_merge_local %>%
      dplyr::filter(is.na(asunum)) %>%
      dplyr::mutate(score = ((1+ur)**2) * tract_ASU_unemp) %>%
      dplyr::arrange(-th_score)   # th_score now = prob_asu


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
