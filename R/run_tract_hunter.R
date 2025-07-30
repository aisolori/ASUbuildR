# -----------------------------------------------------------------------
# run_tract_hunter.R  –  “as‑is” port of your original script
# -----------------------------------------------------------------------
run_tract_hunter <- function(tract_list,
                             bls_df,
                             ur_thresh  = 0.0645,
                             pop_thresh = 10000,
                             join_touching  = TRUE,   # <- NEW
                             verbose    = TRUE) {



  # --- progress helper ---------------------------------------------------
  update_status <- function(msg) {
    # If we're running inside a Shiny session *and* inside a withProgress block
    if (requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()) {
      shiny::incProgress(amount = 0, detail = msg)   # 0 ⇒ just change the text
    } else if (isTRUE(verbose)) {
      cat("\r", msg); flush.console()
    }
  }


  # ---- 0 · PREP --------------------------------------------------------
  data_merge <- tract_list %>%
    left_join(bls_df, by = "GEOID") %>%
    mutate(
      ur      = ifelse(tract_ASU_unemp+tract_ASU_emp ==0, 0, tract_ASU_unemp/(tract_ASU_unemp+tract_ASU_emp)),
      row_num = if_else(!is.na(row_num), row_num, row_number())
    )

  nb <- data_merge$continuous
  g  <- igraph::graph_from_adj_list(nb)

  emp_vec        <- data_merge$tract_ASU_emp
  unemp_vec      <- data_merge$tract_ASU_unemp
  population_vec <- data_merge$tract_pop_cur
  ur_vec         <- data_merge$ur

  used_indexes           <- integer(0)
  tried_starting_indexes <- integer(0)
  asu_groups             <- list()

  # ---- helper: BFS paths (unchanged) ----------------------------------
  # BFS to get *all* paths up to k hops
  bfs_paths_up_to_k_hops <- function(start, nb, max_hops = 3, blocked = integer(0)) {

    # Each queue element is a list: (path_vec, depth)
    queue <- list(list(path = c(start), depth = 0))
    all_paths <- list()

    while (length(queue) > 0) {
      current <- queue[[1]]
      queue <- queue[-1]

      path_vec <- current$path
      depth    <- current$depth

      # If we've reached the max depth, store the path
      if (depth == max_hops) {
        all_paths[[ length(all_paths) + 1 ]] <- path_vec
        next
      }

      # Otherwise, expand neighbors of the last node in the path
      last_node <- tail(path_vec, 1)
      these_neighbors <- nb[[last_node]]

      # Exclude blocked or already in path
      these_neighbors <- setdiff(these_neighbors, c(path_vec, blocked))

      if (length(these_neighbors) == 0) {
        # No further expansion from here
        all_paths[[ length(all_paths) + 1 ]] <- path_vec
      } else {
        # Enqueue expansions
        for (nxt in these_neighbors) {
          new_path <- c(path_vec, nxt)
          queue[[ length(queue) + 1 ]] <- list(path = new_path, depth = depth + 1)
        }
      }
    }

    return(all_paths)
  }

  # ---- 1 · SEED‑AND‑EXPAND -------------------------------------------
  repeat {
    # 1) Identify all **unused** tracts with UR >= 0.0645
    all_unused <- setdiff(seq_along(ur_vec), used_indexes)
    valid_unused <- all_unused[ ur_vec[all_unused] >= .0645]

    # Exclude any that we already tried as a starting point
    possible_starts <- setdiff(valid_unused, tried_starting_indexes)

    # If none remain, we're done
    if (length(possible_starts) == 0) {
      # message("No more possible starting tracts meeting UR >= 0.0645 that haven't been tried.")
      break
    }

    # 2) Pick the starting tract with the highest unemployment among possible_starts
    starting_index <- possible_starts[which.max(ur_vec[possible_starts])]

    # Initialize metrics for the new ASU
    asu_emp   <- emp_vec[starting_index]
    asu_unemp <- unemp_vec[starting_index]
    asu_pop   <- population_vec[starting_index]
    asu_ur    <- ifelse(asu_emp+asu_unemp == 0, 0, asu_unemp / (asu_emp + asu_unemp))

    # Tracts in this ASU
    asu_list <- c(starting_index)

    repeat {

      # 1) Identify the ASU boundary: any neighbor of asu_list that is not in asu_list or used_indexes
      boundary_tracts <- unique(unlist(nb[asu_list]))
      boundary_tracts <- setdiff(boundary_tracts, c(asu_list, used_indexes))

      # If no boundary remains, we can't expand
      if (length(boundary_tracts) == 0) {
        # message("No boundary tracts remain; can't expand further.")
        break
      }

      # 2) Find all possible "candidate" tracts out to X hops (2 or 3),
      #    but do *not* skip bridging tracts along the way.
      #    We'll BFS from each boundary tract to possible "far" tracts
      #    collecting all contiguous paths that stay out of used_indexes & asu_list.

      best_improvement <- -Inf  # track the best improvement or best final unemp, etc.
      best_path <- NULL         # store the path (bridge + final neighbor)
      best_ur   <- NA
      best_emp  <- NA
      best_unemp <- NA
      best_pop   <- NA

      for (b in boundary_tracts) {

        # BFS or DFS up to 2 or 3 hops from b, ignoring asu_list & used_indexes
        candidate_paths <- bfs_paths_up_to_k_hops(start = b, nb = nb,
                                                  max_hops = 0,
                                                  blocked = c(asu_list, used_indexes))

        # candidate_paths is a *list of paths*, each a vector of tract indices
        # The last node in each path is the "far" neighbor.

        for (path_vec in candidate_paths) {

          # The set of new tracts if we add path_vec to the ASU
          # (We also include bridging tracts if they're in path_vec)
          new_tracts <- setdiff(path_vec, asu_list)  # any that aren't already in the ASU

          # Calculate new totals if we add all of them
          total_emp   <- asu_emp   + sum(emp_vec[new_tracts])
          total_unemp <- asu_unemp + sum(unemp_vec[new_tracts])
          total_pop   <- asu_pop   + sum(population_vec[new_tracts])
          total_ur    <- total_unemp / (total_emp + total_unemp)

          # Check if this keeps UR above threshold
          if (total_ur >= 0.0645) {
            # If your main goal is to *maximize total_unemp*:
            #   improvement <- total_unemp
            # Or to maximize final UR:
            #   improvement <- total_ur
            # Or any other logic.
            improvement <- ((total_unemp)^.9) * total_ur

            if(is.na(improvement)){
              improvement <-0
            }

            if (improvement > best_improvement) {
              best_improvement <- improvement
              best_path        <- path_vec
              best_ur          <- total_ur
              best_emp         <- total_emp
              best_unemp       <- total_unemp
              best_pop         <- total_pop
            }
          }


        } # end loop over candidate_paths
      } # end loop over boundary tracts

      # If we never found a path that yields UR >= 0.0645, we stop
      if (is.null(best_path)) {
        #message("No contiguous path found that maintains UR >= 0.0645.")
        break
      }

      # Otherwise, add all bridging tracts in best_path to the ASU
      new_tracts <- setdiff(best_path, asu_list)
      asu_list   <- c(asu_list, new_tracts)

      # Update your metrics
      asu_emp    <- best_emp
      asu_unemp  <- best_unemp
      asu_pop    <- best_pop
      asu_ur     <- best_ur

      cat(glue::glue("UR: {round(asu_ur, 4)} | Unemp: {round(asu_unemp)} | Emp: {round(asu_emp)} | Pop: {round(asu_pop)}   \r"))
      flush.console()  # Especially important in RGui or RStudio


      # (loop repeats, looking for the next set of bridging expansions)
    }

    # ---------------------------------------------------------------------
    # 4) Check final criteria for this ASU (pop >= 10000, UR >= 0.0645)
    # ---------------------------------------------------------------------
    if (asu_pop >= 10000 && asu_ur >= 0.0645) {
      # It's a valid ASU; save it and mark these tracts used
      asu_groups[[length(asu_groups) + 1]] <- asu_list
      used_indexes <- c(used_indexes, asu_list)
      # message(glue("Valid ASU created. pop={asu_pop}, UR={asu_ur}"))
    } else {
      # Not valid -> record that we tried this start and skip it next time
      tried_starting_indexes <- c(tried_starting_indexes, starting_index)
      # message(glue(
      #   "ASU not valid. pop={asu_pop}, UR={asu_ur}. Marking tract {starting_index} as tried."
      # ))
    }
  }



  data_merge$asunum <- NA
  for (i in seq_along(asu_groups)) {
    # Each asu_groups[[i]] is a vector of tract row indices in data_merge
    group_rows <- asu_groups[[i]]

    # Assign ASU number `i` to these rows
    data_merge$asunum[group_rows] <- i
  }

  # ---- helpers for trade / merge (identical to original) --------------
  unemployment_rate <- function(indexes){
    return(sum(unemp_vec[indexes])/(sum(emp_vec[indexes])+sum(unemp_vec[indexes])))
  }


  find_boundary_path <- function(target_index, asu_indexes) {
    # --- Step 1: Find the asu neighbor(s) ---

    # Get direct neighbors of the target tract.
    current_neighbors <- unlist(nb[target_index])
    # Check if any direct neighbor is in asu_indexes.
    found_neighbors <- current_neighbors[current_neighbors %in% asu_indexes]

    # If no direct neighbors are in asu_indexes, iteratively expand.
    if (length(found_neighbors) == 0) {
      # Initialize a vector of visited nodes to avoid re-expanding the same nodes.
      visited <- target_index
      # Set current level to the direct neighbors of target.
      current_level <- current_neighbors

      # Iteratively expand until at least one neighbor in asu_indexes is found.
      while (length(found_neighbors) == 0 && length(current_level) > 0) {
        # Mark these nodes as visited.
        visited <- c(visited, current_level)

        # Expand: get neighbors for all nodes in the current level.
        next_level <- unique(unlist(nb[current_level]))

        # Remove nodes that have already been visited.
        next_level <- setdiff(next_level, visited)

        # Check if any of the new nodes are in asu_indexes.
        found_neighbors <- next_level[next_level %in% asu_indexes]

        # Update current_level.
        current_level <- next_level
      }
    }

    asu_emp <- sum(emp_vec[asu_indexes])
    asu_unemp <- sum(unemp_vec[asu_indexes])

    # valid_neighbors <- c()
    # for (neighbor in found_neighbors){
    #   neighbor_ur <- unemployment_rate(c(neighbor,target_index))
    #
    #   valid_neighbors <- c(valid_neighbors,neighbor_ur)
    # }
    #
    # optimal_neighbor <- found_neighbors[which.max(valid_neighbors)]


    # Display the found neighbors in asu_indexes (for diagnostic purposes).
    #cat("Found neighbor(s) in asu_indexes:", found_neighbors, "\n")

    # --- Step 2: Find All Simple Paths ---
    # Using igraph's all_simple_paths() to get all paths from each found neighbor to the target.
    all_paths <- list()  # container for candidate paths
    for (nbr in found_neighbors) {
      # Using a cutoff (here 5) to limit path length. Adjust the cutoff as needed.
      paths_temp <- igraph::k_shortest_paths(g, from = nbr, to = target_index, mode = "out", k = 50)
      all_paths <- c(all_paths, paths_temp$vpath)
    }



    # --- Step 3: Compute the Candidate Unemployment Ratio for Each Path ---
    cands_ur <- c()  # container for candidate unemployment rates
    for (path in all_paths) {
      # Convert vertex sequence to a vector of vertex IDs
      path_ids <- as.numeric(path)
      # Remove asu indexes from the path
      new_tracts <- setdiff(path_ids, asu_indexes)

      # Compute the unemployment rate for the path.
      # (Assumes unemp_vec and emp_vec have been defined and are indexed similarly to the graph.)
      path_ur <- (sum(unemp_vec[new_tracts])+asu_unemp) / (sum(unemp_vec[new_tracts]) + sum(emp_vec[new_tracts])+asu_emp+asu_unemp)

      cands_ur <- c(cands_ur, path_ur)
    }

    # Check if any paths were found.
    if (length(cands_ur) == 0) {
      #stop("No candidate paths found that connect the target to asu_indexes")
      break
    }

    # Get the full path (as a vertex sequence) corresponding to the maximum unemployment ratio.
    full_path_to_target <- all_paths[[which.max(cands_ur)]]

    # Return the set difference between the full path and asu_indexes.
    result <- list(
      new_tracts = setdiff(as.numeric(full_path_to_target), asu_indexes),
      neighbors = found_neighbors,
      full_path = full_path_to_target
    )
    return(result)
  }

  update_tract_data <- function(target_index) {
    # 1. Use base R indexing to get all asu indexes (non-NA)
    all_asu_indexes <- which(!is.na(data_merge$asunum))

    # 2. Find the boundary path using your helper
    # (Assuming data_merge$index contains numeric indexes)
    path_finder <- find_boundary_path(target_index, asu_indexes = data_merge$row_num[all_asu_indexes])
    new_indexes <- path_finder$new_tracts

    # 3. Determine the ASU being processed from the boundary path (using the first entry)
    asu_being_processed <- data_merge$asunum[as.numeric(path_finder$full_path[[1]])]

    # 4. Gather all indexes for the current ASU using base R filtering
    asu_indexes <- which(data_merge$asunum == asu_being_processed)

    # 5. Create the union and induced subgraph (compute once)
    union_indexes <- sort(c(asu_indexes, new_indexes))
    sub_g <- igraph::induced_subgraph(g, vids = union_indexes)

    # 6. Get cut vertices from subgraph and compute invalid drop ids
    cp <- igraph::articulation_points(sub_g)
    cut_verts <- if (length(cp) > 0) union_indexes[cp] else integer(0)
    invalid_drop_ids <- c(cut_verts, new_indexes)

    # 7. Determine drop candidates (asu_indexes not on the boundary or among new_indexes)
    drop_candidates <- setdiff(asu_indexes, invalid_drop_ids)

    # 8. Set the initial remaining indexes and precompute sums
    remaining_indexes <- asu_indexes
    total_new_unemp <- sum(unemp_vec[new_indexes], na.rm = TRUE)
    total_new_emp   <- sum(emp_vec[new_indexes],   na.rm = TRUE)
    remaining_unemp <- sum(unemp_vec[remaining_indexes], na.rm = TRUE)
    remaining_emp   <- sum(emp_vec[remaining_indexes],   na.rm = TRUE)

    new_ur <- (remaining_unemp + total_new_unemp) /
      (remaining_unemp + total_new_unemp + remaining_emp + total_new_emp)

    # 9. Early exit: if the new rate is already acceptable, update and return TRUE.
    if (new_ur >= 0.0645) {
      flush.console()
      # cat(green(glue::glue("\n Adding in {toString(new_indexes)}, no trade needed.\n Added {sum(unemp_vec[new_indexes])} unemp \n")))
      # flush.console()
      data_merge[new_indexes, "asunum"] <<- asu_being_processed
      return(TRUE)
    }

    # 10. Initialize iterative dropping process
    dropped_indexes <- integer(0)
    trade_complete <- FALSE
    unemp_buffer <- total_new_unemp  # Total unemp being added

    # Iterate until the updated unemployment rate meets the threshold
    while(new_ur < 0.0645) {
      # Filter candidates based on whether their unemp is less than the unemp buffer
      drop_candidates <- drop_candidates[ unemp_vec[drop_candidates] < unemp_buffer ]
      if (length(drop_candidates) == 0) return(FALSE)

      # Vectorized evaluation: calculate candidate new sums if each candidate were dropped
      candidate_new_unemp <- remaining_unemp - unemp_vec[drop_candidates] + total_new_unemp
      candidate_new_emp   <- remaining_emp   - emp_vec[drop_candidates]   + total_new_emp
      candidate_new_ur    <- candidate_new_unemp / (candidate_new_unemp + candidate_new_emp)

      # Choose candidate with maximum resulting unemployment rate
      new_drop_index <- drop_candidates[ which.max(candidate_new_ur) ]

      # Update: add candidate to dropped_indexes and remove it from remaining_indexes
      dropped_indexes <- c(dropped_indexes, new_drop_index)
      remaining_indexes <- setdiff(remaining_indexes, new_drop_index)

      remaining_unemp <- remaining_unemp - unemp_vec[new_drop_index]
      remaining_emp   <- remaining_emp   - emp_vec[new_drop_index]

      # Safety check: if dropped too much, break out
      if(sum(unemp_vec[dropped_indexes], na.rm = TRUE) > unemp_buffer) return(FALSE)

      # Recalculate new unemployment rate
      new_ur <- (remaining_unemp + total_new_unemp) /
        (remaining_unemp + total_new_unemp + remaining_emp + total_new_emp)

      # Compute improvement and current combined unemployment level
      current_improvement <- total_new_unemp - sum(unemp_vec[dropped_indexes], na.rm = TRUE)
      current_total_unemp <- remaining_unemp + total_new_unemp

      # Report status after candidate drop
      # status_msg <- glue::glue("Dropped candidate {new_drop_index} | Improvement so far: {current_improvement} | Combined Unemp: {current_total_unemp} | New UR: {round(new_ur, 4)}")
      # cat(green(status_msg), "\n")
      # flush.console()

      if(new_ur >= 0.0645) {
        # cat("Hooray!\n")
        # flush.console()
        trade_complete <- TRUE
        break
      }

      # Update drop candidates: recompute subgraph and invalid drop ids
      union_indexes <- sort(c(remaining_indexes, new_indexes))
      sub_g <- igraph::induced_subgraph(g, vids = union_indexes)
      cp <- igraph::articulation_points(sub_g)
      cut_verts <- if(length(cp) > 0) union_indexes[cp] else integer(0)
      invalid_drop_ids <- c(cut_verts, new_indexes)
      drop_candidates <- setdiff(remaining_indexes, invalid_drop_ids)

      # Filter drop candidates again
      drop_candidates <- drop_candidates[ unemp_vec[drop_candidates] < unemp_buffer ]
      if (length(drop_candidates) == 0) return(FALSE)
    }

    # 11. Finalize the update if the trade succeeded
    if(trade_complete) {
      improvement <- total_new_unemp - sum(unemp_vec[dropped_indexes], na.rm = TRUE)
      # Clear the line with a newline, ensuring you’re on a new line:
      # cat("\n\r")
      # flush.console()
      # cat(green(glue::glue("\nTrade Complete, adding {toString(new_indexes)} and dropping {toString(dropped_indexes)} ")))
      # cat(green(glue::glue(" Improved unemp by {improvement}\n")))
      # flush.console()


      data_merge[dropped_indexes, "asunum"] <<- NA_character_
      data_merge[new_indexes, "asunum"] <<- asu_being_processed
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  combine_asu_groups <- function(tract_data, nb) {

    # ------------------------------------------------
    # STEP 1: Build an edge list between ASUs based on touching tracts.
    # ------------------------------------------------

    # Filter to tracts that are assigned to an ASU
    assigned <- tract_data %>% filter(!is.na(asunum))
    edge_list <- list()

    # For each assigned tract, look at its neighbors.
    for (i in seq_len(nrow(assigned))) {
      # IMPORTANT: if your 'row_num' column is not the row number, you might need a mapping.
      tract_id <- assigned$row_num[i]
      asu_current <- assigned$asunum[i]

      # Get the neighbor tract indexes from nb.
      # Ensure that "tract_id" here correctly matches the row indexing of nb.
      neighbs <- nb[[tract_id]]
      if (length(neighbs) == 0) next

      for (n in neighbs) {
        # Get neighbor's asunum (if any)
        asu_neighbor <- tract_data$asunum[tract_data$row_num == n]
        # Only record edges if neighbor is assigned and from a different ASU.
        if (!is.na(asu_neighbor) && asu_neighbor != asu_current) {
          # Record the pair (order doesn't matter)
          edge_list[[length(edge_list) + 1]] <- c(as.character(asu_current), as.character(asu_neighbor))
        }
      }
    }

    # If no edges found, nothing needs to be merged.
    if (length(edge_list) == 0) {
      message("No ASU groups are touching; no merging required.")
      return(tract_data)
    } else {
      # Convert the list of edges to a two-column matrix and remove duplicate edges.
      edges_mat <- do.call(rbind, edge_list)
      # Sort each edge so that A-B and B-A become identical.
      edges_sorted <- t(apply(edges_mat, 1, sort))
      edges_unique <- unique(edges_sorted)

      # ------------------------------------------------
      # STEP 2: Build the ASU graph and compute connected components.
      # ------------------------------------------------
      asunums <- unique(assigned$asunum)
      # Create the vertex data frame with a "name" column
      vertices_df <- data.frame(name = asunums, stringsAsFactors = FALSE)

      asu_graph <- igraph::graph_from_data_frame(
        d = as.data.frame(edges_unique, stringsAsFactors = FALSE),
        vertices = vertices_df,
        directed = FALSE
      )

      comps <- igraph::components(asu_graph)

      # Create a lookup table mapping the original asunum to the new combined asunum.
      lookup <- data.frame(
        original_asu = names(comps$membership),
        comp = comps$membership,
        stringsAsFactors = FALSE
      )
      # For each component choose the smallest original asu id as the new id.
      new_ids <- lookup %>%
        group_by(comp) %>%
        summarize(new_asu = min(original_asu)) %>%
        ungroup()

      lookup <- lookup %>% left_join(new_ids, by = "comp")

      # ------------------------------------------------
      # STEP 3: Update tract_data so that each tract gets the combined asunum.
      # ------------------------------------------------
      tract_data <- tract_data %>%
        mutate(asunum = ifelse(!is.na(asunum),
                               lookup$new_asu[match(as.character(asunum), lookup$original_asu)],
                               asunum))

      message(crayon::yellow("Combined ASU groups based on touching tracts."))
      return(tract_data)
    }
  }

  # ---- 2 · TRADE / MERGE PASSES ---------------------------------------
  asu_pass <- function(verbose = TRUE) {
    repeat {
      ## ---- 1. current state --------------------------------------
      data_merge_local <- data_merge      # it’s already in scope here
      # refresh
      tracts_not_in_asu <- data_merge_local %>%
        filter(is.na(asunum)) %>%
        arrange(-ur)

      if (nrow(tracts_not_in_asu) == 0L) {
        if (verbose) cat("\nNo more tracts to process.\n")
        break
      }

      successful_update <- FALSE

      ## ---- 2. loop over candidates ------------------------------
      for (i in seq_len(nrow(tracts_not_in_asu))) {

        target_index <- tracts_not_in_asu$row_num[i]

        # run the user‑supplied updater (works by side‑effect on global data_merge)
        ok <- update_tract_data(target_index)

        # refresh state **after** possible change
        data_merge_local <- data_merge
        tracts_in_asu    <- data_merge_local$row_num[!is.na(data_merge_local$asunum)]
        unemp_tot        <- sum(unemp_vec[tracts_in_asu])

        if (verbose) {
          update_status(
            glue::glue("Targeting: {target_index} | Remaining: {nrow(tracts_not_in_asu) - i} | Unemployed: {unemp_tot}")
          )
        }

        if (isTRUE(ok)) {        # update succeeded
          successful_update <- TRUE
          break                 # start a fresh repeat cycle
        }
      }

      ## ---- 3. nothing worked this pass --------------------------
      if (!successful_update) {
        if (verbose) cat("\nNone of the target indexes produced an update. Exiting loop.\n")
        break
      }
    }
  }

  ##### ---- 2 · INITIALIZE ASU NUMBERS -----------------------------------

  # 0. make sure asunum is character
  data_merge <- data_merge %>%
    mutate(asunum = as.character(asunum))

  # 1st pass
  asu_pass(verbose = TRUE)

  # optionally merge touching ASUs  -------------------------------
  if (isTRUE(join_touching)) {
    data_merge <- combine_asu_groups(data_merge, nb)

    # 2nd pass
    asu_pass(verbose = TRUE)
  }


  # final report
  cat(
    "\nFinal unemployment total: ",
    sum(unemp_vec[data_merge$row_num[!is.na(data_merge$asunum)]], na.rm = TRUE),
    "\n"
  )


  # ---- 3 · RETURN OBJECTS --------------------------------------------
  data_merge$asunum[is.na(data_merge$asunum)] <- 0

  full_data <- data_merge %>%
    select(-continuous) %>%
    st_as_sf() %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    mutate(ur = ur*100,
           asunum = as.integer(asunum))

  asu_tracts <- full_data %>% filter(asunum > 0)

  asu_summary <- asu_tracts %>%
    st_drop_geometry() %>%
    group_by(asunum) %>%
    summarise(
      tracts     = n(),
      population = sum(tract_pop_cur,   na.rm = TRUE),
      unemp      = sum(tract_ASU_unemp, na.rm = TRUE),
      emp        = sum(tract_ASU_emp,   na.rm = TRUE),
      ur         = round(unemp/(unemp+emp)*100, 5),
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
