# Chooser that wraps C++ chooser when possible, else applies grace logic in R
choose_best_neighbor_grace <- function(boundary_tracts,
                                       emp_vec, unemp_vec, pop_vec,
                                       asu_emp, asu_unemp, asu_pop,
                                       ur_thresh, asu_list, used_indexes, nb,
                                       grace, pop_max = Inf, deficit_cap = Inf) {
  # Fast path: try C++ chooser first (it only returns moves with UR >= \u03c4)
  res <- try(
    choose_best_neighbor(boundary_tracts, emp_vec, unemp_vec, pop_vec,
                         asu_emp, asu_unemp, asu_pop, ur_thresh),
    silent = TRUE
  )
  if (!inherits(res, "try-error") &&
      !is.na(res$best_index) &&
      res$best_pop <= pop_max) {
    # if we were in a grace episode, update deficit/steps using the chosen idx
    if (isTRUE(grace$active)) {
      k <- grace$k
      grace$deficit    <- update_deficit(grace$deficit,
                                         unemp_vec[res$best_index],
                                         emp_vec[res$best_index], k)
      grace$steps_left <- max(0L, grace$steps_left - 1L)
      if (grace$deficit == 0) { grace$active <- FALSE; grace$steps_left <- 0L }
    }
    return(c(as.list(res), list(grace = grace, used_grace = FALSE)))
  }

  # Grace path: allow a temporary UR dip, but only if clearable
  r <- pick_with_grace(boundary_tracts, emp_vec, unemp_vec, pop_vec,
                       asu_emp, asu_unemp, asu_pop, asu_list, used_indexes,
                       nb, grace, ur_thresh, pop_max, deficit_cap)
  if (isTRUE(r$accepted)) {
    return(list(
      best_index = r$idx, best_emp = r$emp, best_unemp = r$unemp,
      best_pop = r$pop, best_ur = r$ur, grace = r$grace, used_grace = TRUE
    ))
  }

  list(best_index = NA_integer_, grace = grace, used_grace = FALSE)
}

