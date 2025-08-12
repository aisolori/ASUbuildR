# ---- UR grace math ------------------------------------------------------
ur_deficit <- function(U, E, k) max(0, k * (U + E) - U)
update_deficit <- function(D, ui, ei, k) max(0, D + k * ei - ui)

# Sum of top-T positive (u - k e) among scores
clearable <- function(scores, T) {
  if (T <= 0 || length(scores) == 0) return(0)
  pos <- sort(pmax(scores, 0), decreasing = TRUE)
  sum(head(pos, T))
}

init_ur_grace <- function(tau, W = 50L) {
  list(active = FALSE, W = as.integer(W), steps_left = 0L, deficit = 0.0,
       k = tau / (1 - tau))
}

# Lightweight frontier recompute given the current ASU set
frontier_after_add <- function(nb, selected_idx, used_idx) {
  f <- unique(unlist(nb[selected_idx]))
  setdiff(f, c(selected_idx, used_idx))
}

# Try a candidate with grace; returns either an accepted move + updated totals
pick_with_grace <- function(boundary_tracts, emp_vec, unemp_vec, pop_vec,
                            asu_emp, asu_unemp, asu_pop, asu_list, used_indexes,
                            nb, grace, ur_thresh, pop_max = Inf,
                            deficit_cap = Inf) {
  k <- grace$k
  U <- asu_emp; E <- asu_unemp
  D <- if (grace$active) grace$deficit else ur_deficit(U, E, k)

  best_idx <- NA_integer_
  best_emp <- best_unemp <- best_pop <- best_ur <- NA_real_

  # 1) UR-improving first: scores = u - k e > 0
  scores_all <- unemp_vec[boundary_tracts] - k * emp_vec[boundary_tracts]
  pos <- which(scores_all > 0)
  if (length(pos) > 0) {
    j <- pos[which.max(scores_all[pos])]
    idx <- boundary_tracts[j]
    # apply add
    total_emp   <- asu_emp   + emp_vec[idx]
    total_unemp <- asu_unemp + unemp_vec[idx]
    total_pop   <- asu_pop   + pop_vec[idx]
    total_ur    <- ifelse(total_emp + total_unemp == 0, 0,
                          total_unemp / (total_emp + total_unemp))
    # update grace state if active
    if (grace$active) {
      D_new <- update_deficit(D, unemp_vec[idx], emp_vec[idx], k)
      grace$deficit    <- D_new
      grace$steps_left <- max(0L, grace$steps_left - 1L)
      if (D_new == 0) { grace$active <- FALSE; grace$steps_left <- 0L }
    }
    return(list(
      accepted=TRUE, idx=idx, emp=total_emp, unemp=total_unemp,
      pop=total_pop, ur=total_ur, grace=grace
    ))
  }

  # 2) Otherwise, consider “least-bad” negatives but only if clearable
  ord <- order(scores_all, decreasing = TRUE) # closest to 0 first
  for (j in ord) {
    idx <- boundary_tracts[j]
    total_emp   <- asu_emp   + emp_vec[idx]
    total_unemp <- asu_unemp + unemp_vec[idx]
    total_pop   <- asu_pop   + pop_vec[idx]
    if (total_pop > pop_max) next

    D_new <- update_deficit(D, unemp_vec[idx], emp_vec[idx], k)
    if (D_new == 0) {
      # actually didn’t dip — accept
      total_ur <- ifelse(total_emp + total_unemp == 0, 0,
                         total_unemp / (total_emp + total_unemp))
      grace$active <- FALSE; grace$steps_left <- 0L; grace$deficit <- 0.0
      return(list(accepted=TRUE, idx=idx, emp=total_emp, unemp=total_unemp,
                  pop=total_pop, ur=total_ur, grace=grace))
    }

    if (is.finite(deficit_cap) && D_new > deficit_cap) next

    # start grace if needed
    if (!grace$active) {
      grace$active     <- TRUE
      grace$steps_left <- grace$W
      grace$deficit    <- D
    }

    steps_remain <- max(0L, grace$steps_left - 1L)

    # optimistic clearability check: after taking idx, frontier has enough u-k e?
    st2_selected <- c(asu_list, idx)
    pool2 <- frontier_after_add(nb, st2_selected, used_indexes)
    s2 <- unemp_vec[pool2] - k * emp_vec[pool2]
    if (clearable(s2, steps_remain) >= D_new) {
      total_ur <- ifelse(total_emp + total_unemp == 0, 0,
                         total_unemp / (total_emp + total_unemp))
      grace$deficit    <- D_new
      grace$steps_left <- steps_remain
      return(list(accepted=TRUE, idx=idx, emp=total_emp, unemp=total_unemp,
                  pop=total_pop, ur=total_ur, grace=grace))
    }
  }

  list(accepted=FALSE)
}

force_catch_up <- function(asu_list, used_indexes, nb, emp_vec, unemp_vec,
                           pop_vec, asu_emp, asu_unemp, asu_pop, grace,
                           pop_max = Inf) {
  k <- grace$k
  repeat {
    pool <- frontier_after_add(nb, asu_list, used_indexes)
    if (length(pool) == 0) break
    scores <- unemp_vec[pool] - k * emp_vec[pool]
    j <- which.max(scores)
    if (length(j) == 0 || scores[j] <= 0) break
    idx <- pool[j]
    if (asu_pop + pop_vec[idx] > pop_max) break
    asu_list   <- c(asu_list, idx)
    asu_emp    <- asu_emp    + emp_vec[idx]
    asu_unemp  <- asu_unemp  + unemp_vec[idx]
    asu_pop    <- asu_pop    + pop_vec[idx]
    grace$deficit <- update_deficit(grace$deficit, unemp_vec[idx], emp_vec[idx], k)
    if (grace$deficit == 0) { grace$active <- FALSE; grace$steps_left <- 0L; break }
    grace$steps_left <- max(0L, grace$steps_left - 1L)
  }
  list(asu_list=asu_list, asu_emp=asu_emp, asu_unemp=asu_unemp,
       asu_pop=asu_pop, grace_ok = !grace$active, grace=grace)
}

