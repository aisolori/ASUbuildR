#' Build ASUs with a greedy algorithm maximizing unemployment
#'
#' This algorithm repeatedly seeds new ASU regions with the tract
#' that has the highest number of unemployed individuals and then
#' expands by attaching adjacent tracts while the combined unemployment
#' rate remains above the required threshold.  Only regions with at
#' least `pop_thresh` population are kept.  Each tract is assigned to at
#' most one ASU.
#'
#' @param tract_sf sf object from `tigris::tracts()` already containing
#'   a `row_num` column and a `continuous` list-column produced by
#'   `sfdep::st_contiguity()`.
#' @param bls_df Data frame read from the BLS Excel file
#' @param ur_thresh Minimum unemployment rate (as a proportion).
#'   Default is `0.0645` (which rounds up to 6.5%).
#' @param pop_thresh Minimum population for a valid ASU. Default `10000`.
#' @return A named list with the objects expected by the dashboard.
#' @export
run_asu_greedy_unemp <- function(tract_sf, bls_df,
                                 ur_thresh = 0.0645,
                                 pop_thresh = 10000) {

  data_merge <- dplyr::left_join(tract_sf, bls_df, by = "GEOID") %>%
    dplyr::mutate(
      row_num = ifelse(!is.na(row_num), row_num, dplyr::row_number()),
      ur      = ifelse(tract_ASU_emp + tract_ASU_unemp == 0, 0,
                       tract_ASU_unemp / (tract_ASU_emp + tract_ASU_unemp))
    )

  nb <- data_merge$continuous
  emp_vec   <- data_merge$tract_ASU_emp
  unemp_vec <- data_merge$tract_ASU_unemp
  pop_vec   <- data_merge$tract_pop_cur
  ur_vec    <- data_merge$ur

  used <- rep(FALSE, nrow(data_merge))
  asu_sf <- data_merge[0, ]
  asu_num <- 1L

  repeat {
    candidates <- which(!used & ur_vec >= ur_thresh)
    if (length(candidates) == 0) break

    start <- candidates[which.max(unemp_vec[candidates])]

    cluster <- c(start)
    used[start] <- TRUE
    asu_emp   <- emp_vec[start]
    asu_unemp <- unemp_vec[start]
    asu_pop   <- pop_vec[start]

    repeat {
      boundary <- unique(unlist(nb[cluster]))
      boundary <- setdiff(boundary, cluster)
      boundary <- boundary[!used[boundary]]
      if (length(boundary) == 0) break

      cand_emp   <- emp_vec[boundary]
      cand_unemp <- unemp_vec[boundary]
      new_emp   <- asu_emp + cand_emp
      new_unemp <- asu_unemp + cand_unemp
      new_rate  <- new_unemp / (new_emp + new_unemp)

      valid <- which(new_rate >= ur_thresh)
      if (length(valid) == 0) break

      best <- boundary[valid[which.max(cand_unemp[valid])]]

      cluster   <- c(cluster, best)
      used[best] <- TRUE
      asu_emp   <- asu_emp + emp_vec[best]
      asu_unemp <- asu_unemp + unemp_vec[best]
      asu_pop   <- asu_pop + pop_vec[best]
    }

    final_rate <- asu_unemp / (asu_unemp + asu_emp)
    if (asu_pop >= pop_thresh && final_rate >= ur_thresh) {
      asu_sf <- dplyr::bind_rows(
        asu_sf,
        dplyr::mutate(data_merge[cluster, ], asunum = asu_num)
      )
      asu_num <- asu_num + 1L
    }
  }

  reshape_for_dashboard(all_tr = data_merge, asu_sf = asu_sf)
}


# ------------------------------ helpers ------------------------------
# Convert algorithm output to the shape the dashboard wants
reshape_for_dashboard <- function(all_tr, asu_sf) {
  full_tbl <- all_tr %>%
    dplyr::select(where(~ !is.list(.))) %>%
    dplyr::left_join(
      asu_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::select(GEOID, asunum),
      by = "GEOID"
    ) %>%
    dplyr::mutate(
      asunum = dplyr::coalesce(as.integer(asunum), 0L),
      dplyr::across(c(tract_ASU_clf, tract_pop_cur, tract_ASU_unemp), as.integer)
    )

  if (!inherits(full_tbl, "sf")) {
    full_tbl <- sf::st_as_sf(full_tbl)
  }

  asu_tbl <- asu_sf %>%
    dplyr::select(GEOID, name, tract_pop_cur,
                  tract_ASU_clf, tract_ASU_unemp, tract_ASU_urate, asunum)

  summary_tbl <- asu_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(asunum) %>%
    dplyr::summarise(
      tracts     = dplyr::n(),
      population = sum(tract_pop_cur,   na.rm = TRUE),
      lf         = sum(tract_ASU_clf,   na.rm = TRUE),
      unemp      = sum(tract_ASU_unemp, na.rm = TRUE),
      ur         = round(unemp / lf * 100, 2),
      .groups    = "drop"
    )

  list(
    full_data       = full_tbl,
    full_data_reset = full_tbl,
    asu_data        = asu_sf,
    asu_tracts      = asu_tbl,
    asu_summary     = summary_tbl
  )
}
