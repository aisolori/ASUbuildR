# ------------------------------
# run_asu_original.R
# ------------------------------
# Your colleague’s original contiguity‑driven algorithm, wrapped in a single
# function so the flex‑dashboard (or any other caller) can invoke it.
#
# It returns the four objects that the dashboard expects inside a list:
#   * full_data       – all tracts (sf) with asunum (0 for unassigned)
#   * full_data_reset – identical copy for the Reset button
#   * asu_data        – sf of just the tracts that joined an ASU (keeps geometry)
#   * asu_tracts      – same rows but without the heavy list‑columns
#   * asu_summary     – one summary row per ASU (tracts, population, ur)
#
# No side‑effects, no <<‑; everything stays local.

#' Build ASUs with the original algorithm
#' @param tract_sf  sf object from `tigris::tracts()` already containing a
#'        `row_num` column and a `continuous` list‑column produced by
#'        `sfdep::st_contiguity()`.
#' @param bls_df    Data frame read from the BLS Excel file (column names must
#'        match the `excel_names` vector in the dashboard).
#' @return A named list (see details above).
#' @export
run_asu_original <- function(tract_sf, bls_df) {

  # ------------------------------------------------------------------
  # 0. merge shapefile & BLS data, then sort by unemployment
  # ------------------------------------------------------------------
  data_merge <- dplyr::left_join(tract_sf, bls_df, by = "GEOID") %>%
    dplyr::arrange(dplyr::desc(tract_ASU_urate), dplyr::desc(tract_ASU_unemp))

  # objects we build along the way
  ASU_assigned        <- data_merge[0, ]   # empty clone with same cols
  filtered_data_merge <- data_merge
  asunum              <- 1L

  # ------------------------------------------------------------------
  # 1. keep creating ASUs until the highest‑UR unassigned tract < 6.45 %
  # ------------------------------------------------------------------
  while (filtered_data_merge$tract_ASU_urate[1] > 6.45) {

    orderid       <- 1L
    combined_area <- dplyr::slice(filtered_data_merge, 1)
    combined_area$orderid <- orderid
    existing      <- combined_area$row_num
    rate          <- 100L   # sentinel to enter the inner loop

    # ------------------------------ inner expansion loop -------------
    while (rate > 6.45) {
      neighbor_list <- combined_area %>%
        sf::st_drop_geometry() %>%
        dplyr::pull(continuous) %>% unlist() %>%
        setdiff(existing)

      adjacent <- filtered_data_merge %>% dplyr::filter(row_num %in% neighbor_list)
      if (nrow(adjacent) == 0) break

      if (min(adjacent$tract_ASU_clf) < 1) {
        next_tract <- adjacent %>% dplyr::filter(tract_ASU_clf < 1) %>% dplyr::slice(1)
      } else {
        next_tract <- adjacent %>%
          dplyr::arrange(dplyr::desc(tract_ASU_urate), dplyr::desc(tract_ASU_unemp)) %>%
          dplyr::slice(1)
      }

      orderid <- orderid + 1L
      next_tract$orderid <- orderid
      combined_area <- dplyr::bind_rows(combined_area, next_tract)

      rate <- sum(combined_area$tract_ASU_unemp) /
              sum(combined_area$tract_ASU_clf) * 100
      existing <- combined_area$row_num
    }

    # remove last tract if it dragged the ASU below the threshold
    if (rate < 6.45) combined_area <- utils::head(combined_area, -1L)

    ASU_assigned <- dplyr::bind_rows(
      ASU_assigned,
      dplyr::mutate(combined_area, asunum = asunum)
    )

    asunum              <- asunum + 1L
    assigned_names      <- ASU_assigned$name
    filtered_data_merge <- data_merge %>%
      dplyr::filter(!name %in% assigned_names) %>%
      dplyr::arrange(dplyr::desc(tract_ASU_urate))
  }

  # ------------------------------------------------------------------
  # 2. reshape for the dashboard
  # ------------------------------------------------------------------
  reshape_for_dashboard(all_tr = data_merge, asu_sf = ASU_assigned)
}

# ------------------------------ helpers ------------------------------
# Convert algorithm output to the shape the dashboard wants
# R/reshape.R
#' Reshape algorithm output for the dashboard
#'
#' @param all_tr sf object of all tracts
#' @param asu_sf sf object of ASU tracts
#' @return A named list used by the dashboard
#' @export
reshape_for_dashboard <- function(all_tr, asu_sf) {
  # Drop list-columns and preserve sf structure
  full_tbl <- all_tr %>%
    # Clean join: drop list-columns like `continuous` before join
    dplyr::select(where(~ !is.list(.))) %>%
    left_join(
      asu_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::select(GEOID, asunum),
      by = "GEOID"
    ) %>%
    dplyr::mutate(
      asunum = dplyr::coalesce(as.integer(asunum), 0L),
      dplyr::across(c(tract_ASU_clf, tract_pop_cur, tract_ASU_unemp), as.integer)
    )

  # If geometry dropped (e.g. due to inner joins etc), reclass as sf
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


