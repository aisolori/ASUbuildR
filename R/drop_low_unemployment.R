#' Identify cut vertices within ASU groups
#'
#' Computes articulation points for each ASU based on tract contiguity.
#'
#' @param asu_sf An sf object containing tracts with an `asunum` column.
#' @return A character vector of GEOID values that are cut vertices.
#' @export
asu_cut_vertices <- function(asu_sf) {
  cuts <- character(0)
  asu_ids <- unique(asu_sf$asunum)
  for (asu in asu_ids) {
    if (is.na(asu) || asu == 0) next
    group <- asu_sf[asu_sf$asunum == asu, ]
    if (nrow(group) < 2) next
    nb <- sfdep::st_contiguity(group$geometry)
    g  <- igraph::graph_from_adj_list(nb, mode = "undirected")
    ap <- igraph::articulation_points(g)
    if (length(ap) > 0) {
      cuts <- c(cuts, group$GEOID[as.integer(ap)])
    }
  }
  cuts
}

#' Drop low-unemployment tracts by percentile
#'
#' Removes tracts with the lowest unemployment metrics from ASUs while
#' optionally preserving contiguity.
#'
#' @param full_sf sf object of all tracts with an `asunum` column.
#' @param percentile Numeric value between 0 and 100 specifying the bottom
#'   percentile of tracts to drop.
#' @param metric Character string indicating which unemployment metric to use:
#'   "rate" for unemployment rate or "unemp" for unemployment level.
#' @param allow_breaks Logical; if `TRUE`, drop tracts even if doing so
#'   disconnects existing ASU groups.
#'
#' @return A list with updated `full_data`, `asu_data`, `asu_tracts`,
#'   `asu_summary`, and the GEOIDs of `dropped` tracts.
#' @export
drop_lowest_percentile <- function(full_sf, percentile = 5,
                                   metric = c("rate", "unemp"),
                                   allow_breaks = FALSE) {
  metric <- match.arg(metric)
  var <- if (metric == "rate") "tract_ASU_urate" else "tract_ASU_unemp"

  asu_sf <- dplyr::filter(full_sf, asunum > 0)
  if (nrow(asu_sf) == 0) {
    return(list(full_data = full_sf, asu_data = asu_sf,
                asu_tracts = sf::st_drop_geometry(asu_sf),
                asu_summary = dplyr::tibble(), dropped = character(0)))
  }

  thresh <- stats::quantile(asu_sf[[var]], probs = percentile / 100,
                            na.rm = TRUE)
  candidates <- asu_sf$GEOID[asu_sf[[var]] <= thresh]

  if (allow_breaks) {
    to_drop <- candidates
  } else {
    cut_ids <- asu_cut_vertices(asu_sf)
    to_drop <- setdiff(candidates, cut_ids)
  }

  if (length(to_drop) == 0) {
    return(list(full_data = full_sf, asu_data = asu_sf,
                asu_tracts = sf::st_drop_geometry(asu_sf),
                asu_summary = dplyr::tibble(), dropped = character(0)))
  }

  full_sf$asunum[full_sf$GEOID %in% to_drop] <- 0L

  asu_data <- dplyr::filter(full_sf, asunum > 0)
  asu_tracts <- asu_data |>
    sf::st_drop_geometry() |>
    dplyr::select(GEOID, name, tract_pop_cur, tract_ASU_clf,
                  tract_ASU_unemp, tract_ASU_urate, asunum)
  asu_summary <- asu_data |>
    sf::st_drop_geometry() |>
    dplyr::group_by(asunum) |>
    dplyr::summarise(
      tracts     = dplyr::n(),
      population = sum(tract_pop_cur,   na.rm = TRUE),
      lf         = sum(tract_ASU_clf,   na.rm = TRUE),
      unemp      = sum(tract_ASU_unemp, na.rm = TRUE),
      ur         = round(unemp / lf * 100, 2),
      .groups    = "drop"
    )

  list(full_data  = full_sf,
       asu_data   = asu_data,
       asu_tracts = asu_tracts,
       asu_summary = asu_summary,
       dropped    = to_drop)
}

