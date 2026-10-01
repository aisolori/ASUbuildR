# Match geography to the uploaded GEOIDs before building any adjacency indices.
# Population year and geography vintage are independent.
asu_load_tract_geography <- function(data, year, new_england_2021 = FALSE,
                                     fetch = function(state, year)
                                       tigris::tracts(state = state, year = year, progress_bar = FALSE),
                                     log = message) {
  ids <- as.character(data$GEOID)
  if (!length(ids) || anyNA(ids) || any(!grepl("^[0-9]{11}$", ids)) || anyDuplicated(ids))
    stop("Uploaded tract GEOIDs must be unique, nonmissing 11-digit identifiers.", call. = FALSE)
  states <- unique(substr(ids, 1, 2))
  parts <- lapply(states, function(st) {
    wanted <- ids[substr(ids, 1, 2) == st]
    vintage <- as.integer(year)
    if (isTRUE(new_england_2021) && st %in% c("09", "23", "25", "33", "44", "50"))
      vintage <- 2021L
    # Connecticut changed county-equivalent GEOIDs in TIGER 2022. Use the
    # original counties' geometry when the workbook uses those identifiers;
    # never replace identifiers or redistribute population between tracts.
    old_ct <- st == "09" && all(substr(wanted, 3, 5) %in% sprintf("%03d", seq(1, 15, 2)))
    if (old_ct && vintage >= 2022L) {
      log("[geography] Connecticut workbook uses legacy county GEOIDs; using TIGER 2021 for Connecticut only.")
      vintage <- 2021L
    }
    shapes <- fetch(st, vintage)
    shape_ids <- as.character(shapes$GEOID)
    if (!inherits(shapes, "sf") || !length(shape_ids) || anyNA(shape_ids) || anyDuplicated(shape_ids))
      stop(sprintf("Invalid or duplicate geometry GEOIDs for state %s, TIGER %d.", st, vintage), call. = FALSE)
    missing <- setdiff(wanted, shape_ids)
    if (length(missing))
      stop(sprintf(paste0("State %s: %d uploaded tracts have no matching TIGER %d geometry (examples: %s). ",
                          "Select the geography vintage matching the workbook. No uploaded tracts were discarded."),
                   st, length(missing), vintage, paste(head(missing, 5), collapse = ", ")), call. = FALSE)
    # Only tracts represented in the workbook belong to this solver graph.
    keep <- shape_ids %in% wanted
    log(sprintf("[geography] state=%s year=%d matched=%d unused_shapes=%d", st, vintage, sum(keep), sum(!keep)))
    shapes[keep, , drop = FALSE]
  })
  result <- dplyr::bind_rows(parts)
  stopifnot(nrow(result) == length(ids), setequal(result$GEOID, ids))
  result
}

asu_join_tract_data <- function(shapes, data) {
  if (anyDuplicated(shapes$GEOID) || anyDuplicated(data$GEOID) ||
      nrow(shapes) != nrow(data) || !setequal(shapes$GEOID, data$GEOID))
    stop("Tract geometry and uploaded data must match one-to-one before solving.", call. = FALSE)
  result <- dplyr::left_join(shapes, data, by = "GEOID")
  counts <- c("tract_ASU_unemp", "tract_ASU_emp", "tract_pop_cur", "tract_ASU_clf")
  for (name in counts) {
    values <- result[[name]]
    if (is.null(values) || !is.numeric(values) || any(!is.finite(values) | values < 0))
      stop(sprintf("Invalid or missing %s after matching tract geometry. Solver was not started.", name), call. = FALSE)
  }
  result
}
