#' Aggregate tract statistics for dashboard summaries
#'
#' Internal helper shared by the Modify and Review dashboard pages. It accepts
#' either an sf object or a data frame and keeps the exact aggregate rate
#' separate from the rounded display value.
#'
#' @keywords internal
asu_summarize_tracts <- function(data, by_asu = FALSE, rate_digits = 5L) {
  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)

  required <- c("tract_pop_cur", "tract_ASU_clf", "tract_ASU_unemp")
  if (isTRUE(by_asu)) required <- c("asunum", required)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Summary data missing required column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  for (column in c("tract_pop_cur", "tract_ASU_clf", "tract_ASU_unemp")) {
    data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
  }

  summarize_one <- function(rows, asunum = NULL) {
    labor_force <- sum(rows$tract_ASU_clf, na.rm = TRUE)
    unemployment <- sum(rows$tract_ASU_unemp, na.rm = TRUE)
    exact_rate <- if (labor_force > 0) unemployment / labor_force * 100 else NA_real_
    result <- data.frame(
      Tracts = nrow(rows),
      Population = sum(rows$tract_pop_cur, na.rm = TRUE),
      `Labor Force` = labor_force,
      Unemployment = unemployment,
      `Unemployment Rate Exact` = exact_rate,
      `Unemployment Rate` = round(exact_rate, as.integer(rate_digits)),
      check.names = FALSE
    )
    if (!is.null(asunum)) result <- cbind(asunum = asunum, result)
    result
  }

  if (!isTRUE(by_asu)) return(summarize_one(data))

  data$asunum <- suppressWarnings(as.integer(as.character(data$asunum)))
  data <- data[!is.na(data$asunum) & data$asunum > 0L, , drop = FALSE]
  ids <- sort(unique(data$asunum))
  if (!length(ids)) {
    empty <- summarize_one(data)
    return(cbind(asunum = integer(0), empty[FALSE, , drop = FALSE]))
  }
  do.call(rbind, lapply(ids, function(id) {
    summarize_one(data[data$asunum == id, , drop = FALSE], id)
  }))
}
