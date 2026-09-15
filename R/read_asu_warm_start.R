# Internal dashboard helper: read data only, align by tract ID, then validate
# against the current solver inputs. Saved geometry/economic columns are ignored.
asu_read_warm_start <- function(path, geoids, data = NULL, neighbors = NULL,
                               tau = NULL, pop_thresh = NULL, max_asus = NULL,
                               max_nodes = NULL) {
  saved <- tryCatch(readRDS(path), error = function(e) {
    stop("Cannot read warm-start RDS: ", conditionMessage(e), call. = FALSE)
  })
  if (!is.data.frame(saved) || anyDuplicated(names(saved))) {
    stop("Warm-start RDS must contain an sf object or data frame with unique column names.", call. = FALSE)
  }
  geoid_columns <- intersect(c("GEOID", "geoid"), names(saved))
  asu_columns <- intersect(c("asunum", "asu_id"), names(saved))
  if (!length(geoid_columns) || !length(asu_columns)) {
    stop("Warm-start RDS needs GEOID (or geoid) and asunum (or asu_id) columns.", call. = FALSE)
  }
  normalize_geoids <- function(values) {
    if (is.factor(values)) values <- as.character(values)
    if (is.numeric(values)) {
      if (any(!is.finite(values) | values < 0 | values > 99999999999 | values != floor(values))) {
        stop("Warm-start matching requires whole, nonmissing Census tract GEOIDs.", call. = FALSE)
      }
      values <- sprintf("%011.0f", values)
    }
    if (!is.character(values) || anyNA(values)) stop("GEOIDs must be nonmissing tract IDs.", call. = FALSE)
    values <- trimws(values)
    if (any(!grepl("^[0-9]{1,11}$", values))) stop("GEOIDs must contain at most 11 digits.", call. = FALSE)
    paste0(strrep("0", 11L - nchar(values)), values)
  }
  normalize_labels <- function(values) {
    if (is.factor(values)) values <- as.character(values)
    if (!is.numeric(values) && !is.character(values)) stop("ASU IDs must be integers.", call. = FALSE)
    parsed <- suppressWarnings(as.numeric(values))
    parsed[is.na(values)] <- 0
    if (any(!is.finite(parsed) | parsed < -1 | parsed > .Machine$integer.max | parsed != floor(parsed))) {
      stop("ASU IDs must be positive integers; 0, -1, or NA mean unassigned.", call. = FALSE)
    }
    as.integer(ifelse(parsed > 0, parsed, -1))
  }
  saved_geoids <- normalize_geoids(saved[[geoid_columns[1]]])
  current_geoids <- normalize_geoids(geoids)
  labels <- normalize_labels(saved[[asu_columns[1]]])
  if (length(geoid_columns) == 2L && !identical(saved_geoids, normalize_geoids(saved[[geoid_columns[2]]]))) {
    stop("GEOID and geoid columns disagree.", call. = FALSE)
  }
  if (length(asu_columns) == 2L && !identical(labels, normalize_labels(saved[[asu_columns[2]]]))) {
    stop("asunum and asu_id columns disagree.", call. = FALSE)
  }
  if (anyDuplicated(saved_geoids) || anyDuplicated(current_geoids)) {
    stop("Duplicate tract GEOIDs prevent unambiguous warm-start matching.", call. = FALSE)
  }
  unmatched <- !(saved_geoids %in% current_geoids)
  missing_assigned <- saved_geoids[unmatched & labels > 0]
  if (length(missing_assigned)) {
    stop(sprintf("%d assigned warm-start tract(s) are missing from the current data: %s. Check states and tract vintage.",
                 length(missing_assigned), paste(head(missing_assigned, 5), collapse = ", ")), call. = FALSE)
  }
  matching <- match(current_geoids, saved_geoids)
  aligned <- rep(-1L, length(current_geoids))
  present <- !is.na(matching)
  aligned[present] <- labels[matching[present]]
  original_ids <- sort(unique(aligned[aligned > 0]))
  if (!length(original_ids)) stop("Warm-start RDS has no assigned ASUs matching the current data.", call. = FALSE)
  if (!is.null(max_asus) && length(original_ids) > max_asus) {
    stop(sprintf("Warm start has %d ASUs but Max ASUs is %d. Increase Max ASUs or choose another file.",
                 length(original_ids), max_asus), call. = FALSE)
  }
  baseline <- NULL
  if (!is.null(data)) {
    if (nrow(data) != length(aligned) || length(neighbors) != length(aligned) ||
        is.null(tau) || is.null(pop_thresh)) stop("Incomplete current data for warm-start validation.", call. = FALSE)
    required <- c("tract_ASU_unemp", "tract_ASU_emp", "tract_pop_cur")
    if (!all(required %in% names(data))) stop("Current data lacks population or employment columns.", call. = FALSE)
    u <- round(data[[required[1]]]); emp <- round(data[[required[2]]]); pop <- round(data[[required[3]]])
    threshold <- round(tau * 10000)
    for (id in original_ids) {
      unit <- which(aligned == id)
      reason <- character()
      if (any(!is.finite(c(u[unit], emp[unit], pop[unit])))) {
        reason <- "missing current population/employment data"
      } else {
        if (sum(pop[unit]) < as.integer(pop_thresh)) reason <- c(reason, "population threshold")
        if ((10000 - threshold) * sum(u[unit]) - threshold * sum(emp[unit]) < 0 ||
            (tau > 0 && sum(u[unit] + emp[unit]) <= 0)) reason <- c(reason, "unemployment-rate threshold")
      }
      if (!is.null(max_nodes) && is.finite(max_nodes) && length(unit) > max_nodes) reason <- c(reason, "tract cap")
      seen <- rep(FALSE, length(aligned)); seen[unit[1]] <- TRUE
      queue <- unit[1]; cursor <- 1L
      while (cursor <= length(queue)) {
        adjacent <- as.integer(unlist(neighbors[[queue[cursor]]], use.names = FALSE)) + 1L
        adjacent <- adjacent[adjacent %in% unit]
        fresh <- unique(adjacent[!seen[adjacent]])
        seen[fresh] <- TRUE; queue <- c(queue, fresh); cursor <- cursor + 1L
      }
      if (!all(seen[unit])) reason <- c(reason, "connectivity on the current neighbor graph")
      if (length(reason)) stop(sprintf("Warm-start ASU %d fails: %s. No groups were imported.",
                                      id, paste(reason, collapse = ", ")), call. = FALSE)
    }
    baseline <- sum(u[aligned > 0])
  }
  positive <- aligned > 0
  aligned[positive] <- match(aligned[positive], original_ids)
  list(ids = aligned, asu_count = length(original_ids), assigned_tracts = sum(positive),
       matched_tracts = sum(present), ignored_unassigned = sum(unmatched), baseline_unemp = baseline,
       label_map = data.frame(original_id = original_ids, asu_id = seq_along(original_ids)))
}
