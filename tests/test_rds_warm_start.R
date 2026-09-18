# Base-R tests; run directly from the checkout or under R CMD check.
if (file.exists("R/read_asu_warm_start.R")) {
  source("R/read_asu_warm_start.R")
} else {
  asu_read_warm_start <- getFromNamespace("asu_read_warm_start", "ASUbuildR")
}

run_warm_start_tests <- function() {
  folder <- tempfile("asu-warm-start-tests-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "input.rds")
  fixture <- function(x) { saveRDS(x, file); file }
  expect_error <- function(expr, text) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(text, conditionMessage(error), fixed = TRUE))
  }
  a <- "01001000100"; b <- "01001000200"; c <- "01001000300"; d <- "01001000400"
  saved <- data.frame(GEOID = c(a, b, c), asunum = c(4L, 0L, 9L))
  result <- asu_read_warm_start(fixture(saved), c(c, a, b, d), max_asus = 2)
  stopifnot(identical(result$ids, c(2L, 1L, -1L, -1L)), result$assigned_tracts == 2,
            identical(result$label_map$original_id, c(4L, 9L)))
  # Numeric GEOIDs and leading zeros are normalized, never matched by row.
  result <- asu_read_warm_start(fixture(data.frame(geoid = as.numeric(c(a, b)), asu_id = c(2, NA))), c(b, a))
  stopifnot(identical(result$ids, c(-1L, 1L)))
  # An out-of-scope unassigned row is harmless, but assigned tracts cannot vanish.
  result <- asu_read_warm_start(fixture(saved), c(a, c))
  stopifnot(result$ignored_unassigned == 1)
  expect_error(asu_read_warm_start(file, a), "assigned warm-start tract(s) are missing")
  expect_error(asu_read_warm_start(file, c(a, b, c), max_asus = 1), "Max ASUs")
  expect_error(asu_read_warm_start(fixture(rbind(saved, saved[1, ])), c(a, b, c)), "Duplicate")
  expect_error(asu_read_warm_start(fixture(saved), c(a, a, c)), "Duplicate")
  expect_error(asu_read_warm_start(fixture(list(instruction = "not a data frame")), a), "data frame")
  expect_error(asu_read_warm_start(fixture(data.frame(GEOID = a)), a), "asunum")
  expect_error(asu_read_warm_start(fixture(data.frame(GEOID = a, asunum = 1.5)), a), "positive integers")
  expect_error(asu_read_warm_start(fixture(data.frame(GEOID = a, asunum = 0)), a), "no assigned")
  conflict <- saved; conflict$asu_id <- c(2L, 0L, 9L)
  expect_error(asu_read_warm_start(fixture(conflict), c(a, b, c)), "disagree")
  # Current data, not saved economic values, governs validation.
  assigned <- data.frame(GEOID = c(a, b), asunum = c(4L, 4L), tract_ASU_unemp = c(0, 0))
  current <- data.frame(tract_ASU_unemp = c(10, 10), tract_ASU_emp = c(0, 0), tract_pop_cur = c(6000, 6000))
  result <- asu_read_warm_start(fixture(assigned), c(a, b), current, list(1L, 0L), .2, 10000, 2)
  stopifnot(result$baseline_unemp == 20, identical(result$ids, c(1L, 1L)))
  expect_error(asu_read_warm_start(file, c(a, b), current, list(integer(), integer()), .2, 10000, 2), "connectivity")
  expect_error(asu_read_warm_start(file, c(a, b), current, list(1L, 0L), .2, 20000, 2), "population")
  current$tract_ASU_emp <- c(100, 100)
  expect_error(asu_read_warm_start(file, c(a, b), current, list(1L, 0L), .2, 10000, 2), "rate")
  cat("RDS warm-start import checks passed.\n")
}
run_warm_start_tests()
