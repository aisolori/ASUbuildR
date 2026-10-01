# Run with Rscript tests/test_neighbor_cache.R.
if (file.exists("R/neighbor_cache.R")) {
  source("R/neighbor_cache.R")
} else {
  asu_cached_neighbors <- getFromNamespace("asu_cached_neighbors", "ASUbuildR")
}
run_tests <- function() {
  folder <- tempfile("neighbor-cache-test-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  tracts <- sf::st_sf(GEOID = c("a", "b", "c"), INTPTLAT = c(0, 1, 2),
    INTPTLON = c(0, 0, 0), geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(0, 1)), sf::st_point(c(0, 2)), crs = 4326))
  calls <- 0L
  build <- function() { calls <<- calls + 1L; list(2L, 1L, integer(0)) }
  get <- function(x = tracts, mode = "queen")
    asu_cached_neighbors(x, build(), mode, folder, log = function(...) NULL)
  expected <- get()
  stopifnot(calls == 1L, identical(get(), expected), calls == 1L)
  changed <- tracts
  changed$unemployment <- c(10, 20, 30)
  stopifnot(identical(get(changed), expected), calls == 1L)
  get(tracts[3:1, ]); stopifnot(calls == 2L)
  changed <- tracts
  sf::st_geometry(changed)[[1]] <- sf::st_point(c(1, 0))
  get(changed); stopifnot(calls == 3L)
  changed <- tracts; changed$INTPTLAT[1] <- 3
  get(changed); stopifnot(calls == 4L)
  get(mode = "queen-island-v1"); stopifnot(calls == 5L)
  for (path in list.files(folder, full.names = TRUE)) writeLines("broken JSON", path)
  stopifnot(identical(get(), expected), calls == 6L)
  # Valid JSON with out-of-range neighbors must also be rebuilt.
  for (path in list.files(folder, full.names = TRUE)) {
    entry <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
    if (!is.null(entry)) {
      entry$neighbors <- list(list(999), list(1), list())
      jsonlite::write_json(entry, path, auto_unbox = TRUE)
    }
  }
  stopifnot(identical(get(), expected), calls == 7L)
  stopifnot(identical(asu_cached_neighbors(tracts, build(), cache_dir = FALSE), expected), calls == 8L)
  # An unwritable cache must not block a solve (use a file as the directory).
  blocked <- tempfile(); writeLines("file", blocked)
  on.exit(unlink(blocked), add = TRUE)
  result <- suppressWarnings(asu_cached_neighbors(tracts, build(),
    cache_dir = blocked, log = function(...) NULL))
  stopifnot(identical(result, expected), calls == 9L)
  cat("Neighbor cache tests passed\n")
}
run_tests()
