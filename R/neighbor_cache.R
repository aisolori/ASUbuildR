# Persistent adjacency cache. `build` is lazy, so a hit skips spatial work.
# Increment the schema when contiguity or island-repair semantics change.
asu_cached_neighbors <- function(tracts, build, mode = "queen",
                                 cache_dir = getOption("asu.neighbor_cache_dir",
                                   rappdirs::user_cache_dir("ASUbuildR", "neighbors")),
                                 log = message) {
  normalize <- function(nb) lapply(nb, function(v) {
    if (!length(v) || (length(v) == 1L && v == 0L)) integer(0) else as.integer(v)
  })
  valid <- function(nb) {
    n <- nrow(tracts)
    is.list(nb) && length(nb) == n && all(vapply(seq_len(n), function(i) {
      v <- nb[[i]]
      is.numeric(v) && !anyNA(v) && all(is.finite(v)) &&
        all(v == floor(v) & v >= 1 & v <= n & v != i) && !anyDuplicated(v) &&
        all(vapply(v, function(j) i %in% nb[[j]], logical(1)))
    }, logical(1)))
  }
  if (identical(cache_dir, FALSE)) return(normalize(force(build)))
  key <- tryCatch({
    fingerprint <- list(schema = 1L, mode = mode,
      geoids = as.character(tracts$GEOID),
      geometry = sf::st_as_binary(sf::st_geometry(tracts)), crs = sf::st_crs(tracts),
      coordinates = lapply(c("INTPTLAT", "INTPTLON"), function(nm) tracts[[nm]]),
      sf = as.character(utils::packageVersion("sf")),
      sfdep = as.character(utils::packageVersion("sfdep")),
      spdep = as.character(utils::packageVersion("spdep")),
      engines = sf::sf_extSoftVersion(), s2 = sf::sf_use_s2())
    tmp <- tempfile("asu-neighbor-key-")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(fingerprint, tmp, compress = FALSE, version = 2)
    unname(tools::md5sum(tmp))
  }, error = function(e) NULL)
  path <- if (!is.null(key)) file.path(cache_dir, paste0(key, ".json")) else NULL
  if (!is.null(path) && file.exists(path)) {
    cached <- tryCatch({
      entry <- jsonlite::read_json(path, simplifyVector = FALSE)
      nb <- lapply(entry$neighbors, function(v) {
        if (!length(v)) integer(0) else unlist(v, use.names = FALSE)
      })
      if (!identical(entry$key, key) || !valid(nb)) stop("Invalid cache")
      normalize(nb)
    }, error = function(e) NULL)
    if (!is.null(cached)) {
      log("[NB] Reusing cached neighbors (", mode, ")")
      return(cached)
    }
  }
  log("[NB] Building neighbors (", mode, ")")
  nb <- normalize(force(build))
  if (!valid(nb)) stop("Neighbor builder returned invalid adjacency lists.")
  if (!is.null(path)) tryCatch({
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    pending <- tempfile("neighbors-", tmpdir = cache_dir, fileext = ".json")
    on.exit(unlink(pending), add = TRUE)
    jsonlite::write_json(list(key = key, neighbors = lapply(nb, as.list)),
                         pending, auto_unbox = TRUE)
    # Publish only a complete file; concurrent readers never see partial JSON.
    if (file.exists(path)) unlink(path)
    if (!file.rename(pending, path)) stop("Could not publish cache file")
  }, error = function(e) log("[NB] Could not save neighbor cache: ", conditionMessage(e)))
  nb
}
