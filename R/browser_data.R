# Read all populated rows; never interpret workbook cells as instructions.
asu_read_upload <- function(path) {
  df <- readxl::read_excel(path, range = readxl::cell_cols("A:AB"), col_types = "text")
  required <- c("geoid", "st_fips", "cnty_fips", "tract_fips", "name",
                "tract_ASU_clf", "tract_ASU_emp", "tract_ASU_unemp", "tract_ASU_urate")
  missing <- setdiff(required, names(df))
  pop <- grep("^tract_pop[0-9]{4}$", names(df), value = TRUE)
  if (length(missing) || length(pop) < 2L)
    stop("Expected the BLS ASU columns in A:AB, including tract population years. Missing: ",
         paste(c(missing, if (length(pop) < 2L) "tract population columns"), collapse = ", "))
  population_column <- pop[length(pop)]
  df <- df[!is.na(df$geoid) & nzchar(trimws(df$geoid)), , drop = FALSE]
  if (!nrow(df)) stop("The workbook contains no tract records.")
  df$GEOID <- sub("^14000US", "", trimws(df$geoid))
  if (any(!grepl("^[0-9]{11}$", df$GEOID))) stop("Tract GEOIDs must contain 11 digits.")
  if (anyDuplicated(df$GEOID)) stop("Duplicate tract GEOIDs were found. Supply one row per tract.")
  state <- suppressWarnings(as.integer(df$st_fips))
  if (anyNA(state) || any(!grepl("^[0-9]{1,2}$", df$st_fips)) ||
      any(sprintf("%02d", state) != substr(df$GEOID, 1L, 2L)))
    stop("State FIPS codes must match the first two digits of each tract GEOID.")
  df$st_fips <- sprintf("%02d", state)
  df$tract_pop_cur <- df[[population_column]]
  numeric_columns <- c("tract_pop_cur", "tract_ASU_clf", "tract_ASU_emp",
                       "tract_ASU_unemp", "tract_ASU_urate")
  for (nm in numeric_columns) {
    values <- suppressWarnings(as.numeric(df[[nm]]))
    # BLS zero-labor-force tracts can have a blank rate. Counts must be present.
    missing_rate <- nm == "tract_ASU_urate" & (is.na(df[[nm]]) | !nzchar(df[[nm]]))
    if (any((!is.finite(values) | values < 0) & !missing_rate))
      stop("Column ", nm, " contains missing, negative, or nonnumeric values.")
    if (nm != "tract_ASU_urate") {
      if (any(values > .Machine$integer.max)) stop("Counts in ", nm, " exceed the supported range.")
      values <- as.integer(round(values))
    }
    df[[nm]] <- values
  }
  df <- as.data.frame(df[c("GEOID", "st_fips", "cnty_fips", "tract_fips", "name", numeric_columns)])
  list(data = df, population_year = sub("tract_pop", "", population_column))
}

asu_state_slice <- function(data, state) {
  if (is.null(data)) return(NULL)
  if (length(state) != 1L || is.na(state)) return(data[FALSE, , drop = FALSE])
  if (identical(state, "ALL")) return(data)
  data[!is.na(data$GEOID) & substr(as.character(data$GEOID), 1L, 2L) == state, , drop = FALSE]
}

# Keep Alaska's Aleutian Islands and the rest of the US in the same viewport.
# This changes only camera bounds, never the displayed or solver geometry.
asu_display_bounds <- function(data) {
  data <- sf::st_transform(data, 4326)
  bounds <- as.numeric(sf::st_bbox(data))
  if (bounds[3L] - bounds[1L] > 180) {
    shifted <- as.numeric(sf::st_bbox(sf::st_shift_longitude(data)))
    if (shifted[3L] - shifted[1L] < bounds[3L] - bounds[1L]) {
      shifted[c(1L, 3L)] <- shifted[c(1L, 3L)] - 360
      bounds <- shifted
    }
  }
  bounds
}

asu_display_geometry <- function(data) {
  if (!nrow(data)) return(data)
  # A projected tolerance is in metres, unlike .005 on geographic coordinates.
  data <- sf::st_transform(sf::st_make_valid(data), 3857)
  data <- sf::st_simplify(data, dTolerance = 25, preserveTopology = TRUE)
  data <- sf::st_zm(sf::st_transform(data, 4326), what = "ZM", drop = TRUE)
  data[!sf::st_is_empty(data) & sf::st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"), ]
}

asu_payload_log <- function(id, bytes, transport = "websocket", ...) {
  line <- paste0("[payload] ", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                 " transport=", transport, " id=", id, " bytes=", bytes, ...)
  message(line)
  path <- getOption("asu.payload_log", NULL)
  if (!is.null(path)) try(cat(line, "\n", file = path, append = TRUE), silent = TRUE)
}

asu_install_payload_logging <- function(session) {
  if (!isTRUE(getOption("asu.payload_logging", TRUE))) return(invisible(FALSE))
  if (isTRUE(session$userData$asu_payload_logging)) return(invisible(TRUE))
  # Instrument the already-serialized logical message immediately before httpuv.
  # Guard this internal hook because Shiny versions may change its implementation.
  private <- session$.__enclos_env__$private
  if (is.null(private) || !is.function(private$write) || !is.function(private$sendMessage)) {
    message("[payload] Shiny wire hook unavailable; source byte logging remains enabled.")
    return(invisible(FALSE))
  }
  original_write <- private$write
  original_send <- private$sendMessage
  sequence <- 0L
  write <- function(json) {
    sequence <<- sequence + 1L
    bytes <- nchar(enc2utf8(json), type = "bytes")
    if (bytes >= getOption("asu.payload_min_bytes", 1024L))
      asu_payload_log(paste0("message-", sequence), bytes)
    original_write(json)
  }
  send <- function(...) {
    fields <- list(...)
    tryCatch({
    for (id in names(fields$values)) {
      bytes <- nchar(enc2utf8(shiny:::toJSON(fields$values[[id]], strict_atomic = FALSE)), type = "bytes")
      asu_payload_log(id, bytes, "output", paste0(" message=", sequence + 1L))
    }
    for (kind in names(fields$custom)) {
      value <- fields$custom[[kind]]
      id <- if (is.list(value) && is.character(value$id)) value$id else kind
      asu_payload_log(id, nchar(enc2utf8(shiny:::toJSON(value, strict_atomic = FALSE)), type = "bytes"),
                      "custom", paste0(" message=", sequence + 1L, " type=", kind))
    }
    }, error = function(e) message("[payload] Output metadata unavailable: ", conditionMessage(e)))
    original_send(...)
  }
  replace <- function(name, value) {
    locked <- bindingIsLocked(name, private)
    if (locked) unlockBinding(name, private)
    on.exit(if (locked) lockBinding(name, private))
    assign(name, value, envir = private)
  }
  tryCatch({
    replace("write", write)
    replace("sendMessage", send)
    session$userData$asu_payload_logging <- TRUE
    invisible(TRUE)
  }, error = function(e) {
    try(replace("write", original_write), silent = TRUE)
    try(replace("sendMessage", original_send), silent = TRUE)
    message("[payload] Wire hook unavailable: ", conditionMessage(e))
    invisible(FALSE)
  })
}

asu_geojson_response <- function(data, req) {
  shiny::httpResponse(200L, "application/geo+json; charset=UTF-8", data,
                      headers = list("Cache-Control" = "private, max-age=3600"))
}

asu_browser_source <- function(data, session, revision) {
  # Use mapgl's own conversion, then serve it through the authenticated Shiny
  # session URL. This respects /p/.../ proxies and never embeds polygons in WS.
  map <- mapgl::add_source(mapgl::maplibre(), "tracts-source", data)
  source <- map$x$sources[[1L]]
  json <- charToRaw(enc2utf8(as.character(source$data)))
  asu_payload_log("tracts-source", length(json), "http-geojson",
                  paste0(" state=", substr(data$GEOID[1L], 1L, 2L), " rows=", nrow(data)))
  source$data <- session$registerDataObj(paste0("asu-tracts-", revision), json, asu_geojson_response)
  source
}

# PMTiles uses byte-range HTTP requests. Serve them under the authenticated
# Shiny session instead of exposing a separate localhost port through HTTPS.
asu_pmtiles_response <- function(path, req) {
  size <- file.info(path)$size
  if (is.na(size)) return(shiny::httpResponse(404L, "text/plain", "Archive unavailable"))
  start <- 0
  end <- size - 1
  status <- 200L
  range <- req$HTTP_RANGE
  headers <- list("Accept-Ranges" = "bytes", "Cache-Control" = "private, max-age=3600",
                  "ETag" = paste0('"', basename(path), '"'))
  if (!is.null(range)) {
    match <- regmatches(range, regexec("^bytes=([0-9]*)-([0-9]*)$", range))[[1L]]
    if (length(match) != 3L || (!nzchar(match[2L]) && !nzchar(match[3L])))
      return(shiny::httpResponse(416L, "text/plain", "Unsupported range",
                                headers = list("Content-Range" = paste0("bytes */", size))))
    if (!nzchar(match[2L])) {
      start <- max(0, size - as.numeric(match[3L]))
    } else {
      start <- as.numeric(match[2L])
      if (nzchar(match[3L])) end <- min(end, as.numeric(match[3L]))
    }
    if (!is.finite(start) || !is.finite(end) || start > end || start >= size)
      return(shiny::httpResponse(416L, "text/plain", "Range outside archive",
                                headers = list("Content-Range" = paste0("bytes */", size))))
    status <- 206L
    headers$`Content-Range` <- sprintf("bytes %.0f-%.0f/%.0f", start, end, size)
  }
  con <- file(path, "rb")
  on.exit(close(con))
  seek(con, start, origin = "start")
  body <- readBin(con, "raw", n = end - start + 1)
  asu_payload_log("tracts-source", length(body), "http-pmtiles-range")
  shiny::httpResponse(status, "application/vnd.pmtiles", body, headers)
}

asu_map_source <- function(data, session, revision, require_tiles = FALSE) {
  states <- unique(substr(data$GEOID, 1L, 2L))
  scope <- if (length(states) > 1L) paste0(length(states), " states") else paste0("state ", states[1L])
  if (isTRUE(getOption("ASUbuildR.use_pmtiles", TRUE)) &&
      requireNamespace("freestiler", quietly = TRUE) &&
      "add_pmtiles_source" %in% getNamespaceExports("mapgl")) {
    result <- tryCatch({
      # Assignment values are dynamic paint/tooltip state, not tile content.
      tiles <- data
      tiles$asunum <- 0L
      cache <- getOption("asu.pmtiles_cache_dir", rappdirs::user_cache_dir("ASUbuildR", "pmtiles"))
      dir.create(cache, recursive = TRUE, showWarnings = FALSE)
      fingerprint <- tempfile()
      on.exit(unlink(fingerprint), add = TRUE)
      saveRDS(list(schema = 3L, data = tiles,
                   freestiler = as.character(utils::packageVersion("freestiler"))),
              fingerprint, compress = FALSE, version = 2)
      key <- unname(tools::md5sum(fingerprint))
      path <- file.path(cache, paste0("tracts_v3_", key, ".pmtiles"))
      if (!file.exists(path)) {
        pending <- tempfile("tracts-", tmpdir = cache, fileext = ".pmtiles")
        on.exit(unlink(pending), add = TRUE)
        freestiler::freestile(tiles, pending, layer_name = "tracts", tile_format = "mvt",
                             min_zoom = 0L, max_zoom = 14L, simplification = TRUE,
                             overwrite = TRUE, quiet = TRUE)
        if (!file.rename(pending, path) && !file.exists(path)) stop("Could not cache PMTiles archive")
      }
      message("[map] Using PMTiles for ", scope,
              ": ", file.info(path)$size, " bytes on disk")
      url <- session$registerDataObj(paste0("asu-tracts-", key), path, asu_pmtiles_response)
      map <- mapgl::add_pmtiles_source(mapgl::maplibre(), "tracts-source", url, promote_id = "GEOID")
      list(source = map$x$sources[[1L]], source_layer = "tracts")
    }, error = function(e) {
      message("[map] PMTiles unavailable: ", conditionMessage(e))
      NULL
    })
    if (!is.null(result)) return(result)
  }
  if (isTRUE(require_tiles))
    stop("All states display requires working PMTiles support (freestiler and mapgl). Select a single state while PMTiles is unavailable.", call. = FALSE)
  list(source = asu_browser_source(data, session, revision), source_layer = NULL)
}
