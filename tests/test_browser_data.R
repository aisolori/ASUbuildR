if (file.exists("R/browser_data.R")) {
  source("R/browser_data.R")
} else {
  for (nm in c("asu_state_slice", "asu_display_geometry", "asu_browser_source",
               "asu_read_upload", "asu_geojson_response", "asu_pmtiles_response", "asu_map_source", "asu_display_bounds"))
    assign(nm, getFromNamespace(nm, "ASUbuildR"))
}

run_browser_data_tests <- function() {
  # Refuse a nationwide GeoJSON fallback if PMTiles is disabled.
  options_before <- options(ASUbuildR.use_pmtiles=FALSE)
  on.exit(options(options_before),add=TRUE)
  e <- tryCatch(asu_map_source(data.frame(GEOID=c("01001000100","06001000100")),
                              NULL,1L,require_tiles=TRUE),error=identity)
  stopifnot(inherits(e,"error"),grepl("requires working PMTiles",conditionMessage(e)))

  square <- function(x) sf::st_polygon(list(matrix(c(x,0, x+0.01,0,
    x+0.01,0.01, x,0.01, x,0), ncol = 2, byrow = TRUE)))
  original <- sf::st_sf(GEOID = c("01001000100", "06001000100", "06001000200"),
    asunum = c(1L, 2L, 2L), tract_pop_cur = c(10L, 20L, 30L),
    tract_ASU_clf = c(8L, 16L, 24L), tract_ASU_unemp = c(1L, 2L, 3L),
    tract_ASU_urate = c(12.5, 12.5, 12.5), unnecessary = "must not leave server",
    geometry = sf::st_sfc(square(-87), square(-121), square(-120), crs = 4326))
  before <- serialize(original, NULL)
  california <- asu_state_slice(original, "06")
  stopifnot(nrow(california) == 2L, identical(california$tract_pop_cur, c(20L, 30L)),
            nrow(asu_state_slice(original, "01")) == 1L,
            nrow(asu_state_slice(original, "99")) == 0L)
  display <- asu_display_geometry(california[, setdiff(names(california), "unnecessary")])
  stopifnot(identical(serialize(original, NULL), before), identical(display$GEOID, california$GEOID))
  captured <- NULL
  session <- list(registerDataObj = function(name, data, filterFunc) {
    captured <<- list(name = name, data = data, response = filterFunc(data, NULL))
    paste0("session/test/dataobj/", name, "?w=&nonce=test")
  })
  source <- asu_browser_source(display, session, 1L)
  stopifnot(startsWith(source$data, "session/"), !grepl("FeatureCollection", source$data),
            captured$response$status == 200L,
            captured$response$content_type == "application/geo+json; charset=UTF-8")
  geojson <- jsonlite::fromJSON(rawToChar(captured$data), simplifyVector = FALSE)
  stopifnot(length(geojson$features) == 2L,
            all(vapply(geojson$features, function(f) startsWith(f$properties$GEOID, "06"), logical(1))),
            all(vapply(geojson$features, function(f) is.null(f$properties$unnecessary), logical(1))))
  # Every overlay references the same source, so its geometry is never repeated.
  map <- mapgl::maplibre()
  map$x$sources <- list(source)
  for (id in c("basemap", "selected", "highlighted"))
    map <- mapgl::add_fill_layer(map, id = id, source = "tracts-source")
  wire <- htmlwidgets:::toJSON(htmlwidgets:::createPayload(map))
  stopifnot(!grepl("FeatureCollection", wire), length(map$x$sources) == 1L,
            nchar(wire, type = "bytes") < 10000)
  stopifnot(identical(asu_state_slice(original,"ALL"), original))
  dateline <- sf::st_sf(GEOID=c("02001000100","02001000200"),
    geometry=sf::st_sfc(square(179),square(-170),crs=4326))
  bounds <- asu_display_bounds(dateline)
  stopifnot(bounds[3]-bounds[1]<12, bounds[1]< -180, bounds[3]<0)
  archive <- tempfile(fileext = ".pmtiles")
  on.exit(unlink(archive), add = TRUE)
  writeBin(as.raw(0:99), archive)
  range <- asu_pmtiles_response(archive, list(HTTP_RANGE = "bytes=10-19"))
  stopifnot(range$status == 206L, identical(range$content, as.raw(10:19)),
            range$headers$`Content-Range` == "bytes 10-19/100")
  range <- asu_pmtiles_response(archive, list(HTTP_RANGE = "bytes=90-999"))
  stopifnot(range$status == 206L, identical(range$content, as.raw(90:99)))
  stopifnot(asu_pmtiles_response(archive, list(HTTP_RANGE = "bytes=100-200"))$status == 416L,
            asu_pmtiles_response(archive, list(HTTP_RANGE = "bytes=0-1,4-5"))$status == 416L,
            identical(asu_pmtiles_response(archive, list(HTTP_RANGE = "bytes=-5"))$content, as.raw(95:99)))

  path <- Sys.getenv("ASU_NATIONAL_TEST_FILE")
  if (nzchar(path) && file.exists(path)) {
    result <- asu_read_upload(path)
    raw <- readxl::read_excel(path, range = readxl::cell_cols("A:AB"))
    raw <- raw[!is.na(raw$geoid), ]
    stopifnot(nrow(result$data) == nrow(raw), !anyDuplicated(result$data$GEOID))
    for (nm in c("tract_ASU_clf", "tract_ASU_emp", "tract_ASU_unemp"))
      stopifnot(identical(result$data[[nm]], as.integer(round(raw[[nm]]))))
    pop <- paste0("tract_pop", result$population_year)
    stopifnot(identical(result$data$tract_pop_cur, as.integer(round(raw[[pop]]))))
    for (state in unique(result$data$st_fips)) {
      view <- asu_state_slice(result$data, state)
      stopifnot(all(view$st_fips == state), nrow(view) == sum(result$data$st_fips == state))
    }
    cat("Actual workbook validated:", nrow(result$data), "tracts;",
        length(unique(result$data$st_fips)), "states; exact count columns preserved.\n")
  }
  cat("Browser data/source tests passed.\n")
}
run_browser_data_tests()
