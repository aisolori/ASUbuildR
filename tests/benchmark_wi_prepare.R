args <- commandArgs(trailingOnly = TRUE)
tracts <- readRDS(args[1])
raw <- sfdep::st_contiguity(sf::st_geometry(tracts))
neighbors <- lapply(raw, function(v) {
  if (length(v) == 1L && v == 0L) integer(0) else as.integer(v) - 1L
})
jsonlite::write_json(list(
  nb = lapply(neighbors, as.list),
  u = as.integer(round(tracts[["tract_ASU_unemp"]])),
  E = as.integer(round(tracts[["tract_ASU_emp"]])),
  P = as.integer(round(tracts[["tract_pop_cur"]])),
  lat = as.numeric(tracts[["INTPTLAT"]]),
  lon = as.numeric(tracts[["INTPTLON"]])
), args[2], auto_unbox = TRUE)
