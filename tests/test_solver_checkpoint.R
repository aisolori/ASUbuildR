source("R/solver_checkpoint.R")
source("R/read_asu_warm_start.R")
run_checkpoint_tests <- function() {
  folder <- tempfile("checkpoint-test-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  polygon <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE)))
  data <- sf::st_sf(GEOID=c("55001000100", "55001000200"), asunum=c(0L, 0L),
    tract_pop_cur=c(6000L,6000L), tract_ASU_unemp=c(20L,30L),
    tract_ASU_emp=c(80L,70L), geometry=sf::st_sfc(polygon, polygon, crs=4326))
  before <- serialize(data, NULL)
  request <- file.path(folder, "legacy_solve_1.request.json")
  jsonlite::write_json(list(asu_id=c(1L,1L)), request)
  asu_service_checkpoints(folder, data)
  target <- file.path(folder, "legacy_solve_1.rds")
  saved <- readRDS(target)
  stopifnot(identical(sf::st_geometry(saved), sf::st_geometry(data)),
    identical(saved$GEOID,data$GEOID), identical(saved$tract_pop_cur,data$tract_pop_cur),
    identical(saved$tract_ASU_unemp,data$tract_ASU_unemp),
    identical(saved$tract_ASU_emp,data$tract_ASU_emp), identical(saved$asunum,c(1L,1L)),
    identical(serialize(data,NULL), before),
    isTRUE(jsonlite::fromJSON(file.path(folder,"legacy_solve_1.ack.json"))$ok))
  warm <- asu_read_warm_start(target, rev(data$GEOID))
  stopifnot(identical(warm$ids,c(1L,1L)))
  # A repeated tick must not rewrite the RDS; a malformed request must fail closed.
  stamp <- file.info(target)$mtime
  asu_service_checkpoints(folder, data)
  stopifnot(identical(stamp,file.info(target)$mtime))
  jsonlite::write_json(list(asu_id=c(1.5,1)),file.path(folder,"bad.request.json"))
  asu_service_checkpoints(folder,data)
  stopifnot(!file.exists(file.path(folder,"bad.rds")),
    identical(jsonlite::fromJSON(file.path(folder,"bad.ack.json"))$ok,FALSE))
  # Disk/publication failure cannot be acknowledged as saved.
  jsonlite::write_json(list(asu_id=c(1,1)),file.path(folder,"blocked.request.json"))
  dir.create(file.path(folder,"blocked.rds"))
  suppressWarnings(asu_service_checkpoints(folder,data))
  stopifnot(identical(jsonlite::fromJSON(file.path(folder,"blocked.ack.json"))$ok,FALSE))
  cat("Checkpoint round-trip, immutable geometry/counts, warm start and failure tests passed.\n")
}
run_checkpoint_tests()
