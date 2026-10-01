# Exercise the dashboard's actual handlers through Shiny's download mechanism.
run_save_download_tests <- function() {
  dashboard <- file.path("inst", "shiny_app", "ASU_Flexdashboard_mapgl.Rmd")
  if (!file.exists(dashboard)) dashboard <- system.file("shiny_app", "ASU_Flexdashboard_mapgl.Rmd", package = "ASUbuildR")
  folder <- tempfile("asu-save-tests-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  script <- file.path(folder, "dashboard.R")
  knitr::purl(dashboard, output = script, quiet = TRUE)
  expressions <- parse(script)
  names <- c("cpsat_log_download", "output$save_live_log", "output$save_finished_log", "output$save_data")
  handlers <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("<-")) &&
                       paste(deparse(expr[[2]]), collapse = "") %in% names, as.list(expressions))
  stopifnot(length(handlers) == 4L)
  live <- file.path(folder, "live.log")
  finished <- file.path(folder, "finished.log")
  # More than the display tail, including UTF-8; download must preserve every byte.
  writeBin(charToRaw(paste(rep("solver line\n", 10000), collapse = "")), live)
  writeBin(as.raw(c(65, 13, 10, 195, 169, 10)), finished)
  expected <- data.frame(GEOID = c("01001000100", "01001000200"), asunum = c(1L, 0L))
  shiny::testServer(function(input, output, session) {
    `%||%` <- function(a, b) if (!is.null(a) && length(a)) a else b
    full_data <- shiny::reactiveVal(expected)
    cpsat_log_file <- shiny::reactiveVal(live)
    cpsat_log_saved <- shiny::reactiveVal(finished)
    for (expr in handlers) eval(expr, envir = environment())
  }, {
    stopifnot(identical(readRDS(output$save_data), expected))
    downloaded <- output$save_live_log
    stopifnot(identical(readBin(downloaded, "raw", file.info(downloaded)$size),
                        readBin(live, "raw", file.info(live)$size)))
    cpsat_log_file(NULL)
    downloaded <- output$save_finished_log
    stopifnot(identical(readBin(downloaded, "raw", file.info(downloaded)$size),
                        readBin(finished, "raw", file.info(finished)$size)))
  })
  cat("RDS and complete-log downloads passed.\n")
}
run_save_download_tests()
