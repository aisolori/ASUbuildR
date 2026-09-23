# Execute the dashboard's actual generated runner and service its RDS requests.
source("R/solver_checkpoint.R")
run_checkpoint_runner <- function() {
  python <- Sys.getenv("ASU_TEST_PYTHON")
  if (!nzchar(python)) {
    cat("SKIP runner integration: set ASU_TEST_PYTHON.\n")
    return(invisible(NULL))
  }
  folder <- tempfile("checkpoint-runner-")
  dir.create(folder)
  on.exit(unlink(folder, recursive=TRUE),add=TRUE)
  script <- file.path(folder,"dashboard.R")
  knitr::purl("inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd",output=script,quiet=TRUE)
  find_runner <- function(expr) {
    if (missing(expr)) return(NULL)
    if (is.call(expr) && identical(expr[[1]],as.name("<-")) &&
        identical(expr[[2]],as.name("runner_code"))) return(expr)
    if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
      for (child in as.list(expr)) {
        result <- find_runner(child)
        if (!is.null(result)) return(result)
      }
    }
    NULL
  }
  expr <- find_runner(parse(script))
  settings <- new.env(parent=baseenv())
  for (name in all.vars(expr)) assign(name,1L,settings)
  for (name in c("df_csv","nb_json","out_json","progress_json","stop_file","skip_file"))
    assign(name,file.path(folder,name),settings)
  settings$py_mod_path <- normalizePath("inst/python",winslash="/")
  settings$run_dir <- normalizePath(folder,winslash="/")
  settings$input <- list(cpsat_final_consolidation=FALSE,cpsat_polish_consolidated_asus=FALSE)
  settings$partition_seed_strategy <- "connectivity_free"
  settings$tau <- .1
  settings$pop_thr <- 10000L
  settings$max_asus <- 1L
  settings$tlimit <- 1L
  settings$workers <- 1L
  settings$rel_gap <- NA_real_
  polygon <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2,byrow=TRUE)))
  data <- sf::st_sf(GEOID=c("55001000100","55001000200"),asunum=c(0L,0L),
    tract_pop_cur=c(6000L,6000L),tract_ASU_unemp=c(30L,30L),tract_ASU_emp=c(70L,70L),
    geometry=sf::st_sfc(polygon,polygon,crs=4326))
  utils::write.csv(data.frame(geoid=data$GEOID,tract_ASU_unemp=data$tract_ASU_unemp,
    tract_ASU_emp=data$tract_ASU_emp,tract_pop2024=data$tract_pop_cur),settings$df_csv,row.names=FALSE)
  jsonlite::write_json(list(list(1L),list(0L)),settings$nb_json,auto_unbox=TRUE)
  for (strategy in c("single","partition","split")) {
    settings$use_partitioning <- strategy=="partition"
    settings$use_split <- strategy=="split"
    settings$legacy_checkpoint <- strategy=="single"
    eval(expr,settings)
    runner <- file.path(folder,paste0(strategy,".py"))
    writeLines(settings$runner_code,runner)
    stopifnot(system2(python,c("-m","py_compile",shQuote(runner)))==0L)
    if (strategy!="single") next
    px <- processx::process$new(python,runner,stdout=file.path(folder,"stdout.log"),
      stderr=file.path(folder,"stderr.log"),windows_hide_window=TRUE)
    on.exit(if(px$is_alive()) px$kill(),add=TRUE)
    start <- Sys.time()
    while(px$is_alive() && as.numeric(difftime(Sys.time(),start,units="secs"))<60) {
      asu_service_checkpoints(folder,data)
      Sys.sleep(.05)
    }
    if(px$is_alive() || px$get_exit_status()!=0L) {
      cat(readLines(file.path(folder,"stdout.log")),sep="\n")
      cat(readLines(file.path(folder,"stderr.log")),sep="\n")
      stop("Runner failed")
    }
    # Both the exact result and final result must round-trip with full geometry.
    saves <- list.files(folder,pattern="\\.rds$",full.names=TRUE)
    stopifnot(any(grepl("legacy_solve_",saves)),any(grepl("legacy_done_",saves)))
    for (path in saves) {
      saved <- readRDS(path)
      stopifnot(identical(saved$asunum,c(1L,1L)),identical(saved$GEOID,data$GEOID),
        identical(sf::st_geometry(saved),sf::st_geometry(data)),
        identical(saved$tract_pop_cur,data$tract_pop_cur))
    }
  }
  cat("Generated runner: real solve, synchronized early/final RDS saves, all strategies compile.\n")
}
run_checkpoint_runner()
