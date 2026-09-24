source('R/solver_jobs.R')
test_job_dashboard <- function() {
  script <- tempfile(fileext='.R')
  folder <- tempfile('job-viewer-')
  dir.create(folder)
  on.exit(unlink(c(script,folder), recursive=TRUE))
  knitr::purl('inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd',output=script,quiet=TRUE)
  expressions <- as.list(parse(script))
  lhs <- function(e) if (is.call(e) && identical(e[[1]],as.name('<-')))
    paste(deparse(e[[2]]),collapse='') else ''
  names <- vapply(expressions,lhs,character(1))
  selected <- expressions[names %in% c('append_cpsat_log','attach_cpsat_job','%||%') |
    (startsWith(names,'cpsat_') & vapply(expressions,function(e)
      is.call(e) && length(e)>=3 && is.call(e[[3]]) &&
      identical(paste(deparse(e[[3]][[1]]),collapse=''),'shiny::reactiveVal'),logical(1)))]
  ending <- expressions[vapply(expressions,function(e)
    is.call(e) && identical(paste(deparse(e[[1]]),collapse=''),'session$onSessionEnded'),logical(1))]
  stopifnot(length(ending)==1L, any(names=='attach_cpsat_job'))
  polygon <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2,byrow=TRUE)))
  data <- sf::st_sf(GEOID=c('55001000100','55001000200'),asunum=c(0L,0L),
    tract_pop_cur=c(6000L,6000L),tract_ASU_unemp=c(30L,30L),tract_ASU_emp=c(70L,70L),
    tract_ASU_clf=c(100L,100L),tract_ASU_urate=c(30,30),
    geometry=sf::st_sfc(polygon,polygon,crs=4326))
  saveRDS(data,file.path(folder,'input.rds'))
  utils::write.csv(data.frame(geoid=data$GEOID),file.path(folder,'df.csv'),row.names=FALSE)
  jsonlite::write_json(list(list(1L),list(0L)),file.path(folder,'nb.json'),auto_unbox=TRUE)
  jsonlite::write_json(list(asu_id=c(1L,1L)),file.path(folder,'out.json'))
  jsonlite::write_json(list(asu_id=c(1L,1L)),file.path(folder,'progress.json'))
  jsonlite::write_json(list(status='completed',exit_code=0L),file.path(folder,'status.json'),auto_unbox=TRUE)
  writeLines('[runner] OK',file.path(folder,'solver.log'))
  original <- readBin(file.path(folder,'solver.log'),'raw',n=10000)
  shiny::testServer(function(input,output,session) {
    `%||%` <- function(a,b) if (!is.null(a) && length(a)) a else b
    for (name in c('full_data','full_data_reset','nb_adjacency','selected_tracts',
                   'highlighted_tracts','state','display_state','asu_summary'))
      assign(name,shiny::reactiveVal(NULL),envir=environment())
    update_map_efficiently <- function(...) invisible(NULL)
    apply_overlay_filter <- function(...) invisible(NULL)
    for (e in c(selected,ending)) eval(e,envir=environment())
  }, {
    attach_cpsat_job(folder)
    session$flushReact()
    stopifnot(identical(full_data()$asunum,c(1L,1L)), !cpsat_running(),
              cpsat_stage_text()=='completed', display_state()=='55',
              identical(nb_adjacency()$nb,list(2L,1L)))
    # Reattaching must be idempotent and must not append the log back to itself.
    attach_cpsat_job(folder)
    session$flushReact()
    stopifnot(identical(full_data()$asunum,c(1L,1L)),
      identical(readBin(file.path(folder,'solver.log'),'raw',n=10000),original))
    session$close()
  })
  stopifnot(!file.exists(file.path(folder,'stop.flag')),
    any(grepl('session ended',readLines(file.path(folder,'events.log')))))
  cat('PASS fresh-session attach / assignments / logs / disconnect does not request stop\n')
}
test_job_dashboard()
