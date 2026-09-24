# Integration: launcher R exits, yet solver logs, checkpoints and final RDS survive.
source('R/solver_checkpoint.R')
source('R/solver_jobs.R')
test_solver_jobs <- function() {
  python <- Sys.getenv('ASU_TEST_PYTHON')
  if (!nzchar(python)) return(cat('SKIP detached integration: set ASU_TEST_PYTHON\n'))
  root <- tempfile('detached-jobs-', tmpdir = Sys.getenv('TEMP', tempdir()))
  dir.create(root)
  previous <- Sys.getenv('ASU_JOB_DIR', unset = NA_character_)
  Sys.setenv(ASU_JOB_DIR = root)
  on.exit(if (is.na(previous)) Sys.unsetenv('ASU_JOB_DIR') else Sys.setenv(ASU_JOB_DIR = previous))
  polygon <- sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE)))
  data <- sf::st_sf(GEOID=c('55001000100','55001000200'), asunum=c(0L,0L),
    tract_pop_cur=c(6000L,6000L), tract_ASU_unemp=c(30L,30L),
    tract_ASU_emp=c(70L,70L), geometry=sf::st_sfc(polygon,polygon,crs=4326))
  rscript <- file.path(R.home('bin'), 'Rscript')
  if (.Platform$OS.type == 'windows') rscript <- paste0(rscript, '.exe')
  module_dir <- normalizePath('inst/python', winslash='/')
  prepare <- function(strategy) {
    folder <- asu_job_run_dir()
    asu_job_prepare(folder, data, python, module_dir, strategy)
    utils::write.csv(data.frame(geoid=data$GEOID), file.path(folder,'df.csv'), row.names=FALSE)
    jsonlite::write_json(list(list(1L),list(0L)), file.path(folder,'nb.json'), auto_unbox=TRUE)
    folder
  }
  wait_job <- function(folder) {
    deadline <- Sys.time() + 60
    repeat {
      state <- asu_job_state(folder)
      if (!state$alive) break
      if (Sys.time() > deadline) stop('Detached integration timeout; job files retained: ', folder)
      Sys.sleep(.1)
    }
    if (!state$status %in% c('completed','stopped','failed')) {
      cat(readLines(file.path(folder,'supervisor.log')), sep='\n')
      cat(asu_job_log_tail(folder), sep='\n')
      stop('Unexpected job status: ', state$status)
    }
    state
  }
  folder <- prepare('single')
  writeLines(c(
    'import sys,time,json',
    'from pathlib import Path',
    'folder = Path(__file__).parent',
    'sys.path.insert(0, str(folder / "python"))',
    'from asu_checkpoint import request_rds_checkpoint',
    'deadline=time.monotonic()+20',
    'while not (folder / "launcher_exited.flag").exists():',
    '    if time.monotonic()>deadline: raise RuntimeError("launcher did not exit independently")',
    '    time.sleep(.05)',
    'print("AFTER_LAUNCHER_EXIT", flush=True)',
    'print("STDERR_SURVIVES", file=sys.stderr, flush=True)',
    'print("large output " * 20000, flush=True)',
    'request_rds_checkpoint(folder, "LEGACY_SOLVE", [1,1])',
    '(folder / "out.json").write_text(json.dumps({"asu_id":[1,1]}))'
  ), file.path(folder,'runner.py'))
  launcher <- file.path(root, 'launcher.R')
  writeLines(c(
    paste0('.libPaths(', paste(deparse(.libPaths()), collapse=''), ')'),
    paste0('source(', deparse(normalizePath('R/solver_jobs.R', winslash='/')), ')'),
    paste0('asu_job_launch(', deparse(folder), ')'),
    'quit(save="no")'
  ), launcher)
  launch_process <- processx::process$new(rscript, c('--vanilla', launcher), windows_hide_window=TRUE,
    stdout=file.path(root,'launcher.stdout'), stderr=file.path(root,'launcher.stderr'))
  launch_process$wait(10000)
  if (launch_process$is_alive()) stop('Launcher did not exit within 10 seconds: ', root)
  if (launch_process$get_exit_status() != 0) stop(paste(readLines(file.path(root,'launcher.stderr')), collapse='\n'))
  if (!isTRUE(asu_job_state(folder)$alive)) {
    print(asu_job_state(folder))
    print(asu_job_read(file.path(folder,'owner.json')))
    if (file.exists(file.path(folder,'supervisor.log'))) cat(readLines(file.path(folder,'supervisor.log')),sep='\n')
    stop('Expected running detached job; files retained at ', folder)
  }
  file.create(file.path(folder,'launcher_exited.flag'))
  state <- wait_job(folder)
  stopifnot(state$status == 'completed', state$exit_code == 0L,
            state$recovery_exit_code == 0L)
  log <- readLines(file.path(folder,'solver.log'))
  stopifnot(any(grepl('AFTER_LAUNCHER_EXIT',log)), any(grepl('STDERR_SURVIVES',log)),
            any(grepl('Solver exit_code=0',log)), any(grepl('RDS saved:',log)))
  saved <- readRDS(file.path(folder,'result.rds'))
  stopifnot(identical(saved$asunum,c(1L,1L)),
            identical(sf::st_geometry(saved), sf::st_geometry(data)),
            length(list.files(folder, pattern='legacy_solve_.*\\.rds$')) == 1L)
  # A new viewer reads a finished job; it never starts another process.
  viewer <- asu_job_handle(folder)
  stopifnot(!viewer$is_alive(), viewer$get_exit_status() == 0L,
            length(viewer$read_output_lines()) > 0, length(viewer$read_output_lines()) == 0)
  stopifnot(inherits(try(asu_job_launch(folder), silent=TRUE),'try-error'))
  cat('PASS launcher exit / direct logs / independent checkpoint / final RDS / reattach\n')

  folder <- prepare('partition')
  writeLines(c('import json', 'from pathlib import Path',
    'folder=Path(__file__).parent',
    '(folder / "progress.json").write_text(json.dumps({"asu_id":[1,1]}))',
    'raise RuntimeError("INTENTIONAL_TEST_FAILURE")'), file.path(folder,'runner.py'))
  asu_job_launch(folder)
  state <- wait_job(folder)
  stopifnot(state$status == 'failed', state$exit_code != 0L,
            file.exists(file.path(folder,'recovered.rds')),
            any(grepl('INTENTIONAL_TEST_FAILURE', asu_job_log_tail(folder))))
  cat('PASS failed solve exit status / traceback / progress recovery\n')

  folder <- prepare('split')
  writeLines(c('import json,time', 'from pathlib import Path', 'folder=Path(__file__).parent',
    'while not (folder / "stop.flag").exists(): time.sleep(.05)',
    '(folder / "out.json").write_text(json.dumps({"asu_id":[1,1]}))'),
    file.path(folder,'runner.py'))
  asu_job_launch(folder)
  viewer <- asu_job_handle(folder)
  stopifnot(viewer$is_alive())
  file.create(file.path(folder,'stop.flag'))
  state <- wait_job(folder)
  stopifnot(state$status == 'stopped', file.exists(file.path(folder,'result.rds')),
            file.exists(file.path(folder,'stop.flag')))
  cat('PASS explicit stop / final RDS / stop evidence retained\n')
  # Only remove this test's uniquely created root, after every child has exited.
  stopifnot(all(!vapply(unname(asu_job_list()), function(p) asu_job_state(p)$alive, logical(1))))
  unlink(root, recursive=TRUE)
}
test_solver_jobs()
