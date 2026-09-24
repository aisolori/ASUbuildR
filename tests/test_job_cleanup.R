source('R/solver_jobs.R')
test_job_cleanup <- function() {
  root <- tempfile('job-cleanup-test-')
  dir.create(root)
  previous <- Sys.getenv('ASU_JOB_DIR', unset = NA_character_)
  Sys.setenv(ASU_JOB_DIR = root)
  on.exit({
    if (is.na(previous)) Sys.unsetenv('ASU_JOB_DIR') else Sys.setenv(ASU_JOB_DIR = previous)
    unlink(root, recursive = TRUE)
  })
  now <- floor(as.numeric(Sys.time()))
  make <- function(name, status, age, pid = NULL) {
    folder <- file.path(root, name)
    dir.create(folder)
    jsonlite::write_json(list(strategy = 'partition'), file.path(folder, 'job.json'), auto_unbox = TRUE)
    jsonlite::write_json(list(status = status, finished_at = now - age, solver_pid = pid),
                        file.path(folder, 'status.json'), auto_unbox = TRUE)
    Sys.setFileTime(list.files(folder, full.names = TRUE),
                    as.POSIXct(now - age, origin = '1970-01-01'))
    folder
  }
  old <- make('job_old', 'completed', 86401)
  interrupted <- make('job_interrupted', 'interrupted', 90000)
  stale <- make('job_stale_running', 'running', 90000)
  recent_activity <- make('job_recent_activity', 'running', 90000)
  file.create(file.path(recent_activity, 'solver.log'))
  keep <- c(
    make('job_recent', 'completed', 86399),
    make('job_boundary', 'completed', 86400),
    make('job_failed', 'failed', 90000),
    make('job_stopped', 'stopped', 90000),
    make('job_running', 'running', 90000, Sys.getpid()),
    make('job_interrupted_recent', 'interrupted', 3600),
    make('job_interrupted_live', 'interrupted', 90000, Sys.getpid()),
    recent_activity,
    make('job_live_pid', 'completed', 90000, Sys.getpid()),
    make('unrelated', 'completed', 90000))
  removed <- asu_job_cleanup(now)
  stopifnot(length(removed) == 3L, !dir.exists(old),
            !dir.exists(interrupted), !dir.exists(stale), all(dir.exists(keep)),
            length(asu_job_cleanup(now)) == 0L)
  cat('PASS completed/interrupted cleanup / recent activity / live PID protection / scope / idempotence\n')
}
test_job_cleanup()
