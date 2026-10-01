# Listing filters inactive jobs without deleting their saved artifacts.
test_running_job_list <- function() {
  env <- new.env(parent = globalenv())
  sys.source('R/solver_jobs.R', envir = env)
  root <- tempfile('running-job-list-')
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  statuses <- c('running', 'starting', 'completed', 'stopped', 'failed', 'interrupted', 'stale')
  folders <- file.path(root, statuses)
  for (folder in folders) {
    dir.create(folder)
    file.create(file.path(folder, 'job.json'))
  }
  env$asu_job_root <- function() root
  env$asu_job_read <- function(path) list(strategy = 'partition')
  env$asu_job_state <- function(folder) {
    status <- basename(folder)
    list(status = if (status == 'stale') 'running' else status,
         alive = status %in% c('running', 'starting'))
  }
  jobs <- env$asu_job_list()
  stopifnot(setequal(basename(unname(jobs)), c('running', 'starting')),
            all(file.exists(file.path(folders, 'job.json'))))
  # A finished process disappears on the next refresh, even if its saved
  # status still says running.
  env$asu_job_state <- function(folder) list(status = 'running', alive = FALSE)
  stopifnot(length(env$asu_job_list()) == 0L,
            all(file.exists(file.path(folders, 'job.json'))))
  cat('PASS running-only job listing / stale status / retained files\n')
}
test_running_job_list()
