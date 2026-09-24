# Durable per-user jobs; all commands operate on a server-side selected folder.
asu_job_root <- function() {
  path.expand(Sys.getenv('ASU_JOB_DIR', '~/ASUbuildR/jobs'))
}

asu_job_run_dir <- function() {
  root <- asu_job_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  folder <- tempfile(paste0('job_', format(Sys.time(), '%Y%m%d_%H%M%S'), '_'), tmpdir = root)
  if (!dir.create(folder)) stop('Cannot create job folder: ', folder)
  normalizePath(folder, winslash = '/', mustWork = TRUE)
}

asu_job_read <- function(path) {
  tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
}

asu_job_prepare <- function(folder, data, python, module_dir, strategy, settings = list()) {
  stopifnot(inherits(data, 'sf'), !anyDuplicated(data$GEOID))
  saveRDS(data, file.path(folder, 'input.rds'), compress = FALSE)
  dir.create(file.path(folder, 'python'))
  modules <- list.files(module_dir, pattern = '\\.py$', full.names = TRUE)
  if (!length(modules) || !all(file.copy(modules, file.path(folder, 'python'))))
    stop('Could not snapshot solver modules')
  # Resolve package/dev helper locations without depending on a future session.
  helper <- getSrcFilename(asu_service_checkpoints, full.names = TRUE)
  if (!length(helper) || !file.exists(helper)) {
    helper <- file.path(module_dir, '..', '..', 'R', 'solver_checkpoint.R')
  }
  if (!file.exists(helper)) {
    # Installed packages have lazy-loaded functions rather than R source files.
    helper <- file.path(folder, 'checkpoint_functions.R')
    dump('asu_service_checkpoints', file = helper, envir = environment(asu_service_checkpoints))
  } else if (!file.copy(helper, file.path(folder, 'checkpoint_functions.R'))) {
    stop('Cannot snapshot checkpoint helper')
  }
  dump('asu_job_recover', file = file.path(folder, 'recovery_functions.R'),
       envir = environment(asu_job_recover))
  writeLines(c(
    'args <- commandArgs(trailingOnly = TRUE)',
    'folder <- normalizePath(args[1L], winslash = "/", mustWork = TRUE)',
    'config <- jsonlite::fromJSON(file.path(folder, "job.json"))',
    '.libPaths(unique(c(config$r_libraries, .libPaths())))',
    'source(file.path(folder, "checkpoint_functions.R"))',
    'source(file.path(folder, "recovery_functions.R"))',
    'data <- readRDS(file.path(folder, "input.rds"))',
    'asu_service_checkpoints(folder, data)',
    'if (length(args) == 1L) asu_job_recover(folder)'
  ), file.path(folder, 'recover.R'))
  rscript <- file.path(R.home('bin'), if (.Platform$OS.type == 'windows') 'Rscript.exe' else 'Rscript')
  config <- list(schema = 1L, created_at = as.numeric(Sys.time()), strategy = strategy,
                 python = normalizePath(python, winslash = '/'), rscript = rscript,
                 r_libraries = as.list(.libPaths()), settings = settings)
  jsonlite::write_json(config, file.path(folder, 'job.json'), auto_unbox = TRUE, pretty = TRUE)
  # --vanilla R must find jsonlite before it can read the saved library paths.
  writeLines(c(paste0('.libPaths(', paste(deparse(.libPaths()), collapse = ''), ')'),
               readLines(file.path(folder, 'recover.R'))), file.path(folder, 'recover.R'))
  invisible(config)
}

asu_job_launch <- function(folder) {
  config <- asu_job_read(file.path(folder, 'job.json'))
  if (file.exists(file.path(folder, 'owner.json')) || file.exists(file.path(folder, 'status.json')))
    stop('This job has already been launched; attach to it instead')
  px <- processx::process$new(config$python,
    c('-u', file.path(folder, 'python', 'asu_job.py'), folder),
    stdin = NULL, stdout = file.path(folder, 'supervisor.log'), stderr = '2>&1',
    cleanup = FALSE, supervise = FALSE, windows_detached_process = TRUE,
    windows_hide_window = TRUE)
  owner <- list(pid = px$get_pid(), created = as.numeric(ps::ps_create_time(ps::ps_handle(px$get_pid()))))
  jsonlite::write_json(owner, file.path(folder, 'owner.json'), auto_unbox = TRUE)
  asu_job_handle(folder)
}

asu_job_state <- function(folder) {
  state <- asu_job_read(file.path(folder, 'status.json'))
  if (is.null(state)) state <- list(status = 'starting', exit_code = NULL)
  owner <- asu_job_read(file.path(folder, 'owner.json'))
  alive <- tryCatch({
    if (is.null(owner)) FALSE else {
      handle <- ps::ps_handle(owner$pid)
      # JSON/Windows clock precision differs; still reject recycled PIDs.
      abs(as.numeric(ps::ps_create_time(handle)) - owner$created) < .01 && ps::ps_is_running(handle)
    }
  }, error = function(e) FALSE)
  # A surviving child after a supervisor failure is still stoppable via flags.
  child_alive <- tryCatch(!is.null(state$solver_pid) &&
    ps::ps_is_running(ps::ps_handle(state$solver_pid)) &&
    any(normalizePath(ps::ps_cmdline(ps::ps_handle(state$solver_pid)), winslash = '/',
                      mustWork = FALSE) == normalizePath(file.path(folder, 'runner.py'), winslash = '/')),
    error = function(e) FALSE)
  terminal <- state$status %in% c('completed', 'stopped', 'failed')
  state$alive <- !terminal && (alive || child_alive)
  if (!state$alive && !terminal) state$status <- 'interrupted'
  state
}

asu_job_log_tail <- function(folder, bytes = 64000L) {
  path <- file.path(folder, 'solver.log')
  if (!file.exists(path)) return(character())
  con <- file(path, 'rb')
  on.exit(close(con))
  start <- max(0, file.info(path)$size - bytes)
  seek(con, start)
  value <- rawToChar(readBin(con, 'raw', n = bytes))
  lines <- strsplit(value, '\n', fixed = TRUE)[[1L]]
  if (start > 0 && length(lines)) lines <- lines[-1L]
  enc2utf8(lines)
}

asu_job_handle <- function(folder) {
  last_size <- -1
  read_new <- function() {
    size <- file.info(file.path(folder, 'solver.log'))$size
    if (is.na(size) || identical(size, last_size)) return(character())
    last_size <<- size
    asu_job_log_tail(folder)
  }
  list(is_alive = function() isTRUE(asu_job_state(folder)$alive),
       get_pid = function() asu_job_read(file.path(folder, 'owner.json'))$pid,
       get_exit_status = function() {
         status <- asu_job_state(folder)
         if (status$status %in% c('completed', 'stopped')) 0L else
           if (!is.null(status$exit_code)) as.integer(status$exit_code) else NA_integer_
       },
       read_output_lines = read_new, read_error_lines = function() character(),
       read_all_output_lines = read_new, read_all_error_lines = function() character())
}

asu_job_list <- function() {
  root <- asu_job_root()
  folders <- if (dir.exists(root)) list.dirs(root, recursive = FALSE, full.names = TRUE) else character()
  folders <- folders[file.exists(file.path(folders, 'job.json'))]
  folders <- sort(folders, decreasing = TRUE)
  stats::setNames(folders, vapply(folders, function(folder) {
    paste(basename(folder), asu_job_read(file.path(folder, 'job.json'))$strategy,
          paste0('[', asu_job_state(folder)$status, ']'))
  }, character(1)))
}

asu_job_recover <- function(folder) {
  # Reconstruct by GEOID/order-checked immutable input, not transient map state.
  data <- readRDS(file.path(folder, 'input.rds'))
  table <- utils::read.csv(file.path(folder, 'df.csv'), colClasses = c(geoid = 'character'))
  if (!identical(as.character(data$GEOID), table$geoid)) stop('Recovery GEOID order mismatch')
  final <- file.path(folder, 'out.json')
  path <- if (file.exists(final)) final else file.path(folder, 'progress.json')
  if (!file.exists(path)) stop('No assignments have been published yet')
  payload <- jsonlite::fromJSON(path)
  ids <- payload$asu_id
  if (!is.numeric(ids) || length(ids) != nrow(data) || anyNA(ids) ||
      any(!is.finite(ids) | ids != trunc(ids) | ids < -1 | ids > .Machine$integer.max))
    stop('Invalid recovery assignments')
  data$asunum <- pmax(0L, as.integer(ids))
  target <- file.path(folder, if (identical(path, final)) 'result.rds' else 'recovered.rds')
  pending <- tempfile('recovery-', tmpdir = folder, fileext = '.pending')
  on.exit(unlink(pending), add = TRUE)
  saveRDS(data, pending, compress = FALSE)
  # Never silently return an older progress recovery or overwrite a prior save.
  if (file.exists(target)) target <- tempfile(
    paste0(tools::file_path_sans_ext(basename(target)), '_'), tmpdir = folder, fileext = '.rds')
  if (!file.rename(pending, target)) stop('Cannot publish recovery RDS')
  message('[job] Saved ', target)
  invisible(target)
}
