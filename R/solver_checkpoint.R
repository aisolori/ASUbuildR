# A persistent, unique directory per run. Never put recovery files in tempdir().
asu_checkpoint_run_dir <- function() {
  root <- path.expand(Sys.getenv("ASU_CHECKPOINT_DIR", "~/ASUbuildR/checkpoints"))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(root)) stop("Cannot create checkpoint folder: ", root)
  folder <- tempfile(paste0("legacy_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_"), tmpdir = root)
  if (!dir.create(folder)) stop("Cannot create checkpoint run folder: ", folder)
  normalizePath(folder, winslash = "/", mustWork = TRUE)
}

# Python waits for the acknowledgment before starting refinement/another solve.
# Immutable requests survive missed progress ticks and failed later stages.
asu_service_checkpoints <- function(folder, data, log = message) {
  requests <- list.files(folder, pattern = "\\.request\\.json$", full.names = TRUE)
  for (request in requests) {
    stem <- sub("\\.request\\.json$", "", request)
    ack <- paste0(stem, ".ack.json")
    if (file.exists(ack)) next
    target <- paste0(stem, ".rds")
    result <- tryCatch({
      payload <- jsonlite::fromJSON(request)
      ids <- payload$asu_id
      if (!inherits(data, "sf") || !is.numeric(ids) || length(ids) != nrow(data) ||
          anyNA(ids) || any(!is.finite(ids) | ids != trunc(ids) | ids < -1 | ids > .Machine$integer.max))
        stop("Invalid checkpoint assignments or source geometry")
      snapshot <- data
      snapshot$asunum <- pmax(0L, as.integer(ids))
      pending <- paste0(target, ".pending")
      on.exit(unlink(pending), add = TRUE)
      # No compression: minimize the pause and CPU work while retaining exact sf.
      saveRDS(snapshot, pending, compress = FALSE)
      if (!file.rename(pending, target)) stop("Could not publish RDS checkpoint")
      log(paste0("[checkpoint] Saved ", target, " (", file.info(target)$size, " bytes)"))
      list(ok = TRUE, path = target)
    }, error = function(e) {
      log(paste0("[checkpoint][error] ", conditionMessage(e)))
      list(ok = FALSE, error = conditionMessage(e))
    })
    jsonlite::write_json(result, paste0(ack, ".pending"), auto_unbox = TRUE)
    if (!file.rename(paste0(ack, ".pending"), ack)) stop("Could not acknowledge checkpoint")
  }
  invisible(NULL)
}
