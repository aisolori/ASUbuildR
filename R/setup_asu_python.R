# ASUbuildR — Python setup (conda-only) ---------------------------------------

# Internal helpers -------------------------------------------------------------

#' @keywords internal
.asu_data_dir <- function() {
  dir <- rappdirs::user_data_dir("ASUbuildR")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' @keywords internal
.asu_state_path <- function() file.path(.asu_data_dir(), "python_state.json")

#' @keywords internal
.write_state <- function(x) {
  x$last_updated <- format(Sys.time(), tz = "UTC")
  jsonlite::write_json(x, .asu_state_path(), auto_unbox = TRUE, pretty = TRUE)
}

#' @keywords internal
.read_state <- function() {
  p <- .asu_state_path()
  if (file.exists(p)) {
    tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
  } else {
    NULL
  }
}

#' @keywords internal
.unset_conflicting_env <- function() {
  Sys.unsetenv("PYTHONPATH")
  Sys.unsetenv("RETICULATE_PYTHON")
  invisible(TRUE)
}

#' @keywords internal
.run_quiet <- function(cmd, args) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  res <- tryCatch(system2(cmd, args, stdout = tf, stderr = tf), error = function(e) 1L)
  if (is.null(res)) 0L else res
}

#' @keywords internal
.find_conda <- function() {
  st <- .read_state()
  if (!is.null(st$conda_bin) && nzchar(st$conda_bin) && file.exists(st$conda_bin)) {
    return(st$conda_bin)
  }
  path_guess <- Sys.which("conda")
  if (nzchar(path_guess)) return(path_guess)
  mc <- tryCatch(reticulate::miniconda_path(), error = function(e) NULL)
  if (!is.null(mc)) {
    cb <- file.path(mc, "bin", "conda")
    if (file.exists(cb)) return(cb)
  }
  ""
}

#' @keywords internal
.ensure_miniconda <- function() {
  conda_bin <- .find_conda()
  if (nzchar(conda_bin)) return(conda_bin)
  message("No Conda found; installing Miniconda via reticulate...")
  reticulate::install_miniconda()
  mc_root <- reticulate::miniconda_path()
  conda_bin <- file.path(mc_root, "bin", "conda")
  if (!file.exists(conda_bin)) stop("Miniconda install reported success but conda not found.")
  conda_bin
}

#' @keywords internal
.configure_conda_forge <- function(conda_bin) {
  condarc_path <- file.path(Sys.getenv("HOME"), ".condarc")
  writeLines(
    paste(
      "channels:",
      "  - conda-forge",
      "channel_priority: strict",
      "default_channels: []",
      "custom_channels: {}",
      sep = "\n"
    ),
    condarc_path
  )
  .run_quiet(conda_bin, c("config", "--remove", "channels", "defaults"))
  .run_quiet(conda_bin, c("config", "--remove", "channels", "https://repo.anaconda.com/pkgs/main"))
  .run_quiet(conda_bin, c("config", "--remove", "channels", "https://repo.anaconda.com/pkgs/r"))
  system2(conda_bin, c("config", "--add", "channels", "conda-forge"), stdout = TRUE, stderr = TRUE)
  system2(conda_bin, c("config", "--set", "channel_priority", "strict"), stdout = TRUE, stderr = TRUE)
  invisible(TRUE)
}

#' @keywords internal
.conda_env_path <- function(conda_bin, env_name) {
  txt <- suppressWarnings(system2(conda_bin, c("info", "--envs", "--json"), stdout = TRUE))
  if (!length(txt)) return(NA_character_)
  dat <- tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n")), error = function(e) NULL)
  if (is.null(dat) || is.null(dat$envs)) return(NA_character_)
  cand <- dat$envs[grepl(sprintf("/%s$", env_name), dat$envs)]
  if (length(cand)) cand[[1]] else NA_character_
}

#' @keywords internal
.ensure_asu_env <- function(conda_bin, env_name = "asu-cpsat-venv") {
  # create if missing
  out <- suppressWarnings(system2(conda_bin, c("env", "list"), stdout = TRUE))
  has_env <- any(grepl(sprintf("(^|[[:space:]])%s([[:space:]]|$)", env_name), out)) ||
    any(grepl(sprintf("/%s$", env_name), out))

  if (!has_env) {
    message(sprintf("Creating Conda env '%s' with Python 3.10...", env_name))
    st <- system2(
      conda_bin,
      c("create", "--yes", "--name", env_name, "python=3.10",
        "--override-channels", "-c", "conda-forge"),
      stdout = TRUE, stderr = TRUE
    )
    if (!is.null(attr(st, "status")) && attr(st, "status") != 0L) {
      stop("conda create failed; see logs above.")
    }
  } else {
    message(sprintf("Conda env '%s' already exists.", env_name))
  }

  # install core deps from conda-forge (no pip '==')
  st2 <- system2(
    conda_bin,
    c("install", "--yes", "--name", env_name,
      "numpy", "pandas", "networkx", "ortools", "pip",
      "--override-channels", "-c", "conda-forge"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(st2, "status")) && attr(st2, "status") != 0L) {
    stop("conda install failed; see logs above.")
  }

  # optional: immutabledict (often pip-only)
  py <- tryCatch(reticulate::conda_python(env_name), error = function(e) NA_character_)
  if (!is.na(py)) {
    # install only if not available already
    have_imm <- tryCatch({
      system2(py, c("-c", "import importlib; import sys; sys.exit(0 if importlib.util.find_spec('immutabledict') else 1)"))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(have_imm)) {
      suppressWarnings(system2(py, c("-m", "pip", "install", "immutabledict"), stdout = TRUE, stderr = TRUE))
    }
  }

  env_path <- .conda_env_path(conda_bin, env_name)
  list(env_name = env_name, env_path = env_path)
}

#' @keywords internal
.use_asu_env <- function(env_name = "asu-cpsat-venv") {
  reticulate::use_condaenv(env_name, required = TRUE)
  ok1 <- reticulate::py_module_available("ortools")
  ok2 <- reticulate::py_module_available("pandas")
  ok3 <- reticulate::py_module_available("numpy")
  if (!all(ok1, ok2, ok3)) stop("Conda env is active, but required modules are missing.")
  invisible(TRUE)
}

# helper: %||%
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x

# Public API ------------------------------------------------------------------

#' Set up Python for ASUbuildR (Conda + conda-forge + env creation)
#'
#' Creates/updates 'asu-cpsat-venv', installs deps, activates it, prints versions,
#' and persists conda/env paths. Conda-only, no virtualenv fallback.
#'
#' @return logical TRUE on success, FALSE on failure
#' @export
setup_asu_python <- function() {
  .unset_conflicting_env()
  on.exit({ invisible(NULL) }, add = TRUE)

  conda_bin <- .ensure_miniconda()
  .configure_conda_forge(conda_bin)
  env_info <- .ensure_asu_env(conda_bin, env_name = "asu-cpsat-venv")
  .use_asu_env(env_info$env_name)

  # expose packaged Python module (inst/python/asu_cpsat.py) if present
  py_dir <- system.file("python", package = "ASUbuildR")
  if (nzchar(py_dir) && dir.exists(py_dir)) {
    reticulate::py_run_string(sprintf(
      "import sys; p=r\"%s\"; sys.path.insert(0, p) if p not in sys.path else None",
      py_dir
    ))
    # try to import once (non-fatal if absent)
    try(reticulate::import_from_path("asu_cpsat", path = py_dir, delay_load = TRUE), silent = TRUE)
  }

  # persist state
  state <- list(
    conda_bin  = normalizePath(conda_bin, mustWork = TRUE),
    conda_root = normalizePath(dirname(dirname(conda_bin)), mustWork = TRUE),
    env_name   = env_info$env_name,
    env_path   = if (!is.na(env_info$env_path)) normalizePath(env_info$env_path, mustWork = FALSE) else NA_character_
  )
  .write_state(state)

  # print versions (no '\\n' escapes)
  reticulate::py_run_string("
import sys, ortools, pandas, numpy
print('Python', sys.version.split()[0])
print('ortools', ortools.__version__)
print('pandas', pandas.__version__)
print('numpy', numpy.__version__)
")

  invisible(TRUE)
}

#' Check whether ASUbuildR Python env is usable
#'
#' @return TRUE if usable, FALSE otherwise
#' @export
check_asu_python <- function() {
  st <- .read_state()
  .unset_conflicting_env()

  if (!is.null(st$env_name)) {
    try(reticulate::use_condaenv(st$env_name, required = TRUE), silent = TRUE)
  }

  ok <- all(
    reticulate::py_module_available("ortools"),
    reticulate::py_module_available("pandas"),
    reticulate::py_module_available("numpy")
  )

  # optional info about asu_cpsat
  py_dir <- system.file("python", package = "ASUbuildR")
  if (nzchar(py_dir) && dir.exists(py_dir)) {
    have_asu <- tryCatch(reticulate::py_module_available("asu_cpsat"), error = function(e) FALSE)
    if (!have_asu) {
      # attempt a non-fatal exposure
      try(reticulate::py_run_string(sprintf(
        "import sys; p=r\"%s\"; sys.path.insert(0, p) if p not in sys.path else None", py_dir
      )), silent = TRUE)
    }
  }

  if (!ok) {
    message("ASUbuildR: Python environment not ready. Run ASUbuildR::setup_asu_python().")
    return(FALSE)
  }
  TRUE
}
