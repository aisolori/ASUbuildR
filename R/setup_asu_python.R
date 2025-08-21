# Internal helpers for managing ASUbuildR's Python configuration -----------------

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
  if (!file.exists(conda_bin)) {
    stop("Miniconda install reported success but conda not found.")
  }
  conda_bin
}

#' @keywords internal
.configure_conda_forge <- function(conda_bin) {
  condarc_path <- file.path(Sys.getenv("HOME"), ".condarc")
  condarc <- paste(
    "channels:",
    "  - conda-forge",
    "channel_priority: strict",
    "default_channels: []",
    "custom_channels: {}",
    sep = "\n"
  )
  writeLines(condarc, condarc_path)
  suppressWarnings(try(system2(conda_bin, c("config", "--remove", "channels", "defaults"))))
  suppressWarnings(try(system2(conda_bin, c("config", "--remove", "channels", "https://repo.anaconda.com/pkgs/main"))))
  suppressWarnings(try(system2(conda_bin, c("config", "--remove", "channels", "https://repo.anaconda.com/pkgs/r"))))
  system2(conda_bin, c("config", "--add", "channels", "conda-forge"), stdout = TRUE, stderr = TRUE)
  system2(conda_bin, c("config", "--set", "channel_priority", "strict"), stdout = TRUE, stderr = TRUE)
  invisible(TRUE)
}

#' @keywords internal
.ensure_asu_env <- function(conda_bin, env_name = "asu-cpsat-venv") {
  out <- suppressWarnings(system2(conda_bin, c("env", "list"), stdout = TRUE))
  has_env <- any(grepl(sprintf("^%s\\s", env_name), out)) ||
    any(grepl(sprintf("/%s$", env_name), out))

  if (!has_env) {
    message(sprintf("Creating Conda env '%s' with Python 3.10...", env_name))
    system2(
      conda_bin,
      c(
        "create", "--yes", "--name", env_name, "python=3.10",
        "--override-channels", "-c", "conda-forge"
      ),
      stdout = TRUE, stderr = TRUE
    )
  } else {
    message(sprintf("Conda env '%s' already exists.", env_name))
  }

  system2(
    conda_bin,
    c(
      "install", "--yes", "--name", env_name,
      "protobuf==4.25.3", "absl-py", "immutabledict",
      "numpy", "pandas", "networkx", "ortools==9.9.3963", "pip",
      "--override-channels", "-c", "conda-forge"
    ),
    stdout = TRUE, stderr = TRUE
  )

  info <- suppressWarnings(system2(conda_bin, c("info", "--envs"), stdout = TRUE))
  env_path <- NA_character_
  if (length(info)) {
    line <- info[grepl(sprintf("\\s%s\\s", env_name), info)]
    if (length(line)) {
      toks <- strsplit(trimws(line), "\s+")[[1]]
      env_path <- toks[length(toks)]
    }
  }
  list(env_name = env_name, env_path = env_path)
}


#' @keywords internal
.use_asu_env <- function(env_name = "asu-cpsat-venv") {
  reticulate::use_condaenv(env_name, required = TRUE)
  ok1 <- reticulate::py_module_available("ortools")
  ok2 <- reticulate::py_module_available("pandas")
  ok3 <- reticulate::py_module_available("numpy")
  if (!all(ok1, ok2, ok3)) {
    stop("Conda env is active, but required modules are missing.")
  }
  invisible(TRUE)
}


# Public functions -------------------------------------------------------------

#' Set up Python for ASUbuildR (Conda + conda-forge + env creation)
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

  state <- list(
    conda_bin  = normalizePath(conda_bin, mustWork = TRUE),
    conda_root = normalizePath(dirname(dirname(conda_bin)), mustWork = TRUE),
    env_name   = env_info$env_name,
    env_path   = if (!is.na(env_info$env_path)) normalizePath(env_info$env_path, mustWork = FALSE) else NA_character_
  )
  .write_state(state)

  reticulate::py_run_string("import sys, ortools, pandas, numpy; \\nprint('Python', sys.version.split()[0]); \\nprint('ortools', ortools.__version__); \\nprint('pandas', pandas.__version__); \\nprint('numpy', numpy.__version__)")

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
  if (!ok) {
    message("ASUbuildR: Python environment not ready. Run ASUbuildR::setup_asu_python().")
    return(FALSE)
  }
  TRUE
}

