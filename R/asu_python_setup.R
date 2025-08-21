#' Create (or reuse) a Python env for ASUbuildR and install deps
#'
#' Installs Python 3.11 and required packages (ortools, numpy, pandas, networkx),
#' then activates the environment for the current R session. Uses conda via the
#' conda-forge channel by default when available and falls back to virtualenv.
#'
#' @param env_name Character. Name of the environment. Default "asubuildr".
#' @param engine Character. One of "auto", "conda", "virtualenv".
#' @return (invisible) env_name
#' @export
asu_py_env_create <- function(env_name = "asubuildr",
                              engine = c("auto", "conda", "virtualenv")) {
  engine <- match.arg(engine)
  pkgs   <- c("ortools", "numpy", "pandas", "networkx")

  # Decide engine
  have_conda <- tryCatch(!is.na(reticulate::conda_binary()), error = function(e) FALSE)
  if (engine == "auto") {
    engine <- if (have_conda) "conda" else "virtualenv"
  }

  message("• Using engine: ", engine)

  if (engine == "conda") {
    # Ensure Miniconda exists; install if needed
    reticulate::miniconda_path()  # triggers bootstrap if missing

    # Create env with Python 3.11 if it doesn't exist
    envs <- reticulate::conda_list()$name
    if (!env_name %in% envs) {
      message("• Creating conda env '", env_name, "' (python=3.11, channel conda-forge)…")
      reticulate::conda_create(envname = env_name,
                               packages = "python=3.11",
                               forge = TRUE)
    } else {
      message("• Reusing existing conda env: ", env_name)
    }

    # Prefer conda-forge for core packages; pip fallback if needed
    message("• Installing core packages via conda-forge…")
    tryCatch(
      reticulate::conda_install(envname = env_name,
                                packages = pkgs,
                                forge = TRUE,
                                pip = FALSE),
      error = function(e) {
        message("  ↪ conda install had an issue (", e$message, "). Falling back to pip…")
        reticulate::py_install(pkgs, envname = env_name, method = "conda", pip = TRUE)
      }
    )

    # Activate in this session
    reticulate::use_condaenv(env_name, required = TRUE)

  } else {
    # virtualenv path + Python
    # Install a session-local Python 3.11 if needed
    py <- reticulate::install_python(version = "3.11")
    venvs <- tryCatch(reticulate::virtualenv_list(), error = function(e) character())
    if (!env_name %in% venvs) {
      message("• Creating virtualenv '", env_name, "' (python=3.11)…")
      reticulate::virtualenv_create(envname = env_name, python = py)
    } else {
      message("• Reusing existing virtualenv: ", env_name)
    }

    # Activate
    reticulate::use_virtualenv(env_name, required = TRUE)

    # Install via pip
    message("• Installing core packages via pip…")
    reticulate::py_install(pkgs, envname = env_name, method = "virtualenv", pip = TRUE)
  }

  # Verify imports (Python side)
  ok <- asu_py_check(silent = TRUE)
  if (!ok) stop("Python environment created but imports failed; see messages above.", call. = FALSE)

  message("✓ Python env ready: ", env_name)
  invisible(env_name)
}

#' Activate an existing ASUbuildR Python env for this R session
#' @param env_name Character. Environment name.
#' @param engine   Character. One of "auto","conda","virtualenv"
#' @return logical TRUE if success
#' @export
asu_py_use <- function(env_name = "asubuildr", engine = c("auto","conda","virtualenv")) {
  engine <- match.arg(engine)
  have_conda <- tryCatch(!is.na(reticulate::conda_binary()), error = function(e) FALSE)
  if (engine == "auto") engine <- if (have_conda) "conda" else "virtualenv"

  if (engine == "conda") {
    reticulate::use_condaenv(env_name, required = TRUE)
  } else {
    reticulate::use_virtualenv(env_name, required = TRUE)
  }

  asu_py_check()
}

#' Check Python deps + import the bundled asu_cpsat module
#'
#' @param silent logical: if TRUE, return TRUE/FALSE without messages.
#' @return logical TRUE if all good
#' @export
asu_py_check <- function(silent = FALSE) {
  need <- c("ortools", "numpy", "pandas", "networkx")
  ok <- TRUE

  # Check core modules
  for (m in need) {
    if (!reticulate::py_module_available(m)) {
      ok <- FALSE
      if (!silent) message("✗ Missing Python module: ", m)
    } else if (!silent) {
      message("✓ Found Python module: ", m)
    }
  }

  # Try importing our packaged solver module
  # (inst/python/asu_cpsat.py via system.file)
  py_dir <- system.file("python", package = "ASUbuildR")
  if (nzchar(py_dir) && dir.exists(py_dir)) {
    try({
      reticulate::import_from_path("asu_cpsat", path = py_dir, delay_load = TRUE)
      if (!silent) message("✓ asu_cpsat is importable from: ", py_dir)
    }, silent = silent)
  } else {
    ok <- FALSE
    if (!silent) message("✗ Could not locate packaged python dir (system.file('python', 'ASUbuildR'))")
  }

  if (!silent) {
    cfg <- tryCatch(reticulate::py_discover_config(), error = function(e) NULL)
    if (!is.null(cfg)) {
      message(sprintf("• Python: %s (%s)", cfg$python, cfg$version))
      message(sprintf("• Lib:    %s", cfg$libpython))
      message(sprintf("• Env:    %s", cfg$pythonhome %||% "<unknown>"))
    }
  }

  isTRUE(ok)
}

# small helper: %||%
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x
