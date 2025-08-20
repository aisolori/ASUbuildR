#' Setup Python Environment for ASUbuildR
#'
#' Creates or repairs the Python virtual environment used by ASUbuildR. If a
#' suitable Python installation is not found, the function will install
#' Miniconda and create an isolated virtual environment called
#' `asu-cpsat-venv`. Required Python packages (``ortools``, ``pandas``,
#' ``numpy``, ``setuptools`` and ``wheel``) are installed using ``conda`` when
#' available and fall back to ``pip`` otherwise. When automatic installation
#' fails, the user is given instructions to complete the setup manually.
#'
#' @param force Recreate the virtual environment even if it already exists.
#' @return Logical. ``TRUE`` if the environment is ready, ``FALSE`` otherwise.
#' @export
setup_asu_python <- function(force = FALSE) {
  message("Setting up Python environment for ASUbuildR...")

  app_dir  <- rappdirs::user_data_dir("ASUbuildR")
  venv_path <- file.path(app_dir, "asu-cpsat-venv")
  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

  if (force && reticulate::virtualenv_exists(venv_path)) {
    message("Removing existing virtual environment...")
    unlink(venv_path, recursive = TRUE, force = TRUE)
  }

  # Ensure a Python binary is available ------------------------------------------------
  miniconda_path <- reticulate::miniconda_path()
  miniconda_exists <- dir.exists(miniconda_path) && file.exists(miniconda_path)
  if (!reticulate::py_available(initialize = FALSE) || force) {
    if (!miniconda_exists || force) {
      message("Installing Miniconda (one-time operation)...")
      tryCatch(reticulate::install_miniconda(force = force),
               error = function(e) stop("Miniconda installation failed: ", e$message))
    }
  }

  # Determine Python binary ---------------------------------------------------
  py_bin <- Sys.which("python")
  if (py_bin == "") py_bin <- Sys.which("python3")
  if (py_bin == "") {
    py_bin <- if (.Platform$OS.type == "windows") {
      file.path(miniconda_path, "python.exe")
    } else {
      file.path(miniconda_path, "bin", "python")
    }
  }

  # Create virtual environment ---------------------------------------------------------
  if (!reticulate::virtualenv_exists(venv_path)) {
    message("Creating virtual environment at ", venv_path)
    reticulate::virtualenv_create(envname = venv_path, python = py_bin)
  } else {
    message("Virtual environment already exists")
  }

  # Install required packages ----------------------------------------------------------
  pkgs <- c("ortools", "pandas", "numpy", "setuptools", "wheel")
  ok <- FALSE
  try({
    reticulate::py_install(pkgs, envname = venv_path, method = "conda")
    ok <- TRUE
  }, silent = TRUE)
  if (!ok) {
    message("Conda installation failed or unavailable; trying pip...")
    try({
      reticulate::py_install(pkgs, envname = venv_path, method = "pip")
      ok <- TRUE
    }, silent = TRUE)
  }
  if (!ok) {
    message("Automatic installation failed. Please install these packages in",
            " the environment located at ", venv_path, ":",
            paste(pkgs, collapse = ", "))
    return(FALSE)
  }

  message("Python environment is ready.")
  TRUE
}

#' Check Python Setup for ASUbuildR
#'
#' Verifies that the ASUbuildR Python virtual environment exists and that the
#' required modules can be imported. The custom ``asu_cpsat`` module bundled with
#' the package is also checked. The result is stored in
#' ``options(asu_python_ready)`` for later use.
#'
#' @param quiet Suppress status messages.
#' @return Logical ``TRUE`` if the environment is ready, ``FALSE`` otherwise.
#' @export
check_asu_python <- function(quiet = FALSE) {
  msg <- function(...) if (!quiet) message(...)

  venv_path <- file.path(rappdirs::user_data_dir("ASUbuildR"), "asu-cpsat-venv")
  if (!reticulate::virtualenv_exists(venv_path)) {
    msg("Virtual environment not found: run setup_asu_python()")
    options(asu_python_ready = FALSE)
    return(FALSE)
  }

  ok <- tryCatch({
    reticulate::use_virtualenv(venv_path, required = TRUE)
    TRUE
  }, error = function(e) {
    msg("Failed to activate virtual environment: ", e$message)
    FALSE
  })
  if (!ok) {
    options(asu_python_ready = FALSE)
    return(FALSE)
  }

  required <- c("ortools", "pandas", "numpy", "setuptools", "wheel")
  missing <- required[!vapply(required, reticulate::py_module_available, logical(1))]
  if (length(missing)) {
    msg("Missing Python packages: ", paste(missing, collapse = ", "))
    options(asu_python_ready = FALSE)
    return(FALSE)
  }

  asu_path <- system.file("python", "asu_cpsat.py", package = "ASUbuildR")
  if (asu_path == "" || !file.exists(asu_path)) {
    msg("ASU CP-SAT module not found in package.")
    options(asu_python_ready = FALSE)
    return(FALSE)
  }
  loaded <- tryCatch({
    reticulate::source_python(asu_path, envir = new.env(parent = emptyenv()))
    TRUE
  }, error = function(e) {
    msg("Unable to load asu_cpsat module: ", e$message)
    FALSE
  })
  if (!loaded) {
    options(asu_python_ready = FALSE)
    return(FALSE)
  }

  options(asu_python_ready = TRUE)
  msg("\u2713 Python environment is ready")
  TRUE
}
