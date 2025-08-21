asu_venv_path <- function() {
  file.path(rappdirs::user_data_dir("ASUbuildR"), "asu-cpsat-venv")
}

#' Setup Python Environment for ASUbuildR
#'
#' Creates or repairs the Python environment used by ASUbuildR. The function
#' installs Miniconda if necessary and then attempts to create a conda
#' environment at ``asu-cpsat-venv`` containing the required Python packages
#' (``ortools``, ``pandas``, ``numpy``, ``setuptools`` and ``wheel``). If the
#' conda-based setup fails, the user is advised to install the packages
#' manually. The final environment location is stored in
#' ``options('asu_python_env')`` for later use.
#'
#' @param force Recreate the environment even if it already exists.
#' @return Logical. ``TRUE`` if the environment is ready, ``FALSE`` otherwise.
#' @export
setup_asu_python <- function(force = FALSE) {
  message("Setting up Python environment for ASUbuildR...")

  venv_path <- asu_venv_path()
  dir.create(dirname(venv_path), showWarnings = FALSE, recursive = TRUE)

  if (force && dir.exists(venv_path)) {
    message("Removing existing environment...")
    unlink(venv_path, recursive = TRUE, force = TRUE)
  }

  # Ensure a Python binary is available --------------------------------------
  py_bin <- Sys.which("python")
  if (py_bin == "") py_bin <- Sys.which("python3")
  if (py_bin == "") {
    mc_path <- reticulate::miniconda_path()
    if (!dir.exists(mc_path) || force) {
      message("Installing Miniconda (one-time operation)...")
      tryCatch(reticulate::install_miniconda(force = force),
               error = function(e) stop("Miniconda installation failed: ", e$message))
    }
    py_bin <- if (.Platform$OS.type == "windows") {
      file.path(mc_path, "python.exe")
    } else {
      file.path(mc_path, "bin", "python")
    }
  }

  pkgs <- c("ortools", "pandas", "numpy", "setuptools", "wheel")
  conda <- tryCatch(reticulate::conda_binary(), error = function(e) "")

  if (!nzchar(conda)) {
    message("Conda not available. Ensure reticulate can locate conda.")
    options(asu_python_env = venv_path, asu_python_ready = FALSE)
    return(FALSE)
  }

  if (!dir.exists(venv_path) || force) {
    if (dir.exists(venv_path) && force) {
      message("Removing existing environment...")
      unlink(venv_path, recursive = TRUE, force = TRUE)
    }
    message("Creating conda environment at ", venv_path)
    created <- tryCatch({
      reticulate::conda_create(venv_path, packages = pkgs,
                               forge = TRUE, channel = "conda-forge",
                               conda = conda)
      TRUE
    }, error = function(e) {
      message("Automatic installation failed. Please install these packages in",
              " the environment located at ", venv_path, ":",
              paste(pkgs, collapse = ", "))
      FALSE
    })
    if (!created) {
      options(asu_python_env = venv_path, asu_python_ready = FALSE)
      return(FALSE)
    }
  } else {
    message("Conda environment already exists; ensuring packages are installed")
    installed <- tryCatch({
      reticulate::conda_install(venv_path, pkgs,
                                channel = "conda-forge", forge = TRUE,
                                conda = conda)
      TRUE
    }, error = function(e) {
      message("Package installation failed: ", e$message)
      FALSE
    })
    if (!installed) {
      options(asu_python_env = venv_path, asu_python_ready = FALSE)
      return(FALSE)
    }
  }

  options(asu_python_env = venv_path)
  res <- check_asu_python(quiet = TRUE)
  if (res) message("Python environment is ready.")
  res
}

#' Check Python Setup for ASUbuildR
#'
#' Verifies that the ASUbuildR Python environment exists and that the
#' required modules can be imported. The custom ``asu_cpsat`` module bundled with
#' the package is also checked. The result is stored in
#' ``options(asu_python_ready)`` for later use.
#'
#' @param quiet Suppress status messages.
#' @return Logical ``TRUE`` if the environment is ready, ``FALSE`` otherwise.
#' @export
check_asu_python <- function(quiet = FALSE) {
  msg <- function(...) if (!quiet) message(...)

  venv_path <- getOption("asu_python_env", asu_venv_path())
  if (!dir.exists(venv_path)) {
    msg("Python environment not found: run setup_asu_python()")
    options(asu_python_ready = FALSE)
    return(FALSE)
  }

  ok <- try({
    reticulate::use_condaenv(venv_path, required = TRUE)
    TRUE
  }, silent = TRUE)

  if (!isTRUE(ok)) {
    msg("Failed to activate Python environment at ", venv_path)
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
