#' Setup Python Environment for ASUbuildR
#'
#' This function sets up the Python environment needed for ASUbuildR,
#' including installing Python if necessary and all required packages.
#'
#' @param force Logical. If TRUE, recreates the virtual environment even if it exists.
#' @return Invisible NULL. Called for side effects.
#' @export
#' @examples
#' \dontrun{
#' # First-time setup
#' setup_asu_python()
#'
#' # Force reinstall
#' setup_asu_python(force = TRUE)
#' }
venv_exists <- function(path) {
  file.exists(file.path(path, "pyvenv.cfg"))
}

setup_asu_python <- function(force = FALSE) {

  message("Setting up Python environment for ASUbuildR...")

  app_dir <- rappdirs::user_data_dir("ASUbuildR")
  venv_path <- file.path(app_dir, "asu-cpsat-venv")

  if (dir.exists(venv_path) && (force || !venv_exists(venv_path))) {
    message("Removing existing virtual environment...")
    unlink(venv_path, recursive = TRUE, force = TRUE)
  }

  if (!reticulate::py_available(initialize = FALSE)) {
    message("Python not found. Installing Miniconda...")
    message("This is a one-time installation and may take a few minutes...")

    reticulate::install_miniconda()
    message("Miniconda installed successfully!")
  }

  if (!dir.exists(venv_path)) {
    message("Creating virtual environment...")

    reticulate::virtualenv_create(
      envname = venv_path,
      packages = c("numpy", "pandas", "networkx", "ortools==9.9.3963")
    )

    message("Virtual environment created successfully!")
  } else {
    message("Virtual environment already exists.")

    reticulate::use_virtualenv(venv_path, required = TRUE)

    required <- c("numpy", "pandas", "networkx", "ortools")
    missing <- required[!sapply(required, reticulate::py_module_available)]

    if (length(missing) > 0) {
      message("Installing missing packages: ", paste(missing, collapse = ", "))
      reticulate::py_install(missing, envname = venv_path, pip = TRUE)
    }
  }

  message("\nPython setup complete!")
  message("You can now run launch_ASUbuildR()")

  invisible(NULL)
}

#' Check Python Setup for ASUbuildR
#'
#' Checks if the Python environment is properly configured for ASUbuildR.
#'
#' @return Logical. TRUE if properly configured, FALSE otherwise.
#' @export
check_asu_python <- function() {
  venv_path <- file.path(rappdirs::user_data_dir("ASUbuildR"), "asu-cpsat-venv")

  if (!venv_exists(venv_path)) {
    message("Virtual environment not found or invalid.")
    message("Run setup_asu_python() to set up the Python environment.")
    return(FALSE)
  }

  reticulate::use_virtualenv(venv_path, required = TRUE)

  required <- c("numpy", "pandas", "networkx", "ortools")
  available <- sapply(required, reticulate::py_module_available)

  if (all(available)) {
    message("✓ Python environment is properly configured")
    return(TRUE)
  } else {
    missing <- required[!available]
    message("✗ Missing packages: ", paste(missing, collapse = ", "))
    message("Run setup_asu_python() to install missing packages.")
    return(FALSE)
  }
}
