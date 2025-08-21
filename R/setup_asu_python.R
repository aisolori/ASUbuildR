#' Setup Python Environment for ASUbuildR
#'
#' This function sets up the Python environment needed for ASUbuildR
#' using a conda environment from the \emph{conda-forge} channel. It
#' checks for an existing conda installation (installing Miniconda if
#' necessary) and ensures all required packages are available.
#'
#' @param force Logical. If TRUE, recreates the conda environment even if it exists.
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
setup_asu_python <- function(force = FALSE) {

  message("Setting up Python environment for ASUbuildR...")

  env_name <- "asu-cpsat"

  # Determine whether conda is available; if not, install Miniconda
  have_conda <- tryCatch(!is.na(reticulate::conda_binary()), error = function(e) FALSE)
  if (!have_conda) {
    message("Conda not found. Installing Miniconda...")
    message("This is a one-time installation and may take a few minutes...")
    reticulate::install_miniconda()
    have_conda <- TRUE
    message("Miniconda installed successfully!")
  }

  if (!have_conda) {
    stop("Conda installation failed; cannot set up Python environment.")
  }

  # Remove existing environment if force = TRUE
  envs <- reticulate::conda_list()$name
  if (force && env_name %in% envs) {
    message("Removing existing conda environment...")
    reticulate::conda_remove(envname = env_name, packages = "--all")
    envs <- setdiff(envs, env_name)
  }

  # Create conda environment if needed (using conda-forge channel)
  if (!(env_name %in% envs)) {
    message("Creating conda environment '", env_name, "' via conda-forge...")
    reticulate::conda_create(envname = env_name,
                             packages = "python=3.11",
                             forge = TRUE)
    message("Conda environment created successfully!")
  } else {
    message("Conda environment already exists. Use force=TRUE to recreate.")
  }

  # Ensure required packages are installed
  message("Installing required Python packages from conda-forge...")
  reticulate::conda_install(envname = env_name,
                            packages = c("numpy", "pandas",
                                         "networkx", "ortools"),
                            forge = TRUE,
                            pip = FALSE)

  reticulate::use_condaenv(env_name, required = TRUE)

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
  env_name <- "asu-cpsat"

  envs <- reticulate::conda_list()$name
  if (!(env_name %in% envs)) {
    message("Conda environment not found.")
    message("Run setup_asu_python() to set up the Python environment.")
    return(FALSE)
  }

  reticulate::use_condaenv(env_name, required = TRUE)

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
