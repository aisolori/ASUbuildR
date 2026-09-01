#' Setup Python Environment for ASUbuildR
#'
#' This function sets up the Python environment needed for ASUbuildR. It
#' checks for an existing conda installation (installing Miniconda if
#' necessary), creates a conda environment using only the \emph{conda-forge}
#' channel for the Python interpreter itself (avoiding channels that require
#' Terms of Service acceptance), and installs the required Python packages
#' (\code{numpy}, \code{pandas}, \code{networkx}, \code{ortools},
#' \code{openpyxl}) via pip inside that environment. OR-Tools is not
#' distributed on conda-forge, so
#' pip is used for the packages rather than \code{conda install}.
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

  # Run a conda subcommand and stop with an informative error if it fails,
  # instead of silently continuing as if the step had succeeded.
  run_conda_step <- function(args, step_label) {
    status <- system2(conda_bin, args)
    if (!identical(status, 0L)) {
      stop(
        "Failed to ", step_label, " (conda exit status: ", status, ").\n",
        "Command: ", conda_bin, " ", paste(args, collapse = " "), "\n",
        "See the conda output above for details. Common causes: no internet ",
        "connection, a proxy blocking conda-forge, or insufficient disk space.",
        call. = FALSE
      )
    }
    invisible(status)
  }

  # Determine whether conda is available; if not, install Miniconda
  conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) "")
  have_conda <- length(conda_bin) == 1L && !is.na(conda_bin) &&
    nzchar(conda_bin) && file.exists(conda_bin)
  just_installed <- FALSE
  if (!have_conda) {
    message("Conda not found. Installing Miniconda...")
    message("This is a one-time installation and may take a few minutes...")

    # Some networks block one installer host (e.g. GitHub release assets)
    # but allow another (e.g. repo.anaconda.com), or vice versa. Retry with
    # alternate hosts before giving up, since reticulate's default choice of
    # host varies by platform and package version.
    arch <- if (.Platform$OS.type == "windows") "x86_64" else NA_character_
    fallback_urls <- if (.Platform$OS.type == "windows") {
      c(
        sprintf("https://repo.anaconda.com/miniconda/Miniconda3-latest-Windows-%s.exe", arch),
        sprintf("https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Windows-%s.exe", arch)
      )
    } else {
      character(0)
    }

    install_attempt <- function(url_override = NULL) {
      opt_name <- "reticulate.miniconda.url"
      old <- getOption(opt_name)
      on.exit(options(stats::setNames(list(old), opt_name)), add = TRUE)
      if (!is.null(url_override)) options(stats::setNames(list(url_override), opt_name))
      reticulate::install_miniconda(update = FALSE)
    }

    candidates <- c(list(NULL), as.list(fallback_urls))
    last_error <- NULL
    installed <- FALSE
    for (url in candidates) {
      result <- tryCatch({ install_attempt(url); TRUE },
                          error = function(e) { last_error <<- e; FALSE })
      if (isTRUE(result)) { installed <- TRUE; break }
    }

    if (!installed) {
      stop(
        "Could not download or install a conda distribution from any known host.\n",
        "Original error: ", conditionMessage(last_error), "\n\n",
        "Installer hosts tried include github.com/release-assets.githubusercontent.com ",
        "and repo.anaconda.com. If your organization requires a proxy, configure ",
        "HTTPS_PROXY (and HTTP_PROXY when required) before running ",
        "setup_asu_python() again.\n\n",
        "Alternatively, ask IT to install Miniforge for your user account ",
        "from https://github.com/conda-forge/miniforge, restart R, and run ",
        "setup_asu_python() again.",
        call. = FALSE
      )
    }
    have_conda <- TRUE
    just_installed <- TRUE
    message("Miniconda installed successfully!")
  }

  if (!have_conda) {
    stop("Conda installation failed; cannot set up Python environment.")
  }

  # Path to conda executable (re-resolved in case Miniconda was just installed)
  conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) "")
  if (length(conda_bin) != 1L || is.na(conda_bin) || !nzchar(conda_bin) ||
      !file.exists(conda_bin)) {
    stop("Miniconda installation did not provide a usable conda executable.",
         call. = FALSE)
  }

  # Remove existing environment if force = TRUE
  envs <- reticulate::conda_list()$name
  if (force && env_name %in% envs) {
    message("Removing existing conda environment...")
    run_conda_step(
      c("remove", "--yes", "--name", env_name, "--all",
        "--override-channels", "-c", "conda-forge"),
      paste0("remove the existing '", env_name, "' conda environment")
    )
    envs <- setdiff(envs, env_name)
  }

  # Ensure base conda uses only conda-forge when freshly installed
  if (just_installed) {
    run_conda_step(
      c("update", "--name", "base", "conda", "--yes",
        "--quiet", "--override-channels", "-c", "conda-forge"),
      "update the base conda installation"
    )
  }

  # Create conda environment if needed (using only the conda-forge channel)
  if (!(env_name %in% envs)) {
    message("Creating conda environment '", env_name, "' via conda-forge...")
    run_conda_step(
      c("create", "--yes", "--name", env_name, "python=3.11",
        "--quiet", "--override-channels", "-c", "conda-forge"),
      paste0("create the '", env_name, "' conda environment")
    )
    message("Conda environment created successfully!")
  } else {
    message("Conda environment already exists. Use force=TRUE to recreate.")
  }

  # Locate Python by full path to avoid ambiguity when multiple conda
  # installations are present. This also bypasses reticulate's one-time-init
  # constraint entirely.
  envs_df     <- reticulate::conda_list()
  env_row     <- envs_df[envs_df$name == env_name, ]
  if (nrow(env_row) == 0)
    stop("Conda environment '", env_name, "' not found after creation.", call. = FALSE)
  python_path <- env_row$python[1]

  required_imports <- c(
    numpy = "numpy",
    pandas = "pandas",
    networkx = "networkx",
    ortools = "ortools.sat.python.cp_model",
    openpyxl = "openpyxl"
  )
  required_imports_py <- paste0(
    "{",
    paste0(
      '"', names(required_imports), '": "', required_imports, '"',
      collapse = ", "
    ),
    "}"
  )

  # Write Python snippets to temp files to avoid Windows cmd.exe quoting issues.
  check_file  <- tempfile(fileext = ".py")
  verify_file <- tempfile(fileext = ".py")
  on.exit(unlink(c(check_file, verify_file), force = TRUE), add = TRUE)

  writeLines(c(
    "import importlib",
    paste0("required = ", required_imports_py),
    "missing = []",
    "for package, module in required.items():",
    "    try:",
    "        importlib.import_module(module)",
    "    except Exception:",
    "        missing.append(package)",
    'print("\\n".join(missing))'
  ), check_file)

  raw          <- system2(python_path, args = check_file, stdout = TRUE, stderr = FALSE)
  missing_pkgs <- raw[nzchar(trimws(raw))]

  if (length(missing_pkgs) == 0) {
    message("All required Python packages are already installed.")
  } else {
    message("Installing or repairing Python packages via pip: ",
            paste(missing_pkgs, collapse = ", "), "...")
    status <- system2(
      python_path,
      args = c(
        "-m", "pip", "install", "--quiet", "--isolated", "--upgrade",
        "--force-reinstall", "--no-cache-dir", missing_pkgs
      )
    )
    if (!identical(status, 0L))
      stop("pip install failed (exit status: ", status, "). ",
           "See the output above for details.", call. = FALSE)
  }

  writeLines(c(
    "import importlib",
    paste0("required = ", required_imports_py),
    "errors = []",
    "for package, module in required.items():",
    "    try:",
    "        importlib.import_module(module)",
    "    except Exception as exc:",
    "        errors.append(f'{package}: {type(exc).__name__}: {exc}')",
    'print("\\n".join(errors) if errors else "OK")'
  ), verify_file)

  verify <- trimws(system2(python_path, args = verify_file, stdout = TRUE, stderr = FALSE))
  if (!identical(verify, "OK"))
    stop(
      "Required Python modules are still not importable:\n",
      paste(verify, collapse = "\n"),
      ". Try setup_asu_python(force = TRUE) to recreate the environment.",
      call. = FALSE
    )

  message("\nPython setup complete! You can now run launch_ASUbuildR()")


}

#' Check Python Setup for ASUbuildR
#'
#' Checks if the Python environment is properly configured for ASUbuildR.
#'
#' @return Logical. TRUE if properly configured, FALSE otherwise.
#' @export
check_asu_python <- function() {
  if (!asu_use_python(required = FALSE)) {
    message("Conda environment not found.")
    message("Run setup_asu_python() to set up the Python environment.")
    return(FALSE)
  }

  required <- c(
    numpy = "numpy",
    pandas = "pandas",
    networkx = "networkx",
    ortools = "ortools.sat.python.cp_model",
    openpyxl = "openpyxl"
  )
  available <- vapply(
    unname(required), reticulate::py_module_available, logical(1)
  )

  if (all(available)) {
    message("Python environment is properly configured")
    return(TRUE)
  } else {
    missing <- names(required)[!available]
    message("Missing packages: ", paste(missing, collapse = ", "))
    message("Run setup_asu_python() to install missing packages.")
    return(FALSE)
  }
}
