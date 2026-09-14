#' Select the ASU CP-SAT Python environment
#' @keywords internal
asu_use_python <- function(required = FALSE) {
  conda_bin <- tryCatch(reticulate::conda_binary(), error = function(e) "")
  have_conda <- length(conda_bin) == 1L && !is.na(conda_bin) &&
    nzchar(conda_bin) && file.exists(conda_bin)
  if (!have_conda) {
    if (required) stop("Conda was not found. Run ASUbuildR::setup_asu_python() first.")
    return(FALSE)
  }

  envs_df <- tryCatch(
    reticulate::conda_list(conda = conda_bin),
    error = function(e) NULL
  )
  env_row <- envs_df[envs_df$name == "asu-cpsat", ]
  if (is.null(envs_df) || nrow(env_row) == 0) {
    if (required) stop("Python environment 'asu-cpsat' was not found. Run ASUbuildR::setup_asu_python() first.")
    return(FALSE)
  }

  # Force this interpreter via RETICULATE_PYTHON so a project-local .venv/renv
  # (auto-detected by reticulate) cannot silently override use_condaenv(),
  # even when use_condaenv() is called with required = TRUE.
  Sys.setenv(RETICULATE_PYTHON = env_row$python[1])
  reticulate::use_condaenv("asu-cpsat", conda = conda_bin, required = required)
  TRUE
}

# Check compatibility after the caller has selected its Python interpreter.
# Keep this in the package namespace: build_asu() also runs in fresh R sessions
# where dashboard development helpers have never been sourced.
asu_assert_ortools_version <- function(required = FALSE) {
  problem <- tryCatch({
    ortools <- reticulate::import("ortools", convert = TRUE)
    version <- as.character(ortools[["__version__"]])
    if (length(version) != 1L || is.na(version) || !nzchar(version)) {
      stop("OR-Tools did not report a version.")
    }
    if (utils::compareVersion(version, "9.15") < 0L) {
      paste0("ASUbuildR requires OR-Tools >= 9.15; found ", version, ".")
    } else {
      NULL
    }
  }, error = function(e) {
    paste0("Could not verify the OR-Tools version: ", conditionMessage(e))
  })

  if (is.null(problem)) return(TRUE)
  problem <- paste0(
    problem,
    " Run ASUbuildR::setup_asu_python(force = TRUE), then restart R."
  )
  if (required) stop(problem, call. = FALSE)
  message(problem)
  FALSE
}

#' Load the ASU CP-SAT Python module
#' @keywords internal
asu_load_py <- function() {
  path <- system.file("python", "asu_cpsat.py", package = "ASUbuildR")
  if (path == "") stop("Couldn't find inst/python/asu_cpsat.py in the installed package.")

  module <- reticulate::import_from_path(
    module = "asu_cpsat",
    path = dirname(path),
    convert = TRUE
  )
  if (!reticulate::py_has_attr(module, "build_many_asus_cpsat")) {
    stop("The bundled Python module does not define build_many_asus_cpsat().")
  }

  list(build_many_asus_cpsat = module$build_many_asus_cpsat)
}
