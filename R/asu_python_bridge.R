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
