#' Load the ASU CP-SAT Python module
#' @keywords internal
asu_load_py <- function() {
  py <- reticulate::py_config() # force initialize
  path <- system.file("python", "asu_cpsat.py", package = "ASUbuildR")
  if (path == "") stop("Couldn't find inst/python/asu_cpsat.py in the installed package.")
  reticulate::source_python(path)
  # after source_python, Python symbols are in R as functions / classes
  # we return an env-like list of the functions we need
  list(
    build_many_asus_cpsat = get("build_many_asus_cpsat", envir = .GlobalEnv)
  )
}
