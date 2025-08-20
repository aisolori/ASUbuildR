.onLoad <- function(libname, pkgname) {
  ready <- tryCatch(check_asu_python(quiet = TRUE), error = function(e) FALSE)
  options(asu_python_ready = ready)

  cache_dir <- rappdirs::user_cache_dir("ASUbuildR", "tigris")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  options(tigris_cache_dir = cache_dir, tigris_use_cache = TRUE, tigris_class = "sf")
}

.onAttach <- function(libname, pkgname) {
  if (!isTRUE(getOption("asu_python_ready"))) {
    packageStartupMessage("Python environment not ready. Run ASUbuildR::setup_asu_python() to enable CP-SAT features.")
  } else {
    packageStartupMessage("Python environment detected for CP-SAT features.")
  }
}
