.onLoad <- function(libname, pkgname) {
  options(asu_python_env = asu_venv_path(), asu_python_ready = FALSE)

  cache_dir <- rappdirs::user_cache_dir("ASUbuildR", "tigris")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  options(tigris_cache_dir = cache_dir, tigris_use_cache = TRUE, tigris_class = "sf")
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    ready <- tryCatch(check_asu_python(quiet = TRUE), error = function(e) FALSE)
    options(asu_python_ready = ready)
  }

  if (!isTRUE(getOption("asu_python_ready"))) {
    packageStartupMessage("Python environment not ready. Run ASUbuildR::setup_asu_python() to enable CP-SAT features.")
  } else {
    packageStartupMessage("Python environment detected for CP-SAT features.")
  }
}
