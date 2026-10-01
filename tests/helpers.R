# Helpers for running the same tests from a checkout or R CMD check.
asu_test_asset <- function(path) {
  local <- file.path("inst", path)
  if (file.exists(local)) return(local)
  installed <- system.file(path, package = "ASUbuildR", mustWork = TRUE)
  installed
}

asu_test_source <- function(path, envir = parent.frame()) {
  if (file.exists(path)) {
    sys.source(path, envir = envir, keep.source = TRUE)
  } else {
    # Copy internal helpers into the test environment so local stubs continue
    # to work without modifying the installed package's locked namespace.
    ns <- asNamespace("ASUbuildR")
    for (name in ls(ns, all.names = TRUE)) {
      value <- get(name, envir = ns, inherits = FALSE)
      if (is.function(value) && identical(environment(value), ns)) {
        environment(value) <- envir
        assign(name, value, envir = envir)
      }
    }
  }
  invisible(NULL)
}
