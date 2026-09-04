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

  envs <- tryCatch(
    reticulate::conda_list(conda = conda_bin)$name,
    error = function(e) character()
  )
  if (!("asu-cpsat" %in% envs)) {
    if (required) stop("Python environment 'asu-cpsat' was not found. Run ASUbuildR::setup_asu_python() first.")
    return(FALSE)
  }

  reticulate::use_condaenv("asu-cpsat", conda = conda_bin, required = required)
  TRUE
}

asu_min_ortools_version <- function() {
  "9.15"
}

asu_get_ortools_version <- function() {
  if (!reticulate::py_module_available("ortools")) return(NA_character_)

  mod <- tryCatch(reticulate::import("ortools", convert = TRUE), error = function(e) NULL)
  version <- ""
  if (!is.null(mod)) {
    version <- tryCatch(as.character(mod$`__version__`), error = function(e) "")
  }
  version <- if (length(version) > 0L) trimws(version[1]) else ""

  if (!nzchar(version)) {
    version <- tryCatch(
      as.character(
        reticulate::py_eval("__import__('importlib.metadata', fromlist=['version']).version('ortools')")
      ),
      error = function(e) ""
    )
    version <- if (length(version) > 0L) trimws(version[1]) else ""
  }

  if (nzchar(version)) version else NA_character_
}

asu_ortools_version_is_supported <- function(version) {
  !is.na(version) && nzchar(version) &&
    utils::compareVersion(version, asu_min_ortools_version()) >= 0L
}

asu_assert_ortools_version <- function(required = TRUE) {
  version <- asu_get_ortools_version()
  if (asu_ortools_version_is_supported(version)) {
    return(invisible(version))
  }

  min_version <- asu_min_ortools_version()
  msg <- if (is.na(version) || !nzchar(version)) {
    paste0(
      "OR-Tools is not installed or its version could not be detected. ",
      "ASUbuildR requires OR-Tools >= ", min_version, ". ",
      "Run ASUbuildR::setup_asu_python()."
    )
  } else {
    paste0(
      "Detected OR-Tools ", version, ", but ASUbuildR requires OR-Tools >= ",
      min_version, ". Run ASUbuildR::setup_asu_python(force = TRUE) to upgrade."
    )
  }

  if (required) stop(msg, call. = FALSE)
  message(msg)
  invisible(FALSE)
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
