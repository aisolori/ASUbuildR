# ---- helpers ---------------------------------------------------------------

# quiet system2 (hide "CondaKeyError: 'channels' not present" noise)
.run_quiet <- function(cmd, args) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  res <- tryCatch(system2(cmd, args, stdout = tf, stderr = tf), error = function(e) 1L)
  if (is.null(res)) 0L else res
}

# force conda-forge only to avoid Anaconda TOS
.configure_conda_forge <- function(conda_bin) {
  condarc_path <- file.path(Sys.getenv("HOME"), ".condarc")
  writeLines(
    paste(
      "channels:",
      "  - conda-forge",
      "channel_priority: strict",
      "default_channels: []",
      "custom_channels: {}",
      sep = "\n"
    ),
    condarc_path
  )

  .run_quiet(conda_bin, c("config","--remove","channels","defaults"))
  .run_quiet(conda_bin, c("config","--remove","channels","https://repo.anaconda.com/pkgs/main"))
  .run_quiet(conda_bin, c("config","--remove","channels","https://repo.anaconda.com/pkgs/r"))

  # keep these visible (or wrap in .run_quiet if you want them quiet too)
  system2(conda_bin, c("config","--add","channels","conda-forge"), stdout = TRUE, stderr = TRUE)
  system2(conda_bin, c("config","--set","channel_priority","strict"), stdout = TRUE, stderr = TRUE)
  invisible(TRUE)
}

# robust env path via JSON
.conda_env_path <- function(conda_bin, env_name) {
  txt <- suppressWarnings(system2(conda_bin, c("info","--envs","--json"), stdout = TRUE))
  if (!length(txt)) return(NA_character_)
  dat <- tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n")), error = function(e) NULL)
  if (is.null(dat) || is.null(dat$envs)) return(NA_character_)
  cand <- dat$envs[grepl(sprintf("/%s$", env_name), dat$envs)]
  if (length(cand)) cand[[1]] else NA_character_
}

# ---- main API --------------------------------------------------------------

#' Create (or reuse) a Python env for ASUbuildR and install deps (conda only)
#'
#' Installs Python 3.11 and required packages (ortools, numpy, pandas, networkx)
#' into a conda env, then activates it for the current R session.
#' Uses **conda only** (no virtualenv fallback) and forces conda-forge to avoid TOS prompts.
#'
#' @param env_name Character. Name of the environment. Default "asubuildr".
#' @param engine Character. One of "auto", "conda", "virtualenv". If not "conda",
#'   an error is thrown (no virtualenv fallback by design).
#' @return (invisible) env_name
#' @export
asu_py_env_create <- function(env_name = "asubuildr",
                              engine   = c("auto","conda","virtualenv")) {
  engine <- match.arg(engine)

  # We only support conda; block virtualenv by design
  have_conda <- tryCatch(!is.na(reticulate::conda_binary()), error = function(e) FALSE)
  if (engine == "auto") engine <- "conda"
  if (engine != "conda") stop("asu_py_env_create supports conda only (no virtualenv fallback).", call. = FALSE)

  # Ensure conda exists (install Miniconda if needed)
  if (!have_conda) reticulate::install_miniconda()
  conda_bin <- reticulate::conda_binary()
  if (is.na(conda_bin)) stop("Conda not available after install_miniconda().", call. = FALSE)

  # Force conda-forge only (avoid Anaconda TOS)
  .configure_conda_forge(conda_bin)

  # Create env if missing — pass --override-channels to guarantee conda-forge only
  envs <- tryCatch(reticulate::conda_list()$name, error = function(e) character())
  if (!env_name %in% envs) {
    message("• Creating conda env '", env_name, "' (python=3.11)…")
    st <- system2(
      conda_bin,
      c("create","--yes","--name", env_name, "python=3.11",
        "--override-channels","-c","conda-forge"),
      stdout = TRUE, stderr = TRUE
    )
    if (!is.null(attr(st, "status")) && attr(st, "status") != 0L) {
      stop("conda create failed; see logs above.", call. = FALSE)
    }
  } else {
    message("• Reusing existing conda env: ", env_name)
  }

  # Install core packages (conda syntax; no '==')
  pkgs <- c("numpy","pandas","networkx","ortools","pip")
  st2 <- system2(
    conda_bin,
    c("install","--yes","--name", env_name, pkgs,
      "--override-channels","-c","conda-forge"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(st2, "status")) && attr(st2, "status") != 0L) {
    stop("conda install failed; see logs above.", call. = FALSE)
  }

  # Activate env for this session
  reticulate::use_condaenv(env_name, required = TRUE)

  # Verify imports
  if (!asu_py_check(silent = TRUE)) {
    stop("Python environment created but imports failed; see messages above.", call. = FALSE)
  }

  # Optional: print versions
  reticulate::py_run_string(
    "import sys, ortools, pandas, numpy; \
print('Python', sys.version.split()[0]); \
print('ortools', ortools.__version__); \
print('pandas', pandas.__version__); \
print('numpy', numpy.__version__)"
  )

  message("✓ Python env ready: ", env_name,
          " (", .conda_env_path(conda_bin, env_name) %||% "<path unknown>", ")")
  invisible(env_name)
}

#' Activate an existing ASUbuildR Python env for this R session (conda only)
#' @param env_name Character. Environment name.
#' @param engine   Character. Must be "conda" (or "auto", which resolves to conda).
#' @return logical TRUE if success
#' @export
asu_py_use <- function(env_name = "asubuildr", engine = c("auto","conda","virtualenv")) {
  engine <- match.arg(engine)
  if (engine == "auto") engine <- "conda"
  if (engine != "conda") stop("asu_py_use supports conda only.", call. = FALSE)

  reticulate::use_condaenv(env_name, required = TRUE)
  asu_py_check()
}

#' Check Python deps + import the bundled asu_cpsat module
#'
#' @param silent logical: if TRUE, return TRUE/FALSE without messages.
#' @return logical TRUE if all good
#' @export
asu_py_check <- function(silent = FALSE) {
  need <- c("ortools","numpy","pandas","networkx")
  ok <- TRUE

  for (m in need) {
    if (!reticulate::py_module_available(m)) {
      ok <- FALSE
      if (!silent) message("✗ Missing Python module: ", m)
    } else if (!silent) {
      message("✓ Found Python module: ", m)
    }
  }

  # Try importing packaged solver module if present
  py_dir <- system.file("python", package = "ASUbuildR")
  if (nzchar(py_dir) && dir.exists(py_dir)) {
    try({
      reticulate::import_from_path("asu_cpsat", path = py_dir, delay_load = TRUE)
      if (!silent) message("✓ asu_cpsat is importable from: ", py_dir)
    }, silent = silent)
  }

  if (!silent) {
    cfg <- tryCatch(reticulate::py_discover_config(), error = function(e) NULL)
    if (!is.null(cfg)) {
      message(sprintf("• Python: %s (%s)", cfg$python, cfg$version))
      message(sprintf("• Lib:    %s", cfg$libpython))
      message(sprintf("• Env:    %s", cfg$pythonhome %||% "<unknown>"))
    }
  }

  isTRUE(ok)
}

# helper: %||%
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x
