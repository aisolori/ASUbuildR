# Render the actual save controls without companion assets, as happens when
# a dashboard is copied independently or an installed package lacks save-as.js.
run_save_script_render_tests <- function() {
  dashboard <- file.path("inst", "shiny_app", "ASU_Flexdashboard_mapgl.Rmd")
  if (!file.exists(dashboard)) dashboard <- system.file("shiny_app", "ASU_Flexdashboard_mapgl.Rmd", package = "ASUbuildR")
  lines <- readLines(dashboard, warn = FALSE, encoding = "UTF-8")
  script_start <- which(lines == '<script id="asu-save-as">')
  stopifnot(length(script_start) == 1L)
  script_end <- min(which(lines == "</script>" & seq_along(lines) > script_start))
  controls_start <- which(lines == "# Save and Load Data")
  controls_end <- which(lines == "# ASU Review and Finalization") - 1L
  stopifnot(length(controls_start) == 1L, length(controls_end) == 1L)
  # Keep the production YAML, save script, and entire save-controls section.
  yaml_end <- which(lines == "---")[2L]
  fixture <- c(lines[seq_len(yaml_end)], "", lines[script_start:script_end], "",
               lines[controls_start:controls_end])

  if (file.exists(file.path("R", "launch_ASUbuildR.R"))) {
    source(file.path("R", "launch_ASUbuildR.R"), local = TRUE)
    asu_ensure_pandoc()
  } else {
    getFromNamespace("asu_ensure_pandoc", "ASUbuildR")()
  }
  folder <- tempfile("asu-save-render-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  for (layout in c("standalone", "library/ASUbuildR/shiny_app")) {
    location <- file.path(folder, layout)
    dir.create(location, recursive = TRUE)
    input_file <- file.path(location, "ASU_Flexdashboard_mapgl.Rmd")
    writeLines(fixture, input_file, useBytes = TRUE)
    stopifnot(!file.exists(file.path(location, "save-as.js")))
    shiny::testServer(function(input, output, session) {
      full_data <- shiny::reactiveVal(data.frame(GEOID = "01001000100", asunum = 1L))
      rendered <- rmarkdown::render(input_file, envir = environment(), quiet = TRUE)
      html <- paste(readLines(rendered, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      stopifnot(grepl('<script id="asu-save-as">', html, fixed = TRUE),
                grepl("window.showSaveFilePicker", html, fixed = TRUE),
                grepl('data-asu-save-name="saved_data.rds"', html, fixed = TRUE))
    }, {})
  }
  cat("Save controls render without companion scripts in standalone and package layouts.\n")
}
run_save_script_render_tests()
