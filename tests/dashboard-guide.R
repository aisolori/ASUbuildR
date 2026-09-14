# Exercise the installed Help-tab renderer during R CMD check, including its
# runtime dependencies. No Python environment or interactive browser is needed.
guide <- system.file("shiny_app", "dashboard-guide.md", package = "ASUbuildR")
stopifnot(nzchar(guide), file.exists(guide))
html <- as.character(shiny::includeMarkdown(guide))
stopifnot(
  grepl("Quick start", html, fixed = TRUE),
  grepl("<table", html, fixed = TRUE),
  grepl("CP-SAT parameters", html, fixed = TRUE)
)
