# Script to run ASU Flexdashboard RMD file
# This script renders and launches the Shiny-based flexdashboard

# Dependencies are imported in the package's NAMESPACE; explicit library
# calls are unnecessary and can cause side effects during package load.

#' Launch ASU Flexdashboard Application
#'
#' This function renders and launches a Shiny-based R Markdown Flexdashboard.
#' The dashboard will automatically open in the default web browser.
#'
#' @param rmd_file Character string specifying the path to the R Markdown file.
#'   If NULL (default), uses the package's built-in dashboard.
#' @param host Character string specifying the host. Default is "127.0.0.1".
#' @param port Numeric port number. If NULL, R will choose an available port.
#' @param launch.browser Logical. Should the dashboard open in browser? Default TRUE.
#'
#' @return This function is called for its side effects (launching the dashboard).
#'   It does not return a value.
#'
#' @examples
#' \dontrun{
#' # Run dashboard with package's built-in file
#' launch_ASUbuildR()
#'
#' # Run dashboard with custom file name
#' launch_ASUbuildR("path/to/MyCustomDashboard.Rmd")
#' }
#'
#' @seealso \code{\link[rmarkdown]{run}} for more details on running Shiny documents
#'
#' @export
launch_ASUbuildR <- function(rmd_file = NULL,
                             host = "127.0.0.1",
                             port = NULL,
                             launch.browser = TRUE) {

  # If no file specified, use the package's built-in dashboard
  if (is.null(rmd_file)) {
    rmd_file <- system.file("shiny_app", "ASU_Flexdashboard_mapgl.Rmd",
                            package = "ASUbuildR")
  }

  if (!file.exists(rmd_file)) {
    stop("Dashboard Rmd not found; reinstall ASUbuildR or provide a valid file path.")
  }

  rmarkdown::run(
    file = rmd_file,
    shiny_args = list(host = host, port = port, launch.browser = launch.browser)
  )
}
