# Script to run ASU Flexdashboard RMD file
# This script renders and launches the Shiny-based flexdashboard

# Load required libraries
library(rmarkdown)
library(flexdashboard)
library(shiny)

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
    # Try installed package location first
    rmd_file <- system.file("shiny_app", "ASU_Flexdashboard_mapgl.Rmd",
                            package = "ASUbuildR")  # Replace with your actual package name

    # If not found (development mode), try local inst directory
    if (rmd_file == "" || !file.exists(rmd_file)) {
      # Look for the file in development directory structure
      dev_file <- file.path("inst", "shiny_app", "ASU_Flexdashboard_mapgl.Rmd")
      if (file.exists(dev_file)) {
        rmd_file <- dev_file
      } else {
        # Try alternative development paths
        alt_paths <- c(
          "inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd",  # Your original name
          #"R/ASU_Flexdashboard_mapgl.Rmd",  # Current location
          "ASU_Flexdashboard_mapgl.Rmd"     # Working directory
        )

        for (path in alt_paths) {
          if (file.exists(path)) {
            rmd_file <- path
            break
          }
        }
      }
    }
  }

  # Check if the RMD file exists
  if (!file.exists(rmd_file)) {
    stop(paste("File", rmd_file, "not found. Please check the file path or ensure the .Rmd file is in the correct location."))
  }

  # Print status message
  cat("Starting ASU Flexdashboard...\n")
  cat("File:", rmd_file, "\n")

  # Run the flexdashboard with shiny
  # This will render and launch the dashboard in the browser
  rmarkdown::run(
    file = rmd_file,
    shiny_args = list(
      host = host,
      port = port,
      launch.browser = launch.browser
    )
  )
}
