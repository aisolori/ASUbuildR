run_national_import_tests <- function() {
  dashboard <- file.path("inst", "shiny_app", "ASU_Flexdashboard_mapgl.Rmd")
  if (!file.exists(dashboard)) {
    dashboard <- system.file(
      "shiny_app", "ASU_Flexdashboard_mapgl.Rmd", package = "ASUbuildR"
    )
  }
  extracted <- tempfile(fileext = ".R")
  on.exit(unlink(extracted), add = TRUE)
  knitr::purl(dashboard, output = extracted, quiet = TRUE)
  parse(extracted)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    cat("Dashboard R syntax passed; SKIP workbook fixture: writexl unavailable.\n")
    return(invisible(NULL))
  }
  if (file.exists("R/browser_data.R")) {
    source("R/browser_data.R", local = TRUE)
  } else {
    asu_read_upload <- getFromNamespace("asu_read_upload", "ASUbuildR")
  }
  columns <- c("record", "geoid", "st_fips", "cnty_fips", "tract_fips", "name",
    "tract_pop2020", "tract_pop2025", "tract_emp", "tract_unemp", "tract_urate",
    "tract_urate_error", "cnty_pop_dec", "cnty_pop_cur", "cnty_emp", "cnty_unemp",
    "cnty_urate", "cnty_urate_error", "pop_shr", "emp_shr", "unemp_shr",
    "laus_primary", "cnty_emp_ave", "cnty_unemp_ave", "tract_ASU_clf",
    "tract_ASU_emp", "tract_ASU_unemp", "tract_ASU_urate")
  n <- 25005L
  data <- as.data.frame(setNames(rep(list(rep("0", n)), length(columns)), columns))
  ids <- sprintf("06001%06d", seq_len(n))
  data$geoid <- paste0("14000US", ids)
  data$st_fips <- "06"
  data$tract_pop2025 <- "100"
  data$tract_ASU_emp <- "70"
  data$tract_ASU_unemp <- "7"
  data$tract_ASU_clf <- "77"
  data$tract_ASU_urate <- "9.09"
  workbook <- tempfile(fileext = ".xlsx")
  on.exit(unlink(workbook), add = TRUE)
  writexl::write_xlsx(data, workbook)
  result <- asu_read_upload(workbook)
  stopifnot(nrow(result$data) == n, identical(result$data$GEOID, ids),
            result$population_year == "2025", sum(result$data$tract_ASU_unemp) == 7L * n,
            all(result$data$tract_pop_cur == 100L))
  # Missing data after the old row cap must still be validated, not truncated.
  data$tract_ASU_unemp[n] <- NA_character_
  writexl::write_xlsx(data, workbook)
  error <- tryCatch({ asu_read_upload(workbook); NULL }, error = identity)
  stopifnot(inherits(error, "error"),
            grepl("tract_ASU_unemp", conditionMessage(error), fixed = TRUE))
  cat("National import: all 25,005 rows preserved, final-row validation and dashboard R syntax passed.\n")
}

run_national_import_tests()
