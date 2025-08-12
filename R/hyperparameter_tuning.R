#' Hyperparameter tuning for Tract Hunter scoring
#'
#' Executes the Tract Hunter algorithm across a grid of scoring
#' parameters.  Computations are distributed across multiple cores
#' using the base \code{parallel} package.
#'
#' @param tract_list List of tract data with contiguity information
#' @param bls_df BLS data frame
#' @param unemp_power Numeric vector of exponents applied to unemployment counts
#' @param ur_weight Numeric vector of powers applied to unemployment rates
#' @param path_len_weight Numeric vector of penalties applied per tract in a path
#' @param cores Number of parallel workers to use
#' @param ur_thresh Unemployment rate threshold
#' @param pop_thresh Population threshold
#' @return Data frame summarising ASU counts for each parameter combination
#' @export
#' @examples
#' \dontrun{
#' tune_scoring(tract_list, bls_df,
#'              unemp_power = c(0.8, 0.9),
#'              ur_weight = c(1, 1.2),
#'              path_len_weight = c(0, 0.05),
#'              cores = 2)
#' }

tune_scoring <- function(tract_list, bls_df,
                         unemp_power = 0.9,
                         ur_weight = 1.0,
                         path_len_weight = 0,
                         cores = parallel::detectCores() - 1,
                         ur_thresh = 0.0645,
                         pop_thresh = 10000) {

  params <- expand.grid(unemp_power = unemp_power,
                        ur_weight = ur_weight,
                        path_len_weight = path_len_weight)

  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  results <- parallel::parLapply(cl, seq_len(nrow(params)), function(i) {
    p <- params[i, ]
    state <- tract_hunter_seed(tract_list, bls_df,
                               ur_thresh = ur_thresh,
                               pop_thresh = pop_thresh,
                               verbose = FALSE,
                               unemp_power = p$unemp_power,
                               ur_weight   = p$ur_weight)

    state <- tract_hunter_asu_pass(state,
                                   verbose = FALSE,
                                   path_len_weight = p$path_len_weight,
                                   unemp_power = p$unemp_power,
                                   ur_weight   = p$ur_weight)

    asu_count <- length(unique(stats::na.omit(state$data_merge$asunum)))

    data.frame(unemp_power   = p$unemp_power,
               ur_weight     = p$ur_weight,
               path_len_weight = p$path_len_weight,
               asu_count     = asu_count)
  })

  dplyr::bind_rows(results)
}
