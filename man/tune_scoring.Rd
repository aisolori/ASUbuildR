% Manual documentation for tune_scoring
\name{tune_scoring}
\alias{tune_scoring}
\title{Hyperparameter tuning for Tract Hunter scoring}
\usage{
  tune_scoring(
    tract_list,
    bls_df,
    unemp_power = 0.9,
    ur_weight = 1,
    path_len_weight = 0,
    cores = parallel::detectCores() - 1,
    ur_thresh = 0.0645,
    pop_thresh = 10000
  )
}
\arguments{
\item{tract_list}{List of tract data with contiguity information}
\item{bls_df}{BLS data frame}
\item{unemp_power}{Vector of exponents applied to unemployment counts}
\item{ur_weight}{Vector of powers applied to unemployment rates}
\item{path_len_weight}{Vector of penalties applied per tract in a path}
\item{cores}{Number of parallel workers to use}
\item{ur_thresh}{Unemployment rate threshold}
\item{pop_thresh}{Population threshold}
}
\value{Data frame summarising ASU counts for each parameter combination}
\description{Runs the Tract Hunter algorithm over a grid of scoring parameters using parallel processing.}
\keyword{internal}
