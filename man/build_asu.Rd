\name{build_asu}
\alias{build_asu}
\title{Build ASUs using OR-Tools CP-SAT (Python) from an R data frame}
\usage{build_asu(df, neighbors, tau = 0.0645, pop_thresh = 10000,
  max_asus = 30, time_limit = 1200,
  workers = max(1L, parallel::detectCores(logical = TRUE) - 1L),
  rel_gap = NA_real_, verbose = interactive())}
\arguments{
  \item{df}{Data frame with required columns.}
  \item{neighbors}{List of integer vectors giving contiguity indices.}
  \item{tau}{Unemployment rate threshold.}
  \item{pop_thresh}{Population threshold.}
  \item{max_asus}{Maximum number of ASUs to carve.}
  \item{time_limit}{Seconds per window.}
  \item{workers}{CP-SAT threads.}
  \item{rel_gap}{Optional relative MIP gap.}
  \item{verbose}{Logical; print CP-SAT logs.}
}
\value{The input data frame with an added \code{asu_id} column.}
\description{Calls the bundled Python solver to assign tracts to Areas of
Substantial Unemployment using OR-Tools CP-SAT.}
