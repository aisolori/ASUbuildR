\name{drop_lowest_percentile}
\alias{drop_lowest_percentile}
\title{Drop low-unemployment tracts by percentile}
\description{Removes tracts with the lowest unemployment metrics from ASUs while preserving contiguity.}
\usage{drop_lowest_percentile(full_sf, percentile = 5, metric = c("rate", "unemp"))}
\arguments{
  \item{full_sf}{sf object of all tracts with an \code{asunum} column.}
  \item{percentile}{Numeric value between 0 and 100 specifying the bottom percentile of tracts to drop.}
  \item{metric}{Character string, either \code{"rate"} for unemployment rate or \code{"unemp"} for unemployment level.}
}
\value{A list with updated \code{full_data}, \code{asu_data}, \code{asu_tracts}, \code{asu_summary}, and the GEOIDs of dropped tracts.}
\author{ASUbuildR contributors}
\keyword{internal}
