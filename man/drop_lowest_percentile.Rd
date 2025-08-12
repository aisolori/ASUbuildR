\name{drop_lowest_percentile}
\alias{drop_lowest_percentile}
\title{Drop low-unemployment tracts by percentile}
\description{Removes tracts with the lowest unemployment metrics from ASUs while optionally preserving contiguity. If drops split an ASU into multiple pieces, each piece is tested for validity (population \verb{>=} 10,000 and unemployment rate \verb{<=} 0.0645). Valid pieces are assigned new ASU IDs and invalid ones are cleared.}
\usage{drop_lowest_percentile(full_sf, percentile = 5, metric = c("rate", "unemp"), allow_breaks = FALSE)}
\arguments{
  \item{full_sf}{sf object of all tracts with an \code{asunum} column.}
  \item{percentile}{Numeric value between 0 and 100 specifying the bottom percentile of tracts to drop.}
  \item{metric}{Character string, either \code{"rate"} for unemployment rate or \code{"unemp"} for unemployment level.}
  \item{allow_breaks}{Logical; if \code{TRUE}, tracts are dropped even if this disconnects existing ASU groups. Disconnected components are re-evaluated and either re-numbered or unassigned based on size and unemployment rate thresholds.}
}
\value{A list with updated \code{full_data}, \code{asu_data}, \code{asu_tracts}, \code{asu_summary}, and the GEOIDs of dropped tracts.}
\author{ASUbuildR contributors}
\keyword{internal}
