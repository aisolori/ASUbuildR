\name{asu_cut_vertices}
\alias{asu_cut_vertices}
\title{Identify cut vertices within ASU groups}
\description{Computes articulation points for each ASU based on tract contiguity.}
\usage{asu_cut_vertices(asu_sf)}
\arguments{
  \item{asu_sf}{sf object containing tracts with an \code{asunum} column.}
}
\value{A character vector of GEOID values that are cut vertices.}
\author{ASUbuildR contributors}
\keyword{internal}
