\name{setup_asu_python}
\alias{setup_asu_python}
\title{Setup Python Environment for ASUbuildR}
\usage{setup_asu_python(force = FALSE)}
\arguments{
  \item{force}{Logical; if TRUE, recreate the virtual environment even if it exists.}
}
\value{Invisible NULL.}
\description{Creates a Python virtual environment for ASUbuildR, installing Miniconda
and required packages if necessary. Existing Miniconda installations are reused
unless \code{force} is TRUE.}
