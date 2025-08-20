\name{setup_asu_python}
\alias{setup_asu_python}
\title{Setup Python Environment for ASUbuildR}
\usage{setup_asu_python(force = FALSE)}
\arguments{
  \item{force}{Recreate the virtual environment even if it exists.}
}
\value{Logical TRUE if the environment is ready, otherwise FALSE.}
\description{Creates or repairs the Python virtual environment used by ASUbuildR.
Automatically installs Miniconda when needed and installs required packages (\code{ortools}, \code{pandas}, \code{numpy}, \code{setuptools}, \code{wheel}) using conda with a pip fallback.}
