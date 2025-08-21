\name{setup_asu_python}
\alias{setup_asu_python}
\title{Setup Python Environment for ASUbuildR}
\usage{setup_asu_python(force = FALSE)}
\arguments{
  \item{force}{Recreate the environment even if it exists.}
}
\value{Logical TRUE if the environment is ready, otherwise FALSE.}
\description{Creates or repairs the isolated Python environment used by ASUbuildR.
Installs Miniconda when needed and installs required packages (\code{ortools}, \code{pandas}, \code{numpy}, \code{setuptools}, \code{wheel}) in a dedicated directory called \code{asu-cpsat-venv}. Package installation prefers conda with the \code{conda-forge} channel and falls back to a virtualenv with pip. The environment path is recorded in \code{options("asu_python_env")}.
}
