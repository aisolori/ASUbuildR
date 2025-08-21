\name{setup_asu_python}
\alias{setup_asu_python}
\title{Setup Python Environment for ASUbuildR}
\usage{setup_asu_python(force = FALSE)}
\arguments{
  \item{force}{Recreate the environment even if it exists.}
}
\value{Logical TRUE if the environment is ready, otherwise FALSE.}
\description{Creates or repairs the isolated Python environment used by ASUbuildR.
Ensures Miniconda is available (installing it when necessary) and installs required packages (\code{ortools}, \code{pandas}, \code{numpy}, \code{setuptools}, \code{wheel}) in a dedicated directory called \code{asu-cpsat-venv}. Package installation uses conda with the \code{conda-forge} channel; if this fails, a message is emitted so users can install the packages manually. The environment path is recorded in \code{options("asu_python_env")}. 
}
