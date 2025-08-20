\name{asu_py_use}
\alias{asu_py_use}
\title{Activate an existing ASUbuildR Python env for this R session}
\usage{asu_py_use(env_name = "asubuildr", engine = c("auto", "conda", "virtualenv"))}
\arguments{
  \item{env_name}{Environment name to activate.}
  \item{engine}{"auto", "conda", or "virtualenv".}
}
\value{Logical TRUE if the environment was activated successfully.}
\description{Activates an existing Python environment and verifies the required packages.}
