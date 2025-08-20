\name{asu_py_env_create}
\alias{asu_py_env_create}
\title{Create (or reuse) a Python env for ASUbuildR and install deps}
\usage{asu_py_env_create(env_name = "asubuildr", engine = c("auto", "conda", "virtualenv"))}
\arguments{
  \item{env_name}{Character string giving the environment name.}
  \item{engine}{Character string, one of "auto", "conda", or "virtualenv".}
}
\value{Invisibly returns the environment name.}
\description{
Creates or reuses a Python 3.11 environment with the packages
\code{ortools}, \code{numpy}, \code{pandas}, and \code{networkx} installed.
}
