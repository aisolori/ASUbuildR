\name{check_asu_python}
\alias{check_asu_python}
\title{Check Python Setup for ASUbuildR}
\usage{check_asu_python(quiet = FALSE)}
\arguments{
  \item{quiet}{Suppress status messages.}
}

\value{Logical TRUE if the environment is properly configured, otherwise FALSE.}
\description{Checks whether the ASUbuildR Python environment exists, required packages (including OR-Tools) are available, and the bundled asu\_cpsat module can be loaded.}
