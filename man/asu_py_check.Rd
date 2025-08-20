\name{asu_py_check}
\alias{asu_py_check}
\title{Check Python deps and import the bundled module}
\usage{asu_py_check(silent = FALSE)}
\arguments{
  \item{silent}{Logical; if TRUE, suppress messages and return the check result.}
}
\value{Logical TRUE if all required modules are available.}
\description{Verifies that required Python modules are installed and that the packaged
\code{asu_cpsat} module can be imported.}
