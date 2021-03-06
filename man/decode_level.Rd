% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panthr.r
\name{decode_level}
\alias{decode_level}
\title{Convert Student Level Codes to Full Descriptions}
\usage{
decode_level(code, clean = FALSE)
}
\arguments{
\item{code}{A scalar or vector of length n and class character containing
one or more level codes.}

\item{clean}{A logical value (i.e. \code{TRUE} or \code{FALSE}) to
indicate whether returned values should be made more human-readable. Defaults
to \code{FALSE}.}
}
\value{
A scalar or vector of values of class character containing
full-length level descriptions. Missing values are preserved.
}
\description{
\code{decode_level} converts student level code fields into full-length descriptions
by evaluating a scalar or vector of values of class character. It also
allows automated "cleaning" of validated values in a more human-readable
format.
}
\details{
Output may be made categorical using function
\code{factor} or \code{as.factor}. This function also allows for easily
tallying counts using, e.g., function \code{table}.

Values containing unrecognized level codes are coerced to \code{NA}
(missing) values and a warning message is thrown.
}
\seealso{
\code{table}
}
\author{
Jamison R. Crawford, Institutional Research Associate, Georgia State University
}
