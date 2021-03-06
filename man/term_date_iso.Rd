% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panthr.r
\name{term_date_iso}
\alias{term_date_iso}
\title{Format Term as ISO Date}
\usage{
term_date_iso(term)
}
\arguments{
\item{term}{A 6-character \code{TERM} code, which may be a scalar value or vector
of length n in \code{YYYYMM} format.}
}
\value{
A scalar value or vector values in ISO date format.
}
\description{
\code{term_date_iso} formats the \code{TERM} field by converting it into ISO
(International Organization for Standardization) date format. That is,
the \code{YYYYMM} value converts to \code{YYYY-MM-DD}, using the first day of the
month.
}
\details{
Base functions, e.g. \code{as.Date}, \code{as.POSIXct}, and \code{as.POSXlt},
will coerce return values to various date classes, as ISO format is standard,
unambiguous, and easily recognized by base R.
}
\seealso{
\code{substr}, \code{paste}, \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}
}
\author{
Jamison R. Crawford, Institutional Research Associate, Georgia State University
}
