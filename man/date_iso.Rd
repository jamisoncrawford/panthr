% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panthr.r
\name{date_iso}
\alias{date_iso}
\title{Convert Dates to ISO Format}
\usage{
date_iso(date)
}
\arguments{
\item{date}{A scalar or vector of length n and class character containing
one or more date values in \code{DD-MMM-YY} format.}
}
\value{
A scalar or vector of values of class character containing
standard ISO dates in \code{YYYY-MM-DD} format. Missing values are preserved.
}
\description{
\code{date_iso} converts date field values in \code{DD-MMM-YY} format to the
International Organization for Standardization (ISO) format, i.e. \code{YYYY-MM-DD}.
}
\details{
Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
automatically convert to post-2000 C.E.

Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
format or contain unrecognized month abbreviations are coerced to \code{NA}
(missing) values and a warning message is thrown.

ISO dates are automatically recognized as dates in R and may be converted to
different formats using \code{as.*} coersion functions, e.g. \code{as.Date}.
}
\seealso{
\code{date}, \code{POSIXct}, \code{POSIXlt}
}
\author{
Jamison R. Crawford, Institutional Research Associate, Georgia State University
}
