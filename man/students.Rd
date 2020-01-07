% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panthr.r
\docType{data}
\name{students}
\alias{students}
\title{10,000 Student Graduation Records}
\format{A data frame with 10,000 rows and 9 variables.}
\source{
GSU Data Warehouse: \code{edwprd.sdmcohortfr_us}
}
\usage{
students
}
\description{
\code{students} is a dataset containing 10,000 records of undergraduate students with degrees
conferred by Georgia State University between Summer, 2007 and Summer, 2019.
The format simulates a 10,000-record SQL query but potentially identifying
data have been shuffled for anonymity.
}
\details{
Variables \code{SEX}, \code{RACE_CODES}, \code{ETHNIC_CODES}, and
\code{GRAD_GPA} were shuffled across all 10,000 records using the formula:

\code{students$variable <- sample(students$variable, replace = FALSE)}

Random number generation function \code{set.seed()} was randomly selected and is unpublished.
}
\keyword{datasets}