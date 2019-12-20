#' 10,000 Student Graduation Records
#'
#' A dataset containing 10,000 records of undergraduate students with degrees
#' conferred by Georgia State University between Summer, 2007 and Summer, 2019.
#' The format simulates a 10,000-record SQL query but potentially identifying
#' data have been shuffled for anonymity.
#'
#' @format A data frame with 10,000 rows and 9 variables.
#'
#' @details Variables \code{SEX}, \code{RACE_CODES}, \code{ETHNIC_CODES}, and
#' \code{GRAD_GPA} were shuffled across all 10,000 records using the formula:
#'
#' \code{students$variable <- sample(students$variable, replace = FALSE)}
#'
#' Random number generation function \code{set.seed()} was randomly selected and is unpublished.
#'
#' @source GSU Data Warehouse: \code{edwprd.sdmcohortfr_us}

"students"



#' Format Term by Separating Year and Month
#'
#' This basic function formats the \code{TERM} field by separating the 4-digit year
#' and 2-digit month with a dash. That is, \code{YYYYMM} converts to \code{YYYY-MM}. It
#' accepts values of both class numeric and character.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar \code{TERM} value or vector of \code{TERM} values in \code{YYYY-MM} format.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @seealso \code{substr}, \code{paste}
#' @export

term_dash <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])

    if (na){y[i] <- NA}
    else if (!na){y[i] <- paste(substr(x[i], 1, 4),
                                substr(x[i], 5, 6),
                                sep = "-")}

  }

  return(y)

}




#' Format Term as ISO Date
#'
#' This function formats the \code{TERM} field by converting it into ISO
#' (International Organization for Standardization) date format. That is,
#' the \code{YYYYMM} value converts to \code{YYYY-MM-DD}, using the first day of the
#' month.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar value or vector values in ISO date format.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Base functions, e.g. \code{as.Date}, \code{as.POSIXct}, and \code{as.POSXlt},
#' will coerce return values to various date classes, as ISO format is standard,
#' unambiguous, and easily recognized by base R.
#' @seealso \code{substr}, \code{paste}, \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}
#' @export

term_date <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])

    if (na){y[i] <- NA}
    else if (!na){y[i] <- paste(substr(x[i], 1, 4),
                                substr(x[i], 5, 6),
                                "01",
                                sep = "-")}

  }

  return(y)

}



#' Convert Term to Fall, Spring, or Summer
#'
#' This function formats the \code{TERM} field by converting it into a string of class
#' character that matches each term.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values of class character indicating \code{Fall},
#' \code{Summer}, or \code{Spring} according to the term(s) passed.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order.
#' @seealso \code{factor}, \code{as.factor}
#' @export

term_season <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    mo <- substr(x[i], 5, 6)

    if (na){y[i] <- NA}
    else if (!na & mo == "01"){y[i] <- "Spring"}
    else if (!na & mo == "05"){y[i] <- "Summer"}
    else if (!na & mo == "08"){y[i] <- "Fall"}

  }

  return(y)

}



#' Format Term as Season & Year
#'
#' This function formats the \code{TERM} field by converting it into a string of class
#' character containing the season or semester and calendar year, e.g.
#' \code{Fall 2019}. An option is available to customize how season and year are joined.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @param sep A character string which separates season and year. Input must be
#' wrapped in quotes, e.g. \code{" "}, \code{"-"}, or \code{", "}.
#'
#' @return A scalar or vector of values of class character combininging season
#' and year, e.g. \code{Summer 2016}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order.
#' @seealso \code{factor}, \code{as.factor}
#' @export

term_name <- function(term, sep = " "){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    yr <- substr(x[i], 1, 4)
    mo <- substr(x[i], 5, 6)

    if (na){y[i] <- NA}
    else if (!na & !grepl("01|05|08", mo)){

      y[i] <- NA;
      warning("Value(s) passed to 'term =' do not end in '01', '05', or '08'; coercing to NA", call. = FALSE)}

    else if (!na & mo == "01"){y[i] <- paste("Spring", yr, sep = sep)}
    else if (!na & mo == "05"){y[i] <- paste("Summer", yr, sep = sep)}
    else if (!na & mo == "08"){y[i] <- paste("Fall", yr, sep = sep)}

  }

  return(y)

}



#' Convert Term to Calendar Year
#'
#' This basic function formats the \code{TERM} field by eliminating the last two
#' digits, i.e. from \code{YYYYMM} to \code{YYYY}, in effect converting it to the
#' calendar year.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the calendar year.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#' @seealso \code{substr}, \code{table}
#' @export

term_year_calendar <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])

    if (na){y[i] <- NA}
    else if (!na){y[i] <- substr(x[i], 1, 4)}

  }

  return(y)

}



#' Convert Term to Academic Year
#'
#' This basic function formats the \code{TERM} field by referencing the semester or
#' season in order to modify and return the academic year during which the term
#' took place.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the academic year.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#' @seealso \code{substr}, \code{table}
#' @export

term_year_academic <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    mo <- substr(x[i], 5, 6)
    yr <- as.integer(substr(x[i], 1, 4))

    if (na){y[i] <- NA}
    else if (!na & !grepl("01|05|08", mo)){

      y[i] <- NA;
      warning("Value(s) passed to 'term =' do not end in '01', '05', or '08'; coercing to NA", call. = FALSE)}

    else if (!na & mo == "01"){y[i] <- yr - 1}
    else if (!na & mo == "05"){y[i] <- yr - 1}
    else if (!na & mo == "08"){y[i] <- yr}

  }

  return(y)

}



#' Convert Term to Fiscal Year
#'
#' This basic function formats the \code{TERM} field by referencing the semester or
#' season in order to modify and return the fiscal year during which the term
#' took place.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the fiscal year.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#' @seealso \code{substr}, \code{table}
#' @export

term_year_fiscal <- function(term){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    mo <- substr(x[i], 5, 6)
    yr <- as.integer(substr(x[i], 1, 4))

    if (na){y[i] <- NA}
    else if (!na & !grepl("01|05|08", mo)){

      y[i] <- NA;
      warning("Value(s) passed to 'term =' do not end in '01', '05', or '08'; coercing to NA", call. = FALSE)}

    else if (!na & mo == "01"){y[i] <- yr}
    else if (!na & mo == "05"){y[i] <- yr + 1}
    else if (!na & mo == "08"){y[i] <- yr + 1}

  }

  return(y)

}



#' Convert Sex Codes to Descriptive Labels
#'
#' This function formats the \code{SEX} field by evaluating a scalar or vector of
#' values with gender codes and missing values. \code{F} is converted to \code{Female},
#' \code{M} is converted to \code{Male}, and \code{NULL} values are converted to \code{Not Reported}.
#'
#' @param code A 1-character \code{SEX} code, which may be a scalar value or vector
#' of length n comprised of \code{F}, \code{M}, or \code{NULL}.
#'
#' @return A scalar or vector of values converted into full-length, descriptive labels or
#' \code{Not Reported} if missing.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values which are not \code{NULL} nor contain \code{F} or \code{M} are coerced to NA.
#'
#' @seealso \code{grepl}, \code{table}
#' @export

decode_sex <- function(code){

  x <- code
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    s <- x[i]

    if (na){y[i] <- "Not Reported"}
    else if (!na & !grepl("M|F", s)){

      y[i] <- NA;
      warning("Value(s) passed to 'code =' do not contain 'M' or 'F'; coercing to NA", call. = FALSE)}

    else if (!na & s == "M"){y[i] <- "Male"}
    else if (!na & s == "F"){y[i] <- "Female"}


  }

  return(y)

}



#' Convert Race Codes to Multiple Labels
#'
#' This function formats the \code{RACE_CODES} field by evaluating a scalar or vector
#' of values with race codes and missing values and converting them to all full
#' race labels, separated by commas. Missing values are converted to \code{Not Reported}.
#'
#' @param code A scalar or vector of length n and class character containing race
#' codes, e.g. \code{W}, \code{B}, \code{Z}, \code{I}, \code{P}, or any permutation containing more than
#' one code.
#'
#' @return A scalar or vector of values of length n and class character
#' converted into full-length labels and separated by commas. \code{NULL} values are
#' converted into the character string: \code{Not Reported}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing race codes other than \code{W}, \code{B}, \code{Z}, \code{I}, or \code{P} are
#' coerced to \code{NA} rather than converted to \code{Not Reported}. In these instances,
#' a warning message is thrown.
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#' @export

decode_race_parsed <- function(code){

  x <- code
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    r <- x[i]

    if (na){y[i] <- "Not Reported"}
    else if (!na & grepl("[AC-HJ-OQ-VX-Y]{,1}", r)){

      y[i] <- NA
      warning("Unrecognized race code(s) passed to 'code ='; coercing to NA", call. = FALSE)}

    else if (!na){

      r <- strsplit(x = r, split = "")[[1]];
      r <- gsub("W", "White", r);
      r <- gsub("B", "Black or African American", r);
      r <- gsub("Z", "Asian", r);
      r <- gsub("I", "Native American or Native Alaskan", r);
      r <- gsub("P", "Native Hawaiian or Pacific Islander", r);
      y[i] <- paste0(r, collapse = ", ")}

  }

  return(y)

}



#' Convert Race Codes to a Single Label
#'
#' This function formats the \code{RACE_CODES} field by evaluating a scalar or vector
#' of values with race codes and missing values and converting them to a single
#' category. For example, while \code{B} will convert to \code{Black or African American},
#' \code{BZ} will convert to \code{Two or More Races}. Missing values are converted to
#' \code{Not Reported}.
#'
#' @param code A scalar or vector of length n and class character containing race
#' codes, e.g. \code{W}, \code{B}, \code{Z}, \code{I}, \code{P}, or any permutation containing more than
#' one code, e.g. \code{ZI}, \code{WBP}, etc.
#'
#' @return A scalar or vector of values of length n and class character
#' converted into a single label describing the race code. \code{NULL} values are
#' converted into the character string: \code{Not Reported}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing race codes other than \code{W}, \code{B}, \code{Z}, \code{I}, or \code{P} are
#' coerced to \code{NA} (missing) rather than converted to \code{Not Reported}. In these
#' instances, a warning message is thrown. In instances where ethnicity codes
#' are detected, evaluation halts and an error message is thrown.
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#' @export

decode_race <- function(code){

  x <- code
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    rc <- x[i]

    if (na){y[i] <- "Not Reported"}
    else if (!na & grepl("[AC-HJ-OQ-VX-Y03-9]{,1}", rc)){

      y[i] <- NA
      warning("Unrecognized race code(s) passed to 'code ='; coercing to NA", call. = FALSE)}

    else if (!na & grepl("[1-2]{,1}", rc)){

      stop("'1' or '2' was passed to 'code ='; did you accidentally input ethnicity codes?", call. = FALSE)}

    else if (!na & nchar(rc) > 1){

      y[i] <- "Two or More Races"

    }

    else if (!na & nchar(rc) == 1){

      rc <- gsub("W", "White", rc)
      rc <- gsub("B", "Black or African American", rc)
      rc <- gsub("Z", "Asian", rc)
      rc <- gsub("I", "Native American or Native Alaskan", rc)
      rc <- gsub("P", "Native Hawaiian or Pacific Islander", rc)

      y[i] <- rc}

  }

  return(y)

}



#' Convert Ethnicity Codes to a Single Label
#'
#' This function formats the \code{ETHNIC_CODES} field by evaluating a scalar or vector
#' of values with ethnic codes and missing values and converting them to a single
#' label. Missing values are converted to \code{Not Reported}.
#'
#' @param code A scalar or vector of length n and either class character or numeric
#' that contain an ethnicity code, i.e. \code{1} or \code{2}.
#'
#' @return A scalar or vector of values of length n and class character
#' containing a single label describing the ethnicity code. \code{NULL} values are
#' converted into the character string: \code{Not Reported}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing ethnicity codes other than \code{1} or \code{2} are coerced to
#' \code{NA} (missing) rather than converted to \code{Not Reported}. In these
#' instances, a warning message is thrown. In instances where race codes
#' are detected, evaluation halts and an error message is thrown.
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#' @export

decode_ethnicity <- function(code){

  x <- code
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])
    et <- x[i]

    if (na){y[i] <- "Not Reported"}
    else if (!na & grepl("[3-9]{,1}", et)){

      y[i] <- NA
      warning("Unrecognized ethnicity code(s) passed to 'code ='; coercing to NA", call. = FALSE)}

    else if (!na & grepl("[a-zA-Z]{,1}", et)){

      stop("One or more alphabetical characters were passed to 'code ='; did you accidentally use race codes?", call. = FALSE)}

    else if (!na & et == "1"){y[i] <- "Not Hispanic/Latinx"}
    else if (!na & et == "2"){y[i] <- "Hispanic/Latinx"}

  }

  return(y)

}



#' Convert Race & Ethnicity Codes to Single Labels
#'
#' This function formats both the \code{RACE_CODES} and \code{ETHNIC_CODES} fields by
#' evaluating two scalar values or vectors of values of equal length containing
#' race and ethnicity codes. Missing values for both race and ethnicity, as well
#' as missing race codes and ethnicity of \code{1}, are converted to \code{Not Reported}.
#' In instances where more than one race or ethnicity is evaluated, the value is
#' labeled \code{Two or More Races}.
#'
#' @param race A scalar or vector of length n and class character comprised of
#' race codes, e.g. \code{B}, \code{Z}, or some permutation of race codes, e.g. \code{WZI}.
#'
#' @param ethnicity A scalar or vector of length n and class character or numeric
#' that contain an ethnicity code, i.e. \code{1} or \code{2}.
#'
#' @param ethnic.precedence A logical value indicating whether ethnicity takes
#' precedence over one or more races. As \code{Hispanic or Latinx} is the only
#' other ethnicity, any race or permutation of race codes will be overridden
#' if \code{TRUE}. Defaults to \code{ethnic.precedence = TRUE}.
#'
#' @return A scalar or vector of values of length n and class character
#' containing a single label describing the combined race and ethnicity code.
#'
#' \code{NULL} values are converted into the character string: \code{Not Reported}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Ethnicity codes other than \code{1} or \code{2} are coerced to \code{NA} (missing) rather
#' than converted to \code{Not Reported}. Likewise, unrecognized race codes are
#' coerced to \code{NA} (missing) values rather than \code{Not Reported}. In both instances,
#' a warning is thrown.
#'
#' In instances were race codes are detected in argument \code{ethnicity =} or
#' ethnicity codes are detected in argument \code{race =}, evaluation is halted and
#' an error is thrown.
#'
#' @seealso \code{table}
#' @export

decode_ethnorace <- function(race, ethnicity, ethnic.precedence = TRUE){

  r <- race
  e <- ethnicity
  v <- vector()

  for (i in seq_along(r)){

    if (is.na(r[i])){r[i] <- ""}
    if (is.na(e[i])){e[i] <- ""}
    if (e[i] == 1){e[i] <- ""}

    v[i] <- paste0(r[i], e[i], collapse = "")

    if (v[i] == ""){v[i] <- "Not Reported"}

    if (ethnic.precedence == FALSE){

      if (nchar(v[i]) > 1){v[i] <- "Two or More Races"}

      else if (v[i] == "W"){v[i] <- "White"}
      else if (v[i] == "Z"){v[i] <- "Asian"}
      else if (v[i] == "2"){v[i] <- "Hispanic or Latinx"}
      else if (v[i] == "B"){v[i] <- "Black or African American"}
      else if (v[i] == "I"){v[i] <- "Native American or Native Alaskan"}
      else if (v[i] == "P"){v[i] <- "Native Hawaiian or Pacific Islander"}

    }

    else if (ethnic.precedence == TRUE){

      if (nchar(v[i]) > 1 & !grepl(pattern = "2", x = v[i])){v[i] <- "Two or More Races"}
      else if (nchar(v[i]) > 1 & grepl(pattern = "2", x = v[i])){v[i] <- "Hispanic or Latinx"}

      else if (v[i] == "W"){v[i] <- "White"}
      else if (v[i] == "Z"){v[i] <- "Asian"}
      else if (v[i] == "2"){v[i] <- "Hispanic or Latinx"}
      else if (v[i] == "B"){v[i] <- "Black or African American"}
      else if (v[i] == "I"){v[i] <- "Native American or Native Alaskan"}
      else if (v[i] == "P"){v[i] <- "Native Hawaiian or Pacific Islander"}

    }

  }

  return(v)

}



#' Convert Department Codes to Full Labels
#'
#' This function converts all \code{*DEPARTMENT} code fields, e.g. \code{GRAD_DEPARTMENT},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more department codes, e.g. \code{BIO}, \code{PSY}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length department names. Missing values are preserved.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized department codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @seealso \code{%in%}, \code{table}
#' @export

decode_department <- function(code){

  c <- code
  v <- vector()
  d <- department

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% d$department_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% d$department_code) {v[i] <- d[c[i] == d$department_code, "department_full"]}

  }

  return(unlist(v))

}



#' Convert College Codes to Full or Abbreviated Labels
#'
#' This function converts all \code{*COLLEGE} code fields, e.g. \code{GRAD_COLLEGE},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more college codes, e.g. \code{BU}, \code{AS}, etc.
#'
#' @param short A logical value indicating whether labels should be abbreviated.
#' Useful for tables and graphics in limiting real estate and non-data ink.
#' Defaults to \code{short = FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length or abbreviated college names. Missing values are preserved.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized college codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @seealso \code{%in%}, \code{table}
#' @export

decode_college <- function(code, short = FALSE){

  c <- code
  v <- vector()
  d <- college

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% d$college_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    if (!is.na(c[i]) & c[i] %in% d$college_code & short == FALSE) {v[i] <- d[c[i] == d$college_code, "college_full"]}
    else if (!is.na(c[i]) & c[i] %in% d$college_code & short == TRUE) {v[i] <- d[c[i] == d$college_code, "college_short"]}

  }

  return(unlist(v))

}



#' Convert Degree Codes to Full Labels
#'
#' This function converts all \code{*DEGREE} code fields, e.g. \code{GRAD_DEGREE},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more degree codes, e.g. \code{BA}, \code{MPP}, \code{PHD}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length degree names. Missing values are preserved.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized degree codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @seealso \code{%in%}, \code{table}
#' @export

decode_degree <- function(code){

  c <- code
  v <- vector()
  d <- degree

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% d$degree_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% d$degree_code) {v[i] <- d[c[i] == d$degree_code, "degree_full"]}

  }

  return(unlist(v))

}



#' Convert Major Codes to Full Labels
#'
#' This function converts all \code{*MAJOR} code fields, e.g. \code{GRAD_MAJOR},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more major codes, e.g. \code{CJ}, \code{AAS}, \code{LIT}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length major names. Missing values are preserved.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized major codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @seealso \code{%in%}, \code{table}
#' @export

decode_major <- function(code){

  c <- code
  v <- vector()
  d <- major

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% d$major_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% d$major_code) {v[i] <- d[c[i] == d$major_code, "major_full"]}

  }

  return(unlist(v))

}



#' Round GPA Values Directionally
#'
#' This function enhances the base R \code{round} function to allow directionality
#' in rounding GPA' fields, e.g. \code{GRAD_GPA}, allowing for conventional
#' rounding, rounding up, rounding down, and to which decimal place.
#'
#' @param gpa A scalar or vector of length n and class numeric containing
#' one or more GPA values.
#'
#' @param digits A single numeric value indicating quantity of decimal places to
#' round. E.g. \code{digits = 2} will round to two decimal places. Parameters exceeding
#' decimal places of values passed in argument \code{gpa =} automatically round the
#' last digit. Defaults to \code{digits = 1}.
#'
#' @param direction A single string, in quotations, indicating directionality.
#' Possible values include \code{"up"}, \code{"down"}, or \code{"none"}. Defaults to
#' \code{direction = "none"}.
#'
#' @return A scalar or vector of GPA values of class numeric containing per the
#' given parameters. Missing values are preserved.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{round}, \code{ceiling}, \code{floor}
#' @export

gpa_round <- function(gpa, digits = 1, direction = "none"){

  g <- gpa
  v <- vector()

  for (i in seq_along(gpa)){

    if (is.na(gpa[i])){v[i] <- NA}

    else if (!is.na(g[i]) & direction == "none"){v[i] <- round(g, digits = digits)}

    else if (!is.na(g[i]) & direction == "up"){v[i] <- ceiling(g[i] * 10^digits) / 10^digits}

    else if (!is.na(g[i]) & direction == "down"){v[i] <- floor(g[i] * 10^digits) / 10^digits}

  }

  return(v)

}



#' Convert GPA Values into Discrete Ranges
#'
#' This function optimizes base R function \code{cut} specifically for values
#' in \code{*GPA} fields, e.g. \code{GRAD_GPA}, by streamlining binning into tenths,
#' quarters, thirds, and halves, as well as cleaning bin labels.
#'
#' @param gpa A scalar or vector of length n and class numeric containing
#' one or more GPA values.
#'
#' @param range A single string, in quotations, indicating the range of cut
#' points. Possible values include \code{"tenths"}, \code{"quarters"}, \code{"thirds"}, and \code{"halves"}.
#' Defaults to \code{range = "thirds"}.
#'
#' @param right A logical value indicating if ranges should be closed on the right
#' and open on the left (\code{TRUE}). Defaults to \code{right = TRUE}.
#'
#' @return A scalar or vector of clean character strings, according to the
#' parameter passed to argument \code{range =}, comprised of the lower and upper
#' bounds in which the value passed to \code{gpa =} falls. Missing values are preserved.
#'
#' @details Values passed to argument \code{gpa =} which exceed 4.33 are coerced to
#' \code{NA} (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{cut}, \code{gsub}
#' @export

gpa_discretize <- function(gpa, range = "thirds", right = FALSE){

  g <- gpa
  v <- vector()
  tnth <- seq(0, 4.4, by = 0.1)
  tnlb <- c("0.0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0", "1.0-1.1",
            "1.1-1.2", "1.2-1.3", "1.3-1.4", "1.4-1.5", "1.5-1.6", "1.6-1.7", "1.7-1.8", "1.8-1.9", "1.9-2.0", "2.0-2.1", "2.1-2.2",
            "2.2-2.3", "2.3-2.4", "2.4-2.5", "2.5-2.6", "2.6-2.7", "2.7-2.8", "2.8-2.9", "2.9-3.0", "3.0-3.1", "3.1-3.2", "3.2-3.3",
            "3.3-3.4", "3.4-3.5", "3.5-3.6", "3.6-3.7", "3.7-3.8", "3.8-3.9", "3.9-4.0", "4.0-4.1", "4.1-4.2", "4.2-4.3", "4.3-4.4")
  qrts <- seq(0, 4.5, by = 0.25)
  qrlb <- c("0.00-0.25", "0.25-0.50", "0.50-0.75", "0.75-1.00", "1.00-1.25", "1.25-1.50",
            "1.50-1.75", "1.75-2.00", "2.00-2.25", "2.25-2.50", "2.50-2.75", "2.75-3.00",
            "3.00-3.25", "3.25-3.50", "3.50-3.75", "3.75-4.00", "4.00-4.25", "4.25-4.50")
  thrd <- c(0, 0.33, 0.66, 1, 1.33, 1.66, 2, 2.33, 2.66, 3, 3.33, 3.66, 4, 4.33)
  thlb <- c("0.00-0.33", "0.33-0.66", "0.66-1.00", "1.00-1.33", "1.33-1.66", "1.66-2.00", "2.00-2.33",
            "2.33-2.66", "2.66-3.00", "3.00-3.33", "3.33-3.66", "3.66-4.00", "4.00-4.33")
  halv <- seq(0, 4.5, by = 0.5)
  halb <- c("0.0-0.5", "0.5-1.0", "1.0-1.5", "1.5-2.0", "2.0-2.5", "2.5-3.0", "3.0-3.5", "3.5-4.0", "4.0-4.5")

  for (i in seq_along(gpa)){

    if (is.na(g[i])){v[i] <- NA}

    else if (g[i] > 4.33){

      v[i] <- NA
      warning("One or more values passed to 'gpa =' exceeds 4.33; coercing to NA", call. = FALSE)

    }

    else if (!is.na(g[i]) & range == "tenths"){v[i] <- as.character(cut(x = g[i], breaks = tnth, labels = tnlb, right = right))}
    else if (!is.na(g[i]) & range == "quarters"){v[i] <- as.character(cut(x = g[i], breaks = qrts, labels = qrlb, right = right))}
    else if (!is.na(g[i]) & range == "thirds"){v[i] <- as.character(cut(x = g[i], breaks = thrd, labels = thlb, right = right))}
    else if (!is.na(g[i]) & range == "halves"){v[i] <- as.character(cut(x = g[i], breaks = halv, labels = halb, right = right))}

  }

  v <- gsub(x = v, pattern = ",", replacement = "-")
  v <- gsub(x = v, pattern = "\\[|\\)", replacement = "")

  return(v)

}
