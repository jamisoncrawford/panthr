# Load Global Variables from 'sysdata.rda'

globalVariables(c("admit",
                  "attribute",
                  "building",
                  "campus",
                  "college",
                  "county",
                  "decision",
                  "degree",
                  "department",
                  "email",
                  "grade",
                  "institution",
                  "level",
                  "major",
                  "state"))



#' 10,000 Student Graduation Records
#'
#' \code{students} is a dataset containing 10,000 records of undergraduate students with degrees
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
#' \code{term_separate} is a simple function that formats the \code{TERM} field by
#' separating the 4-digit year and 2-digit month with a user-defined delimiter.
#' For example, the \code{YYYYMM} format converts to \code{YYYY-MM} by setting
#' the delimiter to a dash, or \code{"-"}.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @param sep A quoted string of class character specifying a delimiter between the
#' term year and month, e.g. \code{sep = "."} for \code{term = "201908"} results in
#' \code{2019.08}. Defaults to \code{sep = "_"}.
#'
#' @return A scalar \code{TERM} value or vector of \code{TERM} values in
#' \code{YYYY*MM} delimited format.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{substr}, \code{paste}
#'
#' @export

term_separate <- function(term, sep = "-"){

  x <- term
  y <- vector()

  for (i in seq_along(x)){

    na <- is.na(x[i])

    if (na){y[i] <- NA}
    else if (!na){y[i] <- paste(substr(x[i], 1, 4),
                                substr(x[i], 5, 6),
                                sep = sep)}

  }

  return(y)

}




#' Format Term as ISO Date
#'
#' \code{term_date_iso} formats the \code{TERM} field by converting it into ISO
#' (International Organization for Standardization) date format. That is,
#' the \code{YYYYMM} value converts to \code{YYYY-MM-DD}, using the first day of the
#' month.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar value or vector values in ISO date format.
#'
#' @details Base functions, e.g. \code{as.Date}, \code{as.POSIXct}, and \code{as.POSXlt},
#' will coerce return values to various date classes, as ISO format is standard,
#' unambiguous, and easily recognized by base R.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{substr}, \code{paste}, \code{as.Date}, \code{as.POSIXct}, \code{as.POSIXlt}
#'
#' @export

term_date_iso <- function(term){

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
#' \code{term_season} formats the \code{TERM} field by converting it into a string of class
#' character that matches each term.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values of class character indicating \code{Fall},
#' \code{Summer}, or \code{Spring} according to the term(s) passed.
#'
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{factor}, \code{as.factor}
#'
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
#' \code{term_name} formats the \code{TERM} field by converting it into a string of class
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
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{factor}, \code{as.factor}
#'
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
#' \code{term_year_calendar} formats the \code{TERM} field by eliminating the last two
#' digits, i.e. from \code{YYYYMM} to \code{YYYY}, in effect converting it to the
#' calendar year.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the calendar year.
#'
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{substr}, \code{table}
#'
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
#' \code{term_year_academic} formats the \code{TERM} field by referencing the semester or
#' season in order to modify and return the academic year during which the term
#' took place.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the academic year.
#'
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{substr}, \code{table}
#'
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
#' \code{term_year_fiscal} formats the \code{TERM} field by referencing the semester or
#' season in order to modify and return the fiscal year during which the term
#' took place.
#'
#' @param term A 6-character \code{TERM} code, which may be a scalar value or vector
#' of length n in \code{YYYYMM} format.
#'
#' @return A scalar or vector of values in \code{YYYY} format with the fiscal year.
#'
#' @details Output may be made categorical or ordinal nominal using function
#' \code{factor} or \code{as.factor}, which may specify order. This function
#' also allows for easily tallying counts using, e.g., function \code{table}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{substr}, \code{table}
#'
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
#' \code{decode_sex} formats the \code{SEX} field by evaluating a scalar or vector of
#' values with gender codes and missing values. \code{F} is converted to \code{Female},
#' \code{M} is converted to \code{Male}, and \code{NULL} values are converted to \code{Not Reported}.
#'
#' @param code A 1-character \code{SEX} code, which may be a scalar value or vector
#' of length n comprised of \code{F}, \code{M}, or \code{NULL}.
#'
#' @return A scalar or vector of values converted into full-length, descriptive labels or
#' \code{Not Reported} if missing.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values which are not \code{NULL} nor contain \code{F} or \code{M} are coerced to NA.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{grepl}, \code{table}
#'
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
#' \code{decode_race_parsed} formats the \code{RACE_CODES} field by evaluating a scalar or vector
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
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing race codes other than \code{W}, \code{B}, \code{Z}, \code{I}, or \code{P} are
#' coerced to \code{NA} rather than converted to \code{Not Reported}. In these instances,
#' a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#'
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
#' \code{decode_race} formats the \code{RACE_CODES} field by evaluating a scalar or vector
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
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing race codes other than \code{W}, \code{B}, \code{Z}, \code{I}, or \code{P} are
#' coerced to \code{NA} (missing) rather than converted to \code{Not Reported}. In these
#' instances, a warning message is thrown. In instances where ethnicity codes
#' are detected, evaluation halts and an error message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#'
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
#' \code{decode_ethnicity} formats the \code{ETHNIC_CODES} field by evaluating a scalar or vector
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
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing ethnicity codes other than \code{1} or \code{2} are coerced to
#' \code{NA} (missing) rather than converted to \code{Not Reported}. In these
#' instances, a warning message is thrown. In instances where race codes
#' are detected, evaluation halts and an error message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{grepl}, \code{table}, \code{paste}, \code{gsub}
#'
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
#' \code{decode_ethnorace} formats both the \code{RACE_CODES} and \code{ETHNIC_CODES} fields by
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
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
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
#' \code{decode_department} converts all \code{*DEPARTMENT} code fields, e.g. \code{GRAD_DEPARTMENT},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more department codes, e.g. \code{BIO}, \code{PSY}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length department names. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized department codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
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
#' \code{decode_college} converts all \code{*COLLEGE} code fields, e.g. \code{GRAD_COLLEGE},
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
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized college codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
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
#' \code{decode_degree} converts all \code{*DEGREE} code fields, e.g. \code{GRAD_DEGREE},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more degree codes, e.g. \code{BA}, \code{MPP}, \code{PHD}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length degree names. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized degree codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
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
#' \code{decode_major} converts all \code{*MAJOR} code fields, e.g. \code{GRAD_MAJOR},
#' into full-length labels by evaluating a scalar or vector of values of class
#' character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more major codes, e.g. \code{CJ}, \code{AAS}, \code{LIT}, etc.
#'
#' @return A scalar or vector of values of class character containing
#' full-length major names. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized major codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
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



#' Convert Admission Codes to Full Descriptions
#'
#' \code{decode_admit} converts admission code fields into full-length, descriptive
#' labels by evaluating a scalar or vector of values of class character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more admission codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether descriptions should be converted to a "cleaner", more human-readable format.
#' Defaults to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length admission code descriptions. Missing values are preserved.
#'
#' @details Values containing unrecognized admission codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @export

decode_admit <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  a <- admit

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% a$admit_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% a$admit_code & clean == FALSE) {v[i] <- a[c[i] == a$admit_code, "admit_full"]}

    else if (!is.na(c[i]) & c[i] %in% a$admit_code & clean == TRUE) {v[i] <- a[c[i] == a$admit_code, "admit_clean"]}

  }

  return(unlist(v))

}



#' Convert Attribute Codes to Full Labels
#'
#' \code{decode_attribute} converts attribute code fields into full-length labels
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more attribute codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length attribute descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized attribute codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_attribute <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  a <- attribute

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% a$attribute_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% a$attribute_code & clean == FALSE) {v[i] <- a[c[i] == a$attribute_code, "attribute_full"]}

    else if (!is.na(c[i]) & c[i] %in% a$attribute_code & clean == TRUE) {v[i] <- a[c[i] == a$attribute_code, "attribute_clean"]}

  }

  return(unlist(v))

}



#' Convert Building Codes to Full Labels
#'
#' \code{decode_building} converts building code fields into full-length labels
#' by evaluating a scalar or vector of values of class character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more building codes.
#'
#' @return A scalar or vector of values of class character containing
#' full-length building descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized building codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_building <- function(code){

  c <- code
  v <- vector()
  b <- building

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% b$building_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% b$building_code) {v[i] <- b[c[i] == b$building_code, "building_full"]}

  }

  return(unlist(v))

}



#' Convert Campus Codes to Full Labels
#'
#' \code{decode_campus} converts campus code fields into full-length labels
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more campus codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length campus descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized campus codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_campus <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  j <- campus

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$campus_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$campus_code & clean == FALSE) {v[i] <- j[c[i] == j$campus_code, "campus_full"]}

    else if (!is.na(c[i]) & c[i] %in% j$campus_code & clean == TRUE) {v[i] <- j[c[i] == j$campus_code, "campus_clean"]}

  }

  return(unlist(v))

}



#' Convert County Codes to Full Names
#'
#' \code{decode_county} converts county code fields into full-length names
#' by evaluating a scalar or vector of values of class character. It also
#' provides the option to include a "County" label.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more county codes.
#'
#' @param label A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned county names should be labeled with "County". Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length county names Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized county codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_county <- function(code, label = FALSE){

  c <- code
  v <- vector()
  j <- county

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$county_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$county_code & label == FALSE) {v[i] <- j[c[i] == j$county_code, "county_full"]}

    else if (!is.na(c[i]) & c[i] %in% j$county_code & label == TRUE) {

      v[i] <- j[c[i] == j$county_code, "county_full"]

      v[i] <- paste(v[i], "County", sep = " ")

    }

  }

  return(unlist(v))

}



#' Convert Decision Codes to Full Descriptions
#'
#' \code{decode_decision} converts decision code fields into full-length descriptions
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more decision codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length decision descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized decision codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_decision <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  j <- decision

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$decision_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$decision_code & clean == FALSE) {v[i] <- j[c[i] == j$decision_code, "decision_full"]}

    else if (!is.na(c[i]) & c[i] %in% j$decision_code & clean == TRUE) {v[i] <- j[c[i] == j$decision_code, "decision_clean"]}

  }

  return(unlist(v))

}



#' Convert Email Codes to Full Descriptions
#'
#' \code{decode_email} converts email code fields into full-length descriptions
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more email codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length email descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized email codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_email <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  j <- email

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$email_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$email_code & clean == FALSE) {v[i] <- j[c[i] == j$email_code, "email_full"]}

    else if (!is.na(c[i]) & c[i] %in% j$email_code & clean == TRUE) {v[i] <- j[c[i] == j$email_code, "email_clean"]}

  }

  return(unlist(v))

}



#' Convert Grade Codes to Full Descriptions
#'
#' \code{decode_grade} converts grade code fields into full-length labels
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format. Due to duplications in the validation data, multiple values may be
#' returned. This function is currently in development.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more grade codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length grade descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized grade codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_grade <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  j <- grade

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$grade_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$grade_code & clean == FALSE) {v[i] <- j[c[i] == j$grade_code, "grade_full"]}

    else if (!is.na(c[i]) & c[i] %in% j$grade_code & clean == TRUE) {v[i] <- j[c[i] == j$grade_code, "grade_clean"]}

  }

  return(unlist(v))

}



#' Convert Institution Codes to Full Labels
#'
#' \code{decode_institution} converts institution code fields into full-length labels
#' by evaluating a scalar or vector of values of class character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more institution codes.
#'
#' @return A scalar or vector of values of class character containing
#' full-length institution labels. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized institution codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_institution <- function(code){

  c <- code
  v <- vector()
  j <- institution

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$institution_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$institution_code) {v[i] <- j[c[i] == j$institution_code, "institution_full"]}

  }

  return(unlist(v))

}



#' Convert Student Level Codes to Full Descriptions
#'
#' \code{decode_level} converts student level code fields into full-length descriptions
#' by evaluating a scalar or vector of values of class character. It also
#' allows automated "cleaning" of validated values in a more human-readable
#' format.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more level codes.
#'
#' @param clean A logical value (i.e. \code{TRUE} or \code{FALSE}) to
#' indicate whether returned values should be made more human-readable. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' full-length level descriptions. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized level codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_level <- function(code, clean = FALSE){

  c <- code
  v <- vector()
  j <- level

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$level_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i] & clean == FALSE) & c[i] %in% j$level_code) {v[i] <- j[c[i] == j$level_code, "level_full"]}

    else if (!is.na(c[i] & clean == TRUE) & c[i] %in% j$level_code) {v[i] <- j[c[i] == j$level_code, "level_clean"]}

  }

  return(unlist(v))

}



#' Convert State Codes to Full Names
#'
#' \code{decode_state} converts state code fields into full-length names
#' by evaluating a scalar or vector of values of class character.
#'
#' @param code A scalar or vector of length n and class character containing
#' one or more attribute codes.
#'
#' @return A scalar or vector of values of class character containing
#' full-length state names. Missing values are preserved.
#'
#' @details Output may be made categorical using function
#' \code{factor} or \code{as.factor}. This function also allows for easily
#' tallying counts using, e.g., function \code{table}.
#'
#' Values containing unrecognized state codes are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{table}
#'
#' @export

decode_state <- function(code){

  c <- code
  v <- vector()
  j <- state

  '%!in%' <- function(x,y)!('%in%'(x,y))

  for (i in seq_along(code)){

    if (is.na(c[i])){v[i] <- NA}

    if (!is.na(c[i]) & c[i] %!in% j$state_code){

      v[i] <- NA
      warning("One or more values passed to 'code =' was not recognized; coercing to NA", call. = F)

    }

    else if (!is.na(c[i]) & c[i] %in% j$state_code) {v[i] <- j[c[i] == j$state_code, "state_full"]}

  }

  return(unlist(v))

}



#' Round GPA Values Directionally
#'
#' \code{gpa_round} enhances the base R \code{round} function to allow directionality
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
#'
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
#' \code{gpa_discretize} optimizes base R function \code{cut} specifically for values
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
#'
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



#' Convert Dates to ISO Format
#'
#' \code{date_iso} converts date field values in \code{DD-MMM-YY} format to the
#' International Organization for Standardization (ISO) format, i.e. \code{YYYY-MM-DD}.
#'
#' Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
#' automatically convert to post-2000 C.E.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @return A scalar or vector of values of class character containing
#' standard ISO dates in \code{YYYY-MM-DD} format. Missing values are preserved.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' ISO dates are automatically recognized as dates in R and may be converted to
#' different formats using \code{as.*} coersion functions, e.g. \code{as.Date}.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{date}, \code{POSIXct}, \code{POSIXlt}
#'
#' @export

date_iso <- function(date){

  m <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  n <- c(paste0("0", 1:9), 10:12)

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) <= 30){

      day <- NA
      year <- NA
      month <- NA

      day[i] <- substr(x = d[i], start = 1, stop = 2)
      year[i] <- paste0("20", substr(x = d[i], start = 8, stop = 9))
      month[i] <- substr(x = d[i], start = 4, stop = 6)

      for (j in 1:12){

        month[i] <- gsub(x = month[i], m[j], n[j])

      }

      v[i] <- paste(year[i], month[i], day[i], sep = "-")

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) > 30){

      day <- NA
      year <- NA
      month <- NA

      day[i] <- substr(x = d[i], start = 1, stop = 2)
      year[i] <- paste0("19", substr(x = d[i], start = 8, stop = 9))
      month[i] <- substr(x = d[i], start = 4, stop = 6)

      for (j in 1:12){

        month[i] <- gsub(x = month[i], m[j], n[j])

      }

      v[i] <- paste(year[i], month[i], day[i], sep = "-")

    }

  }

  return(v)

}




#' Convert Dates to Full-Length Day, Month, Year
#'
#' \code{date_full} converts date field values in \code{DD-MMM-YY} format to long-form,
#' descriptive dates, including full-length years in \code{YYYY} format and
#' full-length months, with the option to "cleanly" abbreviate them. The option to
#' convert to the UK date format is also available.
#'
#' Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
#' automatically convert to post-2000 C.E.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @param abbreviate A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether to convert months to an abbreviated, 3-character format, e.g. "Aug." Defaults
#' to \code{FALSE}.
#'
#' @param uk.notation A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether to reorder day, month, and year to common UK notation. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing
#' long-form, descriptive dates including the day, full or abbreviated month, and 4-digit
#' year, in either US or UK notation.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @export

date_full <- function(date, abbreviate = FALSE, uk.notation = FALSE){

  m <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  f <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  a <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) <= 30){

      day <- NA
      year <- NA
      month <- NA

      day[i] <- substr(x = d[i], start = 1, stop = 2)
      year[i] <- paste0("20", substr(x = d[i], start = 8, stop = 9))
      month[i] <- substr(x = d[i], start = 4, stop = 6)

      if (abbreviate == FALSE){

        for (j in 1:12){

          month[i] <- gsub(x = month[i], m[j], f[j])

        }

      }

      if (abbreviate == TRUE){

        for (j in 1:12){

          month[i] <- gsub(x = month[i], m[j], a[j])

        }

      }

      if (uk.notation == TRUE){

        v[i] <- paste(day[i], month[i], year[i], sep = " ")

      }

      else if (uk.notation == FALSE){

        v[i] <- paste0(month[i], " ", day[i], ", ", year[i])

      }

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) > 30){

      day <- NA
      year <- NA
      month <- NA

      day[i] <- substr(x = d[i], start = 1, stop = 2)
      year[i] <- paste0("19", substr(x = d[i], start = 8, stop = 9))
      month[i] <- substr(x = d[i], start = 4, stop = 6)

      if (abbreviate == FALSE){

        for (j in 1:12){

          month[i] <- gsub(x = month[i], m[j], f[j])

        }

      }

      if (abbreviate == TRUE){

        for (j in 1:12){

          month[i] <- gsub(x = month[i], m[j], a[j])

        }

      }

      if (uk.notation == TRUE){

        v[i] <- paste(day[i], month[i], year[i], sep = " ")

      }

      else if (uk.notation == FALSE){

        v[i] <- paste0(month[i], " ", day[i], ", ", year[i])

      }

    }

  }

  return(v)

}



#' Convert Dates to Calendar Year
#'
#' \code{date_year_calendar} converts date field values in \code{DD-MMM-YY} format to full-length
#' calendar years in \code{YYYY} format.
#'
#' Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
#' automatically convert to post-2000 C.E.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @return A scalar or vector of values of class character containing full-length
#' calendar years in \code{YYYY} format.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @export

date_year_calendar <- function(date){

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) <= 30){

      v[i] <- paste0("20", substr(x = d[i], start = 8, stop = 9))

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) > 30){

      v[i] <- paste0("19", substr(x = d[i], start = 8, stop = 9))

    }

  }

  return(v)

}



#' Convert Dates to Calendar Month
#'
#' \code{date_month} converts date field values in \code{DD-MMM-YY} format to full-length
#' or abbreviated month names.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @param abbreviate A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether to convert months to an abbreviated, 3-character format, e.g. "Aug." Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing full-length
#' or abbreviated month names, e.g. "January" or "Jan", respectively.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @export

date_month <- function(date, abbreviate = FALSE){

  m <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  f <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  a <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    if (!is.na(d[i]) & abbreviate == FALSE){

      v[i] <- f[which(m == substr(d[i], 4, 6))]

    }

    if (!is.na(d[i]) & abbreviate == TRUE){

      v[i] <- a[which(m == substr(d[i], 4, 6))]

    }

  }

  return(v)

}



#' Convert Dates to Full-Length Month & Year
#'
#' \code{date_month_year} converts date field values in \code{DD-MMM-YY} format to full-length
#' or abbreviated month names and 4-digit years in \code{YYYY} format.
#'
#' Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
#' automatically convert to post-2000 C.E.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @param sep A single, quoted string of class character specifying which punctuation,
#' characters, or metacharacters are used to separate month and year. Defaults to
#' a single space, or \code{sep = " "}.
#'
#' @param abbreviate A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether to convert months to an abbreviated, 3-character format, e.g. "Aug." Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing full-length
#' or abbreviated month names, as well as a 4-digit year in \code{YYYY} format,
#' separated per user-specified delimiters.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @export

date_month_year <- function(date, sep = " ", abbreviate = FALSE){

  m <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  f <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  a <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) <= 30){

      if (!is.na(d[i]) & abbreviate == FALSE){

        v[i] <- paste(f[which(m == substr(d[i], 4, 6))],
                      paste0("20", substr(d[i], 8, 9)),
                      sep = sep)

      }

      if (!is.na(d[i]) & abbreviate == TRUE){

        v[i] <- paste(a[which(m == substr(d[i], 4, 6))],
                      paste0("20", substr(d[i], 8, 9)),
                      sep = sep)

      }

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) > 30){

      if (!is.na(d[i]) & abbreviate == FALSE){

        v[i] <- paste(f[which(m == substr(d[i], 4, 6))],
                      paste0("19", substr(d[i], 8, 9)),
                      sep = sep)

      }

      if (!is.na(d[i]) & abbreviate == TRUE){

        v[i] <- paste(a[which(m == substr(d[i], 4, 6))],
                      paste0("19", substr(d[i], 8, 9)),
                      sep = sep)

      }

    }

  }

  return(v)

}



#' Convert Dates to Academic Terms
#'
#' \code{date_term} converts date field values in \code{DD-MMM-YY} format to standard
#' academic terms in \code{YYYYMM} format. Dates falling within each term are assigned
#' to the existing semester, i.e. "Fall", "Spring", and "Summer" as \code{08},
#' \code{01}, and \code{05}, respectively. The option to "round up" to the subsequent
#' semester is also available.
#'
#' Notably, conversion to academic terms fluidly opens all \code{term*} family functions,
#' e.g. \code{term_year_fiscal}, in package \code{panthr}.
#'
#' Because the warehouse year format is \code{YY}, dates prior to 1930 C.E. will
#' automatically convert to post-2000 C.E.
#'
#' @param date A scalar or vector of length n and class character containing
#' one or more date values in \code{DD-MMM-YY} format.
#'
#' @param round.up A logical value (i.e. \code{TRUE} or \code{FALSE}) indicating
#' whether to convert date values to the subsequent academic term. Defaults
#' to \code{FALSE}.
#'
#' @return A scalar or vector of values of class character containing standardized
#' terms in \code{YYYYMM} format.
#'
#' @details Values passed to argument \code{date =} which are not in \code{DD-MMM-YY}
#' format or contain unrecognized month abbreviations are coerced to \code{NA}
#' (missing) values and a warning message is thrown.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{term_dash}, \code{term_date}, \code{term_name}, \code{term_season},
#' \code{term_year_academic}, \code{term_year_calendar}, \code{term_year_fiscal}
#'
#' @export

date_term <- function(date, round.up = FALSE){

  m <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  n <- c(paste0("0", 1:9), 10:12)

  d <- date
  v <- NA

  for (i in seq_along(d)){

    if (is.na(d[i])) {

      v[i] <- NA

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "[0-9]{2}-[A-Z]{3}-[0-9]{2}")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' are not in 'DD-MMM-YY' format; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & !grepl(x = d[i], pattern = "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) {

      v[i] <- NA
      warning("One or more values passed to 'date =' contain unrecognized month abbreviations; coercing to NA", call. = F)

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) <= 30){

      term <- NA
      year <- NA
      month <- NA

      year[i] <- paste0("20", substr(x = d[i], start = 8, stop = 9))
      month[i] <- n[which(m == substr(x = d[i], start = 4, stop = 6))]

      x <- month[i]

      if (round.up == FALSE){

        if (as.numeric(x) >= 1 & as.numeric(x) < 5){x <- "01"}
        if (as.numeric(x) >= 5 & as.numeric(x) < 8){x <- "05"}
        if (as.numeric(x) >= 8 & as.numeric(x) <= 12){x <- "08"}

        v[i] <- paste0(year[i], x)

      }

      if (round.up == TRUE){

        if (as.numeric(x) >= 1 & as.numeric(x) < 5){x <- "05"}
        if (as.numeric(x) >= 5 & as.numeric(x) < 8){x <- "08"}
        if (as.numeric(x) >= 8 & as.numeric(x) <= 12){

          x <- "01"

          year[i] <- as.character(as.numeric(year[i]) + 1)

          }

        v[i] <- paste0(year[i], x)

      }

    }

    else if (!is.na(d[i]) & as.numeric(substr(x = d[i], start = 8, stop = 9)) > 30){

      term <- NA
      year <- NA
      month <- NA

      year[i] <- paste0("19", substr(x = d[i], start = 8, stop = 9))
      month[i] <- n[which(m == substr(x = d[i], start = 4, stop = 6))]

      x <- month[i]

      if (round.up == FALSE){

        if (as.numeric(x) >= 1 & as.numeric(x) < 5){x <- "01"}
        if (as.numeric(x) >= 5 & as.numeric(x) < 8){x <- "05"}
        if (as.numeric(x) >= 8 & as.numeric(x) <= 12){x <- "08"}

        v[i] <- paste0(year[i], x)

      }

      if (round.up == TRUE){

        if (as.numeric(x) >= 1 & as.numeric(x) < 5){x <- "05"}
        if (as.numeric(x) >= 5 & as.numeric(x) < 8){x <- "08"}
        if (as.numeric(x) >= 8 & as.numeric(x) <= 12){

          x <- "01"

          year[i] <- as.character(as.numeric(year[i]) + 1)

          }

        v[i] <- paste0(year[i], x)

      }

    }

  }

  return(v)

}



#' Modify Separating Characters in Field Names
#'
#' \code{field_separator} accepts tabular data with column headers or field names
#' and modifies the separating characters within the column names. Because warehouse
#' field names are separated by underscores (\code{_}), this replaces underscores
#' with one or more user-defined characters or metacharacters.
#'
#'
#' @param data A \code{data.frame}, \code{matrix}, or other tabular object with
#' identifiable column headers or field names.
#'
#' @param sep A single character string in quotations specifying the character(s)
#' used to separate words in the column headers or field names. Defaults to a
#' single space, i.e. \code{sep = " "}.
#'
#' @return A \code{data.frame} object or other tabular data structure with modified
#' separators in column headers, as specified by the user in \code{sep =}.
#'
#' @details Objects passed to argument \code{data =} that are not tabular or, if
#' comprised of a single column, without column names will throw an error.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{gsub}, \code{names}, \code{colnames}, \code{ncol}
#'
#' @export

field_separator <- function(data, sep = " "){

  if (ncol(data) < 1 | is.null(ncol(data)) | is.vector(data)){

    stop("Argument 'data =' requires tabular data with at least one column header or named field", call. = FALSE)

  }

  else if (!is.null(ncol(data))) {

    colnames(data) <- gsub(x = colnames(data), pattern = "_", replacement = sep)

  }

  return(data)

}



#' Modify Case & Separators in Field Names
#'
#' \code{field_case} is a versatile function that accepts tabular data with column
#' headers or field names and modifies their case, separators and, for special
#' cases, initial letter capitalization.
#'
#' \code{field_case} automatically formats field names to all lowercase letters
#' and underscore separators (\code{_}). However, it supports both "lower", "upper",
#' and "title" cases, exotic cases such as "camel", "kebab", "snake", and "leopard",
#' and custom separators where applicable.
#'
#' @param data A \code{data.frame}, \code{matrix}, or other tabular object with
#' identifiable column headers or field names.
#'
#' @param case A single character string in quotations specifying the case in which
#' column headers or field names will convert. Acceptable parameters include \code{"lower"},
#' \code{"upper"}, \code{"title"}, \code{"camel"}, \code{"snake"}, \code{"leopard"},
#' and \code{"kebab"}. Defaults to \code{case = "lower"}.
#'
#' @param sep A single character string in quotations specifying the character(s)
#' used to separate words in column headers or field names. Defaults to a
#' single space, i.e. \code{sep = " "}.
#'
#' Conflicting parameters passed to \code{sep =} are overridden with exotic cases,
#' since they use specific separators by default (e.g. \code{"-"}, \code{"_"}, \code{"."})
#' or no separator in the case of \code{"camel"}. In such cases, a warning is thrown
#' alerting the user of the override.
#'
#' @param lower.case A logical value (\code{TRUE} or \code{FALSE}) indicating whether
#' all characters in one or more column headers or field names should be lowercase.
#' Defaults to \code{lower.case = TRUE}.
#'
#' Conflicting parameters passed to \code{lower.case =} may be overridden when conflicting
#' with parameters passed to argument \code{case =}, e.g. \code{"title"}, \code{"upper"},
#' and \code{"camel"}. This argument distinguishes "upper" and "lower" cases,
#' particularly for exotic cases, e.g. "upper snake case" and "lower snake case".
#'
#' @return A \code{data.frame} object or other tabular data structure with a modified
#' case and separators in column headers and field names specified by the user.
#'
#' @details Objects passed to argument \code{data =} that are not tabular or, if
#' comprised of a single column, without column names will throw an error.
#'
#' @author Jamison R. Crawford, Institutional Research Associate, Georgia State University
#'
#' @seealso \code{gsub}, \code{names}, \code{colnames}, \code{ncol}, \code{tolower},
#' \code{toupper}, \code{toTitleCase}
#'
#' @export

field_case <- function(data, case = "lower", sep = "_", lower.case = TRUE){

  if (ncol(data) < 1 | is.null(ncol(data)) | is.vector(data)){

    stop("Argument 'data =' requires tabular data with at least one column header or named field", call. = FALSE)

  }

  if (sum(grepl(x = c("lower", "upper", "title", "camel", "snake", "leopard", "kebab"), pattern = case)) == 0){

    stop("Argument 'case =' contains an unrecognizable string; run 'help(field_case)' to see available cases", call. = FALSE)

  }

  if (case == "camel" & sep != ""){

    warning("Argument 'sep =' has been overridden for 'camel' case", call. = FALSE)

  }

  if (case == "camel" & lower.case == TRUE){

    warning("Argument 'lower.case =' has been overridden for 'camel' case", call. = FALSE)

  }

  if (case == "kebab" & sep != "-"){

    warning("Argument 'sep =' has been overridden for 'kebab' case", call. = FALSE)

  }

  if (case == "snake" & sep != "_"){

    warning("Argument 'sep =' has been overridden for 'snake' case", call. = FALSE)

  }

  if (case == "leopard" & sep != "."){

    warning("Argument 'sep =' has been overridden for 'leopard' case", call. = FALSE)

  }

  if (case == "upper" & lower.case == TRUE){

    warning("Argument 'lower.case =' has been overridden for 'upper' case", call. = FALSE)

  }

  if (case == "title" & lower.case == TRUE){

    warning("Argument 'lower.case =' has been overridden for 'title' case", call. = FALSE)

  }

  if (case == "lower" & lower.case == FALSE){

    warning("Argument 'lower.case =' has been overridden for 'lower' case", call. = FALSE)

  }

  if (case == "camel"){

    colnames(data) <- gsub(x = tools::toTitleCase(tolower(gsub(x = colnames(data),
                                                               pattern = "_",
                                                               replacement = " "))),
                           pattern = " ",
                           replacement = "")

  }

  if (case == "snake"){

    colnames(data) <- gsub(x = tools::toTitleCase(tolower(gsub(x = colnames(data),
                                                               pattern = "_",
                                                               replacement = " "))),
                           pattern = " ",
                           replacement = "_")

  }

  if (case == "kebab"){

    colnames(data) <- gsub(x = tools::toTitleCase(tolower(gsub(x = colnames(data),
                                                               pattern = "_",
                                                               replacement = " "))),
                           pattern = " ",
                           replacement = "-")

  }

  if (case == "leopard"){

    colnames(data) <- gsub(x = tools::toTitleCase(tolower(gsub(x = colnames(data),
                                                               pattern = "_",
                                                               replacement = " "))),
                           pattern = " ",
                           replacement = ".")

  }

  if (case == "title"){

    colnames(data) <- gsub(x = tools::toTitleCase(tolower(gsub(x = colnames(data),
                                                               pattern = "_",
                                                               replacement = " "))),
                           pattern = " ",
                           replacement = sep)

  }

  if (case == "upper"){

    colnames(data) <- gsub(x = toupper(colnames(data)), pattern = "_", replacement = sep)

  }

  if (case == "lower"){


    colnames(data) <- gsub(x = tolower(colnames(data)), pattern = "_", replacement = sep)


  }

  if (lower.case == TRUE & case != "camel" & case != "title" & case != "upper") {

    colnames(data) <- tolower(colnames(data))

  }

  return(data)

}
