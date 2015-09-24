\name{checkData}
\alias{checkData}
\title{Check Datasets for Missing Values and Invalid Codes}
\description{Check data frames for missing or duplicated entries in the ID variable, persons and/or variables without valid codes, and invalid codes. Invalid codes are codes which are not specified in table \code{values}. }
\usage{checkData(dat, datnam, values, subunits, units, verbose = TRUE)}
\arguments{
  \item{dat}{A data frame to be checked.}
  \item{datnam}{Name of the data frame. Especially useful if more than one data frame is checked (e.g., in a list of data frames).}
  \item{values}{A data frame with code information. See \sQuote{Details}.}
  \item{subunits}{A data frame with subunit information. See \sQuote{Details}.}
  \item{units}{A data frame with unit information. See \sQuote{Details}.}
    \item{verbose}{Logical. If \code{FALSE} no information is printed.}
}
\details{
The results of \code{checkData} will be written to the console. 

Examples of data frames \code{values}, \code{subunits} and \code{units} can be found via \code{data(inputList)}.
}

\author{Nicole Haag, Anna Lenski, Karoline Sachse}
