\name{make.dummies}
\alias{make.dummies}

\title{
Create Dummy Variables from a Data Frame
}

\description{
Create dummy variables using \link[psych]{dummy.code} from the \code{psych} package. The dummy variables' names can be customized and the variables can be added to the input data frame.
}

\usage{
make.dummies(dat, cols, colname.as.prefix = TRUE, delimiter = ".", 
capitalize = FALSE, nchar = NULL, add = TRUE, sort.into.dat = TRUE, 
oneToColname = FALSE, zeroToNA = FALSE)
}

\arguments{
  \item{dat}{ A data frame }
  \item{cols}{colnames of variables to be dummy coded }
  \item{colname.as.prefix}{ Logical: If \code{TRUE} the original variable name is added as prefix }
  \item{delimiter}{ A character string by which the variable name and the level name will be separated (only evaluated if \code{colname.as.prefix = TRUE}) }
  \item{capitalize}{Logical: If \code{TRUE} the level names are capitalized }
  \item{nchar}{ Number of characters the level names should be truncated to}
  \item{add}{Logical: If \code{TRUE} the dummy variables are appended to \code{dat} }
  \item{sort.into.dat}{Logical: If \code{TRUE} (and \code{add = TRUE}) the dummy variables are added and sorted into \code{dat} according to their column names }
  \item{oneToColname}{Logical: If \code{TRUE}, the values of cases with a value of 1 on the dummy variable are set to the colname of respective column. This changes the column class from \code{numeric} to \code{character}. }
  \item{zeroToNA}{Logical: If \code{TRUE}, the values of cases with a value of 0 on the dummy variable are set to \code{NA}.}
}
\details{
}
\value{
A data frame with dummy variables. Depending on \code{add} the returned object contains either the original data frame with the dummy variables appended or only the dummy variables.
}

\author{
Martin Hecht
}

\examples{
\dontrun{
data(science1)

science1.dum <- make.dummies(science1, c("sex","booklet"))
str(science1.dum[,3:10])

science1.dum <- make.dummies(science1, c("sex","booklet"), nchar = 1)
str(science1.dum[,3:10])

science1.dum <- make.dummies(science1, c("sex","booklet"), delimiter = "_")
str(science1.dum[,3:10])

science1.dum <- make.dummies(science1, c("sex","booklet"), delimiter = "", capitalize = TRUE)
str(science1.dum[,3:10]) 

science1.dum <- make.dummies(science1, c("sex","booklet"), colname.as.prefix = FALSE)
str(science1.dum[,3:10]) 

science1.dum <- make.dummies(science1, c("sex","booklet"), sort.into.dat = FALSE)
str(science1.dum[ , (ncol(science1.dum)-5):ncol(science1.dum)])

science1.dum <- make.dummies(science1, c("sex","booklet"), add = FALSE)
str(science1.dum)

science1.dum <- make.dummies(science1, c("sex","booklet"), oneToColname = TRUE, zeroToNA = TRUE)
str(science1.dum)

}
}
