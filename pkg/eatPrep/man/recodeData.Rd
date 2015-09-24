\name{recodeData}
\alias{recodeData}

\title{Recode Datasets with Several Kinds of Missing Values
}
\description{Recode datasets with special consideration of missing values. See \code{\link{collapseMissings}} for supported types of missing values.
}
\usage{
recodeData(dat, values, subunits, verbose = FALSE)
}
\arguments{
  \item{dat}{A data frame}
  \item{values}{A data frame with code information. See \sQuote{Details}.}
  \item{subunits}{A data frame with subunit information. See \sQuote{Details}.}
   \item{verbose}{Logical. If \code{TRUE} additional information is printed.
}
}
\details{
\code{recodeData} recodes data frames with special consideration of missing values. \code{recodeData} will give warnings, if missing or incomplete recode informations are found. Values without recode information will not be recoded.

Examples of data frames \code{values} and \code{subunits} can be found via \code{data(\link{inputList})}.

}
\value{
A data frame with recoded variables according to the specifications in \code{values} and \code{subunits}. The columns will be named according to the specifications in \code{subunits$subunitRecoded}. 
}
\author{
Martin Hecht, Christiane Penk, Nicole Haag
}

\seealso{
	\code{\link{aggregateData}},  \code{\link{automateDataPreparation}}, \code{\link{inputList}}
}
\examples{
data(inputDat)
data(inputList)

dat1 <- inputDat[[1]]  # get first dataset from inputDat
datRec <- recodeData(dat1, inputList$values, inputList$subunits, verbose = TRUE)
str(datRec)
}
