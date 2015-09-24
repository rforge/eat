\name{scoreData}
\alias{scoreData}

\title{Score Datasets with Several Kinds of Missing Values
}
\description{Score datasets with special consideration of missing values.
}
\usage{
scoreData(dat, unitrecodings, units, verbose = FALSE)
}
\arguments{
  \item{dat}{A data frame}
  \item{unitrecodings}{A data frame with information about the scoring of units. See \sQuote{Details}.}
  \item{units}{A data frame with unit information. See \sQuote{Details}.}
  \item{verbose}{logical. If \code{TRUE} additional information is printed.}
}
\details{
This function is very similar to \code{recodeData}, but with a few defaults that are better suited for scoring. \code{scoreData} will give warnings when incomplete scoring informations are found. Values without scoring information will not be scored.

Examples of data frames \code{unitrecodings} and \code{units} can be found via \code{data(\link{inputList})}.

}
\value{
A data frame with scored variables according to the specifications in \code{unitrecodings} and \code{units}. 
}
\author{
Nicole Haag
}

\seealso{
	\code{\link{recodeData}}, \code{\link{automateDataPreparation}}, \code{\link{inputList}}
}
\examples{
data(inputDat)
data(inputList)

prepDat <- automateDataPreparation (inputList = inputList, datList = inputDat,
    readSpss = FALSE, checkData=FALSE, mergeData = TRUE, recodeData=TRUE, 
    aggregateData=TRUE, scoreData= FALSE, writeSpss=FALSE, verbose = TRUE)
           
datSco <- scoreData(prepDat, inputList$unitRecodings, inputList$units, 
    verbose = TRUE)
str(datSco)

}
