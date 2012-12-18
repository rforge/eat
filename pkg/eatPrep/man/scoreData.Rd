\name{scoreData}
\alias{scoreData}

\title{Score Datasets with Missing Values
}
\description{Score datasets with special consideration of missing values.
}
\usage{
scoreData(dat, unitrecodings, units)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A data frame}
  \item{unitrecodings}{A data frame with information about the scoring of units. See 'Details'.}
  \item{units}{A data frame with unit information. See 'Details'.}
}
\details{
\code{scoreData} scored data frames with special consideration of missing values. This function is very similar to \code{recodeData}, but with a few defaults that are sensible for scoring. \code{scoreData} will give warnings when incomplete scoring informations are found. Values without scoring information will NOT be scored!

Examples of data frames \code{unitrecodings} and \code{units} can be found via \code{data(inputList)}

}
\value{
A data frame with scored variables according to the specifications in \code{unitrecodings} and \code{units}. 
}
\references{
http://code.google.com/p/zkdlib/wiki/MissingHandling
}
\author{
Nicole Haag
}

\seealso{
	\code{\link{recodeData}}, \code{\link{automateDataPreparation}}
}
\examples{
data(inputDat)
data(inputList)

prepDat <- automateDataPreparation (inputList = inputList, datList = inputDat,
           readSpss = FALSE, checkData=FALSE, mergeData = TRUE, recodeData=TRUE, 
  	       aggregateData=TRUE, scoreData= FALSE, writeSpss=FALSE)
           
datSco <- scoreData(prepDat, inputList$unitRecodings, inputList$units)
str(datSco)

}
