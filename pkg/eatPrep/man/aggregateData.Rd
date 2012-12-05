\name{aggregateData}
\alias{aggregateData}
\title{Aggregate Datasets with Missing Values}
\description{Aggregates datasets with constraints on missing values}
\usage{
aggregateData(dat, subunits, units, aggregatemissings = "use.default", rename = FALSE, recodedData = TRUE)
}
\arguments{
  \item{dat}{A data frame.}
  \item{subunits}{A data frame with subunit information. See 'Details'.}
  \item{units}{A data frame with unit information. See 'Details'.}
  \item{aggregatemissings}{Either the character string \code{"use.default"} or a \emph{n x n} matrix with information on how missing values should be aggregated. See 'Details'.}
  \item{rename}{Should units with only one subunit be renamed to their unit name? Default is \code{FALSE}.}
  \item{recodedData}{Logical indicating whether colnames in dataset to aggregate are the subunit names (as in \code{subunits$subunit}) or recoded subunit names (as in \code{subunits$subunitRecoded}). Default is \code{TRUE}, meaning that colnames are recoded subitem names.}
}
\details{
\code{aggregateData} aggregates units in data frames with special consideration of missing values. 
The aggregation of missing values is specified in argument \code{aggregatemissings}. 

The results of \code{aggregateData} will be written to a protocol file with \code{sunk}. 

Examples of data frames \code{subunits} and \code{units} can be found via \code{data(inputList)}.
}

\value{A data frame with aggregated units and, if \code{rename = TRUE}, renamed subunits.
}
\references{
For missing types see http://code.google.com/p/zkdlib/wiki/MissingHandling
}
\author{Nicole Haag, Anna Lenski
}
%\note{
%%  ~~further notes~~
%}

\section{Warning}{Missings are only correctly aggregated if their values correspond to the values given in \code{aggregatemissings}. \code{aggregateData} does not check for value types or whether codes are valid. Use of \code{checkData} and \code{recodeData} before using \code{aggregateData} is therefore strongly recommended.}

\seealso{
	\code{\link{recodeData}}, \code{\link{checkData}}
}
	
\examples{
	data(inputDat)
	data(inputList)

	dat1 <- inputDat[[1]]  # get first dataset from inputDat
	datRec <- recodeData(dat1, inputList$values, inputList$subunits) # recode Data first
	datAggr <- aggregateData(datRec, inputList$subunits, inputList$units, rename = TRUE, recodedData = TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
