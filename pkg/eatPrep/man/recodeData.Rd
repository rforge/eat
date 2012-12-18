\name{recodeData}
\alias{recodeData}

\title{Recode Datasets with Missing Values
}
\description{Recode datasets with special consideration of missing values.
}
\usage{
recodeData(dat, values, subunits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{A data frame}
  \item{values}{A data frame with code information. See 'Details'.}
  \item{subunits}{A data frame with subunit information. See 'Details'.}
}
\details{
\code{recodeData} recodes data frames with special consideration of missing values. The results of \code{recodeData} will be written to a protocol file with \code{sunk}. \code{recodeData} will give warnings, if missing or incomplete recode informations are found. Values without recode information will NOT be recoded!

Examples of data frames \code{values} and \code{subunits} can be found via \code{data(inputList)}

}
\value{
A data frame with recoded variables according to the specifications in \code{values} and \code{subunits}. Colnames will be the names specified in \code{subunits$subunitRecoded}. 
}
\references{
http://code.google.com/p/zkdlib/wiki/MissingHandling
}
\author{
Martin Hecht, Christiane Penk, Nicole Haag
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{aggregateData}}, \code{\link{checkData}}
}
\examples{
data(inputDat)
data(inputList)

dat1 <- inputDat[[1]]  # get first dataset from inputDat
datRec <- recodeData(dat1, inputList$values, inputList$subunits)
str(datRec)

}
