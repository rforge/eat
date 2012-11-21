\name{checkData}
\alias{checkData}
\title{Check Datasets for Missing Values and Invalid Codes}
\description{Check data frames for missing or duplicated entries in the ID variable, persons and/or variables without valid codes, and invalid codes. Invalid codes are codes which are not specified in table \code{values}. }
\usage{checkData (dat, values, subunits, units)}
\arguments{
  \item{dat}{A data frame}
  \item{values}{A data frame with code information. See 'Details'.}
  \item{subunits}{A data frame with subunit information. See 'Details'.}
  \item{units}{A data frame with unit information. See 'Details'.}
}
\details{
The results of \code{checkData} will be written to a protocol file with \code{eatTools:::sunk}. 

Examples of data frames \code{values}, \code{subunits} and \code{units} can be found via \code{data(inputList)}.
}
\value{
Used for its side effects. The return value is NULL.
}
\references{
For missing types see http://code.google.com/p/zkdlib/wiki/MissingHandling
}
\author{Nicole Haag, Anna Lenski}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{eatTools:::sunk}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
