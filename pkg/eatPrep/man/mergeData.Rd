\name{mergeData}
\alias{mergeData}
\title{Merge Data Frames using one Key Variable}
\description{Merges several data frames and matches them using one key variable}
\usage{
mergeData(newID = "ID", datList, oldIDs=NULL, addMbd = FALSE, writeLog=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newID}{
character string containing the key variable's name in the merged dataset
}
  \item{datList}{
list of data frames to be merged
}
  \item{oldIDs}{
character vector OR numeric vector containing either names of the key variables in datList or their column number in each dataframe in datList
default is a vector containing replicates of the value of \code{newID}.
}
  \item{addMbd}{
logical; string "mbd" (missing by desgin) will be added instead of NA
}
  \item{writeLog}{
logical; if Logfile shall be written via \code{eatTools:::sunk}.
}
}
%\details{
%Errors and warnings of \code{mergeData} will be written to a protocol file with
%\code{eatTools:::sunk}.
%}
\value{
A data frame containing unique cases and unique variables. All cases and all variables that could be identified the original data frames will be kept and matched.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Karoline Sachse, Nicole Haag
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(inputDat)
str(inputDat)

mergedDataset <- mergeData("person-id", inputDat, c("idstud", "idstud", "idstud"), addMbd=TRUE)
str(mergedDataset)

mergedDataset <- mergeData("idstud", inputDat, writeLog=FALSE)
str(mergedDataset)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
