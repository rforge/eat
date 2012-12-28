\name{mergeData}
\alias{mergeData}
\title{Merge Data Frames and fill NA gaps}
\description{Merges data frames using one key variable. Fills NA gaps.}
\usage{
mergeData(newID = "ID", datList, oldIDs=NULL, addMbd = FALSE, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newID}{
character string containing the ID variable's name in the future merged dataset
}
  \item{datList}{
list of data frames to be merged
}
  \item{oldIDs}{
character vector OR numeric vector containing either names of the key variables in datList or their column number in each dataframe in datList
Default is a vector containing replicates of the value of \code{newID}.
}
  \item{addMbd}{
logical. If \code{TRUE} \code{NA} is replaced by "mbd" (missing by desgin).
}
  \item{verbose}{
logical. If \code{TRUE} progress is printed.
}
}
%\details{
%}
\value{
a data frame containing unique cases and unique variables. All cases and all variables that could be identified in the original data frames will be kept and matched.
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

mergedDataset <- mergeData("idstud", inputDat, c("ID", "ID", "ID"), addMbd=TRUE)
str(mergedDataset)

mergedDataset <- mergeData("ID", inputDat, verbose=TRUE)
str(mergedDataset)
}
