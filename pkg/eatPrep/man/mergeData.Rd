\name{mergeData}
\alias{mergeData}
\title{Merge Data Frames, Check For Inconsistencies, and Replace NA Values}
\description{This function merges a list of data frames, checks whether cases with the same value on the identifier variable have different values in different datasets. Additionally, \code{NA} resulting from the merge (e.g., in repeated block designs) can be replaced with a customed character missing to facilitate future data preparation of the merged dataset. See \code{\link{collapseMissings}} for details on supported character missings for other functions in the \code{eatPrep} package. 
}
\usage{
mergeData(newID = "ID", datList, oldIDs=NULL, addMbd = FALSE, verbose=FALSE)
}

\arguments{
  \item{newID}{A character vector of length 1 indicating the name of the identifier variable in the merged dataset.
}
  \item{datList}{
A list of data frames to be merged
}
  \item{oldIDs}{
A character vector or a numeric vector containing either the names of the identifier variables in datList or their column number in each data frame in datList. Default is a vector containing \code{length(datList)} replicates of the value of \code{newID}.
}
  \item{addMbd}{
logical. If \code{TRUE}, \code{NA} is replaced by "mbd" (missing by design) in the merged dataset.
}
  \item{verbose}{
logical. If \code{TRUE}, progress is printed.
}
}
%\details{
%}
\value{
A data frame containing unique cases and unique variables. All cases and all variables from the original datasets will be kept and matched.
}
\author{Karoline Sachse, Nicole Haag
}

\examples{
data(inputDat)
str(inputDat)

mergedDataset <- mergeData("idstud", inputDat, c("ID", "ID", "ID"), addMbd=TRUE)
str(mergedDataset)

mergedDataset <- mergeData("ID", inputDat, verbose=TRUE)
str(mergedDataset)
}
