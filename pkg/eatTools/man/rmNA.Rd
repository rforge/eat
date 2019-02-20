\name{rmNA}
\alias{rmNA}
\title{
Remove Columns and Rows with Missing Data
}
\description{
Remove columns and rows that contain only missing values from a data frame or a matrix
}
\usage{
rmNA(dat, remove = TRUE, verbose = FALSE)
}
\arguments{

  \item{dat}{ A data frame or a matrix }
  \item{remove}{ if \code{TRUE} columns and rows containing only missing values are removed, if \code{FALSE} a list of identified columns and rows is returned}
  \item{verbose}{ if \code{TRUE} the removed columns and rows are printed on the console. }
  
}
%\details{
%}
\value{
Either a list indicating which columns and which rows in the data contain only missing values or a data frame or matrix in which all columns and rows containing only missing values are removed.
}
\author{
Martin Hecht
}
\seealso{
\code{\link{rmNAcols}}, \code{\link{rmNArows}}
}

\examples{
(mat <- matrix(c(1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 
               1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA), ncol=6, byrow=TRUE))
rmNA(mat, verbose = TRUE)
rmNA(mat, remove = FALSE)
}

