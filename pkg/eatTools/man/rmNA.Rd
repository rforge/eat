\name{rmNA}
\alias{rmNA}
\title{
remove NA columns and rows from data
}
\description{
remove columns and rows that are completely NA from data.frame or matrix
}
\usage{
rmNA(dat, remove = TRUE, verbose = FALSE)
}
\arguments{

  \item{dat}{ data.frame or matrix }
  \item{remove}{ if \code{TRUE} columns and rows are removed, if \code{FALSE} a list of identified columns and rows is returned }
  \item{verbose}{ if \code{TRUE} removed columns and rows are printed on output window }
  
}
\details{
}
\value{
depends on option \code{remove}
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

