\name{rmNArows}
\alias{rmNArows}
\title{
remove NA rows from data
}
\description{
remove rows that are completely or partially NA from data.frame or matrix
}
\usage{
rmNArows(dat, cols = NULL, tolerance = 0, cumulate = TRUE, 
        remove = TRUE, verbose = FALSE)
}
\arguments{

  \item{dat}{ data.frame or matrix }
  \item{cols}{ columns to include, can be a list of vectors to specify column subsets }  
  \item{tolerance}{ number of non-NA cells that are "tolerated", can be a list corresponding to \code{cols} }
  \item{cumulate}{ if \code{TRUE}, \code{tolerance} is cumulated; if \code{FALSE}, exact \code{tolerance} is used }  
  \item{remove}{ if \code{TRUE}, columns and rows are removed; if \code{FALSE}, identified rows are returned }
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
calls \code{\link{rmNA}} and \code{\link{rmNAcols}}
}

\examples{
# example matrix
(mat <- matrix(c( 1,1,1,1,1, 1,1,1,1,NA, 1,1,1,NA,NA, 1,1,NA,NA,NA, 
  1,NA,NA,NA,NA, NA,NA,NA,NA,NA), ncol=5, byrow=TRUE))

# remove row with entirely NA (row 6)
rmNArows(mat, verbose = TRUE)

# remove row with NA on column 3, 4, 5 (rows 4, 5, 6)
rmNArows(mat, c(3,4,5), verbose = TRUE) 
rmNArows(mat, c(-1,-2), verbose = TRUE) 

# tolerance=1 , 1 non-NA is permitted (rows 5 and 6)
rmNArows(mat, tolerance=1, verbose = TRUE) 

# tolerance=5 , 5 non-NA are permitted (all rows are removed)
rmNArows(mat, tolerance=5, verbose = TRUE) 

# do not cumulate / exact tolerance (row 1 is removed)
rmNArows(mat, tolerance=5, cumulate=FALSE, verbose = TRUE) 
rmNArows(mat, tolerance=5, cumulate=FALSE, remove = FALSE) 

# two subsets of columns
rmNArows(mat, cols = list(c(1, 2), c(4, 5)), verbose = TRUE)

# two subsets of columns with different tolerance
rmNArows(mat, cols = list(c(1), c(2, 3, 4, 5)), tolerance = list(0, 1), verbose = TRUE)

# identify rows, no deletion
rmNArows(mat, cols = list(c(1), c(2, 3, 4, 5)), tolerance = list(0, 1), remove = FALSE)
}
