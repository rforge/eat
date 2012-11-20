\name{rmNAcols}
\alias{rmNAcols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
remove NA columns from data
}
\description{
remove columns that are completely or partially NA from data.frame or matrix
}
\usage{
rmNAcols ( dat , rows = NULL , tolerance = 0 , cumulate = TRUE , remove = TRUE , verbose = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame or matrix }
  \item{rows}{ rows to include, can be a list of vectors to specify row subsets }  
  \item{tolerance}{ number of non-NA cells that are "tolerated", can be a list corresponding to \code{rows} }
  \item{cumulate}{ if \code{TRUE}, \code{tolerance} is cumulated; if \code{FALSE}, exact \code{tolerance} is used }  
  \item{remove}{ if \code{TRUE}, columns and rows are removed; if \code{FALSE}, identified columns are returned }
  \item{verbose}{ if \code{TRUE} removed columns and rows are printed on output window }
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on option \code{remove}
}
\references{

}
\author{
Martin Hecht
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
calls \code{\link{rmNA}} and \code{\link{rmNArows}}
}

\examples{
# example matrix
( mat <- matrix( c( 1,1,1,1,1,1, 1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA) , ncol=7 ) )

# remove column with entirely NA (column 7)
rmNAcols( mat , verbose = TRUE )

# remove column with NA on rows 3, 4, 5 (columns 5, 6, 7)
rmNAcols( mat , c(3,4,5) , verbose = TRUE ) 
rmNAcols( mat , c(-1,-2,-6) , verbose = TRUE ) 

# tolerance=1 , 1 non-NA is permitted (columns 6 and 7)
rmNAcols( mat , tolerance=1 , verbose = TRUE ) 

# tolerance=6 , 6 non-NA are permitted (all columns are removed)
rmNAcols( mat , tolerance=6 , verbose = TRUE ) 

# do not cumulate / exact tolerance (column 1)
rmNAcols( mat , tolerance=6 , cumulate=FALSE , verbose = TRUE ) 

# two subsets of rows
rmNAcols( mat , rows = list( c(1, 2), c(4, 5) ) , verbose = TRUE )

# two subsets of rows with different tolerance
rmNAcols( mat , rows = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , verbose = TRUE )

# identify cols, no deletion
rmNAcols( mat , rows = list( c(1, 2), c(3, 4, 5) ) , tolerance = list( 0 , 1 ) , remove = FALSE )
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
