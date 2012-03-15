\name{rmNA}
\alias{rmNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
remove NA columns and rows from data
}
\description{
remove columns and rows that are completely NA from data.frame or matrix
}
\usage{
rmNA ( dat , remove = TRUE , verbose = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame or matrix }
  \item{remove}{ if \code{TRUE} columns and rows are removed, if \code{FALSE} a list of identified columns and rows is returned }
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
\code{\link{rmNAcols}}, \code{\link{rmNArows}}
}

\examples{
( mat <- matrix( c( 1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) , ncol=6 , byrow=TRUE ) )
( rmNA ( mat , verbose = TRUE ) )
( rmNA ( mat , remove = FALSE ) )
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
