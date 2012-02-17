\name{set.col.type}
\alias{set.col.type}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
set type of variable in data.frame
}
\description{
converts type of column(s) to "character" , "numeric" , "logical" , "integer" or "factor" 
}
\usage{
set.col.type ( data , col.type = list ( "character" = NULL ) , verbose = FALSE , ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{ data.frame }
  \item{col.type}{ named list of variable names that are to be converted. names of list is conversion type ( "character" , "numeric" , "numeric.if.possible" , "logical" , "integer" or "factor" ) }
  \item{verbose}{ if \code{TRUE} variables that have been converted are printed }
%%  \item{...}{arguments to be passed to \link{asNumericIfPossible}}
  \item{...}{arguments to be passed to asNumericIfPossible}
}
\details{
%% use \code{col.type="numeric.if.possible"} if conversion to numeric should be tested upfront, see \link{asNumericIfPossible} for details
use \code{col.type="numeric.if.possible"} if conversion to numeric should be tested upfront, see asNumericIfPossible for details
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

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
}

\examples{
str ( d <- data.frame ( "var1" = 1 , "var2" = TRUE , "var3" = FALSE , "var4" = as.factor ( 1 ) , "var5" = as.factor ( "a" ) ,"var6" = "b" , stringsAsFactors = FALSE ) )
str ( set.col.type ( d ) )
str ( set.col.type ( d , list ( "numeric" = NULL ) ) )
str ( set.col.type ( d , list ( "character" = c ( "var1" , "var2" ) , "numeric" = "var3" , "logical" = "var4" ) ) )
str ( set.col.type ( d , list ( "numeric.if.possible" = NULL ) ) )
str ( set.col.type ( d , list ( "numeric.if.possible" = NULL ) , transform.factors = TRUE ) )
str ( set.col.type ( d , list ( "numeric.if.possible" = NULL ) , transform.factors = TRUE , maintain.factor.scores = FALSE ) )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
