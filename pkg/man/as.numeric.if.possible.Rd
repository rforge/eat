\name{as.numeric.if.possible}
\alias{as.numeric.if.possible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform columns of a data.frame into numeric values if possible
}
\description{
In contrast to \code{as.numeric}, Function transforms only "transformable" columns of a data.frame into numeric values 
(i.e. without creating \code{NA} when transformation fails. Non-transformable columns are maintained. Optionally, 
only a logical vector is given, indicating which columns are transformable.
}
\usage{
as.numeric.if.possible ( dataFrame, set.numeric = TRUE, transform.factors = FALSE, maintain.factor.scores = TRUE, verbose = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dataFrame}{ A data.frame which columns should be transformed. }
  \item{set.numeric}{ Logical: If \code{TRUE}, data.frame with transformed columns is returned. If \code{FALSE}, a
                      logical vector is returned, indicating which columns are transformable.  }
  \item{transform.factors }{ Logical: Should columns of class \code{factor} transformed? If \code{FALSE}, columns of class \code{factor} are maintained. If \code{TRUE}, columns of class \code{factor} are attempted to transform.   } 
  \item{maintain.factor.scores}{ Logical. Only relevant if \code{transform.factors = TRUE}. If \code{TRUE}, the nominal values of the 
                                 factor are transformed if possible. If \code{FALSE}, the integer numbers representing the factors' nominal values are returned. See details.  }
  \item{verbose}   {Logical: If \code{TRUE}, informations about the class of the columns in the data.frame are printed to the console.}

}
\details{
In R, factors may represent ordered categories or nominal variables. Depending on the meaning of the variable, a transformation 
of the nominal values (of a factor variable) to numeric values may be desirable or not. The arguments \code{transform.factors} 
and \code{maintain.factor.scores} serve to specify if and how factor variables should be transformed. See examples. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Either a logic vector, indicating which columns in teh data.frame are transformable according to the specified conditions, or
a data.frame in which transformable columns are transformed. 
}
\references{

}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
   \code{
   
   ( dat <- data.frame( X1=c("1",NA,"0"), X2=c("a",NA,"b"), X3=c(TRUE,FALSE,FALSE),  X4=as.factor(c("a",NA,"b")), X5=as.factor(c("5","6","7")) , stringsAsFactors=FALSE) )
   str(dat)
   as.numeric.if.possible(dat)
   as.numeric.if.possible(dat, transform.factors=T, maintain.factor.scores=F)
   as.numeric.if.possible(dat, transform.factors=T, maintain.factor.scores=T)
  
   }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
