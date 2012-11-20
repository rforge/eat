\name{dichotomize}
\alias{dichotomize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dichotomize a numeric vector 
}
\description{
dichotomize a numeric vector by median or mean split
}
\usage{
dichotomize ( numvec , method = c("median","mean") , randomize = TRUE , ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{numvec}{ numeric vector }
  \item{method}{ either median or mean split }
  \item{randomize}{ logical, if \code{TRUE} elements that equal the split threshold are randomly assigned to one of the two groups
							 if \code{FALSE} default behavior of cut is used}
  \item{...}{ arguments are passed to \link[base]{set.seed} and \link[base]{cut} }
  
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns vector with dichotomization indicators

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
numvec <- c(1,2,3,4,5)
dichotomize ( numvec )

# set seed for random assignment of elements that match split threshold by passing argument 'seed' to function set.seed
# ( '3' in numvec is on threshold if median is used )
dichotomize ( numvec , seed = 12345 )

# set level names by passing argument 'labels' to cut function
dichotomize ( numvec , labels = c ( "low" , "high") )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
