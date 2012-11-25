\name{multiseq}
\alias{multiseq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
multiple sequences
}
\description{
creates a sequence for every unique value in a vector
}
\usage{
multiseq ( v )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{v}{ a vector }
  
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a vector with multiple sequences
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
v <- c("a", "a", "a", "c", "b", "b" , "a" )
( multiseq ( v ) )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
