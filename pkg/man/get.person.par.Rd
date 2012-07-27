\name{get.person.par}
\alias{get.person.par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get person parameters
}
\description{
obtain person parameters from \link{automateModels} results
}
\usage{
get.person.par ( results )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{ return object from \code{automateModels} }
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns a data.frame with person parameters from \code{automateModels} run:

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
\dontrun{
# example results structure from \code{automateModels}
data ( ex5 )
#
( person.par <- get.person.par ( ex5 ) )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
