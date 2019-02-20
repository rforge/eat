\name{pvFromRes}
\alias{pvFromRes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collects all plausible values from the object returned by getResults()}
\description{Collects plausible values in a wide or long format data frame. }
\usage{
pvFromRes(resultsObj, toWideFormat = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultsObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{getResults}.
}
  \item{toWideFormat}{
%%     ~~Describe \code{file} here~~
Logical: Should the plausible values be arranged in the wide format? Alternatively, they
will appear in the long format. 
}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the wide format with ten columns.
}
\references{
%% ~put references to the literature/web site here ~
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
# see examples in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
