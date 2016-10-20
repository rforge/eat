\name{prepRep}
\alias{prepRep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prepares 'eatModel' results for jackknifing and trend estimation via 'eatRep'.}
\description{Function uses output of \code{transformToBista} to provide one data.frame
with results from T1 and T2 along with original and transformed linking errors.}
\usage{
prepRep(calibT2, bistaTransfT1, bistaTransfT2) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{calibT2}{
%%     ~~Describe \code{file} here~~
The object returned by \code{transformToBista} for the calibration model at T2. 
}
  \item{bistaTransfT1}{
%%     ~~Describe \code{file} here~~
The object returned by \code{transformToBista} for the conditioning model at T1. 
}
  \item{bistaTransfT2}{
%%     ~~Describe \code{file} here~~
The object returned by \code{transformToBista} for the conditioning model at T2. 
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
A single data.frame with person parameter estimates and linking errors for the two
measurement points according to trend estimation. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
This version is beta. Please use with care!
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# see example 6a in the help file of defineModel() for a detailed demonstration of 
# trend estimation. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
