\name{prepJack}
\alias{prepJack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prepares results getResults() for eatRep analyses.}
\description{Prepares a data.frame suitable for eatRep analyses. }
\usage{
prepJack(resultsObj, arrangeDependentVar = c("singleFrame", "multipleFrames"), 
         modelAsGroup = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultsObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{getResults}.
}
  \item{arrangeDependentVar}{
%%     ~~Describe \code{file} here~~
Necessary only if the analyses contains more than one (latent) dependent variable, i.e. if
multidimensional IRT models were run. If "singleFrame", the output is one data.frame with
several columns according to each dimension. If "multipleFrames", the output is a list of 
data.frames. 
}
  \item{modelAsGroup}{
%%     ~~Describe \code{file} here~~
Logical: Treat model names as groups?
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
A data frame in the long format or a list of data frames. 
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
