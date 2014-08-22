\name{get.equ}
\alias{get.equ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads equivalence table created in Conquest analysis.}
\description{Reads Conquest files comprising equivalence tables for MLE or WLE parameters.}
\usage{
get.equ(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the Conquest equ file (This file is requested using the \code{"equivalence"}
statement in the conquest synax file).
}
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
A list of \eqn{n+1} elements, with \eqn{n} the number of dimensions in the analysis. Each element is a
data frame, whose name correponds to the name of the dimension the values belongs to. All
data frames except the last one give the transformation of each possible raw score to the WLE
or MLE score including its standard error. First column in each data frame contains the raw score,
second column the transformed WLE or MLE score, third columns its standard error.
The last element of the list give some sparse information about the model specifications.
}
\references{
See pp. 162 of Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
