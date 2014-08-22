\name{get.prm}
\alias{get.prm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Conquest prm (parameter) Output Files}
\description{Read Conquest files comprising item parameters (prm). }
\usage{
get.prm(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the ConQuest prm file (This file is requested using the \code{"export par"}
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
A data frame with three columns:
\item{Case}{Case number}
\item{item}{Identifier for this item }
\item{parameter}{parameter estimate for this item}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
