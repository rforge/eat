\name{get.wle}
\alias{get.wle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Conquest WLE or MLE Output Files}
\description{Read Conquest files comprising maximum likelihood estimates (MLE) or weighted likelihood estimates
(WLE). }
\usage{
get.wle(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the ConQuest MLE or WLE file.
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
A data frame with one row per person containing the following columns:
\item{case}{Case number}
\item{ID}{Identifier for this case }
\item{n.solved}{Number of items this person answered correctly}
\item{n.total}{Number of total items presented to this person}
\item{wle}{WLE or MLE estimate. The last number of the columns name indicates the
dimension the WLE or MLE estimate belongs to.}
\item{std.wle}{Standard error of WLE or MLE estimate. The last number of the columns name
indicates the dimension the WLE or MLE estimate belongs to.}
}
\references{
See pp. 230 of Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
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
