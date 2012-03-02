\name{get.history}
\alias{get.history}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads Conquest history files.}
\description{Reads Conquest history file comprising parameter estimates of each iteration. }
\usage{
get.history(historyfile, shw.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{historyfile}{
%%     ~~Describe \code{file} here~~
Character string of the Conquest history file.
}
  \item{shw.object}{
%%     ~~Describe \code{file} here~~
Optional: R-Object created by \code{get.shw()}. Necessary to label the columns of the history file. 
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
A data.frame according to the corresponding Conquest history file. First column comprises the iteration number, second column 
the deviance of the corresponding iteration. Estimates of model parameters are listed in further columns. 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
