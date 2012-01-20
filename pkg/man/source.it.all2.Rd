\name{source.it.all2}
\alias{source.it.all2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
source.it.all2
}
\description{
sources *.R files of \code{folder}
}
\usage{
source.it.all2 ( folder="p:/ZKD/development" , development = TRUE , development.only = FALSE , exclude = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{folder}{ folder with *.R files }
  \item{development}{ if \code{TRUE} development versions are sourced (if non-existent the latest stable is sourced or nothing is sourced, see option \code{development.only}\\
					  if \code{FALSE} stable versions are sourced }
  \item{development.only}{ if \code{TRUE} only development versions are sourced \\
						   if \code{FALSE} stable versions are included }
  \item{exclude}{ character vector of R files that should not be sourced }

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
sources R files
}
\references{

}
\author{
Martin Hecht, Christiane Penk
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
