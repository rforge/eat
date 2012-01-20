\name{source.it.all}
\alias{source.it.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
source.it.all
}
\description{
sources *.R files of \code{folder}
}
\usage{
source.it.all ( folder="p:/ZKD/development" , develop.modules = NULL , return.stable = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{folder}{ folder with *.R files }
  \item{develop.modules}{ character vector of R files that should be sourced in development status }
  \item{return.stable}{ if \code{TRUE} nothing is sourced and a vector of all stable versions is returned }

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
\item{return.stable = FALSE }{sources R files}
\item{return.stable = TRUE }{character vector of stable R files}
}
\references{

}
\author{
Christiane Penk, Martin Hecht
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
