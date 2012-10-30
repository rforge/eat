\name{fill.na}
\alias{fill.na}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
fill NAs in a vector
}
\description{
fill NAs with non-NA values depending on left (forward) or right (backward) non-NA value
}
\usage{
fill.na ( vec , backwards = FALSE , na.rm = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{vec}{ a vector }
  \item{backwards}{ if \code{FALSE} NAs are filled forward, if \code{TRUE} NAs are filled backwards }
  \item{na.rm}{ if \code{TRUE} NAs at start and end of vector are removed }
  
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a vector with filled NAs
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
( vec <- c ( NA , 1 , NA , NA , 2 , NA , 3 , NA ) )
fill.na ( vec )
fill.na ( vec , backwards = TRUE )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
