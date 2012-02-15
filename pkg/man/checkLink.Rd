\name{checkLink}
\alias{checkLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
checkLink
}
\description{
checks whether items in a dataset are linked via design
}
\usage{
checkLink ( dataFrame, sysmis = NA, verbose = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dataFrame}{ A data.frame where all columns denote test items }
  \item{sysmis}{ character string specifying values to be treat as missing by design  }
  \item{verbose}{ logical: Should output printed to console? }
  

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
A logical value, i.e. \code{TRUE} or \code{FALSE}, indicating whether items in dataset are linked to each other. 
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
