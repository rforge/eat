\name{get.equ}
\alias{get.equ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads equivalence table created in Conquest analysis.}
\description{Reads Conquest files comprising equivalence tables for MLE or WLE parameters.}
\usage{
get.equ(file.equ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.equ}{
%%     ~~Describe \code{file.equ} here~~
Character string of the Conquest equ-file.
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
A list of n+1 elements, with n the number of dimensions in the analysis. Each element is a data.frame, whose
name correponds to the name of the dimension the values belongs to. All data.frames except the last one give 
the transformation of each possible raw score to the WLE or MLE score including it's standard error. First column
in each data.frame contains the raw score, second column the transformed WLE or MLE score, third columns it's standard
error. 

The last element of the list give some sparse information about the model specifications. 

}
\references{
See Conquest manual, pp.162.
}
\author{
%%  ~~who you are~~
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
