\name{log2init}
\alias{log2init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert ConQuest Log to ConQuest Init 
}
\description{
Convert a ConQuest logfile to ConQuest covariance, regression and item init files
}
\usage{
log2init ( log.path , out.path = dirname(log.path) , iteration = c("highestLikelihood","last","first") , out.files.suffix = "_INIT" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{log.path}{ full path to ConQuest logfile }
  \item{out.path}{ path of output files }
  \item{iteration}{ either "highestLikelihood" (default), "last" or "first", or a number }
  \item{out.files.suffix}{ suffix to be added to output file names }
  
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
writes files to \code{out.path}

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
\dontrun{
log2init ( bzfile ( file.path( .Library , "eat/extdata/ConQuest.Log.Example1.log.bz2" ) ) , "c:/temp" )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
