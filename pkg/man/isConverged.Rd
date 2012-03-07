\name{isConverged}
\alias{isConverged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
check convergence of ConQuest models
}
\description{
checks if ConQuest models in a directory have converged or not
}
\usage{
isConverged ( path , txt = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{path}{ path to models }
  \item{txt}{ if \code{TRUE} a convergence summary is written to convergence_summary.txt in \code{path}, and a file (either "_CONVERGED_" or "_N_O_T_CONVERGED_") is written to each model directory
			  if \code{FALSE} a data.frame of convergence information is returned }
  
}
\details{
checks recursively in \code{path} for shw files, determines whether model converged or not;
models that converged, but the solution is not the bese solution ( "At termination the solution was not the best attained solution" ), are treated as not converged
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on \code{txt}

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
isConverged ( log.path = bzfile ( file.path( .Library , "eat/extdata/ConQuest.Log.Example1.log.bz2" ) ) )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
