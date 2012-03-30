\name{plotDevianceChange}
\alias{plotDevianceChange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot deviance change
}
\description{
extract or plot (on console or to pdf) deviance change from ConQuest logfile
}
\usage{
plotDevianceChange ( path , plot = TRUE , pdf = FALSE , out.path = NULL , extreme.crit = 0.75 , pdftk.path = NULL ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{path}{ full path to or connection of ConQuest logfile, or just a path (in which ConQuest logfiles are to be searched for) }
  \item{plot}{ if \code{TRUE} deviance change plot is created }
  \item{pdf}{ if \code{TRUE} plot ist written to pdf }
  \item{out.path}{ path for pdf output file }
  \item{extreme.crit}{ numeric, threshold criterion to remove outliers, is multiplied with standard deviation of deviance change }
  \item{pdftk.path}{ if more than one pdf are created (implies \code{plot=TRUE}, \code{pdf=TRUE} and \code{path} beeing a directory) pdftk merges these pdf files into one single pdf file "deviance_change_plots.pdf";
					 \code{pdftk.path} is the path to the pdftk exexutable, if \code{NULL} it is searched in file.path(.Library,"eat/winexe/pdftk"), if not found it will not happen with no warning issued }
  
}
\details{
ConQuest tends to not completely write out log if running and option 'update = yes' is used.
To avoid warnings and malfunction manually delete the last potentially incomplete iteration from logfile.
Points below 0 are red; if model converged ( see \code{link{isConverged}} for details ), the last point is larger and green
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on \code{plot} and \code{pdf}; if both are \code{FALSE} the deviance change data is returned, this is a named vector with names = iteration number and values = deviance change from previous iteration;
if more than one ConQuest logfile is processed a list of named vectors is returned

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
plotDevianceChange ( path = bzfile ( file.path( .Library , "eat/extdata/ConQuest.Log.Example1.log.bz2" ) ) )
plotDevianceChange ( path = bzfile ( file.path( .Library , "eat/extdata/ConQuest.Log.Example1.log.bz2" ) ) , plot = FALSE )
}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
