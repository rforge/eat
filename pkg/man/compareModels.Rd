\name{compareModels}
\alias{compareModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compare ConQuest models
}
\description{
retrieves model information ( sample size, deviance, number of parameters ) and calculates AIC and BIC;
if more than 1 model is specified models are compared ( AIC difference, BIC difference, Chi square prob. )
}
\usage{
compareModels ( path , xlsx = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ 
		either 1 of 4 inputs:
		[1] full path (directory + file name) of a ConQuest shw file
		[2] a list of ConQuest shw files (full paths)
		[3] a character vector of ConQuest shw files (full paths)
		[4] a folder in which it will be searched for ConQuest shw files
	}
  \item{xlsx}{ 
		full path (directory + file name) to Excel to be written (don't forget ".xlsx" suffix)
	}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns a list of 2 data.frames:
the first called 'models' contains model information;
the second called 'model.comparison' contains information of model comparison 
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
# just run any example of \link{automateModels}
# then:
#      compareModels ( <folder> ) 
# e.g. compareModels ( "C:/temp/automateModels/Example3" ) 
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
