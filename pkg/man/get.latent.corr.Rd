\name{get.latent.corr}
\alias{get.latent.corr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get latent correlations, covariance and variances
}
\description{
retrieve latent correlations, covariance and variances from ConQuest shw files
}
\usage{
get.latent.corr ( path , xlsx = NULL , covariance = TRUE , variance = TRUE )
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
  \item{covariance}{ 
		logical: should covariance(s) be extracted (default: TRUE)
	}
  \item{variance}{ 
		logical: should variance(s) be extracted (default: TRUE)
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
returns a data.frame with correlations, covariance and variances of ConQuest analysis/analyses
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
#      get.latent.corr ( <folder> ) 
# e.g. get.latent.corr ( "C:/temp/automateModels/Example1" ) 
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
