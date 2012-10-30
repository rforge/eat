\name{getConquestVersion}
\alias{getConquestVersion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get version (build) of ConQuest
}
\description{
get version (build) of ConQuest
}
\usage{
getConquestVersion ( path.conquest , path.temp, asDate = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{path.conquest}{ full path to ConQuest executable console }
  \item{path.temp}{ optional: writeable folder used for temporary files. If not specified, R working directory will be used. 
Without writing access, NULL is returned. }
  \item{asDate}{ if \code{TRUE} an object of class 'date' is returned
				 if \code{FALSE} a character string is returned	}

}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on option 'asDate'

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
getConquestVersion ( "c:/ConQuest/console_Feb2007.exe" )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
