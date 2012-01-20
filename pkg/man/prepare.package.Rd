\name{prepare.package}
\alias{prepare.package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
prepare.package
}
\description{
prepares package
}
\usage{
prepare.package (	source.folder = "p:/ZKD/development" ,
					files ,
					package.folder = "p:/ZKD/packages" ,
					package.name ,
					package.version )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{source.folder}{ folder of R files }
  \item{files}{ character vector of R files that should be included in the package }
  \item{package.folder}{ folder of packages }
  \item{package.name}{ name of package }
  \item{package.version}{ version of package, must be in format "0.0.0" }
  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
copies R files from \code{source.folder} to package folder
copies "x.x.x" folder content to package folder
modifies version and date in DESCRIPTION and automateModels
creates ChangeLog
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
