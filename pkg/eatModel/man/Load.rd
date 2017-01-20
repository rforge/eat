\name{Load}
\alias{Load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reload datasets written with the function \code{save}.}
\description{This is a wrapper for the \code{load} function of the \code{base}
package. \code{Load} additionally gives a warning if the object already exists
on the workspace and allows for different options than.}
\usage{
Load(file, exportIfOverlap = c("nothing","onlyNew","all") ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
a (readable binary-mode) connection or a character string giving the name of the file to load. 
}
  \item{exportIfOverlap}{
%%     ~~Describe \code{file} here~~
Specifies handling if one or more objects already exist(s) on the workspace. If \code{"nothing"}, 
none of the objects saved in the file will be exported to the workspace. If \code{"onlyNew"}, 
only these objects which are not yet existsing on the workspace will be exported. If \code{"all"}, 
all objects will be exported to the workspace. Existing objects will be overwritten. This is the 
standard behavior of the \code{load} function.
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
}
\references{
%% ~put references to the literature/web site here ~
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
\examples{
# see example 6a in the help file of defineModel() for a detailed demonstration of 
# trend estimation. 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
