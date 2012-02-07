\name{exploreDesign}
\alias{exploreDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
explore data design
}
\description{
explore data structure with respect to specific missing code (e.g. "missing by design")
}
\usage{
exploreDesign ( data , missing = NA , id = NULL , itemsPerPerson = TRUE , personsPerItem = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{ data.frame }
  \item{missing}{ missing specification }
  \item{id}{ id variable in \code{data} if exists }
  \item{itemsPerPerson}{ logical , if \code{TRUE} items per person list is returned }
  \item{personsPerItem}{ logical , if \code{TRUE} persons per item list is returned}
  
}
\details{
%% use \code{col.type="numeric.if.possible"} if conversion to numeric should be tested upfront, see \link{as.numeric.if.possible} for details

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on \code{itemsPerPerson} and \code{personsPerItem} , if both are \code{TRUE} a list with both elements is returned

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
data(science1)
d <- science1[,!colnames(science1) \%in\% science1.context.vars]
design <- exploreDesign ( data = d , missing = "mbd" , id = "id" )
str(design)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
