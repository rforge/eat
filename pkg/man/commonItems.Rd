\name{commonItems}
\alias{commonItems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
identify common items of groups
}
\description{
This function identifies items that groups of persons have in common.
}
\usage{
commonItems ( data , group.var , missing = NA )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{ data.frame }
  \item{group.var}{ group variable in data.frame , eihter numeric indicator of column or column name }  
  \item{missing}{ missing specification }
  
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
returns a list of all \code{group.var} combinations with character vectors of common item names

names of list are both group names concatenated by "|"

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
d <- science1[,c("version",science1.items)]
commonItems ( data = d , group.var = "version" , missing = "mbd" )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
