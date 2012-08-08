\name{commonItems.percent}
\alias{commonItems.percent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate the percentage of common items of groups
}
\description{
This function calculates the percentage of items that groups of persons have in common.
}
\usage{
commonItems.percent ( dat , group.var , na = NA , xlsx = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame }
  \item{group.var}{ group variable in data.frame , eihter numeric indicator of column or column name }  
  \item{na}{ missing specification }
  \item{xlsx}{ 
		full path (directory + file name) to Excel to be written (don't forget ".xlsx" suffix)
	}
  
}
\details{
\code{dat} must only contain the group variable and the items, if further variables are in \code{dat} they are treated as items

}
\value{
returns a data.frame with common item percentage(s)
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
( commonItems.percent ( dat = d , group.var = "version" , na = "mbd" ) )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
