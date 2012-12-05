\name{collapseMissings}
\alias{collapseMissings}
\title{Collapse Missings
}
\description{converts character missings of different types to 0 or NA
}
\usage{
collapseMissings(dat, missing.rule = NULL, items)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
data frame containing character missings (e.g. type 'mbd' - missing by design)
}
  \item{missing.rule}{
list, definition how to recode distinct missings in dataset. See details for default.
}
  \item{items}{
character vector containing column names of the data frames whose character missings are to be collapsed
}
}
\details{
 Default \code{missing.rule} 
in \code{collapseMissings} is:
text volume insufficient = 0 , missing not reached = 0 , missing coding impossible = NA , missing by design = NA , 
missing invalid response = 0 , missing by intention = 0

The results of \code{collapseMissings} will be written to a protocol file with \code{sunk}. 
}
\value{
A data frame with recoded missings.
}
\references{
For missing types see http://code.google.com/p/zkdlib/wiki/MissingHandling
}
\author{
Karoline Sachse, Martin Hecht
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
%\examples{
%	data(inputDat)
%	dat1 <- inputDat[[1]]  # get first dataset from inputDat
%	datColMis <- collapseMissings(dat = dat1, 
%	missing.rule = list(mvi = 0 ,mnr = 0 ,mci = 0 ,mbd = NA ,mir = 0 ,mbi = 0), 
%	items=colnames(dat1)[- c(1:2)])
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
