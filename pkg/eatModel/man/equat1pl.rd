\name{equat1pl}
\alias{equat1pl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{1pl equating with optional elimination of linking DIF items}
\description{Function does the 1pl linking according to \code{equating.rasch} from the \code{sirt} package. 
Moreover, optional elimination of items with linking DIF is allowed.}
\usage{
equat1pl(results , prmNorm , excludeLinkingDif = TRUE, difBound = 1, 
         iterativ = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{file} here~~
The object returned by \code{getResults}.
}
  \item{prmNorm}{
%%     ~~Describe \code{file} here~~
Data frame with two columns: First column items, second column item difficulties. 
Column names are arbitrary. 
}
  \item{excludeLinkingDif}{
%%     ~~Describe \code{file} here~~
Logical. Should items with linking DIF excluded? 
}
  \item{difBound}{
%%     ~~Describe \code{file} here~~
Defines the boundary. Items with absolut linking DIF greater than the boundary will 
be removed from the linking procedure. 
}
  \item{iterativ}{
%%     ~~Describe \code{file} here~~
Logical. Should the exclusion of linking DIF items executed in an iterative loop?
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
A list with equating information intended for further transformation by the \code{transformToBista}
function. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
%% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# see example 5 in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
