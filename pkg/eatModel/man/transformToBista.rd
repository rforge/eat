\name{transformToBista}
\alias{transformToBista}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transformation of item and person parameters to the Bista metric.}
\description{Function uses output of \code{equat1pl} to provide two data.frames, 
one for the item parameters on the bista metric and one for the person parameter
(PVs) on the bista metric.}
\usage{
transformToBista ( equatingList, refPop, cuts, weights = NULL, defaultM = 500, 
                   defaultSD = 100)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{equatingList}{
%%     ~~Describe \code{file} here~~
The object returned by \code{equat1pl}.
}
  \item{refPop}{
%%     ~~Describe \code{file} here~~
Data frame with at least three columns. First column indicates the domain name. 
Note that this name must match the domain names in the output of \code{getResults}.
Second column contains the mean of the reference population. Third column contains
the standard deviation of the reference population. Fourth column optionally contains 
the transformed mean on the Bista metric of the reference population. Fifth column 
optionally contains the transformed standard deviation on the Bista metric of the 
reference population. If the fourth and fifth columns are missing, values will be 
defaulted to 500/100. 
}
  \item{cuts}{
%%     ~~Describe \code{file} here~~
A named list with cut scores. Names of the list must match the domain names in the 
output of \code{getResults}. Each element of the list is a list with one or two 
elements---the cut scores (in ascending order) and (optionally) the labels of the 
stages. See the examples of \code{defineModel} for further details. 
}
  \item{weights}{
%%     ~~Describe \code{file} here~~
Optional: a data.frame with two columns, first column is person identifier, second
columns is individual caseweight. Necessary for the transformation of linking 
error for (ordered) factors and/or if descriptives of the reference population 
should be computed directly from the data. See the examples of \code{defineModel}
for further details. 
}
  \item{defaultM}{
%%     ~~Describe \code{file} here~~
Mean of the reference population on the ``bista'' metric. 
}
  \item{defaultSD}{
%%     ~~Describe \code{file} here~~
Standard deviation of the reference population in the ``bista'' metric. 
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
A list with three data frames: the first one contains original and transformed 
item parameters and competence levels. The second one contains original and transformed 
person parameters and competence levels. The third one contains transformation 
information. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
This version is alpha. Please use with care!
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# see example 5, 6, and 6a in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
