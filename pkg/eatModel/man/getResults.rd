\name{getResults}
\alias{getResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collect all results from Conquest/TAM analysis into a common data frame}
\description{First the IRT model should be defined using \code{defineModel}. Afterwards,
call \code{runModel} with the argument returned by \code{defineModel} to start the estimation.
The last step then is to create a results frame using \code{getResults}. }
\usage{
getResults(runModelObj)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{runModelObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{runModel}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the long format with ten columns.
\item{model}{The name of the model (as specified by the user in \code{analysis.name}.}
\item{source}{The estimation software (i.e, conquest or TAM) }
\item{var1}{The variable name for which the corresponding value is given, i.e. its indicator. }
\item{var2}{Additional variable information if necessary.}
\item{type}{Type of coefficient (for example, random or fixed).}
\item{indicator.group}{The type of the group the corresponding variable belongs to.}
\item{group}{The group the corresponding variable belongs to. Note: group is nested within \code{indicator.group}.}
\item{par}{The type of the parameter.}
\item{derived.par}{Optionally: The derived parameter.}
\item{value}{The value of the corresponding estimate.}
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
# see examples in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
