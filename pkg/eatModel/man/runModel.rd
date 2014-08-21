\name{runModel}
\alias{runModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Run IRT model specified by 'defineModel' using Conquest or TAM}
\description{First the IRt model should be defined using \code{defineModel}. Afterwards,
call \code{runModel} with the argument returned by \code{defineModel} to start the estimation.}
\usage{
runModel(defineModelObj, show.output.on.console = FALSE, show.dos.console = TRUE,
    wait = TRUE) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{defineModelObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{defineModel}.
}
  \item{show.output.on.console}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{defineModel} previously was called with \code{software = "conquest"}.
Logical: Should the output of the conquest console be printed on the R console during estimation?
}
  \item{show.dos.console}{
%%     ~~Describe \code{split.dif} here~~
Applies only if \code{defineModel} previously was called with \code{software = "conquest"}.
Logical: Should the output of the conquest console be printed on screen?
}
  \item{wait}{
%%     ~~Describe \code{abs.dif.bound} here~~
Applies only if \code{defineModel} previously was called with \code{software = "conquest"}.
A logical (not NA) indicating whether the R interpreter should wait for the command to finish,
or run it asynchronously.
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
If \code{defineModel} previously was called with \code{software = "tam"}, the returned value
is identically to the corresponding TAM output. If \code{defineModel} previously was called
with \code{software = "conquest"}, the returned value contains only internally used information
useful for \code{getResults}.
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
