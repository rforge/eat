\name{dM}
\alias{dM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display results of jk2.mean analyses.}
\description{This is an easy function only addressed to display results of jk2.mean analyses in an abbreviated manner.}
\usage{
dM ( object, omitTerms = c("mean","sd","var", "Ncases","NcasesValid",
     "meanGroupDiff", "se","est") )}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{file} here~~
A results object created by jk2.mean().
}
  \item{omitTerms}{
%%     ~~Describe \code{dif.term} here~~
Optionally: character vector of terms you do not want to see on console.
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
A simply data frame, not intended for further processing.
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
### see examples in the vignette and in the help page of jk2.glm()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
