\name{get.plausible}
\alias{get.plausible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Conquest Plausible Values Output Files}
\description{This function reads Conquest plausible value files and automatically identifies the number of cases,
the number of plausible values and the number of dimensions. }
\usage{
get.plausible(file, quiet = FALSE, forConquestResults = FALSE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the ConQuest plausible values file.
}
  \item{quiet}{
%%     ~~Describe \code{file} here~~
Logical: Suppress printing messages on console?
}
  \item{forConquestResults}{
%%     ~~Describe \code{file} here~~
This argument only applies if \code{get.plausible} is called by \code{getResults} to enhance performance.
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
A data frame with one row per person containing the following columns:
\item{case}{Case number}
\item{ID}{Identifier for this case }
\item{pv}{Plausible values columns. Columns are named pv.[number of PV].[number of dimension].
For example, pv.5.2 refers to the 5th plausible value of the second dimension.}
\item{eap}{Expected a posteriori (EAP) ability estimate for each person. Columns are named eap_Dim.[number of dimension].
For example, eap_Dim.2 refers to the eap estimate of the second dimension.}
\item{se.eap}{Standard error of the EAP estimate. Columns are named se.eap_Dim.[number of dimension].
For example, se.eap_Dim.2 refers to the standard error of the EAP estimate of the second dimension.}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
