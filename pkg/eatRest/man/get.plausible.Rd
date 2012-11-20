\name{get.plausible}
\alias{get.plausible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read ConQuest Plausible Values Output Files}
\description{This function reads ConQuest plausible value files and automatically identifies the number of cases, the number of plausible values and the number of dimensions.}
\usage{
get.plausible(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the ConQuest plausible values file. 
}
}
\details{
}
\value{
A data frame with one row per person containing the following columns:

 \item{case}{Case number} 
 \item{ID}{Identifier for this case}
 \item{pv}{Plausible value. Columns are named \code{pv.[name of dimension]_[number of plausible value]}. For example, pv.reading_6 refers to the 6th plausible value of reading dimension.}
 \item{eap}{Expected\emph{a posteriori} ability estimate for this person. Columns are named \code{eap.[name of dimension]}}
 \item{eap.se}{Standard error of the EAP estimate. Columns are named \code{eap.se.[name of dimension]}}

}
\references{
See pp.230 of Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest Version 2.0. Generalised Item Response Modeling Software}. Camberwell, Victoria: ACER Press.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
