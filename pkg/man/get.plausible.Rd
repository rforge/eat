\name{get.plausible}
\alias{get.plausible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads Conquest plausible values files}
\description{Function reads Conquest plausible value files and transforms them into a R data frame.}
\usage{
get.plausible(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string of the Conquest plausible values file to be read in. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Funktion identifies number of cases, number of plausible values and number of dimensions. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame, where each row corresponds to one case. Columns are labeled with dimension names and number
of corresponding plausible value. 

 \item{case}{Case number. Each row represents one person.}
 \item{ID}{Case ID, if listed in Conquest plausible values file.}
 \item{pv}{Plausible value. Denotation of columns names is pv.[name of dimension]_[number of plausible value]. For example, pv.reading_6 refers to the 6th plausible value of reading dimension.}
 \item{eap}{Expectation value of the a posterio distribution of the corresponding dimension.}
 \item{eap.se}{Standard error of the EAP estimate.}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
