\name{simEquiTable}
\alias{simEquiTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes equivalence table based on simulated data}
\description{Function provides the equivalence table for unidimensional models, 
specifying the individual competence level for each total score of the test.}
\usage{simEquiTable  ( anchor, mRef, sdRef, addConst = 500, multConst = 100, 
cutScores , dir , n = 2000, conquest.folder )}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anchor}{A data frame with anchor parameters on the logit scale, transformed 
  to the metric of the reference population. The first column contains the names of 
  all anchored items. The second column contains anchor parameters.
}
  \item{mRef}{
%%     ~~Describe \code{dif.term} here~~
Scalar: mean of the reference population. 
}
  \item{sdRef}{
%%     ~~Describe \code{split.dif} here~~
Scalar: Standard deviation of the reference population. 
}
  \item{addConst}{
%%     ~~Describe \code{dif.term} here~~
Additive constant for parameter transformation.
}
  \item{multConst}{
%%     ~~Describe \code{abs.dif.bound} here~~
Multiplicative constant for parameter transformation.
}
  \item{cutScores}{
%%     ~~Describe \code{abs.dif.bound} here~~
Named list of one or two elements. "values" is a numeric vector of cut scores (increasing),
"labels" is an optional character vector of cut score labels. Note that "labels" (if specified)
has to be of one more length than "values". 
}
  \item{dir}{
%%     ~~Describe \code{sig.dif.bound} here~~
An already existsing directory with writing permission in which the (temporary) output will be written to.
}
  \item{n}{
%%     ~~Describe \code{sig.dif.bound} here~~
Scalar: sample size for the simulated data.
}
  \item{conquest.folder}{
%%     ~~Describe \code{dif.term} here~~
Character string with path and name of the ConQuest console, for 
example \code{"c:/programme/conquest/console_Feb2007.exe"}.
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
A list of two data frames, including the complete table and the reduced table with
the following 5 columns. 
  \describe{
    \item{Score}{Students raw score}
    \item{Estimate}{Estimated individual WLE according to the raw score.}
    \item{std.error}{Standard error of the individual WLE estimate. This column is 
    not included in the reduced table.}
    \item{estBista}{Transformed WLE}
    \item{ks}{competence level}
 }
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
\dontrun{
# create arbitrary anchor parameter
anchor <- data.frame ( item = paste("i",1:20,sep=""), 
          par = rnorm(20, mean = -.1, sd = 1.5))

# create arbitrary cut scores
# note that the number of labels (if specified) must equal the number of cuts + 1
cuts   <- list ( values = 330+0:4*75, labels = c("1a", "1b", 2:5) )

# create the equivalence table
ret <- simEquiTable( anchor = anchor, cutScores = cuts , mRef = -0.05, sdRef = 0.9, 
       dir = "c:/users/weirichs/test", conquest.folder = "N:/console_Feb2007.exe")
View(ret$short)       
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
