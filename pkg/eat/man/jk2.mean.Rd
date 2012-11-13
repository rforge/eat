\name{jk2.mean}
\alias{jk2.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for mean estimates.}
\description{Compute means, variances and standard deviations as well as their standard errors
for complex cluster designs based on Jackknife (JK2) procedure. This is a wrapper for the svymean() and
svyvar() functions of the survey package.}
\usage{
jk2.mean(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), group.differences.by = NULL,
         dependent = list(), complete.permutation = c("nothing", "groups", "all"))}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{file} here~~
Data frame containing all variables for analysis.
}
  \item{ID}{
%%     ~~Describe \code{dif.term} here~~
Variable name or column number of ID variable.
}
  \item{wgt}{
%%     ~~Describe \code{split.dif} here~~
Optional: Variable name or column number of weighting variable. If no weighting variable is specified,
all cases will be equally weighted.
}
  \item{JKZone}{
%%     ~~Describe \code{abs.dif.bound} here~~
Variable name or column number of variable indicating Jackknifing Zone.
}
  \item{JKrep}{
%%     ~~Describe \code{sig.dif.bound} here~~
Variable name or column number of variable indicating replicate ID.
}
  \item{group}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: List of one or more grouping variables. If grouping variable is a multiple imputation variable,
all names concerning one variable are interpreted as its imputations. See details for more information.
}
  \item{group.differences.by}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Specifies variable group differences should be computed for.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more grouping variables. Each dependent variable will result in a separate analysis.
If grouping variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Becomes relevant if the numbers of imputations used for group variable(s) differs from the number of
imputation used for dependent variable(s).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Funktion first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. Afterwards, function serves as a wrapper for several functions implemented in the
'survey' package.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, one for each dependent variable. Each data frame contains means, variances, standard deviations and
standard errors for all of them. If group differences were estimated, they occur as attributes in the outcome. See
examples for details.
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
data(science2)
### First example: only means, SD and variances for each country
means  <- jk2.mean(dat = science2, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone", JKrep = "JKrep",
          group = list(country = "country"),
          dependent = list(reading = paste("reading_score",1:3,sep=""), writing = paste("writing_score",1:3,sep="") ),
          complete.permutation = "all" )
### Second example: Sex differences by country
means  <- jk2.mean(dat = science2, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone", JKrep = "JKrep",
          group = list(country = "country", GENDER = "sex"), group.differences.by = "GENDER",
          dependent = list(reading = paste("reading_score",1:3,sep=""), writing = paste("writing_score",1:3,sep="") ),
          complete.permutation = "all" )
attr(means[[1]], "difference")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
