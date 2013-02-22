\name{jk2.quantile}
\alias{jk2.quantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 method for quantiles.}
\description{Compute quantiles with standard errors for complex cluster designs with multiple imputed variables
(e.g. plausible values) based on Jackknife (JK2) procedure. Conceptually, the function combines
replication methods and methods for multiple imputed data. Technically, this is a wrapper for
the \code{svyquantile()} function of the survey package.
}
\usage{
jk2.quantile(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), dependent = list(),
            probs = seq(0, 1, 0.25),
            complete.permutation = c("nothing", "groups", "all")) }
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
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more grouping variables. Each dependent variable will result in a separate analysis.
If grouping variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{probs}{
%%     ~~Describe \code{sig.dif.bound} here~~
Numeric vector with probabilities for which to compute quantiles.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputation of one variable (e.g. number of plausible values of the dependent variable)
differs from the number of imputations used for group variable(s). If \code{"all"}, number of datasets are determined through
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in 3x5=15 imputed data sets. If \code{"nothing"}, only 5 imputed
data sets will be used, no matter whether the larger number of imputations is a whole multiple of the smaller number of imputations. If \code{"groups"},
only the number of imputations of more than one grouping variable will be permutated.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svyquantile()} called by \code{svyby()} implemented in the 'survey' package.
The results of the several analyses are then pooled according to Rubin's rule.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, one for each dependent variable. Each data frame contains percentile values and their standard errors.
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
data(reading_writing)
### Computes percentile for reading and writing scores conditionally on country
perzent   <- jk2.quantile(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD",
             JKZone = "JKZone", JKrep = "JKrep", group = list(country = "country"),
             dependent = list(reading = paste("reading_score", 1:3, sep = ""),
                              writing = paste("writing_score", 1:3, sep = "")  ) ,
             probs = seq(0.1,0.9,0.2), complete.permutation = "no" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
