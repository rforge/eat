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
jk2.quantile(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(),
            group.splits = length(groups), group.delimiter = "_", dependent = list(),
            probs = seq(0, 1, 0.25),  na.rm = FALSE,
            complete.permutation = c("nothing", "groups", "all"), nBoot = NULL,
            bootMethod = c("wSampling","wQuantiles"))}
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
Variable name or column number of variable indicating Jackknifing Zone. If \code{NULL}, no cluster structure is assumed and
standard errors are computed according to a random sample.
}
  \item{JKrep}{
%%     ~~Describe \code{sig.dif.bound} here~~
Variable name or column number of variable indicating replicate ID. If \code{NULL}, no cluster structure is assumed and
standard errors are computed according to a random sample.
}
  \item{groups}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: List of one or more grouping variables. If grouping variable is a multiple imputed variable,
all names concerning one variable are interpreted as its imputations. See details for more information.
}
  \item{group.splits}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: If groups are defined, \code{group.splits} optionally specifies whether analysis should be done also
in the whole group or overlying groups. See examples for more details.
}
  \item{group.delimiter}{
%%     ~~Describe \code{sig.dif.bound} here~~
Character string which separates the group names in the output frame.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more grouping variables. Each dependent variable will result in a separate analysis.
If grouping variable is a multiple imputed variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{probs}{
%%     ~~Describe \code{sig.dif.bound} here~~
Numeric vector with probabilities for which to compute quantiles.
}
  \item{na.rm}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should cases with missing values be dropped?
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputations of one variable (e.g. number of plausible values of the dependent variable)
differs from the number of imputations used for group variable(s). If \code{"all"}, number of datasets are determined through
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in 3x5=15 imputed data sets. If \code{"nothing"}, only 5 imputed
data sets will be used, no matter whether the larger number of imputations is a whole multiple of the smaller number of imputations. If \code{"groups"},
only the number of imputations of more than one grouping variable will be permutated.
}
  \item{nBoot}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Without replicates, standard error cannot be computed in a weighted sample. Alternatively, standard errors may
be computed using the \code{boot} package. \code{nBoot} therefore specifies the number of bootstrap samples. If not specified,
no standard errors will be given. In analyses containing replicates or samples without specifying person weights,
\code{nBoot} will be ignored.
}
  \item{bootMethod}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: If standard error are computed in a bootstrap, two possible methods may be applied.
\code{'wSampling'} requests the function to draw \code{nBoot} weighted bootstrap samples for which unweighted quantiles
are computed. \code{'wQuantiles'} requests the function to draw \code{nBoot} unweighted bootstrap samples for which
weighted quantiles are computed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svyquantile()} called by \code{svyby()} implemented in the 'survey' package.
The results of the several analyses are then pooled according to Rubins rule, which is adapted for nested imputations if the \code{dependent} argument implies a nested structure.
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
### First example: Computes percentile for reading and writing scores conditionally on country
perzent   <- jk2.quantile(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD",
             JKZone = "JKZone", JKrep = "JKrep", groups = list(country = "country"),
             dependent = list(reading = paste("reading_score", 1:3, sep = ""),
                              writing = paste("writing_score", 1:3, sep = "")  ) ,
             probs = seq(0.1,0.9,0.2), complete.permutation = "no" )

### Second example: Computes percentile for reading and writing scores conditionally on country,
### use 100 bootstrap samples
perzent   <- jk2.quantile(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD",
             groups = list(country = "country"),
             dependent = list(reading = paste("reading_score", 1:3, sep = ""),
                              writing = paste("writing_score", 1:3, sep = "")  ) ,
             probs = seq(0.1,0.9,0.2), complete.permutation = "no", nBoot = 100 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
