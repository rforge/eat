\name{jk2.table}
\alias{jk2.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for frequency tables.}
\description{Compute frequency tables for categorical variables (e.g. factors: dichotomous or polytomous) in complex
cluster designs. Estimation of standard errors optionally takes the clustered structure and multiple imputed
variables into account. To date, only the Jackknife-2 procedure (JK2) is implemented to account for clustered designs.
Procedures of Rubin (1987) and Rubin (2003) are implemented to account for multiple imputed data and nested imputed data.
Conceptually, the function combines replication and imputation methods. Technically, this is a wrapper
for the svymean() function of the survey package.
}
\usage{
jk2.table(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(),
          group.splits = length(groups), group.delimiter = "_", dependent = list(),
          separate.missing.indikator = FALSE, expected.values = list(),
          complete.permutation = c("nothing", "groups", "all"), doCheck = TRUE )}
          
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
Optional: Variable name or column number of variable indicating Jackknifing Zone. If \code{NULL}, no cluster structure is assumed and
standard errors are computed according to a random sample.
}
  \item{JKrep}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Variable name or column number of variable indicating replicate ID. If \code{NULL}, no cluster structure is assumed and
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
List of one or more dependent variables, which have to be dichotomous or polytomous. (In R, variables of this kind
are often represented as factors. However, dependent variable may also be of class numeric or character.)
Each dependent variable will result in a separate analysis.
If dependent variable is a multiple imputed variable, all names concerning one variable are interpreted
as its imputations. If dependent variable is a nested imputed variable, the nesting structure has to be specified
in an appropriate listing structure. This structure has to be in agreement with the grouping variable if necessary. If nested imputed
variables are included in the analysis, \code{complete.permutation} is strongly recommended to set to \code{FALSE}.
}
  \item{separate.missing.indikator}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical. Should frequencies of missings in dependent variable be integrated? Note: That is only useful if missing occur as \code{NA}. If the dependent variable
is coded as character, for example \code{'male', 'female', 'missing'}, separate missing indicator is not necessary.
}
  \item{expected.values}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional. A vector auf values expected in dependent variable. Recommend to left this argument empty.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputations of one variable (e.g. dependent variable)
differs from the number of imputations used for group variable(s). If \code{"all"}, number of datasets are determined through
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in 3x5=15 imputed data sets. If \code{"nothing"}, only 5 imputed
data sets will be used, no matter whether the larger number of imputations is a whole multiple of the smaller number of imputations. If \code{"groups"},
only the number of imputations of more than one grouping variable will be permutated.
}
  \item{doCheck}{
Logical: Check the data for consistency before analysis? If \code{TRUE} groups with insufficient data are excluded from
analysis to prevent subsequent functions from crashing.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svymean()} called by \code{svyby()} implemented in the \code{'survey'} package.
Relative frequencies of the categories of the dependent variable are computed by the means of the dichotomous indicators
(e.g. dummy variables) of each category. The results of the several analyses are then pooled according to Rubin's rule,
which is adapted for nested imputations if the \code{dependent} argument implies a nested structure.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the long format. For each subpopulation denoted by the \code{groups} statement, each
dependent variable, each parameter (i.e., the values of the corresponding categories of the dependent variable)
and each coefficient (i.e., the estimate and the corresponding standard error) the corresponding value is given.
}
\references{
  Rubin, D.B. (2003): Nested multiple imputation of NMES
  via partially incompatible MCMC.
  \emph{Statistica Neerlandica} \bold{57, 1}, 3--18.
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
### First example: Computes frequencies of the hisei group conditionally on the
### groups of passed and failed with respect to reading score
freq.tab  <- jk2.table(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD",
             JKZone = "JKZone", JKrep = "JKrep",
             groups = list(country = "country",
                          passed.reading = paste("passed_reading",1:3,sep="" )),
             dependent = list(hisei = paste("zehisei", 1:5, sep = "") ) ,
             complete.permutation = "no" )

### Second example: Computes frequencies of the hisei group conditionally on the
### groups of passed and failed with respect to reading score. Assuming no cluster
### but a nested imputation structure. 'complete.permutation' is recommended to set
### to 'nothing'.
freq.tab  <- jk2.table(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD",
             groups = list(country = "country",
                          passed.reading = paste("passed_reading",1:2,sep="" )),
             dependent = list(hisei = list(nest1 = paste("zehisei", 1:2, sep = "") ,
                                           nest2 = paste("zehisei", 3:4, sep = "") )),
             complete.permutation = "no" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
