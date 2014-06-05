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
jk2.quantile(dat, ID, wgt = NULL, type = c("JK2", "BRR"), PSU = NULL, repInd = NULL, 
            groups = list(), group.splits = length(groups), group.delimiter = "_", 
            dependent = list(), probs = seq(0, 1, 0.25),  na.rm = FALSE,
            complete.permutation = c("nothing", "groups", "all"), nBoot = NULL,
            bootMethod = c("wSampling","wQuantiles"), doCheck = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{file} here~~
Data frame in wide format (i.e. each line represents one ID unit) containing all variables for analysis.
}
  \item{ID}{
%%     ~~Describe \code{dif.term} here~~
Variable name or column number of unique identifier (ID) variable. ID variable must not contain any missing values. 
}
  \item{wgt}{
%%     ~~Describe \code{split.dif} here~~
Optional: Variable name or column number of weighting variable. If no weighting variable is specified,
all cases will be equally weighted.
}
  \item{type}{
%%     ~~Describe \code{abs.dif.bound} here~~
Defines the replication method which is to be applied. 
}
  \item{PSU}{
%%     ~~Describe \code{abs.dif.bound} here~~
Variable name or column number of variable indicating the primary sampling unit (PSU). When a jackknife procedure is applied, 
the PSU is the jackknife zone variable. If \code{NULL}, no cluster structure is assumed and
standard errors are computed according to a random sample.
}
  \item{repInd}{
%%     ~~Describe \code{sig.dif.bound} here~~
Variable name or column number of variable indicating replicate ID. In a jackknife procedure, this is the jackknife replicate 
variable. If \code{NULL}, no cluster structure is assumed and standard errors are computed according to a random sample.
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
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in \eqn{3 \times 5=15} imputed data sets. If \code{"nothing"}, only 5 imputed
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
  \item{doCheck}{
Logical: Check the data for consistency before analysis? If \code{TRUE} groups with insufficient data are excluded from
analysis to prevent subsequent functions from crashing.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on PSU and repInd variables according to JK2 or BRR procedure
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
A data frame in the long format. For each subpopulation denoted by the \code{groups} statement, each
dependent variable, each parameter (i.e., the values of the corresponding categories of the dependent variable)
and each coefficient (i.e., the estimate and the corresponding standard error) the corresponding value is given.
\item{group}{Denotes the group an analysis belongs to. If no groups were specified and/or analysis for the 
whole sample were requested, the value of 'group' is 'wholeGroup'.}
\item{depVar}{Denotes the name of the dependent variable in the analysis. }
\item{modus}{Denotes the mode of the analysis. For example, if a JK2 analysis without sampling weights was 
conducted, 'modus' takes the value 'jk2.unweighted'. If a analysis without any replicates but with sampling
weights was conducted, 'modus' takes the value 'weighted'.}
\item{parameter}{Denotes the parameter of the regression model for which the corresponding value is given
further. For frequency tables, this is the value of the category of the dependent variable which relative
frequency is given further.}
\item{coefficient}{Denotes the coefficient for which the corresponding value is given further. Takes the 
values 'est' (estimate) and 'se' (standard error of the estimate).}
\item{value}{The value of the parameter, i.e. the relative frequency or its standard error.}
If groups were specified, further columns which are denoted by the group names are added to the data frame. 
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
             type = "JK2", PSU = "JKZone", repInd = "JKrep", 
             groups = list(country = "country"),
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
