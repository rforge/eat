\name{jk2.table}
\alias{jk2.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK1, JK2 and BRR for frequency tables.}
\description{Compute frequency tables for categorical variables (e.g. factors: dichotomous or polytomous) in complex
cluster designs. Estimation of standard errors optionally takes the clustered structure and multiple imputed
variables into account. To date, Jackknife-1 (JK1), Jackknife-2 (JK2) and Balanced repeated replicate (BRR) methods are 
implemented to account for clustered designs. Procedures of Rubin (1987) and Rubin (2003) are implemented to account for 
multiple imputed data and nested imputed data, if necessary. Conceptually, the function combines replication and imputation 
methods. Technically, this is a wrapper for the svymean() function of the survey package.
}
\usage{
jk2.table(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"), PSU = NULL, repInd = NULL, 
          repWgt = NULL, nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups), 
          group.differences.by = NULL, chiSquare = FALSE, correct = TRUE, group.delimiter = "_", 
          trend = NULL, linkErr = NULL, dependent, separate.missing.indicator = FALSE, na.rm=FALSE, 
          expected.values = NULL, doCheck = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datL}{
%%     ~~Describe \code{file} here~~
Data frame in the long format (i.e. each line represents one ID unit in one imputation of one nest) containing all 
variables for analysis.
}
  \item{ID}{
%%     ~~Describe \code{dif.term} here~~
Variable name or column number of student identifier (ID) variable. ID variable must not contain any missing values. 
}
  \item{wgt}{
%%     ~~Describe \code{split.dif} here~~
Optional: Variable name or column number of weighting variable. If no weighting variable is specified,
all cases will be equally weighted.
}
  \item{type}{
%%     ~~Describe \code{abs.dif.bound} here~~
Defines the replication method for cluster replicates which is to be applied. Without cluster replicates (i.e., if \code{PSU} 
and/or \code{repInd} is NULL, \code{type} will be ignored.
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
  \item{repWgt}{
%%     ~~Describe \code{sig.dif.bound} here~~
Normally, replicate weights are created by \code{jk2.table} directly from \code{PSU} and \code{repInd} variables. Alternatively, 
if replicate weights are included in the data.frame, specify the variable names or column number in the \code{repWgt} argument.
}
  \item{nest}{Optional: name or column number of the nesting variable. Only applies in nested multiple imputed data sets.
}
  \item{imp}{Optional: name or column number of the imputation variable. Only applies in multiple imputed data sets.  
}
  \item{groups}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: vector of names or column numbers of one or more grouping variables. 
}
  \item{group.splits}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: If groups are defined, \code{group.splits} optionally specifies whether analysis should be done also
in the whole group or overlying groups. See examples for more details.
}
  \item{group.differences.by}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Specifies one grouping variable for which a chi-square test should be applied. 
The corresponding variable must be included in the \code{groups} statement. If specified, the 
distribution of the dependent variable is compared between the groups. See examples for 
further details.
}
  \item{chiSquare}{
Logical. Applies only if 'group.differences.by' was specified and does not contain 'wholePop'. Defines whether 
group differences should be represented in a chi square test or in (mean) differences of each 
group's relative frequency. 
}
  \item{correct}{
Logical. Applies only if 'group.differences.by' is requested without cluster replicates. A logical indicating whether to apply 
continuity correction when computing the test statistic for 2 by 2 tables. See help page of 'chisq.test' for further details. 
}
  \item{group.delimiter}{
%%     ~~Describe \code{sig.dif.bound} here~~
Character string which separates the group names in the output frame.
}
  \item{trend}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: name or column number of the trend variable. Note: Trend variable must have exact two levels. Levels for 
grouping variables must be equal in both 'sub populations' partitioned by the trend variable. 
}
  \item{linkErr}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: name or column number of the trend variable. If 'NULL', a linking error of 0 will be assumed in trend estimation. 
Alternatively, the linking error may be given as a single scalar value (i.e. 'linkErr = 1.225'). 
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
Variable name or column number of the dependent variable.
}
  \item{separate.missing.indicator}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical. Should frequencies of missings in dependent variable be integrated? Note: That is only useful if missing occur as \code{NA}. If the dependent variable
is coded as character, for example \code{'male', 'female', 'missing'}, separate missing indicator is not necessary.
}
  \item{na.rm}{
Logical: Should cases with missing values be dropped?
}
  \item{expected.values}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional. A vector auf values expected in dependent variable. Recommend to left this argument empty.
}
  \item{doCheck}{
Logical: Check the data for consistency before analysis? If \code{TRUE} groups with insufficient data are excluded from
analysis to prevent subsequent functions from crashing.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on PSU and repInd variables according to JK2 procedure
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
\item{group}{Denotes the group an analysis belongs to. If no groups were specified and/or analysis for the 
whole sample were requested, the value of \sQuote{group} is \sQuote{wholeGroup}.}
\item{depVar}{Denotes the name of the dependent variable in the analysis. }
\item{modus}{Denotes the mode of the analysis. For example, if a JK2 analysis without sampling weights was 
conducted, \sQuote{modus} takes the value \sQuote{jk2.unweighted}. If a analysis without any replicates but with sampling
weights was conducted, \sQuote{modus} takes the value \sQuote{weighted}.}
\item{parameter}{Denotes the parameter of the regression model for which the corresponding value is given
further. For frequency tables, this is the value of the category of the dependent variable which relative
frequency is given further.}
\item{coefficient}{Denotes the coefficient for which the corresponding value is given further. Takes the 
values \sQuote{est} (estimate) and \sQuote{se} (standard error of the estimate).}
\item{value}{The value of the parameter, i.e. the relative frequency or its standard error.}
If groups were specified, further columns which are denoted by the group names are added to the data frame. 
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
data(hisei)
### First example: Computes frequencies of the hisei group conditionally on country
### The distribution of the hisei categories is compared for eachcategory separately, 
### i.e. no chi square test is used. 
freq.tab  <- jk2.table(datL = hisei, ID = "idstud", wgt = "wgtSTUD",
             imp="imputation", type = "JK2", PSU = "JKZone", repInd = "JKrep",
             groups = "country", group.differences.by = "country", dependent = "hisei")

### Second example: Computes frequencies of passed and failed conditionally on the
### groups of sex and country. Assuming no cluster but a nested imputation structure. 
freq.tab  <- jk2.table(datL = reading, ID = "idstud", wgt = "wgtSTUD",
             nest="nest", imp="imputation", groups = c("country","sex"), dependent = "passed")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
