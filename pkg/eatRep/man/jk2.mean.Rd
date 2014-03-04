\name{jk2.mean}
\alias{jk2.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for mean estimates.}
\description{Compute totals, means, variances and standard deviations with standard errors
for complex cluster designs with multiple imputed variables (e.g. plausible values) based on Jackknife (JK2)
procedure. Conceptually, the function combines replication methods and methods for multiple imputed data. Nested imputations of the dependent variable(s) are supported as well.
Technically, this is a wrapper for the \code{svymean()} and \code{svyvar()} functions of the \code{'survey'} package.}
\usage{
jk2.mean(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(),
         group.splits = length(groups), group.differences.by = NULL,
         group.delimiter = "_", dependent = list(), na.rm = FALSE,
         complete.permutation = c("nothing", "groups", "all"),
         forcePooling = TRUE, boundary = 3, doCheck = TRUE)}
         
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
all names concerning one variable are interpreted as its imputations. If a nesting structure in the dependent variable
is implemented, all grouping variables are expected to have no imputations (i.e. only one variable in the data frame)
or exactly as many imputations as nests are specified in the dependent variable. For example, if two nests are specified
in the dependent variable, a statement like this may be appropriate:
\code{group = list(gender = "sex", hisei = c("hisei1","hisei2"))}. The function will be assume that no imputation
takes place for variable \code{gender} and two imputations for variable \code{hisei}, corresponding to the two nests.
Conversely, a statement like \code{group = list(gender = "sex", hisei = c("hisei1","hisei2","hisei3"))}
would be inconsistent. Albeit the function will not crash, the corresponding standard errors may not be trustworthy.
}
  \item{group.splits}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: If groups are defined, \code{group.splits} optionally specifies whether analysis should be done also
in the whole group or overlying groups. See examples for more details.
}
  \item{group.differences.by}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Specifies variable group differences should be computed for.
}
  \item{group.delimiter}{
%%     ~~Describe \code{sig.dif.bound} here~~
Character string which separates the group names in the output frame.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more dependent variables. Each dependent variable will result in a separate analysis.
If dependent variable is a multiple imputed variable, all names concerning one variable are interpreted
as its imputations. If dependent variable is a nested imputed variable, the nesting structure has to be specified
in an appropriate listing structure. This structure has to be in agreement with the grouping variable if necessary. If nested imputed
variables are included in the analysis, \code{complete.permutation} is strongly recommended to set to \code{FALSE}. To give a short example:
If you are interested in one single dependent variable
which may be labeled as \code{"reading_competence_plausible_value"} in the data file,
type \code{dependent = list(reading = "reading_competence_plausible_value")}. This statement does not imply any imputation structure, therefore standard errors will not be pooled after
the analysis. If your dependent variable is a multiple imputed one whereas the imputations are labeled as \code{"r_PV1"}, \code{"r_PV2"}, \code{"r_PV3"} and so on,
type \code{dependent = list(reading = c("r_PV1", "r_PV2", "r_PV3"))}. This is to define three imputations
belonging to one common scale which you can label arbitrarily. The results afterwards will be pooled according to Rubin (1987). If you are interested in one multiple imputed
dependent variable with a nesting structure, where you have \emph{N} = 2 nests and \emph{M} = 3 imputations in each nest, your
corresponding variables may be labeled as \code{"PV1_1"}, \code{"PV1_2"}, \code{"PV1_3"}, \code{"PV2_1"}, \code{"PV2_2"}, \code{"PV2_3"}. In that case the nesting structure has to
be reproduced in an appropriate list structur. Therefore type \code{dependent = list(reading = list(nest1 = c("PV1_1","PV1_2","PV1_3"), nest2 = c("PV2_1","PV2_2","PV2_3"))) }. This
is to define which variables belong to which nest, whereas all variables belong to one common scale. The results afterwards will be pooled according to Rubin (2003).
See details and examples for more information.
}
  \item{na.rm}{
Logical: Should cases with missing values be dropped?
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputations of one variable (e.g. plausible values of the
dependent variable) differs from the number of imputations used for group variable(s). If \code{"all"}, number of datasets are determined through
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in 3 x 5 = 15 imputed data sets. If \code{"nothing"}, only 5 imputed
data sets will be used, no matter whether the larger number of imputations is a whole multiple of the smaller number of imputations. If \code{"groups"},
only the number of imputations of more than one grouping variable will be permutated. Note: Within the logic of nested imputations, \code{complete.permutation}
should be set to \code{FALSE}. See examples for further details.
}
  \item{forcePooling}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: If variables in groups or subgroups does not have a positive variance, standard errors cannot be computed or pooled.
Function will abort with an error message consequently. However, \code{forcePooling = TRUE} forces the algorithm to continue until the
number of missing values does not exceed the criteria defined in \code{boundary}.
Invalid variance estimators and standard errors will set to zero to allow pooling. A warning is printed to console. However, it is strongly
advised not to trust in starndard errors then.
}
  \item{boundary}{
Numerical: Defines the maximum number of missing values in standard errors to continue pooling when \code{forcePooling == TRUE}.
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
The function afterwards serves as a wrapper for \code{svymean()} called by \code{svyby()} implemented in
the 'survey' package.
The results of the several analyses are then pooled according to Rubin's rule.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the long format. For each subpopulation denoted by the \code{groups} statement, each
dependent variable, each parameter (i.e., mean, variance, or group differences) and each coefficient (i.e., the estimate
and the corresponding standard error) the corresponding value is given.
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
### First example: only means, SD and variances for each country
means  <- jk2.mean(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
          JKrep = "JKrep", groups = list(country = "country"),
          dependent = list(reading = paste("reading_score",1:3,sep=""),
                           writing = paste("writing_score",1:3,sep="") ),
          complete.permutation = "all" )

### Second example: Sex differences by country. Assume equally weighted cases by omitting
### 'wgt' argument.
means  <- jk2.mean(dat = reading_writing, ID = "idstud", JKZone = "JKZone",JKrep = "JKrep",
          groups = list(country = "country", GENDER = "sex"), group.differences.by = "GENDER",
          dependent = list(reading = paste("reading_score",1:3,sep=""),
                           writing = paste("writing_score",1:3,sep="") ),
          complete.permutation = "all" )

### Third example: Nested imputations of dependent variable
### First split the income in above and below 2000
reading_writing[,"incomeS1"] <- ifelse(reading_writing[,"income1"]>2000,1,0)
reading_writing[,"incomeS2"] <- ifelse(reading_writing[,"income2"]>2000,1,0)
### Assuming 2 nests (i.e. variable "income" with 2 imputations), and one common dependent variable
### of reading and writing. Note that complete.permutation should be set to FALSE
means  <- jk2.mean(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
          JKrep = "JKrep", groups = list(country = "country", INCOME = c("incomeS1","incomeS2")),
          dependent = list(ability = list(nest1 = paste("reading_score",1:3,sep=""),
                                          nest2 = paste("writing_score",1:3,sep="") )),
          complete.permutation = "no" )

### Fourth example: Assume a completely random sample (e.g. no cluster structure and no weights),
### and give descriptives for subpopulations and the overlying populations
means  <- jk2.mean(dat = reading_writing, ID = "idstud",
          groups = list(country = "country", GENDER = "sex"),
          group.splits = c(0:2),
          dependent = list(reading = paste("reading_score",1:3,sep=""),
                           writing = paste("writing_score",1:3,sep="") ),
          complete.permutation = "all" )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
