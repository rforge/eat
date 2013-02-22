\name{jk2.mean.M}
\alias{jk2.mean.M}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multicore version of jk2.mean().}
\description{Compute means, variances and standard deviations with standard errors
for complex cluster designs with multiple imputed variables (e.g. plausible values) based on Jackknife (JK2)
procedure. Conceptually, the function combines replication methods and methods for multiple imputed data. Nested imputations of the dependent variable(s) are supported as well.
Technically, this is a wrapper for the \code{svymean()} and \code{svyvar()} functions of the survey package which uses the
parallel package to distribute analysis to different logical processors.}
\usage{
jk2.mean.M(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), dependent = list(), na.rm = FALSE, 
          complete.permutation = c("nothing", "groups", "all"), forcePooling = TRUE,
          multicoreOptions = list(n.cores = NULL, GBcore = NULL, tempFolder = NULL, nameLogfile = NULL) )}
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
all names concerning one variable are interpreted as its imputations. If a nesting structure in the dependent variable is implemented,
all grouping variables are expected to have no imputations (i.e. only one variable in the data frame) or exactly as many imputations as
nests are specified in the dependent variable. For example, if two nests are specified in the dependent variable, a statement like this may be appropriate:
\code{group = list(gender = "sex", hisei = c("hisei1","hisei2"))}. The function will be assume that no imputation takes place for variable \code{gender} and
two imputations for variable \code{hisei}, corresponding to the two nests. Conversely, a statement like \code{group = list(gender = "sex", hisei = c("hisei1","hisei2","hisei3"))}
would be inconsistent. Albeit the function will not crash, the corresponding standard errors may not be trustworthy.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more dependent variables. Each dependent variable will result in a separate analysis.
If dependent variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. If dependent variable is a nested imputation variable, the nesting structure has to be specified
in an appropriate listing structure. This structure has to be in agreement with the grouping variable if necessary. If nested imputation
variables are included in the analysis, \code{complete.permutation} is strongly recommended to set to \code{FALSE}. To give a short example:
If you are interested in one single dependent variable which may be labeled as \code{"reading_competence_plausible_value"} in the data file,
type \code{dependent = list(reading = "reading_competence_plausible_value"}. If you are interested in one multiple imputed dependent variable
labeled as \code{"reading_PV1"}, \code{"reading_PV2"}, \code{"reading_PV3"} and so on, type \code{dependent = list(reading = c("reading_PV1", "reading_PV2", "reading_PV3"))}.
If you are interested in one multiple imputed dependent variable with a nesting structure, where you have \code{n = 2} nests and \code{m = 3} imputations in each nest, your
corresponding variables may be labeled as \code{"PV1_1"}, \code{"PV1_2"}, \code{"PV1_3"}, \code{"PV2_1"}, \code{"PV2_2"}, \code{"PV2_3"}. In that case the nesting structure has to
be reproduced in an appropriate list structur. Therefore type \code{dependent = list(reading = list(nest1 = c("PV1_1","PV1_2","PV1_3"), nest2 = c("PV2_1","PV2_2","PV2_3"))) }.
See details and examples for more information.
}
  \item{na.rm}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should cases with missing values be dropped?
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputation of one variable (e.g. plausible values of the
dependent variable) differs from the number of imputations used for group variable(s). If \code{"all"}, number of datasets are determined through
permutation, e.g. 5 plausible values and 3 imputations of one group variable results in 3x5=15 imputed data sets. If \code{"nothing"}, only 5 imputed
data sets will be used, no matter whether the larger number of imputations is a whole multiple of the smaller number of imputations. If \code{"groups"},
only the number of imputations of more than one grouping variable will be permutated. Note: Within the logic of nested imputations, \code{complete.permutation}
should be set to \code{FALSE}. See examples for further details.
}
  \item{forcePooling}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: If variables in groups or subgroups does not have a positive variance, standard errors cannot be computed or pooled.
Function will abort with an error message consequently. However, \code{forcePooling = TRUE} force the algorithm to continue.
Invalid variance estimators and standard errors will set to zero to allow pooling. A warning is printed to console.
}
  \item{multicoreOptions}{
%%     ~~Describe \code{sig.dif.bound} here~~
A list of several parameters concerning multicore processing.
\describe{
 \item{\code{n.cores}}{Desired number of cores to be used for analysis. If \code{NULL}, function chooses appropriate number of cores 
   considering system memory.}
 \item{\code{GBcore}}{Desired memory (in giga byte) of RAM preserved for each core. For example, if you work on a 4 core machine with 4 GB
 RAM and want to preserve 2 GB for each core, only 2 cores are used instead of 4.}
 \item{\code{tempFolder}}{Multicore operation does not allow to print messages on console. Therefore, a log file will be written in the
 specified folder. If no folder is specified, no log file will be created.}
 \item{\code{nameLogfile}}{Name of the logfile. If no name is specified, file will be named \code{analyse.log}. }
 }
 }
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svymean()} called by \code{svyby()} implemented in the 'survey' package.
The results of the several analyses are then pooled according to Rubin's rule. Additionally, the multicore option 
allows to distribute the replications on several logical processors. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, one for each dependent variable. Each data frame contains means, variances, standard deviations and
standard errors for all of them. If group differences were estimated, they occur as attributes of each data frame. See
examples for further details.
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
## Not run:
### First example: only means, SD and variances for each country
#means  <- jk2.mean.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#          JKrep = "JKrep", group = list(country = "country"),
#          dependent = list(reading = paste("reading_score",1:3,sep=""),
#                           writing = paste("writing_score",1:3,sep="") ),
#          complete.permutation = "all" )
#

### Second example: Sex differences by country
#means  <- jk2.mean.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#          JKrep = "JKrep", group = list(country = "country", GENDER = "sex"),
#          group.differences.by = "GENDER",
#          dependent = list(reading = paste("reading_score",1:3,sep=""),
#                           writing = paste("writing_score",1:3,sep="") ),
#          complete.permutation = "all" )
#attr(means[[1]], "difference")

### Third example: Nested imputations of dependent variable
### First split the income in above and below 2000
#reading_writing[,"income1"] <- ifelse(reading_writing[,"income1"]>2000,1,0)
#reading_writing[,"income2"] <- ifelse(reading_writing[,"income2"]>2000,1,0)
### Assuming 2 nests (i.e. variable "income" with 2 imputations), and one common dependent variable
### of reading and writing. Note that complete.permutation should be set to FALSE
#means  <- jk2.mean.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#          JKrep = "JKrep", group = list(country = "country", INCOME = c("income1","income2")),
#          dependent = list(ability = list(nest1 = paste("reading_score",1:3,sep=""),
#                                          nest2 = paste("writing_score",1:3,sep="") )),
#          complete.permutation = "no" )
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
