\name{jk2.glm.M}
\alias{jk2.glm.M}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multicore version of jk2.glm().}
\description{Compute linear regression models for complex cluster designs with multiple imputed variables based on Jackknife (JK2) procedure.
Conceptually, the function combines replication methods and methods for multiple imputed data. Technically, this is a wrapper for the \code{svyglm()} function
of the survey package which uses the parallel package to distribute analysis to different logical processors.
}
\usage{
jk2.glm.M(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), independent = list(),
        reg.statement = NULL, dependent = list(),
        complete.permutation = c("nothing", "groups", "independent", "all") ,
        glm.family, multicoreOptions = list(n.cores = NULL, GBcore = NULL,
        tempFolder = NULL, nameLogfile = NULL) ) }
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
all names concerning one variable are interpreted as its imputations. See example 1, where two group variables are used: One group variable
is multiple imputed, the other is not.
}
  \item{independent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more independent variables. If independent variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details and example 2 for more information.
}
 \item{reg.statement}{
Optional: By default, the regression formula is created automatically  by the independent variables connected by a "+" symbol. However, when interaction terms should
be defined, the regression has to be specified by the user. The right side of the regression formula has to defined in \code{"reg.statement"} as a string, whereas the
names of the independent variables occurring in the \code{"independent"} argument have to be used. This is due to various names of one variable in multiple imputed data
sets. See example 3 for further details.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more dependent variables. Each dependent variable will result in a separate analysis.
If dependent variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details and example 1 for more information. If dependent variable is a nested imputation variable, the nesting structure has to be specified
in an appropriate listing structure. This structure has to be in agreement with the grouping variable and the independent variable if necessary. If nested imputation
variables are included in the analysis, \code{complete.permutation} is strongly recommended to set to \code{FALSE}. If you are interested in one single dependent variable
which may be labeled as \code{"reading_competence_plausible_value"} in the data file,
type \code{dependent = list(reading = "reading_competence_plausible_value"}. If you are interested in one multiple imputed dependent variable
labeled as \code{"reading_PV1"}, \code{"reading_PV2"}, \code{"reading_PV3"} and so on, type \code{dependent = list(reading = c("reading_PV1", "reading_PV2", "reading_PV3"))}.
If you are interested in one multiple imputed dependent variable with a nesting structure, where you have \code{n = 2} nests and \code{m = 3} imputations in each nest, your
corresponding variables may be labeled as \code{"PV1_1"}, \code{"PV1_2"}, \code{"PV1_3"}, \code{"PV2_1"}, \code{"PV2_2"}, \code{"PV2_3"}. In that case the nesting structure has to
be reproduced in an appropriate list structur. Therefore type \code{dependent = list(reading = list(nest1 = c("PV1_1","PV1_2","PV1_3"), nest2 = c("PV2_1","PV2_2","PV2_3"))) }.
Example 3 gives a short introduction to this concept.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument defines the number of multiple imputed data sets. In general, the number is defined by the number of imputations
of a variable. Therefore, this argument only becomes relevant, if the number of imputation of one variable
(e.g. the number of plausible values of the dependent variable) differs from the number of imputations used for independent variable(s).
If \code{"all"}, number of datasets are determined through permutation, e.g. 5 plausible values and 3 imputations of the independent variable
results in 3x5=15 imputed data sets. If \code{"nothing"}, only 5 imputed data sets will be used, no matter whether the larger number of imputations
is a whole multiple of the smaller number of imputations. If \code{"groups"}, only the number of imputations of more than one grouping variable will be permutated.
If \code{"independent"}, only the number of imputations of more than one independent variable will be permutated.
}
  \item{glm.family}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument of class \code{"family"}, specifying the link function. See help file of \code{'glm()'} function for details.
}
  \item{multicoreOptions}{
%%     ~~Describe \code{sig.dif.bound} here~~
A list of several optional parameters concerning multicore processing.
\describe{
 \item{\code{n.cores}}{Desired number of cores to be used for analysis. If \code{NULL}, function chooses appropriate number of cores 
   considering system memory.}
 \item{\code{GBcore}}{Desired memory (in giga byte) of RAM preserved for each core. For example, if you work on a quad core machine with 4 GB
 RAM and want to preserve 2 GB for each core, type \code{multicoreOptions = list ( GBcore = 2 )}. This will result in automatically using only 2 cores instead of 4.
 If \code{NULL}, no restriction on cores to be used is made. \code{n.cores} will be used than, or, if \code{n.cores = NULL}, all available cores will be used.}
 \item{\code{tempFolder}}{Multicore operation does not allow to print messages on console. Therefore, a log file will be written in the
 specified folder. If no folder is specified, no log file will be created.}
 \item{\code{nameLogfile}}{Name of the logfile. If no name is specified, file will be named \code{analyse.log}. If \code{analyse.log} already exists in the
 specified folder, logfile will be renamed automatically \code{analyse1.log}.}
 }
 }
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on \code{JKZone} and \code{JKrep} variables according to JK2 procedure
implemented in WesVar. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svyglm()} implemented in the \code{'survey'} package.
The results of the several analyses are then pooled according to Rubin's rule, which is adapted for nested imputations if the \code{dependent} argument implies a nested structure.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, one for each dependent variable. Each data frame contains columns for group variables (if specified),
regression weights, standard errors and r squared. Each row in the data frame corresponds to one parameter (specified in column \code{"reg"})
in one group (specified in the column which name refers to the group name) for one dependent variable (specified in the name of the
corresponding list element).
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
### Example 1: Computes linear regression from reading score on gender separately for each
### country and each hisei. This serves as an example for two group variables, where one
### group variable is a multiple imputed one.
#mod1      <- jk2.glm.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#             JKrep = "JKrep",
#             group = list(country = "country", hisei = paste("zehisei", 1:5, sep = "")),
#             dependent = list(reading = paste("reading_score", 1:3, sep = ""),
#                              writing = paste("writing_score", 1:3, sep = "")),
#             independent = list(gender = "sex"),
#             complete.permutation = "no", glm.family = gaussian(link="identity") )
### Example 2: Computes log linear regression from pass/fail on hisei and gender separately
### for each country
#mod2      <- jk2.glm.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#             JKrep = "JKrep", group = list(country = "country"),
#             dependent = list(reading.pass = paste("passed_reading", 1:3, sep = ""),
#                              writing.pass = paste("passed_writing", 1:3, sep = "")),
#             independent = list(hisei = paste("zehisei", 1:5, sep = ""), gender = "sex"),
#             complete.permutation = "no", glm.family = binomial(link="logit") )
### Example 3: Computes linear regression from pass/fail on hisei and gender (and its' interaction)
### separately for each country. The regression statement has to be defined manually. Note that,
### despite "hisei" occurs as a multiple imputed variable, only one statement has to be defined.
### Specifically, write "hisei*gender" instead of "zehisei1*sex" in the reg.statement argument.
### Moreover, the dependent variable herin is defined as a nested imputed variable with two nests
### and 3 imputations in each nest. The group and independent variable(s) have to correspond to
### the nested structure, i.e. they have to be specified as a single variable (i.e. without any
### imputations), or they also may be imputed in two nests with 3 imputations in each nest, or
### they may be imputed with two imputations (according to the number of nests in the dependent
### variable.
### In example 3, group variable has no imputations, and the first independent variable has 2
### imputations (according to the two nests), whereas the second independent variable also has
### no imputations.
#mod3  <- jk2.glm.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#         JKrep = "JKrep", group = list(country = "country"),
#         dependent = list(ability = list ( nest1 = paste("passed_reading", 1:3, sep = ""),
#                                           nest2 = paste("passed_writing", 1:3, sep = ""))),
#         independent = list(hisei = paste("zehisei", 1:2, sep = ""), gender = "sex"),
#         reg.statement = "hisei*gender",
#         complete.permutation = "no", glm.family = binomial(link="logit") )
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
