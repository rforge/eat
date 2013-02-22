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
as its imputations. See details and example 1 for more information.
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
Argument of class "family", specifying the link function. See help file of 'glm' for details.
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
The function afterwards serves as a wrapper for svyglm() implemented in the 'survey' package.
The results of the several analyses are then pooled according to Rubin's rule.
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
#mod3      <- jk2.glm.M(dat = reading_writing, ID = "idstud", wgt = "wgtSTUD", JKZone = "JKZone",
#             JKrep = "JKrep", group = list(country = "country"),
#             dependent = list(reading.pass = paste("passed_reading", 1:3, sep = ""),
#                              writing.pass = paste("passed_writing", 1:3, sep = "")),
#             independent = list(hisei = paste("zehisei", 1:5, sep = ""), gender = "sex"),
#             reg.statement = "hisei*gender",
#             complete.permutation = "no", glm.family = binomial(link="logit") )
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
