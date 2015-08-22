\name{jk2.glm}
\alias{jk2.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for linear regression models.}
\description{Compute generalized linear models for complex cluster designs with multiple imputed variables based 
on the Jackknife (JK2) or balanced repeated replicates (BRR) procedure. Conceptually, the function combines replication 
methods and methods for multiple imputed data. Technically, this is a wrapper for the \code{svyglm()} function
of the \code{'survey'} package.
}
\usage{
jk2.glm(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"), PSU = NULL, repInd = NULL, 
        nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups), 
        group.delimiter = "_", formula, family=gaussian, 
        forceSingularityTreatment = FALSE, doCheck = TRUE, na.rm = FALSE )}
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
  \item{group.delimiter}{
%%     ~~Describe \code{sig.dif.bound} here~~
Character string which separates the group names in the output frame.
}
  \item{formula}{
%%     ~~Describe \code{sig.dif.bound} here~~
Model formula, see help page of \code{glm} for details. 
}
  \item{family}{
%%     ~~Describe \code{sig.dif.bound} here~~
A description of the error distribution and link function to be used in the model. See help page of \code{glm} for details. 
}
  \item{forceSingularityTreatment}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Forces the function to use the workaround to handle singularities in regression models.
}
  \item{doCheck}{
Logical: Check the data for consistency before analysis? If \code{TRUE} groups with insufficient data are excluded from
analysis to prevent subsequent functions from crashing.
}
  \item{na.rm}{
Logical: Should cases with missing values be dropped?
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Function first creates replicate weights based on \code{PSU} and \code{repInd} variables according to JK2 or 
BRR procedure. According to multiple imputed data sets, a workbook with several analyses is created.
The function afterwards serves as a wrapper for \code{svyglm()} implemented in the \code{'survey'} package.
The results of the several analyses are then pooled according to Rubin's rule, which is adapted for nested 
imputations if the \code{dependent} argument implies a nested structure.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the long format with at least six columns. For each subpopulation denoted by the \code{groups} 
statement, each dependent variable, each parameter and each coefficient the corresponding value is given. 
\item{group}{Denotes the group an analysis belongs to. If no groups were specified and/or analysis for the 
whole sample were requested, the value of \sQuote{group} is \sQuote{wholeGroup}.}
\item{depVar}{Denotes the name of the dependent variable in the analysis. }
\item{modus}{Denotes the mode of the analysis. For example, if a JK2 analysis without sampling weights was 
conducted, \sQuote{modus} takes the value \sQuote{jk2.unweighted}. If a analysis without any replicates but with sampling
weights was conducted, \sQuote{modus} takes the value \sQuote{weighted}.}
\item{parameter}{Denotes the parameter of the regression model for which the corresponding value is given
further. Amongst others, the \sQuote{parameter} column takes the values \sQuote{(Intercept)} and \sQuote{gendermale} if \sQuote{gender} 
was the dependent variable, for instance. See example 1 for further details.}
\item{coefficient}{Denotes the coefficient for which the corresponding value is given further. Takes the 
values \sQuote{est} (estimate) and \sQuote{se} (standard error of the estimate).}
\item{value}{The value of the parameter estimate in the corresponding group.}
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
data(reading)
### Example 1: Computes linear regression from reading score on gender separately for each
### country. Assume no nested structure. 
readingNest1 <- reading[which(reading[,"nest"] == 1),]
mod1  <- jk2.glm(datL = readingNest1, ID = "idstud", wgt = "wgtSTUD", type = "JK2", 
         PSU = "JKZone", repInd = "JKrep", imp = "imputation", 
         groups = "country",  formula = score~sex)
dG(mod1)         

### Example 2: Computes log linear regression from pass/fail on hisei and gender
### separately for each country in a nested structure
mod1  <- jk2.glm(datL = reading, ID = "idstud", wgt = "wgtSTUD", type = "JK2", 
         PSU = "JKZone", repInd = "JKrep", imp = "imputation", nest="nest", 
         groups = "country",  formula = passed~sex*income, family = binomial(link="logit"))
dG(mod1)         
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
