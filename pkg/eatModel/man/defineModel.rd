\name{defineModel}
\alias{defineModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prepares IRT analysis for Conquest and TAM}
\description{Facilitates data analysis using the software Conquest and/or TAM. It automatically
checks data for IRT consistency, generates Conquest syntax, label, anchor and data files or
corresponding TAM call for a single model specified by several arguments in R. Finally, an
R object is created which contain the required input for Conquest or TAM. To start the estimation,
call \code{runModel} with the argument returned by \code{defineModel}.}
\usage{
defineModel (dat, items, id, splittedModels = NULL,
   irtmodel = c("1PL", "2PL", "PCM", "PCM2", "RSM", "GPCM", "2PL.groups", "GPCM.design", "3PL"),
   qMatrix=NULL, DIF.var=NULL, HG.var=NULL, group.var=NULL, weight.var=NULL, anchor = NULL, 
   check.for.linking = TRUE, boundary = 6, remove.boundary = FALSE, remove.no.answers = TRUE,
   remove.no.answersHG = TRUE, remove.missing.items = TRUE, remove.constant.items = TRUE, 
   remove.failures = FALSE, remove.vars.DIF.missing = TRUE, remove.vars.DIF.constant = TRUE, 
   verbose=TRUE, software = c("conquest","tam"), dir = NULL, analysis.name, 
   withDescriptives = TRUE, model.statement = "item",  compute.fit = TRUE, 
   n.plausible=5, seed = NULL, conquest.folder=NULL,constraints=c("cases","none","items"),
   std.err=c("quick","full","none"), distribution=c("normal","discrete"),
   method=c("gauss", "quadrature", "montecarlo"), n.iterations=2000,
   nodes=NULL, p.nodes=2000, f.nodes=2000,converge=0.001,deviancechange=0.0001,
   equivalence.table=c("wle","mle","NULL"), use.letters=FALSE,
   allowAllScoresEverywhere = TRUE, guessMat = NULL, est.slopegroups = NULL,
   progress = FALSE, increment.factor=1 , fac.oldxsi=0, 
   export = list(logfile = TRUE, systemfile = FALSE, history = TRUE,
   covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE))}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{file} here~~
A data frame containing all variables necessary for analysis.
}
  \item{items}{
%%     ~~Describe \code{dif.term} here~~
Names or column numbers of variables with item responses. Item response values must 
be numeric (i.e. 0, 1, 2, 3 ... ). Character values (i.e. A, B, C ... or a, b, c, ...) 
are not allowed. Class of item columns are expected to be numeric or integer. 
Columns of class \code{character} will be transformed. 
}
  \item{id}{
%%     ~~Describe \code{split.dif} here~~
Name or column number of the identifier (ID) variable.
}
  \item{splittedModels}{
%%     ~~Describe \code{dif.term} here~~
Optional: Object returned by 'splitModels'. Definition for multiple model handling. 
}
  \item{irtmodel}{
%%     ~~Describe \code{abs.dif.bound} here~~
Specification of the IRT model. The argument corresponds to the \code{irtmodel} 
argument of \code{tam}. See the help page of \code{tam} for further details.
}
  \item{qMatrix}{
%%     ~~Describe \code{abs.dif.bound} here~~
Optional: A named data frame indicating how items should be grouped to dimensions. The
first column contains the names of all items and should be named item. The other
columns contain dimension definitions and should be named with the respective
dimension names. A positive value (e.g., 1 or 2 or 1.4) indicates the loading weight
with which an item loads on the dimension, a value of 0 indicates that the respective
item does not load on this dimension. If no q matrix is specified by the user, an
unidimensional structure is assumed.
}
  \item{DIF.var}{
%%     ~~Describe \code{sig.dif.bound} here~~
Name or column number of one grouping variable for which differential item
functioning analysis is to be done.
}
  \item{HG.var}{
  Optional: Names or column numbers of one or more context variables (e.g., sex, school).
These variables will be used for latent regression model in ConQuest or TAM.
}
  \item{group.var}{
Optional: Names or column numbers of one or more grouping variables. Descriptive
statistics for WLEs and Plausible Values will be computed separately for each
group in ConQuest.
}
  \item{weight.var}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: Name or column number of one weighting variable.
}
  \item{anchor}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: A named data frame with anchor parameters. The first column contains the
names of all items which are used to be anchor items and should be named item.
The second column contains anchor parameters. Anchor items can be a subset of the
items in the dataset and vice versa.
}
  \item{check.for.linking}{
%%     ~~Describe \code{sig.dif.bound} here~~
A logical value indicating whether the items in dataset are checked for being
connected with each other via design.
}
  \item{boundary}{
%%     ~~Describe \code{sig.dif.bound} here~~
Numerical: A message is printed on console if a subject has answered less than the number of items 
defined in boundary. 
}
  \item{remove.boundary}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Remove subjects who have answered less items than defined in the \code{boundary} argument?
}
  \item{remove.no.answers}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should persons without any item responses being removed prior to analysis?
}
  \item{remove.no.answersHG}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should persons without any responses on any background variable being removed prior to analysis?
}
  \item{remove.missing.items}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should items without any item responses being removed prior to analysis?
}
  \item{remove.constant.items}{
Logical: Should items without variance being removed prior to analysis?
}
  \item{remove.failures}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical: Should persons without any correct item response (i.e., only \dQuote{0} responses) being removed prior to analysis?
}
  \item{remove.vars.DIF.missing}{
Logical: Applies only in DIF analyses. Should items without any responses in at least one 
DIF group being removed prior to analyses? Note: Conquest may crash if these items
remain in the data. 
}
  \item{remove.vars.DIF.constant}{
Logical: Applies only in DIF analyses. Should items without variance in at least one 
DIF group being removed prior to analyses? Note: Conquest may crash if these items 
remain in the data. 
}
  \item{verbose}{
A logical value indicating whether messages are printed on the R console.
}
  \item{software}{
%%     ~~Describe \code{dif.term} here~~
The desired estimation software for the analysis.
}
  \item{dir}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. An already existsing directory in which the output will be written to.
}
  \item{analysis.name}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string specifying the analysis name.
All Conquest input and output files will named \code{analysis.name} with their corresponding extensions.
}
  \item{withDescriptives}{
%%     ~~Describe \code{dif.term} here~~
Logical: Compute descriptives (e.g., p values, number of valid items, empirical discrimination)?
}
  \item{model.statement}{
%%     ~~Describe \code{dif.term} here~~
Optional: Applies only if \code{software = "conquest"}. A character string given the model
statement in the Conquest syntax. If omitted, the statement is generated automatically
with respect to the defined model.
}
  \item{compute.fit}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. Compute item fit statistics?
}
  \item{n.plausible}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. The number of plausible values which are
to be drawn from the conditioning model.
}
  \item{seed}{
%%     ~~Describe \code{dif.term} here~~
Optional: Set seed value for analysis.
}
  \item{conquest.folder}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string with path and name
of the ConQuest console, for example \code{"c:/programme/conquest/console_Feb2007.exe"}.
}
  \item{constraints}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string specifying how
the scale should be constrained. Possible options are "cases" (default), "items"
and "none". When anchor parameter are specified in anchor, constraints will be set
to "none" automatically.
}
  \item{std.err}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string specifying which
type of standard error should be estimated. Possible options are "full", "quick"
(default) and "none". See ConQuest manual pp.167 for details on standard error estimation.
}
  \item{distribution}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string indicating the
a priori trait distribution. Possible options are "normal" (default) and "discrete".
See ConQuest manual pp.167 for details on population distributions.
}
  \item{method}{
%%     ~~Describe \code{dif.term} here~~
A character string indicating which method should be used for analysis. Possible 
options are "gauss" (default), "quadrature" and "montecarlo". See ConQuest manual 
pp.225 for details on these methods. When using \code{software = "tam"}, "gauss" and 
"quadrature" essentially leads to calling TAM with \code{QMC = FALSE}, "montecarlo"
leads to calling TAM with \code{QMC = TRUE}.
}
  \item{n.iterations}{
%%     ~~Describe \code{dif.term} here~~
An integer value specifying the maximum number of iterations for which estimation
will proceed without improvement in the deviance. 
}
  \item{nodes}{
%%     ~~Describe \code{dif.term} here~~
An integer value specifying the number of nodes to be used in the analysis. The
default value is 15. When using \code{software = "tam"}, the value specified here
leads to calling TAM with \code{nodes = 15} AND \code{snodes = 0} if "gauss" or 
"quadrature" was used in the \code{method} argument. If "montecarlo" was used in 
the \code{method} argument, the value specified here leads to calling TAM with 
\code{snodes = 15} AND \code{nodes = 0}.
}
  \item{p.nodes}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. An integer value specifying the
number of nodes that are used in the approximation of the posterior distributions,
which are used in the drawing of plausible values and in the calculation of EAP
estimates. The default value is 2000.
}
  \item{f.nodes}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. An integer value specifying the
number of nodes that are used in the approximation of the posterior distributions
in the calculation of fit statistics. The default value is 2000.
}
  \item{converge}{
%%     ~~Describe \code{dif.term} here~~
An integer value specifiying the convergence criterion for parameter estimates.
The estimation will terminate when the largest change in any parameter estimate
between successive iterations of the EM algorithm is less than converge. The
default value is 0.001.
}
  \item{deviancechange}{
%%     ~~Describe \code{dif.term} here~~
An integer value specifiying the convergence criterion for the deviance. The
estimation will terminate when the change in the deviance between successive
iterations of the EM algorithm is less than deviancechange. The default value
is 0.0001.
}
  \item{equivalence.table}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A character string specifying the
type of equivalence table to print. Possible options are "wle" (default), "mle"
and NULL.
}
  \item{use.letters}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. A logical value indicating whether
item response values should be coded als letters. This option can be used in partial
credit models comprising items with more than 10 categories to avoid response columns
with width 2 in ConQuest.
}
  \item{allowAllScoresEverywhere}{
%%     ~~Describe \code{dif.term} here~~
Actually, I don't know what this means.
}
  \item{guessMat}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "tam"} for 3PL models. A named data frame
with two columns indicating for which items a common guessing parameter should
be estimated. The first column contains the names of all items in the analysis
and should be named "item". The second column is numerical (integer values
recommended) and allocates the items to groups. For each group of items, a
separate guessing parameter is estimated. If the value in the second columns equals
zero, the guessing parameter is fixed to zero.
}
  \item{est.slopegroups}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "tam"} for 2PL models. Optionally, a named data frame
with two columns indicating for which items a common discrimination parameter should
be estimated. The first column contains the names of all items in the analysis
and should be named "item". The second column is numerical (integer values
recommended) and allocates the items to groups. For each group of items, a
separate discrimination parameter is estimated. Without specifying \code{est.slopegroups}, 
a discrimination parameter for each item is estimated.
}
  \item{progress}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "tam"}. Print estimation progress messages on console?
}
  \item{increment.factor}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "tam"}. Should only be varied if the model does not converge.
See help page of \code{tam.mml} for further details.
}
  \item{fac.oldxsi}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "tam"}. Should only be varied if the model does not converge.
See help page of \code{tam.mml} for further details.
}
  \item{export}{
%%     ~~Describe \code{dif.term} here~~
Applies only if \code{software = "conquest"}. Specifies which additional files should be written
on hard disk.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list which contains information about the desired estimation. The list is intended for 
further processing via \code{runModel}. Structure of the list varies depending on 
whether multiple models were called using \code{splitModels} or not. If \code{splitModels} was called, 
the number of elements in the list equals the number of models defined via \code{splitModels}. Each
element in the list is a list with various elements: 
  \item{software}{
Character string of the software which is intended to use for the further estimation, i.e. "conquest" or "tam"
}
  \item{qMatrix}{
The Q matrix allocating items to dimensions.
}
  \item{all.Names}{
Named list of all relevant variables of the data set.
}
  \item{dir}{
Character string of the directory the results are to be saved. 
}
  \item{analysis.name}{
Character string of the analysis' name. 
}
  \item{deskRes}{
Data frame with descriptives (e.g., p values) of the test items. 
}
  \item{discrim}{
Data frame with item discrimination values. 
}
  \item{perNA}{
The person identifiers of examinees which are excluded from the analysis due to solely missing values.
}
  \item{per0}{
The person identifiers of examinees which are excluded from the analysis due to solely false responses.
(applies only if \code{remove.failues} was set to be TRUE)
}
  \item{perExHG}{
The person identifiers of examinees which are excluded from the analysis due to missing values on explicit variables.
}
  \item{itemsExcluded}{
Character string of items which were excluded, for example due to zero variance or solely missing values. 
}
If software == "conquest", the output additionally includes the following elements:
  \item{input}{
Character string of the path with Conquest input (cqc) file. 
}
  \item{conquest.folder}{
Character string of the path of the conquest executable file. 
}
  \item{model.name}{
Character string of the model name. 
}
If software == "tam", the output additionally includes the following elements:
  \item{anchor}{
Optional: data frame of anchor parameters (if anchor parameters were defined).
}
  \item{daten}{
The prepared data for TAM analysis. 
}
  \item{irtmodel}{
Character string of the used IRT model. 
}
  \item{est.slopegroups}{
Applies for 2pl modeling. Information about which items share a common slope parameter. 
}
  \item{guessMat}{
Applies for 3pl modeling. Information about which items share a common guessing parameter. 
}
  \item{control}{
List of control parameters for TAM estimation.
}
  \item{n.plausible}{
Desired number of plausible values.
}
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
# load example data
# (these are simulated achievement test data)
data(sciences)
# first reshape the data set into wide format
datW <- reshape2::dcast(sciences, id+grade+sex~variable, value.var="value")
# second, create the q matrix from the long format data frame
qMat <- sciences[ which( sciences[,"subject"] == "biology") ,c("variable","domain")]
qMat <- qMat[!duplicated(qMat[,1]),]
qMat <- data.frame ( qMat[,1,drop=FALSE], knowledge  = as.numeric(qMat[,"domain"] == "knowledge"),
        procedural = as.numeric(qMat[,"domain"] == "procedural"))
\dontrun{
# Example 1: define and run a unidimensional Rasch model with all variables in dataset
# using "Conquest". Note: if software="conquest", the path of the windows executable
# ConQuest console must be specified by setting conquest.folder = "<path_to_your_conquest.exe>"
# defining the model: specifying q matrix is not necessary
mod1 <- defineModel(dat=datW, items= -c(1:3), id="id", analysis.name = "unidim",
        conquest.folder = "N:/console_Feb2007.exe",  dir = "N:/test")
# run the model
run1 <- runModel(mod1)
# get the results
res1 <- getResults(run1)
#
# Example 2: running a multidimensional Rasch model on a subset of items with latent
# regression (sex). Use item parameter from the first model as anchor parameters
# use only biology items from both domains (procedural/knowledge)
# read in anchor parameters from showfile
aPar <- itemFromRes ( res1 ) 
aPar <- aPar[,c("item", "est")]
# defining the model: specifying q matrix now is necessary. please acknowledge the notes
# printed on console!
mod2 <- defineModel(dat=datW, items= qMat[,1], id="id", analysis.name = "twodim",
        qMatrix = qMat, HG.var = "sex", anchor = aPar, n.plausible = 20,
        conquest.folder = "N:/console_Feb2007.exe",  dir = "N:/test")
# run the model
run2 <- runModel(mod2)
# get the results
res2 <- getResults(run2)
#
# Example 3: the same model in TAM
# first use unidimensional calibration (model 1) to get item parameters
mod1T<- defineModel(dat=datW, items= -c(1:3), id="id", software = "tam")
run1T<- runModel(mod1T)
res1T<- getResults(run1T)
# anchor parameters from TAM
aParT<- itemFromRes(res1T)
aParT<- aParT[,c("item", "est")]
# estimate model 2 with latent regression and anchored parameters in TAM
mod2T<- defineModel(dat=datW, items= qMat[,1], id="id", qMatrix = qMat,
        HG.var = "sex", anchor = aParT, software = "tam")
# run the model
run2T<- runModel(mod2T)
class(run2T)
# the class of 'run2T' corresponds to the class defined by the TAM package; all
# functions of the TAM package intended for further processing (e.g. drawing
# plausible values, plotting deviance change etc.) work, for example:
wle  <- tam.wle(run2T)
# Now, the model result are collected in a single data frame
res2T<- getResults(run2T)
#
# Example 4: define und run multiple models defined by 'splitModels'
# define person grouping
pers  <- data.frame ( idstud = datW[,"id"] , group1 = datW[,"sex"], group2 = datW[,"grade"], stringsAsFactors = FALSE )
# define 18 models
l1    <- splitModels ( qMatrix = qMat, person.groups = pers)
# run 'defineModel' for each model in 'l1'
modMul<- defineModel(dat = datW, items = qMat[,1], id = "id", check.for.linking = TRUE, splittedModels = l1, software = "tam")
# run all models 
runMul<- runModel(modMul)
# get results
resMul<- getResults(runMul)
#
# Example 5: define und run multiple models according to different domains (item groups)
# and further linking/equating
# define item groups via the Q matrix
l2    <- splitModels ( qMatrix = qMat)
# define 2 models
mods  <- defineModel(dat = datW, id = "id", check.for.linking = TRUE, splittedModels = l2, software = "tam")
# run 2 models 
runs  <- runModel(mods)
# get the results 
ress  <- getResults(runs)
# only for illustration, we create arbitrary parameters for anchoring
prmNrm<- itemFromRes(ress)[ sample ( 1:56, 15,F) ,c("item", "est")]
prmNrm[,"est"] <- prmNrm[,"est"] - 0.6 + rnorm ( 15, 0, 0.6)
# anchoring without exclusion of linking DIF items
anch  <- equat1pl ( results = ress, prmNorm = prmNrm, excludeLinkingDif = FALSE, 
         difBound = 0.6)
# anchoring with exclusion of linking DIF items
anch2 <- equat1pl ( results = ress, prmNorm = prmNrm, excludeLinkingDif = TRUE, 
         difBound = 0.6, iterativ = FALSE)
# anchoring with iterative exclusion of linking DIF items
anch3 <- equat1pl ( results = ress, prmNorm = prmNrm, excludeLinkingDif = TRUE, 
         difBound = 0.6, iterativ = TRUE)
# transformation to the Bista metric
# first we arbitrarily define mean and standard deviation of the reference 
# population according to both dimensions (defined in the Q matrix): 
# procedural and knowledge
refPop<- data.frame ( domain = c("procedural", "knowledge"), m = c(0.122, -0.047), 
         sd = c(0.899, 1.121))
# second, we specify a list with cut scores. Values must be in ascending order.
# Labels of the competence stages are optional. If no values are specified, 
# the will be defaulted to 1, 2, 3 ... etc.
# Note: if labels are specified, there must be one label more than cut scores. 
# (i.e. 4 cut scores --> 5 labels, etc.)
cuts  <- list ( procedural = list ( values = c(380, 420, 500, 560)) , 
         knowledge = list ( values = 400+0:2*100, labels = c("A1", "A2", "B1", "B2")))
# transformation
dfr   <- transformToBista ( equatingList = anch3, refPop = refPop, cuts=cuts )          
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
