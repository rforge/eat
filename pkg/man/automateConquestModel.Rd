\name{automateConquestModel}
\alias{automateConquestModel}
\title{
automateConquestModel: Write all requirend Input for a single ConQuest Run.
}
\description{
\code{automateConquestModel} facilitates data analysis using the software ConQuest. It automatically writes ConQuest syntax, label, anchor and data files
for a single model specified by several arguments in R. Moreover, a batch file is created to start the analysis. For automatically specifying and running 
several models in a row, see \code{automateModels}.
}
\usage{
automateConquestModel(dat, ID, regression=NULL, DIF=NULL, group.var=NULL, 
weight=NULL, items, na=list(items=NULL, DIF=NULL, HG=NULL, group=NULL, 
weight=NULL), person.grouping=NULL, item.grouping=NULL, compute.fit = TRUE, model.statement="item",
m.model="1pl", Title = NULL, jobName, jobFolder, subFolder=list(), dataName=NULL, 
anchor=NULL, pathConquest, method=NULL, std.err=NULL ,distribution=NULL, 
n.plausible=NULL, set.constraints=NULL, nodes=NULL, p.nodes=NULL, f.nodes=NULL, 
n.iterations=NULL, converge=NULL, deviancechange=NULL, name.unidim=NULL, 	allowAllScoresEverywhere = TRUE,
equivalence.table="wle", use.letters=FALSE, checkLink=FALSE, verbose=TRUE,
export = list(logfile = TRUE, systemfile = TRUE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = TRUE))
}

\arguments{
  \item{dat}{
    A data frame containing all variables necessary for analysis.
}
  \item{ID}{
    Name or column number of the identifier (ID) variable.
}
  \item{regression}{
    Names or column numbers of one or more context variables (e.g., sex, school). These variables will be used for latent regression in ConQuest.
}
  \item{DIF}{
	Name or column number of one grouping variable for which differential item functioning analysis is to be done.
}
  \item{group.var}{
	Names or column numbers of one or more grouping variables. Descriptive statistics for WLEs and Plausible Values will be computed separately for each group in ConQuest.
}
  \item{weight}{
	Name or column number of one weighting variable. 
}
  \item{items}{
	Names or column numbers of variables with item responses.
}
  \item{na}{
	 A named list of numerical vectors indicating values to be considered as missing. Specific missing codes can be defined for each type of variable. 
	% hier noch hinschreiben, welche Variablentypen es gibt.
}

  \item{item.grouping}{
	A named data frame indicating how items should be grouped to dimensions. The first column contains the names of all items and must be named \code{item}. The other columns contain dimension definitions and must be named with the respective dimension names. A value of 1 indicates that an item loads on this dimension, a value of 0 indicates that the respective item does not load on this dimension.
}
  \item{person.grouping}{
	A named data frame indicating which persons should be grouped. The first column contains the identifier variable and must have the same name as the respective column in \code{dat}. The other columns contain grouping definitions and must be named with the respective group names. A value of 1 indicates that a person belongs to this group, a value of 0 indicates that the respective person does not belong to this group.
}
  \item{compute.fit}{
	Logical: Should fit statistics computed in ConQuest analysis?
}
  \item{model.statement}{
	A character string with the model statement to use in the ConQuest syntax. If \code{model.statement == NULL}, the model statement in the ConQuest syntax is set to \code{item} by default. When a DIF variable is specified, the model statement is set to \code{item - [name of DIF variable] + item*[name of DIF variable]} by default. 
}
  \item{m.model}{
	A character string specifying the IRT model used for analysis. At the time, only \code{"1PL"} is available.
}
  \item{Title}{
A character string with the analysis title for the ConQuest syntax. If \code{Title == NULL}, informations about computer and user name and R version are used as title.
}
  \item{jobName}{
A character string specifying the analysis name. All Conquest input and output files will named \code{jobName} with their corresponding extensions. 
}
  \item{jobFolder}{
 A character string specifying an already existing folder where all analysis files will be written to, for example \code{"C:/programme/analysis"}
}
  \item{subFolder}{
A named list of character strings specifying a maximum of two folders relative to \code{jobFolder} for data and output files. 
Character strings must be named \code{data} and \code{out}, for example \code{subFolder=list(data="../../dataset/analysis1", out="../../output/analysis1")}. If \code{subFolder$data == NULL}, the dataset is written to the folder specified by \code{jobFolder}. The same is true for \code{subFolder$out == NULL}.
}
  \item{dataName}{
A character string specifying the dataset name if it is intended to be different from the name specified by \code{jobName}. If \code{dataName == NULL}, the dataset is named \code{[jobName].dat} 
}
  \item{anchor}{
	A named data frame with anchor parameters. The first column contains the names of all anchor items and must be named \code{item}. The second column contains anchor parameters. 
	Anchor items can be a subset of the items in the dataset and vice versa. 
}
  \item{pathConquest}{
 A character string with path and name of the ConQuest console, for example \code{"c:/programme/conquest/console_Feb2007.exe"}
 if \code{NULL} the newest executable in file.path(.Library,"eat/winexe/conquest") is used
 }
  \item{method}{
A character string indicating which method should be used for analysis. Possible options are \code{"gauss"} (default), \code{"quadrature"} and \code{"montecarlo"}.
See ConQuest manual pp.225 for details on these methods.
}
  \item{std.err}{
  A character string specifying which type of standard error should be estimated. Possible options are \code{"full"}, \code{"quick"} (default) and \code{"none"}.
See ConQuest manual pp.167 for details on standard error estimation.
}
  \item{distribution}{
A character string indicating the  a priori trait distribution. Possible options are \code{"normal"} (default) and \code{"discrete"}.
See ConQuest manual pp.167 for details on population distributions.
}
  \item{n.plausible}{
An integer value specifying the number of plausible values to draw. The default value is 5.
}
  \item{set.constraints}{
A character string specifying how the scale should be constrained. Possible options are \code{"cases"} (default), \code{"items"} and \code{"none"}. When anchor parameter are specified in \code{anchor}, constraints will be set to \code{"none"}. 
}
  \item{nodes}{
An integer value specifying the number of nodes to be used in the analysis. The default value is 15.
}
  \item{p.nodes}{
An integer value specifying the number of nodes that are used in the approximation of the posterior distributions, which are
used in the drawing of plausible values and in the calculation of EAP estimates. The default value is 2000.
}
  \item{f.nodes}{
An integer value specifying the number of nodes that are used in the approximation of the posterior distributions in the
calculation of fit statistics. The default value is 2000.
}
  \item{n.iterations}{
An integer value specifying the maximum number of iterations for which estimation will proceed without improvement in the
deviance. The minimum value permitted is 5. The default value is 20.
}
  \item{converge}{
An integer value specifiying the convergence criterion for parameter estimates. The estimation will terminate when the largest change in any parameter estimate between successive iterations of the EM algorithm is less than \code{converge}. The default value is 0.0001.
}
  \item{deviancechange}{
An integer value specifiying the convergence criterion for the deviance. The estimation will terminate when  the change in the deviance between successive iterations of the EM algorithm is less than \code{deviancechange}. The default value is 0.0001.
}
  \item{name.unidim}{
A character string with the name of one latent dimension. Alternatively, the dimension name can be specified using the argument \code{item.grouping}.
}
\item{allowAllScoresEverywhere}{
Logical: Relevant only in multidimensional models for polytomous data. If \code{FALSE}, different codes are allowed to occur in both dimensions, for example one dimension is measured with dichotomous items, i.e. 0/1, and the other dimension is measured with polytomous items, i.e. 1, 2, 3, 4. If \code{TRUE}, common codes for both dimension are used, i.e. 0, 1, 2, 3, 4 for both dimensions. In unidimensional models this argument has no effect.
}
  \item{equivalence.table}{
A character string specifying the type of equivalence table to print. Possible options are \code{"wle"} (default), \code{"mle"} and \code{NULL}. 
}
  \item{use.letters}{
A logical value indicating whether item response values should be coded als letters. This option can be used in partial credit models comprising items with more than 10 categories to avoid response columns with width 2 in ConQuest.  
}
  \item{checkLink}{
A logical value indicating whether the items in dataset are checked for being connected with each other via design. If \code{TRUE}, the function \code{\link{checkLink}} is called.
}
  \item{verbose}{
 A logical value indicating whether messages are printed on the \R console. 
}
  \item{export}{
 A named or unnamed list or vector of logical elements indicating whether Conquest should create logfile, systemfile, history file, covariance file, file of regression coefficients and file of designmatrix.
}

}

\details{
If the folders specified in \code{subFolder} should be parent folders to \code{jobFolder}, they can be specified using double dots \code{..}. For example, if \code{jobFolder} is \code{"C:/programme/analysis"} and \code{subFolder} 
is \code{list(data="../dataset/analysis1", out="../../output/analysis1")}, dataset is written to \code{"C:/programme/dataset/analysis1"} and output is 
written to \code{"C:/output/analysis1"}.

}
\value{
No results are returned to console. Input files and batch string are written to disk in specified folder(s). 
}
\references{
Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest Version 2.0. Generalised Item Response Modeling Software}. Camberwell, Victoria: ACER Press.
}
\author{
Sebastian Weirich, Karoline Sachse, Martin Hecht
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{automateModels}}, \code{\link{checkLink}}
}

\examples{
\dontrun{
#
# if software="conquest" (currently the only and default option) the path of the
# 		windows executable ConQuest console must be specified by setting
#		conquestParameters = list ("pathConquest"="<path_to_your_conquest.exe>")
#				e.g. conquestParameters = list ("pathConquest"=""C:/ConQuest/console.exe"")
# if not explicitely specified it is searched for in 
#		file.path(.Library,"eat/winexe/conquest")
#				e.g. "C:/R/R-2.14.2/library/eat/winexe/conquest"
#		you can put your ConQuest executable there
#
# load example data
# (these are simulated achievement test data)
# Note: all examples corresponding to examples in help file of 'automateModels'
data ( science1 )
#
### Example 1: create input files for a unidimensional Rasch model with all variables in dataset 'science1'
# only variables of 'science1' you want to use for analysis must be classified. In unidimensional Rasch model, 
# 'id' and 'items' have to be specified
# automateConquestModel needs data with collapsed missing
science1.collapsed <- collapseMissings(science1, items = science1.items)
dir.create("C:/temp")
ex1 <- automateConquestModel ( dat = science1.collapsed, ID = "id", items = science1.items, 
				jobFolder = "C:/temp", jobName = "rasch_unidim")
#
### Example 2: create input files for a multidimensional Rasch model with DIF
# option 'item.grouping' specifies dimensions and mapping of items to dimensions
# 'item.grouping' is a data.frame with item names in first column ('item')
#		and dimensions in further columns, mapping of items to dimension is
#		indicated by 0 (item loads not on dimension) or 1 (item loads on dimension)
#		(have a look at the example item.grouping 'science1.scales')
# since 6 dimensions are specified in 'science1.scales' input for a 6-dimensional Rasch model is run
# running this example may take some time + convergence is suboptimal. This is only for illustration. 
# Note: in higher dimensional modes, number of nodes increased to 2^[number of dimensions]. If not explicitly 
# specified by the user, automateModels automatically uses the estimator 'montecarlo', if nodes increased 3500
# Note: As DIF variable(s) have to be numeric in Conquest, factor variables (e.g. "sex" with male/female)
# will be expressed as numeric indicator variables.
science1.collapsed <- collapseMissings(science1, items = science1.items)
dir.create("C:/temp")
ex2 <- automateConquestModel ( item.grouping = science1.scales, dat = science1.collapsed, ID = "id", items = science1.items,
			 DIF = "sex", jobFolder = "C:/temp", jobName = "rasch_multidim" )

### Example 3: create input files for a multidimensional multigroup Rasch model with latent regression
# Note: As regression and group variables have to be numeric, factor variables (e.g. "sex" with male/female)
# will be expressed as numeric indicator variables.
# Moreover, unless estimation method is not specified explicitly, automateConquestModel chooses 'montecarlo' for
# estimation as 'gaussian quadrature' is not available due to latent regression model and 'Bock-Aitken' would use
# 11390625 nodes. Note: As 'montecarlo' needs to fix all item parameter when latent regression is applied, the
# parameters estimated in example 2 are used as anchor parameters here.
# Warning: This example may take a considerable amount of time. It's only for instruction.

science1.collapsed <- collapseMissings(science1, items = science1.items)
# in Conquest, latent regressors have to be numeric
dir.create("C:/temp")
# Run example 2 to gain item parameters
ex2 <- automateConquestModel ( item.grouping = science1.scales, dat = science1.collapsed, ID = "id", items = science1.items,
			 DIF = "sex", jobFolder = "C:/temp", jobName = "rasch_multidim" )
setwd("C:/temp")
system ( "rasch_multidim.bat" ,	 wait = TRUE , show.output.on.console = FALSE , invisible = FALSE )
### get ANCHOR parametern
prm <-  get.shw("rasch_multidim.shw")[[1]][,2:3]
ex3 <- automateConquestModel ( item.grouping = science1.scales, dat = science1.collapsed, ID = "id", items = science1.items,
				 anchor = prm, group = "track", regression = "grade", jobFolder = "C:/temp", jobName = "rasch_multidim_regression_group" )
}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
