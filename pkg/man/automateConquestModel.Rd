\name{automateConquestModel}
\alias{automateConquestModel}
\title{
Specify Models and Write Corresponding Input for ConQuest Software
}
\description{
\code{automateConquestModel} facilitates data analysis using the software ConQuest. It automatically writes ConQuest syntax, label, anchor and data files.
}
\usage{
automateConquestModel(dat, ID, regression=NULL, DIF=NULL, group.var=NULL, 
weight=NULL, items, na=list(items=NULL, DIF=NULL, HG=NULL, group=NULL, 
weight=NULL), person.grouping=NULL, item.grouping=NULL, model.statement="item", 
m.model="1pl", Title = NULL, jobName, jobFolder, subFolder=list(), dataName=NULL, 
anchor=NULL, pathConquest, method=NULL, std.err=NULL ,distribution=NULL, 
n.plausible=NULL, set.constraints=NULL, nodes=NULL, p.nodes=NULL, f.nodes=NULL, 
n.iterations=NULL, converge=NULL, deviancechange=NULL, name.unidim=NULL, 
equivalence.table="wle", use.letters=FALSE, checkLink=FALSE, verbose=TRUE)
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
 A character string specifying the folder where all analysis files will be written to, for example \code{"C:/programme/analysis"}
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
A character string with the name of one latent dimension. Alternatively, the dimension name can be specified using the argument  \code{item.grouping}. 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
