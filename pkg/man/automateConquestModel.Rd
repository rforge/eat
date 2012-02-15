\name{automateConquestModel}
\alias{automateConquestModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
automateConquestModel
}
\description{
Wrapper function which calls several functions to build and write Conquest input files.
}
\usage{
automateConquestModel(dataset, ID, regression=NULL, DIF=NULL, group.var=NULL, 
weight=NULL, testitems, na=list(items=NULL, DIF=NULL, HG=NULL, group=NULL, 
weight=NULL), person.grouping=NULL, item.grouping=NULL, model.statement="item", 
m.model="1pl", Title = NULL, jobName, jobFolder, subFolder=list(), dataName=NULL, 
anchor=NULL, pathConquest, method=NULL,std.err=NULL,distribution=NULL, 
n.plausible=NULL, set.constraints=NULL, nodes=NULL, p.nodes=NULL, f.nodes=NULL, 
n.iterations=NULL, converge=NULL, deviancechange=NULL, name.unidim=NULL, 
equivalence.table="wle",use.letters=FALSE, check.for.linking=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
data.frame containing all variables necessary for analysis
}
  \item{ID}{
%%     ~~Describe \code{id} here~~
name or column number of 'id' variable
}
  \item{regression}{
%%     ~~Describe \code{context.vars} here~~
character vector with names or integer vector with column numbers of one or more 'context' variables ( e.g. sex, school , ... )
}
  \item{DIF}{
character string with name or scalar with column number of only one variable denominating groups for which analysis of differential item functioning is to be done ( e.g. sex, class , ... )
}
  \item{group.var}{
character vector with names or integer vector with column numbers of one or more 'group' variables ( e.g. sex, school , ... )
}
  \item{weight}{
character string with name or scalar with column number of only one 'weighting' variable 
}
  \item{testitems}{
character vector with names or integer vector with column numbers of 'item' variables ( e.g. sex, school , ... )
}
  \item{na}{
List of numerical vector including numbers to be considered as 'sysmis'. Specific missing codes can be defined for each type of variable, e.g. testitems, DIF variables, ... 
}

  \item{item.grouping}{
%%     ~~Describe \code{item.grouping} here~~
data.frame with grouping information of items, first column must be 'item' which includes item names, further columns contain scale definitions, 0 indicates that the respective item is NOT part of the scale, 1 indicates that this item is part of the scale, colnames of columns are the names of the scales
}
  \item{person.grouping}{
%%     ~~Describe \code{person.grouping} here~~
data.frame with grouping information of persons, first column must be the name of 'id' (e.g. idstud), further columns contain group definitions, 0 indicates that the respective person is NOT part of the group, 1 indicates that this person is part of the group, colnames of columns are the names of the groups
}
  \item{model.statement}{
character string which appears in Conquest Syntax as model statement. Set to \code{item} by default. When DIF variable is specified, statement is set 
to \code{item - [name of DIF variable] + item*[name of DIF variable]} by default. However, user's specification of \code{model.statement} overwrites default in each case.
}
  \item{m.model}{
character string specifying the IRT model used for analysis. At the time, only \code{"1PL"} is available.
}
  \item{Title}{
optional: character string with title of analysis which appears in Conquest Syntax. If no title is specified, informations about computer and user name and R version 
are printed in Conquest Syntax.
}
  \item{jobName}{
character string specifying name of analysis. All Conquest input and output files will named by \code{jobName} and their corresponding suffixes. 
}
  \item{jobFolder}{
character string specifying the folder where all analysis files will be generated, for example \code{"C:/programme/analysis"}
}
  \item{subFolder}{
optional: List of character strings specifying maximal 2 optional subfolders relative to \code{jobFolder} for the data and the output files. 
Character strings must be named with \code{data} and \code{out}, for example \code{subFolder=list(data="../../dataset/analysis1", out="../../output/analysis1")}
Double dots \code{..} indicates to move one level above in folder structure. For example, if \code{jobFolder} is \code{"C:/programme/analysis"} and \code{subFolder} 
is \code{list(data="../dataset/analysis1", out="../../output/analysis1")}, dataset is written to \code{"C:/programme/dataset/analysis1"} and output is 
written to \code{"C:/output/analysis1"}. When \code{subFolder$data == NULL}, dataset is written to the folder specified by \code{jobFolder}. Same is true for 
\code{subFolder$out == NULL}.
}
  \item{dataName}{
optional: character string specifying name of dataset if intend to differ from name specified by \code{jobName}. When \code{dataName == NULL}, 
dataset is named \code{[jobName].dat} 
}
  \item{anchor}{
optional: data frame with anchor parameters. First column of \code{anchor} refers to item name, second columns refers to parameter used as reference for anchoring.
}
  \item{pathConquest}{
 character string with path and name of Conquest console, for example \code{"c:/programme/conquest/console_Feb2007.exe"}
 }
  \item{method}{
optional: character string with method for analysis. Possible options are \code{"gauss"} (default), \code{"quadrature"}, \code{"montecarlo"}.
See Conquest manual, pp.225.
}
  \item{std.err}{
optional: character string specifying types of standard errors to be estimated. Possible options are \code{"full"}, \code{"quick"} (default), \code{"none"}.
See Conquest manual, pp.167.
}
  \item{distribution}{
optional: character string with a priori trait distribution. Possible options are \code{"normal"} (default), \code{"discrete"}.
See Conquest manual, pp.167.
}
  \item{n.plausible}{
optional: integer scalar specifying numbers of plausible values to draw. Default is 5.
}
  \item{set.constraints}{
optional: character string specifying the constraints of the scale. Possible options are \code{"cases"} (default), \code{"items"}, \code{"none"}. When anchor parameter 
are specified in \code{anchor}, constraints will set to \code{"none"} in each case. 
}
  \item{nodes}{
optional: integer scalar specifying numbers of nodes in analysis. Default is 15.
}
  \item{p.nodes}{
optional: integer scalar specifying numbers of p nodes in analysis. Sets the number of nodes that are used in the approximation of the posterior distributions, which are
used in the drawing of plausible values and in the calculation of EAP estimates. The default is 2000.
}
  \item{f.nodes}{
optional: integer scalar specifying numbers of f nodes in analysis. Sets the number of nodes that are used in the approximation of the posterior distributions in the
calculation of fit statistics. The default is 2000.
}
  \item{n.iterations}{
optional integer scalar. Sets the maximum number of iterations for which estimation will proceed without improvement in the
deviance. The minimum value permitted is 5. The default value is 20.
}
  \item{converge}{
optional scalar. Instructs estimation to terminate when the largest change in any parameter estimate between successive
iterations of the EM algorithm is less than \code{converge}. The default value is 0.0001.
}
  \item{deviancechange}{
optional scalar. Instructs estimation to terminate when the change in the deviance between successive iterations of
the EM algorithm is less than \code{deviancechange}. The default value is 0.0001.
}
  \item{name.unidim}{
optional: character string with name of one latent dimension, if not specified in labels. 
}
  \item{equivalence.table}{
optional: character string specifying type of equivalence table to print. Possible options are \code{"wle"} (default), \code{"mle"} or \code{NULL}. 
}
  \item{use.letters}{
logical: Should values coded als letters? May be relevant only in partial credit models comprising items with more than 10 categories to avoid 
columns with width 2 in Conquest.  
}
  \item{check.for.linking}{
logical: If \code{TRUE}, items in dataset are checked for being connected with each other via design. 
}
 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
No results are returned to console. Input files and batch string are written to disk in specified folder(s). 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich, Karoline Sachse, Martin Hecht
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
