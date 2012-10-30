\name{automateModels}
\alias{automateModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
automateModels
}
\description{
specify and run several ConQuest models
}
\usage{
automateModels( dat, id = NULL, context.vars = NULL, items = NULL, 
item.grouping = NULL, select.item.group = NULL, person.grouping.vars = NULL, 
person.grouping.vars.include.all = FALSE, person.grouping = NULL, 
select.person.group = NULL, checkLink = FALSE, additional.item.props = NULL,
folder, overwrite.folder = TRUE, analyse.name.prefix = NULL,
analyse.name = NULL, analyse.name.elements = NULL, data.name = NULL,
m.model = NULL, software = NULL, dif = NULL, weight = NULL, anchor = NULL,
regression = NULL, adjust.for.regression = TRUE, q3 = FALSE,
q3.p.est = c ( "wle" , "pv" , "eap" ), icc = FALSE, missing.rule = NULL,
cross = NULL, subfolder.order = NULL, subfolder.mode = NULL,
allNAdelete = TRUE, additionalSubFolder = NULL, run.mode = NULL,
n.batches = NULL, run.timeout = 1440, run.status.refresh = 0.2,
cores = NULL, email = NULL, smtpServer = NULL, write.txt.dataset = FALSE,
write.xls.results = TRUE, delete.folder.countdown = 5,
conquestParameters = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
data.frame containing all variables
type of variables ("id" , "context.vars" or "items") must be set using options \code{id}, \code{context.vars}, \code{items}
}
  \item{id}{
%%     ~~Describe \code{id} here~~
name or column number of 'id' variable in \code{dat}
}
  \item{context.vars}{
%%     ~~Describe \code{context.vars} here~~
names or column numbers of 'context' variables ( e.g. sex, school , ... ) in \code{dat}
}
  \item{items}{
%%     ~~Describe \code{items} here~~
names or column numbers of 'item' variables in \code{dat}
if omitted, all variables that are not classified as 'id' or 'context' variables are treated as 'items'
}
  \item{item.grouping}{
%%     ~~Describe \code{item.grouping} here~~
data.frame with grouping information of items, first column must be 'item' which includes item names, further columns contain scale definitions, 0 indicates that the respective item is NOT part of the scale, 1 indicates that this item is part of the scale, colnames of columns are the names of the scales
}
  \item{select.item.group}{
%%     ~~Describe \code{select.item.group} here~~
character vector of scale names chosen for analysis
}
  \item{person.grouping.vars}{
%%     ~~Describe \code{person.grouping.vars} here~~
character vector of 'context' variables in dataset which are used to automatically generate 'person.grouping', each category is transformed into the 'person.grouping' format
}
  \item{person.grouping.vars.include.all}{
%%     ~~Describe \code{person.grouping.vars.include.all} here~~
logical vector (along person.grouping.vars), indicates whether to generate a variable 'all' for the specific variable
}
  \item{person.grouping}{
%%     ~~Describe \code{person.grouping} here~~
data.frame with grouping information of persons, first column must be the name of 'id' (e.g. idstud), further columns contain group definitions, 0 indicates that the respective person is NOT part of the group, 1 indicates that this person is part of the group, colnames of columns are the names of the groups
}
  \item{select.person.group}{
%%     ~~Describe \code{select.person.group} here~~
character vector of group names chosen for analysis
}
  \item{checkLink}{
		logical: If \code{TRUE}, items in dataset are checked for being connected with each other via design (function \code{\link{checkLink}} is called)
		23.02.2012: not yet implemented
}

  \item{additional.item.props}{
%%     ~~Describe \code{additional.item.props} here~~
data.frame of additional item information to be merged to model results, first column must be 'item' and contain item names
}
  \item{folder}{
%%     ~~Describe \code{folder} here~~
folder to write output into
}
  \item{overwrite.folder}{
%%     ~~Describe \code{overwrite.folder} here~~
logical, if TRUE (default), folder is completely emptied
}
  \item{analyse.name.prefix}{
%%     ~~Describe \code{analyse.name.prefix} here~~
prefix (e.g. "pilotStudy") to be attached to all analyses names
}
  \item{analyse.name}{
%%     ~~Describe \code{analyse.name} here~~
analyses names are usually automatically set, if you want to set them manually use this option
}
  \item{analyse.name.elements}{
%%     ~~Describe \code{analyse.name.elements} here~~
analyses names are set automatically using these elements: c ( "scale" , "group" , "dif" , "regression" , "anchor" ), use this option to change composition and order of the analyses names generation
}
  \item{data.name}{
%%     ~~Describe \code{data.name} here~~
optional: character string specifying name of dataset if intend to differ from
name specified by jobName. When dataName == NULL, dataset is named [jobName].dat
}
  \item{m.model}{
%%     ~~Describe \code{m.model} here~~
measurement model, "1pl" (default), "2pl", "3pl", "4pl"
}
  \item{software}{
%%     ~~Describe \code{software} here~~
"conquest" (default)
no other software implemented yet
}
  \item{dif}{
%%     ~~Describe \code{dif} here~~
variable that is used for differential item functioning
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
case weight variable
}
  \item{anchor}{
%%     ~~Describe \code{anchor} here~~
data.frame with anchor information
}
  \item{regression}{
%%     ~~Describe \code{regression} here~~
variable(s) that is/are used 
}
  \item{adjust.for.regression}{
%%     ~~Describe \code{regression} here~~
center plausible values and items on grand mean
}
  \item{q3}{
%%     ~~Describe \code{missing.rule} here~~
Logical: If \code{TRUE}, Yen's Q3 statistic is computed.
}
  \item{q3.p.est}{
%%     ~~Describe \code{missing.rule} here~~
person estimates that are used in q3 calculation, default: wle
}
  \item{icc}{
%%     ~~Describe \code{missing.rule} here~~
Logical: If \code{TRUE}, pdfs of item icc are generated.
}
  \item{missing.rule}{
%%     ~~Describe \code{missing.rule} here~~
definition how to recode distinct missings in dataset
}
  \item{cross}{
%%     ~~Describe \code{cross} here~~
scales in 'item.grouping' and groups in 'person.grouping' can be crossed to define distinct analyses
"all": scales and groups are crossed
"item.groups", scales are separately (unidimensional) run (instead of one multidimensional model)
"person.groups", person groups are separately (single group) run (instead of one multigroup model)
}
  \item{subfolder.order}{
%%     ~~Describe \code{subfolder.order} here~~
subfolders are automatically generated in this order
c ( "i.model" , "p.model" , "m.model" , "software" , "dif" , "regression" , "anchor" )
}
  \item{subfolder.mode}{
%%     ~~Describe \code{subfolder.mode} here~~
"none": no subfolders are created
"full": complete subfolders are created according to 'subfolder.order'
"intelligent" (default): meaningful subfolders are created
}
  \item{allNAdelete}{
if \code{TRUE} all cases with complete missings on \code{items} are removed,
if \code{FALSE} these cases are not deleted
Note: this is a global option, that is set for all modelss
}
  \item{additionalSubFolder}{
%%     ~~Describe \code{additionalSubFolder} here~~
specification for 'data' and 'out' subfolder (constant over all analyses)
}
  \item{run.mode}{
%%     ~~Describe \code{run.mode} here~~
"serial": serial runs on local machine. see option 'cores' to specify number of parallel runs
"parallel": batch files must be started manually (e.g. on several machines). see option 'n.batches' to specify number batch files
}
  \item{n.batches}{
%%     ~~Describe \code{n.batches} here~~
if \code{run.mode}="parallel", number of batch files that are created, batch files contain one or more analyses
}
  \item{run.timeout}{
%%     ~~Describe \code{run.timeout} here~~
minutes to wait for analyses to finish, default: 1440 (24h)
}
  \item{run.status.refresh}{
%%     ~~Describe \code{run.status.refresh} here~~
time for console refresh of model run status, default: 0.2 (12sec)
}
  \item{cores}{
if \code{run.mode}="serial" and multiple analyses are run, number of cores to use.
if \code{cores=NULL} (default) all cores are used
if number of cores specified is greater than number of actual cores, number of actual cores is used
}
  \item{email}{
%%     ~~Describe \code{email} here~~
set email address to receive an email when analyses are finished or time's up
}
  \item{smtpServer}{
%%     ~~Describe \code{smtpServer} here~~
smtpServer for sending emails, default: "mailhost.cms.hu-berlin.de"
}
  \item{write.txt.dataset}{
%%     ~~Describe \code{write.txt.dataset} here~~
write out datasets as ascii, default: FALSE
}
  \item{write.xls.results}{
if \code{TRUE} (default) results are written to Excel files
}
  \item{delete.folder.countdown}{
%%     ~~Describe \code{delete.folder.countdown} here~~
countdown for deletion of 'folder', default: 5 (seconds)
}
  \item{conquestParameters}{
							Set ConQuest parameters as a named list.

							Available option are:

							"compute.fit", "model.statement", "pathConquest", "method", "std.err",
							"distribution", "n.plausible", "set.constraints", "nodes", "p.nodes", "f.nodes",
							"n.iterations", "converge", "deviancechange", "equivalence.table", "use.letters",
							"checkLink", "export"

							See \link{automateConquestModel} documentation for details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
To run several models list parameters as corresponding lists
Explicitly list NULL if parameter should not be set or be defaulted
See examples
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns results in specific format
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Martin Hecht, Karoline Sachse, Sebastian Weirich, Christiane Penk, Malte Jansen, Sebastian Wurster
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{
# 'folder' must be specified, WARNING: this folder is deleted by automateModels!!!
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
data ( science1 )
#
### Example 1: running a unidimensional Rasch model with all variables in dataset 'science1'
# all variables in 'science1' must be classified as either 'id', 'context.vars' or 'items'
# 'items' may be omitted, then it is defaulted to variables that are not 'id' or 'context.vars'
ex1 <- automateModels ( dat = science1, id = "id", context.vars = science1.context.vars,
				 folder = "C:/temp/automateModels/Example1" )
#
# item and person parameters can be obtained using \link{get.item.par} and \link{get.person.par}
item.par <- get.item.par ( ex1 )
person.par <- get.person.par ( ex1 )
#
### Example 2: running a multidimensional Rasch model
# option 'item.grouping' specifies dimensions and mapping of items to dimensions
# 'item.grouping' is a data.frame with item names in first column ('item')
#		and dimensions in further columns, mapping of items to dimension is
#		indicated by 0 (item loads not on dimension) or 1 (item loads on dimension)
#		(have a look at the example item.grouping 'science1.scales')
# since 6 dimensions are specified in 'science1.scales' a 6-dimensional Rasch model is run
# this example runs some time + convergence is suboptimal
ex2 <- automateModels ( item.grouping = science1.scales, dat = science1, id = "id",
				 context.vars = science1.context.vars, folder = "C:/temp/automateModels/Example2" )
#
### Example 3: running several unidimensional Rasch models in a row
# we use item.grouping = 'science1.scales' with 6 dimensions
# instead of running one 6-dimensional model we will run 6 unidimensional models
# by specifying cross = "item.groups"
ex3 <- automateModels ( cross = "item.groups", item.grouping = science1.scales, dat = science1,
				 id = "id", context.vars = science1.context.vars,
				 folder = "C:/temp/automateModels/Example3" )
#
### Example 4: running 15 2-dimensional models (every scale combined with every other)
# Option 'select.item.group' is used to specify various combinations of dimensions
# it is a list of 15 character vectors that incorporate scale names (from 'item.grouping' data)
ex4 <- automateModels ( select.item.group =
				 list ( c("BioKno","BioPro"),c("BioKno","CheKno"),c("BioKno","ChePro"),
				 c("BioKno","PhyKno"),c("BioKno","PhyPro"),c("BioPro","CheKno"),c("BioPro","ChePro"),
				 c("BioPro","PhyKno"),c("BioPro","PhyPro"),c("CheKno","ChePro"),c("CheKno","PhyKno"),
				 c("CheKno","PhyPro"),c("ChePro","PhyKno"),c("ChePro","PhyPro"),c("PhyKno","PhyPro") ),
				 item.grouping = science1.scales, dat = science1,
				 id = "id", context.vars = science1.context.vars,
				 folder = "C:/temp/automateModels/Example4" )
#
### Example 5: running Rasch models for several person subgroups
# we specify person.grouping.vars = "grade" to run seperate analysis for every value of grade (9/10)
# to include the complete analysis (all grades) 'person.grouping.vars.include.all' is set to TRUE
# to trigger separate person subgroup analyses 'cross' must be set to "person.groups"
# with this specification 3 models are run: all grades (9 and 10), grade 9, grade 10
ex5 <- automateModels ( person.grouping.vars = "grade",
				 person.grouping.vars.include.all = TRUE,
				 cross = "person.groups",
				 dat = science1, id = "id", context.vars = science1.context.vars,
				 folder = "C:/temp/automateModels/Example5" )				 
#
### Example 6: running Rasch models for several person subgroups and scales
# cross = "all" triggers unidimensional models with the combination of scales and person subgroups
# in this example every scale is run with grade 9 and with grade 10 separately (=12 models)
ex6 <- automateModels ( person.grouping.vars = "grade",
				 item.grouping = science1.scales,
				 cross = "all",
				 dat = science1, id = "id", context.vars = science1.context.vars,
				 folder = "C:/temp/automateModels/Example6" )				 
				 
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
