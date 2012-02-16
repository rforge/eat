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
automateModels(dataset, id = NULL, context.vars = NULL, items = NULL, 
item.grouping = NULL, select.item.group = NULL, person.grouping.vars = NULL, 
person.grouping.vars.include.all = FALSE, person.grouping = NULL, 
select.person.group = NULL, additional.item.props = NULL, folder, 
overwrite.folder = TRUE, analyse.name.prefix = NULL, analyse.name = NULL, 
analyse.name.elements = NULL, data.name = NULL, m.model = NULL, software = NULL, 
dif = NULL, weight = NULL, anchor = NULL, regression = NULL, 
adjust.for.regression = FALSE, q3 = FALSE, missing.rule = NULL, cross = NULL, 
subfolder.order = NULL, subfolder.mode = NULL, allNAdelete = TRUE, additionalSubFolder = NULL, 
run.mode = NULL, n.batches = NULL, run.timeout = 1440, run.status.refresh = 0.2, 
all.local.cores = TRUE, email = NULL, smtpServer = NULL, write.txt.dataset = FALSE,
delete.folder.countdown = 5, conquestParameters = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
data.frame containing all variables
type of variables ("id" , "context.vars" or "items") must be set using options \code{id}, \code{context.vars}, \code{items}
}
  \item{id}{
%%     ~~Describe \code{id} here~~
name or column number of 'id' variable in \code{dataset}
}
  \item{context.vars}{
%%     ~~Describe \code{context.vars} here~~
names or column numbers of 'context' variables ( e.g. sex, school , ... ) in \code{dataset}
}
  \item{items}{
%%     ~~Describe \code{items} here~~
names or column numbers of 'item' variables in \code{dataset}
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
%%     ~~Describe \code{adjust.for.regression} here~~
if TRUE item parameters (difficulty) are centered on the mean of the entire sample 
if FALSE (default) item parameters (difficulty) are centered on the mean of the regression reference group 
}
  \item{q3}{
%%     ~~Describe \code{missing.rule} here~~
Logical: If \code{TRUE}, Yen's Q3 statistic is computed.
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
"serial": serial runs on local machine
"parallel": batch files must be started manually (e.g. on several machines)
}
  \item{n.batches}{
%%     ~~Describe \code{n.batches} here~~
number of batch files that are created, batch files contain one or more analyses
}
  \item{run.timeout}{
%%     ~~Describe \code{run.timeout} here~~
minutes to wait for analyses to finish, default: 1440 (24h)
}
  \item{run.status.refresh}{
%%     ~~Describe \code{run.status.refresh} here~~
time for console refresh of model run status, default: 0.2 (12sec)
}
  \item{all.local.cores}{
if \code{TRUE} and \code{run.mode}="serial" all cores of local machine are used for analysis
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
  \item{delete.folder.countdown}{
%%     ~~Describe \code{delete.folder.countdown} here~~
countdown for deletion of 'folder', default: 5 (seconds)
}
  \item{conquestParameters}{
							Set ConQuest parameters as a named list.

							Available option are:

							"pathConquest","method","std.err","distribution","n.plausible","set.constraints",
							"nodes","p.nodes","f.nodes","n.iterations","converge","deviancechange",
							"equivalence.table","use.letters","na","model.statement"

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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
