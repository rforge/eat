\name{splitModels}
\alias{splitModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create several models by splitting the qMatrix and/or person.groups
}
\description{
\code{\link{defineModel}} is programmed to define a single model.
With \code{splitModels} several models can be set up.
The output of \code{splitModels} can be directly passed to 
the \code{splittedModels} argument of \code{\link{defineModel}}
}
\usage{
splitModels ( qMatrix = NULL , person.groups = NULL , 
    split = c ( "qMatrix" , "person.groups" ) , add = NULL , cross = NULL , 
    all.persons = TRUE , all.persons.lab = "all" , 
    person.split.depth = 0:length(person.groups[,-1,drop=FALSE]) , 
    full.model.names = TRUE , model.name.elements = c ( "dim" , "group" , "cross" ) , 
    include.var.name = FALSE , env = FALSE , nCores=NULL , GBcore=NULL , verbose = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qMatrix}{
Same argument as in \code{\link{defineModel}}:

Optional: A named data frame indicating how items should be grouped to dimensions. The
first column contains the names of all items and should be named item. The other
columns contain dimension definitions and should be named with the respective
dimension names. A positive value (e.g., 1 or 2 or 1.4) indicates the loading weight
with which an item loads on the dimension, a value of 0 indicates that the respective
item does not load on this dimension. If no q matrix is specified by the user, an
unidimensional structure is assumed.
}
  \item{person.groups}{
data.frame, first row must be person ID, further columns contain group categories, e.g. data.frame ( "id" = 1:10 , "sex" = sample ( c ( "male" , "female" ) , 10 , replace = TRUE ) )
}
  \item{split}{
character, possible values:

NULL							: qMatrix and person.groups are not split, one model with original qMatrix and all persons is set up
"qMatrix"						: qMatrix is split into single dimensions, number of created models equals number of dimensions
"person.groups"					: person.groups is split into single groups, number of created models equals number of all combinations of groups (with at least one person)
c("qMatrix","person.groups")	: default, both qMatrix and person.groups is split and single dimensions and single groups are crossed, number of created models equals number of dimension multiplied with number of all combinations of groups

}
  \item{add}{
list of elements with single values, names of elements should be arguments of \code{\link{defineModel}}, elements are the value that is passed when running \code{\link{defineModel}}; elements in \code{add} are used for all models; e.g. list ( "software" = "conquest" , "nodes" = 15 ), that means that all models are estimated with "conquest" and 15 nodes
}
  \item{cross}{
list of elements with several values, names of elements should be arguments of \code{\link{defineModel}}, elements are the value that is passed when running \code{\link{defineModel}}; elements in \code{cross} are crossed into models; e.g. list ( "software" = c("conquest","tam") , "nodes" = c(15,30) ), now all models are set up to run once with "conquest" and once with "tam", and with 15 and 30 nodes
}
  \item{all.persons}{
logical (default: TRUE), for each group variable in \code{person.groups} an "all" category is included
  }
  \item{all.persons.lab}{
character, name of the "all" category 
  }
  \item{person.split.depth}{
integer, depth of group splits, 0: global all persons are included, 1: groups of all variables are included, 2: groups of all pairs of variables are included, n: groups of n variables are included. Can be a vector with more than one argument, e.g. for 3 variables, the full number of splits (which is also the default) can be obtained by c(0,1,2,3); this creates a model with all persons (0), all groups of all variables (1), groups from pairs of variables (2), and groups from combining all 3 variables (3). Using \code{person.split.depth} usually makes most sense if \code{all.persons=TRUE}; if \code{all.persons=FALSE} the depth equals the number of variables (if another depth is set, no splits will be performed).
  }
  \item{full.model.names}{
logical (default: TRUE), model names are derived from \code{model.name.elements}; if FALSE models are numbered ascendingly
}
  \item{model.name.elements}{
character, elements that model names are built of, possible values: "dim" , "group" , "add" , "cross"; default: c ( "dim" , "group" , "cross" ) , that means that model names include the name of the dimension(s) , group(s) , and parameter values that are crossed in
}
  \item{include.var.name}{
logical (default: FALSE), include the name of the variable when building model names; e.g. (FALSE) "science__sex.female__conquest" , (TRUE) "dim.science__group.sex.female__software.conquest"
}
  \item{env}{
logical (default: FALSE)

(FALSE) returns a list with two elements: data.frame with model information (model overview), list with model specifications
(TRUE)  returns a list with two elements: data.frame with model information (model overview), list of environments with model specifications set as objects

for intended subsequent use with \code{\link{defineModel}} use \code{env=FALSE}

}
  \item{nCores}{
integer (default: NULL), number of cores to use for subsequent data preparation, model estimation and results compilation
}
  \item{GBcore}{
numeric (default: NULL), maximum RAM usage per core
}
  \item{verbose}{
logical (default: TRUE), print progess
}
  
  
}
%% \details{}
\value{
depending on \code{env} either:

(env=FALSE) returns a list with two elements: data.frame with model information (model overview), list with model specifications
(env=TRUE)  retuns a list with two elements: data.frame with model information (model overview), list of environments with model specifications set as objects

for intended subsequent use with \code{\link{defineModel}} use \code{env=FALSE}

}
%% \references{}
\author{
Martin Hecht
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{defineModel}}
}

\examples{

# see also examples in 'defineModel'
# example qMatrix
qMatrix <- data.frame ( "item" = 1:4 , "science" = c(1,1,0,0) , "math" = c(0,0,1,1) , stringsAsFactors = FALSE )

# example person.groups
person.groups <- data.frame ( "person" = 1:4 , "state" = rep(c("Berlin","Bavaria"),2) , "sex" = c(rep("female",2),rep("male",2)) , stringsAsFactors = FALSE )

# Example 1: one 2-dimensional model with all persons (no split)
m01 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups, split=NULL )
m01$models

# Example 2: split qMatrix to create two unidimensional models, each with all persons
m02 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups, split=c("qMatrix") )
m02$models

# Example 3: split person.groups to create 2-dimensional models, each with a subgroup of persons
m03 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups, split=c("person.groups") )
m03$models

# Example 4: split both qMatrix and person.groups to create unidimensional models for all subgroups
m04 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups, split=c("qMatrix","person.groups") )
m04$models

# Example 5: set "software"="conquest" and "method"="montecarlo" for all models
m05 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups , add = list ( "software"="conquest" , "method"="montecarlo" ) )
m05$models

# Example 6: cross "nodes"=c(1000,5000) and "seed"=c(1234,4321) into all models
m06 <- splitModels ( qMatrix=qMatrix, person.groups=person.groups , add = list ( "software"="conquest" , "method"="montecarlo" ) , cross = list ( "nodes"=c(1000,5000) , "seed"=c(1234,4321) ) )
m06$models

# Example 7: list elements in cross that contain more than one element need to be lists themselves
m07 <- splitModels ( qMatrix=qMatrix, person.groups=NULL , cross = list ( "regression"=list( c("sex") , c("sex","state") ) , "seed"=c(1234,4321) ) )
m07$models

# Example 8: create an "empty" model without qMatrix and person.groups
m08 <- splitModels ( qMatrix=NULL, person.groups=NULL )
m08$models

}
