\name{bi.linking}
\alias{bi.linking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bi.linking
}
\description{
Links results from several analysis. Each analysis is linked with each other. 
}
\usage{
bi.linking ( results , folder=NULL , file.name=NULL , method = NULL , lower.triangle = TRUE , scales=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
result list from automateModels run
}
  \item{folder}{
output folder, will be emptied!
}
  \item{file.name}{
file.name for output excel, default: "bi.linking.results.xlsx"
}
  \item{method}{
set linking method to either "Mean-Mean" , "Haebara" or "Stocking-Lord" (default)
}
  \item{lower.triangle}{
set reference groups for the linking
}
  \item{scales}{
Character vector of scales for which linking should separately done. If NULL, all analysis in the results list are linked.
Note: due to suboptimalities in development process, analysis name must contain 'scale'! use this option with care!!
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
writes linking results to excel file.
returns linking results as list.
}
\references{

}
\author{
Martin Hecht
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
# load example data
# this is the results structure returned from running Example 5 of \link{automateModels}
# see there for details of analyses
#
data ( ex5 )
#
# ex5 contains the results of 3 analyses:
names(ex5)
# [1] "all.i__grade.10"  "all.i__grade.9"   "all.i__grade.all"
#
# each pair of these 3 analyses are linked together by bi.linking
# if not run together, you can easily combine analyses from seperate \link{automateModels} runs by calling c(<analysis1>,<analysis2>,...)
# in this case make sure that analysis names are unique
#
# start linking, results are written to folder and are returned 
ex5_linked <- bi.linking ( ex5 , folder = "C:/temp/automateModels/Example5/Linking" , file.name = "ex5_linked" )
#
#
# if you want to link analyses for which no \code{automateModels} results structure is available
# you can use \link{make.link.dummy} to create a structure similar to \code{automateModels} results structure
# this can be used as input for \code{bi.linking}
#
# e.g. let's add an additional analysis for which only item difficulty and standard errors are available
# this information must be in a data.frame, see \link{make.link.dummy} for details
dfr <- data.frame ( 
		"item" = c ( "BioKno01" , "CheKno02" , "PhyKno03" ) ,
		"b"	   = c ( -3.14      , -2.24      , -3.42      ) ,
		"b.se" = c ( 0.612      , 0.453      , 0.783      )
)
#
# create a results object
add <- make.link.dummy ( dfr , "additional" )
#
# add this object to Example 5
ex5add <- c ( add , ex5 )
#
# start the linking procedure with the additional analysis
ex5add_linked <- bi.linking ( ex5add , folder = "C:/temp/automateModels/Example5/Linking2" , file.name = "ex5add_linked" )
}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
