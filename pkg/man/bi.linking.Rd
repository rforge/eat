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
bi.linking ( results , scales=NULL , folder=NULL , file.name=NULL , method = NULL , lower.triangle = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
result list from automateModels run
}
  \item{scales}{
Character vector of scales for which linking should separately done. If NULL, all analysis in the results list are linked.
Note: due to suboptimalities in development process, analysis name must contain scale!!
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
