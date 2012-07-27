\name{make.link.dummy}
\alias{make.link.dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make.link.dummy
}
\description{
create a structure as input for \link{bi.linking}
}
\usage{
make.link.dummy ( dfr , analysis.name = "dummy.analysis" , scale.name = "dummy.scale", group.name = "dummy.group" ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfr}{
data.frame with items, item difficulty ("b") and standard error of b ("b.se");
colnames must be "item", "b", "b.se"
}
  \item{analysis.name}{
name of analysis
}
  \item{scale.name}{
name of scale (dimension)
}
  \item{group.name}{
name of (person) group
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
returns a structure as if created by \link{automateModels} with data contained in \code{dfr}; this object can be used as input for \link{bi.linking}
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
dfr <- data.frame ( 
		"item" = c ( "BioKno01" , "CheKno02" , "PhyKno03" ) ,
		"b"	   = c ( -3.14      , -2.24      , -3.42      ) ,
		"b.se" = c ( 0.612      , 0.453      , 0.783      )
)
link.dummy <- make.link.dummy ( dfr )
}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
