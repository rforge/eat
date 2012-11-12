\name{design-class}
\Rdversion{1.1}
\docType{class}
\alias{design-class}
\alias{-,design,design-method}
\alias{+,design,design-method}
\alias{show,design-method}

\title{Class \code{"design"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
A design object contains descriptives of a data structure.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("design")}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{definition}:}{Object of class \code{"data.frame"}
	
	contains the definition of the design }
    \item{\code{elements}:}{Object of class \code{"character"}
	
	contains the names of the elements of the design (these are simply the column names of \code{definition}}
    \item{\code{units}:}{Object of class \code{"list"}
	
	contains the unique units of \code{elements}}
    \item{\code{nunits}:}{Object of class \code{"integer"}
	
	contains the number of \code{units}}
    \item{\code{structure}:}{Object of class \code{"data.frame"}
	
	contains information on the relation between \code{elements}, the relation between two elements can be "equivalent", "unconnected", "nested", "nestor" (this is the grouping variable of "nested" units), "crossedpartially" or "crossedcompletely" }
    \item{\code{structureList}:}{Object of class \code{"list"}
	
	contains units of one element in relation to one unit of another element}
    \item{\code{descriptives}:}{Object of class \code{"data.frame"}
	
	contains information on the number of units of one element with reference to another element}
    \item{\code{linkList}:}{Object of class \code{"list"}
	
	contains a graph of class "igraph" for each pair of \code{elements}}
    \item{\code{adjacency}:}{Object of class \code{"list"}
	
	contains "adjacency" matrices, see \link[igraph]{get.adjacency}}
    \item{\code{link}:}{Object of class \code{"data.frame"}
	
	contains link descriptives:
	
			average path length, see \link[igraph]{average.path.length}
			
			relative frequency of realized (unique) pairwise links in reference to all pairwise links
			
			relative frequency of realized pairwise links in reference to all theoretically possible pairwise links if elements were completely crossed
			
			mean degree of units, see \link[igraph]{degree}
			
			standard deviation of degree of units, see \link[igraph]{degree}
			
	}
    \item{\code{varCovMatrix}:}{Object of class \code{"matrix"}, contains the Variance-Covariance Matrix of the design, see Frey (2009) for details}
    \item{\code{designDescriptives}:}{Object of class \code{"list"}, contains the D-optimality index that is computed from \code{varCovMatrix}}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "design")}: displays an object of class "design" }
	\item{+}{\code{signature(e1 = "design", e2 = "design")}: add one design to another ("merge" two designs)}
	\item{-}{\code{signature(e1 = "design", e2 = "design")}: distract one design from another, this is functional only for designs that contain the same elements}
	 }
}
\references{
Frey, A., Hartig, J., & Rupp, A. A. (2009). An NCME Instructional Module on Booklet Designs in Large-Scale Assessments of Student Achievement: Theory and Practice. Educational Measurement: Issues and Practice, 28(3), 39-53.
}
\author{
Martin Hecht
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
This version is alpha. Use with care.
}

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\link{defineDesign}}

\code{\link{updateDesign}}
}
\examples{
showClass("design")
}
\keyword{classes}
