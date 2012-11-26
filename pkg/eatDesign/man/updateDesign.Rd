\name{updateDesign}
\alias{updateDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
update an object of class "design"
}
\description{
This function can be used to update a design. This might be useful to compute descriptives on a previously created design object.
}
\usage{
updateDesign ( dsgn , descriptives = TRUE , verbose = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsgn}{Object of class \code{"design"}}
  \item{descriptives}{logical, compute descriptives (can be time consuming)}
  \item{verbose}{logical, print information while processing}
}
%% \details{}
\value{
returns an object of class \code{"design"}
}
%% \references{}
\author{
Martin Hecht
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning }{
This version is alpha. Use with care.
} 

\seealso{
\code{\link{defineDesign}}
\code{\link{design-class}}
}

\examples{

# Table 7 (Frey, 2009)
table7 <- data.frame ( "Booklet" = c(1,1,2,2,3,3) , "Position" = c(1,2,1,2,1,2) ,
"Cluster" = c(1,2,2,3,3,1) )

# create a new design object
( design7 <- new ( "design" ) )

# use table7 as the definition of the design, do not compute descriptives
( design7 <- defineDesign ( dsgn = design7 , def = table7 , descriptives = FALSE ) )

# compute descriptives
( design7 <- updateDesign ( dsgn = design7 , descriptives = TRUE ) )

}
