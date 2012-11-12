\name{defineDesign}
\alias{defineDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
define a data structure ("design")
}
\description{
This function can be used to define a design. An object of class \code{"design"} is created. Descriptives can be computed.
}
\usage{
defineDesign ( dsgn , def = data.frame() , append = FALSE , descriptives = TRUE ,
verbose = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsgn}{Object of class \code{"design"}}
  \item{def}{a data frame containing design elements in columns and units of these elements in rows, see example}
  \item{append}{logical, append \code{def} to \code{dsgn} or overwrite}
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
\code{\link{updateDesign}}
}

\examples{

# Table 7 (Frey, 2009)
table7 <- data.frame ( "Booklet" = c(1,1,2,2,3,3) , "Position" = c(1,2,1,2,1,2) ,
"Cluster" = c(1,2,2,3,3,1) )

# create a new design object
( design7 <- new ( "design" ) )

# use table7 as the definition of the design
design7 <- defineDesign ( dsgn = design7 , def = table7 )

# print design object (with auto-generated descriptives)
design7

# add some more cases, using option 'append' in 'defineDesign'
add <- data.frame ( "Booklet" = c(4,4) , "Position" = c(3,4) , "Cluster" = c(4,5) )
( design8 <- defineDesign ( dsgn = design7 , def = add , append = TRUE ) )

# add even more cases, this time using '+' operator on 2 designs
# (results are the same, but one of either method might be more convenient)
add2 <- data.frame ( "Booklet" = c(5,5,6,6) , "Position" = c(5,6,5,6) , "Cluster" = c(6,7,7,8) )
( design2 <- defineDesign ( def = add2 ) )
( design9 <- design7 + design2 )

# add items that are nested within clusters
add3 <- data.frame ( "Item" = paste ( "item" , 1:12 , sep = "" ) ,
"Cluster" = as.vector ( sapply ( 1:3 , rep , 4 ) ) )
( design3 <- defineDesign ( def = add3 ) )
( design10 <- design3 + design7 )

}
