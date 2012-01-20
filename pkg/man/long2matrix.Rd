\name{long2matrix}
\alias{long2matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
long2matrix
}
\description{
transforms long format data.frame into a matrix format data.frame
}
\usage{
long2matrix ( data , sort = TRUE , triangle = NULL ,
				force.diagonal = FALSE , exclude.diagonal = FALSE ,
				long2matrix = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data}{ data.frame with columns "row" , "col" , "val" }
  \item{sort}{ sort rows and columns of matrix }
  \item{triangle}{ if not \code{NULL} a symmetric matrix will be constructed
				   available options are "upper" , "lower" , "both" }
  \item{force.diagonal}{ a diagonal is forced into matrix even if no diagonal elements are in \code{data}  }
  \item{exclude.diagonal}{ the diagonal is excluded if possible }
  \item{long2matrix}{ if \code{FALSE} \code{data} is not transformed }  
  
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
\item{long2matrix = TRUE }{data.frame in matrix format}
\item{long2matrix = FALSE }{data.frame in long format}
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
d1 <- data.frame (
"row" = c ( "v1" , "v2" , "v2" , "v3" , "v1" , "v3" ) , 
"col" = c ( "v1" , "v3" , "v2" , "v1" , "v2" , "v3" ) , 
"val" = c ( 1 , 5 , 4 , 3 , 2 , 6 ) , stringsAsFactors = FALSE )

# unsorted matrix
long2matrix  ( data = d1 , sort = FALSE )
# sorted by default
long2matrix  ( data = d1 )
# extract upper triangle of symmetric matrix
long2matrix  ( data = d1 , triangle = "upper" )
# exclude diagonal elements
long2matrix  ( data = d1 , triangle = "upper" , exclude.diagonal = TRUE )
# if full matrix ("both" triangles) is requested, the diagonal cannot be excluded, option is ignored
long2matrix  ( data = d1 , triangle = "both" , exclude.diagonal = TRUE )

# no diagonal elements are specified
d2 <- data.frame (
"row" = c ( "v2" , "v1" , "v1" ) , 
"col" = c ( "v3" , "v3" , "v2" ) , 
"val" = c ( 5 , 3 , 2 ) , stringsAsFactors = FALSE )

long2matrix ( data = d2 )
# diagonal is set (with NAs)
long2matrix ( data = d2 , triangle = "upper" , force.diagonal = TRUE )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
