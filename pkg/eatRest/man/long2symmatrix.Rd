\name{long2symmatrix}
\alias{long2symmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
long2symmatrix
}
\description{
transforms long format data.frame into a symmetric matrix format data.frame
}
\usage{
long2symmatrix ( dat , sort = FALSE , triangle = c ("both","lower","upper") ,
				 include.diagonal = TRUE , full.symmetric = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame with columns "row" , "col" , "val" }
  \item{sort}{ sort rows and columns of matrix, can be either logical (if TRUE variables are alphatecially sorted) or a character vector that indicates order }
  \item{triangle}{ if "lower" or "upper" only this triangle is extracted (the other is set to NA), if "both" both triangles are extracted }
  \item{include.diagonal}{ logical, should diagonal elements be included or not  }
  \item{full.symmetric}{ if \code{TRUE}  }
  
}
\details{
WARNING: This function has not been thoroughly tested.
if \code{sort = FALSE} and \code{triangle = "lower"} matrix is sorted by occurence on dat$row
if \code{sort = FALSE} and \code{triangle = "upper"} matrix is sorted by occurence on dat$col
\code{triangle = "both"} implies \code{sort = TRUE}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
data.frame in "matrix format"
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
long <- data.frame (
"row" = c ( "v1" , "v2" , "v2" , "v3" , "v1" , "v3" ) , 
"col" = c ( "v1" , "v3" , "v2" , "v1" , "v2" , "v3" ) , 
"val" = c ( 1 , 5 , 4 , 3 , 2 , 6 ) , stringsAsFactors = FALSE )		
		
long2symmatrix ( long )

long2symmatrix ( long , triangle = "lower" )
long2symmatrix ( long , triangle = "upper" )
long2symmatrix ( long , triangle = "both" )

long2symmatrix ( long , triangle = "lower" , include.diagonal = FALSE)
long2symmatrix ( long , triangle = "upper" , include.diagonal = FALSE)
long2symmatrix ( long , triangle = "both" , include.diagonal = FALSE)

long2symmatrix ( long , sort = c("v2","v3") )
long2symmatrix ( long , sort = c("v2","v3") , triangle = "lower" , include.diagonal = FALSE )

long2symmatrix ( long , full.symmetric = TRUE )
long2symmatrix ( long , full.symmetric = TRUE , triangle = "lower" )
long2symmatrix ( long , full.symmetric = TRUE , triangle = "lower" , include.diagonal = FALSE )
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
