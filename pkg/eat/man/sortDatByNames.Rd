\name{sortDatByNames}
\alias{sortDatByNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sort data.frame by colnames and/or rownames
}
\description{
specify new colnames and/or rownames order, data.frame is sorted in accordance
}
\usage{
sortDatByNames ( dat , col.order = NULL , row.order = NULL , warn = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame }
  \item{col.order}{ character vector of colnames in new order }
  \item{row.order}{ character vector of rownames in new order }
  \item{warn}{logical, if \code{TRUE} warnings are printed on output window if \code{col.order}/\code{row.order} do not correspond to colnames/rownames resp.}
  
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
data.frame
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
dat <- data.frame ( matrix ( rnorm ( 100 ) , ncol = 10 ) )
colnames ( dat ) <- paste ( "X" , 10:1 , sep = "" )
rownames ( dat ) <- paste ( "X" , 11:2 , sep = "" )
dat

# sort data.frame by 'col.order' and 'row.order'
sortDatByNames ( dat , paste ( "X" , 1:10 , sep = "" ) , paste ( "X" , 2:11 , sep = "" ) )
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
