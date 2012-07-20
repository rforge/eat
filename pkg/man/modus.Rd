\name{modus}
\alias{modus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
modus
}
\description{
calculates modus (most frequent value)
}
\usage{
modus ( x , randTies = FALSE ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector
}
  \item{randTies}{
if ties occur draw a randomized value out of tied values
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
returns the modus (most frequent element)
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
x <- c ( 1 , 1 , 2 , 2 )
( modus ( x ) )
( modus ( x , randTies = TRUE ) )

x <- c ( 1 , NA , NA )
( modus ( x ) )

x <- c ( "x" , "x" , "y" )
( modus ( x ) )
}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
