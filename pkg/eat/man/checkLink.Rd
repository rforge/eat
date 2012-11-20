\name{checkLink}
\alias{checkLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
checkLink
}
\description{
Checks whether items in a dataset are linked via design. This may be useful in multiple matrix sampling designs.
}
\usage{
checkLink ( dat, remove.non.responser = FALSE, na = NA, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ A data.frame where all columns denote test items }
  \item{remove.non.responser}{ logical: Should cases with missings on all items be deleted?}
  \item{na}{ character string specifying values to be treat as missing by design  }
  \item{verbose}{ logical: Should output printed to console? }
  

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
A logical value, i.e. \code{TRUE} or \code{FALSE}, indicating whether items in dataset are linked to each other. 
 }
\references{

}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat    <- data.frame(item01 = c(1,0,NA,NA,NA), item02 = c(NA,NA,1,0,0), item03 = c(1,NA,NA,NA,NA) )
result <- checkLink(dat)
dat    <- data.frame(dat, item04 = c(0, NA,NA,NA,1) )
result <- checkLink(dat)

# checkLink for each dimension in science1
data(science1)
results <- by (data = science1.item.characteristics, INDICES = science1.item.characteristics$scale, FUN = function ( scales ) {
               collapsed <- collapseMissings(science1[,scales$item])
               results   <- checkLink(collapsed)
           })

# However, if only persons which anwered at least one item are considered
results <- by (data = science1.item.characteristics, INDICES = science1.item.characteristics$scale, FUN = function ( scales ) {
               collapsed <- collapseMissings(science1[,scales$item])
               results   <- checkLink(collapsed, remove.non.responser = TRUE)
           })
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
