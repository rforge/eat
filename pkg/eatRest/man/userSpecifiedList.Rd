\name{userSpecifiedList}
\alias{userSpecifiedList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{userSpecifiedList}
\description{When a function requires several arguments as a list, \code{userSpecifiedList} is designed to 'match' users arguments to default argument structure.}
\usage{
userSpecifiedList(l, l.default, el.default = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
%%     ~~Describe \code{file} here~~
A named or unnamed list or vector of elements.
}
  \item{l.default}{
%%     ~~Describe \code{dif.term} here~~
The default list of arguments needed by a function.
}
  \item{el.default}{
if length of \code{l} is longer than 1, \code{el.default} can be set to an numeric value to select one default element
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of arguments where the user specified arguments are matched into default arguments.
If \code{el.default} is set, one element is returned.
}
\references{
%% ~put references to the literature/web site here ~
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
default.arguments <- list(logfile = TRUE, systemfile = TRUE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = TRUE)
users.arguments   <- c(FALSE, FALSE)
userSpecifiedList ( l = users.arguments, l.default = default.arguments)

users.arguments   <- list(history = FALSE)
userSpecifiedList ( l = users.arguments, l.default = default.arguments)

users.arguments   <- FALSE
names(users.arguments) <- "covariance"
userSpecifiedList ( l = users.arguments, l.default = default.arguments)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
