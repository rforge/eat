\name{makeNumeric}
\alias{makeNumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Change Character Variables to \code{numeric}
}
\description{Converts character variables, which contain only values, to \code{numeric}. Character variables containing letters are not converted. This avoids warnings, if conversion to \code{numeric} is attempted for variables, which contain characters.
}
\usage{
makeNumeric(variable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{ Variable to be changed to \code{numeric}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Variable converted to \code{numeric}, if possible.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Haag
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
a <- c("1", "2", "3", "4")
b <-  c("1", "2", "x", "4")
makeNumeric(a)
makeNumeric(b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
