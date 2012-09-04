\name{readSpss}
\alias{readSpss}

\title{
Read SPSS Data Files and Truncate Space in String Variables and Change Column Width
}

\description{
Read SPSS Data Files and Truncate Space in String Variables and Change Column Width
}

\usage{
loadSav(file, correctDigits = FALSE, truncateSpaceChar = TRUE)
}

\arguments{
  \item{file}{
 Name of the SPSS data file to be read in
 }
  \item{correctDigits}{
 Logical: whether codes should be transformed to have uniform width in each column, see Details.
}
  \item{truncateSpaceChar}{
  Logical: whether string variables should be trimmed to remove leading and trailing spaces.
}
}
\details{
coming soon
}
\value{
  A data frame with trimmed character variables and corrected codes (if specified). All columns are of mode \code{character}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Haag, Sebastian Weirich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
