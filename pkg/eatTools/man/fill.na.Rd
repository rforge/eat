\name{fill.na}
\alias{fill.na}
\title{
fill NAs in a vector
}
\description{
fill NAs with non-NA values depending on left (forward) or right (backward) non-NA value
}
\usage{
fill.na(vec, backwards = FALSE, na.rm = FALSE)
}
\arguments{

  \item{vec}{ a vector }
  \item{backwards}{ if \code{FALSE} NAs are filled forward, if \code{TRUE} NAs are filled backwards }
  \item{na.rm}{ if \code{TRUE} NAs at start and end of vector are removed }
  
}
\details{
}
\value{
a vector with filled NAs
}

\author{
Martin Hecht
}

\examples{
(vec <- c(NA, 1, NA, NA, 2, NA, 3, NA))
fill.na(vec)
fill.na(vec, backwards = TRUE)
}
