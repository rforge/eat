\name{fill.na}
\alias{fill.na}
\title{
Replace Missing Values in a Vector
}
\description{
Missing values in a vector are replaced by the last (forward) or next (backward) observed value.
}
\usage{
fill.na(vec, backwards = FALSE, na.rm = FALSE)
}
\arguments{

  \item{vec}{ a vector }
  \item{backwards}{ if \code{FALSE} NAs are replaced by the last observed value, if \code{TRUE} NAs are replaced by the next observed value }
  \item{na.rm}{ if \code{TRUE} NAs at the start and end of vector are removed }
  
}
\details{
In the clinical literature, the procedure of replacing a missing value with the last observed value is known as the "Last Observation Carried Forward" imputation technique. However, there is a large body of literature suggesting that this method may lead to biased estimates of means and covariances and should therefore be avoided for imputation.

}
\value{
A vector with replaced missing values.
}

\author{
Martin Hecht
}

\examples{
(vec <- c(NA, 1, NA, NA, 2, NA, 3, NA))
fill.na(vec)
fill.na(vec, backwards = TRUE)
}
