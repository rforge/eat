\name{modus}
\alias{modus}
\title{
modus
}
\description{
calculates modus (most frequent value)
}
\usage{
modus(x, randTies = FALSE) 
}
\arguments{
  \item{x}{
a vector
}
  \item{randTies}{
if ties occur draw a randomized value out of tied values
}
}
\details{
}
\value{
returns the modus (most frequent element)
}
\author{
Martin Hecht
}
\examples{
\dontrun{
x <- c(1, 1, 2, 2)
modus(x)
modus(x, randTies = TRUE)

x <- c(1, NA, NA)
modus(x)

x <- c("x", "x", "y")
modus(x)
}}
