\name{modus}
\alias{modus}
\title{Compute the Mode of a Variable
}
\description{
Calculate the mode (most frequent value) of a variable
}
\usage{
modus(x, randTies = FALSE) 
}
\arguments{
  \item{x}{
a vector
}
  \item{randTies}{If \code{TRUE}, in case more than one mode is found, one random value of all modes is returned.
}
}
\details{The \code{modus} function is designed to always return only one value for the mode of a variable. If the variable is bimodal or multimodal, the function returns either \code{NA} (if \code{randTies = FALSE}) or a randomly chosen value of all modes (if \code{randTies = TRUE}).

}
\value{
the mode (most frequent value) of the variable
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
