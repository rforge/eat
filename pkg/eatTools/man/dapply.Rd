\name{dapply}
\alias{dapply}

\title{
apply a function on data.frame
}

\description{
split a data.frame, apply a function, and get data.frame with results
}

\usage{
dapply ( data , split.vars = NULL , fun = mean , new.name = NULL , wide = TRUE , verbose = FALSE , ... )
}

\arguments{
  \item{data}{a data frame}
  \item{split.vars}{colnames of variables to split data.frame}
  \item{fun}{a function}
  \item{new.name}{character vector of new name(s) of transformed variables}
  \item{wide}{logical, if \code{TRUE} (default) a data.frame in wide format is returned, if \code{FALSE} a data.frame in long format is returned}
  \item{verbose}{logical, print progress or not}
  \item{...}{arguments to be passed to \code{fun}}
}
\details{
}
\value{
A data frame with results. Depending on \code{wide} a data.frame in either wide or long format.
}

\author{
Martin Hecht
}

%\examples{
%\dontrun{
%}
%}
