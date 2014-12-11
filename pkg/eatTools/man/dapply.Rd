\name{dapply}
\alias{dapply}

\title{
apply a function on data.frame
}

\description{
split a data.frame, apply a function, and get data.frame with results
}

\usage{
dapply ( data , split.vars = NULL , fun = mean , wide = TRUE , drop = TRUE , all.level = FALSE , verbose = FALSE , ... )
}

\arguments{
  \item{data}{a data frame}
  \item{split.vars}{colnames of variables to split data.frame}
  \item{fun}{a function}
  \item{wide}{logical, if \code{TRUE} (default) a data.frame in wide format is returned, if \code{FALSE} a data.frame in long format is returned}
  \item{drop}{logical, if \code{TRUE} (default) empty combinations will be dropped, if \code{FALSE} all combinations are kept and NAs are generated}
  \item{all.level}{logical, if \code{TRUE} all levels of factors are incorporated, if \code{FALSE} (default) only factor levels existing in the data are used}
  \item{verbose}{logical, if \code{TRUE} progress is printed, if \code{FALSE} (default) no console output is generated}
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
