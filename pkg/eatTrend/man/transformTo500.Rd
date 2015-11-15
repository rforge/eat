\name{transformTo500}
\alias{transformTo500}
\title{Transform IRT parameters to another metric}
\description{
This function transforms IRT parameters to an arbitrary metric.
}
\usage{transformTo500(pars, mRefPop=NULL, sdRefPop=NULL, wgts=NULL, 
type=c("itPar", "persPar"), cutScores=NULL)}
\arguments{
  \item{pars}{
   A data frame containing the parameter estimates. The first column has to 
   be an ID variable, the second column contains the estimates.
}
 \item{mRefPop}{
Numeric. The mean of the reference population.
}
  \item{sdRefPop}{
Numeric. The standard deviation in the reference population.
}
  \item{wgts}{
  A vector containing the case weights.
}
 \item{type}{
Character. If \code{itPar}, a constant of \code{log(0.625/0.375)} will be added to
the parameters. If \code{persPar}, no additional constant will be added.
}
 \item{cutScores}{
Named vector indicating the proficiency level cut scores in descending(!) order.
If is not \code{NULL}, the function \code{\link{addCuts}} will be called.
}
}

\value{
A data frame containing the transformed parameters.
}

\author{
Karoline Sachse
}

\examples{
}
