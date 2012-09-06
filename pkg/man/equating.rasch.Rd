\name{equating.rasch}
\alias{equating.rasch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Align Item Parameters from Separate Analyses
}
\description{This function can be used to align two sets of item parameters from two different Rasch analyses (e.g., two populations of examinees of differing abilities) so that they are on the same scale. The item parameters of one group are transformed to the scale of the other group by adding a constant.
}
\usage{
equating.rasch(x, y, theta = seq( -4, 4, len=100), method = c("Mean-Mean", "Haebara", "Stocking-Lord"), compute.dif = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{ A data.frame with item names and parameters for group 1. This is the group which will be linked to the scale of group 2. The data.frame has to follow the structure: First column contains item names for the focal group, second column contains item parameters, and, if \code{compute.dif = TRUE}, third colunm contains the standard errors of the item parameters. }
  \item{y}{A data.frame with item names and parameters for group 2. This is the group for which the scale is defined. The data.frame has to follow the structure: First column contains item names for the focal group, second column contains item parameters, and, if \code{compute.dif = TRUE}, third colunm contains the standard errors of the item parameters.  }
  \item{theta}{theta values where the test characteristic curves are evaluated}
  \item{method}{Method for determining the linking constant, either \code{Mean-Mean}, \code{Haebara} or code{Stocking-Lord} }
  \item{compute.dif}{Logical: Whether differential item functioning in the two groups should be examined.}

}
\details{\code{equating.rasch} provides three methods to determine this constant: \code{Mean-Mean} the difference of the item parameter means of both samples are obtained. \code{Haebara} additionally takes the difference between item characteristic curves into account. \code{Stocking-Lord} additionally takes the test characteristic functions in account, thus minimizing differences in expected scores rather than observed scores or parameters. In most practical applications, the three linking constants should be fairly similar.

When \code{compute.dif = TRUE}, differential item functioning (DIF) in anchor items is examined. This can be useful to examine items with large shifts, which can be subsequently excluded from the linking procedure. DIF is computed according to the formula in Lord (1980). Additionally, the   magnitude of DIF is categorized as small, moderate or large according to criteria established by the Educational Testing Service (ETS): category A (small DIF) if |DIF| < 0.43 or not significantly > 0, category B (moderate DIF) if 0.43 < |DIF| < 0.64 and |DIF| significantly > 0, and category C (large DIF) if |DIF| > 0.64 and significantly > 0.43. 

}
\value{
A list with the following components:
  \item{B.est}{Linking constants determined by all three methods}
  \item{descriptives}{A list with the number of items used for linking, linking variance and standard deviation, and the linking error}
\item{anchor}{A data.frame with all item parameters used for linking from both samples and the transformed parameters for group 1. If \code{compute.dif = TRUE}, additional statistics for DIF are also included.}
\item{transf.par}{A data.frame with all item parameters from both samples and the transformed parameters for group 1.}
}
\references{
Kolen, M. J. & Brennan, R. L. (2004). \emph{Test equating, scaling, and linking: Methods and practices}. New York: Springer.
Yen, W. M., & Fitzpatrick, A. R. (2006). \emph{Item response theory}. In R. L. Brennan (Ed.), Educational Measurement (4th ed., pp. 111-153). Westport, CT: American Council on Education.
}
\author{
Alexander Robitzsch
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{\code{bi.linking}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
