\name{eatRep-package}
\alias{eatRep-package}
\docType{package}
\title{
	Statistical analyses in complex survey designs with multiple imputed data.
}
\description{
  Computes some basic statistic operations (means, standard deviations, frequency tables,
  percentiles and generalized linear models) in complex survey designs comprising multiple imputed variables
  and/or a clustered sampling structure which both deserve special procedures at least in estimating standard errors.

  For example, computing standard errors for the mean of a multiple imputed variable (e.g. plausible values) involves the
  formulas provided by Rubin (1987). Computing standard errors for the mean of a nested imputed variable
  involves the formulas provided by Rubin (2003). Both methods are implemented in the package. The estimation of 
  \eqn{R^2} and adjusted \eqn{R^2} in linear and generalized linear regression models with multiple imputed data sets is 
  realized using the methods provided in Harel (2009). 

  Moreover, computing standard errors for the mean of a variable which stems from a clustered design may involve
  replication methods like balanced repeated replicate (BRR), bootstrap or Jackknife methods.
  See Weststat (2000), Foy, Galia & Li (2008), Rust and Rao (1996), and Wolter (1985) for details. 
  To date, only the Jackknife-2 (JK2) and the Balanced Repeated Replicates (BRR) procedures are supported.

  The package \code{eatRep} is designed to combine both methods which is necessary if (nested) multiple imputed
  data are used in clustered designs. Considering the structure is relevant especially for the estimation of
  standard errors.

  Technically, \code{eatRep} is a wrapper for the \code{survey} package (Lumley, 2004). Each function in
  \code{eatRep} corresponds to a specific function in \code{survey} which is called repeatedly during the analysis.
  Hence, a nested loop is used. We use \dQuote{imputation replicates} in the outer loop to account for multiple imputed 
  data, and \dQuote{cluster replicates} in the inner loop to account for the clustered sampling structure. While the 
  functional principle of \code{survey} is based on replication of standard analyses, \code{eatRep} is based on 
  replication of \code{survey} analyses to take multiple imputed data into account. 
  
  For each imputed data set, i.e. in the inner loop, the \code{eatRep} function first creates replicate weights 
  based on the primary sampling unit (PSU) variable and the replication indicator variable. In the jackknife procedure, 
  the first one is often referred to as \dQuote{Jackknife Zone}, whereas the second one is often referred to 
  as \dQuote{Jackknife Replicate}. The number of distinct units in the PSU variable define the number of replications
  which are necessary due to the clustered structure. A design object is created and the appropriate \code{survey} 
  function is called. The process is repeated for each imputed dataset and the results of the analyses are pooled 
  according to Rubin (1987) or Rubin (2003).
  
  Without multiple imputations, the outer loop has only one cycle. Without a clustered sampling structure (i.e, in a 
  random sample), the inner loop has only one cycle. Without both, no replication is performed at all. To compute 
  simple mean estimates, for example, \code{eatRep} then simply calls \code{mean} instead of \code{svymean} from 
  the \code{survey} package. A special case occurs with nested multiple imputation. We then have three loops in a 
  nested structure. Hence, the corresponding analyses may take considerably computational effort. 
}
\details{
\tabular{ll}{
Package: \tab eatRep\cr
Type: \tab Package\cr
Version: \tab 0.6.3\cr
Date: \tab 2014-08-24\cr
License: \tab GPL(>=2)
}
}
\author{
    Author/maintainer: Sebastian Weirich <sebastian.weirich@iqb.hu-berlin.de>
}
\references{
  Foy, P., Galia , J. & Li, I. (2008). Scaling the data from the TIMSS 2007 mathematics
  and science asssessment. In J. F. Olson, M. O. Martin & I. V. S. Mullis (ed.),
  \emph{TIMSS 2007 Technical Report} (S. 225--280). Chestnut Hill, MA: TIMSS & PIRLS
  International Study Center, Lynch School of Education, Boston College.
  
  Harel, O. (2009): The estimation of \eqn{R^2} and adjusted \eqn{R^2} in incomplete data 
  sets using multiple imputation. \emph{Journal of Applied Statistics.} \bold{36, 10}, 1109--1118.

  Lumley, T. (2004). Analysis of complex survey samples. \emph{Journal of Statistical Software} \bold{9(1)}: 1--19

  Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys.} New York: Wiley.

  Rubin, D.B. (2003): Nested multiple imputation of NMES via partially incompatible MCMC.
  \emph{Statistica Neerlandica} \bold{57, 1}, 3--18.
  
  Rust, K., & Rao, JNK. (1996): Variance estimation for complex surveys using 
  replication techniques. \emph{Statistical Methods in Medical Research} \bold{5}, 283--310.

	Satorra, A., & Bentler, P. M. (1994). Corrections to test statistics
		and standard errors in covariance structure analysis.

  Westat (2000). \emph{WesVar.} Rockville, MD: Westat.

  Wolter, K. M. (1985). \emph{Introduction to variance estimation.} New York: Springer.

}
\keyword{ package }
\seealso{
}
\examples{
}
