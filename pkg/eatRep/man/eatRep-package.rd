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
  involves the formulas provided by Rubin (2003). Both methods are implemented in the package.

  Moreover, computing standard errors for the mean of a variable which stems from a clustered design may involve
  replication methods like balanced repeated replicate (BRR), bootstrap or Jackknife methods.
  See Weststat (2000), Foy, Galia & Li, 2008, and Wolter, 1985 for details. To date, only the Jackknife-2
  procedure (JK2) is supported.

  The package \code{'eatRep'} is designed to combine both methods which is necessary if (multiple) imputed
  data are used in clustered designs. Considering the structure is relevant especially for the estimation of
  standard errors.

  Technically, \code{'eatRep'} is a wrapper for the \code{'survey'} package (Lumley, 2004). Each function in
  \code{'eatRep'} corresponds to a specific function in \code{'survey'} which is called repeatedly during the analysis.
  The process is often similar: Each \code{'eatRep'} function first create replicate weights based on JKzone and JKrep
  variables according to the JK2 procedure. According to multiple imputed data sets, a workbook with several analyses
  is created. Without multiple imputations, the workbook only contains one analysis. For each entry in the workbook,
  a design object is created and the appropriate \code{'survey'} function is called. If multiple imputed or nested
  imputed data are analyzed, the results of the workbook analyses are pooled according to
  Rubin (1987) or Rubin (2003).

}
\details{
\tabular{ll}{
Package: \tab eatRep\cr
Type: \tab Package\cr
Version: \tab 0.4.4\cr
Date: \tab 2014-01-08\cr
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

  Lumley, T. (2004). Analysis of complex survey samples. \emph{Journal of Statistical Software} \bold{9(1)}: 1--19

  Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys.} New York: Wiley.

  Rubin, D.B. (2003): Nested multiple imputation of NMES via partially incompatible MCMC.
  \emph{Statistica Neerlandica} \bold{57, 1}, 3--18.

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
