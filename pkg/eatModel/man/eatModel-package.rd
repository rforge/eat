\name{eatModel-package}
\alias{eatModel-package}
\docType{package}
\title{
  Specify and run IRT models from R (using Conquest or TAM).
}
\description{
  The software Conquest (Wu, Adams, Wilson, & Haldane, 2007) is a computer program for fitting
  item response and latent regression models. It is based on the Muldi-dimensional mixed-Coefficients
  Multinomial Logit Model, which is a Generalized Form of the Rasch Model (Adams & Wu, 2007). For
  example, Conquest allows for the estimation of the Rasch model, the rating scale model, the
  partial credit model, the linear logistic test model, multifacet models, multidimensional and
  latent regression models.
  
  Like Mplus, the interface of Conquest is command-line (cmd) based, where the syntax, the data and
  fixed effects indicator names (i.e., names of items) have to be saved in separated ASCII files.
  Hence, the package \code{eatModel} was created to allow for more fail-save, less cumbersome
  specification of IRT models in R, which subsequently can be estimated in Conquest. At the heart
  of the package are three functions, which build on each other and should be called consecutively:
  \itemize{
    \item \code{defineModel} is used to specify the model and the analysis software (e.g., Conquest or
          TAM) as well as the data. Several consistency checks are performed and all required input
          for the estimation software is prepared.
    \item \code{runModel} simply starts the \sQuote{estimator} (e.g. Conquest or TAM)
    \item \code{retResults} is to re-collect all created model output which is represented then in a
          single R data frame.
  }

  Basically, \code{eatmodel} is useful for Conquest analyses calling from R. Recently the R package
  \code{TAM} allows to estimate parameters of the mixed-Coefficients Multinomial Logit Model in a
  solely R environment. Hence, \code{eatModel} allows all models to be estimated in \code{TAM} likewise.
  
  Historically, \code{eatModel} is a \sQuote{reboot} of the package \code{eatRest} formerly known as
  \code{eat}. The funtionality of both packages is quite identical, whereas \code{eatModel} uses some
  more efficient routines, though.
}
\details{
\tabular{ll}{
Package: \tab eatModel\cr
Type: \tab Package\cr
Version: \tab 0.0.10\cr
Date: \tab 2014-08-25\cr
License: \tab GPL(>=2)
}
}
\author{
    Author/maintainer: Sebastian Weirich <sebastian.weirich@iqb.hu-berlin.de>
}
\references{
Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.

Adams, R. J., Wilson, M., & Wang, W.-C. (1997). The multidimensional random coefficients
multinomial logit model. \emph{Applied Psychological Measurement, 21}(1), 1-23.

Adams, R. J., & Wu, M. L. (2007). The Mixed-Coefficients Multinomial Logit Model:
A Generalized Form of the Rasch Model. In M. Von Davier & C. H. Carstensen (Eds.),
\emph{Multivariate and Mixture Distribution Rasch Models} (pp. 57-75). New York: Springer.

}
\keyword{ package }
\seealso{
}
\examples{
}
