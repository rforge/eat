\name{eatModel-package}
\alias{eatModel-package}
\docType{package}
\title{
  Specify and run IRT models from R (using Conquest or TAM).
}
\description{
  The software Conquest (Wu, Adams, Wilson, & Haldane, 2007) is a computer program for fitting
  item response and latent regression models. It is based on the Muldi-dimensional mixed-Coefficients
  Multinomial Logit Model, which is a generalized form of the Rasch Model (Adams & Wu, 2007). For
  example, Conquest allows for the estimation of the Rasch model, the rating scale model, the
  partial credit model, the linear logistic test model, multifacet models, multidimensional and
  latent regression models.
  
  Like Mplus, the interface of Conquest is command-line (cmd) based, where the syntax, the data and
  fixed effects indicator names (i.e., names of items) have to be provided in separated ASCII files.
  The package \code{eatModel} was created to allow for more fail-save, less cumbersome
  specification of IRT models in R, which subsequently can be estimated in Conquest. At the heart
  of the package are three functions, which build on each other and should be called consecutively:
  \itemize{
    \item \code{defineModel} is used to specify the model and the analysis software (e.g., Conquest or
          TAM) as well as the data. Several consistency checks are performed and all required input
          for the estimation software is prepared.
    \item \code{runModel} simply starts the \sQuote{estimator} (e.g. Conquest or TAM)
    \item \code{getResults} re-collects all created model output which is represented then in a
          single R data frame. The aim of the function is to provide the model output in a consistent 
          framework which is independent from the software used for analysis. Strictly speaking, 
          \code{getResults} is'nt necessary, i.e. the user is free to work with the output provided 
          by Conquest or TAM directly.
  }

  The tripartite process of model estimation works for one single specified model as well as for a 
  compilation of several models in only one call. To define the compilation of multiple models, use
  the function \code{splitModels}, which returns a list which may be assigned to the 'splittedModels'
  argument in \code{defineModel}. In that case, not only one but several models are defined, called and 
  evaluated. 
  
  Basically, \code{eatModel} is useful for Conquest analyses calling from R. For a short time the R package
  \code{TAM} allows to estimate parameters of the mixed-Coefficients Multinomial Logit Model solely in an
  R environment. Hence, \code{eatModel} allows all models to be estimated in \code{TAM} likewise.
  Please note that some model specifications (for example, 2PL/3PL) lead to models only TAM is able to 
  estimate. Conversely, some other model specifications (for example, partial credit models with 
  many categories or differential item functioning) might be unstable in TAM. 
  
  Historically, \code{eatModel} is a \sQuote{reboot} of the package \code{eatRest} formerly known as
  \code{eat}. The functionality of both packages is quite identical, whereas \code{eatModel} uses some
  more efficient routines, though. Important note: For consistency reasons, some functions in \code{eatModel} 
  have identical names as the corresponding function in \code{eatRest}, for example 'get.shw'. It is strongly
  recommended to \emph{not} have both packages attached simultaneously in one R session. 
}
\details{
\tabular{ll}{
Package: \tab eatModel\cr
Type: \tab Package\cr
Version: \tab 0.2.2\cr
Date: \tab 2015-09-17\cr
License: \tab GPL(>=2)
}
}
\author{
    Author/maintainer: Sebastian Weirich <sebastian.weirich@iqb.hu-berlin.de>
}
\references{
Adams, R. J., Wilson, M., & Wang, W.-C. (1997). The multidimensional random coefficients
multinomial logit model. \emph{Applied Psychological Measurement, 21}(1), 1-23.

Adams, R. J., & Wu, M. L. (2007). The Mixed-Coefficients Multinomial Logit Model:
A Generalized Form of the Rasch Model. In M. Von Davier & C. H. Carstensen (Eds.),
\emph{Multivariate and Mixture Distribution Rasch Models} (pp. 57-75). New York: Springer.

Kiefer, T., Robitzsch, A., & Wu, M. (2014). TAM: Test Analysis Modules. R package version 
1.0-1. http://CRAN.R-project.org/package=TAM

Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.

}
\keyword{ package }
\seealso{
}
\examples{
}
