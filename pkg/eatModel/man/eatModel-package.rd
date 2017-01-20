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
  of the package are several functions, which build on each other and should be called consecutively:
  \itemize{
    \item \code{splitModels} is only necessary if the user wants to estimate several IRT models in 
          a row, using only one function call. This might be useful, if, for example, one model 
          should be fitted separately in various groups, for example the federal states in Germany. 
          If only one model should be defined and estimated, calling \code{splitModels} may be skipped.
          Start directly with \code{defineModel} instead. 
    \item \code{defineModel} is used to specify the model and the analysis software (e.g., Conquest or
          TAM) as well as the data. Several consistency checks are performed and all required input
          for the estimation software is prepared.
    \item \code{runModel} needs the output generated by \code{defineModel} and simply starts the 
          \sQuote{estimator} (e.g. Conquest or TAM)
    \item \code{getResults} needs the output generated by \code{runModel} and re-collects all created 
          model output which is represented then in a single R data frame. The aim of the function is 
          to provide the model output in a consistent framework which is independent from the software 
          used for analysis. Strictly speaking, \code{getResults} is'nt necessary, i.e. the user is free 
          to work with the output provided by Conquest or TAM directly.
    \item \code{equat1pl} needs the output generated by \code{getResults}. It provides equating 
          in a one-parameter (1pl) context, accounting for linking DIF and a clustered structure of
          items (items nested in testlets). In the multiple model case (i.e., if \code{splitModels} 
          was called), the linking is executed for all defined models simultaneously. 
    \item \code{transformToBista} needs the output generated by \code{equat1pl}. It provides 
          transformation of item and person parameters to an arbitrary scale (for example the
          ``PISA''-metric or the metric of the german educational standards. 
    \item \code{prepRep} needs the output generated by \code{transformToBista}. The funtion
          prepares the output for further (trend) analyses using the \code{eatRep} package. 
  }

  The multi-stage process of model estimation works for one single model as well as for a compilation 
  of several models in only one call. The estimation of these models may be accelerated using multicore 
  processing. Depending on the number of available logical CPUs, several models may be estimated 
  simultaneously. See the examples of \code{splitModels} for further details. The help page of
  \code{defineModel} includes a variety of examples which are derived from the context of the
  IQB ``Laendervergleich''.
  
  Basically, \code{eatModel} is useful for Conquest analyses which are called from R. Recently, the R package
  \code{TAM} allows to estimate parameters of the mixed-Coefficients Multinomial Logit Model solely in an
  R environment. Hence, \code{eatModel} allows all models to be estimated in \code{TAM} likewise.
  Please note that some model specifications (for example, 2PL/3PL) lead to models only TAM is able to 
  estimate. Conversely, some other model specifications (for example, partial credit models with 
  many categories or differential item functioning) might be unstable in TAM. 
  
  Historically, \code{eatModel} is a \sQuote{reboot} of the package \code{eatRest} formerly known as
  \code{eat}. The development of the \code{eat} package started in autumn 2010 at the Institute of
  Educational Progress. In 2012, the functionality of the package was partitioned into several 
  small ``sub-packages''---one of them is the \code{eatModel} package. The functionality is
  closely related to \code{eatRest} whose development was expired. Important note: For consistency 
  reasons, some functions in \code{eatModel} have identical names as the corresponding function in 
  \code{eatRest}, for example `get.shw'. It is strongly  recommended to \emph{not} have both packages 
  attached simultaneously in one R session. 
}
\details{
\tabular{ll}{
Package: \tab eatModel\cr
Type: \tab Package\cr
Version: \tab 0.5.5\cr
Date: \tab 2017-01-20\cr
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

Kiefer, T., Robitzsch, A., & Wu, M. (2016). TAM: Test Analysis Modules. R package version 
1.995-0. http://CRAN.R-project.org/package=TAM

Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.

}
\keyword{ package }
\seealso{
}
\examples{
}
