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
  of the package are several functions, which build on each other and should be called consecutively.
  Not every function is mandatory to be called---it depends on the desired analyses.
  \itemize{
    \item \code{splitModels} is only necessary if the user wants to estimate several IRT models in 
          a row, using only one function call. This might be useful, if, for example, one model 
          should be fitted separately in various person groups, for example the federal states in Germany,
          or if one model should be fitted for several domains, say listening comprehension and reading
          comprehension. Hence, the split may be defined according to person groups (deferal states, 
          for example), or item groups (domains) or both. If only one model should be defined and estimated, 
          calling \code{splitModels} is not necessary may be skipped. Start directly with \code{defineModel} 
          instead. Hence, you may think of \code{splitModels} as defining a loop, like \code{lapply} or 
          \code{by}. 
    \item \code{defineModel} is used to specify the model and the analysis software (e.g., Conquest or
          TAM) as well as the data. Several consistency checks are performed and all required input
          for the estimation software is prepared. If several models shopuld be estimated in a row, 
          \code{defineModel} needs the output from \code{splitModels}. 
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
  %%  \item Alternatively, \code{createVeraItemtabForDB} may be called which also needs the output 
  %%        generated by \code{transformToBista}. The function converts the item parameter table
  %%        into a format which is customized to match the guidelines of the ``Vergleichsarbeiten'' 
  %%        at the Institute of Educational Progress (IQB). The output of \code{createVeraItemtabForDB} 
  %%        directly can be imported to the IQB data base for further processing. 
  }

  The multi-stage process of model estimation works for one single model as well as for a compilation 
  of several models in only one call. The estimation of these models may be accelerated using multicore 
  processing. Depending on the number of available logical CPUs, several models may be estimated 
  simultaneously. See the examples of \code{splitModels} for further details. The help page of
  \code{defineModel} includes a variety of examples which are derived from the context of the
  IQB ``Laendervergleich''.
  
  Basically, \code{eatModel} is useful for Conquest analyses which are called from R. Alternatively, the R package
  \code{TAM} allows to estimate parameters of the mixed-Coefficients Multinomial Logit Model solely in an
  R environment. Hence, \code{eatModel} allows all models to be estimated in \code{TAM} likewise.
  Please note that some model specifications (for example, 2pl/3pl) lead to models only TAM is able to 
  estimate. Conversely, some other model specifications (for example, partial credit models with 
  many categories or differential item functioning) might be unstable in TAM. 
  
  Historically, \code{eatModel} is a \sQuote{reboot} of the package \code{eatRest} formerly known as
  \code{eat}. The development of the \code{eat} package started in autumn 2010 at the Institute of
  Educational Progress (IQB). In 2012, the functionality of the package was partitioned into several 
  small ``sub packages''---by name \code{eatPrep} for data preparation, \code{eatTools} for several
  auxiliary functions, \code{eatRest} for Rasch modeling, \code{eatRep} for replication methods,
  \code{eatDesign} for definition and modification of design properties. The further development 
  of \code{eatRest} was expired because the package turned out to be enigmaticly written which 
  leads to undebugable problems. Thus, \code{eatModel} is the second attempt. Its functionality 
  is closely related to \code{eatRest} but adds some new features---for example, the support of 
  2pl models using the \code{TAM} package. Important note: For consistency reasons, some functions 
  in \code{eatModel} have identical names as the corresponding function in \code{eatRest}, for example 
  `get.shw'. It is strongly recommended to \emph{not} have both packages attached simultaneously in one R session.
}
\details{
\tabular{ll}{
Package: \tab eatModel\cr
Type: \tab Package\cr
Version: \tab 0.6.22\cr
Date: \tab 2018-10-11\cr
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

Robitzsch, A., Kiefer, T., & Wu, M. (2018). TAM: Test analysis modules. R package version
2.13-15. https://CRAN.R-project.org/package=TAM

Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.

}
\keyword{ package }
\seealso{
}
\examples{
}
