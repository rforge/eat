\name{eatTrend}
\alias{eatTrend}
\title{Calculate National Trends in International Large-Scale Assessments}
\description{
This function processes LSA data and returns trend estimates in latent variable 
means and proficiency level frequencies.
}
\usage{eatTrend(itParsIntT1, PVsT1, countriesT1, itParsNatT1=NULL, 
jkzoneT1=NULL, jkrepT1=NULL, weightsT1=NULL, groupsT1=NULL, itParsIntT2, PVsT2, 
countriesT2, itParsNatT2=NULL, weightsT2=NULL, jkzoneT2=NULL, jkrepT2=NULL, 
groupsT2=NULL, GES=TRUE, testletNam = NULL, transfTo500=TRUE, mtT=500, sdtT=100, 
mRefPop=NULL, sdRefPop=NULL, cutScores=NULL, type =c("FCIP", "MM"), 
writeCsv=FALSE, path=NULL, plots=FALSE, backwards=FALSE)}
\arguments{
  \item{itParsIntT1}{
  A data frame containing the international item parameter estimates at time 1. 
  The first column has to be an ID variable, the second column contains the estimates.
}
  \item{PVsT1}{
   A data frame containing plausible values for all persons at time 1.
   The first column has to be an ID variable, the following columns contain 
   plausible values.
}
  \item{countriesT1}{
  A vector containing the group membership for every person at time 1. The order 
  has to correspond to the ID column in \code{PVsT1}.
}
  \item{itParsNatT1}{
   A list of data frames. The lists' names should correspond to the groups specified
   in \code{countriesT1}. The first column of every data frame has to be an ID 
   variable, the second column contains the national item parameter estimates.
}
  \item{jkzoneT1}{
  A vector containing the primary sampling unit at time 1. The order 
  has to correspond to the ID column in \code{PVsT1}. If empty a nonsene default
  will be set.
}
  \item{jkrepT1}{
  A vector containing the jackknife replicate ID at time 1. The order 
  has to correspond to the ID column in \code{PVsT1}. If empty a nonsene default
  will be set.
}
  \item{weightsT1}{
A vector containing the case weights at time 1. The order 
  has to correspond to the ID column in \code{PVsT1}.
}
  \item{groupsT1}{
A vector or a data.frame indicating subgroup-membership ("1") or subgroup-
nonmembership ("0") at time 1.
}
  \item{itParsIntT2}{
  A data frame containing the international item parameter estimates at time 2. 
  The first column has to be an ID variable, the second column contains the 
  estimates. Please assure that the item parameters stem from an analysis in 
  which the person parameters were centered around zero or that an equivalent 
  adjustment has taken place.
}
  \item{PVsT2}{
   A data frame containing plausible values for all persons at time 2.
   The first column has to be an ID variable, the following columns contain 
   plausible values. Please note that the columns should be centered around zero.
}
  \item{countriesT2}{
  A vector containing the group membership for every person at time 2. The order 
  has to correspond to the ID column in \code{PVsT2}.
}
  \item{itParsNatT2}{
   A list of data frames. The lists' names should correspond to the groups specified
   in \code{countriesT2}. The first column of every data frame has to be an ID 
   variable, the second column contains the national item parameter estimates.
}
  \item{jkzoneT2}{
  A vector containing the primary sampling unit at time 2. The order 
  has to correspond to the ID column in \code{PVsT2}. If empty a nonsene default
  will be set.
}
  \item{jkrepT2}{
  A vector containing the jackknife replicate ID at time 2. The order 
  has to correspond to the ID column in \code{PVsT2}. If empty a nonsene default
  will be set.
}
  \item{weightsT2}{
  A vector containing the case weights at time 2. The order 
  has to correspond to the ID column in \code{PVsT2}. 
}
  \item{groupsT2}{
A vector or a data.frame indicating subgroup-membership ("1") or subgroup-
nonmembership ("0") at time 2.
}
  \item{GES}{
Logical. If \code{TRUE}, trends for the whole sample will be computed.
}
  \item{testletNam}{
  Character vector containing the names of the units/testlets in which items are
  clustered.
}
  \item{mtT}{
Numeric. Mean of arbitrary metric to which parameters are transformed.
}
  \item{sdtT}{
Numeric. Standard deviation of arbitrary metric to which parameters are transformed.
}
  \item{mRefPop}{
Numeric. The mean of the reference population.
}
  \item{sdRefPop}{
Numeric. The standard deviation in the reference population.
}
  \item{transfTo500}{
Logical. If \code{TRUE}, the function \code{\link{transformTo500}} will be called.
}
  \item{cutScores}{
Named vector indicating the proficiency level cut scores in descending(!) order.
If is not \code{NULL}, the function \code{\link{addCuts}} will be called.
}
  \item{type}{
Character. If \code{MM}, all linking is done via mean-mean-equating (recommended
for diagnostic purposes). If \code{FCIP}, international and national item parameters
will be treated as the same at every time point.
}
  \item{writeCsv}{
Logical. If \code{TRUE}, .csv files will be written.
}
  \item{path}{
A character string containing the path for the output files.
}
 \item{plots}{
Logical. If \code{TRUE}, country DIF will be plotted.
}
 \item{backwards}{
Logical. IQB internal option to transform cutScores internally.
}
}

\value{
A list with two data frames: Countrywise trends in means and proficiency levels.
}

\author{
Karoline Sachse
}

\examples{
}
