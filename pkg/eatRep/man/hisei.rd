
\name{hisei}

\docType{data}

\alias{hisei}

\title{Simulated imputed data in a large-scale assessment context}

\description{
This data set contains fictional scores of 4619 students of 3 countries in a long format. Each row
corresponds to one measure of one student in one imputation.
}

\usage{data(hisei)}

\format{'data.frame':   23095 obs. of  9 variables
  \describe{
    \item{idstud}{Student identifier}
    \item{wgtSTUD}{variable of individual student weights}
    \item{sex}{Examinee's sex}
    \item{country}{Country where the examinee is from.}
    \item{JKZone}{jackknifing zone}
    \item{JKrep}{replicate ID}
  	\item{income}{Mean month income.}
  	\item{hisei}{Indicator of the highest socio-economical income, divided into five distinct groups.}
  	\item{imputation}{The number of the plausible value whose value is given in the 'score' column.}
 }
}

\source{Simulated data}

%\references{
%}

\keyword{datasets}


