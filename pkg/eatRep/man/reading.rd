
\name{reading}

\docType{data}

\alias{reading}

\title{Simulated nested imputed data for a fictional reading achievement test}

\description{
This data set contains fictional achievement scores of 4619 students of 3 countries in a long format. The data corresponds to
a nested multiple imputed structure. Each row corresponds to one measure (e.g. a plausible value) of one student in one nest.
}

\usage{data(reading)}

\format{'data.frame':   27714 obs. of  11 variables
  \describe{
    \item{idstud}{Student identifier}
    \item{wgtSTUD}{variable of individual student weights}
    \item{sex}{Examinee's sex}
    \item{country}{Country where the examinee is from.}
    \item{JKZone}{jackknifing zone}
    \item{JKrep}{replicate ID}
  	\item{income}{Mean month income.}
  	\item{imputation}{The number of the plausible value whose value is given in the 'score' column.}
  	\item{nest}{The number of the nest the value in the 'score' column belongs to.}
  	\item{score}{The value of the plausible value.}
  	\item{passed}{An indicator whether the value is above a certain threshold, i.e. whether the individual passed or failed the test.}
 }
}

\source{Simulated data}

%\references{
%}

\keyword{datasets}


