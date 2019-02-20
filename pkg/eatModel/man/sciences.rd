
\name{sciences}

\docType{data}

\alias{sciences}

\title{Science achievement test data obtained from large-scale assessment.}

\description{
This data set contains fictional achievement scores of 420 students in three subjects (biology, chemistry, physics)
and two domains (procedural, knowledge) in the long format.
}

\usage{data(sciences)}

\format{'data.frame':   21954 obs. of  15 variables
  \describe{
    \item{id}{Student identifier}
    \item{year}{Year of evaluation}
    \item{wgt}{Individual student case weight}
    \item{jkzone}{jackknifing zone (jk2) }
    \item{jkrep}{jackknife replicate}
    \item{country}{The country an examinee stems from}
    \item{grade}{grade of the students}
    \item{sex}{student's sex}
    \item{booklet}{number of the booklet the student is provided with}
    \item{track}{academic track of the student}
    \item{version}{booklet version}
  	\item{subject}{school subject}
  	\item{domain}{The domain the variable belongs to}
  	\item{variable}{variable identifier}
  	\item{value}{The response of the student to the item (0=incorrect; 1=correct)}
 }
}

\source{Simulated data}

%\references{
%}

\keyword{datasets}


