\name{inputDat}
\alias{inputDat}
\docType{data}
\title{List of Three Datasets from Educational Assessment
}
\description{ 
Simulated data for three booklets for an educational assessment study.
}
\usage{data(inputDat)}
\format{
This list contains 3 data frames, each with the following columns:
\describe{
\item{ID}{Person-ID}
\item{Hisei}{A continuous covariate.}
\item{Ixx}{Item responses to a selection of 30 test items.}
}
}
\details{code, subunit and unit descriptions are stored in dataset \code{\link{inputList}}.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(inputDat)
str(inputDat)
}
\keyword{datasets}
