\name{get.shw}
\alias{get.shw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read ConQuest showfiles}
\description{Function reads Conquest showfiles and transforms them into an R list of data frames. }
\usage{
get.shw(file, dif.term, split.dif = TRUE, abs.dif.bound = 0.6,
    sig.dif.bound = 0.3, p.value = 0.9)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the ConQuest show file.
}
  \item{dif.term}{
%%     ~~Describe \code{file} here~~
Optional: Character string. Name of the term considered to be DIF-term. Must
match corresponding term in showfile.
}
  \item{split.dif}{
%%     ~~Describe \code{file} here~~
Logical: When TRUE, DIF-Parameter are only given for Reference group.
}
  \item{abs.dif.bound}{
%%     ~~Describe \code{file} here~~
When DIF-Parameter are evaluated, this specifies the critical value for absolute
DIF.
}
  \item{sig.dif.bound}{
%%     ~~Describe \code{file} here~~
When DIF-Parameter are evaluated, this specifies the critical value for confidence
interval DIF.
}
  \item{p.value}{
%%     ~~Describe \code{file} here~~
When DIF-Parameter are evaluated, this specifies the critical p-value for confidence interval DIF.
}
}
\details{
Funktion searches for \sQuote{TERM}-statements in Conquest showfile and reads the tables associated
with. If one statement, for example \code{"item*gender"} is specified to contain DIF analyses, the
Conquest output concerning this term is read in and some \emph{additional} analyses are conducted. To
compute the absolute DIF, item parameter estimates of the \code{"item*gender"} table are doubled.
Confidence intervalls for 90, 95 and 99 percent are computed via the standard error of item parameter
estimates in the \code{"item*gender"} table. If both criteria - absolute DIF exceeds \code{abs.dif.bound}
and the confidence intervall does not include \code{sig.dif.bound}, item is considered to have DIF.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, named by the \sQuote{TERM}-statements in Conquest showfile, plus additional
data frame named \code{"regression"} with regression coefficients when latent linear regression model
was specified in Conquest analysis, plus an additional data frame named \code{"cov.structure"} with
covariance and correlation matrix of latent dimensions (if uni-dimensional model is specified, the
variance of the latent dimension is given instead), plus an additional data frame named \code{"final.deviance"}
with information about the final deviance and the number of estimated parameters intended for model
comparison. If one term was specified as DIF-statement, the corresponding data frame is augmented with
additional columns for confidence intervals and indicators specifying significant DIF.
Each data frame corresponding to a \sQuote{TERM} statement contains following columns:
\item{No.}{Item number}
\item{item}{Name of item}
\item{ESTIMATE}{Estimated item difficulty}
\item{ERROR}{Standard error of the estimated item difficulty}
\item{MNSQ}{Item's outfit}
\item{CI}{The lower bound of the confidence interval if the item has an ideal outfit of 1. This is to check whether the empirical
outfit lies within or without the confidence interval of an item with hypothetically ideal fit.}
\item{CI.1}{The upper bound of the corresponding confidence interval}
\item{T}{The \emph{T} value concerning the outfit}
\item{MNSQ.1}{Item's infit}
\item{CI.2}{The lower bound of the confidence interval if the item has an ideal infit of 1. This is to check whether the empirical
infit lies within or without the confidence interval of an item with hypothetically ideal fit.}
\item{CI.3}{The upper bound of the corresponding confidence interval}
\item{T.1}{The \emph{T} value concerning the infit}
\item{filename}{Name of the file which was read in.}
When latent regression was specified, the last element of the returned list is a data frame with
regression coefficients, corresponding to the number of dimensions and the number of regressors.
Regressor names, regression coefficients and its standard errors are given for each dimension.
Rows represent the regressors, columns represent the latent dimension to which the regression is
fitted.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
