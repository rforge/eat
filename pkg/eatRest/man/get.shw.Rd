\name{get.shw}
\alias{get.shw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read ConQuest showfiles}
\description{Function reads Conquest showfiles and transforms them into a R list of data frames.}
\usage{
get.shw(file, dif.term = NULL, split.dif = TRUE, 
        abs.dif.bound = 0.64, sig.dif.bound = 0.43)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string of the Conquest showfile to be read in. 
}
  \item{dif.term}{
%%     ~~Describe \code{dif.term} here~~
Optional: Character string. Name of the term considered to be DIF-term. Must match corresponding term in showfile. 
}
  \item{split.dif}{
%%     ~~Describe \code{split.dif} here~~
Logical: When TRUE, DIF-Parameter are only given for Reference group. 
}
  \item{abs.dif.bound}{
%%     ~~Describe \code{abs.dif.bound} here~~
When DIF-Parameter are evaluated, this specifies the critical value for absolute DIF. 
}
  \item{sig.dif.bound}{
%%     ~~Describe \code{sig.dif.bound} here~~
When DIF-Parameter are evaluated, this specifies the critical value for confidence interval DIF. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Funktion searches for 'TERM'-statements in Conquest showfile and reads the tables associated with. 
If one statement is specified to contain DIF analyses, absolute DIF value is computed 2*[group-specific parameter].
Confidence intervalls for 90, 95 and 99 percent are computed via the standard error of specific parameters. 
If both criteria - absolute DIF exceeds \code{abs.dif.bound} and the confidence intervall does not include 
\code{sig.dif.bound}, item is considered to have DIF.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, named by the 'TERM'-statements in Conquest showfile, plus an additional data frame named \code{regression} 
with regression coefficients when latent linear regression model was specified in Conquest analysis, plus an additional
data frame named \code{cov.structure} with covariance and correlation matrix of latent dimensions. If uni-dimensional model is specified, 
the variance of the latent dimension is given instead.
If one term was specified as DIF-statement, the corresponding data frame is augmented with additional columns for
confidence intervals and indicators specifying significant DIF. 

Each data frame corresponding to a 'TERM' statement contains following columns:
 \item{item.nr}{Item number}
 \item{item}{Name of item}
 \item{ESTIMATE}{Estimated difficulty of item}
 \item{ERROR}{Standard error of estimated item difficulty}
 \item{outfit}{Item's 'Outfit'}
 \item{outfit.ci.lb}{Lower bound of the outfit confidence interval}
 \item{outfit.ci.ub}{Upper bound of the outfit confidence interval}
 \item{outfit.t}{T-value for outfit}
 \item{infit}{Items's 'Infit'}
 \item{infit.ci.lb}{Lower bound of the infit confidence interval}
 \item{infit.ci.ub}{Upper bound of the infit confidence interval}
 \item{infit.t}{T-value for infit}
 \item{abs.dif}{Only for DIF analysis. Absolute DIF, computed as 2*[group-specific parameter].}
 \item{ci.lb}{Lower bound confidence interval for specific significance level of 90, 95 or 99 percent.}
 \item{ci.ub}{Upper bound confidence interval for specific significance level of 90, 95 or 99 percent.}
 \item{sig}{Indicates whether the corresponding item matches both DIF criteria. See details.}

When latent regression was specified, the last element of the returned list is a data frame with regression coefficients,
corresponding to the number of dimensions and the number of regressors. Regressor names, regression coefficients and its standard errors
are given for each dimension. 

Rows represent the regressors, columns represent the latent dimension to which the regression is fitted. 
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
