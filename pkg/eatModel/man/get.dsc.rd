\name{get.dsc}
\alias{get.dsc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Conquest \sQuote{descriptives} output files.}
\description{Reads Conquest files with descriptive statistics for the estimated latent variables
generated by the \sQuote{descriptives} statement. }
\usage{
get.dsc(file)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string with the name of the Conquest descriptives file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A named list of n elements with n being the number of groups for which descriptive statistics were
computed. The names of the list are the group names. Each list contains the following elements:
\item{single.values}{A data frame containing the group name, dimension names, the number of observations,
mean, standard deviation and variance for each of the latent dimensions.
If the file contains descriptive statistics for plausible values, the number of rows
in the data frame corresponds to the number of plausible values.}
\item{aggregates}{A data frame containing the group name, dimension names and aggregated
statistics for the mean, standard deviation and variance for each of the latent
dimensions as well as (in a separate row) their standard errors.}
}
\references{
See pp. 162 of Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest
Version 2.0. Generalised Item Response Modeling Software.} Camberwell, Victoria: ACER Press.
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