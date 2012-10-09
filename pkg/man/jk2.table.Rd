\name{jk2.table}
\alias{jk2.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for frequency tables.}
\description{Compute Standard errors for frequency tables based on Jackknife (JK2) procedure.}
\usage{
jk2.table(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), dependent = list(),
          separate.missing.indikator = FALSE, expected.values = list(), complete.permutation = FALSE) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{file} here~~
Data frame containing all variables for analysis.
}
  \item{ID}{
%%     ~~Describe \code{dif.term} here~~
Variable name or column number of ID variable.
}
  \item{wgt}{
%%     ~~Describe \code{split.dif} here~~
Optional: Variable name or column number of weighting variable. If no weighting variable is specified,
all cases will be equally weighted.
}
  \item{JKZone}{
%%     ~~Describe \code{abs.dif.bound} here~~
Variable name or column number of variable indicating Jackknifing Zone.
}
  \item{JKrep}{
%%     ~~Describe \code{sig.dif.bound} here~~
Variable name or column number of variable indicating replicate ID.
}
  \item{group}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional: List of one or more grouping variables. If grouping variable is a multiple imputation variable,
all names concerning one variable are interpreted as its imputations. See details for more information.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more grouping variables. Each dependent variable will result in a separate analysis.
If grouping variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{separate.missing.indikator}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical.
}
  \item{expected.values}{
%%     ~~Describe \code{sig.dif.bound} here~~
Optional. A vector auf values expected in dependent variable. Recommend to left this argument empty.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Logical. Becomes relevant if the numbers of imputations used for group variable differs from the number of
imputation used for dependent variable(s).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Funktion first creates replicate weights based on JKZone and JKrep variables according to JK2 procedure
implemented in WesVar. Afterwards, function serves as a wrapper for several functions implemented in the
'survey' package.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list of data frames, one for each dependent variable. Each data frame contains means, standard deviations and
standard errors for means.
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
