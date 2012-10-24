\name{jk2.glm}
\alias{jk2.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{JK2 for linear regression models.}
\description{Compute Standard errors for linear regression models based on Jackknife (JK2) procedure.}
\usage{
jk2.glm(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), independent = list(), dependent = list(),
        complete.permutation = c("nothing", "groups", "independent", "all") , glm.family) }
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
  \item{independent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more independent variables. If independent variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{dependent}{
%%     ~~Describe \code{sig.dif.bound} here~~
List of one or more dependent variables. Each dependent variable will result in a separate analysis.
If dependent variable is a multiple imputation variable, all names concerning one variable are interpreted
as its imputations. See details for more information.
}
  \item{complete.permutation}{
%%     ~~Describe \code{sig.dif.bound} here~~
Becomes relevant if the numbers of imputations used for group variable(s) differs from the number of
imputation used for independent and/or dependent variable(s).
}
  \item{glm.family}{
%%     ~~Describe \code{sig.dif.bound} here~~
Argument of class "family", specifying the link function. See help file of 'glm' for details.
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
