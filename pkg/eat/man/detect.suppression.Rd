\name{detect.suppression}
\alias{detect.suppression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
detect suppression effects in regression models
}
\description{
This function detects suppression effects in regression models. 
}
\usage{
detect.suppression ( dat , dependent , independent , full.return = FALSE , xlsx.path = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{ data.frame with data to be used }
  \item{dependent}{ dependent variable in regression model }
  \item{independent}{ character vector of independent variables in regression model }
  \item{full.return}{ if \code{FALSE} a data.frame as a quadratic matrix with suppression effects (TRUE/FALSE) of independent variables is returned

					  if \code{TRUE} a data.frame with all calculated terms ist returned }
  \item{xlsx.path}{ full path of Excel file that results should be written to }	

}
\details{
formulae (13.39a) and (13.39b) decribed in Bortz (1999) page 446 are used

if \code{full.return=TRUE} a data.frame is returned.

Columns are:

rownames: <dependent variable> ~ <independent variables> | <independent variable that is tested for suppression>

multiple.reg: logical, indicates wether there are 2 (FALSE) or more than 2 (TRUE) independent variables in the regression model

dep: dependent variabel in regression model

pred: independent variable that is investigated on suppression effect

preds: independent variables in regression model besides \code{pred}

cor_pred_c: correlation of \code{pred} and dependent variable

cor_pred_fitted_c: correlation of predicted \code{pred} by indepenent variables and dependent variable

r.sq_pred: R squared from model predicting \code{pred} by independent variables

rterm.minus: right term in formula (13.39a)

rterm.plus: right termn in formula (13.39b)

rterm.minus.diff: difference of \code{rterm.minus} and \code{cor_pred_c}

rterm.plus.diff: difference of \code{cor_pred_c} and \code{rterm.plus}

(positive difference of \code{rterm.minus.diff} or \code{rterm.plus.diff} indicates suppression effect)

rterm.minus.log: logical value of formula (13.39a)

rterm.plus.log: logical value of formula (13.39b)

suppression: logical, \code{rterm.minus.log} | \code{rterm.plus.log}



if \code{full.return=FALSE} a data.frame as quadratic matrix is returned:

rows and columns are independent variables

diagonal includes \code{suppression} for suppression effect of variable in multiple regression

triangles include \code{suppression} for bivariate independent variables, "row" suppresses "column"

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
depends on options \code{full.return}
}
\references{
for formulae used by \code{detect.suppression} see 

Bortz, J. (1999). Statistik fuer Sozialwissenschaftler. 5. Auflage. Berlin: Springer. p. 446
}
\author{
Martin Hecht
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
