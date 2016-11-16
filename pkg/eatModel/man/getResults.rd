\name{getResults}
\alias{getResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collect all results from Conquest/TAM analysis into a common data frame}
\description{First the IRT model should be defined using \code{defineModel}. Afterwards,
call \code{runModel} with the argument returned by \code{defineModel} to start the estimation.
The last step then is to create a results frame using \code{getResults}. }
\usage{
getResults(runModelObj, overwrite = FALSE, Q3 = TRUE, q3theta = c("pv", "wle", "eap"), 
    omitFit = FALSE, omitRegr = FALSE, omitWle = FALSE, omitPV = FALSE, abs.dif.bound = 0.6, 
    sig.dif.bound = 0.3, p.value = 0.9, nplausible = NULL, ntheta = 2000, normal.approx = FALSE, 
    samp.regr = FALSE, theta.model=FALSE, np.adj=8)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{runModelObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{runModel}.
}
  \item{overwrite}{
%%     ~~Describe \code{file} here~~
Logical. Should result files be overwritten if exist?
}
  \item{Q3}{
%%     ~~Describe \code{file} here~~
Logical. Estimate the Q3 statistic according to Yen (1984)? Note: this is only 
possible for uni-dimensional models. If \code{software == "tam"}, Q3 statistic
is estimated using the \code{tam.modelfit} function. If \code{software == "Conquest"}, 
Q3 statistic is estimated using the \code{Q3} function from the \code{sirt}
package.
}
  \item{q3theta}{
%%     ~~Describe \code{file} here~~
Specify whether the Q3 statistic should be estimated using PVs, WLEs or EAPs 
as the theta variable.
}
  \item{omitFit}{
%%     ~~Describe \code{file} here~~
Logical. Should item fit values be included into the results?
}
  \item{omitRegr}{
%%     ~~Describe \code{file} here~~
Logical. Should regression parameters and their standard errors be included into the results?
}
  \item{omitWle}{
%%     ~~Describe \code{file} here~~
Logical. Should WLE estimates be included into the results?
}
  \item{omitPV}{
%%     ~~Describe \code{file} here~~
Logical. Should plausible values be included into the results?
}
  \item{abs.dif.bound}{
%%     ~~Describe \code{file} here~~
Applies only if DIF analyses are performed before. When DIF-Parameter are evaluated, 
this specifies the critical value for absolute DIF.
}
  \item{sig.dif.bound}{
%%     ~~Describe \code{file} here~~
Applies only if DIF analyses are performed before. When DIF-Parameter are evaluated, 
this specifies the critical value for confidence interval DIF.
}
  \item{p.value}{
%%     ~~Describe \code{file} here~~
Applies only if DIF analyses are performed before. When DIF-Parameter are evaluated, 
this specifies the critical p-value for confidence interval DIF.
}
 \item{nplausible}{
Applies only if \code{software = "tam"}: Number of plausible values to be drawn. Note: 
number of plausible values were already defined in \code{defineModel}, because 
Conquest needs to know the number of PVs prior to estimation. In \code{TAM}, it 
is possible to redefine the number of plausible values and overwrite the definition
that was given in \code{defineModel}.
}
\item{ntheta}{
Applies only if \code{software = "tam"}. Following description is borrowed from the help 
file of \code{tam.pv} from the \code{TAM} package: Number of ability nodes for 
plausible value imputation. Note that in this function ability nodes are simulated 
for the whole sample, not for every person (contrary to the software ConQuest).
}
\item{normal.approx}{
Applies only if \code{software = "tam"}. Following description is borrowed from the help 
file of \code{tam.pv} from the \code{TAM} package: An optional logical indicating 
whether the individual posterior distributions should be approximated by a normal 
distribution? The default is \code{FALSE}. In the case \code{normal.approx=TRUE}
(normal distribution approximation), the number of ability nodes \code{ntheta} can 
be substantially smaller than 2000, say 200 or 500. The normal approximation is 
implemented for unidimensional and multidimensional models.
}
\item{samp.regr}{
Applies only if \code{software = "tam"}. Following description is borrowed from the help 
file of \code{tam.pv} from the \code{TAM} package: An optional logical indicating 
whether regression coefficients should be fixed in the plausible value imputation or
also sampled from their posterior distribution? The default is \code{FALSE}. Sampled 
regression coefficients are obtained by nonparametric bootstrap.
}
\item{theta.model}{
Applies only if \code{software = "tam"}. Following description is borrowed from the help 
file of \code{tam.pv} from the \code{TAM} package: Logical indicating whether the 
theta grid from the \code{tamobj} object should be used for plausible value
imputation. In case of \code{normal.approx=TRUE}, this should be sufficient in many 
applications.
}
\item{np.adj}{
Applies only if \code{software = "tam"}. Following description is borrowed from the help 
file of \code{tam.pv} from the \code{TAM} package: This parameter defines the 
``spread'' of the random theta values for drawing plausible values when 
\code{normal.approx=FALSE}. If \eqn{s_{EAP}} denotes the standard deviation 
of the posterior distribution of theta (in the one-dimensional case), then theta
is simulated from a normal distribution with standard deviation \code{np.adj} 
times \eqn{s_{EAP}}.
}
}
\details{
If \code{defineModel} was run with software Conquest, a path argument (\code{'dir'})
is necessary. The path argument is optional for software TAM. If \code{'dir'} was
specified, \code{getResults} additionally writes its output into the specified folder, 
using the \code{'analysis.name'} argument for file naming. Otherwise, \code{getResults} 
only returnes the result data frame. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame in the long format with ten columns.
\item{model}{The name of the model (as specified by the user in \code{analysis.name}.}
\item{source}{The estimation software (i.e, conquest or TAM) }
\item{var1}{The variable name for which the corresponding value is given, i.e. its indicator. }
\item{var2}{Additional variable information if necessary.}
\item{type}{Type of coefficient (for example, random or fixed).}
\item{indicator.group}{The type of the group the corresponding variable belongs to.}
\item{group}{The group the corresponding variable belongs to. Note: group is nested within \code{indicator.group}.}
\item{par}{The type of the parameter.}
\item{derived.par}{Optionally: The derived parameter.}
\item{value}{The value of the corresponding estimate.}
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
# see examples in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
