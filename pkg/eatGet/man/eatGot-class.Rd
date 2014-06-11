\name{eatGot-class}
\Rdversion{1.1}
\docType{class}
\alias{eatGot-class}
\alias{show,eatGot-method}

\title{Class \code{"eatGot"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
eatGot objects are designed to include results from various model estimation procedures.

An eatGot object contains a data.frame in long format in slot "@results" with columns "model", "source", "var1", "var2", "type", "random.group", "par", "derived.par", and "value".

The concepts and termini behind eatGot objects are closely related to the mixed models framework. For instance, a central concept is to divide effects into "fixed" or "random".

The first column "model" contains the model name. This usually corresponds to the name of the object that is returned by the model estimation function.

The second column "source" indicates the function or software that was used for model estimation.

The third column "var1" contains the names of the variables that parameters were estimated for.

The fourth column "var2" contains the name of an additional variable in case the parameter of interest is a bivariate characterization (e.g. correlation). For non-bivariate parameters (e.g. fixed or random effects) it is defaulted to NA.

The fifth column "type" contains one of three entries: "fixed" for fixed effects, "random" for random effects, and "model" for parameters characterizing the model estimation procedure (e.g. model fit statistics).

The sixth column "random.group" indicates the units that the random effect is corresponding to.

The seventh column "par" contains the names of the parameters that are estimated. For fixed effects this simply might be "est", or some other label that includes hints to the estimation procedure (e.g. "eap", "PV").

Within the eat context the convention is to use "est" for fixed effects of items, "eap" for expected-a-posteriori person estimates, "PV" for plausible values, "se" for standard errors, "z" for a value of a normally distributed test statistic, "p" as the corresponding p value, "var" as the variance estimate of a random effect, "sd" as the standard deviation of a random effect, "correlation" for correlations of two variables, and various model parameters ("AIC", "BIC", "logLik"=Log-Likelihood, "deviance", "REMLdev").

The eighth column "derived.par" contains the names of parameters that are estimated by a second procedure after model estimation. For instance, plausible values are usually computed in a second step after model estimation.

The ninth column "value" contains the values of the parameters.

Since this classification scheme is not completely unambiguous and leaves some room for interpretation, the conventions for some popular cases within the eat context are:

[1] PVs

type="fixed"; par="PV"; derived.par="PV<no>" (e.g. "PV1", "PV2", ...)

[2] EAPs

type="fixed"; par="eap"; derived.par=NA

[3] bootstrapped values for original parameters

par="<name of original parameter>"; derived.par="Iteration<no>" (e.g. "Iteration1", "Iteration2" , ...)

[4] parameters derived from bootstrap procedure

derived.par="<name of derived parameter>" (e.g. "lb.90" for lower bound of 90% confidence interval, "bootSE" for bootstrapped standard error)

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eatGot")}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{results}:}{Object of class \code{"data.frame"}
	
	contains model results }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "eatGot")}: displays an object of class "eatGot" }
	 }
}
%% \author{
%% Martin Hecht, Sebastian Weirich, Karoline Sachse
%% }
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
This version is alpha. Use with care.
}

%%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%% \code{\link{defineDesign}}
%%}

\examples{
showClass("eatGot")
}
\keyword{classes}
