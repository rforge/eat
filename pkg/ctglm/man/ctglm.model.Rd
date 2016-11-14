\name{ctglm.model}
\alias{ctglm.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ctglm model definition
}
\description{
define ctglm model
}
\usage{
ctglm.model ( d, id="id", time="time", person.var=c("b"=TRUE), track.person.par=NULL, Lambda=NULL, measurement.model=binomial(link="logit"), engine=c("jags","ctstan","ctsem"), ..., priors=NULL, verbose=TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{data frame}
  \item{id}{name of id column in data frame}
  \item{time}{name of time column in data frame}
  \item{person.var}{parameters that vary over persons}
  \item{track.person.par}{track person parameters (in jags/ctstan), may very much slow down estimation, possible values: c("theta","b")}
  \item{Lambda}{loading matrix of items and latent variables}
  \item{measurement.model}{measurement model}
  \item{engine}{software used for estimation}
  \item{...}{additional parameters}
  \item{priors}{priors for jags/ctstan}
  \item{verbose}{output messages to console}
}
%% \details{}
\value{
returns a ctglm model list
}
%% \references{}
\author{
Martin Hecht
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning}{
This version is alpha. Use with care.
} 

\seealso{
}

\examples{
}
