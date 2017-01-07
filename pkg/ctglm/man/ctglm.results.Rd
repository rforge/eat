\name{ctglm.results}
\alias{ctglm.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ctglm syntax/call creation
}
\description{
run ctglm model
}
\usage{
ctglm.results ( r, plot.dir=NULL, plot.person.par=FALSE, cores=detectCores(), verbose=TRUE, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{ctglm ran object (from ctglm.run)}
  \item{plot.dir}{plot directory (for jags/ctstan)}
  \item{plot.person.par}{plot person parameters}
  \item{cores}{number of cores}  
  \item{verbose}{output messages to console}
  \item{...}{additional parameters}
}
%% \details{}
\value{
returns a ctglm results list
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
