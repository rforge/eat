\name{get.lmer.effects}
\alias{get.lmer.effects}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extract results
}
\description{
extract results from an object created by lmer from the lme4 package
}
\usage{
get.lmer.effects ( lmerObj , bootMerObj = NULL, conf = .95, saveData = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lmerObj}{bla}
  \item{bootMerObj}{bla}
  \item{conf}{bla}
  \item{saveData}{bla}
}
%% \details{}
\value{
returns an object of class \code{"eatGot"}
}
%% \references{}
\author{
Sebastian Weirich
}
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\section{Warning}{
This version is alpha. Use with care.
} 

\seealso{
\code{\link{eatGot-class}}
}

\examples{
library ( lme4 )
fmVA <- glmer( r2 ~ Anger + Gender + btype + situ + (1|id) + (1|item), family = binomial, data = VerbAgg)
( results <- get.lmer.effects ( fmVA ) )
}
