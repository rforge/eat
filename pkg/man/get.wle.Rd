\name{get.wle}
\alias{get.wle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads Conquest WLE or MLE files.}
\description{Reads Conquest files comprising maximum likelihood estimates (MLE) or weighted likelihood estimates (WLE).}
\usage{
get.wle(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
Character string of the Conquest MLE or WLE file to be read in.
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
A data frame with columns according to the corresponding MLE or WLE file. For each dimension of the analysis
number of solved items, number of presented items, point estimate and its standard error is given. Each row 
represents one person. Columns are named as follows: 

 \item{case}{Case number. Each row represents one person.} 
 \item{n.solved}{Number of solved items by the i-th person.}
 \item{n.total}{Number of total items presented to the i-th person.}
 \item{wle}{WLE or MLE estimate.}
 \item{wle.se}{Standard error of WLE or MLE estimate.} 

The last number of columns names represents the dimension the WLE or MLE estimate belongs to. 
 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
