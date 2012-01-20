\name{sunk}
\alias{sunk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sunk
}
\description{
writes output to file
}
\usage{
sunk ( cmd = NULL , path = NULL , write = TRUE , console.output = TRUE , new.file = FALSE , text.on.error = TRUE , text.out.method = NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmd}{
character string of element to write, may be either text (e.g. "write me to file") or a function call (e.g. "summary(lm)")
}
  \item{path}{
path (folder and name) to output file
if NULL path is defaulted to getwd()+"sunk.txt"
all environments are searched for sunk.path, if sunk.path is found (exists), it is used
}
  \item{write}{
logical, if TRUE (default) output is written to file
}
  \item{console.output}{
logical, if TRUE (default) output is displayed on console
}
  \item{new.file}{
logical, if TRUE the output file is created
if FALSE (default) output is appended to existing file
}
  \item{text.on.error}{
logical, sunk checks if the character string 'cmd' is an evaluatable expression
if TRUE (default), 'cmd' is treated as text if an error occurs when trying to evaluate string
if FALSE, sunk stops on errors/not evaluatable expressions
}
  \item{text.out.method}{
choose "cat" (default) or "print" as the output method for text
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
writes to disk
}
\references{

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
