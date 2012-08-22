\name{cat.pbc}
\alias{cat.pbc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cat.pbc
}
\description{
discrimination of categories (point-biserial correlation of categories with total score)
}
\usage{
cat.pbc (datRaw, datRec, idRaw, idRec, context.vars, values, subunits, xlsx = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datRaw}{
  unrecoded dataset
}
  \item{datRec}{
  the same datset as datRaw, in recoded form
}
  \item{idRaw}{
  name or number of id in unrecoded dataset
}
  \item{idRec}{
  name or number of id in recoded dataset
}
  \item{context.vars}{
  vector of names or numbers of context variables. No pbc will be calculated for these variables.
}

  \item{values}{
  input table values
}
  \item{subunits}{
  input table subunits
}
  \item{xlsx}{
  full path of excel to be written
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
returns a data.frame with category discrimination
}
\references{

}
\author{
Nicole Haag
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

%%\examples{
%%\dontrun{
%%}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
