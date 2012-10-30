\name{zip2con}
\alias{zip2con}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
convert compressed file(s) to connection(s)
}
\description{
convert compressed file(s) to a list of connection(s)
}
\usage{
zip2con ( path , ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ either directory, file, list of directories, list of files, vector of directories or vector of files }
  \item{...}{ arguments passed to file handling function \link{bzfile}; if path is (list/vector of) directory/ies, arguments 'all.files', 'recursive', and 'ignore.case' can be passed to \link{list.files} that is used to get all compressed files from directory }
}
\details{
	function converts file(s) to connections by calling the appropriate uncompress function depending on file extension;
	currently supported are files with extensions "bz2" that are processed by \link{bzfile}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns list of connection(s);
names of list are file names (full path) without compression extension;
if \code{path} is already a connection it is returned as named list
if \code{path} is not processable \code{NULL} is returned, or (when multiple files are processed) it is not appended to return list
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

\examples{
\dontrun{
fl <- file.path( .Library , "eat/extdata/ConQuest.Log.Example1.log.bz2" )
zip2con ( fl )
zip2con ( bzfile ( fl ) )
zip2con ( list ( fl , fl ) )
zip2con ( c ( fl , fl ) )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
