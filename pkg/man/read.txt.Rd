\name{read.txt}
\alias{read.txt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read text files
}
\description{
reads (compressed) text files with specific method
}
\usage{
read.txt ( path , read.function = c ( "readLines" , "read.table" , "read.csv" , "read.csv2" , "read.delim" , "read.delim2" ) , file.ext = NULL , ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ either directory, file, list of directories, list of files, vector of directories or vector of files }
  \item{read.function}{ function to read in txt files, \link{bzfile}, \link{read.table}, \link{read.csv}, \link{read.csv2}, \link{read.delim}, or \link{read.delim2} }
  \item{file.ext}{ character, extension of files that are to be read, extensions of compressed files are ignored, that means that \code{file.ext} refers to extension of uncompressed file and extension of compressed file without compression extension }
  \item{...}{ arguments passed to function used as \code{read.function}; if path is (list/vector of) directory/ies, arguments 'all.files', 'recursive', 'pattern' and 'ignore.case' can be passed to \link{list.files} that is used to get all compressed files from directory }
}
\details{
	reads text files with specific read function (\code{read.function}); compressed files are automatically uncompressed depending on their extension, see \link{zip2con} for supported compression types
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns list of read in files;
names of list are file names (full path) without compression extension;
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
lns <- read.txt ( fl )
lns <- read.txt ( bzfile ( fl ) )
lns <- read.txt ( list ( fl , fl ) )
lns <- read.txt ( c ( fl , fl ) )
str ( lns )
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
