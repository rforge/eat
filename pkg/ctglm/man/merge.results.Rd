\name{merge.results}
\alias{merge.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ctglm syntax/call creation
}
\description{
run ctglm model
}
\usage{
merge.results ( ..., consistent=TRUE, noNAcol=TRUE, pattern="\\\\.\\\\b(Rdata|rda)\\\\b$", results.identifier=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{result data frames from ctglm.results() or folder with .Rdata files}
  \item{consistent}{only parameter (column variable) that are in all analyses will be kept}
  \item{noNAcol}{columns with NAs will be dropped}
  \item{pattern}{argument from list.files to identify Rdata files}
  \item{results.identifier}{how to identify results}
}
%% \details{}
\value{
returns a data frame
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
