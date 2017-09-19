\name{finalizeItemtable}
\alias{finalizeItemtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creates item parameter table for the ``Vergleichsarbeiten'' from IQB database input}
\description{Function transforms the Excel output of the IQB database into desired 
format for the ``Evaluating institutions''.}
\usage{
finalizeItemtable( xlsm, xml, mainTest = 2017, anhangCsv = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlsm}{
%%     ~~Describe \code{file} here~~
character string: the name of the xlsm file generated from IQB database to read.
}
  \item{xml}{
%%     ~~Describe \code{file} here~~
character string: the name of the xml file. Note: To date, the xml-file has to be 
generated manually. Open the xlsm file with Excel (version 2007 or newer), choose 
the ``Daten'' sheet and save it as a xml calculation table (2003 format).
}
  \item{mainTest}{
%%     ~~Describe \code{file} here~~
Specify the year of the main test the item parameter table is dedicated to. 
}
  \item{anhangCsv}{
%%     ~~Describe \code{file} here~~
character string: the name of the csv appendix file. This is only necessary to check
the values for consitency. 
}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame which may be saved into csv format. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
%% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# see example 5, 6, and 6a in the help file of defineModel()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
