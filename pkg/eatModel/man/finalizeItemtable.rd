\name{finalizeItemtable}
\alias{finalizeItemtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts the item parameter table necessary for the german ``Vergleichsarbeiten''.}
\description{Function only converts the item parameter table gained from the IQB data base into the desired 
format for delivery to the ``Evaluating Institution''.}
\usage{finalizeItemtable  ( tab, mainTest = 2017 )}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{The item parameter table in the IQB database format.
}
  \item{mainTest}{
%%     ~~Describe \code{dif.term} here~~
Integer: year of the main test. 
}
}
\details{
The item parameter table received from the the IQB database format is an xlsx file. Hence, this file should be 
loaded vis the \code{read.xlsx} function of the \code{xlsx} package. Afterwards, the table may be converted 
using \code{finalizeItemtable}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data.frame containing the converted table.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
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
# in a first step, the xlsx table generated from the IQB database, is load via "read.xlsx"
# it's important to tell "read.xlsx" which is the first row, i.e. the "header" row. Use the 
# "startRow" argument for this purpose. 
library(xlsx)
iqbTab <-read.xlsx2("r:/Englisch/main/21_VERA8_2017/VERA8_2017_Final/08_Itemkennwertetabelle/V8-ENG-2017_Itemkennwerte_2016-12-15.xlsx", 
         sheetName="Daten", as.data.frame=TRUE, header=TRUE, startRow = 4, stringsAsFactors=FALSE)
         
# convert the table 
convTab <- finalizeItemtable(iqbTab)

# save the table in the desired format (without rownames)
write.csv2(convTab, "N:/temp/table.csv", na="", row.names = FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
