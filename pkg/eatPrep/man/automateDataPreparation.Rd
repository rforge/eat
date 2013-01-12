\name{automateDataPreparation}
\alias{automateDataPreparation}
\title{automateDataPreparation
}
\description{
prepare datasets for \code{automateModels}
}
\usage{
automateDataPreparation(datList = NULL, inputList, path = NULL, 
readSpss, checkData,  mergeData , recodeData, recodeMnr = FALSE,
aggregateData, scoreData, writeSpss,
mnrFunction = c( "recodeMbiToMnr" , "mnrCoding" ),
filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, nMbi = 2,
rotation.id = NULL, aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0), 
verbose = FALSE)
            }
\arguments{
  \item{datList}{
  a list of data frames (see \code{data(\link{inputDat})}). If \code{NULL} SPSS .sav files have to be read in 
  and \code{readSPSS} has to be \code{TRUE}.
}
  \item{inputList}{
  a list of data frames containing additional information (see \code{data(\link{inputList})}).
}
  \item{path}{
a character string containing the path required by \code{\link{readSpss}} (source of SPSS files) and
\code{\link{writeSpss}}.
Default is the current \R working directory.
}
  \item{readSpss}{
 logical. If \code{TRUE} function \code{\link{readSpss}} will be called.
}
  \item{checkData}{
logical. If \code{TRUE} function \code{\link{checkData}} will be called.
}
  \item{mergeData}{
logical. If \code{TRUE} function \code{\link{mergeData}} will be called.
}
  \item{recodeData}{
logical. If \code{TRUE} function \code{\link{recodeData}} will be called.
}
  \item{recodeMnr}{
logical. If \code{TRUE} function \code{\link{recodeMbiToMnr}} will be called.
}
  \item{aggregateData}{
logical. If \code{TRUE} function \code{\link{aggregateData}} will be called.
}
  \item{scoreData}{
logical. If \code{TRUE} function \code{\link{scoreData}} will be called.
}
  \item{writeSpss}{
logical. If \code{TRUE} function \code{\link{writeSpss}} will be called.
}
  \item{mnrFunction}{
character. Choose one of two functions for mnr coding, either "recodeMbiToMnr" (default) or "mnrCoding".
}
  \item{filedat}{
a character string containing the name of the output data file required
by \code{\link{writeSpss}}.
}
  \item{filesps}{
a character string containing the name of the output syntax file required
by \code{\link{writeSpss}}.
}
 \item{breaks}{
numeric vector (argument used by \code{\link{recodeMbiToMnr}}).
}
 \item{nMbi}{
numeric (argument used by \code{\link{recodeMbiToMnr}}).
}
 \item{rotation.id}{
character (argument used by \code{\link{recodeMbiToMnr}}).
}
\item{missing.rule}{
a list containing recode information for character missings required
by \code{\link{writeSpss}}. See 'References' for description of default values.}

  \item{aggregatemissings}{
a symmetrical \emph{n x n} matrix or a data frame from \code{inputList$aggrMiss} with information on how missing values should be aggregated. If no matrix is given, the default will be used. See 'Details' in \code{\link{aggregateData}}.
}
  \item{rename}{
logical. See \code{\link{aggregateData}}.
}
  \item{recodedData}{
logical. See \code{\link{aggregateData}}.
}
  \item{correctDigits}{
logical. See \code{\link{readSpss}}.
}
  \item{truncateSpaceChar}{
logical. See \code{\link{readSpss}}.
}
  \item{newID}{
a character string containing the case IDs name in the final data frame. Default
is \code{"ID"} or a character string specified in \code{inputList$newID}.
}
  \item{oldIDs}{
a vector of character strings containing the IDs names in the original datasets.
Default is as specified in \code{inputList$savFiles}.
}
 \item{verbose}{
logical. If \code{TRUE} progress and additional information is printed.
}

}
\details{
}
\value{
A data frame.
}
\references{http://code.google.com/p/zkdlib/wiki/MissingHandling}
\author{
Karoline Sachse
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(inputList)
data(inputDat)
preparedData <- automateDataPreparation(inputList = inputList, 
                datList = inputDat,	path = "c:/temp/test_eat", 
                readSpss = FALSE, checkData = TRUE,	mergeData = TRUE, 
                recodeData = TRUE, recodeMnr = TRUE, breaks = c(1,2),
                aggregateData = TRUE, scoreData = TRUE, 
                writeSpss = FALSE, verbose = TRUE)
}
