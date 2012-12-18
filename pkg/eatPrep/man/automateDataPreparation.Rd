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
filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, nMbi = 2,
aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0))
            }
\arguments{
  \item{datList}{
  A list of data frames if no .sav files shall be read in.
}
  \item{inputList}{
  A list of data frames containing additional information (see Details).
}
  \item{path}{
A character string containing the path where the logfolder will be created. Also
required by \code{\link{readSpss}} (source of SPSS files) and
\code{\link{writeSpss}}.
Default is the current \R working directory.
}
  \item{readSpss}{
 logical (whether function \code{\link{readSpss}} shall be called).
}
  \item{checkData}{
logical (whether function \code{\link{checkData}} shall be called).
}
  \item{mergeData}{
logical (whether function \code{\link{mergeData}} shall be called).
}
  \item{recodeData}{
logical (whether function \code{\link{recodeData}} shall be called for subunits).
}
  \item{recodeMnr}{
logical (whether function \code{\link{recodeMbiToMnr}} shall be called for subunits).
}
  \item{aggregateData}{
logical (whether function \code{\link{aggregateData}} shall be called).
}
  \item{scoreData}{
logical (whether function \code{\link{recodeData}} shall be called for units).
}
  \item{writeSpss}{
logical (whether function \code{\link{writeSpss}} shall be called).
}
  \item{filedat}{
A character string with the name of the output data file required
by \code{\link{writeSpss}}.
}
  \item{filesps}{
A character string with the name of the output syntax file required
by \code{\link{writeSpss}}.
}
 \item{breaks}{
Numeric vector (argument used by \code{\link{recodeMbiToMnr}}).
}
 \item{nMbi}{
Numeric (argument used by \code{\link{recodeMbiToMnr}}).
}
\item{missing.rule}{
A list containing recode information for character missings required
by \code{\link{writeSpss}}. See 'References' for description of default values.}

  \item{aggregatemissings}{
A symmetrical \emph{n x n} matrix or a data frame from \code{inputList$aggrMiss} with information on how missing values should be aggregated. If no matrix is given, the default will be used. See 'Details' in \code{\link{aggregateData}}.
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
A character string containing the case IDs name in the final data frame. Default
is \code{"ID"} or a character string specified in
inputList sheet 6 (see \code{\link{readDaemonXlsx}}).
}
  \item{oldIDs}{
A vector of character strings containing the IDs names in the original datasets.
Default is as specified in \code{inputList$savFiles}.
}

}
\details{
\code{inputList} is a list of data frames. It can be created by ZKDaemon
via \code{\link{readDaemonXlsx}}.
 Compulsory: units, subunits, values.
 Optional: unitRecodings, savFiles, newID, aggregateMissings.
}
\value{
A single data frame in last transformation status.
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
test <- automateDataPreparation(inputList = inputList, datList = inputDat,
        path = "c:/temp/test_eat", readSpss = FALSE, checkData=TRUE,
        mergeData = TRUE, recodeData=TRUE, recodeMnr = TRUE, breaks = c(1,2),
		    aggregateData=TRUE, scoreData= TRUE, writeSpss=TRUE)
}
