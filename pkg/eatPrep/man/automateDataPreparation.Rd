\name{automateDataPreparation}
\alias{automateDataPreparation}
\title{automateDataPreparation
}
\description{
prepare datasets for \code{automateModels}
}
\usage{
automateDataPreparation( datList = NULL, inputList, path = NULL, loadSav,
checkData, mergeData, recodeData,	aggregateData, scoreData, writeSpss,
filedat = "zkddata.txt", filesps = "readZkdData.sps",
aggregatemissings = "use.default", rename = TRUE, recodedData = TRUE,
correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL,
missing.rule = list(mvi=0, mnr=0, mci=0, mbd=NA, mir=0, mbi=0))
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
required by \code{\link{loadSav}} (source of SPSS files) and
\code{\link{writeSpss}}.
Default is the current \R working directory.
}
  \item{loadSav}{
 logical (whether function \code{\link{loadSav}} shall be called).
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
\item{missing.rule}{
A list containing recode information for character missings required
by \code{\link{writeSpss}}. See 'References' for description of default values.}

  \item{aggregatemissings}{
A character string. Either \code{"use.default"} or \code{"seeInputList"}, if
pattern was specified in \code{inputList$aggrMiss}.
}
  \item{rename}{
logical. See \code{\link{aggregateData}}.
}
  \item{recodedData}{
logical. See \code{\link{aggregateData}}.
}
  \item{correctDigits}{
logical. See \code{\link{loadSav}}.
}
  \item{truncateSpaceChar}{
logical. See \code{\link{loadSav}}.
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
\code{inputList} is a list of data frames. It can be created either by ZKDaemon
via \code{\link{readDaemonXlsx}} or by
\code{\link{makeInputLists}}.
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
        path = "c:/temp/test_eat", loadSav = FALSE, checkData=TRUE,
        mergeData = TRUE, recodeData=TRUE, aggregateData=TRUE, scoreData= TRUE,
        writeSpss=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
