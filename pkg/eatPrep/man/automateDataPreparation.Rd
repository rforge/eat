\name{automateDataPreparation}
\alias{automateDataPreparation}
\title{Automate Data Preparation using Functions from Package eatPrep}
\description{
This function facilitates automated data preparation and wraps most functions from the \code{eatPrep} package.

}
\usage{automateDataPreparation(datList = NULL, inputList, path = NULL, 
    readSpss, checkData,  mergeData, recodeData, recodeMnr = FALSE,
    aggregateData, scoreData, writeSpss, collapseMissings = FALSE, 
    filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, 
    nMbi = 2, rotation.id = NULL, suppressErr = FALSE, recodeErr = "mci",
    aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
    correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
    missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0), 
    verbose=FALSE)}
\arguments{
  \item{datList}{
  A list of data frames (see \code{data(\link{inputDat})}). If \code{NULL}, \code{readSPSS} has to be \code{TRUE}. In this case, the function attempts to read SPSS .sav files.
}
  \item{inputList}{
  A list of data frames containing neccessary information for data preparaton (see \code{data(inputList)} for details).
}
  \item{path}{
A character vector containing the path required by \code{\link{readSpss}} (source of SPSS files) and \code{\link{writeSpss}}.
Default is the current \R working directory.
}
  \item{readSpss}{
 Logical: If \code{TRUE}, the function \code{\link{readSpss}} will be called.
}
  \item{checkData}{
Logical: If \code{TRUE}, the function \code{\link{checkData}} will be called.
}
  \item{mergeData}{
Logical: If \code{TRUE}, the function \code{\link{mergeData}} will be called.
}
  \item{recodeData}{
Logical: If \code{TRUE}, the function \code{\link{recodeData}} will be called.
}
  \item{recodeMnr}{
Logical: If \code{TRUE}, the function \code{\link{mnrCoding}} will be called.
}
  \item{aggregateData}{
Logical: If \code{TRUE}, the function \code{\link{aggregateData}} will be called.
}
  \item{scoreData}{
Logical: If \code{TRUE}, the function \code{\link{scoreData}} will be called.
}
  \item{collapseMissings}{
Logical: If \code{TRUE}, the function \code{\link{collapseMissings}} will be called and a data frame with recoded missing values according to argument \code{missing.rule} will be returned.
}
  \item{writeSpss}{
Logical: If \code{TRUE}, the function \code{\link{writeSpss}} will be called.
}
  \item{filedat}{
a character string containing the name of the output data file for \code{\link{writeSpss}}.
}
  \item{filesps}{
a character string containing the name of the output syntax file for \code{\link{writeSpss}}.
}
 \item{breaks}{
Numeric vector passed on to function \code{\link{mnrCoding}} containing the number of blocks after which \code{mbi} shall be recoded to \code{mnr}, e.g., \code{c(1,2)} to specify breaks after the first and second block.
numeric vector (argument used by ).
}
 \item{nMbi}{
Numeric vector of length 1 passed on to function \code{\link{mnrCoding}} containing the number of \code{mbi}-Codes required at the end of a block to code \code{mnr}. Needs to be >=1..
}
 \item{rotation.id}{

Character vector of length 1 passed on to function \code{\link{mnrCoding}} indicating the name of the rotation indicator (e.g. \dQuote{booklet}) in the dataset. 
}
 \item{suppressErr}{
Logical passed on to function \code{\link{aggregateData}} indicating whether aggregated cells with \code{err} should be recoded to another value..
}
 \item{recodeErr}{Character vector of length 1 passed on to function \code{\link{aggregateData}} indicating to which \code{err} should be recoded. This argument is only evaluated when \code{suppressErr = TRUE} }.
\item{missing.rule}{
A named list with definitions how to recode the different types of missings in the dataset. If \code{writeSPSS = TRUE}, missing values will be recoded to 0 or \code{NA} prior to writing the SPSS dataset. See \code{\link{collapseMissings}} for supported missng values.}

  \item{aggregatemissings}{
A symmetrical \emph{n x n} matrix or a data frame from \code{inputList$aggrMiss} passed on to function \code{\link{aggregateData}} with information on how missing values should be aggregated. If no matrix is given, the default will be used. See 'Details' in \code{\link{aggregateData}}.
}
  \item{rename}{Logical passed on to function \code{\link{aggregateData}} indicating whether units with only one subunit should be renamed to their unit name? Default is \code{FALSE}.}
  \item{recodedData}{Logical passed on to function \code{\link{aggregateData}}indicating whether colnames in \code{dat} are the subunit names (as in \code{subunits$subunit}) or recoded subunit names (as in \code{subunits$subunitRecoded}). Default is \code{TRUE}, meaning that colnames are recoded subitem names.}

  \item{correctDigits}{
logical. See \code{\link{readSpss}}.
}
  \item{truncateSpaceChar}{
logical. See \code{\link{readSpss}}.
}
  \item{newID}{
A character string containing the case IDs name in the final data frame. Default
is \code{ID} or a character string specified in \code{inputList$newID}.
}
  \item{oldIDs}{
A vector of character strings containing the IDs names in the original SPSS datasets.
Default is as specified in \code{inputList$savFiles}.
}
 \item{verbose}{
Logical: If \code{TRUE}, progress and additional information is printed.
}

}

\value{
A data frame resulting from the final data preparation step.
}

\author{
Karoline Sachse
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
