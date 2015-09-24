\name{checkDesign}
\alias{checkDesign}
\title{Check Datasets for Deviations From Test Design}
\description{
This function checks whether a data frame corresponds to a particular rotated block design, i.e. whether all persons have valid codes on all items they were presented with and one consistent missing code for all items they were not presented with.}
\usage{checkDesign(dat, booklets, blocks, rotation, sysMis="NA", id="ID", 
    subunits = NULL, verbose = TRUE)}
\arguments{
  \item{dat}{A data frame}
  \item{booklets}{A data frame containing information about the number of blocks in each booklet and the names of these blocks. See \sQuote{Examples}.}
  \item{blocks}{A data frame containing the names of subunits and their order within each block. See \sQuote{Examples}.}
  \item{rotation}{A data frame containing information about which participant worked on which booklet. See \sQuote{Examples}.}
   \item{sysMis}{A character vector of length 1 indicating the missing code for items that were not administered to a participant. Default is "NA".}
  \item{id}{A character vector of length 1 indicating the name of the participant identifier variable in \code{dat}. Default is \code{ID}.}
  	\item{subunits}{
	Optional: A data frame with subunit information (c.f. \code{\link{inputList}}). This data frame is used to identify the names of recoded subunits. }
	  \item{verbose}{logical. If \code{FALSE}, no information is printed.
}
}
%% \details{
%% tbd
%% }

\author{Karoline Sachse}

\examples{
data(inputDat)
data(inputList)

prepDat <- automateDataPreparation (inputList = inputList, datList = inputDat,
    readSpss = FALSE, checkData = FALSE, mergeData = TRUE, recodeData = TRUE, 
    aggregateData = FALSE, scoreData = FALSE, writeSpss = FALSE, verbose = TRUE)
		
checkDesign(dat = prepDat, booklets = inputList$booklets, blocks = inputList$blocks, 
    rotation = inputList$rotation, sysMis = "mbd", id="ID", 
    subunits = inputList$subunits, verbose = TRUE)

prepDat[1:7,"I22R"] <- "1"
prepDat[1,c("I01R","I02R","I03R")] <- "mbd"

checkDesign(dat = prepDat, booklets = inputList$booklets, blocks = inputList$blocks, 
    rotation = inputList$rotation, sysMis = "mbd", id="ID", 
    subunits = inputList$subunits, verbose = TRUE)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
