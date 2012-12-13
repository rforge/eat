\name{checkDesign}
\alias{checkDesign}
\title{Check Datasets for test design deviations}
\description{Check data frames according to test design for valid codes instead of expected sysMis and for sysMis instead of valid codes. }
\usage{checkDesign (dat, booklets, blocks, rotation, sysMis="NA", id="ID", subunits = NULL)}
\arguments{
  \item{dat}{A data frame}
  \item{booklets}{A data frame with booklet information. See 'Examples'.}
  \item{blocks}{A data frame with block information. See 'Examples'.}
  \item{rotation}{A data frame with rotation information. See 'Examples'.}
   \item{sysMis}{sysMis identifier as character. Default is "NA".}
  \item{id}{Case-id identifier as character. Default is "ID".}
  	\item{subunits}{
	Optional: A data frame with subunit information (c.f. \code{\link{inputList}}). See 'Details'. Used to find names of recoded subunits. }
}
%% \details{
%% tbd
%% }

\author{Karoline Sachse}
\note{
%%  ~~further notes~~
}

\examples{
data(inputDat)
data(inputList)

prepDat <- automateDataPreparation (inputList = inputList, datList = inputDat,
        loadSav = FALSE, checkData=FALSE, mergeData = TRUE, recodeData=TRUE, 
		aggregateData=FALSE, scoreData= FALSE, writeSpss=FALSE)
		
checkDesign(dat = prepDat, booklets = inputList$booklets, blocks = inputList$blocks, rotation = inputList$rotation, sysMis = "mbd", id="ID", subunits = inputList$subunits)

prepDat[1:7,"I22R"] <- "1"
prepDat[1,c("I01R","I02R","I03R")] <- "mbd"

checkDesign(dat = prepDat, booklets = inputList$booklets, blocks = inputList$blocks, rotation = inputList$rotation, sysMis = "mbd", id="ID", subunits = inputList$subunits)

}

%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
