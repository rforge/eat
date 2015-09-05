\name{mnrCoding}
\alias{mnrCoding}
\title{
Recode Missing by Intention to Missing not Reached
}
\description{
\code{mnrCoding} converts missing responses coded as missing by intention at the end of a block of items to missing not reached.
}

\usage{
	mnrCoding (dat, pid, rotation.id, blocks, booklets, breaks, subunits = NULL, nMbi = 2, mbiCode = "mbi", mnrCode = "mnr", invalidCodes = c("mbd", "mir", "mci"), verbose = FALSE)
}

\arguments{
	\item{dat}{
		A dataset. Missing by intention needs to be coded \code{mbi}.
	}
	\item{pid}{
		Name or column number of identifier (ID) variable in dataset.
	}
	\item{rotation.id}{
		Name of rotation indicator (e.g. "booklet") in \code{dat}.
	}	
	\item{blocks}{
		A data frame containing the sequence of subunits in each block in long format. The column names need to be \code{subunit, block, subunitBlockPosition}. 
	}
	\item{booklets}{
		A data frame containing the sequence of blocks in each booklet in wide format. The column names need to be \code{booklet, block1, block2, block3 ...}.
	}
%	\item{rotation}{
%		A data frame containing the assignment of booklets to participants. The first column should have %the same name as the ID variable in \code{dat} and the second column needs to be named\code{booklet}. 	
%	}	
	\item{breaks}{
		Number of blocks after which \code{mbi} shall be recoded to \code{mnr}, e.g., \code{c(1,2)} to specify breaks after the first and second block.
	}
	\item{subunits}{
	Optional: A data frame with subunit information if a dataset is used that has been recoded with \code{recodeData}. This data frame will be used to find the names of recoded subunits in \code{dat}. 
	}
	\item{nMbi}{
		Number of \code{mbi}-Codes required at the end of a block to code \code{mnr}. Needs to be >=1. 
	}   	
	\item{mbiCode}{
		Character indicating \code{mbi} (missing by intention) in dataset. 
	}  
	\item{mnrCode}{
		Character to which \code{mnr} has to be recoded.  
	}  
	\item{invalidCodes}{
		Character vector indicating missing codes to be ignored.  
	}  	
	\item{verbose}{
	logical. If \code{TRUE} additional diagnostics are printed.
	}
}
		
\details{
In order to code \code{mnr}, a certain number of subunits at the end of a block need to be coded \code{mbi}. This number can be specified with the argument \code{nMbi}. The default is 2, i.e. if the last and second to last subitem in a block are coded \code{mbi}, both subunits, as well as the preceding subunits coded \code{mbi}, will be recoded to \code{mnr}. If \code{nMbi} is larger than the number of subunits in a given block, no subitem in this block will be recoded. If all subunits in a block are coded \code{mbi}, none of them will be recoded to \code{mnr}.  

If a \code{subunits} data frame is specified, \code{recodeMbiToMnr} expects to find the recoded subunits in \code{dat}.

Examples for data frames \code{booklets}, \code{blocks}, \code{rotation} and \code{subunits} can be found via \code{data(\link{inputList})}
}
\value{
A data frame with missing not reached coded as \code{mnr}. For each person with at least one \code{mnr} in the returned dataset the names of recoded variables are given as an attribute to \code{dat}.
}



\examples{
data(inputDat)
data(inputList)

prepDat <- automateDataPreparation (inputList = inputList, datList = inputDat,
           readSpss = FALSE, checkData=FALSE, mergeData = TRUE, recodeData=TRUE, 
		   aggregateData=FALSE, scoreData= FALSE, writeSpss=FALSE, verbose = TRUE)
		   
prepDat2 <- mergeData("ID", list(prepDat, inputList$rotation))		   
			   
mnrDat <- mnrCoding (dat = prepDat2, pid = "ID", booklets = inputList$booklets, 
          blocks = inputList$blocks, rotation.id = "booklet", breaks = c(1, 2), subunits = inputList$subunits, nMbi = 2, mbiCode = "mbi", mnrCode = "mnr", invalidCodes = c("mbd", "mir", "mci"), verbose = TRUE)

}