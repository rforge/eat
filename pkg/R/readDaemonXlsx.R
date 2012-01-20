# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# readDaemonXlsx
# Description: automates data preparation
# Version: 	0.3.0
# Status: 
# Release Date: 	2011-11-22 
# Author:    Karoline Sachse
# Change Log:
# 2011-11-25 KS
# CHANGED: sheets 1-3 are compulsory, 4-10 optional in readDaemonXlsx
# 0000-00-00 AA
# 2011-11-22 (KS): Fkt erstellt
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# function:
# 		readDaemonXlsx ( inputDat, inputList )
#
# description:
#		read .xlsx from ZKDaemon
#
# arguments:
#		filename: A character string containing path, name and extension of .xlsx from ZKDaemon
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



readDaemonXlsx <- function(filename) {
	#source ( "p:/ZKD/source.it.all.R" )
	#source ( "p:/ZKD/ZKDaemon/zkdHelpers_0.4.2.R"  )
	#zkdHelpers_LoadPackage ( "xlsx" )
	inputList <- list()
	for(i in 1:3) {
		inputList[[i]] <- read.xlsx2(filename, i, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList[[i]] <- data.frame(lapply(inputList[[i]], as.character), stringsAsFactors=FALSE)
	}
	names(inputList) <- c("units", "subunits", "values") #, "unitRecodings", "savFiles", "newID")
	stopifnot(all(c("unit", "unitType", "unitAggregateRule", "unitScoreRule") %in% colnames(inputList$units)))
	stopifnot(all(c("unit", "subunit", "subunitRecoded") %in% colnames(inputList$subunits)))
	stopifnot(all(c("subunit", "value", "valueRecode", "valueType") %in% colnames(inputList$values)))
	if(inherits(try (read.xlsx2(filename, 4)), "try-error")) {
		sunk("No .xlsx sheet unitRecodings available at sheet position 4. InputList will be created without unitRecodings.\n")
		} else {
		inputList$unitRecodings <- read.xlsx2(filename, 4, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$unitRecodings <- data.frame(lapply(inputList$unitRecodings, as.character), stringsAsFactors=FALSE)
		stopifnot(all(c("unit", "value", "valueRecode", "valueType") %in% colnames(inputList$unitRecodings)))
	}
	if(inherits(try (read.xlsx2(filename, 5)), "try-error")) {
		sunk("No .xlsx sheet savFiles available at sheet position 5. InputList will be created without this sheet.\n")
		} else {
		inputList$savFiles <- read.xlsx2(filename, 5, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$savFiles <- data.frame(lapply(inputList$savFiles, as.character), stringsAsFactors=FALSE)
		stopifnot(all(c("filename", "case.id") %in% colnames(inputList$savFiles)))
	}
	if(inherits(try (read.xlsx2(filename, 6)), "try-error")) {
		sunk("No .xlsx sheet newID available at sheet position 6. InputList will be created without this sheet.\n")
		} else {
		inputList$newID <- read.xlsx2(filename, 6, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$newID  <- data.frame(lapply(inputList$newID , as.character), stringsAsFactors=FALSE)
		stopifnot(all(c("key", "value") %in% colnames(inputList$newID )))
	}
	if(inherits(try (read.xlsx2(filename, 7)), "try-error")) {
		sunk("No .xlsx sheet aggregateMissings available at sheet position 7. InputList will be created without this sheet.\n")
		} else {
		inputList$aggrMiss <- read.xlsx2(filename, 7, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$aggrMiss  <- data.frame(lapply(inputList$aggrMiss, as.character), stringsAsFactors=FALSE)
		stopifnot(all(c("vc", "mbd") %in% colnames(inputList$aggrMiss)))
	}
	if(inherits(try (read.xlsx2(filename, 8)), "try-error")) {
		sunk("No .xlsx sheet itemProperties available at sheet position 8. InputList will be created without this sheet.\n")
		} else {
		inputList$itemProperties <- read.xlsx2(filename, 8, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$itemProperties  <- data.frame(lapply(inputList$itemProperties, as.character), stringsAsFactors=FALSE)
	}
	if(inherits(try (read.xlsx2(filename, 9)), "try-error")) {
		sunk("No .xlsx sheet itemPropertyLabels available at sheet position 9. InputList will be created without this sheet.\n")
		} else {
		inputList$itemPropertyLabels <- read.xlsx2(filename, 9, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$itemPropertyLabels  <- data.frame(lapply(inputList$itemPropertyLabels, as.character), stringsAsFactors=FALSE)
	}
	if(inherits(try (read.xlsx2(filename, 10)), "try-error")) {
		sunk("No .xlsx sheet booklets available at sheet position 10. InputList will be created without this sheet.\n")
		} else {
		inputList$booklets <- read.xlsx2(filename, 10, sheetName=NULL, startRow=1,
			startColumn=1, noRows=NULL, noColumns=NULL, as.data.frame=TRUE,
			header=TRUE, colClasses="character")
		inputList$booklets  <- data.frame(lapply(inputList$booklets, as.character), stringsAsFactors=FALSE)
		stopifnot(all(c("id", "subunitsequence") %in% colnames(inputList$booklets)))
	}
	return(inputList)
}