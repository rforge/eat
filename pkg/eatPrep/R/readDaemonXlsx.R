# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# readDaemonXlsx
# Description: automates data preparation
# Version: 	0.4.0
# Status: 
# Release Date: 	2011-11-22 
# Author:    Karoline Sachse
# Change Log:
# 2012-10-05 KS
# CHANGED: adapt to Daemon
# Change Log:
# 2011-11-25 KS
# CHANGED: sheets 1-3 are compulsory, 4-10 optional in readDaemonXlsx
# 0000-00-00 AA
# 2011-11-22 (KS): Fkt erstellt
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# function:
# 		readDaemonXlsx ( inputDat, inL )
#
# description:
#		read .xlsx from ZKDaemon
#
# arguments:
#		filename: A character string containing path, name and extension of .xlsx from ZKDaemon
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



readDaemonXlsx <- function(filename) {

	inL <- list() 
	
	sheetNameVec <- c("units", "subunits", "values", "unitrecoding", "sav-files", "params", "aggregate-missings", "itemproperties", "propertylabels", "booklets", "blocks")
	
	for(pp in sheetNameVec) {
		if(inherits(try( inL[[pp]] <- read.xlsx2(filename, sheetName=pp, as.data.frame=TRUE, header=TRUE, colClasses="character", stringsAsFactors=FALSE), silent=TRUE)	, "try-error")) {
			cat(paste("No .xlsx sheet '", pp, "' available. InputList will be created without '", pp, "'.\n", sep = ""))
		} else {
			cat(paste("Reading sheet '", pp, "'.\n", sep = ""))
		}
	}
	fileS <- system.file("tests", "test_import.xlsx", package = "xlsx")
	res <- read.xlsx(fileS, 1) 
	
	if(!is.null(inL$units) & !all(c("unit", "unitType", "unitAggregateRule") %in% colnames(inL$units))) {
		cat("Something seems to be wrong with your 'units' sheet. Please check columns! \n")
	}
	if(!is.null(inL$subunits) & !all(c("unit", "subunit", "subunitRecoded") %in% colnames(inL$subunits))) {
		cat("Something seems to be wrong with your 'subunits' sheet. Please check columns! \n")
	}	
	if(!is.null(inL$values) & !all(c("subunit", "value", "valueRecode", "valueType") %in% colnames(inL$values))) {
		cat("Something seems to be wrong with your 'values' sheet. Please check columns! \n")
	}
	if(!is.null(inL$unitrecoding) & !all(c("unit", "value", "valueRecode") %in% colnames(inL$unitrecoding))) {
		cat("Something seems to be wrong with your 'unitrecoding' sheet. Please check columns! \n")
	}
	if(!is.null(inL[["sav-files"]]) & !all(c("filename", "case.id", "fullname") %in% colnames(inL[["sav-files"]]))) {
		cat("Something seems to be wrong with your 'sav-files' sheet. Please check columns! \n")
	}
	if(!is.null(inL$params) & !all(c("key", "value") %in% colnames(inL$params))) {
		cat("Something seems to be wrong with your 'params' sheet. Please check columns! \n")
	}
	if(!is.null(inL[["aggregate-missings"]]) & !all(c("vc", "mbd") %in% colnames(inL[["aggregate-missings"]]))) {
		cat("Something seems to be wrong with your 'aggregate-missings' sheet. Please check columns! \n")
	}
	if(!is.null(inL[["booklets"]]) & !all(c("booklet", "block1") %in% colnames(inL[["booklets"]]))) {
		cat("Something seems to be wrong with your 'booklets' sheet. Please check columns! \n")
	}
	if(!is.null(inL[["blocks"]]) & !all(c("subunit", "block") %in% colnames(inL[["blocks"]]))) {
		cat("Something seems to be wrong with your 'blocks' sheet. Please check columns! \n")
	}
	
	
	# paar ueberfluessige Umbenennungen für Folgefunktionen
	names(inL)[which(names(inL) == "sav-files")] <- "savFiles"
	names(inL)[which(names(inL) == "params")] <- "newID"
	names(inL)[which(names(inL) == "aggregate-missings")] <- "aggrMiss"
	names(inL)[which(names(inL) == "unitrecoding")] <- "unitRecodings"
	#cat("(Sheets savFiles, newID, aggrMiss, unitRecodings were renamed due to naming standards). \n")

	return(inL)
}