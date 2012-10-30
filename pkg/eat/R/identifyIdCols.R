####################################################################################################################
#
# IdentifyIdCols       
# liest ID-Spalten aus cqc-File        
# Subroutine von "read.cqc"
#
# Version: 	1.0.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change-Log
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#
####################################################################################################################

identifyIdCols <- function (cqc.file) {
    funVersion <- "1.0.0"
    
	if (!missing(cqc.file)) {
        syntax <- readLines(cqc.file)
    } else {
		stop(paste(funVersion, ": Found no cqc file.\n", sep = ""))
    }
	
    formatStatement <- syntax[grep("format|Format|FORMAT", syntax)]
    formatStatement <- gsub(";", "", formatStatement)
    formatStatement <- strsplit(formatStatement, " +")[[1]]
    
	options(warn = -1)
    numericEntrys <- sapply(formatStatement, function(ll) { as.numeric(substr(ll, 1, 1))})
    options(warn = 0)
    
	numericEntrys <- which(!is.na(numericEntrys))

	idCols <- as.numeric(lapply(numericEntrys, function(ii) { unlist(strsplit(formatStatement[ii], "-"))})[[1]])
    return(idCols)
}
