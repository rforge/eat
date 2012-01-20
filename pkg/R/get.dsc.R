####################################################################################################################
#
# get.dsc
# liest descriptives von ConQuest ein
#
#
# Version: 	1.2.0
# Imports:	reshape
# Published:
# depends: 	reshape
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
#	MH 14.10.2011: gestabled
#	MH 26.08.2011: auf stable wegen besserer sourcebarkeit
# SW 23.09.2011: erste "wirkliche" Stable-Version
# SW 14.10.2011: "trim" durch "crop" ersetzt
# SW 25.11.2011: 'cat' durch "sunk" ersetzt
#
####################################################################################################################

get.dsc <- function (file) {
    funVersion <- "get.dsc_1.2.0"
    input <- readLines(file)
    
	# find group names
	groupNameLines <- grep("Group: ", input)
    groupNames <- strsplit(input[groupNameLines], " ")
    groupNames <- sapply(groupNames, function(ll) { paste(ll[-1], collapse = " ")})
    nGroups <- length(groupNameLines)
    sunk(paste(funVersion, ": Found ", nGroups, " group(s) in ", file, ".\n", sep = ""))
    
	# find first and last lines for group inputs
	datStart <- grep("------------------", input)
    datStart <- datStart[seq(2, length(datStart), 2)] + 2
    separator <- grep("..................", input, fixed = TRUE)
    datEnd <- separator[seq(1, length(separator), 2)] - 1
    descStart <- separator[seq(1, length(separator), 2)] + 1
    descEnd <- separator[seq(2, length(separator), 2)] - 1
    stopifnot(length(datStart) == length(datEnd))
    
	# initialize output list
	outputList <- vector(nGroups, mode = "list")
    names(outputList) <- groupNames
    
	for (i in seq(along = groupNames)) {
		
		# read data per group
        groupDatInput <- crop(input[datStart[i]:datEnd[i]])
        groupDatInput <- strsplit(groupDatInput, " +")
        groupDatInput <- lapply(groupDatInput, FUN = function(iii) { c(paste(iii[1:(length(iii) - 4)], collapse = " "), iii[-c(1:(length(iii) - 4))]) })
        tempGroupDat <- do.call(rbind, groupDatInput)
        tempGroupDat[tempGroupDat == "NA"] <- NA
        options(warn = -1)
        groupDat <- data.frame(matrix(apply(tempGroupDat, 2, as.numeric), nrow = nrow(tempGroupDat), byrow = FALSE))
        characterCols <- which(colMeans(is.na(groupDat)) == 1)
        groupDat[, characterCols] <- tempGroupDat[, characterCols]
        groupDat <- data.frame(groupNames[i], groupDat, stringsAsFactors = F)
        colnames(groupDat) <- c("group.name", "dimension", "N", "mean", "std.dev", "variance")
		
		# read aggregated data per group
		groupDescInput <- crop(input[descStart[i]:descEnd[i]])
        groupDescInput <- strsplit(groupDescInput, " +")
        groupDescInput <- lapply(groupDescInput, FUN = function(iii) { c(paste(iii[1:(length(iii) - 3)], collapse = " "), iii[-c(1:(length(iii) - 3))]) })
        tempGroupDesc <- do.call(rbind, groupDescInput)
        tempGroupDesc[tempGroupDesc == "NA"] <- NA
        options(warn = -1)
        groupDesc <- data.frame(matrix(apply(tempGroupDesc, 2, as.numeric), nrow = nrow(tempGroupDesc), byrow = FALSE))
        characterCols <- which(colMeans(is.na(groupDesc)) == 1)
        groupDesc[, characterCols] <- tempGroupDesc[, characterCols]
        groupDesc <- data.frame(groupNames[i], groupDesc, stringsAsFactors = F)
        colnames(groupDesc) <- c("group.name", "dimension", "mean", "std.dev", "variance")
        dat.list <- list(single.values = groupDat, aggregates = groupDesc)
        outputList[[i]] <- dat.list
    }
	
    # find number of dimensions
	nDimensions <- names(table(sapply(outputList, function(ii) { length(grep("Error", ii$aggregates$dimension)) })))
    stopifnot(length(nDimensions) == 1)
    sunk(paste(funVersion, ": Found ", nDimensions, " dimension(s) in ", file, ".\n", sep = ""))
    
	return(outputList)
}
