####################################################################################################################
#
# get.itn
# liest Conquest-Outputfiles (*.itn) als R-Objekte ein
#
# Version: 	1.2.0
# Depends: gdata
# Imports: gdata
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
# Change log:
# 25.11.2011: 'cat' duch 'eatTools:::sunk' ersetzt
# 14.10.2011 MH: gestabled
# * zu 1.0.1 (2011-10-05, NH): bugfix: Funktion stürzt nicht mehr ab, wenn ein Item nur eine Antwortkategorie hat
# * zu 1.0.2 (2011-10-14, SW): "trim" durch "crop" ersetzt
#
####################################################################################################################


get.itn <- function (file) {
    funVersion <- "get.itn_1.2.0"
    allInput <- readLines(file)

	# find item names and first and last lines for each item
	itemNameLines <- grep("item:", allInput)
    itemStart <- itemNameLines + 7
    itemEnd <- grep("==========", allInput)
    itemEnd <- itemEnd[-c(1:2, length(itemEnd))] - 1

	# find DIF statements if possible	
	isDIF <- mean(sapply(strsplit(allInput[itemNameLines], ""), function(ll) { sum(ll == ":") })) != 1
    if (isDIF == FALSE) {
        itemNames <- gsub("(.*)\\((.*)\\)(.*|[[:space:]]*)", "\\2", allInput[itemNameLines])
    } else {
        itemNames <- gsub("(.*)\\((.*)\\)(.*|[[:space:]]*)\\((.*)\\)(.*|[[:space:]]*)", "\\4", allInput[itemNameLines])
        difName <- sapply(strsplit(allInput[itemNameLines], ":"), "[[", 1)
        difValue <- crop(gsub("(.*)\\((.*)\\)(.*|[[:space:]]*)\\((.*)\\)(.*|[[:space:]]*)", "\\2", allInput[itemNameLines]))
    }
	
	allOutput <- vector(length (itemNames), mode = "list")
	names (allOutput) <- itemNames
	
	# read data for each item
	for (i in seq(along = itemNameLines)) {
        if (itemStart[i] == itemEnd[i]) {
            eatTools:::sunk(paste(funVersion, ": Found only one category for item ", i, ".\n"))
        }
        itemInput <- allInput[itemStart[i]:itemEnd[i]]
        itemInput <- gsub("_BIG_ ", "NA", itemInput)
        if (length(table(sapply(strsplit(itemInput, " +"), length))) > 1) {
            itemInput <- gsub("          ", "    NA    ", itemInput)
        }
        itemInput <- gsub("\\)", "", itemInput)
        itemInput <- gsub("\\(", " ", itemInput)
		    itemInput <- strsplit(crop(itemInput), " +")
        itemOutput <- do.call(rbind, itemInput)
        itemOutput[itemOutput == "NA"] <- NA
        itemOutput <- data.frame(matrix(apply(itemOutput, 2, as.numeric), ncol = ncol(itemOutput) ) )
		
    		PVs <- unlist(gregexpr( "PV1Avg", allInput[itemStart[i] - 2]))
    		nPVs <- length(PVs)
    		colnames(itemOutput) <- c("Label", "Score", "Abs.Freq", 
                "Rel.Freq", "pt.bis", "t.value", "p.value", 
			   paste(rep(c("PV1.Avg.", "PV1.SD."), nPVs), rep(seq(along = PVs), each = 2), sep = ""))
        
		    pbc <- as.numeric(lapply(strsplit(allInput[itemNameLines[i] + 1], "^.*[Discrimination][[:space:]]*"), "[", 2))
        threshold <- as.numeric(lapply(strsplit(allInput[itemNameLines[i] + 2], " +"), "[[", 3))
        delta <- as.numeric(lapply(strsplit(allInput[itemNameLines[i] + 3], " +"), "[[", 3))
        
		p <- NA
        valid.p <- which(is.na(itemOutput$Score))
        if (length(valid.p) == 0) {
            p <- itemOutput[which(itemOutput$Score == max(itemOutput$Score)), "Abs.Freq"]/sum(itemOutput$Abs.Freq)
        }
 
		allOutput [[i]] <- data.frame(item.nr = i, item.name = itemNames[i], itemOutput[, 1:2], 
							n.valid = sum(itemOutput$Abs.Freq), itemOutput[, 3:4], 
							p = p, itemOutput[, -c(1:4)], pbc, threshold, delta, stringsAsFactors = F)					
		if (isDIF == TRUE) {
			allOutput [[i]] <- data.frame(dif.name = difName[i], dif.value = difValue[i], allOutput[[i]], stringsAsFactors = FALSE)
		}
  }
	allOutput <- do.call ( rbind, allOutput)	
	
    return(allOutput)
}
