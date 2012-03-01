####################################################################################################################
#
# getDimensionNames
# sucht Anzahl der Dimensionen und ggf. Dimensionsnamen in ConQuest-Files
#
#
# Version: 	1.1.0
# Imports:
# Published:
# depends: 
# Author:  
# Maintainer:
#
# Change Log:
# 14.10.2011 MH: gestabled
# *1.0.1 (2011-10-03, NH): parameter lab.file.only hinzugefügt: wenn TRUE = Namen werden nur im lab file gesucht
#							damit kann Funktion auch in get.plausible zur Benennung der Dimensionen genutzt werden.
#
####################################################################################################################

getDimensionNames <- function (lab.file, jobFolder, name.analyse, lab.file.only = FALSE) {
    funVersion <- "getDimensionNames_1.1.0"
	
	# find dimensions in lab-file
    if (is.character(lab.file)) {
        lab.file <- read.table(lab.file, header = T, sep = " ", colClasses = "character")
    }
    dimPosition <- which(lab.file[, 2] == "dimensions")
    if (length(dimPosition) > 0) {
        dimensions <- lab.file[(dimPosition + 1):nrow(lab.file), 2]
        dimensions <- data.frame(nDimension = seq(along = dimensions), dimensionNames = dimensions, stringsAsFactors = F)
    }
	
	# if no dimension names were found in lab-file: find dimensions in pvl-File	
	if (length(dimPosition) == 0) {			
		cat(paste(funVersion, ": No dimension names found in lab-file.\n",  sep = ""))
			
		if (lab.file.only == FALSE ) {
		
			# read pvl-file		
			input <- read.table(file.path(jobFolder, paste(name.analyse, ".pvl", sep = ""), sep = "", header = FALSE, fill = TRUE, stringsAsFactors = FALSE) )
			
			# find number of PVs per person
			nPersonPVs <- sum(input[-1, 1] == 1:(nrow(input) - 1))
			
			# find number of persons
			nPerson <- nrow(input)/(nPersonPVs + 3)
			
			# keep only rows which contain PVs
			isPVrow <- c ( FALSE, rep ( rep ( c(TRUE, FALSE), times = c ( nPersonPVs, 3 )), nPerson))
			isPVrow <- isPVrow [ - length(isPVrow) ]
			output <- input [ isPVrow, ]
			
			# find number of dimensions
			nDimensions <- ncol(output) - 1
			cat(paste("                         ", nDimensions, " dimensions identified.\n",  sep = ""))
			dimensions <- data.frame(nDimension = 1:nDimensions, dimensionNames = paste("dim.", 1:nDimensions, sep = ""), stringsAsFactors = F)	
		} else {
		# if no dimensions were found and lab.file.only = TRUE: return NULL
			dimensions <- NULL
		}
	}	
    return(dimensions)
}

getDimnamesFromLabfile <- function (lab.file) {
    funVersion <- "getDimnamesFromLabfile_0.1.0"
	
	# find dimensions in lab-file
    if (!is.character(lab.file)) {
	   stop(paste(funVersion,": 'lab.file' has to be of class 'character'.\n",sep=""))
	}
	lf <- scan(lab.file, what = "character", sep="\n", quiet=TRUE )
	
	dimPosition <- which ( grepl ( "^===>\\s+dimensions" , lf ) )
	if ( length(dimPosition) != 1 ) {
			dimensions <- NULL
	} else {
	
			endPosition <- which ( grepl ( "^===>" , lf[ dimPosition + 1 : length(lf)]) )[1]
			if ( is.na ( endPosition ) ) endPosition <- length(lf)
	
			lf2 <- lf [ (dimPosition+1):endPosition ]
			
			dimensions <- sapply ( strsplit ( lf2 , "\\s+" ) , "[" , 2 )
		
	}
	
	return(dimensions)
}