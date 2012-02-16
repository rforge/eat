####################################################################################################################
#
# get.shw
# liest Conquest-Outputfiles (*.shw) als R-Objekte ein
#
# Version: 	1.7.0
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
# 14.10.2011 MH: gestabled
# 1.2.1 (2011-10-03, NH): Benennung von Spalten in Output geändert, entsprechen jetzt Namen in ZKD-Ergebnisstruktur
# 1.2.0 (2011-10-01, NH): Bugfix: Regression jetzt auch für nur eine Dimension ohne Namen auslesbar
#						  Konfidenzintervallberechnung für DIF vereinfacht
#						  Benennung von Konfidenzintervallen in Output geändert in ci.lb.X bzw. ci.ub.X	
# 
# 28.09.2011 NH: verschönert, default für dif.term auf NULL gesetzt, parameter datei in file geändert
# 16.09.2011 MH: Regression auslesen, wird als ...$regression an return-Liste rangehangen
# 19.08.2011 SW: DIF, Konfidenzintervalle auf 99%, außerdem Grenzen für absoluten und sig. DIF frei variierbar
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 05.01.2011 NH: Kriterien für signifikanten DIF angepasst: 0.43 und 0.64 statt 0.3 und 0.6
# 14.10.2011 SW: "trim" durch "crop" ersetzt, depends no longer on "gdata"
# 25.11.2011 SW: "cat" durch "sunk" ersetzt
# 28.11.2011 SW: Fehler wenn alle Items verankert sind also also kein einziger Standardfehler bestimmt wird,
#                fuehrte zu falscher Benennung der Spalten: gefixed ... 
#
####################################################################################################################

### dif.term			... Name des DIF-terms (character), muss nur angegeben werden, wenn DIF-Analysen eingelesen werden sollen.
### split.dif			... sollen bei DIF-Analysen nur die Werte der ersten Gruppe zurückgegeben werden?
### abs.dif.bound       ... positive Zahl; Schwellenwert für absoluten DIF
### sig.dif.bound       ... positive Zahl; Schwellenwert für Signifikanzkriterium 


## Hilfsfunktion zur Berechnung von DIF-Konfidenzintervallen
confidence <- function ( par , error, alpha ) {				
	z.alpha <- abs(qnorm(alpha/2))
	ci.lower <- par - 2 * z.alpha * error 
	ci.upper <- par + 2 * z.alpha * error 
	ci <- data.frame ( ci.lower, ci.upper )
	colnames (ci) <- c ( paste ( "ci.lb", 100 - 100 * alpha, sep = "."), paste ( "ci.ub", 100 - 100 * alpha, sep = "."))
	return (ci)
}


get.shw <- function (file, dif.term = NULL, split.dif = TRUE, abs.dif.bound = 0.64, sig.dif.bound = 0.43) {
    funVersion <- "get.shw_1.7.0"
    allInput <- readLines(file)

    # find terms
	termNameLines <- grep("TERM", allInput)
    nTerms <- length(termNameLines)
    if (nTerms == 0) {
        sunk(paste(funVersion, ": No TERM-statement found in file ", file, ".\n", sep = ""))
        stop()
    }
	termNames <- strsplit(allInput[termNameLines], ":")
    termNames <- crop(sapply(termNames, "[[", 2))
    
	# find first and last line for each term
	termStart <- termNameLines + 6
    termEnd <- grep("An asterisk", allInput)[1:nTerms]
    termEnd <- termEnd - 2
    sunk(paste(funVersion, ": Found ", nTerms, " term(s): ", paste(termNames, collapse = ", "), "\n", sep = ""))
    
	# initialize output list
	outputList <- vector(nTerms, mode = "list")
    names(outputList) <- termNames
    
	# read data for each term
	for (i in seq(along = termNames)) {
        termLines <- seq(termStart[i], termEnd[i])
        outputColNamesLine <- termStart[i] - 2
        outputColNames <- strsplit(allInput[outputColNamesLine], " +")
        outputColNames <- c("item.nr", "item.name", outputColNames[[1]][-c(1:2)])
        outputColNames <- gsub("\\^", "", outputColNames)
        outputColNames <- outputColNames[sort(c(seq(along = outputColNames), grep("CI", outputColNames)))]
		
		# name columns according to zkd conventions
		mnsqCols <- grep("MNSQ", outputColNames)
		if (length(mnsqCols) == 2) {
			outputColNames[mnsqCols] <- c ( "outfit", "infit" )
			outputColNames[mnsqCols + 3] <- c ( "outfit.t", "infit.t" )
		}
		
		ciCols <- grep("CI", outputColNames)
		if (length(ciCols) == 4 ) {
			outputColNames[ciCols] <- c ("outfit.ci.lb", "outfit.ci.ub", "infit.ci.lb", "infit.ci.ub")
		}
		
		# read term input
        termInput <- crop(allInput[termLines])
        termInput <- gsub("\\*    ", "  NA", termInput)
        substituteString <- paste(c("\\(", ")", ","), collapse = "|")
        termInput <- gsub(substituteString, " ", termInput)
		
		# find missing values and replace them with NA
		termOutput <- strsplit(termInput, " +")
        nInputCols <- sapply(termOutput, length)
        correctInputLines <- which(nInputCols == max(nInputCols))
        missingInputLines <- which(nInputCols != max(nInputCols))
        lastSpace <- gregexpr(" [[:graph:]]", termInput[correctInputLines])[[1]]
        posEstimateCol <- regexpr("ESTIMATE", allInput[outputColNamesLine])
        lastSpace <- lastSpace[lastSpace > (posEstimateCol - 3)]
        for (ii in seq(along = missingInputLines)) {
            for (iii in seq(along = lastSpace)) {
                isSpace <- substr(termInput[missingInputLines[ii]], lastSpace[iii] + 2, lastSpace[iii] + 2) == " "
                if (isSpace) {
                  termInput[missingInputLines[ii]] <- paste(substr(termInput[missingInputLines[ii]], 1, lastSpace[iii]), 
				    "NA", substring(termInput[missingInputLines[ii]], lastSpace[iii] + 3), sep = "")
                }
            }
        }
        termOutput <- strsplit(termInput, " +")
        maxNInputCols <- max(nInputCols)
		
		# check number of output colnames and number of input columns
        if (maxNInputCols < length(outputColNames)) {
            sunk(paste(funVersion, ": Several columns empty for term '", termNames[i], "' in file: '", file, "'. Outputfile may be corrupted. Please check!\n", sep = ""))
            maxNInputCols <- length(outputColNames)
        }
		if (maxNInputCols > length(outputColNames)) {
            if (maxNInputCols == length(outputColNames) + 1) {
				if ( !is.null(dif.term) )  {
				   if( termNames [i] != dif.term ) {
					   sunk(paste(funVersion, ": Found one more column than column names. Expect missing column name before 'ESTIMATE'. Check outputfile for term '",
						termNames[i], "' in file: '", file, "'. \n", sep = ""))
					}	
                }
				ind.name <- which(outputColNames == "ESTIMATE")
                outputColNames <- c(outputColNames[1:ind.name - 1], "add.column", outputColNames[ind.name:length(outputColNames)])
            }
            if (maxNInputCols > length(outputColNames) + 1) {
                sunk(paste(funVersion, ": Found more columns than column names. Check outputfile for term '", termNames[i], "' in file: '", file, "'. \n", sep = ""))
                outputColNames <- c(outputColNames, rep("add.column", maxNInputCols - length(outputColNames)))
            }
        }
		
		# make output data frame
        tempTermOutput <- do.call(rbind, lapply( termOutput, FUN=function(ii) { gsub("\\*","",ii)}   ))
        tempTermOutput[tempTermOutput == "NA"] <- NA
        options(warn = -1)
        termOutput <- data.frame(apply(tempTermOutput, 2, as.numeric))
        characterCols <- which(colMeans(is.na(termOutput)) == 1)
        termOutput[, characterCols] <- tempTermOutput[, characterCols]
        colnames(termOutput) <- outputColNames
        options(warn = 0)
        if ("ESTIMATE" %in% colnames(termOutput)[characterCols]) {
            sunk(paste(funVersion, ": 'ESTIMATE' column for term '",  termNames[length(termNames)], "' in file: '", file, "' does not seem to be a numeric value. \n",  sep = ""))
        }
		
		# read DIF term (if DIF term is specified)
        if (!is.null(dif.term)) {
            if (sum(termNames == dif.term) == 0) {
                sunk(paste(funVersion, ": Term declared as DIF: '", dif.term, "' was not found in file: '", file, "'. \n", sep = ""))
            }
			
			# compute absolute DIF and DIF significance
            if (termNames[i] == dif.term) {
                sunk(paste(funVersion, ": Treat '", termNames[i], "' as DIF term.\n", sep = ""))
                abs.dif = 2 * termOutput$ESTIMATE
                ci.90 <- confidence ( abs.dif, termOutput$ERROR, alpha = .10 ) 
				ci.95 <- confidence ( abs.dif, termOutput$ERROR, alpha = .05 ) 
				ci.99 <- confidence ( abs.dif, termOutput$ERROR, alpha = .01 ) 
				sig.90 <- ifelse(abs(abs.dif) > abs.dif.bound & 
                  abs(ci.90 [ , 1]) > sig.dif.bound & 
				  abs(ci.90 [ , 2]) > sig.dif.bound, 1, 0)
                sig.95 <- ifelse(abs(abs.dif) > abs.dif.bound & 
                  abs(ci.95 [ , 1]) > sig.dif.bound & 
				  abs(ci.95 [ , 2]) > sig.dif.bound, 1, 0)
				sig.99 <- ifelse(abs(abs.dif) > abs.dif.bound & 
                  abs(ci.99 [ , 1]) > sig.dif.bound & 
				  abs(ci.99 [ , 2]) > sig.dif.bound, 1, 0)
                termOutput <- cbind(termOutput, abs.dif, ci.90, ci.95, ci.99, sig.90, sig.95, sig.99, filename = file)
				
				if (split.dif == TRUE) {
                  termOutput <- termOutput[1:(nrow(termOutput)/2), ]
                }
            }
        }
        outputList[[i]] <- termOutput
    }
	
	# read regression coefficients
	regrStatement <- grep("REGRESSION COEFFICIENTS", allInput) + 2
    isRegression <- length(regrStatement) > 0
	if ( isRegression) {
		regrEnd <- grep("An asterisk next", allInput)
		regrEnd <- regrEnd[which(regrEnd > regrStatement)][1] - 2
		dimensionsLine <- grep("Regression Variable",allInput)
		stopifnot(length(dimensionsLine) ==1)
		
		# check dimensions
		nameDimensions  <- unlist(strsplit(allInput[dimensionsLine], "  +"))[-1]
		nDimensions <- length(nameDimensions)
		if ( nDimensions == 0 ) {
			nameDimensions <- "Dimension1"
			nDimensions <- length (nameDimensions) 
		}
		sunk(paste(funVersion, ": Found ",nDimensions," dimension(s): ",paste(nameDimensions, collapse=", "),"\n",sep=""))
        
		regrStart <- grep("CONSTANT",allInput)
		regrStart <- regrStart[regrStart <= regrEnd][1]
		regrInput <- crop(allInput[regrStart:regrEnd])
		regrInput <- gsub("\\(|)", "", regrInput)
		regrInput <- gsub("\\*", "  NA", regrInput)
		regrInput <- strsplit(regrInput," +") 
		regrNames <- sapply (regrInput, "[", 1 )
		
		tempRegrOutput <- do.call(rbind, regrInput)
        tempRegrOutput[tempRegrOutput == "NA"] <- NA
        options(warn = -1)
        regrOutput <- data.frame(matrix (apply(tempRegrOutput, 2, as.numeric), nrow = nrow(tempRegrOutput), byrow = FALSE))
        characterCols <- which(colMeans(is.na(regrOutput)) == 1)
        regrOutput[, characterCols] <- tempRegrOutput[, characterCols]
        colnames(regrOutput) <- c("reg.var", paste(rep(c("coef","error"),nDimensions), rep(nameDimensions,each=2),sep="_") )
		regrOutput$filename <- file
		
		outputList$regression <- regrOutput
	}
    return(outputList)
}
