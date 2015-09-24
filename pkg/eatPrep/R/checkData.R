checkData <- function (dat, datnam, values, subunits, units, verbose = TRUE) {
  funVersion <- "checkData: "	 
	varinfo <- makeInputCheckData (values, subunits, units)
	
	if(verbose) cat(paste("\n", funVersion, "Checking dataset ", datnam, " \n", sep = ""))
	
	if (class(dat) != "data.frame") {
		stop (paste(funVersion, "dat must be a data.frame.", sep = ""))
	}
 
	# ID-Check <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # find ID - stop if ID cannot be found
	#	eatTools:::sunk(paste(funVersion, "Checking IDs", sep =""))
  idvarname <- getID(varinfo)
  checkID (dat, idvarname, verbose)

  
	# Variables-Check <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  #	eatTools:::sunk(paste(funVersion, "Checking variables", sep = ""))
  checkVars(dat, varinfo, verbose)
	
	# check missing values
  checkMissings(dat, varinfo, idvarname, verbose)

  # check for invalid codes
  checkCodes(dat, varinfo, idvarname, verbose)
  
}



#-----------------------------------------------------------------------------------------

getID <- function(varinfo) {
  # erkennt am Typ in Varinfo, welche Variable die ID ist
  # gibt String mit Name dieser Variable zurueck
  
  funVersion <- "getID: "
  
  if (missing(varinfo)) {
    stop(paste(funVersion, "Found no information about variables.\n", sep=""))
  } 
  # Typen extrahieren
  type <- which ( sapply(varinfo, "[[", "type") == "ID")
  if (length(type) == 0) {
    stop(paste(funVersion, "No ID variable specified. Please check input 'units' for variable with 'unitType' = 'ID'.\n", sep=""))
  } else { 
  # find name of id variable
    nameID <- names(varinfo)[type]
    if ( length(nameID) > 1) {
      stop(paste(funVersion, "Found more than one ID variable.\n", sep=""))
    }    
  }          
  
  return(nameID)

}   

#-----------------------------------------------------------------------------------------


checkID <- function(dat, idvarname, verbose) {
  funVersion <- "checkID: "	
  # check dat for specified id variable
	if (is.na(match(idvarname, colnames(dat)))) {
		stop(paste(funVersion, "ID variable ", idvarname, "not found in dataset.", sep = ""))
	} else {
		emptyID <- which(nchar(dat[, idvarname]) == 0 | is.na(dat[, idvarname]))
		if (length(emptyID > 0)) {
		  stop(paste(funVersion, "ID variable has empty cells in line(s) ", paste(emptyID, collapse = ", "),"\n", sep = ""))
		} else {
		  if(verbose) cat(paste(funVersion, "Only valid codes in ID variable.\n", sep = ""))
		}
		if (length(na.omit(dat[, idvarname])) != length(na.omit(unique(dat[, idvarname])))) {
		  duplicatedID <- na.omit(unique(dat[, idvarname][duplicated(dat[, idvarname])]))
		  stop(paste(funVersion, "ID variable has ", length(duplicatedID), 
					  " duplicated entries for IDs: ", paste(duplicatedID, collapse = ", "), "\n", sep = ""))
		} else {
		  if(verbose) cat(paste(funVersion, "No duplicated entries in ID variable.\n", sep =""))
		}
	}
}

#-----------------------------------------------------------------------------------------

checkVars <- function(dat, varinfo, verbose) {

  funVersion <- "checkVars: "	

	if (length(colnames(dat)) != length(unique(colnames(dat)))) {
		duplicatedVarnames <- colnames(dat)
		duplicatedVarnames <- unique(duplicatedVarnames[duplicated(duplicatedVarnames)])
		stop(paste(funVersion, "Found duplicated variable names for ", length(duplicatedVarnames), " variables.\n", sep = ""))
	} else {
		if(verbose) cat(paste(funVersion, "No duplicated variable names.\n", sep=""))
	}
	
	varsWithoutVarinfo <- setdiff(colnames(dat), names(varinfo))
	if (length(varsWithoutVarinfo) > 0) {
		if(verbose) cat(paste(funVersion, "Found no variable information about variable(s) ", paste(varsWithoutVarinfo, collapse = ", "), 
			".\nThis/These variables will not be checked for missings and invalid codes.\n", sep = ""))
	}
}
#-----------------------------------------------------------------------------------------

checkMissings <- function (dat, varinfo, idvarname, verbose) {
	# check missing values
#  eatTools:::sunk(paste(funVersion, "Checking missing values", sep = ""))	
 
  funVersion <- "checkMissings: "
  
  vars <- intersect(colnames(dat), names(varinfo)[- which(names(varinfo) == idvarname)])
	
  if (length(vars) == 0){
    if(verbose) cat(paste(funVersion, "Found no variable informations for any of the variables in 'dat'. Check for missing values will be skipped.\n", sep = ""))
  } else {
    
  	# Indikatordatensatz fuer Missings initialisieren	
    missingInd <- matrix(data = NA, nrow = nrow(dat), ncol = length(vars) + 1)
  	
    colnames(missingInd) <- c( idvarname, vars)
  	missingInd[, idvarname] <- dat[, idvarname]
  
  	# Missing-Codes rausfinden: Welche Code-Typen beginnen mit einem "m"?	
    for (var in vars) {
  		CodeTypes <- lapply(varinfo[[var]]$values, "[[", "type")
  		MissingCodes <- names(CodeTypes[substring(CodeTypes, 1, 1) == "m"])
  		if (is.null(MissingCodes) | length(MissingCodes) == 0) {
  			if(verbose) cat(paste(funVersion, "Found no missing values definitions for variable ", 
  			  var, ". This variable will only be checked for NA values.\n", sep = ""))
  		}
  		missingInd[, var ] <- 1 * (!dat[, match(var, colnames(dat))] %in% c(NA, "", MissingCodes))
  	}
  	
  	# zuerst missingInd in dataframe umwandeln, damit sie nicht mehr character ist
  	missingInd <- data.frame(missingInd, stringsAsFactors = F)
  	idCol <- which(colnames(missingInd) == idvarname)
  	
  	# check for variables with only missing values
  	missingInd[, - idCol] <- apply(missingInd[, - idCol], 2, as.numeric)
  	varMissing <- colnames(missingInd[, - idCol])[colSums(missingInd[, - idCol]) == 0]
  	if (length(varMissing) > 0) {
  		if(verbose) cat(paste(funVersion, "Variable(s) ", paste(varMissing, collapse = ", "), 
      " contain(s) only missing values.\n", sep = ""))
  	}
  	
  	# check cases for cases with only missing values
  	caseMissing <- missingInd[rowSums(missingInd[, - idCol]) == 0, idvarname]
  	if (length(caseMissing) > 0) {
  		if(verbose) cat(paste(funVersion, "Case(s)", paste(caseMissing, collapse = ", "), " contain(s) only missing values.\n", 
  			sep = ""))
  	}
  }
}

#-----------------------------------------------------------------------------------------

checkCodes <- function(dat, varinfo, idvarname, verbose) {
  funVersion <- "checkCodes: "
  
  vars <- intersect(colnames(dat), names(varinfo)[- which(names(varinfo) == idvarname)])
  if (length(vars) == 0){
    if(verbose) cat(paste(funVersion, "Found no variable informations for any of the variables in 'dat'. Check for invalid codes will be skipped.\n", sep = ""))
  } else {
    count <- 0
  	
  	for (v in vars) {
  		if (!varinfo[[v]]$type %in% c("ID", "T1", "T2", "T3")) {
  			validCodes <- names(varinfo[[v]]$values)
  			givenCodes <- names(table(dat[, match(v, colnames(dat))]))
  			givenCodesFreq <- table(dat[, match(v, colnames(dat))])
  			if (length(validCodes > 0)) {
  			  invalidCodes <- setdiff(givenCodes, validCodes)
  			  if (length(invalidCodes > 0)) {
  				invalidCodesFreq <- givenCodesFreq[names(givenCodesFreq) %in% invalidCodes]
  				warning(paste(funVersion, "Found invalid codes in variable ", v, "\n",sep = ""))
  				if(verbose) cat( paste(paste(paste(invalidCodesFreq, "cases invalid", names(invalidCodesFreq)), collapse = ", "), paste("-- valid Codes: ", paste(validCodes, collapse = ", "), "\n", sep = "")) )
  				count <- count + 1
  			  }
  			} else {
  			  if(verbose) cat(paste(funVersion, "Found no informations about valid codes for variable ",	v, ".\n", sep = ""))
  			}
  		} 
  	}
  	if (count > 0) {
  	}
  	else {
  		if(verbose) cat(paste(funVersion, "Found no invalid codes.\n", sep = ""))
  	}
  }
}
 
