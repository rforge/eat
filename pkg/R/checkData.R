# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# checkData - formerly known as zkdDatasetCheck
# 
# function:    checkData (dat, values, subunits, units)
#
# Description: checkt Datensätze auf einige grundlegende Dinge:
#              - Personen und/oder Variablen mit nur Missings
#              - fehlende oder doppelte Einträge in ID-Variable
#              - Vorhandensein von invaliden Codes
# 
# arguments: 
#     dat (data.frame)      ... Datensatz mit ID-Variablen und allen Variablen, die geprüft werden sollen
#     values (data.frame)   ... ZKD-Inputtabelle für Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
#     subunits (data.frame) ... ZKD-Inputtabelle für Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     units (data.frame)    ... ZKD-Inputtabelle für Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#
# Version:   1.1.0
# Status: alpha
# Release Date:
# depends: Funktionen aus makeInput
#          Funktion sunk aus Package automateModels
# Author:  Nicole Haag, Anna Lenski
#
# Release Notes
# 2011-12-08 NH
# FIXED: error message in getID corresponds with colnames in 'units'
# 0000-00-00 AA

# * 1.2.0 (2011-11-22, NH): bugfix 
#   + check auf Missing Values und Invalid Codes deaktiviert, wenn für keine Variable aus Datensatz varinfo vorliegt.
#
# * 1.1.0 (2011-11-03, NH): modularisiert
#
# * 1.0.0 (2011-11-02, NH): komplett überarbeitet und auf neue ZKD-Struktur angepasst. 
#   + Meldungen werden jetzt per 'sunk' übergeben
#   + Input sind ZKD-Inputtabellen: values, subunits und units   
#
# * zu 0.6.4 (AL & MH)
#	  + "data <- " eingefügt vor recode-Statement
# * zu 0.5.3 (NH)
#	  + bugfix in Missing-Check
#	  + Funktion identifiziert sich bei Fehlermeldungen
# * zu 0.5.1 (NH)
#   + Missing-Check von NA auf ZKD-Missings erweitert
#   + Helper "zkdHelpers_getID" eingebaut
# * zu 0.5.0 (KS)
#   + Message zu NA-Check ausgebessert
# * zu 0.4.4
#   + behandelt im Unterschied zu 0.4.3 validCodes als numerisch, d.h. "01" wird zu "1" 
# * zu 0.4.0
#   + Check auf invalide Codes implementiert: für alle Variablen, für die in 
#     varinfo Codes vergeben werden, werden entsprechende Variablen gecheckt
# * zu 0.2.0
#   + TRUE statt T
#   + zkdDatasetCheck_run gibt Bool zurück; TRUE, wenn fortgesetzt werden kann
#   + Übergabe auch der varinfo
#   + ID wird jetzt aus varinfo genommen
# * zu 0.1.0
#   + jetzt Datensatz als Funktionsparameter übergeben, nicht mehr globale
#     Variable
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------------------------------------------------------

	checkData <- function (dat, values, subunits, units) {
	  funVersion <- "checkData_1.1.0: "	 
		varinfo <- makeInputCheckData (values, subunits, units)
		
		if (class(dat) != "data.frame") {
			stop (paste(funVersion, "dat must be a data.frame.", sep = ""))
		}
	 
		# ID-Check <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	  # find ID - stop if ID cannot be found
		#	sunk(paste(funVersion, "Checking IDs", sep =""))
	  idvarname <- getID(varinfo)
	  .checkData.checkID (dat, idvarname)

	  
		# Variables-Check <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	  #	sunk(paste(funVersion, "Checking variables", sep = ""))
	  .checkData.checkVars(dat, varinfo)
		
		# check missing values
	  .checkData.checkMissings(dat, varinfo, idvarname)

	  # check for invalid codes
	  .checkData.checkCodes(dat, varinfo, idvarname)
	  
	}



#-----------------------------------------------------------------------------------------

getID <- function(varinfo) {
  # erkennt am Typ in Varinfo, welche Variable die ID ist
  # gibt String mit Name dieser Variable zurück
  
  funVersion <- "getID: "
  
  if (missing(varinfo)) {
    stop(paste(funVersion, "Found no information about variables.", sep=""))
  } 
  # Typen extrahieren
  type <- which ( sapply(varinfo, "[[", "type") == "ID")
  if (length(type) == 0) {
    stop(paste(funVersion, "No ID variable specified. Please check input 'units' for variable with 'unitType' = 'ID'.", sep=""))
  } else { 
  # find name of id variable
    nameID <- names(varinfo)[type]
    if ( length(nameID) > 1) {
      stop(paste(funVersion, "Found more than one ID variable.", sep=""))
    }    
  }          
  
  return(nameID)

}   

#-----------------------------------------------------------------------------------------


.checkData.checkID <- function(dat, idvarname) {
  funVersion <- ".checkData.checkID: "	
  # check dat for specified id variable
	if (is.na(match(idvarname, colnames(dat)))) {
		stop(paste(funVersion, "ID variable ", idvarname, "not found in dataset.", sep = ""))
	} else {
		emptyID <- which(nchar(dat[, idvarname]) == 0 | is.na(dat[, idvarname]))
		if (length(emptyID > 0)) {
		  stop(paste(funVersion, "ID variable has empty cells in line(s) ", paste(emptyID, collapse = ", "), sep = ""))
		} else {
		  sunk(paste(funVersion, "Only valid codes in ID variable.", sep = ""))
		}
		if (length(na.omit(dat[, idvarname])) != length(na.omit(unique(dat[, idvarname])))) {
		  duplicatedID <- na.omit(unique(dat[, idvarname][duplicated(dat[, idvarname])]))
		  stop(paste(funVersion, "ID variable has ", length(duplicatedID), 
					  " duplicated entries for IDs: ", paste(duplicatedID, collapse = ", "), sep = ""))
		} else {
		  sunk(paste(funVersion, "No duplicated entries in ID variable.", sep =""))
		}
	}
}

#-----------------------------------------------------------------------------------------

.checkData.checkVars <- function(dat, varinfo) {

  funVersion <- ".checkData.checkVars: "	

	if (length(colnames(dat)) != length(unique(colnames(dat)))) {
		duplicatedVarnames <- colnames(dat)
		duplicatedVarnames <- unique(duplicatedVarnames[duplicated(duplicatedVarnames)])
		stop(paste(funVersion, "Found duplicated variable names for ", length(duplicatedVarnames), " variables.", sep = ""))
	} else {
		sunk(paste(funVersion, "No duplicated variable names.", sep=""))
	}
	
	varsWithoutVarinfo <- setdiff(colnames(dat), names(varinfo))
	if (length(varsWithoutVarinfo) > 0) {
		sunk(paste(funVersion, "Found no variable information about variable(s) ", paste(varsWithoutVarinfo, collapse = ", "), 
			".\nThis/These variables will not be checked for missings and invalid codes.", sep = ""))
	}
}
#-----------------------------------------------------------------------------------------

.checkData.checkMissings <- function (dat, varinfo, idvarname) {
	# check missing values
#  sunk(paste(funVersion, "Checking missing values", sep = ""))	
 
  funVersion <- ".checkData.checkMissings: "
  
  vars <- intersect(colnames(dat), names(varinfo)[- which(names(varinfo) == idvarname)])
	
  if (length(vars) == 0){
    sunk(paste(funVersion, "Found no variable informations for any of the variables in 'dat'. Check for missing values will be skipped.", sep = ""))
  } else {
    
  	# Indikatordatensatz für Missings initialisieren	
    missingInd <- matrix(data = NA, nrow = nrow(dat), ncol = length(vars) + 1)
  	
    colnames(missingInd) <- c( idvarname, vars)
  	missingInd[, idvarname] <- dat[, idvarname]
  
  	# Missing-Codes rausfinden: Welche Code-Typen beginnen mit einem "m"?	
    for (var in vars) {
  		CodeTypes <- lapply(varinfo[[var]]$values, "[[", "type")
  		MissingCodes <- names(CodeTypes[substring(CodeTypes, 1, 1) == "m"])
  		if (is.null(MissingCodes) | length(MissingCodes) == 0) {
  			sunk(paste(funVersion, "Found no missing values definitions for variable ", 
  			  var, ". This variable will only be checked for NA values.", sep = ""))
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
  		sunk (paste(funVersion, "Variable(s) ", paste(varMissing, collapse = ", "), 
      " contain(s) only missing values.\n", sep = ""))
  	}
  	
  	# check cases for cases with only missing values
  	caseMissing <- missingInd[rowSums(missingInd[, - idCol]) == 0, idvarname]
  	if (length(caseMissing) > 0) {
  		sunk(paste(funVersion, "Case(s)", paste(caseMissing, collapse = ", "), " contain(s) only missing values.\n", 
  			sep = ""))
  	}
  }
}

#-----------------------------------------------------------------------------------------

.checkData.checkCodes <- function(dat, varinfo, idvarname) {
  funVersion <- ".checkData.checkCodes: "
  
  vars <- intersect(colnames(dat), names(varinfo)[- which(names(varinfo) == idvarname)])
  if (length(vars) == 0){
    sunk(paste(funVersion, "Found no variable informations for any of the variables in 'dat'. Check for invalid codes will be skipped.", sep = ""))
  } else {
  
    
    #	sunk(paste(funVersion, "Checking for invalid codes.", sep=""))
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
  				sunk(paste(funVersion, "Found invalid codes in variable ", v, sep = ""))
  				sunk( paste(paste(paste(invalidCodesFreq, "cases invalid", names(invalidCodesFreq)), collapse = ", "), 
          paste("-- valid Codes: ", paste(validCodes, collapse = ", "), sep = "")) )
  				count <- count + 1
  			  }
  			} else {
  			  sunk(paste(funVersion, "Found no informations about valid codes for variable ",	v, ".", sep = ""))
  			}
  		} 
  	}
  	if (count > 0) {
  	}
  	else {
  		sunk(paste(funVersion, "Found no invalid codes.", sep = ""))
  	}
  }
}
 
