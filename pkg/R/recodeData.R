# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# recodeData - formerly known as zkdDatasetRecode
#
# function:    recodeData (dat, values, subunits)
#
# Description: rekodiert Datensätze
# 
# arguments: 
#     dat (data.frame)      ... Datensatz mit ID-Variablen und (mindestens) allen Variablen, die rekodiert werden sollen
#     values (data.frame)   ... ZKD-Inputtabelle für Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
#     subunits (data.frame) ... ZKD-Inputtabelle für Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#
# Version: 	1.1.0
# Status: alpha
# Release Date: 	2011-08-02
# Author: 	Martin Hecht , Christiane Penk
# depends: package car
#          Funktionen aus makeInput
#
# Change Log:
# 2012-09-04 NH
# CHANGED: removed calls to 'sunk'
# 0000-00-00 AA
#
# * 1.1.0  (2011-11-23, ZKD) stabilisiert
# * 1.0.1 (2011-11-21, KS) auch positive Rückmeldung (welche Var rekodiert)
# * 1.0.0 (2011-11-03, NH) überarbeitet und auf neue ZKD-Inputtabellen angepasst, 
#               überflüssig gewordene Checks rausgenommen 
# 02.08.11 "mbd" aus Prüfung auf unvollständige Rekodierungsvorschrift rausgenommen
# 12.07.11 kein Abbruch bei unvollständigen Rekodierungsvorschriften, nur Warnung
# 01.07.11 Unvollständige Rekodierungsvorschrift:
#				bugfix
#				Ausgabe für welche Werte ( je Variable ) die Rekodierungsvorschriften fehlt
#				Abbruch bei unvollständigen Rekodierungsvorschriften
# 24.05.11 Anpassungen nach zkd-Sitzung
#			für zu rekodierende Variablen:	
#				Der Variablen-Typ (nach neuer Def.) wird durchgeschleift.
#				Das Transformationsniveau wird auf entsprechenden Wert gesetzt.
#				Das Value-Label rekodierter Variablen wird gelöscht.
#				Plausicheck: bei surjektiven Abbildungen mit missings stoppt Funktion (Fall nicht vorgesehen)
#			nicht zu rekodierende Variablen werden (wie bisher) durchgeschleift
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------------------------------------------------------

recodeData <- function (dat, values, subunits) {
  funVersion <- "recodeData: "

  if (class(dat) != "data.frame") {
  stop (paste(funVersion, "'dat' must be a data.frame.\n", sep = ""))
  }  

  recodeinfo <- makeInputRecodeData (values = values, subunits = subunits)
  
  # make recoded data.frame
  datR <- data.frame(mapply(.recodeData.recode, dat, 
  colnames(dat), MoreArgs = list(recodeinfo), USE.NAMES = TRUE), 
  stringsAsFactors = FALSE)
  
  colnames(datR) <- sapply(colnames(datR), .recodeData.renameIDs, recodeinfo, USE.NAMES = FALSE)
  
  return(datR)
}

#-----------------------------------------------------------------------------------------

.recodeData.recode <- function (variable, variableName, recodeinfo) {
  variableRecoded <- NULL
  funVersion <- "recodeData: "
  
  if (!(class(variable) == "character")) { 
    variable <- as.character(variable)
  }
  
  if (is.null(recodeinfo[[variableName]]$values)) {
    variableRecoded <- variable
    cat(paste(funVersion, "Found no recode information for variable ", variableName, ". This variables will not be recoded.\n", sep =""))
  } else {
    dontcheck <- c("mbd")
    variable.unique <- na.omit(unique(variable[which(!variable %in% dontcheck)]))
    recodeinfoCheck <- (variable.unique %in% names(unlist(recodeinfo[[variableName]]$values)))
    if (!all(recodeinfoCheck == TRUE)) {
      cat(paste(funVersion, "Incomplete recode information for variable ", 
      variableName, ". Value(s) ",  
      paste(sort(variable.unique[!recodeinfoCheck]), collapse = ", "), " will not be recoded.\n", sep = ""))
    }
    
    recodeString <- paste(paste("'", names(unlist(recodeinfo[[variableName]]$values)), 
    "'", "=", "'", unlist(recodeinfo[[variableName]]$values), "'", 
    sep = ""), collapse = "; ")
    variableRecoded <- recode(variable, recodeString, as.factor.result = FALSE, 
    as.numeric.result = FALSE)
	cat(paste(funVersion, variableName, " has been recoded.\n", sep =""))
  }
  return(variableRecoded)
}

#-----------------------------------------------------------------------------------------

.recodeData.renameIDs <-  function(colname, recodeinfo) {
  newID <- recodeinfo[[colname]]$newID
  if (is.null(newID)) {
   colname
  } else {
    newID
  }
}