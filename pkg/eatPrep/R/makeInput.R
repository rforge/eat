# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# makeInput
# Description: erstellt von Daemon gebrauchte Inputdateien aus Codebook 
#               bzw. Tabellen im ZKD-Inputformat
# Version:   0.6.0
# Status: alpha
# Release Date:
# depends: Funktion crop aus Package automateModels
# Author:  Nicole Haag
#
# 2011-12-08 NH
# FIXED: bug in makeInputCheckData
# 0000-00-00 AA

# Change Log:
# * 0.5.0 (2011-11-23, NH): hotfix in makeInputRecodeData: wenn units-Dataframe uebergeben wird, dann tu so, als ob es subunits-Dataframe waere

# * 0.2.0 (2011-11-12, NH): in checkInput Warnung fuer fehlende Subunits fuer Units mit unitType = "ID" entfernt
#                           in .makeAggregateinfo Parameter recodedData ergaenzt: sollen Subunitbezeichungen 
#                                            die aus rekodiertem oder die aus Rohdatensatz sein?

# * 0.1.0 (2011-11-03, NH): Dokus fuer Hauptfunktionen hinzugefuegt
#                           
# * 0.0.3 (2011-11-02, NH): Begriffe items, subitems durch units, subunits ersetzt
#           checkInput kann flexibel angewendet werden, je nach dem ob values und/oder units gecheckt werden soll
#
# * 0.0.2 (2011-10-27, NH): Erstellen von varinfo fuer rekodierten Datensatz und fuer Itemdatensatz
#
# * 0.0.1 (2011-10-20, NH): bis jetzt implementiert: 
#           Erstellen von ZKD-Input-Tabellen values, subunits, units aus Codebook (makeCodebookInput)
#           Checken von Inputtabellen und ggf. Erstellen von Defaults, wenn Tabellen fehlen (checkInput)
#           Erstellen von ZKD-Inputlisten: varinfo fuer Rohdaten, recodeinfo, aggregateinfo (makeInputLists)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source("P:/ZKD/development/crop_0.2.0.R")
# library(xlsReadWrite)
# codebook <- read.xls ( "N:/ZKD/V3-2012_Ma_Kodierbuch_2011-07-27.XLS", from = 2, colClasses = "character")
# unitdefstring <- "[[:alnum:]]{1}$"
# codebook$unit <- gsub(unitdefstring, "", codebook$variable.name)

#### To Do #######################################################
# Missingtypes variabel gestalten
# Probleme mit ID in makeCodebookInput und makeInputLists beheben (vor allem bei varinfo fuer check)
##################################################################
#-----------------------------------------------------------------------------------------
## Alle ZKD-Input-Listen auf einmal erstellen

makeInputLists <- function (values, subunits, units, recodedData = TRUE) {

# Description: prueft ZKD-Inputtabellen mittels checkInput und ueberfuehrt sie in ZKD-Listenformate
#              hat diverse spin-offs, die in checkData usw. aufgerufen werden können 
#              und jeweils nur die benötigten Listen erstellen
# 
# arguments: 
#     values (data.frame)   ... ZKD-Inputtabelle fuer Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
#     subunits (data.frame) ... ZKD-Inputtabelle fuer Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     units (data.frame)    ... ZKD-Inputtabelle fuer Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     recodedData (logical) ... wird Aggregierung fuer rekodierten oder Rohdatensatz durchgefuehrt (wichtig fuer Erstellung der aggregateinfo) 
# 
# returns:  Liste mit folgenden Eintraegen:
#     varinfoRaw (list)       ... ZKD-Varinfo fuer Rohdatensaetze
#     varinfoRecoded (list)   ... ZKD-Varinfo fuer recodierte Datensaetze
#     varinfoAggregated (list)... ZKD-Varinfo fuer aggregierte Datensaetze
#     recodeinfo (list)       ... ZKD-Liste mit zur Rekodierung benötigten Infos
#     aggregateinfo (list)    ... ZKD-Liste mit zur Aggregierung benötigten Infos

  checkedInput  <- checkInput(values, subunits, units)
  
  # make lists
  varinfoRaw        <- .makeVarinfoRaw(checkedInput$values, checkedInput$subunits)
  varinfoRecoded    <- .makeVarinfoRecoded(checkedInput$values, checkedInput$subunits)
  varinfoAggregated <- .makeVarinfoAggregated(checkedInput$units)
  recodeinfo        <- .makeRecodeinfo(checkedInput$values, checkedInput$subunits)
  aggregateinfo     <- .makeAggregateinfo(checkedInput$subunits, checkedInput$units, recodedData = recodedData) 

  return (list(varinfoRaw = varinfoRaw, varinfoRecoded = varinfoRecoded,
            varinfoAggregated = varinfoAggregated, 
            recodeinfo = recodeinfo, aggregateinfo = aggregateinfo ))
}

#-----------------------------------------------------------------------------------------
## fuer checkData benötigte Inputs erstellen: alle Varinfos, die es gibt
## doppelte Eintraege fliegen raus

makeInputCheckData <- function (values, subunits, units) {

  checkedInput  <- checkInput(values = values, subunits = subunits, units = units)  

  # make varinfo
  varinfoRaw        <- .makeVarinfoRaw(checkedInput$values, checkedInput$subunits)
  varinfoRecoded    <- .makeVarinfoRecoded(checkedInput$values, checkedInput$subunits)
  varinfoAggregated <- .makeVarinfoAggregated(checkedInput$units)
  
  varinfoAll <- c(varinfoRaw, varinfoRecoded, varinfoAggregated)
  if (any(duplicated(names(varinfoAll)))){
    varinfoAll <- varinfoAll [ - which(duplicated(names(varinfoAll))) ]
  }
  
  return(varinfoAll)
}

#-----------------------------------------------------------------------------------------
## fuer recodeData benötigte Inputs erstellen
### ACHTUNG: HOTFIX!!!

makeInputRecodeData <- function (values, subunits) {

 nSubunits <- length(grep("subunit", colnames(subunits)))
 
 if (nSubunits == 0 ) {
    
   subunits <- subunits [ subunits$unit %in% values$unit , ]
   subunits <- data.frame ( subunit = subunits$unit, subunitLabel = subunits$unitLabel, 
        subunitDescription = subunits$unitDescription, subunitType = subunits$unitType,
        subunitRecoded = subunits$unit, subunitLabelRecoded = subunits$unitLabel, 
        stringsAsFactors = FALSE)
        
   colnames(values) [ which (colnames(values) == "unit") ] <- "subunit"     
   
 }

  checkedInput  <- checkInput(values = values, subunits = subunits, checkUnits = FALSE)
  # make lists
  recodeinfo        <- .makeRecodeinfo(checkedInput$values, checkedInput$subunits)

  return (recodeinfo)
}

#-----------------------------------------------------------------------------------------
## fuer aggregateData benötigte Inputs erstellen

makeInputAggregateData <- function (subunits, units, recodedData = TRUE) {
  checkedInput  <- checkInput(subunits = subunits, units = units, checkValues = FALSE)

  # make lists
  aggregateinfo     <- .makeAggregateinfo(checkedInput$subunits, checkedInput$units, recodedData = recodedData) 

  return (aggregateinfo)
}

#-----------------------------------------------------------------------------------------
checkInput <- function ( values, subunits, units, checkValues = TRUE, checkUnits = TRUE ) {

# Description: prueft ZKD-Inputtabellen auf Konsistenz und defaulted, wenn was fehlt
# 
# arguments: 
#     values (data.frame)   ... ZKD-Inputtabelle fuer Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
#     subunits (data.frame) ... ZKD-Inputtabelle fuer Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     units (data.frame)    ... ZKD-Inputtabelle fuer Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     checkValues (logical) ... Soll values-Tabelle in Check einbezogen werden?
#     checkUnits (logical)  ... Soll units-Tabelle in Check einbezogen werden?
# 
# returns:  Liste mit 2 oder 3 der folgenden Eintraege:
#     values (data.frame)   ... gepruefte ZKD-Inputtabelle fuer Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
#     subunits (data.frame) ... gepruefte ZKD-Inputtabelle fuer Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#     units (data.frame)    ... gepruefte ZKD-Inputtabelle fuer Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
#
#     wenn checkValues = FALSE, wird values-Tabelle nicht ausgegeben
#     wenn checkUnits = FALSE, wird units-Tabelle nicht ausgegeben
#     mindestens eins von beiden muss TRUE sein (sonst macht Konsistenzcheck auch keinen Sinn)


  if (checkValues == FALSE & checkUnits == FALSE) {
    stop("Please specify whether values, units or both should be checked.")
  }

  # check arguments
  if (checkValues == TRUE) {
    if (missing(values)){
      stop("Missing argument: values")
    } else {
      if (class(values) != "data.frame"){
        stop("Argument values is not a data frame.")
      }
    }
  
    # if subunit input is missing: use default values for subitems
    if (missing(subunits)) {
      subunit  <- unique(values$subunit)
      subunitRecoded <- paste(subunit, "R", sep = "")
      subunitLabelRecoded <- paste("Recoded", subunit)
      subunits <- data.frame(unit = subunit, subunit = subunit, subunitType = "", subunitLabel = subunit,# subunitDescription = "",
                subunitPosition = "", #subunitTransniveau = "", 
				subunitRecoded = subunitRecoded,
                subunitLabelRecoded = subunitLabelRecoded, stringsAsFactors = FALSE)
      cat("Found no subunits input. All subunit labels will be defaulted to subunit name.\n")
  #    stop("Missing argument: subunits")
    } else {
      if (class(subunits) != "data.frame"){
        stop("Argument subunits is not a data frame.")
      }
    }
  }
  
  # check subunit labels
  if (any(subunits$subunitLabel == "" )) {
   emptyLabels <- which(subunits$subunitLabel == "")
   cat("Found no subunit label for subunit(s)", emptyLabels, "\nSubunit label will be defaulted to subunit name.\n")
   subunits$subunitLabel[ emptyLabels ] <- subunits$subunit [ emptyLabels ]
  }

  if (checkUnits == TRUE) {
    # if unit input is missing: use default values for units
    if (missing(units)) {
      units <- data.frame(unit = subunits$unit, unitType = "", unitLabel = subunits$unit,
              unitDescription = subunits$subunitDescription, unitAggregateRule = "",
              unitScoreRule = "", stringsAsFactors = FALSE )
      units <- units [ !duplicated(units) , ]
      cat("Found no units input. Use unit names from subunit input. Unit labels will be defaulted to unit names.\n")
    } else {
      if (class(units) != "data.frame"){
        stop("Argument units is not a data frame.")
      }
    }
  }

 # consistency checks
  if (checkValues == TRUE) {
     checkedValuesSubunits <- .checkValuesSubunits (values, subunits)
     values   <- checkedValuesSubunits$values
     subunits <- checkedValuesSubunits$subunits
  }   
  if (checkUnits == TRUE) {
    checkedSubunitsUnits  <- .checkSubunitsUnits(subunits, units)
    subunits <- checkedSubunitsUnits$subunits
    units    <- checkedSubunitsUnits$units
  }

  # make return list
  if (checkUnits == TRUE) {
    if (checkValues == TRUE ) {
      returnList <- list(values = values, subunits = subunits, units = units)
    } else {
      returnList <- list(subunits = subunits, units = units)
    }
  } else {
    if (checkValues == TRUE) {
      returnList <- list(values = values, subunits = subunits)
    } 
  }


 return (returnList)
}

#-----------------------------------------------------------------------------------------

.checkValuesSubunits <- function(values, subunits) {
  # check consistency of subunit names in subunits & values
  subunitsWithoutValues <- setdiff(subunits$subunit, values$subunit)
  if (length(subunitsWithoutValues) > 0 ) {
    cat("Found no values for subunit(s)", subunitsWithoutValues,
        "\nNo varinfo and/or recodeinfo will be written for this/these subunit(s).\n")
    subunits <- subunits[ - which(subunits$subunit %in% subunitsWithoutValues) , ]
  }
  
  valuesWithoutSubunits <- setdiff(values$subunit, subunits$subunit)
  if (length(valuesWithoutSubunits) > 0 ) {
    cat("Found only values for subunit(s)", valuesWithoutSubunits,
        "\nSubunit label will be defaulted to subunit name for this/these subunit(s).\n")
  missingSubunits <- data.frame ( unit = valuesWithoutSubunits, subunit = valuesWithoutSubunits,
              subunitType = "", subunitLabel = valuesWithoutSubunits, subunitDescription = "",
              subunitPosition = "", #subunitTransniveau = "", 
			  subunitRecoded = paste(valuesWithoutSubunits, "R", sep = ""),
              subunitLabelRecoded = paste("Recoded", valuesWithoutSubunits), stringsAsFactors = FALSE)
  subunits <- rbind(subunits, missingSubunits)
  }

  return(list(values = values, subunits = subunits))
}

#-----------------------------------------------------------------------------------------

.checkSubunitsUnits <- function(subunits, units) {
  # check consistency of unit names in units & subunits
  unitsWithoutSubunits <- setdiff(units$unit, subunits$unit)
  if (any(units[ units$unit %in% unitsWithoutSubunits, "unitType"] == "ID")){
    idName <- units [units$unitType == "ID", "unit"]
    unitsWithoutSubunits <- setdiff(unitsWithoutSubunits, idName)    
  }
  if (length(unitsWithoutSubunits) > 0 ) {  
    cat("Found no subunits for unit(s)", unitsWithoutSubunits, "\n")
 #       "\nNo varinfo and recodeinfo will be written for this/these unit(s).\n")
 #   units <- units[ - which(units$unit %in% unitsWithoutSubunits) , ]
  }

  SubunitsWithoutUnits <- setdiff(subunits$unit, units$unit)
  if (length(SubunitsWithoutUnits) > 0 ) {
    cat(paste("Found only subunits for unit(s)", paste(SubunitsWithoutUnits, collapse = ", "), ".\n",
        "Unit label will be defaulted to unit name for this/these unit(s). Unit type, aggregate rule and score rule will be empty.\n"))
		if("unitScoreRule" %in% colnames(units)){
 		    missingunits <- data.frame ( unit = SubunitsWithoutUnits, unitLabel = SubunitsWithoutUnits,
              unitDescription = "", unitType = "",
              unitAggregateRule = "", # unitScoreRule = "", 
			  stringsAsFactors = FALSE)
		} else {
		missingunits <- data.frame ( unit = SubunitsWithoutUnits, unitLabel = SubunitsWithoutUnits,
              unitDescription = "", unitType = "",
              unitAggregateRule = "", stringsAsFactors = FALSE)
		}
  units <- rbind(units, missingunits)
  }

  return(list(subunits = subunits, units = units))
}

#-----------------------------------------------------------------------------------------

.makeVarinfoRawValues <- function(subunitName, values) {
  
  if ( ! subunitName %in% values$subunit ) {
    stop(paste("Found no values for subunit" , subunitName, "."))
  }  
  
  subValues <- values [ values$subunit == subunitName , ]
  varinfoValues <- mapply(list, label = subValues$valueLabel, description = subValues$valueDescription, 
                        type = subValues$valueType, SIMPLIFY=FALSE, USE.NAMES = FALSE)
  names(varinfoValues) <- subValues$value
  return(varinfoValues)
}

#-----------------------------------------------------------------------------------------

.makeVarinfoRecodedValues <- function(subunitName, values) {
  
  if ( ! subunitName %in% values$subunit ) {
    stop(paste("Found no values for subunit" , subunitName, "."))
  }  
  
  subValues <- values [ values$subunit == subunitName , ]
  subValues <- subValues [ ! duplicated(subValues$valueRecode) , ]
  varinfoValues <- mapply(list, label = subValues$valueLabelRecoded, #description = subValues$valueDescriptionRecoded, 
                        type = subValues$valueType, SIMPLIFY=FALSE, USE.NAMES = FALSE)
  names(varinfoValues) <- subValues$valueRecode
  return(varinfoValues)
}

#-----------------------------------------------------------------------------------------

.makeVarinfoRaw <- function(values, subunits) {
 # values: ZKD-Input-Dataframe values
 # subunits: ZKD-Input-Dataframe subunits
  
  # make varinfo
  varinfoValues <- mapply(.makeVarinfoRawValues, subunits$subunit, MoreArgs = list(values), SIMPLIFY=FALSE)  
  varinfoList <- mapply(list, label = subunits$subunitLabel, #description = subunits$subunitDescription, 
  type = subunits$subunitType, #transniveau=subunits$subunitTransniveau, 
  values = varinfoValues, SIMPLIFY=FALSE, USE.NAMES = FALSE)  
  
  names(varinfoList) <- subunits$subunit
  
  return(varinfoList)
}
  
#-----------------------------------------------------------------------------------------

.makeVarinfoRecoded <- function(values, subunits) {
 # values: ZKD-Input-Dataframe values
 # subunits: ZKD-Input-Dataframe subunits
  
  # make varinfo
  varinfoValues <- mapply(.makeVarinfoRecodedValues, subunits$subunit, MoreArgs = list(values), SIMPLIFY=FALSE)  
  varinfoList <- mapply(list, label = subunits$subunitLabelRecoded,# description = subunits$subunitDescriptionRecoded, 
  type = subunits$subunitType, values = varinfoValues, SIMPLIFY=FALSE, USE.NAMES = FALSE)  
  
  names(varinfoList) <- subunits$subunitRecoded
  
  return(varinfoList)
}

#-----------------------------------------------------------------------------------------

.makeVarinfoAggregated <- function(units) {
  
  # make varinfo
 # varinfoValues <- mapply(.makeVarinfoRecodedValues, subunits$subunit, MoreArgs = list(values), SIMPLIFY=FALSE)  
  varinfoList <- mapply(list, label = units$unitLabel, description = units$unitDescription, 
  type = units$unitType, # values = varinfoValues, 
  SIMPLIFY=FALSE, USE.NAMES = FALSE)  
  
  names(varinfoList) <- units$unit
  
  return(varinfoList)
}
  
#-----------------------------------------------------------------------------------------

.makeRecodeinfoValues <- function(subunitName, values) {
  
  if ( ! subunitName %in% values$subunit ) {
    stop(paste("Found no values for subunit" , subunitName, "."))
  }  
  subValues <- values [ values$subunit == subunitName , ]
  recodeinfoValues <- as.list(subValues$valueRecode)
  names(recodeinfoValues) <- subValues$value
  return(recodeinfoValues)
}

#-----------------------------------------------------------------------------------------

.makeRecodeinfo <- function(values, subunits) {
 # values: ZKD-Input-Dataframe values
 # subunits: ZKD-Input-Dataframe subunits

 # make recodeinfo
  recodeinfoValues <- mapply(.makeRecodeinfoValues, subunits$subunit, MoreArgs = list(values), SIMPLIFY=FALSE)  
  recodeinfoList   <- mapply(list, label = subunits$subunitLabelRecoded, newID = subunits$subunitRecoded, 
     values = recodeinfoValues, SIMPLIFY=FALSE, USE.NAMES = FALSE)  
  names(recodeinfoList) <- subunits$subunit

  return(recodeinfoList)
}

#-----------------------------------------------------------------------------------------
.makeAggregateinfo <- function (subunits, units, recodedData = TRUE) {
  
  # welche units bestehen aus mehr als einem Subunit?
  aggregateunits <- unique ( names(table(subunits$unit))[ table(subunits$unit) > 1] )
  if (recodedData == TRUE){
    aggregateSubunits <- lapply( aggregateunits, function(ll) { subunits$subunitRecoded [subunits$unit == ll ] } )
  } else {
    aggregateSubunits <- lapply( aggregateunits, function(ll) { subunits$subunit [subunits$unit == ll ] } )
  }
  arule <- units$unitAggregateRule [ match(aggregateunits, units$unit) ]
#  srule <- units$unitScoreRule [ match(aggregateunits, units$unit) ]

  # aggregateinfo erstellen
  aggregateinfo <- mapply(list, arule = arule, #srule = srule, 
			subunits=aggregateSubunits, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  names(aggregateinfo) <- aggregateunits
  return(aggregateinfo)
}

