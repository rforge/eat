#-----------------------------------------------------------------------------------------

makeCodebookInput <- function (codebook){

  # Description: erstellt ZKD-Inputtabellen aus IQB-Codebook (bzw. Kodierbuch)
  # 
  # arguments: 
  #     codebook (data.frame) ... IQB-Codebook, ohne die erste leere Zeile
  # 
  # returns:  Liste mit den Eintraegen:
  #     values (data.frame)   ... ZKD-Inputtabelle fuer Codes, siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx             
  #     subunits (data.frame) ... ZKD-Inputtabelle fuer Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   
  #     units (data.frame)    ... ZKD-Inputtabelle fuer Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx   

 # check arguments
  if (missing(codebook)){
    stop("Missing argument: codebook")
  } else {
    if (class(codebook) != "data.frame"){
      stop("Argument codebook is not a data frame.")
    }
  }

  # trim spaces
  codebook <- data.frame ( apply(codebook, 2, crop), stringsAsFactors = FALSE)
  
  values   <- .makeValuesCodebook(codebook)
  subunits <- .makeSubunitsCodebook(codebook)
  units    <- .makeUnitsCodebook(codebook)
  
  inputList <- list(values = values, subunits = subunits, units = units)
  
  return(inputList)
}
 
#-----------------------------------------------------------------------------------------
# Subfunktion, um fehlende Eintraege im Kodierbuch zu ergaenzen.

.fillVarRows <- function(variable) {
 # variable: variable, in which entries have to be filled
 
 if ( ! is.vector(variable)){
  stop("variable is not a vector.")
 }
 if (length(variable) == 0 ) {
  stop("variable is empty.")
 }
 
 nValues <- diff(c(which(variable != ""), length(variable)))
 nValues [ length(nValues) ] <- nValues [ length(nValues) ] + 1
 
 if (any(variable == "")){
  variableNames <- variable [ - which(variable == "") ]
 } else {
  variableNames <- variable
 }
 
 filledVariable      <- rep(variableNames, nValues )
 
 return(filledVariable)
}

#-----------------------------------------------------------------------------------------

.setMissingtypes <- function(variable, missinglabels) {
  variable [grep("^(4|94|-94){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*missing by design$", missinglabels)] <- "mbd"
  variable [grep("^(5|95|-95){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*text volume insufficient$", missinglabels)] <- "mvi"
  variable [grep("^(6|96|-96){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*missing not reached$", missinglabels)] <- "mnr"
  variable [grep("^(7|97|-97){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*Coding impossible$", missinglabels)] <- "mci"
  variable [grep("^(8|98|-98){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*Invalid response$|^(8|98|-98){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*nicht interpretierbar$", missinglabels)] <- "mir"
  variable [grep("^(9|99|-99){0,1}[[:space:]]*([fF]alsch:)*[[:space:]]*missing by intention$", missinglabels)] <- "mbi"

  return(variable)
}

#-----------------------------------------------------------------------------------------

.makeSrule <- function(bew) {

  bew <- gsub ( " ", "", bew)
  
  # Trick, damit die Funktion funktioniert
  bew [ bew == "" ] <- ",:"
  bew <- unlist( strsplit( bew , split= ",|;" ) )
  bew <- data.frame( matrix( unlist( strsplit( bew , split = ":" ) ) , ncol=2 , byrow=T) , stringsAsFactors = FALSE)
  bew <- bew[ order( bew[,1], decreasing = TRUE ) , ]
  
  if(all(rowSums(bew == "") == ncol(bew))== TRUE ) {
    bewString <- ""
  } else {
    # hier könnten ein paar Erlaeuterungen nicht schaden.
    bewTabelle <- cbind(c( lag(bew[ , 2], 1), "lo"), c("hi", bew[ , 2]), c(bew[ , 1], "0"))
    bewString <- paste( paste("'", bewTabelle[ , 1], "':'", bewTabelle[ , 2], "'='", bewTabelle[ , 3], "'", sep = ""), collapse =";")
    bewString <- gsub("'hi'", "hi", bewString)
    bewString <- gsub("'lo'", "lo", bewString)
  }
  return(bewString)
}  

#-----------------------------------------------------------------------------------------
  
.makeValuesCodebook <- function ( codebook ) {

 # make data.frame "values"  
 codebook$valueType <- "vc"
 codebook$valueType <- .setMissingtypes (codebook$valueType, codebook$value.label.long)
 codebook$credit [ codebook$valueType != "vc" ] <-  codebook$valueType [ codebook$valueType != "vc" ]
 
 values <- data.frame ( subunit = codebook$variable.name, value = codebook$value, 
                        valueRecode = codebook$credit, valueLabelRecoded = codebook$credit, #valueDescriptionRecoded = "" , 
                        valueType = codebook$valueType, valueLabel = codebook$value.label.long, 
                        valueDescription = codebook$value.label.long, stringsAsFactors = FALSE)
 
 # fill empty rows
 values$subunit <- .fillVarRows(values$subunit)
 return(values)
} 

#-----------------------------------------------------------------------------------------

.makeSubunitsCodebook <- function(codebook) {

 subunits <- data.frame(unit = codebook$Item, subunit = codebook$variable.name, subunitType = codebook$KA, subunitLabel =codebook$variable.label,
                            subunitDescription = codebook$variable.label, subunitPosition = codebook$Var..Pos, #subunitTransniveau = "", 
              subunitRecoded = codebook$subunitRecoded, subunitLabelRecoded = codebook$subunitLabelRecoded, 
              stringsAsFactors = FALSE)

 # remove all rows without values
 emptyRows <- which(rowSums(subunits == "") == ncol(subunits))
 if (length(emptyRows) > 0){
   subunits <- subunits [ -emptyRows,  ]
 }
 
 # fill empty rows in column unit
 subunits$unit <- .fillVarRows(subunits$unit)

  if (any(subunits$subunitLabel == "" )) {
   emptyLabels     <- which(subunits$subunitLabel == "")
   emptyLabelNames <- subunits$subunit[emptyLabels]
   cat("Found no subunit label for subunit(s)", emptyLabelNames, 
       ". Subunit label will be defaulted to subunit name.\n", fill = 100)
   subunits$subunitLabel[ emptyLabels ] <- subunits$subunit [ emptyLabels ]
  }
  
 # generate subunit names and subunit label for recoded subunits
 subunits$subunitRecoded <- paste(subunits$subunit, "R", sep="")
 subunits$subunitLabelRecoded <- paste("Recoded", subunits$subunitLabel)

 return(subunits)
}

#-----------------------------------------------------------------------------------------

.makeUnitsCodebook <- function(codebook) {

units <- data.frame(unit = codebook$Item, unitLabel = codebook$Item, unitDescription ="", 
          unitType = "", unitAggregateRule = "",# unitScoreRule = codebook$Itembewertung, 
          stringsAsFactors = FALSE)

 # remove all rows without values
 emptyRows <- which(rowSums(units == "") == ncol(units))
 if (length(emptyRows) > 0){
   units <- units [ -emptyRows,  ]
 }
 
 # remove duplicated rows
 duplicatedRows <- which(duplicated(units))
 if (length(duplicatedRows) > 0){
   units <- units [ -duplicatedRows,  ]
 }
 
 
 # ACHTUNG, diese Funktion klappt nur mit mapply!
 #units$unitScoreRule <- mapply(.makeSrule, units$unitScoreRule, USE.NAMES = FALSE)
 
 return(units)
}


