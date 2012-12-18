# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scoreData 
#
# Change Log:
# 2012-12-18 NH
# ADDED: Function 'scoreData'
# 0000-00-00 AA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------------------------------------------------------

scoreData <- function (dat, unitrecodings, units) {
  funVersion <- "scoreData: "

  if (class(dat) != "data.frame") {
  stop (paste(funVersion, "'dat' must be a data.frame.\n", sep = ""))
  }  

  scoreinfo <- makeInputRecodeData (values = unitrecodings, subunits = units)
  
  if(length(setdiff(colnames(dat), names(scoreinfo))) > 0) {
	cat(paste(funVersion, "Found no scoring information for variable(s) ", 
		paste(setdiff(colnames(dat), names(scoreinfo)), collapse = ", "), 
			". \nThis/These variable(s) will not be scored.\n", sep =""))
  }
  
  # make scored data.frame
  datS <- data.frame(mapply(.scoreData.score, dat, 
  colnames(dat), MoreArgs = list(scoreinfo), USE.NAMES = TRUE), 
  stringsAsFactors = FALSE)
  
  colnames(datS) <- sapply(colnames(datS), .recodeData.renameIDs, scoreinfo, USE.NAMES = FALSE)
  
  return(datS)
}

#-----------------------------------------------------------------------------------------

.scoreData.score <- function (variable, variableName, scoreinfo) {
  variableScored <- NULL
  funVersion <- "scoreData: "
  
  if (!(class(variable) == "character")) { 
    variable <- as.character(variable)
  }
  
  if (is.null(scoreinfo[[variableName]]$values)) {
    variableScored <- variable
    # cat(paste(funVersion, "Found no score information for variable ", variableName, ". This variables will not be scored.\n", sep =""))
  } else {
    dontcheck <- c("mbd","mvi", "mnr", "mci", "mbd", "mir", "mbi")
    variable.unique <- na.omit(unique(variable[which(!variable %in% dontcheck)]))
    scoreinfoCheck <- (variable.unique %in% names(unlist(scoreinfo[[variableName]]$values)))
    if (!all(scoreinfoCheck == TRUE)) {
      cat(paste(funVersion, "Incomplete scoring information for variable ", 
      variableName, ". Value(s) ",  
      paste(sort(variable.unique[!scoreinfoCheck]), collapse = ", "), " will not be scored.\n", sep = ""))
    }
    
    scoreString <- paste(paste("'", names(unlist(scoreinfo[[variableName]]$values)), 
    "'", "=", "'", unlist(scoreinfo[[variableName]]$values), "'", 
    sep = ""), collapse = "; ")
    variableScored <- recode(variable, scoreString, as.factor.result = FALSE, 
    as.numeric.result = FALSE)
	cat(paste(funVersion, variableName, " has been scored.\n", sep =""))
  }
  return(variableScored)
}
