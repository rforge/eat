recodeData <- function (dat, values, subunits, verbose = FALSE) {
  funVersion <- "recodeData: "
  
  if (class(dat) != "data.frame") {
  stop (paste(funVersion, "'dat' must be a data.frame.\n", sep = ""))
  }  

  recodeinfo <- makeInputRecodeData (values = values, subunits = subunits)
  
  # make recoded data.frame
  datR <- data.frame(mapply(.recodeData.recode, dat, 
  colnames(dat), MoreArgs = list(recodeinfo = recodeinfo, verbose = verbose), USE.NAMES = TRUE), 
  stringsAsFactors = FALSE)
  
  colnames(datR) <- sapply(colnames(datR), .recodeData.renameIDs, recodeinfo, USE.NAMES = FALSE)
  
  return(datR)
}

#-----------------------------------------------------------------------------------------

.recodeData.recode <- function (variable, variableName, recodeinfo, verbose = TRUE) {
  variableRecoded <- NULL
  funVersion <- "recodeData: "
  
  if (!(class(variable) == "character")) { 
    variable <- as.character(variable)
  }
  
  if (is.null(recodeinfo[[variableName]]$values)) {
    variableRecoded <- variable
    if (verbose) cat(paste(funVersion, "Found no recode information for variable ", variableName, ". This variable will not be recoded.\n", sep =""))
  } else {
    dontcheck <- c("mbd")
    variable.unique <- na.omit(unique(variable[which(!variable %in% dontcheck)]))
    recodeinfoCheck <- (variable.unique %in% names(unlist(recodeinfo[[variableName]]$values)))
    if (!all(recodeinfoCheck == TRUE)) {
      if (verbose) cat(paste(funVersion, "Incomplete recode information for variable ", 
      variableName, ". Value(s) ",  
      paste(sort(variable.unique[!recodeinfoCheck]), collapse = ", "), " will not be recoded.\n", sep = ""))
    }
    
    recodeString <- paste(paste("'", names(unlist(recodeinfo[[variableName]]$values)), 
    "'", "=", "'", unlist(recodeinfo[[variableName]]$values), "'", 
    sep = ""), collapse = "; ")
    variableRecoded <- car:::recode(variable, recodeString, as.factor = FALSE,
    as.numeric = FALSE)
	if (verbose) cat(paste(funVersion, variableName, " has been recoded.\n", sep =""))
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