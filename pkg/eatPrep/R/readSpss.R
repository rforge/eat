### ACHTUNG! read.spss liest Dateinamen manchmal durchgaengig in Gross-, manchmal in Kleinbuchstaben ein. Problem!

readSpss <- function (file, correctDigits=FALSE, truncateSpaceChar = TRUE, oldID = NULL, newID = NULL ) {

  if(file.exists(file) != TRUE) {
    stop("Could not find file.\n")
  }  
   	
  suppressWarnings( dat <- data.frame(read.spss(file.path(file),to.data.frame=FALSE, use.value.labels=FALSE), stringsAsFactors=FALSE) )

	if (!is.null(newID)){
		if(length(newID)!=1) {
			stop("'newID' has to be of length 1.") 
		}   

		if (!is.null(oldID)){
			idCol  <- na.omit(match(oldID, colnames(dat)))
			if(length(na.omit(match(colnames(dat), oldID)))<1) {
				stop("Specified 'oldID' wasn't found in dataset.") 
			}
			if(length(na.omit(match(colnames(dat), oldID)))>1) {
				stop("Specified 'oldID' was found more than once in dataset.") 
			}
			colnames(dat)[idCol] <- newID
		}
	}  
	
  ### Leerzeichen abschnipseln 
  if(truncateSpaceChar == TRUE)  {
    dat <- do.call("data.frame", list(lapply(dat, crop), stringsAsFactors = FALSE ) )
  } else {
    # Umwandlung nach Character ( wird bei if(truncateSpaceChar == TRUE) auch schon mit impliziert )
    dat <- set.col.type(dat)
  }
  
  # Check auf alles character
  stopifnot(all(sapply(dat, is.character )))
  
  ### Stelligkeitskorrektur
  if(correctDigits == TRUE) {
    colsToCorrect <- lapply(seq(along = dat), FUN=function(ii) { sort(unique(nchar(dat[,ii])))})        
    options(warn = -1)                                          
    colsToCorrect <- which(sapply(colsToCorrect, FUN=function(ii){all(ii == c(1,2))})) 
    options(warn = 0)
    if(length(colsToCorrect) > 0) {
      cat(length(colsToCorrect), "columns are corrected for column width.\n")
      for (ii in colsToCorrect) {
        dat[,ii] <- gsub(" ","0", formatC(as.character(dat[,ii]),width=2))
      }
    }
  }             
  return(dat)
}    
              
  