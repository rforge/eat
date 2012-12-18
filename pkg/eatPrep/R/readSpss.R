####################################################################################################################
#
# readSpss
# based on function loadSav: liest SPSS-Datendateien (*.sav) ein, macht Stelligkeitskorrektur bei Bedarf
#
#
# Version: 	0.1.0
# Imports:
# Published:
# Author:   Sebastian Weirich, Nicole Haag
# Maintainer:
#
# Change Log:
# 2012-09-03 NH
# ADDED: new function readSpss based on loadSav
# 2012-12-18 NH
# ADDED: optional renaming of IDs
# 0000-00-00 AA
# 2011-11-23 ZKD: stabilisiert
# 2011_11_04 KS: unique (Z. 61), stringsAsFactors=FALSE (Z. 60)
# 2011-11-11 NH: umbenannt auf loadSav (style guide-konform)
#
####################################################################################################################

### file                 ... Name der SPSS-Datei, die eingelesen werden soll
### correctDigits        ... optional: Stelligkeitskorrektur 
### truncateSpaceChar    ... optional: entfernt für alle Spalten vorangehende und abschließende Leerzeichen ( mit 'crop')
                          
### ACHTUNG! read.spss liest Dateinamen manchmal durchgaengig in Groß-, manchmal in Kleinbuchstaben ein. Problem!

readSpss <- function (file, correctDigits=FALSE, truncateSpaceChar = TRUE, oldIDs = NULL, newID = NULL ) {

  if(file.exists(file) != TRUE) {
    stop("Could not find file.\n")
  }  
           	
  suppressWarnings( dat <- data.frame(read.spss(file.path(file),to.data.frame=FALSE, use.value.labels=FALSE), stringsAsFactors=FALSE) )


	if (!is.null(newID)){
		if(length(newID)!=1) {
			stop("'newID' has to be of length 1.") 
		}   

		if (!is.null(oldIDs)){
			idCol  <- na.omit(match(oldIDs, colnames(dat)))
			if(length(idCol)<1) {
				stop("None of the specified 'oldIDs' were found in dataset.") 
			}
			if(length(idCol)>1) {
				stop("More than one of the specified 'oldIDs' were found in dataset.") 
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
              
  