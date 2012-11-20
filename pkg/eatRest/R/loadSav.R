####################################################################################################################
#
# loadSav
# liest SPSS-Datendateien (*.sav) ein, macht Stelligkeitskorrektur bei Bedarf und bindet alles in eine Liste
#
#
# Version: 	0.1.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
# 2011-11-23 ZKD: stabilisiert
# 2011_11_04 KS: unique (Z. 61), stringsAsFactors=FALSE (Z. 60)
# 2011-11-11 NH: umbenannt auf loadSav (style guide-konform)
#
####################################################################################################################


### path                 ... Pfad, wo SPSS-Dateien liegen
### savFiles             ... optional: Vektor mit Dateinamen 
### oldIDS               ... old ID name(s). Vektor mit Namen, die in Einzeldatensaetzen IDs bezeichnen
###                          In jedem der einzulesenden Einzeldatensaetze darf immer NUR eine und genau eine der IDs aus oldIDS vorkommen 
### newIDS               ... new ID name: Namen der IDs werden - wenn sie es nicht schon sind - vereinheitlicht
### correctDigits        ... optional: Stelligkeitskorrektur 
### truncateSpaceChar    ... optional: entfernt für alle Spalten vorangehende und abschließende Leerzeichen ( mit 'crop')
                          
### ACHTUNG! read.spss liest Dateinamen manchmal durchgaengig in Groß-, manchmal in Kleinbuchstaben ein. Problem!

loadSav <- function ( path=getwd(), savFiles=NULL, oldIDS, newID, correctDigits=FALSE, truncateSpaceChar = TRUE ) {
           funVersion <- "loadSav_0.1.0"
           if(missing(oldIDS))  {stop(paste("Error in ",funVersion,": 'oldIDS' is missing.\n",sep="")) }   
           if(missing(newID))   {stop(paste("Error in ",funVersion,": 'newID' is missing.\n",sep="")) }
           if(length(newID)!=1) {stop(paste("Error in ",funVersion,": 'newID' has to be of length 1.\n",sep="")) }   
           # if(!exists("read.spss"))  {library(foreign)}
           if(!is.null(savFiles)) {
              fileExists <- file.exists(file.path(path,savFiles))
              if(all(!fileExists)) {
                 stop(paste("Error in ",funVersion,": None of the files specified in 'savFiles' were found in ",path,".\n",sep=""))
              }   
              if(!all(fileExists)) {
                 cat(paste(funVersion,": Following files specified in 'savFiles' were not found in ",path,".\n",sep=""))
                 notFoundFiles <- savFiles[!fileExists]
                 FoundFiles    <- savFiles[fileExists]
                 cat(paste(notFoundFiles,collapse=", ")) 
                 cat("\nOnly found files will be read in.\n")
                 savFiles      <- savFiles[fileExists]
              } 
           }     
           if(is.null(savFiles)) {
              savFiles <- list.files(path=path,pattern=".sav|.SAV",recursive=FALSE) 
              if(length(savFiles)==0) {
                 stop(paste("No '.sav'-files found in ",path,".\n",sep=""))
              }   
           cat(paste(funVersion,": Found ", length(savFiles), " 'savFiles' in ",path,".\n",sep=""))
           }
           ### hier beginnt das eigentliche Einlesen
           allDataFrames <- NULL
           for (i in seq(along=savFiles)) {

				suppressWarnings( file.i <- data.frame(read.spss(file.path(path,savFiles[i]),to.data.frame=FALSE, use.value.labels=FALSE), stringsAsFactors=FALSE) )
                idCol  <- unique(unlist(lapply(oldIDS, FUN=function(ii) {grep(ii,colnames(file.i))})))
                if(length(idCol)<1) {
                   stop(paste("Error in ",funVersion,": None of the specified 'oldIDS' were found in dataset ",savFiles[i],".\n",sep="")) 
                }
                if(length(idCol)>1) {
                   stop(paste("Error in ",funVersion,": More than one of the specified 'oldIDS' were found in dataset ",savFiles[i],".\n",sep="")) 
                }
                colnames(file.i)[idCol] <- newID
                ### Leerzeichen abschnipseln 
                if(truncateSpaceChar == TRUE)  {
                    # FALSCH
					#file.i <- do.call("data.frame", lapply(file.i, crop))                        
					file.i <- do.call("data.frame", list ( lapply(file.i, crop), stringsAsFactors = FALSE ) )
				} else {
					# Umwandlung nach Character ( wird bei if(truncateSpaceChar == TRUE) auch schon mit impliziert )
					file.i <- set.col.type ( file.i )
				}
                
				# Check auf alles character
				stopifnot ( all ( sapply ( file.i , is.character ) ) )
				
				### Stelligkeitskorrektur
                if(correctDigits == TRUE) {
                   colsToCorrect <- lapply(1:ncol(file.i), FUN=function(ii) { sort(unique(nchar(file.i[,ii])))})        
                   options(warn = -1)                                          
                   colsToCorrect <- which( unlist( lapply(colsToCorrect,  FUN=function(ii) { all(ii == c(1,2)) })) ) 
                   options(warn = 0)
                   if(length(colsToCorrect)>0) {
                      cat(paste(funVersion,": ",length(colsToCorrect)," columns are corrected for column width in dataset ",savFiles[i],".\n",sep=""))
                      for (ii in colsToCorrect) {
                           file.i[,ii] <- gsub(" ","0", formatC(as.character(file.i[,ii]),width=2))
                      }
                   }
                }   
                allDataFrames[[i]] <- file.i
           }              
          return(allDataFrames)
          }    
              
  