####################################################################################################################
#
# function:
#     .automateModels.createsubfolders ( FolderList )
#
# description:
#     legt Unterverzeichnisse nach vorgegebener Liste an
#
# arguments:
#      FolderList (list) :  Liste mit Verzeichnisnamen
#
# Version: 	0.3.0
# Imports:
# Published:
# Author:   Karoline Sachse, Martin Hecht
# Maintainer:
#
# Change Log:
#		14.10.2011 MH: Ausgabe auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		22.06.2011 (KS):  Abbruch wenn Verzeichnis schon existiert und nicht leer
#
####################################################################################################################


.automateModels.createsubfolders <- function( FolderList , folder ){

    # Plausicheck: Ist FolderList Liste?
    stopifnot ( is.list (FolderList) )

	# Haupt-Folder aus check/create-Liste entfernen, den gibts nämlich schon mit Daten drin
	del <- which ( FolderList %in% folder )
	if ( ! identical ( del , integer(0) ) ) FolderList <- FolderList[-del]

	# wenn Liste nicht leer, dann action 
	if ( ! identical ( FolderList , list() ) ) {
	
			# Welche Folder enthalten schon Dateien?
			for(i in seq(along =FolderList)) {
			   if(!identical(dir(FolderList[[i]]), character(0))) {
			   # ret <- FALSE; sunk(paste("Es liegen bereits Dateien in: ", FolderList[[i]], "\n", sep =""))
			   ret <- FALSE; sunk(paste("Data already exists in: ", FolderList[[i]], "\n", sep =""))
			   
			   }
			}
			# Abbruch
			for(i in seq(along =FolderList)) {
			   stopifnot(identical(dir(FolderList[[i]]), character(0)))
			}

			# Unterverzeichnisse anlegen
			for(i in seq(along =FolderList)) {
				if(inherits(ret <- try(dir.create(FolderList[[i]], showWarnings = FALSE, recursive=TRUE)),"try-error")) {
				# ret <- FALSE; sunk("Fehler beim Anlegen der Verzeichnisse. ")
				ret <- FALSE; sunk("Error while creating folder.")
				}
			}
			
			# Schreibrechte vorhanden?
			for(i in seq(along =FolderList)) {
				if(unname ( file.access( FolderList[[i]], mode = 2 ) )  == 0 ) {
				   ret <- TRUE
				   } else {
					ret <- FALSE
					# sunk(paste("Keine Schreibrechte in ", FolderList[[i]], sep=""))
					sunk(paste("no writing access", FolderList[[i]], sep=""))
					}
			}
    } else ret <-TRUE
	
	return(ret)
 }
 

