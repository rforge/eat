####################################################################################################################
#
# function:
#     .automateModels.monitor.progress ( FolderList , analyse.name , software, refresh=5 , time.out=240, 
#											email= NULL, smtpServer= NULL)
#
# description: 
#     überwacht Fortschritt bei Software-spezifischen Läufen
#
# arguments:
#     folder (list) : Liste mit JobFoldern
#	  folder.out (character) : relative Subfoldern, wo Conquest-Output liegt --> LISTE ?!?!
#     analys.name (list) : korrespondierende Liste mit Analysenamen
#     software (character list) : derzeit nur "conquest"
#     refresh (numeric Skalar) : Zeitintervall in Minuten, nachdem erneut geguckt wird
#     time.out (numeric Skalar) : Abbruchkriterium (Zeit in Minuten, die Analyse höchstens benötigen darf)
#	  email (character) : eigene Emailadresse
#	  smtpServer (character) : smtp Server
#
# Version: 	0.4.0
# Imports: package sendmailR
# Status: development
# Published:
# Author:   Karoline Sachse
# Maintainer:
#
# Change Log:
# 2011-11-28 KS
# CHANGED: display monitoring information only once per analysis in .automateModels.monitor.progress
# 0000-00-00 AA
# 28.11.2011 KS: folder.out nicht wie Liste behandeln
# 25.11.2011 KS: nicht mehr so viele Wiederholungen
# 17.10.2011 KS: Pfadangabe hinten angestellt
# 14.10.2011 MH: Ausgaben auf Englisch
# 08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
# 16.07.2011 (MH): folder.out ergänzt
# 24.06.2011 KS: Funktion erstellt
#
####################################################################################################################

.automateModels.monitor.progress <- function( FolderList , folder.out , analyse.name , software, refresh=5 , time.out=240, 
												email = NULL, smtpServer = NULL ) {
	
	eatTools:::sunk("automateModels.monitor.progress: Start monitoring ... \n")
	
	# 14.10.2011 MH: Paket wird schon früher gesourct, deshalb auskommentiert
	# if(!is.null(email)) { 
		# if(inherits( try( library(sendmailR)) ,"try-error")){
		# eatTools:::sunk("Es wird keine Email kommen, da das Paket sendmailR nicht geladen werden konnte.\n")
		# eatTools:::sunk("Package sendmailR could not be loaded.\n")
		# }
	# }	
	
    # Checks
    stopifnot (is.list (FolderList) )
    stopifnot (is.list (analyse.name) )
    stopifnot (identical(length(analyse.name), length(FolderList)) )
	
	if ( is.null ( smtpServer ) ) smtpServer <- "mailhost.cms.hu-berlin.de"
	
    p1 <- proc.time()
	a <- 0
	ret <- FolderList
	ret[seq(along=ret)] <- FALSE
	tr <- as.data.frame(NULL)
	communicated1 <- communicated2 <- ret

    repeat{
		for(i in which(!unlist(ret))) {
			if(software[[i]] == "conquest") {	
				if (!is.null(folder.out)) {
						check.folder <- paste(FolderList[[i]],folder.out,sep="")
					} else {
						check.folder <- FolderList[[i]]
					}
				if (!identical(dir(check.folder), character(0))) {
					if(any( grep( "laji", dir(check.folder) ) ) ) {
						ret[[i]] <- FALSE
						# eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Die Conquest-Analyse \'",analyse.name[[i]],"\' in: \'", FolderList[[i]], "\' läuft noch. \n", sep =""))
						if(communicated1[[i]] == FALSE) {
							eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Conquest-Analysis \'",analyse.name[[i]], "\' is running. In: \'", check.folder, "\' \n", sep =""))
							communicated1[[i]] <- TRUE
						}
					} else {
						if (any( paste(analyse.name[[i]], ".shw", sep ="") %in% dir(check.folder ) ) ) {
							ret[[i]] <- TRUE
							# eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Die Conquest-Analyse \'",analyse.name[[i]],"\' in \'", FolderList[[i]], "\' ist fertig !!! \n", sep =""))
							eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Conquest-Analysis \'",analyse.name[[i]], "\' is done !!! In: \'", check.folder, "\' \n", sep =""))
						} else {
							ret[[i]] <- FALSE
							if(communicated2[[i]] == FALSE) {
							# eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Die Conquest-Analyse \'",analyse.name[[i]],"\' in \'", FolderList[[i]], "\' wurde noch nicht gestartet. \n", sep ="")) #\n Weder \'laji...\' noch \'.shw\' auffindbar.
								eatTools:::sunk(tr[i+a,1] <- paste(Sys.time(), ": Conquest-Analysis \'",analyse.name[[i]],"\' has not been started yet. In: \'", check.folder, "\' \n", sep ="")) #\n Weder \'laji...\' noch \'.shw\' auffindbar.
								communicated2[[i]] <- TRUE
							}
						}
					}	
				} else { ret[[i]] <- FALSE
						 # eatTools:::sunk( tr[i+a,1] <-  paste(Sys.time(), ": Das Verzeichnis: \'", FolderList[[i]], "\' ist leer. \n", sep =""))}
						 eatTools:::sunk( tr[i+a,1] <-  paste(Sys.time(), ": Folder: \'", check.folder, "\' is not empty. \n", sep =""))}
			} else {
				eatTools:::sunk(paste("automateModels.monitor.progress: monitoring for other software than ConQuest still not implemented yet."))
			}
		flush.console()
		}
		a <- a + length(FolderList) #[!unlist(ret)]		
		
		# if (all(ret == TRUE)) {eatTools:::sunk(paste(Sys.time(), ": Alle Analysen wurden erfolgreich abgeschlossen! \n", sep =""))		
		if (all(ret == TRUE)) {eatTools:::sunk(paste(Sys.time(), ": All analyses terminated successfully! \n", sep =""))		
								if(!is.null(email)) {
									tr <-  tr[-which(is.na(tr)),]
									# body <- list(paste(Sys.time(), ": Alle Analysen wurden erfolgreich abgeschlossen!", sep =""), try(mime_part(tr, row.names=FALSE, col.names=FALSE)))
									body <- list(paste(Sys.time(), ": All analyses terminated successfully!", sep =""), try(mime_part(tr, row.names=FALSE, col.names=FALSE)))
									# subject <- ".automateModels.monitor.progress erfolgreich beendet"
									subject <- ".automateModels.monitor.progress terminated successfully"
									from <- to <- email								
									if(inherits(try(sendmail(from, to, subject, body, control=list(smtpServer=smtpServer))),"try-error")){
									# eatTools:::sunk(paste("automateModels.monitor.progress: Fehler beim versenden der Email.\n",sep=""))}
									eatTools:::sunk(paste("automateModels.monitor.progress: Error while sending email.\n",sep=""))}
								}
								return(ret)
								break}		

		z <- round((proc.time()-p1)[[3]]/60, 1)
		# if (z > time.out) {eatTools:::sunk( paste(Sys.time(), ": Time-Out erreicht!! Monitoring wird abgebrochen. \n", sep =""))
		if (z > time.out) {eatTools:::sunk( paste(Sys.time(), ": Time-Out!! Monitoring aborted. \n", sep =""))
							if(!is.null(email)) {
									tr <-  tr[-which(is.na(tr)),]
									body <- list(paste(Sys.time(), ": Time-Out!! Monitoring aborted.", sep =""), try(mime_part(tr, row.names=FALSE, col.names=FALSE)))
									subject <- ".automateModels.monitor.progress terminated"
									from <- to <- email
									if(inherits(try(sendmail(from, to, subject, body, control=list(smtpServer=smtpServer))),"try-error")){
									eatTools:::sunk(paste("automateModels.monitor.progress: Error while sending email.\n",sep=""))}
							}
							return(ret)
							break}
							
		Sys.sleep(refresh*59)
     
    }
          
}

# FolderList <-  list("p:/ZKD/temp/10_Test/unidim/BF", "p:/ZKD/temp/10_Test/unidim/BF")
# software <- list("conquest", "conquest")
# analyse.name <- list("BF__sf_dich.Gy__REGR_bl_sf", "BF__grade.9__REGR_bl_sf")
# test <- .automateModels.monitor.progress(FolderList, list("sf_dich.Gy", "grade.9"), analyse.name, software, refresh=1/10, time.out=5, email="sachseka@hu-berlin.de" )


