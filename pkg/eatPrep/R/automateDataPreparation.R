# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# automateDataPreparation
# Description: automates data preparation
# Version: 	0.6.0
# Status: alpha
# Release Date: 	2011-11-13
# Author:    Karoline Sachse
# Change Log:
# 2012-01-10 KS
# ADDED: mergeData argument writeLog=TRUE
# 2011-11-25 KS
# ADDED: argument aggregatemissings, several checks in automateDataPreparation
# CHANGED: loadSav gets information from inputList, inputDat can be NULL in automateDataPreparation
# 0000-00-00 AA
# 2011-11-24 (KS): units an zweite recode
# 2011-11-23 (NH): source-calls rausgenommen
# 2011-11-22 (KS): verschiedenes
# 2011-11-15 (KS): erste lauffähige Version
# 2011-11-13 (KS): Fkt erstellt
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# function:
# 		automateDataPreparation ( inputDat, inputList )
#
# description:
#		load .sav datasets, check, merge, recode, aggregate, recode, write2SPSS
#
# arguments:
#		inputDat (list OR dataframe): list of dataframes OR single dataframe of the following type:
#							~ [,1] first column: .sav filenames (Excluding path --> set as argument "folder")
#							~ [,2] 2nd column: ID name in corresponding .sav file
#							~ [,3] 3rd column: 1st element contains 
#		inputList (list of 3 dataframes): values, units, subunits 
# 		readSpss (logical)
#		checkData
#		mergeData
#		recodeData
#		aggregateData
#		scoreData
#		writeSpss
#		filedat (txt file path and name for spss)
#		filesps (sps file path and name)
#		folder (character): folder for logfile, default: getwd()
#		aggregatemissings
#		rename
#		correctDigits
# 		truncateSpaceChar
#		newID
#		oldIDs
#		missing.rule
#
# example:
#		p:\ZKD\02_Beispieldaten\Karoline\inputList.RData
#	    p:\ZKD\02_Beispieldaten\Karoline\inputDat.RData
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

automateDataPreparation <- function(datList = NULL, inputList, path = NULL, 
						readSpss, checkData,  mergeData , recodeData, recodeMnr = FALSE,
						aggregateData, scoreData, writeSpss, 
						filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, nMbi = 2,
						aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
            correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
            missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0)) {
							 
		### Funktionsname für Meldungen
		f. <- "automateDataPreparation"
		f.n <- paste ( f. , ":" , sep = "" )


		###folder erstellen
		if( is.null ( path ) ) {path <- getwd()}
		folder.e <- path
		folder.aDP <- file.path ( path , "_automateDataPreparation_" )
		if ( ! file.exists ( folder.aDP ) ) { dir.create ( folder.aDP , recursive = TRUE ) }		
	
		###source.it.all sourcen
	#	source ( "p:/ZKD/source.it.all.R" )
	#	source.it.all ( folder = "p:/ZKD/development" ,develop.modules = c("loadSav", "recodeData")) #, "checkData", "mergeData", "recodeData", 
			# "aggregateData", "crop", "writeSpss"))
	#	source.it.all ( folder = "p:/ZKD/ZKDaemon" , develop.modules = "zkdHelpers" )	
	#	zkdHelpers_LoadPackage ( "car" )
	#	zkdHelpers_LoadPackage ( "foreign" )		
	
		### logfile initieren
		sunk.path <- file.path ( folder.aDP , "automateDataPreparation.Log.txt" )
		
		### Begrüßung
		eatTools:::sunk ( "\n" )
		eatTools:::sunk ( paste (f.n , "Starting automateDataPreparation", Sys.time(), "\n" ) ) 
		
		### Checks
		if(!is.null(newID)) {
			stopifnot(is.character(newID))
			stopifnot(length(newID) == 1)
			}
		stopifnot(is.logical(readSpss))
		stopifnot(is.logical(checkData))
		stopifnot(is.logical(mergeData))
		stopifnot(is.logical(recodeData))
		stopifnot(is.logical(recodeMnr))
		stopifnot(is.logical(aggregateData))
		stopifnot(is.logical(scoreData))
		stopifnot(is.logical(writeSpss))
		stopifnot(is.logical(correctDigits))
		stopifnot(is.logical(truncateSpaceChar))
		
		if(is.null(datList)) {
			stopifnot(readSpss == TRUE)
			stopifnot(class(inputList$savFiles) == "data.frame")
		}
		
		### ggf. sav-files einlesen
		if( readSpss) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Load .sav Files\n" ) )
			if(!is.null(datList)) {
				eatTools:::sunk(paste ( f.n , "If readSpss == TRUE, datList will be ignored." ) )
			}
			savFiles <- inputList$savFiles$filename
			if( is.null (oldIDs) ) {oldIDs <- inputList$savFiles$case.id}
			if( is.null (newID) ) {
				if( !is.null (inputList$newID$value[which(inputList$newID$key == "master-id")]) ) {
					newID <- inputList$newID$value[which(inputList$newID$key == "master-id")]
				}
			}
			if( is.null (newID) ) {newID <- "ID"}
			dat <- datList <- lapply(file.path (folder.e, savFiles), readSpss,
                  correctDigits=correctDigits, truncateSpaceChar = truncateSpaceChar, oldIDs = oldIDs, newID = newID )
		} 			
		stopifnot ( class ( datList ) == "list" )		
		stopifnot ( class ( inputList ) == "list" )
		if( is.null (oldIDs) ) {oldIDs <- inputList$savFiles$case.id}
		stopifnot ( !is.null (oldIDs) )
		
		if( checkData ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Check data...\n" ) )
			mapply(checkData, datList, MoreArgs = list(inputList$values, inputList$subunits, inputList$units))
		} else {eatTools:::sunk ( paste ( f.n , "Check has been skipped\n" ) )}
		
		if( mergeData ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Start merging\n" ) )
			if( readSpss) {oldIDs <- rep(newID, length(datList))}
			if(is.null(newID)) {newID <- "ID"}
			dat <- mergeData(newID = newID, datList = datList, oldIDs = oldIDs, addMbd=TRUE, writeLog=TRUE)
		} else {eatTools:::sunk ( paste ( f.n , "Merge has been skipped\n" ) )}
		
		if( recodeData ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Start recoding\n" ) )
			dat <- recodeData (dat= dat, values=inputList$values, subunits=inputList$subunits)
		} else {eatTools:::sunk ( paste ( f.n , "Recode has been skipped\n" ) )}
		
		if( recodeMnr ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Start recoding Mbi to Mnr\n" ) )
			if(is.null(inputList$booklets)) {eatTools:::sunk ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$booklets. Data frame not available!\n" ) ); stop()}
			if(is.null(inputList$blocks)) {eatTools:::sunk ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$blocks. Data frame not available!\n" ) ); stop()}
			if(is.null(inputList$rotation)) {eatTools:::sunk ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$rotation. Data frame not available!\n" ) ); stop()}
			dat <- recodeMbiToMnr(dat = dat, id = newID, booklets = inputList$booklets, blocks = inputList$blocks, rotation = inputList$rotation, breaks, nMbi = nMbi, subunits = inputList$subunits)
		} else {eatTools:::sunk ( paste ( f.n , "RecodeMnr has been skipped\n" ) )}
						
			
		if( aggregateData ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Start aggregating\n" ) )
#			if ( aggregatemissings == "seeInputList" ) {
#				stopifnot(!is.null(inputList$aggrMiss))
#				aMiss <- unname(inputList$aggrMiss)
#				aMiss[,8] <- rep("err", 7)
#				aMiss[8,] <- rep("err", 8)
#				aggregatemissings <- as.matrix(aMiss, nrow=8, ncol=8)
#			}		
			dat <- aggregateData (dat=dat, subunits=inputList$subunits, units=inputList$units,
            aggregatemissings = aggregatemissings, rename = rename, recodedData = recodedData)
		} else {eatTools:::sunk ( paste ( f.n , "Aggregate has been skipped\n" ) )}
		
		if( scoreData ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Start scoring\n" ) )
			dat <- scoreData (dat= dat, unitrecodings=inputList$unitRecodings, units=inputList$units)
		} else {eatTools:::sunk ( paste ( f.n , "Scoring has been skipped\n" ) )}
	
		if( writeSpss ) {
			eatTools:::sunk ( "\n" )
			eatTools:::sunk ( paste ( f.n , "Writing dataset in last transformation status to disk\n" ) )
			if (class(dat) != "data.frame") {
				eatTools:::sunk ( paste ( f.n , "Data is no data frame (data frames probably need to be merged).\n" ) )
			}
			if(inherits(try( writeSpss (dat=dat , values=inputList$values, subunits=inputList$subunits, units=inputList$units,
					filedat = filedat, filesps = filesps, missing.rule = missing.rule,
					path = folder.aDP, sep = "\t", dec = ",", silent = FALSE)  ),"try-error")) {
				eatTools:::sunk ( "\n" )	
				eatTools:::sunk ( paste ( f.n , "No SPSS-File could be written.\n" ) )
			}
		} else {eatTools:::sunk ( paste ( f.n , "No SPSS-File has been written.\n" ) )}
		
		# finale Ausgabe 
		eatTools:::sunk ( "\n" )
		eatTools:::sunk ( paste ( f.n , "terminated successfully!", Sys.time(), "\n\n" ) )
	
		# Ergebnisse returnen
		return ( dat )

}



