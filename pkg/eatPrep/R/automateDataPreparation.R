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
						mnrFunction = c( "recodeMbiToMnr" , "mnrCoding" ),
						filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, nMbi = 2,
						rotation.id = NULL,
						aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
            correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
            missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0), verbose=FALSE) {
							 
		### Funktionsname für Meldungen
		f. <- "automateDataPreparation"
		f.n <- paste ( f. , ":" , sep = "" )


		###folder erstellen
		if( is.null ( path ) ) {path <- getwd()}
		folder.e <- path
		folder.aDP <- file.path ( path , "_eat_writeSPSS_" )
		if ( ! file.exists ( folder.aDP ) ) { dir.create ( folder.aDP , recursive = TRUE ) }		
	
		### Begrüßung
		if(verbose) cat ( "\n" )
		if(verbose) cat ( paste (f.n , "Starting automateDataPreparation", Sys.time(), "\n" ) ) 
		
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
		stopifnot(is.logical(verbose))
		
		if(is.null(datList)) {
			stopifnot(readSpss == TRUE)
			stopifnot(class(inputList$savFiles) == "data.frame")
		}
	
		### ggf. sav-files einlesen
		if( readSpss ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Load .sav Files\n" ) )
			if(!is.null(datList)) {
				warning(paste ( f.n , "If readSpss == TRUE, datList will be ignored." ) )
			}
		
			savFiles <- inputList$savFiles$filename
			if( is.null (oldIDs) ) {oldIDs <- inputList$savFiles$case.id}
			if( is.null (newID) ) {
				if( !is.null (inputList$newID$value[which(inputList$newID$key == "master-id")]) ) {
					newID <- inputList$newID$value[which(inputList$newID$key == "master-id")]
				}
			}
			if( is.null (newID) ) {newID <- "ID"}
			
			# MH 10.01.2013
			# Problem: folder.e wird oben auf path gesetzt
			# wenn path=NULL wird auf getwd() defaultet
			# in getwd() liegen natürlich nicht die savFiles
			# da ich nicht alle Kombinationen überblicke wird hier folgendermaßen minimalinvasiv gehotfixed:
			# wenn file.path (folder.e, savFiles) nicht existent wird fullname aus datList genommen
			# wenn dann immernoch nicht existent, dezidierter Abbruch, da es dann ja keine Daten gibt
			fulln <- inputList$savFiles$fullname
			names(fulln) <- inputList$savFiles$filename
			fls <- file.path (folder.e, savFiles)
			ex <- sapply ( fls , file.exists )
			fls2 <- unname ( mapply ( function ( fls, ex , fulln ) if ( ex ) fls else fulln[basename(fls)] , fls , ex , MoreArgs = list ( fulln ) ) )
			ex2 <- sapply ( fls2 , file.exists )
			fls3 <- fls2[ex2]
			
			if ( ! identical ( fls3 , character(0) ) ) {
					# MH 10.01.2013
					# Problem: bei nur einem File crashts unten bei stopifnot ( class ( datList ) == "list" )
					# deshalb wird hier noch SIMPLIFY=FALSE eingefügt
					dat <- datList <- mapply(readSpss, file = fls3, oldID = oldIDs,
						MoreArgs = list(correctDigits=correctDigits, truncateSpaceChar = truncateSpaceChar, newID = newID ),
						SIMPLIFY=FALSE)
			} else {
					stop ( "No data available. Check 'datList', 'inputList' and/or 'path'." )
			}

		}
		stopifnot ( class ( datList ) == "list" )		
		stopifnot ( class ( inputList ) == "list" )
		if( is.null (oldIDs) ) {oldIDs <- inputList$savFiles$case.id}
		stopifnot ( !is.null (oldIDs) )

		if( checkData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Check data...\n" ) )
			mapply(checkData, datList, MoreArgs = list(inputList$values, inputList$subunits, inputList$units, verbose))
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Check has been skipped\n" ) )}

		# MH 10.01.13 für user convenience hier noch ne Warnung wenn
		# mergeData=FALSE ist, aber mehrere Datensätze in der Liste
		# (bricht zwar dann unten eh ab, aber warum nicht)
		if ( !mergeData & length ( datList ) > 1 ) {
				warning ( "datList contains more than 1 data.frame" )
		}
		
		if( mergeData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start merging\n" ) )
			if( readSpss) {oldIDs <- rep(newID, length(datList))}
			if(is.null(newID)) {newID <- "ID"}
			dat <- mergeData(newID = newID, datList = datList, oldIDs = oldIDs, addMbd=TRUE, verbose=verbose)
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Merge has been skipped\n" ) )}


# MH 11.01.2013: temporär zum debuggen, kann wieder rausgenommen werden		
# browser()		
# dat.o <- dat
# i <- c("M3319A01a","M3312A01n","M3312A01m","M3312A01l","M3312A01k",
# "M3312A01j","M3312A01i","M3312A01h","M3312A01g","M3312A01f","M3312A01e",
# "M3312A01d","M3312A01c","M3312A01b","M3312A01a")

		
		# dat <- dat[, c("idstud",i) ]

		
		if( recodeData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start recoding\n" ) )

			# MH 10.01.13
			# bei mergeData = FALSE ist das hier noch ne Liste mit Data.frame(s)
			# bei einem data.frame ist das völlig ok, da wird jetzt "unlisted"
			# bei mehreren stoppts
			if ( !is.data.frame ( dat ) & is.list ( dat ) ) {
					if ( ( l <- length ( dat ) ) > 1 ) {
							stop ( "Your data contains more then one data.frame. Use option 'mergeData=TRUE'." )
					} else if ( l == 1 ) {
							dat <- dat[[1]]
					} else {
							dat <- NULL
					}
			}
			if ( is.data.frame(dat)) {
					if ( nrow ( dat ) == 0 | ncol ( dat ) == 0 ) dat <- NULL
			}
			if (!is.data.frame(dat)) {
					stop ( "internal error: 'dat' is not a data.frame or empty" )
			}

			dat <- recodeData (dat= dat, values=inputList$values, subunits=inputList$subunits, verbose=verbose)
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Recode has been skipped\n" ) )}

		if( recodeMnr ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start recoding Mbi to Mnr\n" ) )
			if(is.null(inputList$booklets)) {warning ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$booklets. Data frame not available!\n" ) ) }
			if(is.null(inputList$blocks)) {warning ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$blocks. Data frame not available!\n" ) ) }

			# MH 10.01.13
			# neuer Parameter rotation.id
			# wenn dieser (valid) gesetzt ist wird inputList$rotation gesetzt
			# nur mal so interessehalber: wo soll "rotation" überhaupt sonst herkommen? konnt mir auch keiner sagen :-)
	
			if( is.null(inputList$rotation) ) {
					if (is.character(rotation.id)) {
							if ( rotation.id %in% colnames ( dat ) ) {
									idname <- inputList$newID[inputList$newID$key=="master-id","value"]
									if ( idname %in% colnames ( dat ) ) {
											inputList$rotation <- dat[,c(idname,rotation.id),drop=FALSE]
									}
							}
					}
			}
		
			if(is.null(inputList$rotation)) {
					warning ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$rotation. Data frame not available!\n" ) )
			}
			
			if ( any ( is.null(inputList$booklets), is.null(inputList$blocks), is.null(inputList$rotation) ) ) {
					warning ( "RecodeMnr had to be skipped due to missing input variables.\n" )
			} else {
					# Funktion auswählen
					if ( length ( mnrFunction ) > 1 ) {
							mnrFunction <- mnrFunction[1]
					} else {
							if ( ! mnrFunction %in% c( "recodeMbiToMnr" , "mnrCoding" ) ) {
									mnrFunction <- "recodeMbiToMnr"
							}
					}
					if ( mnrFunction == "recodeMbiToMnr" ) {
							dat <- recodeMbiToMnr(dat = dat, id = newID, rotation.id = rotation.id, booklets = inputList$booklets, blocks = inputList$blocks, rotation = inputList$rotation, breaks, nMbi = nMbi, subunits = inputList$subunits, verbose = verbose)
					} else if ( mnrFunction == "mnrCoding" ) {
							# dat <- eatPrep:::mnrCoding ( dat = dat , pid = newID , rotation.id = rotation.id , blocks = inputList$blocks , booklets = inputList$booklets , breaks = breaks , subunits = inputList$subunits , nMbi = nMbi  , mbiCode = "mbi" , mnrCode = "mnr" , invalidCodes = c ( "mbd", "mir", "mci" ) , verbose = verbose )
							# MH 14.01.2013: zum debuggen/developen ist "eatPrep:::"mnrCoding extrem scheisse :D (wieder mal selbst verarscht)
							dat <- mnrCoding ( dat = dat , pid = newID , rotation.id = rotation.id , blocks = inputList$blocks , booklets = inputList$booklets , breaks = breaks , subunits = inputList$subunits , nMbi = nMbi  , mbiCode = "mbi" , mnrCode = "mnr" , invalidCodes = c ( "mbd", "mir", "mci" ) , verbose = verbose )
					}
			}
			
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "RecodeMnr has been skipped\n" ) )}


# MH 11.01.2013: temporär zum debuggen, kann wieder rausgenommen werden
# dat.o <- dat
# i <- c("M3319A01aR","M3312A01nR","M3312A01mR","M3312A01lR","M3312A01kR",
# "M3312A01jR","M3312A01iR","M3312A01hR","M3312A01gR","M3312A01fR","M3312A01eR",
# "M3312A01dR","M3312A01cR","M3312A01bR","M3312A01aR")
# dat <- dat[, c("idstud",i) ]
# browser()

		
		
		if( aggregateData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start aggregating\n" ) )
#			if ( aggregatemissings == "seeInputList" ) {
#				stopifnot(!is.null(inputList$aggrMiss))
#				aMiss <- unname(inputList$aggrMiss)
#				aMiss[,8] <- rep("err", 7)
#				aMiss[8,] <- rep("err", 8)
#				aggregatemissings <- as.matrix(aMiss, nrow=8, ncol=8)
#			}		
			dat <- aggregateData (dat=dat, subunits=inputList$subunits, units=inputList$units,
            aggregatemissings = aggregatemissings, rename = rename, recodedData = recodedData, verbose = verbose)
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Aggregate has been skipped\n" ) )}
	
		if( scoreData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start scoring\n" ) )
				dat <- scoreData (dat= dat, unitrecodings=inputList$unitRecodings, units=inputList$units, verbose = verbose)
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Scoring has been skipped\n" ) )}
	
		if( writeSpss ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Writing dataset in last transformation status to disk\n" ) )
			if (class(dat) != "data.frame") {
				warning ( paste ( f.n , "Data is no data frame (data frames probably need to be merged).\n" ) )
			}
			if(inherits(try( writeSpss (dat=dat , values=inputList$values, subunits=inputList$subunits, units=inputList$units,
					filedat = filedat, filesps = filesps, missing.rule = missing.rule,
					path = folder.aDP, sep = "\t", dec = ",", verbose = verbose)  ),"try-error")) {
				if(verbose) cat ( "\n" )	
				warning ( paste ( f.n , "No SPSS-File could be written.\n" ) )
			}
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "No SPSS-File has been written.\n" ) )}
		
		# finale Ausgabe 
		if(verbose) cat ( "\n" )
		if(verbose) cat ( paste ( f.n , "terminated successfully!", Sys.time(), "\n\n" ) )
	
		# Ergebnisse returnen
		return ( dat )

}

# data(inputList)
# data(inputDat)
# test <- automateDataPreparation(inputList = inputList, datList = inputDat,
        # path = "c:/temp/test_eat", readSpss = FALSE, checkData=TRUE,
        # mergeData = TRUE, recodeData=TRUE, recodeMnr = TRUE, breaks = c(1,2),
		    # aggregateData=TRUE, scoreData= TRUE, writeSpss=TRUE, verbose=TRUE)


