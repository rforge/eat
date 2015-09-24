automateDataPreparation <- function(datList = NULL, inputList, path = NULL, 
						readSpss, checkData,  mergeData , recodeData, recodeMnr = FALSE,
						aggregateData, scoreData, writeSpss, collapseMissings = FALSE,
						filedat = "mydata.txt", filesps = "readmydata.sps", breaks=NULL, nMbi = 2,
						rotation.id = NULL, suppressErr = FALSE, recodeErr = "mci",
						aggregatemissings = NULL, rename = TRUE, recodedData = TRUE, 
            correctDigits=FALSE, truncateSpaceChar = TRUE, newID = NULL, oldIDs = NULL, 
            missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0), verbose=FALSE) {
							 
		### Funktionsname fuer Meldungen
		f. <- "automateDataPreparation"
		f.n <- paste ( f. , ":" , sep = "" )

		###folder erstellen
		if( is.null ( path ) ) {path <- getwd()}
		folder.e <- path
		folder.aDP <- file.path ( path , "_eat_writeSPSS_" )
		if ( ! file.exists ( folder.aDP ) ) { dir.create ( folder.aDP , recursive = TRUE ) }		
	
		### Begruessung
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
		stopifnot(is.logical(collapseMissings))
		stopifnot(is.logical(correctDigits))
		stopifnot(is.logical(truncateSpaceChar))
		stopifnot(is.logical(verbose))
		
		if(is.null(datList)) {
			stopifnot(readSpss == TRUE)
			stopifnot(class(inputList$savFiles) == "data.frame")
		} else {
			stopifnot(class(datList) == "data.frame" || class(datList) == "list")
			if(class(datList) == "data.frame") {datList <- list(datList)}
		}
	
		### ggf. sav-files einlesen
		idname <- NULL
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

			fulln <- inputList$savFiles$fullname
			names(fulln) <- inputList$savFiles$filename
			fls <- file.path (folder.e, savFiles)
			ex <- sapply ( fls , file.exists )
			fls2 <- unname ( mapply ( function ( fls, ex , fulln ) if ( ex ) fls else fulln[basename(fls)] , fls , ex , MoreArgs = list ( fulln ) ) )
			ex2 <- sapply ( fls2 , file.exists )
			fls3 <- fls2[ex2]
			
			if ( ! identical ( fls3 , character(0) ) ) {
					dat <- datList <- mapply(readSpss, file = fls3, oldID = oldIDs,
						MoreArgs = list(correctDigits=correctDigits, truncateSpaceChar = truncateSpaceChar, newID = newID ),
						SIMPLIFY=FALSE)
			} else {
					stop ( "No data available. Check 'datList', 'inputList' and/or 'path'." )
			}
		}
		
		# Checks
		stopifnot ( class ( datList ) == "list" )		
		stopifnot ( class ( inputList ) == "list" )
		if( is.null (oldIDs) ) {oldIDs <- inputList$savFiles$case.id}
		stopifnot ( !is.null (oldIDs) )

		if( checkData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Check data...\n" ) )
			mapply(checkData, datList, names(datList), MoreArgs = list(inputList$values, inputList$subunits, inputList$units, verbose))
		} else {if(verbose) cat ( "\n" )	
			if(verbose) cat ( paste ( f.n , "Check has been skipped\n" ) )}

		# ne Warnung wenn
		# mergeData=FALSE ist, aber mehrere Datensaetze in der Liste
		if ( !mergeData & length ( datList ) > 1 ) {
				warning ( "More than one data.frame has been loaded, a list of datasets will be returned \n" )
		} 
		
		if( mergeData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start merging\n" ) )
			if( readSpss) {oldIDs <- rep(newID, length(datList))}
			if(is.null(newID)) {newID <- "ID"}
			dat <- mergeData(newID = newID, datList = datList, oldIDs = oldIDs, addMbd=TRUE, verbose=verbose)
			aaa <- FALSE
		} else {
			aaa <- TRUE
			if(verbose) cat ( "\n" )	
			if(verbose) cat ( paste ( f.n , "Merge has been skipped\n Only the first dataset in datList will be considered for the following steps\n" ) )
			dat <- datList[[1]]
			idname <- oldIDs[1]
		}
		
		if( recodeData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start recoding\n" ) )

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
		
		if(is.null(inputList$rotation) & is.null(rotation.id)) {
				warning ( paste ( f.n , "Recoding Mnr in automateDataPreparation requires inputList$rotation or rotation.id. These are not available!\n" ) )
		}
			
		if ( any ( is.null(inputList$booklets), is.null(inputList$blocks) ) ) {
			warning ( "RecodeMnr had to be skipped due to missing input variables.\n" )
		} else {
			if(is.null(rotation.id)) {
				dat <- mergeData(newID, list(dat, inputList$rotation))
				rotation.id <- names(inputList$rotation)[2]
			}
			dat <- mnrCoding ( dat = dat , pid = newID , rotation.id = rotation.id , blocks = inputList$blocks , booklets = inputList$booklets , breaks = breaks , subunits = inputList$subunits , nMbi = nMbi  , mbiCode = "mbi" , mnrCode = "mnr" , invalidCodes = c ( "mbd", "mir", "mci" ) , verbose = verbose )
		}
		
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "RecodeMnr has been skipped\n" ) )}

	
		if( aggregateData ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Start aggregating\n" ) )
			if ( length(aggregatemissings) > 0 && aggregatemissings == "seeInputList" ) {
				stopifnot(!is.null(inputList$aggrMiss))
				aMiss <- unname(inputList$aggrMiss[,-1])
				aMiss[,8] <- rep("err", 7)
				aMiss[8,] <- rep("err", 8)
				aggregatemissings <- unname(as.matrix(aMiss, nrow=8, ncol=8))
				dimnames(aggregatemissings) <-
                list(c(names(inputList$aggrMiss)[-1], "err"),
										c(names(inputList$aggrMiss)[-1], "err"))
			}				
			dat <- aggregateData (dat=dat, subunits=inputList$subunits, units=inputList$units,
            aggregatemissings = aggregatemissings, rename = rename, recodedData = recodedData, verbose = verbose, suppressErr = suppressErr, recodeErr = recodeErr)
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
		
		if( collapseMissings ) {
			if(verbose) cat ( "\n" )
			if(verbose) cat ( paste ( f.n , "Collapsing missings\n" ) )
			dat <- collapseMissings(dat=dat, missing.rule = missing.rule)
		} else {if(verbose) cat ( "\n" )	
		if(verbose) cat ( paste ( f.n , "Missings are UNcollapsed.\n" ) )}
		
		# finale Ausgabe 
		if(verbose) cat ( "\n" )
		if(verbose) cat ( paste ( f.n , "terminated successfully!", Sys.time(), "\n\n" ) )
	
		# Ergebnisse returnen
		if (aaa) {
			datList[[1]] <- dat
			return(datList)
		} else {
			return ( dat )
		}
}

# data(inputList)
# data(inputDat)
# test <- automateDataPreparation(inputList = inputList, datList = inputDat,
        # path = "c:/temp/test_eat", readSpss = FALSE, checkData=TRUE,
        # mergeData = TRUE, recodeData=TRUE, recodeMnr = TRUE, breaks = c(1,2),
		    # aggregateData=TRUE, scoreData= TRUE, writeSpss=TRUE, verbose=TRUE)


