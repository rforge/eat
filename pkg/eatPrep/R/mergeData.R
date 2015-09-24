mergeData <- function ( newID="ID", datList, oldIDs=NULL, addMbd = FALSE, verbose=FALSE) {
  versNr <- "0.6.0"
  mReturn <- NULL
  stopifnot (is.list (datList))
  if(is.null(oldIDs)) {oldIDs <- rep(newID, length(datList))}
  for ( i in seq(along = datList)) {stopifnot(is.data.frame(datList[[i]]))} 
  for ( i in seq(along = datList)) {stopifnot( oldIDs[i] %in% colnames(datList[[i]]) )} 
  if ( length(datList) > 0 ) {
	fkNam <- list()
	for ( i in seq(along = datList)) {
		if(any(unlist(lapply(datList[[i]], class)) == "factor")) {
			fkNam[[i]] <- names(which(unlist(lapply(datList[[i]], class)) == "factor"))
			datList[[i]] <- set.col.type(datList[[i]], col.type = list ( "character" = fkNam[[i]] ))
		}
	}
    stopifnot(length(datList) == length(oldIDs))
    stopifnot(is.numeric(oldIDs) | is.character(oldIDs))
		for ( i in seq(along = datList)) {
		  if(verbose) {cat(paste ( "mergeData_", versNr, ": Start merging of dataset ", i, ".\n", sep=""))}
			if ( i == 1 ) {
				if ( is.character( oldIDs )) {	IDname1 <- oldIDs[i] }
				if ( is.numeric( oldIDs )) { IDname1 <- colnames(datList[[i]])[oldIDs[i]]   }
		
				if ( length( na.omit( datList[[i]][ , IDname1])) != length( na.omit ( unique( datList[[i]][ , IDname1] ) ) ) ) {
							doppelt <- na.omit( unique( datList[[i]][ , IDname1][ duplicated(datList[[i]][ , IDname1] ) ] ) )
							cat(paste ( "mergeData_", versNr, ": Multiple IDs in dataset ", i, " in " , length(doppelt)," cases. \n",sep=""))
							stop( cat(paste ( "Multiple IDs: ", paste( doppelt, collapse = ", "), "\n" )))}
				if ( is.character(IDname1)) {
					names(datList[[i]])[names(datList[[i]]) == IDname1] <- newID  
					mergedData <- datList[[i]]
				} else {
					warning(paste ( "mergeData_", versNr, ": Found no ID variable in dataset", i, "\n"))
					mReturn <- FALSE
				}
			} else {

        if ( is.character( oldIDs )) {	IDname2 <- oldIDs[i] }
        if ( is.numeric( oldIDs )) { IDname2 <- colnames(datList[[i]])[oldIDs[i]]   }
				if ( is.character(IDname2)) {
					names(datList[[i]])[names(datList[[i]]) == IDname2] <- newID 
					partialData <- merge( mergedData, datList[[i]], all = TRUE )
					colID <- match(newID, colnames(partialData))
#					partialData <- partialData[order(partialData[colID]),]

					# wenn mehrere Eintraege mit gleicher ID gefunden wurden.... 
					if( length(na.omit ( partialData[[colID]]) ) != length(na.omit ( unique(partialData[[colID]])) ) ) {
						
						# total columns and rows for the targetData dataframe
						total.row <- length(na.omit ( unique(partialData[[colID]])) )
						total.col <- ncol(partialData)
						row.ID <- na.omit ( unique(partialData[[colID]]) )
						targetData <- matrix(NA,total.row,total.col)
						targetData <- as.data.frame(targetData)
						names(targetData) <- names(partialData)
						
						# looking for multiple cases
						multipleCases <- na.omit ( unique ( partialData[[ colID ]] [ duplicated(partialData[[ colID ]]) ] )  )
						
						for (k in seq( along = multipleCases ) ){
							# select all rows with the same ID
							partialData.part <- partialData[ which ( partialData[colID] == multipleCases[k] ),]             
							targetData [ k , ] <- partialData.part [ 1 , ]
							# in welchen Spalten treten (mindestens) zwei unterschiedliche Werte auf, inkl. Missings?
							differentCol <- which ( sapply ( partialData.part , function (ll) { length (table(ll, useNA = "ifany")) > 1 }) )
             
							for ( j in differentCol ){
								# Wenn es mehr als einen Nicht-NA-Eintrag gibt:
								if( sum( ! is.na(partialData.part[ ,j]) ) > 1 ) {
								  # nimm den ersten davon, wenn alle gleich sind
								  if ( length( table (partialData.part [ , j ])) == 1 ) {
                    targetData[ k, j] <- partialData.part [ which( !is.na (partialData.part[ , j] ) )[1], j ] 
                  } else {
  									# nimm den ersten davon und gib eine Warnung aus, wenn nicht alle gleich sind
					warning(paste("mergeData_", versNr, ": Multiple different valid codes in variable ", colnames(targetData)[j],
  										  ", ID ", multipleCases [k], ", Codes: ", paste(na.omit ( partialData.part[ 1,j] ), " & ", na.omit ( partialData.part[ 2,j] ),".", sep=""),
  										  " \n The first value will be kept.\n", sep = ""))
  									targetData[ k, j] <- partialData.part [ which( !is.na (partialData.part[ , j] ) )[1], j ]
                  }    
                } else {
  								# wenn es genau einen Nicht-NA-Eintrag gibt, dann nimm den
  								if (sum( ! is.na(partialData.part[ ,j]) ) == 1 ) {
  									targetData[ k, j] <- partialData.part [ which( !is.na (partialData.part[ , j] ) ), j ]
  								} 
  							}	
              }
  				}
					# Daten fuer nur einmal vorkommende IDs in targetData schreiben
					ind <- seq ( length(multipleCases)+1,  nrow(targetData), 1)		
					targetData[ ind ,  ] <- partialData[ which( ! partialData[[colID]] %in% multipleCases ),  ]	
					} else {
						# Wenn keine mehrfachen IDs vorkommen, dann einfach gemergten Datensatz uebernehmen
						targetData <- partialData
					}
					mergedData <- targetData
				} else {
					warning(paste ( "mergeData_", versNr, ": Found no ID variable in dataset", i, "\n"))
					mReturn <- FALSE
				}			
			}
		mReturn <- mergedData
		if(addMbd == TRUE) {mReturn[is.na(mReturn)] <- "mbd"}
		}
	} else {    
		warning(paste("mergeData_", versNr, ": Found no datasets."), "\n")
		mReturn <- FALSE
	}
	if(is.data.frame(mReturn)) {
		if(!is.null(unlist(fkNam))) {
			mReturn <- set.col.type(mReturn, col.type = list ( "factor" = unique(unlist(fkNam))))
		}
	}
	return (mReturn)
}
