mergeData <- function ( newID="ID", datList, oldIDs=NULL, addMbd = FALSE, verbose=FALSE) {
  versNr <- "0.8.0"
  mReturn <- NULL
  stopifnot (is.list (datList))
  if(is.null(oldIDs)) {oldIDs <- rep(newID, length(datList))}
  for ( i in seq(along = datList)) {stopifnot(is.data.frame(datList[[i]]))} 
  for ( i in seq(along = datList)) {stopifnot( oldIDs[i] %in% colnames(datList[[i]]) )} 
  if ( length(datList) > 0 ) {
	
    stopifnot(length(datList) == length(oldIDs))
    stopifnot(is.numeric(oldIDs) | is.character(oldIDs))
	
	datList <- mapply(function(dat, ids, i) {
		if (!(ids %in% colnames(dat))) {
			stop(paste ( "mergeData_", versNr, ": Didn't find ID variable '", ids, "' in dataset ", i, "\n", sep = ""))
		}
		if ( is.numeric( ids )) {
			colnames(dat)[ids] <- newID
			}		
		if ( is.character( ids )) {
			colnames(dat)[match(ids,colnames(dat))] <- newID
			}
		if (length(na.omit(dat[, newID])) != length(na.omit (unique(dat[, newID])))) { 
			doppelt <- na.omit(unique(dat[,newID][ duplicated(dat[,newID])]))
			cat(paste("mergeData_", versNr, ": Multiple ID(s) in dataset ", i, " in " ,length(doppelt), " case(s). \n",sep=""))
			warning(cat(paste("Multiple ID(s): ",paste(doppelt, collapse = ", "), "\n" )))}
		return(dat)
	}, datList, oldIDs, seq(along=datList))
	
### SW, 1. Oktober 2014:
### datList <- lapply(datList, reshape2::melt, id=newID)
	datList <- lapply(datList, FUN = function ( l ) { reshape2::melt( data = l, id.vars = newID) })
	
	for(i in seq(along=datList)) {
		if(i==1) {datLong <- datList[[i]]} else {
			datLong <- rbind(datLong, datList[[i]])
		}
	}
	datLong <- set.col.type(datLong, col.type=list("character" = names(datLong)))
	
	datLong <- datLong[-which(is.na(datLong$value)),]

	if(any(duplicated(paste(datLong[,newID], datLong$variable)))) {
		if(any(duplicated(paste(datLong[,newID], datLong$variable, datLong$value)))) {
			datLong <- datLong[-which(duplicated(paste(datLong[,newID], datLong$variable, datLong$value))),] 
		} 
		if(any(duplicated(paste(datLong[,newID], datLong$variable)))) {
			doppi <- unique(c(which(duplicated(paste(datLong[,newID], datLong$variable))), which(duplicated(paste(datLong[,newID], datLong$variable), fromLast=TRUE))))
			doppi <- doppi[order(doppi)]
			tt <- NULL
			for(ll in unique(datLong$variable[doppi])) {
				warning(paste("mergeData_", versNr, ": Multiple different valid codes in variable: ", ll, ", ID: ", unique(datLong[doppi,][which(datLong$variable[doppi] %in% ll),1]), ", Codes: ", paste(unique(datLong[doppi,][which(datLong$variable[doppi] %in% ll),3]), collapse = ", "), " \n The first value will be kept.\n", sep = ""))
				tt <- c(tt, which(paste(datLong[,1], datLong[,2], datLong[,3]) %in% paste(datLong[doppi,][which(datLong$variable[doppi] %in% ll),][-1,], collapse=" ")))
			}	
			datLong <- datLong[-tt,]
		}
	}
### SW, 1. Oktober 2014:
### mReturn <- reshape2::dcast(datLong, add.missing =TRUE)
	mReturn <- dcast(datLong, as.formula(paste( newID,"~variable",sep="")), value.var = "value")
	mReturn <- set.col.type(mReturn, col.type=list("character" = names(mReturn)))
	mReturn <- data.frame(mReturn)
	if(addMbd) {mReturn[is.na(mReturn)] <- "mbd"}
		
	} else {    
		warning(paste("mergeData_", versNr, ": Found no datasets."), "\n")
		mReturn <- FALSE
	}
	return(mReturn)
}

