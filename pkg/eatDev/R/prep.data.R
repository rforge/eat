
prep.data <- function ( env ) {
		
		# get variables from env
		eval( parse ( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		# put all variable names into vector
		# ls.names <- ls()[!ls() %in% "env"]
		# delete objects in environment
		# rm( list=ls.names, envir=env )
		
		# lagNames
		if ( is.null( lagNames ) ) {
				# search for standard named lag variables
				l <- grepl( "^dT\\d+$", colnames(d) )
				if ( any(l) ) {
						lagNames <- colnames(d)[l]
				} 
		}
		
		# number of lags
		L <- length( lagNames )
		
		# move lags to a matrix and delete from d
		if ( !is.null( lagNames ) ) {
				Lags <- as.matrix( d[,lagNames,drop=FALSE] )
				d <- d[,!colnames(d) %in% lagNames,drop=FALSE]
		} else {
				Lags <- NULL
		}
		
		
		### Data ###
		
		# number of persons
		J <- nrow(d)

		# if id exists, save original person id 
		if ( is.null( id ) | ifelse( !is.null(id), !id %in% colnames(d), TRUE ) ) {
				person.id <- NULL
		} else {
				person.id <- d[,id]
				d[,id] <- NULL
		}

		# create person id by counting 1,2,...,J
		d <- cbind( d, 1:J )
		colnames(d)[ncol(d)] <- "id"
		id <- "id"
	
		# reshape data to long, first time
		l.time <- try( reshape( data.frame(d), idvar="id", varying=colnames(d)[!colnames(d) %in% id], direction="long", sep=timepoint.sep ) )
		if ( inherits( l.time, "try-error") ) {
				stop( "Time suffix Tx cannot be identified in variable names. Check argument timepoint.sep ." , call. = TRUE )
		}
		
		# reshape l.time to long, now items
		l <- melt( l.time, id=c("id","time") )
		colnames(l)[c(ncol(l)-1,ncol(l))] <- c("item.orig","y")
		
		# original item names
		item.names <- levels( l$item.orig )
		
		# number of items
		I <- length( item.names )
		
		# create item id by counting 1,2,...,I
		idfr <- data.frame( "item.orig" = item.names, "item" = seq( along=item.names ) )
		l <- merge( l, idfr, by="item.orig", sort=FALSE )
		l$item.orig <- NULL

		# original time point names
		# !!!sorted!!! assumption is that item names are named to reflect ordering
		colnames(l)[colnames(l) %in% "time"] <- "time.orig"
		time.point.names <- sort( unique ( l$time.orig ) )
		
		# number of time points
		T <- length( time.point.names )
		
		# create time id by counting 1,2,...,T
		tdfr <- data.frame( "time.orig" = time.point.names, "time" = seq( along=time.point.names ) )
		l <- merge( l, tdfr, by="time.orig", sort=FALSE )
		l$time.orig <- NULL

		# sort
		d <- as.matrix( l[order(l$id,l$item,l$time),c("id","item","time","y")] )
		rownames(d) <- seq( along = rownames(d) )

		# column numbers
		col.id <- which( colnames(d) %in% "id" )
		col.item <- which( colnames(d) %in% "item" )
		col.time <- which( colnames(d) %in% "time" )
		col.y <- which( colnames(d) %in% "y" )

		# number of rows in long data set
		R <- nrow(d)

		
		### Lags ###

		# stop if no lags and more than one time point
		if ( T > 1 & is.null( Lags ) ) {
				stop( "No time lags specified. Check or explicitely set argument lagNames ." , call. = TRUE )
		}
		# stop if number of lags does not match number of time points minus 1
		if ( T > 1 & L!=T-1 ) {
				stop( "Number of time lags is not equal to number of time points minus 1. Check colnames of data, arguments manifestNames, timepoint.sep, and lagNames" , call. = TRUE )
		}		
		
		# time points per person
		Tj <- unname( apply( Lags , 1 , function(x) T - length(which(is.na(x))) ) )
		
		# different lag patterns
		Lpat <- Lags[!duplicated(Lags[,]),,drop=FALSE]

		# indices of patterns
		indsL <- list()
		do1 <- sapply(1:nrow(Lpat), function(r) if( any( !is.na(Lpat[r,] ) ) ) paste( "Lags[,",which(!is.na(Lpat[r,])),"]==Lpat[",r,",",which(!is.na(Lpat[r,])),"]", collapse=" & " ) else NULL )
		do2 <- sapply(1:nrow(Lpat), function(r) if( any( is.na(Lpat[r,] ) ) ) paste( "is.na(Lags[,",which(is.na(Lpat[r,])),"])", collapse=" & " ) else NULL )
		do3 <- unname( mapply( function (x,y) paste( c(x,y), collapse=" & " ), do1, do2 ) )
		do4 <- mapply( function(r,do3) paste0 ( "indsL[[",r,"]] <- which( ",do3," ) " ), 1:nrow(Lpat) , do3 )
		eval(parse(text=do4))

		# pattern groups of persons
		Lpat.group <- rep(NA,nrow(Lags))
		do <- mapply( function( nr, inds ) paste0("Lpat.group[c(",paste(inds,collapse=","),")] <- ",nr ), seq(along=indsL), indsL )
		eval(parse(text=do))

		# number of patterns
		P <- nrow( Lpat )

		# non-missing time points per pattern
		Tp <- unname( apply( Lpat, 1 , function(x) T - length(which(is.na(x))) ) )
		
		
		### Stuff for measurement model ###
		
		# default of LAMBDA: each item mapped on one latent variable
		if( is.null( LAMBDA ) ) {
				Lambda <- array( dim=c(I,I,T) )
				eval( parse( text = paste0( "Lambda[,,",1:T,"] <- diag( I )" ) ) )
				rownames( Lambda ) <- item.names
				colnames( Lambda ) <- item.names
		} else {

				# if LAMBDA is set by user, make it time variant if not
				if ( length( dim( LAMBDA ) ) < 3 ) {
						Lambda <- array( dim=c( dim( LAMBDA ),T) )
						# eval( parse( text = paste0( "Lambda[,,",1:T,"] <- LAMBDA" ) ) )
						Lambda[,,] <- LAMBDA
						
						# set row- and colnames if exist
						rownames( Lambda ) <- rownames( LAMBDA )
						colnames( Lambda ) <- colnames( LAMBDA )
				} else {
						Lambda <- LAMBDA
				}

				# if rownames are set then sort according to data set, important!!!
				if ( !is.null( rownames( Lambda ) ) ) {
						
						eval( parse( text = paste0( "Lambda[,,",1:T,"] <- Lambda[match( item.names, rownames( Lambda[,,",1:T,"] ) ),,",1:T,"] " ) ) )
						rownames( Lambda ) <- item.names
						# actually not necessary to loop over T, as rownames are always the same, still works too
						
				} else {
						# if no rownames are set, assume that LAMBDA is sorted as occurrence of variables in data set
						rownames( Lambda ) <- item.names
				}
				
				# if colnames are set, then sort them consistently over time
				# should be sorted by user, just to be sure
				# !!!actually, matrices cannot have different colnames!!! -> not necessary
				# if ( !is.null( colnames( Lambda ) ) ) {
						# consistent.cn <- unique( do.call("c", sapply( 1:dim(Lambda)[3], function(x) colnames( Lambda[,,x] ), simplify=FALSE ) ) )
						# eval( parse( text = paste0( "Lambda[,,",1:T,"] <- Lambda[,match( consistent.cn, colnames( Lambda[,,",1:T,"] ) ),",1:T,"] " ) ) )
						# colnames( Lambda ) <- consistent.cn
				# }
				
				# if no colnames are set, default to hetaX
				if ( is.null( colnames( Lambda ) ) ) colnames( Lambda ) <- paste0( "theta", formatC( 1:ncol(Lambda), format="fg", flag="0", width=max(sapply(1:ncol(Lambda),nchar)) ) )
		}

		# number of latent variables (factors)
		F <- ncol( Lambda )

		
		### Stuff for continuous time model ###		
		
		# I1 (diagonal matrix, same structure as drift matrix)
		I1 <- diag( F )

		# I2 (diagonal matrix, for ct error calculation)
		# error matrix is n^2,n^2 with n of drift matrix (Oud/Delsing, 2010, p. 219)
		I2 <- diag( F^F )

		# for Kronecker product
		# width of drift matrix A 
		Aw <- F
		# width of I1
		I1w <- F

		# replacement matrix for Qt.prec if Qt.prec is not positive definite
		Qt.prec.replace <- matrix( c(0.001,0.0001,0.0001,0.001), nrow=2, ncol=2 )

	
		### (over)write relevant variables to environment ###
		obj <- c( "d", "col.y", "col.id", "col.item", "col.time", "R", "J", "I", "T", "Tj", "P", "Tp", "L", "Lpat", "Lpat.group", "Lambda", "F", "I1", "I2", "Aw", "I1w", "Qt.prec.replace" )
		eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )

		
		### console output ###
		if ( verbose ) {
				cat("preparing data\n")
				cat( paste0( "                      persons: ", J ,"\n") )
				cat( paste0( "                        items: ", I ,"\n") )
				cat( paste0( "             latent variables: ", F ,"\n") )
				cat( paste0( "                  time points: ", T ,"\n") )
				cat( paste0( " responses (without missings): ", R ,"\n") )
				Tj.string <- ifelse( sd(Tj)==0, as.character(T), paste0("M=", formatC(mean(Tj),format="f",digits=2), " min=", min(Tj), " max=", max(Tj)) )
				cat( paste0( "       time points per person: ", Tj.string, "\n") )
				cat( paste0( "                 lag patterns: ", nrow(Lpat),"\n") )
		}
		
		# return
		TRUE
}
