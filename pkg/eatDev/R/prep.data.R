
prep.data <- function ( env ) {
		
		# packages
		require( reshape2 )
		
		# get variables from env
		eval( parse ( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		# put all variable names into vector
		# ls.names <- ls()[!ls() %in% "env"]
		# delete objects in environment
		# rm( list=ls.names, envir=env )
		
		# default vector for console output
		default <- character(0)
		
		# lag.names
		# if ( is.null( lag.names ) ) {
				# search for standard named lag variables
				# l <- grepl( "^dT\\d+$", colnames(d) )
				# if ( any(l) ) {
						# lag.names <- colnames(d)[l]
				# } 
		# }
# browser()	
		### !!! d is now long !!!
		# long d to wide dw (with lag variables)
		dw <- ctLongToWide ( d=d, id=id, time=time, manifestNames=colnames(d)[!colnames(d) %in% c(id,time)], TDpredNames = NULL, TIpredNames = NULL)
		
		dw <- ctIntervalise( datawide=dw, Tpoints=length(unique(d[,"time"])), n.manifest=length(colnames(d)[!colnames(d) %in% c(id,time)]), n.TDpred = 0, n.TIpred = 0,
							 imputedefs = F, manifestNames = "auto", TDpredNames = "auto",
							 TIpredNames = "auto", digits = 5, mininterval = 0.001,
							 individualRelativeTime = TRUE, startoffset = 0)
		
		lag.names <- colnames( dw )[ grepl( "^dT", colnames( dw ) ) ]
		timepoint.sep <- "_"
		
		# number of lags
		L <- length( lag.names )

		# keep dw for ctsem models
		# continue with dw2
		dw2 <- dw
		
		# move lags to a matrix and delete from dw2
		if ( !is.null( lag.names ) ) {
				Lags <- as.matrix( dw2[,lag.names,drop=FALSE] )
				dw2 <- dw2[,!colnames(dw2) %in% lag.names,drop=FALSE]
		} else {
				Lags <- NULL
		}
		
		
		### Data ###

		# if id exists, save original person id 
		if ( is.null( id ) | ifelse( !is.null(id), !id %in% colnames(dw2), TRUE ) ) {
				person.id <- NULL
		} else {
				person.id <- dw2[,id]
				dw2[,id] <- NULL
		}

		# create person id by counting 1,2,...,J
		# dw2 <- cbind( dw2, 1:J )
		dw2 <- cbind( dw2, 1:nrow(dw2) )
		colnames(dw2)[ncol(dw2)] <- "id"
		id <- "id"
	
		# reshape data to long, first time
		l.time <- try( reshape( data.frame(dw2), idvar="id", varying=colnames(dw2)[!colnames(dw2) %in% id], direction="long", sep=timepoint.sep ) )
		if ( inherits( l.time, "try-error") ) {
				stop( "Time suffix Tx cannot be identified in variable names. Check argument timepoint.sep ." , call. = TRUE )
		}
		
		# reshape l.time to long, now items
		l <- melt( l.time, id=c("id","time") )
		colnames(l)[c(ncol(l)-1,ncol(l))] <- c("item.orig","y")
		
		# original item names
		item.names <- levels( l$item.orig )
		
		# create item id by counting 1,2,...,I
		idfr <- data.frame( "item.orig" = item.names, "item" = seq( along=item.names ) )
		l <- merge( l, idfr, by="item.orig", sort=FALSE )
		l$item.orig <- NULL

		# original time point names
		# !!!sorted!!! assumption is that item names are named to reflect ordering
		colnames(l)[colnames(l) %in% "time"] <- "time.orig"
		time.point.names <- sort( unique ( l$time.orig ) )
	
		# create time id by counting 1,2,...,T
		tdfr <- data.frame( "time.orig" = time.point.names, "time" = seq( along=time.point.names ) )
		l <- merge( l, tdfr, by="time.orig", sort=FALSE )
		l$time.orig <- NULL

		# sort
		dl <- as.matrix( l[order(l$id,l$item,l$time),c("id","item","time","y")] )
		rownames(dl) <- seq( along = rownames(dl) )

		# column numbers
		col.id <- which( colnames(dl) %in% "id" )
		col.item <- which( colnames(dl) %in% "item" )
		col.time <- which( colnames(dl) %in% "time" )
		col.y <- which( colnames(dl) %in% "y" )

		
		## inferred number of units from long data set
		# persons
		J <- length( unique( dl[,col.id] ) )
		# items
		I <- length( unique( dl[,col.item] ) )
		# time points
		T <- length( unique( dl[,col.time] ) )
		# number of rows in long data set
		R <- nrow(dl)

		
		### Lags ###

		# stop if no lags and more than one time point
		if ( T > 1 & is.null( Lags ) ) {
				stop( "No time lags specified. Check or explicitely set argument lag.names ." , call. = TRUE )
		}
		# stop if number of lags does not match number of time points minus 1
		if ( T > 1 & L!=T-1 ) {
				stop( "Number of time lags is not equal to number of time points minus 1. Check colnames of data, arguments timepoint.sep, and lag.names" , call. = TRUE )
		}		
		
		# different lag patterns
		Lpat <- Lags[!duplicated(Lags[,]),,drop=FALSE]

		# indices of patterns
		indsL <- list()
		do1 <- sapply(1:nrow(Lpat), function(r) if( any( !is.na(Lpat[r,] ) ) ) paste( "Lags[,",which(!is.na(Lpat[r,])),"]==Lpat[",r,",",which(!is.na(Lpat[r,])),"]", collapse=" & " ) else NULL )
		do2 <- sapply(1:nrow(Lpat), function(r) if( any( is.na(Lpat[r,] ) ) ) paste( "is.na(Lags[,",which(is.na(Lpat[r,])),"])", collapse=" & " ) else NULL )
		do3 <- unname( mapply( function (x,y) paste( c(x,y), collapse=" & " ), do1, do2 ) )
		do4 <- mapply( function(r,do3) paste0 ( "indsL[[",r,"]] <- which( ",do3," ) " ), 1:nrow(Lpat) , do3 )
		eval(parse(text=do4))
		rownames( Lpat ) <- seq( along=rownames( Lpat ) )
		
		# pattern groups of persons
		Lpat.group <- rep(NA,nrow(Lags))
		do <- mapply( function( nr, inds ) paste0("Lpat.group[c(",paste(inds,collapse=","),")] <- ",nr ), seq(along=indsL), indsL )
		eval(parse(text=do))

		## inferred from Lags/Lag pattern
		# time points per person
		Tj <- unname( apply( Lags , 1 , function(x) T - length(which(is.na(x))) ) )		
		# number of patterns
		P <- nrow( Lpat )
		# non-missing time points per pattern
		Tp <- unname( apply( Lpat, 1 , function(x) T - length(which(is.na(x))) ) )

		# number of persons in each pattern
		PNj <- sapply( unique( Lpat.group ), function( gr ) length( which( Lpat.group==gr ) ) )
		
		# specific persons in specific pattern
		Pj <- matrix( NA, P, J )
		do <- paste0(  "Pj[",1:P,",1:PNj[",1:P,"]] <- which( Lpat.group == ",1:P," )" )
		eval( parse( text=do ) )		
		
		
		### Stuff for measurement model ###
		## initialize default measurement parameters (if not set by user)

		## default of Lambda: each item mapped on one latent variable
		if( is.null( Lambda ) ) {
				
				# diagonal matrix
				Lambda <- diag( 1, I )
				
				# rownames are item names
				rownames( Lambda ) <- item.names
				
				# default colnames to thetaX
				colnames( Lambda ) <- paste0( "theta", formatC( 1:ncol(Lambda), format="fg", flag="0", width=max(sapply(1:ncol(Lambda),nchar)) ) )

				# output
				default[length(default)+1] <- paste0( "                          loading matrix Lambda: IxF (", I, "x", I, ") matrix, each manifest variable is mapped to one latent variable (so no measurement model is specified) " )
				#                                                                                                           no error here, F is not yet set
				

		
		} else {
				## Lambda is set by user ##

				# if rownames are set then sort according to data set, important!!!
				if ( !is.null( rownames( Lambda ) ) ) {
						Lambda <- Lambda[ match( item.names, rownames( Lambda ) ), ]
				} #else {
						# if no rownames are set, assume that LAMBDA is sorted as occurrence of variables in data set
				#		rownames( Lambda ) <- item.names
				#}
				
		}
		# mode: manifest to latent 1:1
		# if ( all( sd(dim(Lambda))==0 ) & all( Lambda[lower.tri( Lambda )] == 0 ) & all( Lambda[upper.tri( Lambda )] == 0 ) & all( diag( Lambda ) == 1 )
		man2lat <- ifelse ( sd(dim(Lambda))==0 & identical( Lambda, diag(1,dim(Lambda)[1]) ) , TRUE, FALSE )
		
		# NAs to labeled parameters
		Lambda <- label.pars(Lambda,"Lambda")
		
		# number of latent variables (factors)
		F <- ncol( Lambda )
		
		# names of latent variables
		latent.names <- colnames( Lambda )
		
		# item easiness
		if ( !exists("beta",inherits=FALSE) || is.null(beta) ) {
				
				# default beta (constrained equal over time)
				beta <- matrix( rep( NA, I ), ncol=1 )
				rownames( beta ) <- item.names
	
				# set first beta per latent variable to 0
				# inds <- apply( Lambda, 2, function( col ) which( !col==0 )[1] )
				# do <- paste0( "beta[",inds,"] <- 0" )
				# eval( parse( text=do ) )
				
				# output
				# default[length(default)+1] <- paste0( "             item easiness (column) vector beta: I (", I, ") column vector, first item per latent variable set to 0 " )
				default[length(default)+1] <- paste0( "                             item easiness beta: freely estimable column vector of lenth I (", I, ")" )
				
		} else {
				## beta is set by user ##

				# if names are set by user then sort according to data set, important!!!
				if ( !is.null( names( beta ) ) ) {
						beta <- beta[ match( item.names, names( beta ) ) ]
				} #else {
						# if no names are set, assume that beta is sorted as occurrence of variables in data set
				#		names( beta ) <- item.names
				#}
		}
		# NAs to labeled parameters
		beta <- label.pars(beta,"beta")

		# check if all beta free
		# beta.vec <- do.call( "c", sapply( beta, function(x) x, simplify=FALSE ) )
		# beta.all.free <- all( is.na( suppressWarnings( as.numeric( beta.vec ) ) ) )
		
		# if all beta free, mean and precision of item distribution
		if ( all.free( beta ) ){
		
				## beta mean
				if ( !exists("mu.beta",inherits=FALSE) || is.null(mu.beta) ) {
						mu.beta <- 0
						default[length(default)+1] <- paste0( "                  mean of item easiness mu.beta: scalar set to 0" )
				}
				# NAs to labeled parameters
				# mu.beta <- label.pars(mu.beta,"mu.beta")
				
				## beta prec
				if ( !exists("prec.beta",inherits=FALSE) || is.null(prec.beta) ) {
						prec.beta <- NA
						default[length(default)+1] <- paste0( "           precision of item easiness prec.beta: freely estimable scalar" )
				}
				# NAs to labeled parameters
				prec.beta <- label.pars(prec.beta,"prec.beta")
				# prec.beta[] <- 0		
		
		}
		
		# if gaussian, error var/cov matrix
		if ( measurement.model$family == "gaussian" ) {
				
				# default of E
				if ( !exists("E",inherits=FALSE) || is.null(E) ) {
						
						if ( man2lat ) {
								# if manifest are mapped to latent, all 0
								E <- diag( 0, I )
						} else {
								# uncorrelated errors
								E <- diag( NA, I )
								default[length(default)+1] <- paste0( "                     measurement error matrix E: freely estimable uncorrelated error variances (", I, ")" )
						}
						rownames( E ) <- colnames( E ) <- item.names
				
				} else {
						## E is set by user ##

						# if names are set by user then sort according to data set, important!!!
						if ( !is.null( colnames( E ) ) ) {
								E <- E[ , match( item.names, colnames( E ) ) ]
						}
						if ( !is.null( rownames( E ) ) ) {
								E <- E[ match( item.names, rownames( E ) ) , ]
						}

				}
				# NAs to labeled parameters
				E <- label.pars(E,"E")	
				
		} #else {
		#		E <- NULL
		#}

		

		### Stuff for continuous time model ###		
		
		# I1 (diagonal matrix, same structure as drift matrix)
		I1 <- diag( F )

		# I2 (diagonal matrix, for ct error calculation)
		# n^2,n^2 with n of drift matrix (Oud/Delsing, 2010, p. 219)
		I2 <- diag( F^2 )

		# for Kronecker product
		# width of drift matrix A 
		Aw <- F
		# width of I1
		I1w <- F

		# replacement matrix for Qt.prec if Qt.prec is not positive definite
		Qt.prec.replace <- matrix( c(0.001,0.0001,0.0001,0.001), nrow=F, ncol=F )

		
		## initialize default parameters (if not set by user)

		# drift matrix A
		if ( !exists("A",inherits=FALSE) || is.null(A) ) {
				A <- matrix( NA, nrow=F, ncol=F )
				if ( !is.null( latent.names ) ) colnames( A ) <- rownames( A ) <- latent.names
				default[length(default)+1] <- paste0( "                                 drift matrix A: freely estimable FxF (", F, "x", F, ") matrix" )
		}
		# NAs to labeled parameters
		A <- label.pars(A,"A")
	
		# process error matrix Q
		if ( !exists("Q",inherits=FALSE) || is.null(Q) ) {
				Q <- matrix( NA, nrow=F, ncol=F )
				if ( !is.null( latent.names ) ) colnames( Q ) <- rownames( Q ) <- latent.names
				default[length(default)+1] <- paste0( "                         process error matrix Q: freely estimable symmetric FxF (", F, "x", F, ") matrix" )
		}
		# NAs to labeled parameters
		Q. <- label.pars(Q,"Q")		
		# make Q (potentially) symmetric again (do not overwrite user specific labeled parameters, even if unsymmetric matrix
		Q.[upper.tri(Q.)][ is.na( Q[upper.tri(Q)] ) ]  <- Q.[lower.tri(Q.)][ is.na( Q[lower.tri(Q)] ) ]  
		Q <- Q.
		
		# ct intercepts b
		if ( !exists("b",inherits=FALSE) || is.null(b) ) {
				b <- matrix( rep( NA, F ), ncol=1 )
				if ( !is.null( latent.names ) ) rownames( b ) <- latent.names
				default[length(default)+1] <- paste0( "                   continuous time intercepts b: freely estimable column vector of length F (", F, ")" )
		}
		# NAs to labeled parameters
		b <- label.pars(b,"b")

		# first time point mean
		if ( !exists("mu.t1",inherits=FALSE) || is.null(mu.t1) ) {
				# mu.t1 <- matrix( rep( NA, F ), ncol=1 )
				mu.t1 <- rep( NA, F )
				# if ( !is.null( latent.names ) ) rownames( mu.t1 ) <- latent.names
				if ( !is.null( latent.names ) ) names( mu.t1 ) <- latent.names
				default[length(default)+1] <- paste0( "latent variable means of first time point mu.t1: column vector of length F (", F, ") with 0s" )
		}
		# NAs to labeled parameters
		mu.t1 <- label.pars(mu.t1,"mu.t1")
		# mu.t1[] <- 0
	
		# first time point prec
		if ( !exists("prec.t1",inherits=FALSE) || is.null(prec.t1) ) {
				prec.t1 <- matrix( NA, nrow=F, ncol=F )
				if ( !is.null( latent.names ) ) colnames( prec.t1 ) <- rownames( prec.t1 ) <- latent.names
				default[length(default)+1] <- paste0( "lat. var. precision of first time point prec.t1: freely estimable symmetric FxF (", F, "x", F, ") matrix" )
		}
		# NAs to labeled parameters
		prec.t1. <- label.pars(prec.t1,"prec.t1")		
		# make prec.t1 (potentially) symmetric again (do not overwrite user specific labeled parameters, even if unsymmetric matrix
		prec.t1.[upper.tri(prec.t1.)][ is.na( prec.t1[upper.tri(prec.t1)] ) ]  <- prec.t1.[lower.tri(prec.t1.)][ is.na( prec.t1[lower.tri(prec.t1)] ) ]  
		prec.t1 <- prec.t1.	
		
		### (over)write relevant variables to environment ###
		obj <- c( "d", "dw", "dl", "col.y", "col.id", "col.item", "col.time", "R", "J", "I", "T", "Tj", "P", "Tp", "L", "Lpat", "Lpat.group", "Lambda", "beta", ifelse(exists("mu.beta"),"mu.beta",NA), ifelse(exists("prec.beta"),"prec.beta",NA), ifelse(measurement.model$family=="gaussian","E",NA), "F", "I1", "I2", "Aw", "I1w", "Qt.prec.replace", "A", "b", "Q", "mu.t1", "prec.t1" )
		obj <- obj[!is.na(obj)]
		eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )

		
		### console output ###
		if ( verbose ) {
				cat("preparing data\n\n")
				cat( paste0( "                                    persons (J): ", J ,"\n") )
				cat( paste0( "                                      items (I): ", I ,"\n") )
				cat( paste0( "                           latent variables (F): ", F ,"\n") )
				cat( paste0( "                                time points (T): ", T ,"\n") )
				cat( paste0( "               responses (without missings) (R): ", R ,"\n") )
				Tj.string <- ifelse( all(Tj==Tj[1]), as.character(Tj[1]), paste0("M=", formatC(mean(Tj),format="f",digits=2), " min=", min(Tj), " max=", max(Tj)) )
				cat( paste0( "                    time points per person (Tj): ", Tj.string, "\n") )
				cat( paste0( "                               lag patterns (P): ", P,"\n") )
				Tp.string <- ifelse( all(Tp==Tp[1]), as.character(Tp[1]), paste0("M=", formatC(mean(Tp),format="f",digits=2), " min=", min(Tp), " max=", max(Tp)) )
				cat( paste0( "                   time points per pattern (Tp): ", Tp.string, "\n") )
				PNj.string <- ifelse( length(PNj)==1, as.character(PNj[1]), paste0("M=", formatC(mean(PNj),format="f",digits=2), " min=", min(PNj), " max=", max(PNj)) )
				cat( paste0( "                      persons per pattern (PNj): ", PNj.string, "\n") )				
				cat("\n")
				cat( paste0( "setting defaults\n\n" ) )
				if( length(default)>0 ) {
						cat( paste0( paste( default, collapse="\n" ), "\n" ) )
						cat("\n")
				} else {
						cat( "                     no defaults set, everything is entirely user specified\n\n" )
				}
				
		}
# browser()
		# return
		TRUE
}

# NAs to labeled parameters
label.pars <- function(m,m.name) {

		if( is.null(dim(m)) ) {
				## vector
				if ( is.null( names( m ) ) ) nams <- seq(along=m)[is.na(m)] else nams <- names( m )[is.na(m)]
				m[is.na(m)] <- paste0( m.name, "_", nams )
		} else {

				## arrays
				
				# dimension of m
				dim.m <- dim(m)
				
				m. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.m ), collapse="," ), "),accumulate=FALSE )" )))
				att <- attributes(m)$dimnames
				

				
				
				## remove columns without variance
				## this is the case e.g. for column or row vectors
				del <- sapply( m., function( sp ) length(sp) > 1 & sd( sp ) == 0 )

				if( any( del ) ) {
						m. <- m.[,!del,drop=FALSE]
						dim.m <- dim.m[!del]
						att <- att[!del]
				}
			
			
				
				if ( is.null( att ) ) att <- sapply( seq(along=dim.m), function(x) NULL, simplify=FALSE )
				do <- paste0( "if ( is.null( att[[",seq(along=dim.m),"]] ) )   att[[",seq(along=dim.m),"]] <- as.character( 1:",dim.m," ) " )
				eval( parse( text = do ) )
				if( length( att ) > 1 ) {
						nams.dfr <- Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE), att, accumulate=FALSE )
				} else {
						nams.dfr <- data.frame( att, stringsAsFactors=FALSE )
				}
				
				# parameter names
				nams <- apply( nams.dfr, 1, function (z) paste( z, collapse="_" ) )

				# if all names equal then only one name, e.g. A_lat1 instead of A_lat1_lat1
				# eq <- apply( nams.dfr, 1, function(x) all( x==x[1] ) )
				# if ( any( eq ) ) nams[eq] <- as.character( nams.dfr[eq,1] )
				
				# add indicator as additional column
				m.[,ncol(m.)+1] <- 1:nrow(m.)

				
				# if only one row in m. means that it is just one parameter
				# then no numbering
				if ( nrow( m. ) == 1 ) {
						m. <- m.[,ncol(m.),drop=FALSE]
						nams <- NULL
				}
				
				
				# set name if cell is NA
				do <- apply( m. , 1, function ( z ) paste0( "if( is.na( m[", paste(z[-length(z)],collapse=",") ,"] ) ) m[", paste(z[-length(z)],collapse=",") ,"] <- paste0( '", m.name, ifelse( is.null( nams ), "", "_" ), "', nams[", paste(z[length(z)],collapse=","), "] ) " )  )
				eval(parse(text=do))
				
		}
	
		return(m)
}
