
.prepJAGS <- function ( JAGS.object , burnin ) {

		# wenn vorher schon auf List mit Matrizen reduziert, nix tun, ansonsten reduzieren
		if ( length ( dim ( JAGS.object[[1]] ) ) == 3 ) {
				obj   <- lapply ( JAGS.object , function ( e ) { if ( dim(e)[1]>1 ) {e <- e[,,1]} else {e <- t ( matrix ( e[,,1] ) )} ; return(e) }  )
		} else {
				obj <- JAGS.object
		}
		# burnin-Liste kreeieren korrespondierend zu obj-Elemente-Rows
		if ( is.numeric ( burnin ) ) {
				burnin.l <- sapply ( obj , function ( obj , burnin ) rep ( (burnin+1) , nrow ( obj ) ) , burnin , simplify = FALSE )
		} else if ( inherits ( burnin , "list" ) ) {
				burnin.l <- burnin
		} else {
				burnin.l <- sapply ( obj , function ( obj ) rep ( 1 , nrow ( obj ) ) , simplify = FALSE )
		}
			
		return ( list ( "obj" = obj , "burnin.l" = burnin.l ) )
		
}

resJAGS <- function ( JAGS.object , burnin = 1000 , retList = TRUE ) {

			prepped <- .prepJAGS ( JAGS.object , burnin )
			obj <- prepped$obj
			burnin.l <- prepped$burnin.l
			
			prms  <- mapply ( FUN = function ( prm , prm.name , burnin.l ) {
                 
					# Burnin abziehen
					prm.l <- list()
					do <- paste0 ( "prm.l[[", seq(along=burnin.l), "]]","<-prm[", seq(along=burnin.l), ",", burnin.l , ":", dim(prm)[2] , "]" )
					eval ( parse ( text = do ) )
					# Names setzen
					if ( ! is.null ( rownames ( prm ) ) ) {
							names ( prm.l ) <- rownames ( prm )
					} else {
							# names ( prm.l ) <- as.character ( paste0 ( prm.name , seq ( along = prm.l ) ) )
							names ( prm.l ) <- as.character ( seq ( along = prm.l ) )
					}

					singlePrm.l <- sapply ( prm.l, FUN = function ( chain ) {

								 d         <- density( chain, from = min( chain ), to = max( chain ) )
								 MAP       <- d$x[ which( d$y  == max( d$y ) ) ][1]### maximum a posteriori (MAP)
								 EAP       <- mean(chain)                       ### expected a posteriori (EAP)
								 
								 # MH 27.06.2013, standard error des Mittelwerts
								 chainlength <- length ( chain )
								 if ( chainlength < 2 ) chainlength <- NA
								 if ( !is.na ( chainlength ) ) {
										EAP.sd <- sd ( chain )
										EAP.se <- EAP.sd / sqrt ( chainlength )
								 } else {
										EAP.sd <- NA
										EAP.se <- NA
								}
								 
								 # MH 27.06.2013, effektives N
								 tried <- try ( neff <- fneff ( chain ) , silent = TRUE )
								 if ( inherits ( tried , "try-error" ) ) {
										neff <- NA
								 } else {
										neff <- ceiling ( neff )
								}
								 
								 sortChain <- sort( chain )                     ### Hier wird highest posterior density interval (HPD) bestimmt, auf einem 90, 95 und 99% Niveau
								 HPD       <- lapply( c ( .90, .95, .99 ), FUN = function ( ki ) {
											  index     <- floor( ki * length( sortChain ) )
												num       <- length( sortChain ) - index
												width     <- sortChain[ 1:num + index ] - sortChain[ 1:num ]
												HPD       <- c(sortChain[ which.min( width ) ], sortChain[ which.min( width ) + index ] )
												if(all(chain == chain[1])) {HPD <- rep(NA,3)} else {HPD <-  c(HPD, diff(HPD))}
												names(HPD) <- paste( paste(   ki*100,"%.HPD",sep=""),   c("lb", "ub", "diff"),sep="_")
												return(HPD)})                     ### Returns equal-tailed (I95) interval
								 se        <- if (all(chain == chain[1])) NA else abs(HPD[[2]][3]/(2*qnorm(0.025)))
								 ETI       <- lapply( c ( .90, .95, .99 ), FUN = function ( ki ) {
											  ETI <- c( quantile( chain, (1-ki)/2 ),  quantile( chain, 1-(1-ki)/2 ) )
											  if(all(chain == chain[1]))  {ETI <- rep(NA,3)} else {ETI <-  c(ETI, diff(ETI))}
											  names(ETI) <- paste( paste(   ki*100,"%.ETI",sep=""),   c("lb", "ub", "diff"),sep="_")
											  return(ETI)})
								 
								 ret       <- data.frame( MAP = MAP, EAP = EAP, EAP.sd = EAP.sd, EAP.se = EAP.se, neff = neff , se = se, t(unlist(HPD)), t(unlist(ETI)), stringsAsFactors = FALSE )
								 ind       <- grep("^X[[:digit:]]{2}", names(ret))
								 names(ret)[ind] <- gsub("\\.\\.", "%.", substring(names(ret)[ind],2))
								 
								 return(ret)
								 } , simplify = FALSE )
								 

					singlePrm <- do.call ( "rbind", singlePrm.l )
					singlePrm <- cbind ( data.frame ( "parameter.group" = rep ( prm.name , nrow ( singlePrm ) ) , "parameter" = rownames ( singlePrm ) , stringsAsFactors = FALSE ) , singlePrm )
					
					return ( singlePrm )
			} , obj , names ( obj ) , burnin.l , SIMPLIFY = FALSE )
   

   # wenn nicht Liste returned werden soll, long-Datensatz draus machen

   if ( ! retList ) {
			prms <- do.call ( "rbind" , prms )
   }
   
   return(prms)
}

plotJAGS <- function ( JAGS.object , burnin = 1000 , folder = NULL) {

		require ( ggplot2 )
		
		prepped <- .prepJAGS ( JAGS.object , burnin )
		obj <- prepped$obj
		burnin.l <- prepped$burnin.l
		# Parameter-spezifisches Burnin erstmal nicht implementiert
		
		looppars <- function ( obj , burnin ) {
		
				make.plots <- function ( obj , burnin ) {
						a2 <- data.frame ( "Iteration" = seq ( along = obj ) , "Estimate" = obj )
						pl <- ggplot ( dat = a2 , aes(x=Iteration, y=Estimate) ) +
						geom_point( shape=16, size=4.25 ) +
						geom_point( shape=16, size=1, color="red" ) +
						geom_smooth ( method="loess" , se=FALSE , size = 1.5 , color="red" ) +
						## Achtung, hier wird noch globales burnin genommen, statt burnin.l !!!
						geom_smooth ( dat = a2[a2$Iteration>burnin,] , method="lm" , se=TRUE , size = 1 , color="yellow" )
						return ( pl )
				}
				pl <- apply ( obj , 1 , make.plots , burnin )
				
				return ( pl )
		}
		pl <- sapply ( obj , looppars , burnin , simplify = FALSE )
		
		# printen
		if ( ! is.null ( folder ) ) {
				for ( i in seq ( along = pl ) ) {
						fl <- file.path ( folder , paste0 ( names ( pl )[i] , ".pdf" ) ) 
						pdf ( fl , paper = "a4r" , width = 11.6 , height = 8.2 )
						for ( j in seq ( along = pl[[i]] ) ) {
								print ( pl[[i]][[j]] )
						}
						invisible ( dev.off ( ) )
				}
		}
		
		return ( pl )
}

fspec0 <- function( chain ){
	# Returns spectral density at frequency zero (SPEC0)
	# Argument:
	#	chain
	#		is Marcov chain
	# Value:
	#	is SPEC0

	tried <- try ( fit <- suppressWarnings ( spec.ar( chain , plot = FALSE ) ) , silent = TRUE )
	if ( inherits ( tried , "try-error" ) ) {
			val <- NA
	} else {
			val <- fit$spec[ 1 ]
	}
	
	return( val )
}

fneff <- function( chain ){
	# Returns number INdependent iterations needed to get same SE of mean
	# as from the N (dependent) iterations
	# Argument:
	#	chain
	#		is Marcov chain
	# Value:
	#	is number needed
	tried <- try ( fspec0val <- fspec0( chain ) , silent = TRUE )
	if ( inherits ( tried , "try-error" ) ) {
			val <- NA
	} else {
			val <- length( chain ) * var( chain ) / fspec0val
	}
	
	return( val )
}

	