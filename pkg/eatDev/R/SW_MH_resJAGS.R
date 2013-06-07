
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
								 ret       <- data.frame( MAP = MAP, EAP = EAP, se = se, t(unlist(HPD)), t(unlist(ETI)), stringsAsFactors = FALSE )
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



### 03.04.2013 Plots erstmal nicht prioritär ###
plotJAGS <- function ( JAGS.object , burnin = 1000 ) {
browser()	

		prepped <- .prepJAGS ( JAGS.object , burnin )
		obj <- prepped$obj
		burnin.l <- prepped$burnin.l
		
		plot ( a[1,] )
		
		burnin <- 1000
		
		a2 <- data.frame ( "Iteration" = seq ( along = a[1,] ) , "Estimate" = a[1,] )
			
		ggplot ( dat = a2 , aes(x=Iteration, y=Estimate) ) +
		geom_point( shape=16, size=4.25 ) +
		geom_point( shape=16, size=1, color="red" ) +
		geom_smooth ( method="loess" , se=FALSE , size = 1.5 , color="red" ) +
		geom_smooth ( dat = a2[a2$Iteration>burnin,] , method="lm" , se=TRUE , size = 1 , color="yellow" )
		
		
		ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)

sapply ( e2 , make.plots )		
		
}





		   
		   