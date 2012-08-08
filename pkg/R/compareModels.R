		
compareModels <- function ( path , xlsx = NULL ) {
	
		if ( !is.list ( path ) & length ( path ) > 1 ) shwf <- as.list ( path )
		
		if ( !is.list ( path ) & length ( path ) == 1 ) {
				
				# wenn Directory dann rekursiv alle shw files suchen
				fi <- file.info ( path )$isdir
				fi <- if ( is.na(fi) ) FALSE else fi
				if ( fi ) {
						shwf <- list.files ( path = path , 
										   pattern = "\\.shw$" , all.files = FALSE,
										   full.names = TRUE, recursive = TRUE,
										   ignore.case = FALSE, include.dirs = FALSE )
				} else if ( file.exists(path) ) {
						shwf <- list ( path )
				} else shwf <- NULL
				
				if ( identical ( shwf , character(0) ) ) shwf <- NULL
		} 
		
		if ( is.list ( path ) ) shwf <- unlist ( path )
		if ( is.list ( shwf ) ) shwf <- unlist ( shwf )
		
		if ( !is.null ( shwf ) ) {
		
				# checken auf readable
				.readable <- function ( shwf ) {
						file.access ( shwf , 4 )
				}
				readable <- mapply ( .readable , shwf )
				readable <- as.logical ( readable + 1 )
				if ( !all ( readable ) ) {
						w <- which ( !readable )
						m <- paste ( "Show-File(s) " , paste(shwf[w],collape=", ") , "is/are not readable and will be excluded." , sep = "" )
						warning ( m )
						shwf <- shwf[-w]
				}
				
				.fun1 <- function ( shwf ) {

						dn <- dirname ( shwf )
						
						# kompletter show-File
						tried <- try ( l <- readLines( shwf ) , silent = TRUE )
						if ( inherits ( tried , "try-error" ) ) stop ( paste ( "could not open file" , shwf ) )
						
						# deviance
						what <- "Sample size:"
						l1 <- l[grep ( what , l )]
						sz <- as.integer ( crop ( sub ( what , "" , l1 ) ) )						
						if ( identical ( sz , integer(0) ) ) sz <- NA
						if ( ! length ( sz ) == 1 ) sz <- NA
						what <- "Final Deviance:"
						l1 <- l[grep ( what , l )]
						dev <- as.numeric ( crop ( sub ( what , "" , l1 ) ) )
						if ( identical ( dev , numeric(0) ) ) dev <- NA
						if ( ! length ( dev ) == 1 ) dev <- NA
						what <- "Total number of estimated parameters:"
						l1 <- l[grep ( what , l )]
						np <- as.integer ( crop ( sub ( what , "" , l1 ) ) )
						if ( identical ( np , integer(0) ) ) np <- NA
						if ( ! length ( np ) == 1 ) np <- NA
						
						ret <- data.frame ( "N" = sz , "Deviance" = dev , "Parameter" = np )
						
						return ( ret )
				}
				models <- mapply ( .fun1 , shwf , SIMPLIFY = FALSE )
				
				# Analyse-Namen
				na <- names ( models )
				na <- sub ( ".shw" , "" , na , fixed = TRUE )
				ba <- basename ( na )
				dn <- dirname ( na )
				newna <- ba
				# wenn basename nicht unique, dann versuchen ein Ordner vor dem / noch ran
				if ( any ( ( w <- duplicated ( newna ) ) ) ) {
						newna_d <- paste ( basename ( dn ) , ba[w] , sep = "/" )
						newna[w] <- newna_d
						# wenn immer noch nicht unique, Pech, dann mit make.unique
						if ( any ( duplicated ( newna ) ) ) {
								newna <- make.unique ( newna )
						}
				} else newna <- ba
				names ( models ) <- newna
				
				models <- do.call ( "rbind" , models )
				models$Model <- rownames ( models )
				models <- reinsort.col ( models , "Model" , "N" )
				models <- reinsort.col ( models , "N" , "Model" )
				rownames ( models ) <- seq ( along = rownames ( models ) )
				
				# Kennwerte berechnen
				### Bitte jemand gegenchecken ###
				# http://en.wikipedia.org/wiki/Akaike_information_criterion
				models$AIC <- models$Deviance + 2 * models$Parameter
				# http://en.wikipedia.org/wiki/Bayesian_information_criterion
				models$BIC <- models$Deviance + models$Parameter * log ( models$N )
				
		} else models <- NULL
				
		if ( is.null ( models ) ) {
				model.comparison <- NULL
		} else if ( nrow ( models ) == 1 ) {
				model.comparison <- NULL
		} else if ( nrow ( models ) > 1 ) {
				
				combs <- combVec ( models$Model )
				comp <- function ( m1 , m2 , na , d ) {
						ret <- data.frame ( "Combination" = na ,
											"Model1" = m1 ,
											"Model2" = m2 ,
											"AICdiff" = d[d$Model==m1,"AIC"] - d[d$Model==m2,"AIC"] ,
											"BICdiff" = d[d$Model==m1,"BIC"] - d[d$Model==m2,"BIC"] ,
											"DevianceDiff" = d[d$Model==m1,"Deviance"] - d[d$Model==m2,"Deviance"] ,
											"ParameterDiff" = d[d$Model==m1,"Parameter"] - d[d$Model==m2,"Parameter"] )
						ret$Chi2p <- pchisq ( abs(ret$DevianceDiff) , abs(ret$ParameterDiff) , lower.tail = FALSE )
						
						return(ret)
				}
				model.comparison <- mapply ( comp , combs[,1] , combs[,2] , combs[,3] , MoreArgs = list ( models ) , SIMPLIFY = FALSE )
				model.comparison <- do.call ( "rbind" , model.comparison )
				rownames ( model.comparison ) <- seq ( along = rownames ( model.comparison ) )
		} else {
				model.comparison <- NULL
		}
			
		ret <- list ( models , model.comparison )
		names ( ret ) <- c ( "models" , "model.comparison" )
		
		# Excel
		if ( ! is.null ( xlsx ) & ! is.null ( models ) ) {
		
				try ( write.xlsx2 ( ret$models, file = xlsx , sheetName="models" , 
						col.names=TRUE, row.names=TRUE, append=FALSE ) )
				if ( ! is.null ( ret$model.comparison ) ) {
						try ( write.xlsx2 ( ret$model.comparison, file = xlsx , sheetName="modelComparison" , 
								col.names=TRUE, row.names=TRUE, append=TRUE ) )
				}
		}		
		
		# returnen 
		return ( ret )
				
}


