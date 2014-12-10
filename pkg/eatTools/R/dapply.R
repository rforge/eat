
dapply <- function ( data , split.vars = NULL , fun = mean , new.name = NULL , wide = TRUE , verbose = FALSE , ... ) {

		# Response Variablen
		respVars <- colnames ( data )[! colnames ( data ) %in% split.vars]
		
		# Check-Vektor
		ok <- as.logical()
		
		# keine Response-Variablen im Datensatz
		if ( !identical ( respVars , "character(0)" ) ) {
				ok[1] <- TRUE
		} else {
				ok[1] <- FALSE
		}
		
		if ( all ( ok ) ) {
		
				# Splitvariablen nicht im Datensatz
				splv <- !split.vars %in% colnames(data)
				if ( any ( splv ) ) {
						warning ( paste0 ( "split.vars = " , paste ( split.vars[splv] , collapse = ", " ) , " are not in data and will be ignored." ) )
						split.vars <- split.vars[!splv]
						if ( identical ( split.vars , "character(0)" ) ) split.vars <- NULL
				}
				
				# wenn keine Splitvariablen
				if ( is.null ( split.vars ) ) {
						data[,ncol(data)+1] <- "all"
						del.col <- split.vars <- colnames ( data )[ncol(data)]
						verbose <- FALSE
				} else {
						del.col <- NULL
				}
				
				# Zellen
				do <- paste0 ( "cells <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE ),list( " , paste ( paste0 ( "data.frame ( unique ( data$",split.vars," ) )" ) , collapse = " , " ) , ") , accumulate=FALSE ) " )
				eval ( parse ( text = do ) )
				colnames ( cells ) <- split.vars

				schleifeD <- function ( vec , d , fun , split.vars , respVars , new.name , wide , verbose , ... ) {

						if ( verbose ) {
								out <- paste0 ( "" , paste ( paste0 ( "" , names ( vec ) , "=" , vec , "" ) , collapse = " " ) , " " )
								cat ( out )
								flush.console()
						}
					
						# Subdatensatz machen
						do <- paste0 ( "d <- d [ " , paste ( paste0 ( "d$" , names ( vec ) , " %in% '" , vec , "'" ) , collapse = " & " ) , " , , drop = FALSE ]" )
						eval ( parse ( text = do ) )
						
						# Gruppendatensatz
						do <- paste0 ( "r <- data.frame ( " , paste ( paste0 ( "'" , names ( vec ) , "' = '" , vec , "'" ) , collapse = " , " ) , " , stringsAsFactors = FALSE )" )
						eval ( parse ( text = do ) )
				
						# Statistik berechnen
						# resl <- lapply ( d[,respVars,drop=FALSE] , function ( vec , fun , verbose , ... ) { if ( verbose ) cat ( "#" ); flush.console(); eval ( as.call ( list ( fun , quote(vec) , quote(...) ) ) ) } , fun , verbose , ... )
						appl.fun <- function ( vec , respVar , num , fun , verbose , ... ) {
								if ( verbose ) cat ( paste0( "var",num,"=", respVar , " " ) )
								flush.console()
								eval ( as.call ( list ( fun , quote(vec) , quote(...) ) ) )
						}
						resl <- mapply (  appl.fun ,
										  d[,respVars,drop=FALSE] , respVars , seq ( along = respVars ) ,
										  MoreArgs = list ( fun , verbose , ... ) , SIMPLIFY = FALSE )
						if ( verbose ) cat ( "\n" )
						
						# neue Variablennamen
						if ( ! is.null ( new.name ) ) {
								newName <- make.unique ( rep_len ( new.name , length.out = length ( respVars ) ) )
						} else {
								newName <- respVars
						}
						
						# wide Datensatz
						if ( wide ) {
								resd <- data.frame ( resl )
								colnames ( resd ) <- newName
							
								# Gruppen hinzu
								do2 <- paste0 ( "resd$" , split.vars , " <- '" , r[,split.vars] , "'" ) 
								eval ( parse ( text = do2 ) )
								
								# sortieren
								resd <- resd[,c(split.vars,colnames(resd)[!colnames(resd) %in% split.vars])]
								
						# long Datensatz
						} else {
								f1 <- function ( v , vnam , r , split.vars ) {
										resd <- data.frame ( v , stringsAsFactors = FALSE )
										colnames ( resd ) <- "value"
										resd$var <- vnam
										do2 <- paste0 ( "resd$" , split.vars , " <- '" , r[,split.vars] , "'" )
										eval ( parse ( text = do2 ) )
										return ( resd )
								}
								resl2.l <- mapply ( f1 , resl , newName , MoreArgs = list ( r , split.vars ) , SIMPLIFY = FALSE ) 
								resl2 <- do.call ( "rbind" , resl2.l )
								
								# sortieren
								resd <- resl2[,c(split.vars,"var","value")]
						
						}

						return ( resd )
				}
				r.l <- apply ( cells , 1 , schleifeD , data , fun , split.vars , respVars , new.name , wide , verbose , ... ) 
				r <- do.call ( "rbind" , r.l )

				# ggf. hinzugefügte split.var löschen
				if ( !is.null ( del.col ) ) {
						r <- r[ , !colnames ( r ) %in% del.col , drop = FALSE ]
						split.vars <- split.vars[!split.vars %in% del.col]
				}	
				
				# Faktoren wieder als Faktoren
				facs <- sapply ( data[,split.vars,drop=FALSE] , is.factor )
				if ( any ( facs ) ) {
						facs.nam <- names ( facs )[facs]
						
						do <- paste0 ( "r$" , facs.nam , " <- factor ( r$" , facs.nam , " , levels = levels ( data$" , facs.nam , " ) )" )
						eval ( parse ( text = do ) )
						
				}
				
				# rownames
				rownames ( r ) <- seq ( along = rownames ( r ) )
				
		} else {
				r <- NULL
		}
		
		return ( r )
}
