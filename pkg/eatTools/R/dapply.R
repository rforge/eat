
dapply <- function ( data , split.vars = NULL , fun = mean , new.name = NULL , wide = TRUE , drop = TRUE , all.level = FALSE , verbose = FALSE , ... ) {

		# Check-Vektor
		ok <- as.logical()

		# data is nicht data.frame
		if ( is.data.frame ( data ) ) {
				
				ok[1] <- TRUE
				
				# Response Variablen
				respVars <- colnames ( data )[! colnames ( data ) %in% split.vars]
				
				# keine Response-Variablen im Datensatz, dann eine setzen
				if ( ! identical ( respVars , character(0) ) ) {
						ok[2] <- TRUE
						if ( verbose ) cat ( paste0( "variable(s) to apply function(s): " , paste ( respVars , collapse = ", " ) , "\n" ) )
				} else {
						ok[2] <- FALSE
						if ( verbose ) cat ("no variable(s) to apply function(s)\n")
				}	
				
		} else {
				ok[1] <- FALSE
		}
		
		if ( all ( ok ) ) {
				
				# Splitvariablen nicht im Datensatz
				splv <- !split.vars %in% colnames(data)
				if ( any ( splv ) ) {
						warning ( paste0 ( "split.vars = " , paste ( split.vars[splv] , collapse = ", " ) , " are not in data and will be ignored." ) )
						split.vars <- split.vars[!splv]
						if ( identical ( split.vars , character(0) ) ) split.vars <- NULL
				}
				
				# wenn keine Splitvariablen
				if ( is.null ( split.vars ) ) {
						data[,ncol(data)+1] <- "all"
						del.col <- split.vars <- colnames ( data )[ncol(data)]
						verbose <- FALSE
				} else {
						del.col <- NULL
				}

				# wenn numerische split.vars, diese auf character
				isnum <- sapply ( data[,split.vars,drop=FALSE] , is.numeric )
				if ( any ( isnum ) ) {
						old.class <- sapply ( data[,names(isnum)[isnum],drop=FALSE] , class )
						do <- paste0 ( "data$" , names(isnum)[isnum] , " <- as.character ( data$" , names(isnum)[isnum] , " )" )
						eval ( parse ( text = do ) )
				}
		
				# wenn von Faktoren alle Level (auch die nicht im Datensatz sind verwendet werden sollen
				unique.fun <- rep ( "unique" , length ( split.vars ) )
				if ( all.level ) {
						isfac <- sapply ( data[,split.vars,drop=FALSE] , is.factor )
						unique.fun[isfac] <- "levels"
						
						# noch ne Meldung wenn hier drop=TRUE
						if ( drop & verbose ) {
								cat ( "all.level=TRUE and drop=TRUE implies that empty factor levels are first added and then dropped again. Check if this is intended.\n" )
						}
				}
				
				# Zellen
				do <- paste0 ( "cells <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE ),list( " , paste ( paste0 ( "data.frame ( ",unique.fun," ( data$",split.vars," ) )" ) , collapse = " , " ) , ") , accumulate=FALSE ) " )
				eval ( parse ( text = do ) )
				colnames ( cells ) <- split.vars
				
				schleifeD <- function ( vec , d , fun , split.vars , respVars , new.name , wide , drop , verbose , ... ) {

						if ( verbose ) {
								out <- paste0 ( "" , paste ( paste0 ( "" , names ( vec ) , "=" , vec , "" ) , collapse = " " ) , " " )
								cat ( out )
								flush.console()
						}

						# Subdatensatz machen
						do <- paste0 ( "d <- d [ " , paste ( paste0 ( "d$" , names ( vec ) , " %in% '" , vec , "'" ) , collapse = " & " ) , " , , drop = FALSE ]" )
						eval ( parse ( text = do ) )
						
						# Ausgabe Länge der Zelle
						if ( verbose ) cat ( paste0 ( "(N=" , nrow ( d ) , ") " ) )
						
						# Gruppendatensatz
						do <- paste0 ( "r <- data.frame ( " , paste ( paste0 ( "'" , names ( vec ) , "' = '" , vec , "'" ) , collapse = " , " ) , " , stringsAsFactors = FALSE )" )
						eval ( parse ( text = do ) )
				
						# wenn Zelle existiert, dann Statistik berechnen
						# wenn Zelle nicht exisitert, dann entscheiden
						# entweder NA (drop=FALSE)
						# oder entfernen (drop=TRUE)
						if ( nrow ( d ) > 0 ) {
						
								# Statistik berechnen
								appl.fun <- function ( vec , respVar , num , fun , verbose , ... ) {
										if ( verbose ) cat ( paste0( "var",num,"=", respVar , " " ) )
										flush.console()
						
										# wenn fun eine Funktion ist, Liste drausmachen, um drüber zu lapplyen
										if ( is.function ( fun ) ) {
												fun <- list ( fun )
										}
										
										# Statistiken berechnen
										callfun <- function ( fun , vec , ... ) {
												eval ( as.call ( list ( fun, quote(vec) , quote(...) ) ) )
										}
										l <- lapply ( fun , callfun , vec , ... )
										
										# Namen
										if ( !is.null ( names ( fun ) ) ) {
												names ( l ) <- names ( fun )
										} else {
												if ( length ( fun ) > 1 ) {
														names ( l ) <- paste0 ( "fun" , seq ( along = fun ) )
												} else {
														names ( l ) <- "fun"
												}
										}
										
										return ( l )
								}
								resl <- mapply (  appl.fun ,
												  d[,respVars,drop=FALSE] , respVars , seq ( along = respVars ) ,
												  MoreArgs = list ( fun , verbose , ... ) , SIMPLIFY = FALSE )

								# eine Liste ohne Verschachtelung machen
								resl <-	unlist ( resl , recursive = FALSE )
								
						} else {
								if ( verbose ) cat ( "empty cell" )
								
								if ( drop ) {
										resl <- NULL
										if ( verbose ) cat ( " is dropped" )
								} else {
										resl <- list ( rep ( NA , length(respVars) ) )
										names ( resl ) <- respVars
										if ( verbose ) cat ( ": NA generated" )
								}
						}
						if ( verbose ) cat ( "\n" )
						
						if ( !is.null ( resl ) ) {
						
								# neue Variablennamen
								newName <- names ( resl )
								
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
						
						} else {
								resd <- NULL
						}
						
						return ( resd )
				}
				r.l <- apply ( cells , 1 , schleifeD , data , fun , split.vars , respVars , new.name , wide , drop , verbose , ... ) 
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
				
				# numerische split.vars wieder zurücksetzen
				if ( any ( isnum ) ) {
						do <- paste0 ( "data$" , names(old.class) , " <- as.",old.class," ( data$" , names(old.class) , " )" )
						eval ( parse ( text = do ) )
				}
				
				# rownames
				rownames ( r ) <- seq ( along = rownames ( r ) )
				
		} else {
				r <- NULL
		}
		
		return ( r )
}
