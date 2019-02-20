
dapply <- function ( data , split.vars = NULL , fun = mean , new.name = NULL , wide = TRUE , drop = TRUE , all.level = FALSE , push.data.frame = FALSE , verbose = FALSE , ... ) {

		# Check-Vektor
		ok <- as.logical()

		# data is nicht data.frame
		if ( is.data.frame ( data ) ) {
				
				ok[1] <- TRUE
				
				# Response Variablen
				respVars <- colnames ( data )[! colnames ( data ) %in% split.vars]
				
				# keine Response-Variablen im Datensatz
				if ( ! identical ( respVars , character(0) ) ) {
						ok[2] <- TRUE
						if ( verbose ) {
								if ( !push.data.frame ) {
										cat ( paste0( "function(s) is/are applied to variable(s): " , paste ( respVars , collapse = ", " ) , "\n" ) )
								} else {
										cat ( paste0( "function(s) is/are applied to data.frame with variable(s): " , paste ( respVars , collapse = ", " ) , "\n" ) )
								}
						}
				} else {
						ok[2] <- FALSE
						if ( verbose ) cat ("no variable(s) to apply function(s)\n")
				}	
				
		} else {
				ok[1] <- FALSE
		}

		# fun ist entweder eine Funktion oder mehrere
		if ( is.function ( fun ) ) {
				ok[3] <- TRUE
				
				# wenn fun eine einzelne Funktion ist, Liste drausmachen, um später drüber zu lapplyen
				fun <- list ( fun )
		} else {
				if ( all ( sapply ( fun , is.function ) ) ) {
						ok[3] <- TRUE
				} else {
						ok[3] <- FALSE
				}
		}
		
		if ( all ( ok ) ) {
				
				# Funktionsnamen
				if ( !is.null ( names ( fun ) ) ) {
						names ( fun ) <- make.unique ( names ( fun ) )
				} else {
						if ( length ( fun ) > 1 ) {
								names ( fun ) <- paste0 ( "fun" , seq ( along = fun ) )
						} else {
								names ( fun ) <- "fun"
						}
				}
				
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
				
				schleifeD <- function ( vec , d , fun , split.vars , respVars , new.name , wide , drop , push.data.frame , verbose , ... ) {

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
								appl.fun <- function ( vec , push.respVars , num , fun , verbose , ... ) {
										if ( verbose ) cat ( paste0( "var",num,"=", push.respVars , " " ) )
										flush.console()

										# Statistiken berechnen
										callfun <- function ( fun , vec , ... ) {
												eval ( as.call ( list ( fun, quote(vec) , quote(...) ) ) )
										}
										l <- lapply ( fun , callfun , vec , ... )
										
										# Namen
										# if ( !is.null ( names ( fun ) ) ) {
												# names ( l ) <- names ( fun )
										# } else {
												# if ( length ( fun ) > 1 ) {
														# names ( l ) <- paste0 ( "fun" , seq ( along = fun ) )
												# } else {
														# names ( l ) <- "fun"
												# }
										# }
										
										return ( l )
								}
								# push.data.frame
								# wenn ganzer data.frame übergeben werden soll, diesen listen
								if ( push.data.frame ) {
										push.d <- list ( d[,respVars,drop=FALSE] )
										push.respVars <- paste ( respVars , collapse = ", " )
								} else {
										push.d <- d[,respVars,drop=FALSE]
										push.respVars <- respVars
								}

								resl <- mapply (  appl.fun ,
												  push.d , push.respVars , seq ( along = push.respVars ) ,
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

								# aus Liste data.frame machen
								mdfr <- function ( l , nam ) {
										dfr <- data.frame ( l , stringsAsFactors = FALSE )
										if ( ncol ( dfr ) > 1 ) {
												colnames ( dfr ) <- paste0 ( nam , "." , seq ( along = colnames ( dfr ) ) )
										} else {
												colnames ( dfr ) <- nam
										}
										return ( dfr )
								}
								resd.l <- mapply ( mdfr , resl , names ( resl ) , SIMPLIFY = FALSE )
								
								# gucken ob cbinden (=wide) geht
								resd <- try ( do.call ( "cbind" , resd.l ) , silent = TRUE )
								
								# Warnmeldung
								if ( wide & inherits(resd,"try-error") ) {
										wide <- FALSE
										if ( verbose ) cat ( "results cannot be structured in wide format. long format is returned.\n" )
								}
								
								# wide Datensatz
								if ( wide ) {
						
										# Gruppen hinzu
										do2 <- paste0 ( "resd$" , split.vars , " <- '" , r[,split.vars] , "'" ) 
										eval ( parse ( text = do2 ) )
									
										# sortieren
										resd <- resd[,c(split.vars,colnames(resd)[!colnames(resd) %in% split.vars]),drop=FALSE]
										
								# long Datensatz
								} else {
										
										# Einzeldatensätze nach long
										tolong <- function ( l ) {
												tolong2 <- function ( sp , nam ) {
														dfr <- data.frame ( sp , stringsAsFactors = FALSE )
														dfr[,ncol(dfr)+1] <- nam
														colnames ( dfr ) <- c ("value","var")
														dfr$var <- as.character ( dfr$var )
														return ( dfr )
												}
												mapply ( tolong2 , l , colnames ( l ) , SIMPLIFY = FALSE )
										}
										resd.l2 <- lapply ( resd.l , tolong )
										resd.l3 <- unlist ( resd.l2 , recursive = FALSE )
										
										# wenn value alles numerisch ok, wenn nicht auf as.character
										isnumeric <- sapply ( resd.l3 , function ( x ) is.numeric ( x$value ) )
										if ( ! all ( isnumeric ) ) {
												resd.l3 <- lapply ( resd.l3 , function ( x ) { x$value <- as.character ( x$value ); return(x)} )
										}
										resd <- do.call ( "rbind" , resd.l3 )
										
										# Gruppen hinzu
										do2 <- paste0 ( "resd$" , split.vars , " <- '" , r[,split.vars] , "'" ) 
										eval ( parse ( text = do2 ) )										

										# sortieren
										resd <- resd[,c(split.vars,"var","value")]
								
								}
						
						} else {
								resd <- NULL
						}
						
						return ( resd )
				}
				r.l <- apply ( cells , 1 , schleifeD , data , fun , split.vars , respVars , new.name , wide , drop , push.data.frame , verbose , ... ) 
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
						do <- paste0 ( "r$" , names(old.class) , " <- as.",old.class," ( r$" , names(old.class) , " )" )
						eval ( parse ( text = do ) )
				}
				
				# rownames
				rownames ( r ) <- seq ( along = rownames ( r ) )
				
		} else {
				r <- NULL
		}
		
		return ( r )
}
