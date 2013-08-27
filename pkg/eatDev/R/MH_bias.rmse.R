
bias.rmse <- function ( true , est , id.col , val.col , repl.col = NULL , group.col = NULL , verbose = FALSE ) {
		
		# wenn group.col gesetzt, checken ob diese nur in true Datensatz
		# und nicht auch in est Datensatz, ggf. loeschen
		if ( !is.null ( group.col ) ) {
				if ( group.col %in% colnames ( est ) ) {
						est <- est[,colnames(est)[!colnames(est) %in% group.col ]]
				}
		}
		
		# Spalten in true bestimmen, die "zu viel" sind
		# das sind die zusaetzlichen Split-Variablen
		true.split <- colnames ( true ) [ !colnames ( true ) %in% c ( id.col , val.col , group.col ) ]
		if ( identical ( true.split , character(0) ) ) {
				true.split <- NULL
		}
		
		# id.col muss (in true Datensatz) unique sein (je true.split)
		if ( any ( duplicated ( true [ , c ( id.col , true.split ) , drop = FALSE ] ) ) ) {
				stop ( paste0 ( "values in id.col (" , id.col , ") in true data set are not unique" ) )
		}

		# wenn keine Replicate Spalte, dann hat alles nur ein Replicate
		if ( is.null ( repl.col ) ) {
				est$"__repl__" <- 1
				repl.col <- "__repl__"
		}
		
		# Split-Variablen im Estimates-Datensatz bestimmen
		nams <- colnames ( est )[!colnames ( est ) %in% repl.col]
		spl.vars <- nams [ ! nams %in% colnames ( true ) ]
		if ( !is.null ( true.split ) ) spl.vars <- c ( spl.vars , true.split )
		if ( identical ( spl.vars , character(0) ) ) spl.vars <- NULL
		
		# wenn keine Gruppenvariable vorhanden, dann dummy maesig setzen auf NA
		if ( is.null ( group.col ) ) {
				true$"__group__" <- NA
				group.col <- "__group__"
				rename.later <- TRUE
		} else {
				rename.later <- FALSE
		}
		
		# NAs loswerden auf group.col
		if ( any ( is.na ( true[,group.col] ) ) ) {
				true[,group.col][ is.na ( true[,group.col] ) ] <- paste0 ( "__group__" , 1:(length(which(is.na ( true[,group.col] ))) ) )
		}
		
		# Liste ueber groups und weiteren split-vars auf true Datensatz
		do <- paste0 ( "true.l <- split ( true , f = list ( " , paste ( paste0 ( "true[,'",c(group.col,true.split),"']" ) , collapse = " , " ) , " ) , drop = TRUE ) " )
		eval ( parse ( text = do ) )
		
		# ueber true Datensatz schleifen
		f1 <- function ( d , e , spl.vars , id.col , val.col , group.col , repl.col , true.split ) {

				# match Variablen in true fuer estimates
				m.vars <- colnames ( d ) [ ! colnames ( d ) %in% c ( val.col , group.col ) ]
				
				do1 <- paste0 ( "e2 <- e[" , paste ( paste0 ( "e$" , m.vars , " %in% d[,'" , m.vars , "']" ) , collapse = " & " ) , ",,drop=FALSE]" )
				eval ( parse ( text = do1 ) )
		
				# ranmergen true value und group.var
				e3 <- merge ( e2 , d[,c(m.vars,val.col,group.col)] , by = m.vars )
				
				# splitten, wenn split vars vorhanden
				if ( ! is.null ( spl.vars ) ) {
						
						do2 <- paste0 ( "e4 <- split ( e3 , f = list ( " , paste ( paste0 ( "e3[,'",spl.vars,"']" ) , collapse = " , " ) , " ) , drop = TRUE ) " ) 
						eval ( parse ( text = do2 ) )
						
				} else {
						e4 <- list ( "all" = e3 )
				}
				
				# jetzt ist alles auf untersten Split, bis auf Replicates
				f2 <- function ( l , nam , nr , repl.col , val.col , group.col , maxnr ) {
					
						# Ausgabe 1/X
						if ( verbose ) {
								cat ( paste0 ( Sys.time() , "   " , nr , "/" , maxnr , "  " , nam ) )
								flush.console()
						}
						
						# nach Replicates splitten
						l2 <- split ( l , f = list ( l[,repl.col] ) , drop = TRUE )
						
						# l2 ist jetzt auf unterster Ebene
						# d.h. enthaelt Gruppenelemente (z.B. Items) in den Datensaetzen der Replikationen
						# jetzt Formeln aus Babcock/Albano 2012 anwenden
						
						f3 <- function ( d , val.col , id.col , group.col ) {
							
								if ( verbose ) {
										cat ( paste0 ( "." ) )
										flush.console()
								}
						
								# Check, id.col muss hier unique sein
								# wenn nicht Warnung
								if ( any ( duplicated ( d[,id.col] ) ) ) {
										cat ( paste0 ( "warning: values on id.col (" , id.col , ") are not unique in innerst loop. check your data and function parameter settings!\n" ) )
										cat ( paste0 ( "group.col ( " , group.col , ") table: " , "\n" ) )
										cat ( paste0 ( names ( table ( d[,group.col] ) ) , "\n" ) )
										cat ( paste0 ( table ( d[,group.col] ) , "\n" ) )
								}
								
								### Funktionen ###
								# Bias
								bias <- function ( d ) {
										# q <- apply ( d , 1 , function ( v ) (v[1] - v[2]) )
										q <- d[,1] - d[,2]
										q2 <- sum ( q )
										q3 <- q2 / nrow ( d )
										return ( q3 )
								}	
								# Standardabweichung ohne Bias
								biasfree.sd <- function ( d , bias ) {
										# q <- apply ( d , 1 , function ( v , bias ) ( ( ( v[1] - v[2] ) - bias )^2 ) , bias )
										q <- ( ( d[,1] - d[,2] ) - bias )^2
										q2 <- sum ( q )
										q3 <- q2 / nrow ( d )
										q4 <- sqrt ( q3 )
										return ( q4 )
								}	
								# MSE
								MSE <- function ( d ) {
										# q <- apply ( d , 1 , function ( v ) (v[1] - v[2])^2 )
										q <- ( d[,1] - d[,2] )^2
										q2 <- sum ( q )
										q3 <- q2 / nrow ( d )
										return ( q3 )
								}
								# RMSE
								RMSE <- function ( d ) {
										# q <- apply ( d , 1 , function ( v ) (v[1] - v[2])^2 )
										q <- ( d[,1] - d[,2] )^2
										q2 <- sum ( q )
										q3 <- q2 / nrow ( d )
										q4 <- sqrt ( q3 )
										return ( q4 )
								}
								# Korrelation
								calc.cor <- function ( d ) {
										if ( (length( d[,1] ) > 1) & !any ( is.na( d[,1] )) & !any(is.na( d[,2] )) ) {
												cr <- try ( cor (d[,1] , d[,2]) )
												if ( inherits ( cr , "try-error" ) ) {
														cr <- NA
												}
										} else {
												cr <- NA
										}
										return ( cr )
								}								
								
								# Datensatz reduzieren auf estimate + true value
								val.cols <- c ( paste0 ( val.col , ".x" ) , paste0 ( val.col , ".y" ) )
								d2 <- d[ , val.cols ]
						
								# Rueckgabe-Datensatz
								bias.val <- bias ( d2 )
								res1 <- data.frame ( "bias" = bias.val , "biasfree.sd" = biasfree.sd ( d2 , bias.val ) ,"MSE" = MSE ( d2 ) , "RMSE" = RMSE ( d2 ) , "cor" = calc.cor ( d2 ) , stringsAsFactors = FALSE )
								
								# alle split vars noch setzen
								nams <- colnames ( d )[ !colnames ( d ) %in% c( id.col , val.cols )]
								res2 <- cbind ( d[1,nams,drop=FALSE] , res1 )
								
								# wenn group nur aus einer Variablen besteht, dann noch den Variablennamen setzen
								if ( grepl ( "__group__" , res2[,group.col] ) ) res2[,group.col] <- d[1,id.col]
								
								return ( res2 )
								
						}
						res3.l <- mapply ( f3 , l2 , MoreArgs = list ( val.col , id.col , group.col ) , SIMPLIFY = FALSE )
						if ( verbose ) {
								cat ( paste0 ( "\n" ) )
						}
						res3 <- do.call ( "rbind" , res3.l )

						# Mitteln ueber Replicates
						res4 <- res3[1,colnames(res3)[!colnames(res3) %in% repl.col],drop=FALSE]
						res4$bias <- mean ( res3$bias )
						res4$biasfree.sd <- mean ( res3$biasfree.sd )
						res4$MSE <- mean ( res3$MSE )
						res4$RMSE <- mean ( res3$RMSE )
						res4$cor <- mean ( res3$cor )
						
						return ( res4 )
				}
				res1.l <- mapply ( f2 , e4 , names ( e4 ) , seq ( along = e4 ) , MoreArgs = list ( repl.col , val.col , group.col , length ( e4 ) ) , SIMPLIFY = FALSE )				
				res1 <- do.call ( "rbind" , res1.l )

		}
		res.l <- mapply ( f1 , true.l , MoreArgs = list ( est , spl.vars , id.col , val.col , group.col , repl.col , true.split ) , SIMPLIFY = FALSE )
		res <- do.call ( "rbind" , res.l )
		rownames ( res ) <- seq ( along = rownames ( res ) )
		
		# wenn keine group.col gesetzt war, dann die kuenstlich erzeugt group.col nach id.col benennen
		if ( rename.later ) {
				colnames ( res ) [ colnames ( res ) == group.col ] <- id.col
		}
		
		return ( res )
}

### Beispiele ###

### Beispiel 1, keine Replicates
# true <- data.frame ( "variable" = c ("item1","item2") ,
					 # "value" = c ( 1 , 2 ) ,
					 # "group" = rep ( "items" , 2 ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = c ("item1","item2") ,
					 # "value" = rnorm ( 2 , 1 , 1 ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" )	

### Beispiel 2, zusaetzlich 2 Replicates
# true <- data.frame ( "variable" = c ("item1","item2") ,
					 # "value" = c ( 1 , 2 ) ,
					 # "group" = rep ( "items" , 2 ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = c ("item1","item1","item2","item2") ,
					 # "value" = rnorm ( 4 , 1 , 1 ) ,
					 # "replicates" = c ( 1 , 2 , 1 , 2 ) , 					 
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" )		 
					 
### Beispiel 3, zusaetzlich 2 data conditions
# true <- data.frame ( "variable" = rep ( c ("item1","item2") , 2 ) ,
					 # "value" = 1:4 ,
					 # "group" = rep ( rep ( "items" , 2 ) , 2 ) , 
					 # "data.cond" = c ( rep ( 1 , 2 ) , rep ( 2 , 2 ) ) ,
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = rep ( c ("item1","item1","item2","item2") , 2 ) ,
					 # "value" = rnorm ( 8 , 1 , 1 ) ,
					 # "replicates" = rep ( c ( 1 , 2 , 1 , 2 ) ) , 
					 # "data.cond" = c ( rep ( 1 , 4 ) , rep ( 2 , 4 ) ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" )		 
	
### Beispiel 4, 2 models
# true <- data.frame ( "variable" = c ("item1","item2") ,
					 # "value" = c ( 1 , 2 ) ,
					 # "group" = rep ( "items" , 2 ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = rep ( c ("item1","item1","item2","item2") , 2 ),
					 # "value" = rnorm ( 8 , 1 , 1 ) ,
					 # "replicates" = rep ( c ( 1 , 2 , 1 , 2 ) ) , 
					 # "model" = c ( rep ( "m1" , 4 ) , rep ( "m2" , 4 ) ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" )		 

### Beispiel 5, 2 data conditions und 2 models
# true <- data.frame ( "variable" = rep ( c ("item1","item2") , 2 ) ,
					 # "value" = 1:4 ,
					 # "group" = rep ( rep ( "items" , 2 ) , 2 ) , 
					 # "data.cond" = c ( rep ( 1 , 2 ) , rep ( 2 , 2 ) ) ,
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = rep ( rep ( c ("item1","item1","item2","item2") , 2 ) , 2 ),
					 # "data.cond" = rep ( c ( rep ( 1 , 4 ) , rep ( 2 , 4 ) ) , 2 ) ,
					 # "value" = rnorm ( 16 , 1 , 1 ) ,
					 # "replicates" = rep ( rep ( c ( 1 , 2 , 1 , 2 ) ) , 2 ) , 
					 # "model" = c ( rep ( "m1" , 8 ) , rep ( "m2" , 8 ) ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" )

### Beispiel 6, mehrere groups
# true <- data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 # "value" = 1:5 ,
					 # "group" = c ( rep ( "items" , 2 ) , rep ( "persons" , 2 ) , NA ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 # "value" = rnorm ( 5 , 1 , 1 ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" )	



	

