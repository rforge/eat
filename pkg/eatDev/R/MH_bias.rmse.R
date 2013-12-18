
bias.rmse <- function ( true , est , id.col , val.col , repl.col = NULL , group.col = NULL , method = c ( "group" , "repl" ) , verbose = FALSE ) {
		
		# method bearbeiten
		# if ( length ( method ) > 1 ) method <- method[1]
		if ( ! all ( method %in% c ( "group" , "repl" ) ) ) {
				msg1 <- paste0 ( "method == " , paste ( method , collapse = ", " ) , " is not valid. method has been set to 'group'" )
				if ( verbose ) {
						cat ( paste0 ( "Warning: " , msg1 , "\n" ) )
				} else {
						warning ( msg1 )
				}
		}
		# wenn method == "group" heisst das über Groups die Kennwerte berechnet werden, und über Replicates gemittelt
		# wenn method == "repl" heisst das über Replicates die Kennwerte berechnet werden, und über Groups gemittelt
		# das ist nur für den biasfree.sd und RMSE relevant
		# bei "group" ist biasfree.sd/RMSE die Variabilität über die group-Elemente
		# bei "repl" ist biasfree.sd/RMSE die Variabilität über die replicates
		
		# wenn beide Methoden angefordert werden, dann mit beiden jetzt hier aufrufen
		if ( length ( method ) > 1 ) {
				res1 <- bias.rmse ( true=true,est=est,id.col=id.col,val.col=val.col,repl.col=repl.col,group.col=group.col,method="group",verbose=verbose )
				res1$method <- "group" 
				res2 <- bias.rmse ( true=true,est=est,id.col=id.col,val.col=val.col,repl.col=repl.col,group.col=group.col,method="repl",verbose=verbose )
				res2$method <- "repl"
				res <- rbind ( res1 , res2 )
		} else {

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
				f1 <- function ( d , e , spl.vars , id.col , val.col , group.col , repl.col , true.split , method ) {

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
						f2 <- function ( l , nam , nr , repl.col , val.col , group.col , maxnr , method ) {
							
								# Ausgabe 1/X
								if ( verbose ) {
										cat ( paste0 ( Sys.time() , "   " , nr , "/" , maxnr , "  " , nam , "  " ) )
										flush.console()
								}
								
								# in Abhängigkeit von method den split machen
								if ( method == "group" ) {
										# nach Replicates splitten
										# d.h. Kennwerte werden über group innerhalb jedes replicate berechnet
										l2 <- split ( l , f = list ( l[,repl.col] ) , drop = TRUE )
										if ( verbose ) {
												cat ( paste0 ( "aggregating over variable groups per replicate\n" ) )
												flush.console()
										}
								} else {
										# method == "repl"
										# nach variable splitten
										# d.h. Kennwerte werden über replicates innerhalb jedes Parameters berechnet
										l2 <- split ( l , f = list ( l[,id.col] ) , drop = TRUE )
										if ( verbose ) {
												cat ( paste0 ( "aggregating over replicates per variable\n" ) )
												flush.console()
										}								
								}
								
								# l2 ist jetzt auf unterster Ebene
								# d.h. enthaelt Gruppenelemente (z.B. Items) in den Datensaetzen der Replikationen
								# jetzt Formeln aus Babcock/Albano 2012 anwenden
								
								f3 <- function ( d , val.col , id.col , group.col , repl.col , method ) {

										if ( verbose ) {
												if ( method == "group" ) {
														cat ( paste0 ( "replicate " , d[1,repl.col] , " group " , d[1,group.col] , " Nvar=" , nrow ( d ) , "\n" ) )
												} else {
														cat ( paste0 ( d[1,id.col] , " Nrepl=" , nrow ( d ) , "\n" ) )
												}
												flush.console()
										}
										
										# Checks abhängig von method
										if ( method == "group" ) {
												# id.col muss hier unique sein
												# wenn nicht Warnung
												if ( any ( duplicated ( d[,id.col] ) ) ) {
														cat ( paste0 ( "warning: values on id.col (" , id.col , ") are not unique in innerst loop. check your data and function parameter settings!\n" ) )
														cat ( paste0 ( "group.col (" , group.col , ") table: " , "\n" ) )
														cat ( paste0 ( names ( table ( d[,group.col] ) ) , "\n" ) )
														cat ( paste0 ( table ( d[,group.col] ) , "\n" ) )
												}
										} else {
												# method == "repl"
												# replicates muss hier unique sein
												# wenn nicht Warnung
												if ( any ( duplicated ( d[,repl.col] ) ) ) {
														cat ( paste0 ( "warning: values on id.col (" , id.col , ") are not unique in innerst loop. check your data and function parameter settings!\n" ) )
														cat ( paste0 ( "group.col (" , group.col , ") table: " , "\n" ) )
														cat ( paste0 ( names ( table ( d[,group.col] ) ) , "\n" ) )
														cat ( paste0 ( table ( d[,group.col] ) , "\n" ) )
												}
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
												if ( (length( d[,1] ) > 1) & !any ( is.na( d[,1] )) & !any(is.na( d[,2] )) & !sd(d[,1])==0 & !sd(d[,2])==0 ) {
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
								res3.l <- mapply ( f3 , l2 , MoreArgs = list ( val.col , id.col , group.col , repl.col , method ) , SIMPLIFY = FALSE )
								# if ( verbose ) { 
										# cat ( paste0 ( "\n" ) )
								# }
								res3 <- do.call ( "rbind" , res3.l )

								if ( verbose ) {
										cat ( paste0 ( "Meaning over " , nrow ( res3 ) , " rows" , "\n\n" ) )
								}
								
								# Mitteln
								res4 <- res3[1,,drop=FALSE]
								res4$bias <- mean ( res3$bias )
								res4$biasfree.sd <- mean ( res3$biasfree.sd )
								res4$MSE <- mean ( res3$MSE )
								res4$RMSE <- mean ( res3$RMSE )
								res4$cor <- mean ( res3$cor )
								# replicates Spalte rausschmeissen
								res4 <- res4[,!colnames(res4) %in% repl.col]
							
								return ( res4 )
						}
						res1.l <- mapply ( f2 , e4 , names ( e4 ) , seq ( along = e4 ) , MoreArgs = list ( repl.col , val.col , group.col , length ( e4 ) , method ) , SIMPLIFY = FALSE )				
						res1 <- do.call ( "rbind" , res1.l )

				}
				res.l <- mapply ( f1 , true.l , MoreArgs = list ( est , spl.vars , id.col , val.col , group.col , repl.col , true.split , method ) , SIMPLIFY = FALSE )
				res <- do.call ( "rbind" , res.l )
				rownames ( res ) <- seq ( along = rownames ( res ) )
				
				# wenn keine group.col gesetzt war, dann die kuenstlich erzeugt group.col nach id.col benennen
				if ( rename.later ) {
						colnames ( res ) [ colnames ( res ) == group.col ] <- id.col
				}
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
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , verbose = TRUE )	


### Beispiel 2, zusaetzlich 2 Replicates
# true <- data.frame ( "variable" = c ("item1","item2") ,
					 # "value" = c ( 1 , 2 ) ,
					 # "group" = rep ( "items" , 2 ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = c ("item1","item1","item2","item2") ,
					 # "value" = rnorm ( 4 , 1 , 1 ) ,
					 # "replicates" = c ( 1 , 2 , 1 , 2 ) , 					 
					 # stringsAsFactors = FALSE )

# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , method = "group" , verbose = TRUE)		 
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , method = "repl" , verbose = TRUE)		 
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE)		 

					 
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
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )		 
	
	
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
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )		 


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
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , repl.col = "replicates" , verbose = TRUE )


### Beispiel 6, mehrere groups
# true <- data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 # "value" = 1:5 ,
					 # "group" = c ( rep ( "items" , 2 ) , rep ( "persons" , 2 ) , NA ) , 
					 # stringsAsFactors = FALSE )
# est <-  data.frame ( "variable" = c ("item1","item2","person1","person2","randeff1") ,
					 # "value" = rnorm ( 5 , 1 , 1 ) ,
					 # stringsAsFactors = FALSE )
# bias.rmse ( true , est , id.col = "variable" , val.col = "value" , group.col = "group" , verbose = TRUE )	



	

