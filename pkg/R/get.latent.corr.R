		
get.latent.corr <- function ( path , xlsx = NULL , covariance = TRUE , variance = TRUE , sort = TRUE ) {
		
		if ( !is.list ( path ) & length ( path ) > 1 ) shwf <- as.list ( path )
		
		if ( !is.list ( path ) ) {
				
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
						
						# cov/corr Matrix
						l1 <- grep ( "COVARIANCE/CORRELATION MATRIX" , l )
						if ( identical ( l1 , integer(0) ) ) {
								ret <- NULL
						} else {
								l2 <- grep ( "^-+$" , l )
								l2 <- l2[l2>l1]
								l2 <- l2[2]
								vr <- l[l2 - 1]
								dims <- l [ (l1+6) : (l2-3) ]
								
								ndims <- length(dims)
								if ( ! ndims > 0 ) {
										ret <- NULL
								} else {
										# nur in mehrdimensionalen Modellen gibts Corr
										if ( ndims > 1 ) {
										
												.getcorr <- function ( l , nr , ndims ) {

														# alle Zahlen extrahieren
														spl <- strsplit ( l , " " )[[1]]
														co <- as.numeric ( spl [ grepl ( "-*\\d+\\.\\d+" , spl ) ] )
														
														if ( ! length ( co ) == ( ndims - 1 ) ) {
																ret <- NULL
														} else {
																# noch ein NA dazu je nachdem welche Dimension es ist
																if ( nr == 1 ) {
																		ret <- c ( NA , co )
																} else if ( nr == ndims ) {
																		ret <- c ( co , NA )
																} else {
																		ret <- c ( co[1:(nr-1)] , NA , co[nr:(ndims-1)] )
																}
														}
												return ( ret )
												}
												corrs <- mapply ( .getcorr , dims , seq ( along = dims ) , MoreArgs = list ( ndims ) , SIMPLIFY = FALSE )
												corrs <- data.frame ( do.call ( "rbind" , corrs ) )
												
												# Namen
												na <- rownames ( corrs )
												newna <- sub ( "^\\s+(\\w+|\\d+)\\s+.*$" , "\\1" , na )
												colnames ( corrs ) <- rownames ( corrs ) <- newna
										} else {
												# wenn nur 1 Dimension
												# dann Datensatz mit 0 Spalten, 1 Zeile
												corrs <- data.frame ( NA )
												corrs <- corrs [ , -1 , drop = FALSE ]
												newna <- sub ( "^\\s+(\\w+|\\d+)\\s+.*$" , "\\1" , dims )
												rownames ( corrs ) <- newna
										}
									
										# Varianz in letzte Spalte ran
										# falls es schon "Variance" gibt (doofer User), dann umbenennen
										if ( any ( ( w <- colnames ( corrs ) == "Variance" ) ) ) {
												colnames ( corrs )[which(w)] <- "variance"
												rownames ( corrs )[which(w)] <- "variance"
										}
										spl <- strsplit ( vr , " " )[[1]]
										vrs <- as.numeric ( spl [ grepl ( "-*\\d+\\.\\d+" , spl ) ] )										
										corrs$"Variance" <- vrs
										
										
										# wenn keine Kovarianz gewünscht, obere Matrix (und Leerspalten/-zeilen) löschen
										if ( !covariance & ( ndims > 1) ) {
												num <- seq ( along=colnames(corrs) )
												if ( variance ) num <- num [-length(num)]
												
												for ( sp in num ) {
														for ( z in num ) {
																if ( z < sp ) corrs[z,sp] <- NA
												}}
												corrs <- rmNA ( corrs )
										}

								}
						}
						return ( corrs )
				}
				corrs <- mapply ( .fun1 , shwf , SIMPLIFY = FALSE )
					
				# Analyse-Namen
				na <- names ( corrs )
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
				names ( corrs ) <- newna
				
				
				### jetzt ein Gesamt-Datensatz machen
				
				# bestimmen der Gesamt-Variablen (Dimensionen) Menge
				vars <- unique ( unlist ( unname ( sapply ( corrs , function ( corrs ) { unique ( c ( rownames ( corrs ) , colnames ( corrs ) ) ) } ) ) ) )
				# Variance rausnehmen
				vars <- vars [ !vars == "Variance" ]
				
				if ( sort ) {
						# Sortierung so wie in der Analyse wo am meisten der Variablen dabei
						as <- mapply ( 
								function ( corrs , cln ) { 
										length ( which ( colnames ( corrs ) %in% cln ) )
								}
						, corrs , MoreArgs = list ( vars ) , SIMPLIFY = FALSE )
						asn <- unname ( which ( as == max ( unname ( unlist ( as ) ) ) )[1] )
						vars.sort <- colnames ( corrs [[asn]] )
						vars.sort <- vars.sort [ !vars.sort == "Variance" ]
						vars <- unique ( c ( vars.sort , setdiff ( vars , vars.sort ) , setdiff ( vars.sort , vars ) ) )
				}
				
				# Variance dranhängen
				vars <- c ( vars , "Variance" )
				
				# ein Gesamt-Datensatz erzeugen
				fun <- function ( corrs , vars ) {
						
						# NA Spalten/Zeilen erzeugen
						s <- 1
						z <- 1
						for ( v in vars ) {
								
								if ( !v %in% colnames ( corrs ) ) {
									
										if ( s<length(colnames(corrs)) ) {
												corrs <- cbind ( corrs[,1:s,drop=FALSE], rep ( NA , nrow ( corrs ) ) , corrs[,(s+1):length(colnames(corrs)),drop=FALSE] )
										} else {
												corrs <- cbind ( corrs[,1:s,drop=FALSE], rep ( NA , nrow ( corrs ) ) )
										}
									
										s <- s + 1
										colnames ( corrs )[s] <- v
								}
								if ( ( !v %in% rownames ( corrs ) ) & !v=="Variance" ) {
										if ( z<length(rownames(corrs)) ) {
												corrs <- rbind ( corrs[1:z,], rep ( NA , ncol ( corrs ) ) , corrs[(z+1):length(rownames(corrs)),] )
										} else {
												corrs <- rbind ( corrs[1:z,], rep ( NA , ncol ( corrs ) ) )
										}
										z <- z + 1
										rownames ( corrs )[z] <- v	
								}
						}
						
						# sortieren
						corrs <- corrs [  , vars , drop = FALSE ]
						rsort <- vars[!vars=="Variance"]
						if ( !identical ( rsort , character(0) ) ) corrs <- corrs [ rsort , ]
						
						return ( corrs )
				}
				corrs2 <- mapply ( fun , corrs , MoreArgs = list ( vars ) , SIMPLIFY = FALSE , USE.NAMES = TRUE )
				nam <- names ( corrs2 )
				
				# rbinden und Namen setzen (<analyse>.<dim>) 
				fun2 <- function ( dfr , na ) {
						paste ( crop ( na ) , "." , crop ( rownames ( dfr ) ) , sep = "" )
				}
				newrn <- as.vector ( unname ( unlist ( mapply ( fun2 , corrs2 , names ( corrs2 ) , SIMPLIFY = FALSE ) ) ) )
				# newrn <- paste ( crop ( names ( rn ) ) , crop ( rn ) , sep = "." )
				corrs2 <- do.call ( "rbind" , corrs2 )
				rownames ( corrs2 ) <- newrn
	
				# wenn keine Varianz gewünscht, diese löschen
				if ( !variance ) corrs2 <- corrs2[,!colnames(corrs2)=="Variance",drop=FALSE]
				
				# Analyse und Dimensionen als Spalten setzen
				corrs2$Dimension <- gsub ( paste (  paste ( nam , "\\.", sep = "" ) , collapse = "|" )  , "" , rownames(corrs2) )
				corrs2$Analyse <- gsub ( paste (  paste ( "\\.", corrs2$Dimension , sep = "" ) , collapse = "|" )  , "" , rownames(corrs2) )
				
				corrs2 <- reinsort.col ( corrs2 , c("Dimension","Analyse") , ( h <- colnames ( corrs2 )[1] ) )
				corrs2 <- reinsort.col ( corrs2 , h , "Analyse" )
				corrs2 <- reinsort.col ( corrs2 , "Dimension" , "Analyse" )
				rownames ( corrs2 ) <- seq ( along = rownames ( corrs2 ) )
				
				# wenn nur Analyse und Dimension (z.B. in unidimensionalem Modell mit variance=FALSE) auf NULL setzen
				if ( all ( colnames ( corrs2 ) %in% c("Dimension","Analyse") ) ) corrs2 <- NULL
				
				# Excel
				if ( ! is.null ( xlsx ) & ! is.null ( corrs2 ) ) {
			
						na <- NULL
						if ( variance ) na <- paste ( na , "Var" , sep = "" )
						if ( covariance ) na <- paste ( "Cov" , na , sep = "" )
						if ( length ( colnames ( corrs2 )[!colnames ( corrs2 ) %in% c("Dimension","Analyse","Varianz")] ) > 0 ) na <- paste ( "Corr" , na , sep = "" )
						if ( is.null ( na ) ) na <- "Sheet1"
						
						try ( write.xlsx2 ( corrs2, file = xlsx , sheetName=na, 
								col.names=TRUE, row.names=TRUE, append=FALSE ) )
				}
		
		} else corrs2 <- NULL
		
		# returnen 
		return ( corrs2 )
				
}
