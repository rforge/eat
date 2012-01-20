# 2011-12-16 MH
# OPTIMIZED: sorting in long2matrix
# 2011-12-08 MH
# NEW: long2matrix
# 0000-00-00 AA

.sort.by.col <- function ( d ) {
		# unique col Elemente
		ucol <- unique ( d$col )
		
		# rows sortieren nach cols
		s.row <- unname ( unlist ( mapply ( function ( el , d ) {
				which ( d %in% el )
		} , ucol , MoreArgs = list ( d$row ) , SIMPLIFY = FALSE ) ) )
		# die die rausgeflogen sind, hinten dran hängen, sonst unangenehme Effekte
		s.row <- c ( s.row , seq ( along = rownames ( d ) )[! seq ( along = rownames ( d ) ) %in% s.row ] )
		d <- d [ s.row , ]					

		# cols sortieren
		s.col <- unname ( unlist ( mapply ( function ( el , d ) {
				which ( d %in% el )
		} , ucol , MoreArgs = list ( d$col ) , SIMPLIFY = FALSE ) ) )						
		d <- d [ s.col , ]	
		
		return ( d )
}

long2matrix <- function ( data , sort = TRUE , triangle = NULL ,
						  force.diagonal = FALSE , exclude.diagonal = FALSE ,
						  long2matrix = TRUE ) {

		# Definitionen
		d <- data
		
		# Checks
		stopifnot ( ncol ( d ) == 3 )
		orig.col.names <- colnames ( d )
		colnames ( d ) <- c ( "row" , "col" , "val" )
		stopifnot ( triangle %in% c ( NULL , "both" , "upper" , "lower" ) )
		stopifnot ( is.character ( d$row ) )
		stopifnot ( is.character ( d$col ) )

		# gesetztes triangle impliziert dass Matrix symmetrisch und dass sortiert werden soll
		if ( !is.null ( triangle ) ) {
				symmetric <- TRUE 
		} else {
				symmetric <- FALSE
		}
		
		# duplicated Variable erzeugen
		.fun1 <- function ( row , col ) {
				paste ( sort ( c ( row , col ) ) , collapse = "-" )
		}
		d$var <- mapply ( .fun1 , d$row , d$col , USE.NAMES = FALSE )

		# Check ob Diagonalelemente nur einmal
		if ( any ( b <- d$row == d$col ) ) {
				.fun2 <- function ( var , d ) {
						if ( ( l <- length ( w <- ( which ( d$var %in% var ) ) ) ) > 1 ) {
								warning ( paste ( "Diagonal element" , var , "occurs" , l , "times, that's more than once. Only first occurence is kept.\n" ) )
								delete <- w[ c( 2:length(w) ) ]
						} else delete <- NULL
						return ( delete )
				}
				delete <- unname ( unlist ( mapply ( .fun2 , unique ( d$var[b] ) , MoreArgs = list ( d ) ) ) )
				if ( !is.null ( delete ) ) d <- d [ -delete , ]		
		}

		# wenn eine Kombination mehr als 2 mal, Warnung und nur die ersten beiden behalten		
		.fun3 <- function ( var , d ) {
				if ( ( l <- length ( w <- ( which ( d$var %in% var ) ) ) ) > 2 ) {
						warning ( paste ( "Combination" , var , "occurs" , l , "times, that's more than twice. Only first two occurences are kept.\n" ) )
						delete <- w[ c( 3:length(w) ) ]
				} else delete <- NULL
				return ( delete )
		}
		delete <- unname ( unlist ( mapply ( .fun3 , unique ( d$var ) , MoreArgs = list ( d ) ) ) )
		if ( !is.null ( delete ) ) d <- d [ -delete , ]

		# Check ob wirklich symmetrisch
		# NAs werden ignoriert, also nur wenn zwei gleiche Zellen unterschiedliche Werte haben, ist nicht symmetrisch
		if ( symmetric ) {
				.fun4 <- function ( var , d ) {
						if ( ( l <- length ( w <- ( which ( d$var %in% var ) ) ) ) > 1 ) {
								if ( ! all ( ( val <- d [ d$var==var, "val"] ) == val[1] ) ) {
										warning ( paste ( "Option 'triangle' implies a symmetric matrix, so values of combination" , var , "should be identical, but they are not (" , paste ( val , collapse = " != " ) , ").\n" ) )
										falsch <- TRUE
								} else falsch <- FALSE
						} else falsch <- FALSE
						return ( falsch )
				}
				falsch <- unname ( unlist ( mapply ( .fun4 , unique ( d$var[!d$row==d$col] ) , MoreArgs = list ( d ) ) ) )
				if ( any ( falsch ) ) {
						warning ( paste ( "Option 'triangle' is set to NULL.\n" ) )
						symmetric <- FALSE
				}
		}

		# sortieren
		# if ( sort ) d <- d [ order ( d$row , d$col ) , ]

		# symmetrisch machen, d.h. jede Kombination nur 1mal
		# Diagonale auffüllen
		# Diagonale mit NA forcen wenn keine Diagonalelemente gesetzt

		if ( symmetric ) {

				# wenn triangle spezifiziert
				if ( !is.null( triangle ) ) { 
						
						# sortieren
						# d <- d [ order ( d$row , d$col ) , ]
					
						# Duplikate entfernen
						# d <- d [ !duplicated ( d$var ) , ]
						
						# upper triangle herstellen
						# widrige Elemente swappen
						d$row.ind <- match ( d$row , sort ( unique ( d$row ) ) )
						d$col.ind <- match ( d$col , sort ( unique ( d$col ) ) )
						widrig <- !d$row.ind <= d$col.ind
						if ( any ( widrig ) ) {
								temp <- d$row[widrig]
								d$row[widrig] <- d$col[widrig]
								d$col[widrig] <- temp
						}
						
						# wenn lower triangle, dann tauschen
						if ( triangle == "lower" ) {
								temp <- d$row
								d$row <- d$col
								d$col <- temp
						}
						
						# wenn full matrix, dann dranhängen getauschter elemente
						if ( triangle == "both" ) {
								d <- data.frame (
										"row" = c( d$row , d$col[!d$row==d$col] ) ,
										"col" = c ( d$col , d$row[!d$row==d$col] ) ,
										"val" = c ( d$val , d$val[!d$row==d$col] ) ,
										"var" = c ( d$var , d$var[!d$row==d$col] )
										, stringsAsFactors = FALSE )
						}
						
				}

				# Handling von Diagonale Inkonsistenzen
				if ( force.diagonal & exclude.diagonal ) {
						# wenn die Diagonale vom Nutzer komplett gesetzt ist, und exclude.diagonale angefordert wird
						# dann bekommt er exclude.diagonal
						# wenn Diagonale nicht gesetzt, dann Warnung und exclude.diagonal auf FALSE
						if ( length ( which ( d$row == d$col ) ) == length ( unique ( c ( d$row , d$col ) ) ) ) {
								force.diagonal <- FALSE
						} else {
								warning ( "force.diagonal and exclude.diagonal must not be both TRUE. exclude.diagonal is set to FALSE." )
								exclude.diagonal <- FALSE								
						}

				}
				
				# Diagonale auffüllen, durch dranhängen von Zeilen, die fehlen
				if ( !exclude.diagonal ) {
						l <- length ( w <- ( which ( d$row == d$col ) ) )
						refl <- length ( ( v <- unique ( c ( d$row , d$col ) ) ) )
						if ( ( l > 0 & l < refl ) | ( l == 0 & force.diagonal ) ) {
								.fun5 <- function ( el , d ) {
										d <- d[ d$row == d$col , ]
										if ( identical ( d , character(0) ) | ! ( el %in% d$row ) ) {
												ret <- el
										} else ret <- NULL
										return ( ret )
								}
								add <- unname ( unlist ( mapply ( .fun5 , v , MoreArgs = list ( d ) ) ) )
								
								d <- data.frame ( "row" = c ( d$row , add ) ,
												  "col" = c ( d$col , add ) ,	
												  "val" = c ( d$val , rep ( NA , length( add ) ) ) ,
												  "var" = c ( d$var , paste ( add , "-" , add , sep = "" ) ) , stringsAsFactors = FALSE )
						}
				} else {
						d <- d[ !d$row == d$col , ]
				}

				# sortieren der rows nach cols, für symmetrische Matrizen ansonsten relativ sinnlos
				d <- .sort.by.col ( d ) 
		}
	
		if ( !symmetric ) {
				# wenn alle Diagonal-Elemente gesetzt, rows so sortieren wie cols, sonst siehts u.U. nicht schön aus
				# aber: wenn nicht alle Diagonal-Elemente gesetzt, hat User halt Pech gehabt / wollte das ja so

				# alle Diagonal-Elemente vorhanden?
				all.dia <- all ( d [ d$row == d$col , "col" ] %in% unique ( d$row ) ) & all ( d [ d$row == d$col , "row" ] %in% unique ( d$col ) )
				# dann sortieren der rows damit diese cols entsprechen
				if ( all.dia ) d <- .sort.by.col ( d )
		}
		
		
		if ( long2matrix ) {
				if ( sort ) {
						col.vec <- sort ( unique ( d$col ) )
						row.vec <- sort ( unique ( d$row ) )
				} else {
						col.vec <- unique ( d$col )
						row.vec <- unique ( d$row )
				}
				
				# Matrix erzeugen
				.fun6 <- function ( var , d , rowvec ) {
						temp <- d[ d$col==var , "val" ]
						names ( temp ) <- d[ d$col==var , "row" ]
						unname ( temp[ rowvec ] )
				}
				m <- data.frame ( mapply ( .fun6 , col.vec , MoreArgs = list ( d , row.vec ) , SIMPLIFY=FALSE ) , stringsAsFactors = FALSE )
				rownames ( m ) <- row.vec
		} else {
				if ( sort ) d <- d [ order ( d$col , d$row ) , ]
				
				# var löschen
				m <- d [ , ! colnames(d) %in% "var" ]
				# original colnames
				colnames ( m ) <- orig.col.names
		}
		
		return ( m )
}

# d1 <- data.frame (
# "row" = c ( "v1" , "v2" , "v2" , "v3" , "v1" , "v3" ) , 
# "col" = c ( "v1" , "v3" , "v2" , "v1" , "v2" , "v3" ) , 
# "val" = c ( 1 , 5 , 4 , 3 , 2 , 6 ) , stringsAsFactors = FALSE )

# long2matrix  ( data = d1 , sort = FALSE )
# long2matrix  ( data = d1 )
# long2matrix  ( data = d1 , triangle = "upper" )
# long2matrix  ( data = d1 , triangle = "upper" , exclude.diagonal = TRUE )
# long2matrix  ( data = d1 , triangle = "both" , exclude.diagonal = TRUE )

# d2 <- data.frame (
# "row" = c ( "v2" , "v1" , "v1" ) , 
# "col" = c ( "v3" , "v3" , "v2" ) , 
# "val" = c ( 5 , 3 , 2 ) , stringsAsFactors = FALSE )

# long2matrix ( data = d2 )
# long2matrix ( data = d2 , triangle = "upper" , force.diagonal = TRUE )
		   


			   
