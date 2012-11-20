		
long2symmatrix <- function ( dat , sort = FALSE , triangle = c ("both","lower","upper") , include.diagonal = TRUE , full.symmetric = FALSE ) {
		
		# triangle
		triangle <- match.arg ( triangle , c ("both","lower","upper") )
		
		### Checks
		stopifnot ( is.data.frame ( dat ) )
		stopifnot ( ncol( dat ) >= 3 )
		
		# Colnames "erraten" oder lassen
		coln <- c ( "row" , "col" , "val" )
		coln.exists <- coln %in% colnames ( dat )
		if ( ! all ( coln.exists ) ) {
				if ( ncol ( dat == 3 ) ) {
						warning ( paste ( "'dat' does not inlude column(s) " , paste ( coln[coln.exists] , collapse = ", " ) ,
								  ".\n The following columns in 'dat' are treated as ...\n" ,
								  paste ( paste ( colnames ( which ( coln.exists ) ) , "=" , coln[coln.exists] ) , collapse = "\n" )
								  , sep = "" ) )
						colnames ( dat ) <- coln
				} else {
						stop ( paste ( "'dat' does not inlude column(s) " , paste ( coln[coln.exists] , collapse = ", " ) ,
								  ". Since number of columns in 'dat' is greater than 3 it is not possible to allocate columns." ,
									sep = "" ) )
				}
		}
		
		# Datensatz bereinigen (row/col/val behalten)
		if ( ncol ( dat ) > 3 ) {
				# dat.to.merge <- dat[,!colnames(dat) %in% coln]
				dat <- dat [,coln]
		}
		
		# triangle = "both" , include.diagonal = FALSE
		if ( triangle == "both" & !include.diagonal ) {
				warning ( paste ( "Parameter triangle = 'both' and include.diagonal = 'FALSE' are not combinable, include.diagonal is set to TRUE" ) )
				include.diagonal <- TRUE
		}

		### sortieren der items zur Erzeugung der Matrix
		# bei "lower" nach row
		# bei "upper" nach col
		# "both" impliziert sort = TRUE
		if (triangle=="lower") {
				items <- unique( c ( dat$row , dat$col ) )
		} else if (triangle=="upper") {
				items <- unique( c ( dat$col , dat$row ) )
		} else {
				items <- unique( c ( dat$row , dat$col ) )
				if ( !isTRUE(sort) ) {
						warning ( "triangle = 'both' implies sort = TRUE. matrix will be sorted." )
						sort = TRUE
				}
		}
		
		# sortieren 
		if ( is.logical ( sort ) & isTRUE(sort) ) items <- items[order(items)]
		if ( is.character ( sort ) & length ( sort ) > 0 ) {
				not <- setdiff ( items , sort )
				items.to.sort <- items[!items %in% not]
				new.order <- sort[sort %in% items.to.sort]
				if ( length ( items.to.sort ) > 0 & length ( items.to.sort ) == length ( new.order ) ) {
						items <- c ( new.order , not )
				}
		}
		
		### Checks auf long-Datensatz ###
		# duplicated Variable erzeugen
		.fun1 <- function ( row , col ) {
				paste ( sort ( c ( row , col ) ) , collapse = "-" )
		}
		dat$var <- mapply ( .fun1 , dat$row , dat$col , USE.NAMES = FALSE )

		# Check ob Diagonalelemente nur einmal
		if ( any ( b <- dat$row == dat$col ) ) {
				.fun2 <- function ( var , dat ) {
						if ( ( l <- length ( w <- ( which ( dat$var %in% var ) ) ) ) > 1 ) {
								warning ( paste ( "Diagonal element" , var , "occurs" , l , "times, that's more than once. Only first occurence is kept.\n" ) )
								delete <- w[ c( 2:length(w) ) ]
						} else delete <- NULL
						return ( delete )
				}
				delete <- unname ( unlist ( mapply ( .fun2 , unique ( dat$var[b] ) , MoreArgs = list ( dat ) ) ) )
				if ( !is.null ( delete ) ) dat <- dat [ -delete , ]		
		}

		# wenn eine Kombination mehr als 2 mal, Warnung und nur die ersten beiden behalten		
		.fun3 <- function ( var , dat ) {
				if ( ( l <- length ( w <- ( which ( dat$var %in% var ) ) ) ) > 2 ) {
						warning ( paste ( "Combination" , var , "occurs" , l , "times, that's more than twice. Only first two occurences are kept.\n" ) )
						delete <- w[ c( 3:length(w) ) ]
				} else delete <- NULL
				return ( delete )
		}
		delete <- unname ( unlist ( mapply ( .fun3 , unique ( dat$var ) , MoreArgs = list ( dat ) ) ) )
		if ( !is.null ( delete ) ) dat <- dat [ -delete , ]

		
		### full.symmetric
		# wenn full.symmetric wird gecheckt ob die nicht-Diagonal-Elemente gleich sind bzw. bei valid-NA auf valid gesetzt
		# Check ob wirklich symmetrisch
		# NAs werden ignoriert, also nur wenn zwei gleiche Zellen unterschiedliche Werte haben, ist nicht symmetrisch
		if ( full.symmetric ) {
				.fun4 <- function ( var , dat ) {
						if ( ( l <- length ( w <- ( which ( dat$var %in% var ) ) ) ) > 1 ) {
								if ( ! all ( ( val <- dat [ dat$var==var, "val"] ) == val[1] ) ) {
										warning ( paste ( "Option 'full.symmetric' implies that values of combination" , var , "should be identical, but they are not (" , paste ( val , collapse = " != " ) , ").\n" ) )
										falsch <- TRUE
								} else falsch <- FALSE
						} else falsch <- FALSE
						return ( falsch )
				}
				falsch <- unname ( unlist ( mapply ( .fun4 , unique ( dat$var[!dat$row==dat$col] ) , MoreArgs = list ( dat ) ) ) )
				if ( any ( falsch ) ) {
						warning ( paste ( "Matrix is not symmetric.\n" ) )
				} else {
						# wenn symmetrisch, fehlende NA Elemente setzen, damit volle Matrix
						diag <- dat[ dat$row==dat$col , ]
						dupl <- dat[ !dat$row==dat$col , ]
						dupl <- dupl[!duplicated(dupl$var),]
						dupl.flipped <- dupl
						dupl.flipped.temp <- dupl.flipped$row
						dupl.flipped$row <- dupl.flipped$col
						dupl.flipped$col <- dupl.flipped.temp
						dat <- rbind ( diag , dupl , dupl.flipped )
						dat <- dat [ order ( dat$var , dat$row , dat$col ) , ]
						rownames(dat) <- seq ( along = rownames ( dat ) )
				}
		}
	
		# Diagonale inkluden oder nicht
		if ( length ( items ) > 1 ) {
				if ( include.diagonal ) items.row <- items.col <- items else {
						if (triangle=="lower") {
								items.row <- items[-1]
								items.col <- items[-length(items)]
						} else if (triangle=="upper") {
								items.col <- items[-1]
								items.row <- items[-length(items)]
						} else items.row <- items.col <- items
				}
		} else stop()
		
		# Matrix erzeugen
		fun <- function ( items.col , items.seq , items.row , dat , triangle ) {
				rel <- which ( dat$row %in% items.row & dat$col == items.col )
				vals <- dat$val[rel]
				names(vals) <- dat$row[rel]
				
				# für lower/upper Matrix noch entsprechende Elemente raus
				# werden dann als NA
				if (triangle=="lower") {
						keep <- match(names(vals),items.row) >= items.seq
				} else if (triangle=="upper") {
						keep <- match(names(vals),items.row) <= items.seq
				} else keep <- rep ( TRUE , length(vals) )
				
				vals <- vals [ keep ]
								
				unname ( vals[items.row] )
		}
		matr <- data.frame ( mapply ( fun, items.col, seq(along=items.col), MoreArgs = list ( items.row , dat , triangle ) , SIMPLIFY = FALSE ) , stringsAsFactors = FALSE )
		rownames(matr) <- items.row
		
		return (matr)
}		
