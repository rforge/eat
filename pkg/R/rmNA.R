
rmNA <- function( data , remove = TRUE , verbose = FALSE ) {
		d <- rmNAcols ( data = data , remove = remove , verbose = verbose )
		if ( remove ) {
				ret <- rmNArows ( data = d , remove = remove , verbose = verbose )
		} else {
				d2 <- rmNArows ( data = data , remove = remove , verbose = verbose )
				ret <- list ( "rows" = d , "cols" = d2 )
		}
		return ( ret )
}
### example matrix
# ( mat <- matrix( c( 1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) , ncol=6 , byrow=TRUE ) )
# ( rmNA ( mat , verbose = TRUE ) )
# ( rmNA ( mat , remove = FALSE ) )


rmNArows <- function( data , cols = NULL , tolerance = 0 , cumulate = TRUE , remove = TRUE , verbose = FALSE ) {
		
		return ( removeNA ( data , cols , tolerance , cumulate , remove , verbose , mode="rows") )
		
}

####################################################################################################################
# E x a m p l e s
####################################################################################################################

### example matrix
#( mat <- matrix( c( 1,1,1,1,1, 1,1,1,1,NA, 1,1,1,NA,NA, 1,1,NA,NA,NA, 1,NA,NA,NA,NA, NA,NA,NA,NA,NA ) , ncol=5 , byrow=TRUE ) )

### remove row with entirely NA (row 6)
#rmNArows( mat , verbose = TRUE )

### remove row with NA on column 3, 4, 5 (rows 4, 5, 6)
#rmNArows( mat , c(3,4,5) , verbose = TRUE ) 
#rmNArows( mat , c(-1,-2) , verbose = TRUE ) 

### tolerance=1 , 1 non-NA is permitted (rows 5 and 6)
#rmNArows( mat , tolerance=1 , verbose = TRUE ) 

### tolerance=5 , 5 non-NA are permitted (all rows are removed)
#rmNArows( mat , tolerance=5 , verbose = TRUE ) 

### do not cumulate / exact tolerance (row 1 is removed)
#rmNArows( mat , tolerance=5 , cumulate=FALSE , verbose = TRUE ) 
#rmNArows( mat , tolerance=5 , cumulate=FALSE , remove = FALSE ) 

### two subsets of columns
#rmNArows( mat , cols = list( c(1, 2), c(4, 5) ) , verbose = TRUE )

### two subsets of columns with different tolerance
#rmNArows( mat , cols = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , verbose = TRUE )

### identify rows, no deletion
#rmNArows( mat , cols = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , remove = FALSE )


####################################################################################################################
# T e s t s
####################################################################################################################

### test matrix
#( mat <- matrix( c( 1,1,1,1,1, 1,1,1,1,NA, 1,1,1,NA,NA, 1,1,NA,NA,NA, 1,NA,NA,NA,NA, NA,NA,NA,NA,NA ) , ncol=5 , byrow=TRUE ) )
#( mat.char1 <- matrix(as.character(mat), ncol=5 ) )
#( dafr <- as.data.frame(mat) )

### no data argument
#rmNArows()

### defaults
#rmNArows(mat)
#rmNArows(mat.char1)
#rmNArows(dafr)

### not supported method
#rmNArows( mat , method = "bla" )

### colnames
#rmNArows( dafr , cols=c(1,2,3,4,5) )
#rmNArows( dafr , cols=c("V1","V2","V3","V4","V5") )
#rmNArows( dafr , cols=list( c("V1","V2") , c(3,4,5)  ) )

### wrong cols argument
#rmNArows( mat , cols=c("jklfs","kjkljl") )
#rmNArows( mat , cols=list("jklfs","kjkljl") )

### wrong tolerance argument
#rmNArows( mat , tolerance="1" )
#rmNArows( mat , tolerance="dasfadsf" )

### return vector / list
#rmNArows( mat , tolerance=5 , cumulate=TRUE , remove = FALSE ) 
#rmNArows( mat , tolerance=5 , cumulate=FALSE , remove = FALSE ) 
#rmNArows( mat , cols = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , cumulate = FALSE , remove = FALSE )
#rmNArows( mat , cols = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , cumulate = TRUE , remove = FALSE )
#rmNArows( mat , tolerance=1 , cumulate = FALSE , method = "identify") 


rmNAcols <- function( data , rows = NULL , tolerance = 0 , cumulate = TRUE , remove = TRUE , verbose = FALSE ) {
		
		# data transponieren um removeNA mit mode="cols" zu benutzen
		data.t <- t(data)

		# wieder zum Dataframe machen, da t() nur Matrix zurückgibt
		# colnames setzen
		if ( class(data)=="data.frame" ) {
				data.t <- as.data.frame(data.t)
				colnames(data.t) <- rownames(data)
		}
		
		# removeNA mit transponierten Datensatz und mode="cols" aufrufen
		# Hinweis: removeNA ist standardmäßig auf rows löschen programmiert,
		# deshalb der ganze Spaß mit dem transponieren
		data.return <- removeNA ( data.t , rows , tolerance , cumulate , remove , verbose , mode="cols")

		# nur bei method="remove" rücktransponieren, da method="identify" ne Liste liefert,
		# die nicht transponiert werden darf
		if ( remove ) { 
				data.return <- t( data.return )
		
				### wenn Input Dataframe dann
				# Output nach Dataframe wandeln
				# ursprüngliche rownames wieder herstellen
				# Spalten-Klassen setzen
				if ( class(data)=="data.frame" ) { 
						data.return <- data.frame(data.return)
						rownames(data.return) <- rownames(data)
						# for (colnum in seq(along=data.return))  data.return[,colnum] <- as ( data.return[,colnum] , class(data[, colnames(data.return)[colnum] ]) )
						col.type <- sapply ( colnames ( data )[ colnames ( data ) %in% colnames ( data.return ) ] , c , simplify = FALSE )
						names ( col.type ) <- sapply ( data [ colnames ( data ) %in% colnames ( data.return ) ] , class , simplify = FALSE )
						data.return <- set.col.type ( data.return , col.type )
				}	
		}

		return ( data.return )
}



####################################################################################################################
# E x a m p l e s
####################################################################################################################

### example matrix
#( mat <- matrix( c( 1,1,1,1,1,1, 1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA) , ncol=7 ) )

### remove column with entirely NA (column 7)
#rmNAcol( mat , verbose = TRUE )

### remove column with NA on rows 3, 4, 5 (columns 5, 6, 7)
#rmNAcol( mat , c(3,4,5) , verbose = TRUE ) 
#rmNAcol( mat , c(-1,-2,-6) , verbose = TRUE ) 

### tolerance=1 , 1 non-NA is permitted (columns 6 and 7)
#rmNAcol( mat , tolerance=1 , verbose = TRUE ) 

### tolerance=6 , 6 non-NA are permitted (all columns are removed)
#rmNAcol( mat , tolerance=6 , verbose = TRUE ) 

### do not cumulate / exact tolerance (column 1)
#rmNAcol( mat , tolerance=6 , cumulate=FALSE , verbose = TRUE ) 

### two subsets of rows
#rmNAcol( mat , rows = list( c(1, 2), c(4, 5) ) , verbose = TRUE )

### two subsets of rows with different tolerance
#rmNAcol( mat , rows = list( c(1), c(2, 3, 4, 5) ) , tolerance = list( 0 , 1 ) , verbose = TRUE )

### identify cols, no deletion
#rmNAcol( mat , rows = list( c(1, 2), c(3, 4, 5) ) , tolerance = list( 0 , 1 ) , remove = FALSE )



####################################################################################################################
# T e s t s
####################################################################################################################

### test matrix
#( mat <- matrix( c( 1,1,1,1,1,1, 1,1,1,1,1,NA, 1,1,1,1,NA,NA, 1,1,1,NA,NA,NA, 1,1,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA, NA,NA,NA,NA,NA,NA) , ncol=7 ) )
#( mat.char1 <- matrix(as.character(mat), ncol=7 ) )
#( dafr <- as.data.frame(mat) )

### no data argument
#rmNAcol()

### defaults
#rmNAcol(mat)
#rmNAcol(mat.char1)
#rmNAcol(dafr)

### not supported method
#rmNAcol( mat , method = "bla" )

### colnames
#rmNAcol( dafr , rows=c(1,2,3,4,5) )
#rmNAcol( dafr , rows=c("4","5","6") )
#rmNAcol( dafr , rows=list( c("item1","item2") , c(3,4,5)  ) )

### wrong rows argument
#rmNAcol( mat , rows=c("jklfs","kjkljl") )
#rmNAcol( mat , rows=list("jklfs","kjkljl") )

### wrong tolerance argument
#rmNAcol( mat , tolerance="1" )
#rmNAcol( mat , tolerance="dasfadsf" )

### preserve data.frame col classes
# ( df <- data.frame( num = 1:5 , char = c("a","b","c","d","e") , numNA = rep(NA,5) , charNA = rep(as.character(NA),5) , stringsAsFactors = FALSE ) )
# str(df)
# ( df2 <- rmNAcol ( df ) )
# str(df2)



# "Mutter"-Funktion zu rmNArows und rmNAcol
# mode ("rows" oder "cols") hinzugefügt um entsprechende verbosemeldungen zu bekommen
removeNA <- function( data , cols , tolerance , cumulate , remove , verbose , mode="rows" ) {
		
		# mode check
		if ( ! (mode %in% c("rows", "cols") ) ) stop ( paste ( "internal error: mode \"" , mode , "\" is not supported" , sep="" ) )		
		
		# method check
		# if ( ! (method[1] %in% c("remove", "identify") ) ) stop ( paste ( "method \"" , method , "\" is not supported" , sep="" ) )

		# cumulate / verbose check
		if ( ! ( is.logical(remove) ) ) stop ( paste ( "'remove' is not logical" , sep="" ) )				
		if ( ! ( is.logical(cumulate) ) ) stop ( paste ( "'cumulate' is not logical" , sep="" ) )
		if ( ! ( is.logical(verbose) ) ) stop ( paste ( "'verbose' is not logical" , sep="" ) )				
		
		# wenn Matrix, diese in Dataframe wandeln, für später merken ob Matrix zurückgegeben werden soll
		returnMatrix <- FALSE
		if (is.matrix(data)) { data <- as.data.frame(data) ; returnMatrix <- TRUE }

		# wenn nicht Dataframe, dann stoppen
		if ( !is.data.frame(data) ) stop ( "'data' is not a matrix or data.frame" )

		# wenn Dataframe leer, dann stoppen
		if ( identical ( data, data.frame() ) ) stop ( "data.frame 'data' is empty" )

		# wenn nicht mindestens eine Zeile, stoppen
		if ( nrow(data) < 1 ) {	
				mes <- "row"
				if ( mode == "cols" ) mes <- "column"
				stop ( paste("data.frame 'data' has less than 1", mes) )
		}	
		
		# cols validieren
		modeForVal <- "cols"
		if ( mode=="cols" )	modeForVal <- "rows"
		cols <- validateCols ( data , cols , modeForVal )
		
		# wenn tolerance keine List dann eine draus machen
		if (!class(tolerance)=="list") { tolerance <- list(tolerance) }		

		# wenn tolerance nicht numerisch, stoppen
		lapply ( tolerance , function (tolerance) { 
						if (!(class(tolerance)=="integer"||class(tolerance)=="numeric")) stop (paste("tolerance is not numeric, but",class(tolerance)))
				} ) 
		
		# tolerance an cols anpassen (recyclen oder abschneiden)
		if (length(cols)>length(tolerance)) { tolerance <- rep(tolerance, (length(cols) %/% length(tolerance)) + 1) }		
		if (length(cols)<length(tolerance)) { tolerance <- tolerance[seq(along=cols)] }

		# zu löschende rows identifizieren
		# wenn nur eine Spalte, dann nicht mit rowSums (erzeugt Fehler), sonst mit rowSums
		rows.rm.list <- NULL
		rows.rm.list <-	mapply(
						function(cols,tolerance) {
							if ( ncol(data[,cols, drop=FALSE]) == 1 ) {
									rows.rm.list <- which ( is.na(data[,cols,drop=FALSE]) )
							} else { 
										if (cumulate) rows.rm.list <- unname ( which ( rowSums( !is.na( data[ , cols ] ) ) <= tolerance ) )
										else rows.rm.list <- unname ( which ( rowSums( !is.na( data[ , cols ] ) ) == tolerance ) )
									}
						}
						, cols, tolerance , SIMPLIFY=FALSE)

		# zu löschende Zeilen
		rows.rm <- NULL
		rows.rm <- unique ( unlist ( rows.rm.list ) ) 						
						
		if ( remove ) {
		
				# Zeilen löschen
				if (! ( identical( rows.rm , integer(0) ) || is.null(rows.rm) ) ) {
						data <- data[-rows.rm , , drop=FALSE]
						if (verbose) { 
								l <- length ( rows.rm )
								if ( l > 1 ) waswere <- "were" else waswere <- "was"
								if ( l > 1 ) mes <- "rows" else mes <- "row"
								if ( mode == "cols" ) { if ( l > 1 ) mes <- "columns" else mes <- "column" }
								cat( paste ( mes , paste(rows.rm, collapse=", ") , waswere , "dropped\n") )
						}
				}
				
				# falls Matrix als Input, in Matrix zurückwandeln
				if (returnMatrix) { data <- unname( as.matrix(data) ) }
				
				return ( data )
				
		} else {
						
						# Liste oder Vector mit identifizierten Rows zurückgeben
						if (length(rows.rm.list) == 1) return ( sort ( rows.rm ) )
						else return ( rows.rm.list ) 

				} 
}

# Spalten-Spezifikation und Spalten-Subsets checken
# wandelt character colnames in spaltennummern
# bringt verboseungen wenn spalten nicht in datensatz und droppt diese
# returned ne Liste mit validierten Spalten-Subsets
# mode gibt an ob in verbosemeldungen von columns oder rows die Rede ist
validateCols <- function ( data , cols , mode="cols" ) {

		# mode check
		if ( ! (mode %in% c("rows", "cols") ) ) stop ( paste ( "internal error: mode \"" , mode , "\" is not supported" , sep="" ) )		

		# wenn cols nicht angegeben, dann alle Columns
		if (is.null(cols)) cols <- seq( along=colnames(data) )		

		# wenn cols keine Liste dann eine draus machen
		if (!class(cols)=="list") { cols <- list(cols) }

		# character zu numeric
		# numerische spalten nummern; Spalten, die nicht existieren, raus
		cols.num <- lapply ( cols, function (cols, colnames) {
					if ( class(cols)=="character" ) which( colnames %in% cols )
					else if ( class(cols)=="numeric" || class(cols)=="integer" ) cols [ which ( abs(cols) %in% seq(along=colnames) ) ]
					else return(NULL)
				}
				, colnames(data) )	

				
		lapply ( cols, function (cols, colnames) {
					if ( class(cols)=="character" ) which( colnames %in% cols )
					else if ( class(cols)=="numeric" || class(cols)=="integer" ) cols [ which ( seq(along=colnames) %in% abs(cols) ) ]
					else return(NULL)
				}
				, colnames(data) )	
		
		# verboseungen		
		mes <- "column"
		if (mode=="rows") mes <- "row"
		mapply ( function( cols, cols.num ) {
					if ( ( dif <-  length ( cols ) - length ( cols.num ) ) == 0 ) return (cols.num)
					else if ( identical ( cols.num , integer(0) ) ) {
									warning ( paste ( mes, " subset \"", paste(cols,collapse=", "), "\" is empty or misspecified and has been dropped"  , sep="") )
									return (cols.num)
							} else if ( ! ( dif == 0) ) {
												warning ( paste ( dif , " element(s) in ",mes," subset \"", paste(cols,collapse=", "), "\" is/are misspecified and dropped" , sep="") )
												return (cols.num)
										} else return(NULL)			
				}
				, cols , cols.num )

		# Subsets, die leer sind, raus
		cols.num <- cols.num[which( sapply ( cols.num , function (cols.num) { ! (is.null(cols.num)||identical(cols.num,integer(0))||identical(cols.num,numeric(0))) } ) ) ]
		
		return(cols.num)
}
### T e s t
#mat <- matrix( c( 1,1,1,1,1, 1,1,1,1,NA, 1,1,1,NA,NA, 1,1,NA,NA,NA, 1,NA,NA,NA,NA, NA,NA,NA,NA,NA ) , ncol=5 , byrow=TRUE )
#dafr <- as.data.frame(mat)
#( cols <- list("V1", c("V2","V3") , c("V2","V3","adsff") , c(1,2,3,4) , c("sdfaf") , c(6,7) , c(-1) , NULL) )
#( cols <- c(1,2,3) )
#( validateCols ( dafr , cols )	)


