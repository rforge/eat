
read.txt <- function ( path , read.function = c ( "readLines" , "read.table" , "read.csv" , "read.csv2" , "read.delim" , "read.delim2" ) , file.ext = NULL , ... ) {

		read.function <- match.arg ( read.function , c ( "readLines" , "read.table" , "read.csv" , "read.csv2" , "read.delim" , "read.delim2" ) )

		if ( length ( path ) > 1 ) {
				if ( !is.list ( path ) ) path <- as.list ( path )
				cons <- sapply ( path , .read.txt , file.ext , ... , simplify = FALSE )	
				if ( is.list ( cons2 <- unlist ( cons , recursive = FALSE ) ) ) cons <- cons2
		} else {
				cons <- .read.txt ( path , file.ext , ... )
		}
		
		# Schleife über alle cons
		.fun <- function ( cons , read.function , ... ) {
				ret <- do.call ( read.function , list ( cons , ... ) )
				close ( cons )
				return ( ret )
		}
		ret <- mapply ( .fun , cons , MoreArgs = list ( read.function , ... ) , SIMPLIFY = FALSE )

		if ( length ( ret ) == 0 ) ret <- NULL
		
		return ( ret )		
}

.read.txt <- function ( path , file.ext , ... ) {

	# wenn schon connection dann einfach listen
	if ( inherits ( path , "connection" ) ) {
			path.name <- summary(path)$description
			ret <- list ( path )
			names ( ret ) <- path.name
	} else {

			# Checks
			c1 <- is.character ( path )
			c2 <- ifelse ( c1 , file.exists ( path ) , FALSE )
			c3 <- ifelse ( c1 , (file.access ( path , mode = 2 ) == 0) , FALSE )
			
			if ( !all ( c ( c1 , c2 , c3 ) ) ) {
					warning ( paste ( "read.txt: cannot read path='" , path , "'. NULL is returned." , sep = "" ) )
					ret <- NULL
			} else {
					
					# wenn directory, dann alle Files ziehen
					isdir <- file.info(path)$isdir
					if ( isdir ) {
							path <- list.files( path = path , 
												full.names = TRUE ,
												include.dirs = FALSE ,
												... )												
					}			

					# compressed file types, MUSS KORRESPONDIEREN mit zip2con
					compr.ext <- c ( "bz2" )
					
					# Pattern um Files mit spezifischer Endung zu finden
					# dabei kompressierte File mitbeachten (diese müssen vor der Kompressions-Endung die file.extension haben
					file.ext.pat <- NULL
					if ( is.character ( file.ext ) ) {
							file.ext <- crop ( file.ext , "." )
							if (nchar (file.ext) > 0 & all(!file.ext %in% compr.ext) ) {
									file.ext.pat <- paste ( 
															paste ( paste ( "\\.",file.ext,"$" , sep ="" ), collapse = "|" ) , 
															paste ( unname ( unlist ( sapply ( file.ext , function ( fe , ce ) paste ( paste ( "\\." , fe , "\\." , ce , "$" , sep = "" ) ) ,
																	compr.ext , simplify = FALSE ) ) ) , collapse = "|" ) ,
														    sep = "|" )
							} 
					}

					# valid files
					if ( !is.null(file.ext.pat) ) {
							valid <- grepl ( file.ext.pat , path ) 
							path <- path[valid]
					}
					if ( identical ( path , character(0) ) ) {
							ret <- NULL
					} else {

							# Pattern zum detektieren aller files mit supporteten compression extensions
							compr.pat <- paste ( paste ( "\\." , compr.ext , "$" , sep = "" ) , collapse = "|" )	
							
							# kompressierte Files suchen
							compr <- grepl ( compr.pat , path )

							# Schleife über alle Files
							.fun <- function ( path , compr ) {
									if ( compr ) {
											ret <- zip2con ( path )
									} else {
											ret <- list ( file ( path ) )
											names(ret) <- summary(ret[[1]])$description
									}
									return ( ret )
							}
							ret <- mapply ( .fun , path , compr , SIMPLIFY = TRUE , USE.NAMES = FALSE )
					}
			}
	}
	
	return ( ret )
}
