
zip2con <- function ( path , ... ) {
		
		if ( length ( path ) > 1 ) {
				if ( !is.list ( path ) ) path <- as.list ( path )
				ret <- sapply ( path , .zip2con , ... , simplify = FALSE )	
				if ( is.list ( ret2 <- unlist ( ret , recursive = FALSE ) ) ) ret <- ret2
		} else {
				ret <- .zip2con ( path , ... )
		}
		
		return ( ret )
}

.zip2con <- function ( path , ... ) {

	# wenn schon connection dann einfach listen
	if ( inherits ( path , "connection" ) ) {
			path.name <- summary(path)$description
			# Extension wegwerfen
			path.name <- sub ( "\\.[^\\.]+$" , "" , path.name )
			# return
			ret <- list ( path )
			names ( ret ) <- path.name
	} else {

			# Checks
			c1 <- is.character ( path )
			c2 <- ifelse ( c1 , file.exists ( path ) , FALSE )
			c3 <- ifelse ( c1 , (file.access ( path , mode = 2 ) == 0) , FALSE )
			
			if ( !all ( c ( c1 , c2 , c3 ) ) ) {
					warning ( paste ( "zip2con: cannot convert path='" , path , "' to connection. NULL is returned." , sep = "" ) )
					ret <- NULL
			} else {
			
					# supported file types
					# names ... File Extensions
					# values ... unzip Funktion
					ft <- signature ( "bz2" = "bzfile" )
					
					# erweitern z.B. mit ...
					# ft <- signature ( "bz2" = "bzfile", "gz" = "gzfile", "xz" = "xzfile" )
					# ggf. TESTEN !!!
					
					# Pattern zum detektieren aller files mit supporteten extensions
					pat <- paste ( paste ( "\\." , names ( ft ) , "$" , sep = "" ) , collapse = "|" )			
					
					# wenn directory, dann alle Files ziehen
					isdir <- file.info(path)$isdir
					if ( isdir ) {
							# path <- list.files( path = path , 
												# pattern = pat , all.files = FALSE,
												# full.names = TRUE, recursive = TRUE,
												# ignore.case = FALSE, include.dirs = FALSE )
							path <- list.files( path = path , 
												pattern = pat ,
												full.names = TRUE ,
												include.dirs = FALSE ,
												... )												
					}			
					
					# Namen/Extensions alle Files
					path.names <- unlist ( strsplit ( path , pat ) )
					path.ext <- mapply ( function ( p , pn ) sub ( ( paste (pn,".",sep="") ) , "" , p , fixed = TRUE ) , path , path.names , SIMPLIFY = TRUE , USE.NAMES = FALSE )
					
					# Schleife über alle Files
					.fun <- function ( fl , ext , ft , ... ) {
							do <- paste ( ft[ext] , "('" , fl , "',...)" , sep = "" )
							eval ( parse ( text = do ) )
					}
					ret <- mapply ( .fun , path , path.ext , MoreArgs = list ( ft , ... ) , SIMPLIFY = FALSE )
					names ( ret ) <- path.names
			}
	}
	
	return ( ret )
}
