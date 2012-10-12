
getConquestVersion <- function ( path.conquest , path.temp, asDate = TRUE ) {
    if(missing(path.temp)) {path.temp <- getwd()}
		wd <- path.temp
		f <- file.path ( wd , "delete.cqc" )
		if ( file.create ( f, showWarnings = TRUE ) ) {
		
				write ( "quit;" , f )
				
				f <- normalizePath ( f )
				path.conquest <- normalizePath ( path.conquest )
				cmd <- paste ( '"' , path.conquest , '"' , " " , '"' , f , '"' , sep = "" )
				r <- NULL
				ow <- getOption ( "warn" )
				options ( warn = -1 )
				try ( r <- system ( command = cmd , intern = TRUE ) , silent = TRUE )
				options ( warn = ow )
				
				file.remove ( f )
				
				if ( !is.null ( r ) ) {
				
						r <- r[1]

						r <- sub ( "ConQuest build: " , "" , r )

						r <- gsub ( "\\s+" , "-" , r )
						
						if ( asDate ) r <- as.date(r)
						
				}

		} else {
				r <- NULL
		}
		
		return (r)

}


