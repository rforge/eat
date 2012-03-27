
# gets a specific file (newest, oldest) from files with specific extensions (ext) from a directory
# searches for subdirectories "i386" and "x64" to automatically choose the adequate version
# if nothing can be found, NULL is returned or script is stopped (dependent on crit.level)
get.file.from.dir <- function ( dr , ext = "exe" , vers = c("newest","oldest") , crit.level = c("stop","warning","verbose","silent") ) {
		
		# Argumente
		vers <- match.arg ( vers, c("newest","oldest") )
		crit.level <- match.arg ( crit.level, c("stop","warning","verbose","silent") )
		
		# Checks
		stopifnot ( length ( ext ) < 2 )
		
		# file to be returned
		fl <- NULL
		
		if ( file.info(dr)$isdir ) {
		
				# identify R architecture
				arch <- Sys.getenv("R_ARCH")
				arch1 <- sub ( "/" , "" , arch )
				
				# determine if a subdirectory "i386" or "x64" exists
				# full.names = TRUE and = FALSE gives same results ... strange
				dirs <- list.dirs(path = dr, full.names = TRUE, recursive = FALSE )
				bn <- basename ( dirs )
				
				# set architecture specific subfolder if exists
				if ( arch1 %in% bn ) dr <- dirs[ which ( bn == arch1 ) ]
				
				# search all files with specific pattern
				pattern <- NULL
				if ( ! is.null ( ext ) ) if ( ! ext == "" ) pattern <- paste ( "\\." , ext , "$" , sep = "" )
				
				# files 
				fls <- list.files ( path = dr , pattern = pattern , full.names = TRUE )
				
				if ( ! identical ( fls , character(0) ) ) {
						
						# Version ( newest, oldest ) suchen
						info <- file.info ( fls )$mtime
						what <- signature ( "newest" = "max" , "oldest" = "min" )
						fl.vers <- eval ( parse ( text = paste ( unname ( what[vers] ) , "(info)" , sep ="" ) ) )
						fl <- fls [ which ( info %in% fl.vers ) ]
				
				}
		}
		
		# Output nach criticality level
		if ( ( crit.level != "silent" ) & is.null ( fl ) ) {
				what <- signature ( "stop" = "stop" , "warning" = "warning" , "verbose"="cat" )
				if ( crit.level != "stop" ) add1 <- "\nNULL is returned." else add1 <- ""
				eval ( parse ( text = paste ( unname ( what[crit.level] ) , "('" , 
									  paste ( "get.file.from.dir:\ncould not find a file with extension=\\'" , ext , "\\' in directory " , dr ,
											  "\nCheck directory!!" , add1 , sep = "") ,
									  "')" , sep ="" )
					 ) )
		}
		
		# Output bei verbose
		if ( ( crit.level == "verbose" ) & !is.null ( fl ) ) cat ( paste ( "get.file.from.dir: file '" , fl , "' is returned." ) )
		
		return ( fl )
}

# Tests
# get.file.from.dir ( file.path ( .Library , "eat/winexe/conquest" ) )
# get.file.from.dir ( file.path ( .Library , "eat/winexe/conquest" ) , vers = "oldest" )
# get.file.from.dir ( file.path ( .Library , "eat/winexe/conquest" ) , ext = NULL )
# get.file.from.dir ( file.path ( .Library , "eat/winexe/conquest" ) , ext = "bat" )
# get.file.from.dir ( file.path ( .Library , "eat/winexe/conquest" ) , ext = "bat" , crit.level = "warning" )
# get.file.from.dir ( dr = file.path ( .Library , "eat/winexe/conquest" ) , ext = "exe" , vers = "newest" , crit.level = "stop" )

