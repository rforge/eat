		
isConverged <- function ( path , txt = FALSE ) {

		shwf <- list.files ( path = path , 
						   pattern = "\\.shw$" , all.files = FALSE,
						   full.names = TRUE, recursive = TRUE,
						   ignore.case = FALSE, include.dirs = FALSE )

		.fun1 <- function ( shwf ) {
		
				dn <- dirname ( shwf )
				
				# kompletter show-File
				tried <- try ( l <- readLines( shwf ) , silent = TRUE )
				if ( inherits ( tried , "try-error" ) ) stop ( paste ( "could not open file" , log.path ) )
				
				c1 <- "Iterations terminated because the deviance convergence criteria was reached"
				c2 <- "Iterations terminated because the convergence criteria were reached"
				not1 <- "At termination the solution was not the best attained solution"
				any ( ( grepl ( c1 , l ) ) | any ( grepl ( c2 , l ) ) & ! any ( grepl ( not1 , l ) ) )
				
		}
		conv <- mapply ( .fun1 , shwf )

		# Rückgabe-Dataframe
		model <- sub ( path , ".." , dirname ( names ( conv ) ) )
		nr <- seq ( along = conv )
		converged <- conv
		shw.path <- names ( conv )
		dfr <- data.frame ( nr, model , converged , shw.path , stringsAsFactors = FALSE )
		rownames ( dfr ) <- dfr$nr
		
		if ( txt ) {
				.fun2 <- function ( dn , conv ) {
						tf <- if ( conv ) "_CONVERGED_" else "_N_O_T_CONVERGED_"
						f <- file.path ( dn , tf )
						file.create ( f , recursive = TRUE )
				}
				temp <- mapply ( .fun2 , dirname ( names ( conv ) ) , conv )
				
				# summary rausschreiben
				f <- file.path ( path , "convergence_summary.txt" )
				
				write.fwf (	dfr, file=f, append=FALSE, quote=FALSE, sep=" ", na="",
							rownames=FALSE, colnames=TRUE, rowCol=NULL, justify="left",
							formatInfo=FALSE, quoteInfo=TRUE, width=NULL, eol="\n",
							qmethod=c("escape", "double") )
				
				invisible ( dfr )
		} else {
				return ( dfr )
		}
}
