
eatVersion <- function ( dep = FALSE ) {
		
		ip <- installed.packages()[,c("Package")]
		
		p <- grepl ( "^eat" , ip )
		
		if ( dep ) {
				deps <- c ( "MASS", "car", "date", "foreign", "gdata", "reshape", "sendmailR", "xlsx", "plyr", "R.utils", "parallel", "psych", "ggplot2" )
				p <- p | ( ip %in% deps )
		}
		
		d <- data.frame ( installed.packages()[ p , c("Package","Version") , drop=FALSE ] , stringsAsFactors = FALSE )
		rownames ( d ) <- seq ( along = rownames ( d ) )
		return ( d )
}
