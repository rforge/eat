
eatVersion <- function ( dep = FALSE ) {
		
		ip <- installed.packages()[,c("Package")]
		
		p <- grepl ( "^eat" , ip )
		
		if ( dep ) {
				deps <- c ( "reshape2" , "igraph", "survey", "Hmisc", "combinat", "fmsb", "psych", "xlsx", "date", "gdata", "sendmailR" , "stringr" , "ggplot2" , "R.utils" , "lme4" , "car" , "mice" )
				p <- p | ( ip %in% deps )
		}
		
		d <- data.frame ( installed.packages()[ p , c("Package","Version") , drop=FALSE ] , stringsAsFactors = FALSE )
		rownames ( d ) <- seq ( along = rownames ( d ) )
		return ( d )
}
