install.eat.dep <- function ( ) {
		cat ( paste0 ( "installing eat dependencies (not on r-forge):" , "\n" ) )
		pkgs <- c ( "igraph", "survey", "Hmisc", "combinat", "fmsb", "psych", "xlsx", "date", "gdata", "reshape", "sendmailR" , "reshape2" , "stringr" , "ggplot2" , "R.utils" )
		tried <- try ( install.packages ( pkgs , repos = "http://cran.us.r-project.org" ) )
}
