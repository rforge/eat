install.eat.dep <- function ( ) {
		cat ( paste0 ( "installing eat dependencies (not on r-forge):" , "\n" ) )
		pkgs <- c ( "reshape2" , "igraph", "survey", "Hmisc", "combinat", "fmsb", "psych", "xlsx", "date", "gdata", "sendmailR" , "stringr" , "ggplot2" , "R.utils" , "lme4" , "car" )
		tried <- try ( install.packages ( pkgs , repos = "http://cran.us.r-project.org" ) )
}
