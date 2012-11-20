
modus <- function ( x , randTies = FALSE ) {
	if ( all ( is.na(x) ) ) {xmode <- NA} else {
			xt<-table(x,useNA="always")
			m <- max(xt)
			if ( length ( w <- ( which ( xt == m ) ) ) == 1 ) {
					xmode <- names(xt)[w]
			} else {
					if ( randTies ) {
							s <- sample ( w , 1 )
							xmode <- names(xt)[s]
					} else {
							xmode<- NA
					}
			}
	}
	if ( is.integer ( x ) ) xmode <- as.integer ( xmode )
	if ( is.numeric ( x ) ) xmode <- as.numeric ( xmode )
	
	return ( xmode )
}
