
# MH 30.07.2012: wenn mehrere Elemente, dann kann auf 'el.default' (numeric) Element defaultet werden

userSpecifiedList <- function ( l, l.default , el.default = NULL ) {

		if ( !is.null ( names ( l ) ) ) {
				names ( l ) <- match.arg ( names(l) , names(l.default) , several.ok = TRUE )
		} else {
        if(length(l) > length(l.default) )  {
           stop("Length of user-specified list with more elements than default list.\n")
        }
				names ( l ) <- names ( l.default )[seq(along=l)]
		}
		if ( length(l) < length(l.default) ) {
				l <- c ( l , l.default )
				l <- l[!duplicated(names(l))]
				l <- l[match ( names (l) , names(l.default) )]
		}
		
		if ( !is.null ( el.default ) & length ( l ) > 1 ) {
				if ( el.default %in% seq ( along = l ) ) {
						l <- l[el.default]
				} else {
						m <- paste ( "el.default = " , el.default , " is not an element of 'l' (length = " , length(l), ")." , sep = "" )
						warning ( m )
				}
		}
		
		return(l)
}