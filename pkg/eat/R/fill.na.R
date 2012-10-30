
fill.na <- function ( vec , backwards = FALSE , na.rm = FALSE ) {

		stopifnot ( is.vector ( vec ) )
		
		if ( all ( is.na ( vec ) ) ) {
				ret <- vec
		} else {
				if ( ! length ( vec ) > 1 ) {
						ret <- vec
				} else {
						notna <- !is.na ( vec )
						notna.ind <- which ( notna )
						
						# repetition length
						if ( ! length ( notna.ind ) > 1 ) {
								repl <- if ( !backwards ) length(notna.ind:length(vec)) else length (1:notna.ind)
						} else {
								repl <- if ( !backwards ) c ( notna.ind[-1] , length ( vec ) + 1 ) - notna.ind else notna.ind - c ( 0, notna.ind[1:(length(notna.ind)-1)] )
						}
				
						# repetition
						repv <- rep ( vec[notna.ind] , repl )
						if ( !backwards ) {
								if ( notna.ind[1] > 1 ) {
										ret <- c( vec[1:(notna.ind[1]-1)] , repv ) 
								} else ret <- repv
						} else {
								if ( ( i <- notna.ind[length(notna.ind)] ) < length ( vec ) ) {
										ret <- c( repv , vec [ (i+1):length(vec) ] )
								} else  ret <- repv
						}
				}
		}

		# na.rm
		if ( na.rm ) ret <- ret[!is.na(ret)]
		if ( identical ( ret , logical(0) ) ) ret <- NULL
		
		return ( ret )
}

# Test
# ( vec <- c ( NA , 1 , NA , NA , 2 , NA , 3 , NA ) )
# fill.na ( vec )
# fill.na ( vec , backwards = TRUE )
# ( vec <- c ( NA , 1 , NA ) )
# fill.na ( vec )
# fill.na ( vec , backwards = TRUE )
# ( vec <- c ( NA , 1 ) )
# fill.na ( vec )
# fill.na ( vec , backwards = TRUE )
# ( vec <- c ( 1 , NA) )
# fill.na ( vec )
# fill.na ( vec , backwards = TRUE )
# ( vec <- c ( 1 , 1 , 1 ) )
# fill.na ( vec )
# fill.na ( vec , backwards = TRUE )
