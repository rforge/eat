
dichotomize <- function ( numvec , method = c("median","mean") , randomize = TRUE , ... ) {

		# Checks
		stopifnot ( is.numeric ( numvec ) )
		method <- match.arg ( method , c("median","mean") )
		
		# wenn keine Varianz dann original zurückgeben
		if ( var ( numvec ) == 0 ) {
				warning ( "numvec is a constant and can not be dichotomized. original numvec is returned by function dichotomize." )
				ret <- numvec
		} else {
				# Split value
				spl <- eval ( parse ( text = paste ( method , "(numvec)" , sep = "" ) ) )
				
				# Elemente die genau den Split value treffen
				w <- numvec %in% spl				
				
				# splitten
				ret <- cut ( x = numvec , breaks = c(-Inf,spl,Inf) , ... )
				
				# wenn Elemente die genau den Split value treffen, zufällig zugewiesen werden sollen
				if ( randomize & any ( w ) ) {
						try ( set.seed(...) , silent = TRUE )
						ret[w] <- sample ( levels(ret) , length ( ret[w] ) , replace = TRUE )
				}
		}

		return ( ret )
}
