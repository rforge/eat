
find.object <- function ( otf ) {

		env <- sys.parents ()
		akt.env <- max ( env ) + 1
		
		if ( ! identical ( env , integer(0) ) ) {
			
				.fun <- function ( ev , x ) {
						eval ( parse ( text = paste ( "exists('" , x , "')" , sep = "" ) ) , envir = ev )
				}
				where <- as.logical(mapply ( .fun , env , MoreArgs = list ( otf ) , SIMPLIFY = TRUE ))
				names ( where ) <- env
		
				if ( ! identical ( ( wherei <- names ( which ( where ) ) ) , character(0) ) ) {
						getwhere <- max ( as.numeric ( wherei ) )
						# env <- parent.frame ( akt.env - getwhere )
						# attach ( env , warn.conflicts = FALSE )
						ret <- eval ( parse ( text = paste ( "ret<-" , otf , sep = "" ) ) , envir = getwhere )
						# ret <- eval ( otf , envir = getwhere )
						# detach ( env )
				} else ret <- NULL
		
		} else ret <- NULL
		return ( ret )
}
