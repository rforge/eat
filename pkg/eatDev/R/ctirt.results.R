
ctirt.results <- function ( r, plot.dir=NULL, verbose=TRUE, ... ) {
		
# browser()
		# new environment
		env <- new.env()
		
		# put values of list r into environment
		eval( parse ( text=paste0( "assign( '",names(r), "' , r$'",names(r),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
		vars <- ls()[ !ls() %in% "r" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()			
		### call software specific results preparation
		if( get( "engine", envir=env ) %in% "jags" ) {
				e <- results.jags( env )
		} else {
				e <- NULL
		}
		
		if ( verbose ) cat( paste0( "\nDONE\n" ) )
		
		# return
		return( e )
}
