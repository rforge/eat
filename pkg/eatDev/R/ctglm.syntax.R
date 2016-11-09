
ctglm.syntax <- function ( m, model.name="model", verbose=TRUE ) {
		
# browser()		
		# new environment
		env <- new.env()
		
		# put all variables (values of list m) into environment
		eval( parse ( text=paste0( "assign( '",names(m), "' , m$'",names(m),"' , envir=env )" ) ) )
		# put additional vars into environment
		vars <- c("model.name")
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		# if( length( list(...) ) > 0 ) {
				# eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		# }
# browser()		
		### call software specific syntax/call generator
		if( get( "engine", envir=env ) %in% "jags" ) {
				s <- create.jags.syntax( env )
		} else if( get( "engine", envir=env ) %in% "ctstan" ) {
				s <- create.ctstan.ctsem.syntax( env, mode="ctstan" )
		} else if( get( "engine", envir=env ) %in% "ctsem" ) {
				s <- create.ctstan.ctsem.syntax( env, mode="ctsem" )
		}
		
		if (verbose) cat( paste0( "SYNTAX SUCCESSFULLY CREATED | proceed with ctglm.run() \n" ) )
		
		# return
		return( s )
}
