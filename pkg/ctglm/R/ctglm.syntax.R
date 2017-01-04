
ctglm.syntax <- function ( m, model.name="model", cores=detectCores(), ..., verbose=TRUE ) {
		
# browser()
		# cores modden
		if( !exists("cores") ) cores <- 1L
		if( exists("cores") && !is.numeric(cores) ) cores <- 1L
		if( exists("cores") && is.numeric(cores) && !is.integer(cores) ) cores <- as.integer( floor( cores ) )
		if( exists("cores") && is.numeric(cores) && cores < 1 ) cores <- 1L
		if( exists("cores") && is.numeric(cores) && ( cores.max <- detectCores() ) < cores ) cores <- cores.max

		# new environment
		env <- new.env()
		
		# put all variables (values of list m) into environment
		eval( parse ( text=paste0( "assign( '",names(m), "' , m$'",names(m),"' , envir=env )" ) ) )
		# put additional vars into environment
		vars <- c("model.name","cores","verbose")
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()	

		### TODO matrices consistent callen (falls noch jemand nach model dran rum gefummelt hat)
	
	
	
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
