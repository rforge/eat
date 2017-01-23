
ctglm.results <- function ( r, plot.dir=NULL, plot.person.par=FALSE, cores=detectCores(), value=c("mode","median","mean"), verbose=TRUE, ... ) {
		
# browser()
		# warnings loeschen
		assign("last.warning", NULL, envir = baseenv())
		
		# create plot.dir if not exists
		if( !is.null( plot.dir ) && !dir.exists( plot.dir ) ) dir.create( plot.dir )

		# value auf 1 beschraenken
		if( length( value ) > 1 ) value <- value[1]
		
		# new environment
		env <- new.env()

		# cores modden
		if( !exists("cores") ) cores <- 1L
		cores <- get.cores( cores )
		assign( "cores", cores, envir=env )
		
		# put values of list r into environment
		eval( parse ( text=paste0( "assign( '",names(r), "' , r$'",names(r),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
# browser()		
		vars <- ls()[ !ls() %in% "r" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()			
		### call software specific results preparation
		if( get( "engine", envir=env ) %in% c("jags","ctstan") ) {
				e <- results.jags.ctstan( env, mode=get( "engine", envir=env ) )
		} else if( get( "engine", envir=env ) %in% c("ctsem") ) {
				e <- results.ctsem( env )
		} else {
				e <- NULL
		}
		
# browser()		
		if ( !is.null( e ) ) {
				# add model names
				e$model.name <- get( "model.name", envir=env )
				vorn <- c("model.name","engine")
				e <- e[ , c(vorn, colnames(e)[!colnames(e) %in% vorn]) ]
		}
# browser()		
		if ( verbose ) {
				cat( paste0( "\nDONE | :-)\n" ) )				
		}
		
		# return
		return( e )
}
