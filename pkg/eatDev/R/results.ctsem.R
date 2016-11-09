
results.ctsem <- function ( env ) {
# browser()		
		# require packages
		# require( "ggplot2" )
		# require( "shinystan" )
		# require( "coda" )
		require( "ctsem" )
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# output
		if ( verbose ){
				cat( paste0( "Extracting results from ctsem/OpenMx object...", "\n" ) )
				# cat( paste0( "            iterations: ", iter, "\n" ) )
				# cat( paste0( "                chains: ", chains, "\n" ) )
				# cat( paste0( "     thinning interval: ", thin, "\n" ) )
				cat( paste0( "\n" ) )
				flush.console()
		}		
		
		# make list of all parameters
		pars.l <- mapply( get.par.list, r$parameters, names( r$parameters ), MoreArgs=list(mode="ctsem"), SIMPLIFY=FALSE )
		pars <- do.call( "rbind", pars.l )
		rownames( pars ) <- seq( along=rownames( pars ) )

		extr <- function( z ) {

				if ( verbose ) cat( paste0( "   ", z["parameter"], "\n" ) ); flush.console()
				ret <- data.frame( "name"=z["name"], "variable"=z["parameter"], "value"=eval( parse( text=z["call"] ) ), stringsAsFactors=FALSE )
				
				return( ret )
		}
		est.l <- apply( pars, 1, extr )
		est <- do.call( "rbind", est.l )

		### mods
		# prec is already var
		est$name <- sub( "^prec", "var", est$name )
		est$variable <- sub( "^prec", "var", est$variable )
		
		# return
		rownames( est ) <- seq( along=rownames( est ) )
		return( est )
}
