
run.jags <- function ( env ) {
		

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# defaults fuer jags
		if (!exists("iter")) iter <- 3
		if (!exists("chains")) chains <- 2
		if (!exists("adapt")) adapt <- 0
		if (!exists("thin")) thin <- 0

		# write bugs file to disk
		dir <- file.path( work.dir, model.name )
		if ( !dir.exists( dir ) ) dir.create( dir )
		bugs.file <- file.path( dir, paste0( model.name, ".bug" ) )
		write.table( syntax, file=bugs.file, quote=FALSE, row.names = FALSE, col.names = FALSE )
browser()	
		# run call step by step
		for (z in 1:nrow(call)){
				if( verbose ) cat( paste0( call[z,], "\n" ) )
				eval( parse( text= call[z,] ) )
		
		}

		# return
		return( r )
}
