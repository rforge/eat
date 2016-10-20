
run.jags <- function ( env ) {
		

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# defaults fuer jags
		if (!exists("iter",mode="numeric")) iter <- 10
		if (!exists("chains",mode="numeric")) chains <- 2
		if (!exists("adapt",mode="numeric")) adapt <- 0
		if (!exists("thin",mode="numeric")) thin <- 1

		# probably better to push those to data.env
		# assign( "iter", iter, envir=globalenv() )
		# assign( "chains", chains, envir=globalenv() )
		# assign( "adapt", adapt, envir=globalenv() )
		# assign( "thin", thin, envir=globalenv() )
		
		# write bugs file to disk
		dir <- file.path( work.dir, model.name )
		if ( !dir.exists( dir ) ) dir.create( dir )
		bugs.file <- file.path( dir, paste0( model.name, ".bug" ) )
		write.table( syntax, file=bugs.file, quote=FALSE, row.names = FALSE, col.names = FALSE )
# browser()	
		# rjags has problems with environments
		# hard set variables
		# ini.row <- which(grepl("^ini", call[,1]))
		# call[ini.row,1] <- sub( "=chains", paste0("=",chains), call[ini.row,1] )
		# call[ini.row,1] <- sub( "=adapt", paste0("=",adapt), call[ini.row,1] )
		# run.row <- which(grepl("^res", call[,1]))
		# call[run.row,1] <- sub( "=iter", paste0("=",iter), call[run.row,1] )
		# call[run.row,1] <- sub( "=thin", paste0("=",thin), call[run.row,1] )
		
		# run call step by step
		for (z in 1:nrow(call)){
				if( verbose ) cat( paste0( call[z,], "\n" ) )
				eval( parse( text= call[z,] ) )
		}

		# return
		return( res )
}
