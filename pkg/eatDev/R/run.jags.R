
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

# browser()		
		# return list
		ret <- list()

		# first entry: engine
		ret$engine <- engine

		# second entry: results
		ret$results <- res
		names( ret$results )

		## third entry: parameters
		# for better usability parameter from par.env will be put into list
		pl <- list()
		# do <- paste0( "pl$'", ls( envir=get("par.env",env) ) , "' <- get( '", ls( envir=get("par.env",env) ) ,"', envir=get('par.env',env) )" )
		do <- paste0( "pl$'", ls( envir=par.env ) , "' <- get( '", ls( envir=par.env ) ,"', envir=par.env )" )
		eval( parse( text=do ) )
		ret$parameters <- pl
		names( ret$parameters )

		
		## sortieren von results/parameters
		first <- c("A","Q","b","beta","mu.beta","prec.beta")
			
		# sortieren von results (second entry) wie in Parameter-List
		ord <- match( first, names(ret$results) )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) ret$results <- c( ret$results[ord], ret$results[ !ret$results %in% ret$results[ord] ] )
		rm("ord")
# browser()
		# sortieren von results (second entry) wie in Parameter-List
		ord <- match( first, names(ret$parameters) )
		ord <- ord[!is.na(ord)]
		if( !identical( ord, integer(0) ) ) ret$parameters <- c( ret$parameters[ord], ret$parameters[ !ret$parameters %in% ret$parameters[ord] ] )

		
		# return
		return( ret )
}
