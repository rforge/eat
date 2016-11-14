
ctglm.save.syntax <- function( s, dir, ... ) {
# browser()
		# new environment
		env <- new.env()
		
		# put values of list s into environment
		eval( parse ( text=paste0( "assign( '",names(s), "' , s$'",names(s),"' , envir=env )" ) ) )
		# put all variables (values of arguments of function) into environment
		vars <- ls()[ !ls() %in% "s" ]
		eval( parse ( text=paste0( "assign( '",vars, "' , get('",vars,"') , envir=env )" ) ) )
		# additional arguments from ...
		# if( length( list(...) ) > 0 ) {
				# eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		# }
		# put values from data.env into environment
		eval( parse( text=paste0( "assign( '",ls(envir=s$data.env), "' , get('",ls(envir=s$data.env),"', envir=s$data.env ), envir=env )" ) ) )
		
		# put values from data.env into environment
		eval( parse( text=paste0( "assign( '",ls(envir=s$par.env), "' , get('",ls(envir=s$par.env),"', envir=s$par.env ), envir=env )" ) ) )		
		
		### Rdata
		path.rdata <- file.path( dir, paste0( get( "model.name", envir=env ), ".Rdata" ) )
		
		# save
		save( list=ls(envir=env), file=path.rdata, envir=env )
		
		### Model call
		# modify to include load of objects
		# i.e. load objects from dir/file.name
		call <- get( "call", envir=env )
		# where is ### engine
		ind <- grep( "### engine:", call[,1] )
		# add load
		add <- matrix( paste0( "# load data/objects" ), ncol=1 )
		add <- rbind( add, paste0( "( load( '", path.rdata, "' ) )" ) )
		add <- rbind( add, "" )
		
		# at this point no run parameters are usually not set, default
		add <- rbind( add, '# run parameters defaults' )
		add <- rbind( add, 'if (!exists("iter",mode="numeric")) iter <- 10' )
		add <- rbind( add, 'if (!exists("chains",mode="numeric")) chains <- 2' )
		add <- rbind( add, "" )

		# modified call
		call2 <- do.call( "rbind", list( call[1:(ind+1),,drop=FALSE], add, call[(ind+2):nrow(call),,drop=FALSE] ) )		
		
		# write call
		path.rcall <- file.path( dir, paste0( get( "model.name", envir=env ), ".R" ) )		
		write.table( call2, file=path.rcall, row.names=FALSE, col.names=FALSE, quote=FALSE )
		
		return( TRUE )
	
}
