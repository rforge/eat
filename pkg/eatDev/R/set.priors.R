
set.priors <- function ( env ) {
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )

		# console output
		if ( verbose ) cat("setting priors\n\n")		

		# list of priors
		prior <- list()
		
		# A
		if ( verbose ) cat( "                                 drift matrix A: " )
		if ( any( is.na( suppressWarnings( as.numeric( A ) ) ) ) ) {
				prior[[length(prior)+1]] <- make.priors( A, priors, "dnorm(-0.5,1)", "dnorm(0,1)", verbose=verbose )
		} else {
				if( verbose ) cat( paste0( "n/a (all ", prod(dim(A)), " values fixed)\n" ) )
		}

		# at the end line break
		if ( verbose ) cat( paste0( "\n" ) )
		
		### (over)write relevant variables to environment ###
		obj <- c( "prior" )
		eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )

		# return
		TRUE
}

make.priors <- function( m, priors, diag.prior = "dnorm(0,1)", offdiag.prior = "dnorm(0,1)", prior = "dnorm(0,1)", verbose ){

		v <- as.vector( m )
		vn <- suppressWarnings( as.numeric( v ) )
		vc <- v[is.na(vn)]
		# user defined
		vcuser <- vc[ vc %in% names( priors ) ]
		vc <- vc[!vc %in% vcuser]
		# diagonal elements vs. off-diagonals, bei quadratischen Matrizen
		vcdiag <- ifelse( sd(dim(m))==0, vc[ vc %in% diag(m) ], character(0) )
		vcoffdiag <- ifelse( sd(dim(m))==0, vc[ !vc %in% diag(m) ], character(0) )
		vc <- vc[!vc %in% c(vcdiag,vcoffdiag)]
		
		l <- list()
	    l[[1]] <- sapply( vcuser, function(x) paste0( x, " ~ ", priors[[x]] ) )
		l[[2]] <- sapply( vcdiag, function(x) paste0( x, " ~ ", diag.prior ) )
		l[[3]] <- sapply( vcoffdiag, function(x) paste0( x, " ~ ", offdiag.prior ) )
		l[[4]] <- sapply( vc, function(x) paste0( x, " ~ ", prior ) )
		# pr <- unlist( do.call( "c", l ) )
		pr <- do.call( "c", l )
		
		if( verbose ) cat( paste0( length(vn[is.na(vn)])-length( vcuser ), " default, ", length( vcuser ), " user, (",length(vn[!is.na(vn)]), " fixed value)\n" ) )
		
		return( pr )
}