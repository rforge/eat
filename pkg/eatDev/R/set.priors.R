
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
				prior[[length(prior)+1]] <- make.priors( m.name="A", m=A, priors=priors, env=env, diag.prior = "dnorm(-0.5,0.001)", offdiag.prior = "dnorm(0,0.001)", verbose=verbose )
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

make.priors <- function( m.name, m, priors, env, diag.prior = "dnorm(0,0.001)", offdiag.prior = "dnorm(0,0.001)", prior = "dnorm(0,0.001)", lower=TRUE, upper=TRUE, verbose ){

		if( !exists( paste0( m.name, '.prior' ), envir=env ) ) {

				# all non-numeric and non-NA (= free parameters) to NaN
				# original NA are kept
				m2 <- suppressWarnings( array( as.character( as.numeric( m ) ), dim=dim(m) ) )
				m2[is.na(m2)] <- "<setprior>"
				m2[is.na(m)] <- NA
				m2.fixed <- !( is.na(m2) | m2 %in% "<setprior>" )
				
				# number of free parameters (for which priors need to be set)
				free <- length( m2[!m2 %in% "<setprior>"] )
				# number NA-parameter
				na <- length( m2[is.na( m2 )] )
				# number of fixed parameters
				fixed <- sum( dim( m2 ) ) - free - na
				
				## user defined priors
				if ( !is.null( priors ) ) {
						
						# names of priors list
						pn <- names( priors )
						
						# which matrix?
						matr <- sub( "^([^0-9&^,&^_&^\\[&^\\(]+).*$", "\\1", pn )
					
						if ( any( matr %in% m.name ) ) {
								
								# reduce to relevant user priors
								priors <- priors[ matr %in% m.name ]
								pn <- pn[ matr %in% m.name ]
								
								# get numbers out of string
								x <- gsub( "[^0-9&^,&^_]", "", sub( m.name, "", pn ) )
								
								# _ zu Komma
								x <- sapply( x, function(x) gsub( "_+", ",", x ) )
								
								# if nchar equals number of dims split
								# e.g. 21 -> 2,1
								x <- sapply( x, function ( x ) if ( nchar( x ) == length( dim(m) ) ) paste( unlist( strsplit( x, "" ) ), collapse="," ) else x )
								
								# set user priors
								do <- paste0( "m2[", x, "] <- priors[[", seq(along=x), "]]" )
								eval( parse( text=do ) )
										
						}
				}
				# number user defined parameters
				udef <- free - length( m2[!m2 %in% "<setprior>"] )

				## defaults		
				# if quadratic matrix
				if ( all( dim(m2) == dim(m2)[1] ) ) {
						
						# TODO symmetric abfangen
						
						# set diagonal priors (matrix or array)
						for ( i in 1:dim(m2)[1] ) {
								do2 <- paste0( "if( m2[",paste( rep( i, length(dim(m2)) ), collapse="," ),"] %in% '<setprior>' ) m2[",paste( rep( i, length(dim(m2)) ), collapse="," ),"] <- diag.prior " )
								eval( parse( text = do2 ) )
						}

						# lower triangle (of matrix)
						if (lower) m2[lower.tri(m2) & m2 %in% "<setprior>"] <- offdiag.prior

						# upper triangle (of matrix)
						if (upper) m2[upper.tri(m2) & m2 %in% "<setprior>"] <- offdiag.prior
						
				}
				
				# TODO non-quadratic
				
				# fixed parameter auf NA setzen
				# e.g. 1 -> NA
				m2[m2.fixed] <- NA
				
				# TODO rownames von Matrix fuer prior Matrix
				
				# number defaulted parameters
				def <- free - udef - length( m2[is.na( m2 )] )
				
				# write to environment
				assign( paste0( m.name, '.prior' ), m2, envir=env )
				
				# console output
				if ( verbose ) cat( paste0( def, " default, ", udef, " user (",fixed, " fixed value, ",na," NA)\n" ) )
				
		} else {
				# console output
				if ( verbose ) cat( paste0( m.name, ".prior set by user", "\n" ) )
		}
		
}