
check.consistency <- function ( env ) {
		
		# get variables from env
		eval( parse ( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# error vector for console output
		error <- character(0)
		
		# console output
		if ( verbose ) cat("checking consistency\n")


		# F
		if ( verbose ) cat( paste0( "            number of latent variables (F) is 2: "  ) )
		if ( F==2 ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "number of latent variables (F) is not 2, the currently only supported option   Check LAMBDA." ) }
				
		### measurement stuff ###
		# beta
		if ( verbose ) cat( paste0( "   manifest int./diff. vector beta is IxT (", I, "x", T, "): "  ) )
		if ( identical( dim(beta), c(I,T) ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "manifest intercept vector beta is not IxT (", I, "x", T, ")   Check data and/or beta vector." ) }		

	   # prec.eps
		if ( measurement.model$family == "gaussian" ) {		
				# IxI
				if ( verbose ) cat( paste0( " measurement error matrix prec.eps is IxI (", I, "x", I, "): "  ) )
				if ( identical( dim(prec.eps), c(I,I) ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "measurement error matrix prec.eps is not IxI (", I, "x", I, ")   Check data and/or prec.eps matrix." ) }						
				# symmetric matrix
				if ( verbose ) cat( paste0( "                          prec.eps is symmetric: "  ) )
				if ( identical( prec.eps[lower.tri( prec.eps )], prec.eps[upper.tri( prec.eps )] ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "measurement error matrix prec.eps is not symmetric   Check prec.eps." ) }
				   # uncorrelated errors
				if ( verbose ) cat( paste0( "                uncorrelated measurement errors: "  ) )
				if ( all( c( prec.eps[lower.tri( prec.eps )], prec.eps[upper.tri( prec.eps )] ) == 0 ) ) { if ( verbose ) cat( "OK\n" ) } 
				   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "off-diagonal elements of measurement error matrix are not 0 (correlated errors are currently not supported)   Check prec.eps matrix." ) }		
		}
		
		### continuous time stuff ###
		# A
		if ( verbose ) cat( paste0( "                    drift matrix A is FxF (", F, "x", F, "): "  ) )
		if ( identical( dim(A), c(F,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "drift matrix A is not FxF (", F, "x", F, ")   Check LAMBDA and/or A matrix." ) }
		# Q, FxF
		if ( verbose ) cat( paste0( "            process error matrix Q is FxF (", F, "x", F, "): "  ) )
		if ( identical( dim(Q), c(F,F) ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "drift matrix Q is not FxF (", F, "x", F, ")   Check LAMBDA and/or Q matrix." ) }
		# Q, symmetric matrix
		if ( verbose ) cat( paste0( "                                 Q is symmetric: "  ) )
		if ( identical( Q[lower.tri( Q )], Q[upper.tri( Q )] ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "process error matrix Q is not symmetric   Check Q matrix." ) }		
		# b
		if ( verbose ) cat( paste0( "    continuous time intercept vector b is F (", F, "): "  ) )
		if ( identical( length(b), F ) ) { if ( verbose ) cat( "OK\n" ) } 
		   else { if ( verbose ) cat( "FAIL\n" ); error[length(error)+1] <- paste0( "continuous time intercept vector b is not of length F (", F, ")   Check LAMBDA and/or b vector." ) }		
		
			
		# independent of verbose, output errors to console
		if( length(error)>0 ) {
				cat( paste0( "\nERRORS occured:\n" ) )
				cat( paste0( paste( paste0("[",seq(along=error),"] ",error), collapse="\n" ), "\n" ) )
				cat( paste0( "\nDO NOT RUN THE MODEL.\n" ) )
		}
		
		# if any errors return FALSE	
		if( length(error)>0 ) return( FALSE ) else return( TRUE )

}
