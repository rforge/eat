
# d               data in standard ctsem format
# id              name of person id variable in the data set
# timepoint.sep   separator of manifest variable names and time point suffix Tx, default: "_"
# lag.names       vector of variable names for the time lags as they appear in the data set; if standard naming (dT1, dT2, ...) is used, setting lag.names is not necessary
# Lambda          loading matrix, dimensions must be number of items x number of latent variables; defaults to diag(1,number of items) to map each manifest variable to one latent variable (so no measurement model is specified)
# verbose         print information

ctirt.model <- function ( d, id=NULL, timepoint.sep="_", lag.names=NULL, Lambda=NULL, measurement.model=gaussian(link="identity"), ..., priors=NULL, verbose=TRUE ) {

		# new environment
		env <- new.env()
		
		# put all variables (values of arguments of function) into environment
		eval( parse ( text=paste0( "assign( '",ls(), "' , get('",ls(),"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}

		# data preparation
		invisible( check.input( env ) )
		
		# data preparation
		invisible( prep.data( env ) )
		
		# check consistency
		error <- check.consistency( env )
		
		# set priors
		invisible( set.priors( env ) )
		
		# independent of verbose, output errors to console
		if( length(error)>0 ) {
				cat( paste0( "ERRORS:\n" ) )
				cat( paste0( paste( paste0("[",seq(along=error),"] ",error), collapse="\n" ), "\n\n" ) )
				cat( paste0( "DO NOT RUN THE MODEL!\n" ) )
		} else {
				if (verbose) cat( paste0( "MODEL SUCESSFULLY CREATED | proceed with ctirt.run() \n" ) )
		}
		
		# create jags syntax
		invisible( create.jags.syntax( env ) )
		
}



