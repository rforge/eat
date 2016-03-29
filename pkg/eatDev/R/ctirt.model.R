
# d               data in standard ctsem format
# id              name of person id variable in the data set
# manifestNames   vector of manifest variable names as they appear in the data set, without the Tx time point suffix. Defaults to Y1, Y2, etc.
# timepoint.sep   separator of manifest variable names and time point suffix Tx, default: "_"
# lagNames        vector of variable names for the time lags as they appear in the data set; if standard naming (dT1, dT2, ...) is used, setting lagNames is not necessary
# LAMBDA          loading matrix, dimensions must be  number of items x number of latent variables; defaults to diag(1,number of items) to map each manifest variable to one latent variable (so no measurement model is specified)
# verbose         print information

ctirt.model <- function ( d, id=NULL, manifestNames=NULL, timepoint.sep="_", lagNames=NULL, LAMBDA=NULL, verbose=TRUE ) {

# browser()
		# new environment
		env <- new.env()
		
		# put all variables into environment
		eval( parse ( text=paste0( "assign( '",ls(), "' , get('",ls(),"') , envir=env )" ) ) )

		# data preparation
		invisible( prep.data( env ) )
		
		
		
		
		
		
		
}



