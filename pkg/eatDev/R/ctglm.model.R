
# d               data in standard ctsem format
# id              name of person id variable in data set
# time            name of time variable in data set 
### timepoint.sep separator of manifest variable names and time point suffix Tx, default: "_"
### lag.names     vector of variable names for the time lags as they appear in the data set; if standard naming (dT1, dT2, ...) is used, setting lag.names is not necessary
# Lambda          loading matrix, dimensions must be number of items x number of latent variables; defaults to diag(1,number of items) to map each manifest variable to one latent variable (so no measurement model is specified)
# verbose         print information

# ctglm.model <- function ( d, id=NULL, timepoint.sep="_", lag.names=NULL, Lambda=NULL, measurement.model=gaussian(link="identity"), ..., priors=NULL, verbose=TRUE ) {
# ctglm.model <- function ( d, id=NULL, timepoint.sep="_", lag.names=NULL, Lambda=NULL, measurement.model=binomial(link="logit"), engine="jags", ..., priors=NULL, verbose=TRUE ) {
ctglm.model <- function ( d, id="id", time="time", Lambda=NULL, measurement.model=binomial(link="logit"), engine=c("jags","ctstan"), ..., priors=NULL, verbose=TRUE ) {
# browser()		
		# engine
		if ( is.character( engine ) && length( engine ) > 1 ) engine <- engine[1] else if( !is.character(engine) || !any("jags" %in% c("jags","ctstan")) ) engine <- "jags"
		
		# new environment
		env <- new.env()
		
		# put all variables (values of arguments of function) into environment
		eval( parse ( text=paste0( "assign( '",ls(), "' , get('",ls(),"') , envir=env )" ) ) )
		# additional arguments from ...
		if( length( list(...) ) > 0 ) {
				eval( parse ( text=paste0( "assign( '",names(list(...)), "' , list(...)$'",names(list(...)),"' , envir=env )" ) ) )
		}
# browser()
		# check input
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
				if (verbose) cat( paste0( "MODEL SUCCESSFULLY CREATED | proceed with ctglm.syntax() \n" ) )
		}
		
		# create jags syntax
		# invisible( create.jags.syntax( env ) )
		
		### return list ###
		
		# overwrite user specified prior list
		# assign( "priors", get( "prior", envir=env ), envir=env )
		
		# list elements
		el <- ls( envir=env )
		
		# remove arguments of ctirt.model and other objects
		el <- el[ !el %in% c("timepoint.sep","verbose","prior","env","lag.names") ]

# browser()
		
		## order of elements
		el <- el[ order(el) ]
		first <- c("d","col.id","col.item","col.time","col.y","A",ifelse(exists("A.prior",envir=env),"A.prior",NA),"Q",ifelse(exists("Q.prior",envir=env),"Q.prior",NA),"b",ifelse(exists("b.prior",envir=env),"b.prior",NA),"beta",ifelse(exists("beta.prior",envir=env),"beta.prior",NA),ifelse(exists("mu.beta",envir=env),"mu.beta",NA),ifelse(exists("prec.beta",envir=env),"prec.beta",NA),ifelse(exists("prec.beta.prior",envir=env),"prec.beta.prior",NA),ifelse(exists("E",envir=env),"E",NA),ifelse(exists("mu.t1",envir=env),"mu.t1",NA),ifelse(exists("mu.t1.prior",envir=env),"mu.t1.prior",NA),ifelse(exists("prec.t1",envir=env),"prec.t1",NA),ifelse(exists("prec.t1.prior",envir=env),"prec.t1.prior",NA),"priors","F","I","J","T","R","P","Lambda","measurement.model")
		first <- first[!is.na(first)]
		last <- c("engine")
		el <- el[ c( match( first, el ) , which( !el %in% c( first, last ) ), match( last, el ) ) ]
		
		# create return list
		ret <- list()
		do <- paste0( "ret$'", el, "' <- get( '", el, "', envir=env )" )
		eval( parse( text = do ) )
		
		# remove environment
		rm( env )
		
		return( ret )
		
}



