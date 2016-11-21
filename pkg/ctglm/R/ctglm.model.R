
ctglm.model <- function ( d, id="id", time="time", person.var=c("b"=TRUE,"mu.t1"=TRUE), track.person.par=NULL, Lambda=NULL, measurement.model=binomial(link="logit"), engine=c("jags","ctstan","ctsem"), ..., priors=NULL, verbose=TRUE ) {
# browser()		
		# engine
		if ( is.character( engine ) && length( engine ) > 1 ) engine <- engine[1] else if( !is.character(engine) || !any(engine %in% c("jags","ctstan","ctsem")) ) { if(verbose) cat(paste0("engine='",engine,"' is not correctly specified | engine set to 'jags'\n\n")); engine <- "jags" }
		
		# person.var
		if( person.var["b"] && !person.var["mu.t1"] ){
				if( verbose ) cat( paste0( "if 'b' varies over persons, 'mu.t1' should also vary over persons | person.var['mu.t1'] has been set to TRUE\n\n" ) )
				person.var["mu.t1"] <- TRUE
		}

		# track.person.par
		# if someone puts "b" in, convert to "bj"
		if( "b" %in% track.person.par ) track.person.par[ track.person.par %in% "b" ] <- "bj"
		# if someone puts "mu.t1" in, convert to "mu.t1"
		if( "mu.t1" %in% track.person.par ) track.person.par[ track.person.par %in% "mu.t1" ] <- "mu.t1.j"
		
		
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
		first <- c("d","col.id","col.item","col.time","col.y","A",ifelse(exists("A.prior",envir=env),"A.prior",NA),ifelse(exists("Q",envir=env),"Q",NA),ifelse(exists("Q.prior",envir=env),"Q.prior",NA),ifelse(exists("cholQ",envir=env),"cholQ",NA),ifelse(exists("cholQ.prior",envir=env),"cholQ.prior",NA),"b",ifelse(exists("b.prior",envir=env),"b.prior",NA),"beta",ifelse(exists("beta.prior",envir=env),"beta.prior",NA),ifelse(exists("mu.beta",envir=env),"mu.beta",NA),ifelse(exists("prec.beta",envir=env),"prec.beta",NA),ifelse(exists("prec.beta.prior",envir=env),"prec.beta.prior",NA),ifelse(exists("E",envir=env),"E",NA),ifelse(exists("mu.t1",envir=env),"mu.t1",NA),ifelse(exists("mu.t1.prior",envir=env),"mu.t1.prior",NA),ifelse(exists("prec.t1",envir=env),"prec.t1",NA),ifelse(exists("prec.t1.prior",envir=env),"prec.t1.prior",NA),"priors","F","I","J","T","R","P","Lambda","measurement.model")
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



