
create.jags.syntax <- function ( env ) {

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# objects with free parameters must be overwritten or deleted
		# environment to save original objects
		par.env <- new.env()
		
		# syntax matrix (1 column)
		x<-matrix(  "model", 1, 1 )
		x<-rbind(x, "{                                                                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### DATA ###                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # data d in long format                                          ")
		x<-rbind(x, "    # d[,col.id]: person                                             ")
		x<-rbind(x, "    # d[,col.item]: item                                             ")
		x<-rbind(x, "    # d[,col.time]: time point                                       ")
		x<-rbind(x, "    # d[,col.y]: responses                                           ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "    ### MEASUREMENT MODEL ###                                        ")
		x<-rbind(x, "                                                                     ")
		# x<-rbind(x, "    # estimated parameters                                           ")
		# x<-rbind(x, "    # theta:        J (person)    x F (faktor)    x T (time point)   ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over rows in long data set d                              ")
		x<-rbind(x, "    for (r in 1:R) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # distributional assumption                                  ")
		if( measurement.model$family %in% "binomial" ) {
		x<-rbind(x, "        d[r,col.y] ~ dbern( mu.y[r] )                                ") }
		if( measurement.model$family %in% "gaussian" ) {
		x<-rbind(x, "        d[r,col.y] ~ dnorm( mu.y[r], E[ d[r,col.item] ] )     ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # link                                                       ")
		if( measurement.model$link %in% "logit" ) {
		x<-rbind(x, "        mu.y[r] <- ilogit( eta[r] )                                  ") }
		if( measurement.model$family %in% "gaussian" ) {
		x<-rbind(x, "        mu.y[r] <- eta[r]                                            ") }		
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # linear predictor                                           ")
		# x<-rbind(x, "        eta[r] <- sum( Lambda[ d[r,col.item],  , d[r,col.time] ] * theta[ d[r,col.id], , d[r,col.time] ] )  +  beta[ d[r,col.item], 1 ]  ")
		x<-rbind(x, "        eta[r] <- sum( Lambda[ d[r,col.item], ] * theta[ d[r,col.id], , d[r,col.time] ] )  +  beta[ d[r,col.item], 1 ] ")
		# x<-rbind(x, "        eta[r] <- sum( Lambda[ d[r,col.item], ] * theta[ d[r,col.id], , d[r,col.time] ] ) ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
### TODO E Matrix richtig einpflegen, von Anfang an		
		if( measurement.model$family %in% "gaussian" ) {		
		x<-rbind(x, "    # precision of measurement errors                                ")
		x<-rbind(x, "    for (i in 1:I) {                                                 ")
		x<-rbind(x, "        E[i] ~ dgamma(1,0.005)                                       ")
		x<-rbind(x, "    }                                                                ") 
		x<-rbind(x, "                                                                     ") }
		x<-rbind(x, "    # values/priors of item easiness                                 ")
		x<-rbind(x, make.str( "beta" ) ); invisible(moveTo.par.env("beta",env,par.env))
		x<-rbind(x, "                                                                     ")
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of mean of item easiness                         ")
		x<-rbind(x, make.str( "mu.beta" ) ); invisible(moveTo.par.env("mu.beta",env,par.env))
		x<-rbind(x, "                                                                     ") }
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of precision of item easiness                    ")
		x<-rbind(x, make.str( "prec.beta" ) ); invisible(moveTo.par.env("prec.beta",env,par.env))
		x<-rbind(x, "                                                                     ") }
		x<-rbind(x, "    # values/priors of Lambda                                        ")
		x<-rbind(x, make.str( "Lambda" ) ); invisible(moveTo.par.env("Lambda",env,par.env))
		x<-rbind(x, "                                                                     ")		
		# x<-rbind(x, "    # standard deviation of latent scale                             ")
		# x<-rbind(x, "    #sd.eta <- sd(eta[])                                             ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### CONTINUOUS TIME MODEL ###                                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over persons j                                            ")
		x<-rbind(x, "    for (j in 1:J) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # first time point                                       ")
		x<-rbind(x, "            theta[j,1:F,1] ~ dmnorm( mu.t1, prec.t1 )                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # loop over t=2,...,Tj personal time point               ")
		x<-rbind(x, "            for (t in 2:Tj[j]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    theta[j,1:F,t] ~ dmnorm( At[,,t-1,Lpat.group[j]] %*% theta[j,,t-1] + bt[,t-1,Lpat.group[j]], Qt.prec[1:2,1:2,t,Lpat.group[j]] ) ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## values/prior of parameters                                    ")
		x<-rbind(x, "    ## Note: values are commented (already set in R)                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of mean of first time point                       ")
		x<-rbind(x, make.str( "mu.t1" ) ); invisible(moveTo.par.env("mu.t1",env,par.env))
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of precision of first time point                  ")
		x<-rbind(x, ifelse( exists("prec.t1.prior") && is.null( dim(prec.t1.prior) ), paste0( "    prec.t1 ~ ", prec.t1.prior, "                                      " ), make.str( "prec.t1" ) ) ); invisible(moveTo.par.env("prec.t1",env,par.env))
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of drift matrix                                   ")
		x<-rbind(x, make.str( "A" ) ); invisible(moveTo.par.env("A",env,par.env))
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "    # values/prior of diffusion matrix                               ")
		x<-rbind(x, make.str( "Q" ) ); invisible(moveTo.par.env("Q",env,par.env))
		x<-rbind(x, "                                                                     ")	
		x<-rbind(x, "    # values/prior of continuous time intercepts                     ")
		x<-rbind(x, make.str( "b" ) ); invisible(moveTo.par.env("b",env,par.env))
		x<-rbind(x, "                                                                     ")			
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # rowQ: stack Q row-wise into a column vector                    ")
		x<-rbind(x, "    for ( z in (1:Aw) ) {                                            ")
		x<-rbind(x, "        for ( c in (1:Aw) ) {                                        ")
		x<-rbind(x, "            rowQ[ c+Aw*(z-1), 1 ] <- Q[z,c]                          ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## discrete/continuous time relations                            ")
		x<-rbind(x, "    # Qtv for first time point                                       ")
		x<-rbind(x, "    # for technical reasons, same values are set for all lag patterns (not necessary)  ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            Qtv[1:4,1,p] <- ( -1 * Ah.inv[,] ) %*% rowQ[,1]          ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # loop over p=1,...,P lag patterns                               ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            # loop over t=2,...,Tp pattern-specific time point       ")
		x<-rbind(x, "            for (t in 2:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # autoregression matrix / drift matrix           ")
		x<-rbind(x, "                    At[1:2,1:2,t-1,p] <- mexp( A[,] * Lpat[p,t-1] )  ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # intercepts                                     ")
		x<-rbind(x, "                    bt[1:2,t-1,p] <- ( A.inv[,] %*% ( mexp( A[,] * Lpat[p,t-1] ) - I1 ) ) %*% b[,1]         ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qtv is vectorized Qt matrix                    ")
		x<-rbind(x, "                    #Qtv[1:4,t,p] <- ifelse( t==1, ( -1 * Ah.inv[,] ) %*% rowQ[,1] , ( Ah.inv[,] %*% ( mexp( Ah[,] * Lpat[p,t-1] ) - I2 ) ) %*% rowQ[,1] )     ")
		x<-rbind(x, "                    Qtv[1:4,t,p] <- Ah.inv[,] %*% ( mexp( Ah[,] * Lpat[p,t-1] ) - I2 ) %*% rowQ[,1]     ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## calculate Qt.prec                                             ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            for (t in 1:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qt matrix (create with 'irow')                 ")
		x<-rbind(x, "                    for ( z in (1:Aw) ) {                            ")
		x<-rbind(x, "                        for ( c in (1:Aw) ) {                        ")
		x<-rbind(x, "                            Qt[z,c,t,p] <- Qtv[ c+Aw*(z-1),t,p]      ")
		x<-rbind(x, "                        }                                            ")
		x<-rbind(x, "                    }                                                ")
		x<-rbind(x, "                                                                     ")
		# inverse of Qt; Qt has dimensions of Q; Qt.replace must exist
		x<-rbind(x, make.inverse.2( "Qt", dim(Q)[1] ) )
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		# inverse of A and Ah
		x<-rbind(x, make.inverse.1( "A", dim(A)[1] ) )
		x<-rbind(x, make.inverse.1( "Ah", dim(A)[1]^2 ) )
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # Kronecker of A and I1                                          ")
		x<-rbind(x, "    # https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/17ddb2ce/     ")
		x<-rbind(x, "    for (i in 1:Aw){              # Aw = width of A                  ")
		x<-rbind(x, "        for (j in 1:Aw){                                             ")
		x<-rbind(x, "            for (k in 1:I1w){     # I1w = width of I1                ")
		x<-rbind(x, "                for (l in 1:I1w){                                    ")
		x<-rbind(x, "                     Ah1[(i-1)*I1w+k,(j-1)*I1w+l] <- A[i,j]*I1[k,l]  ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Kronecker of I1 and A                                          ")
		x<-rbind(x, "    for (i in 1:I1w){             # I1w = width of I1                ")
		x<-rbind(x, "        for (j in 1:I1w){                                            ")
		x<-rbind(x, "            for (k in 1:Aw){      # Aw = width of A                  ")
		x<-rbind(x, "                for (l in 1:Aw){                                     ")
		x<-rbind(x, "                     Ah2[(i-1)*Aw+k,(j-1)*Aw+l] <- I1[i,j]*A[k,l]    ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Ah matrix                                                      ")
		x<-rbind(x, "    Ah[1:(I1w*Aw),1:(I1w*Aw)] <- Ah1[,] + Ah2[,]                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "}                                                                    ")
		# call matrix (1 column)
		y<-matrix( paste0( "### R syntax for ", model.name ), 1, 1 )
		y<-rbind(y, "" )
		y<-rbind(y, "# rjags package" )
		y<-rbind(y, "require( rjags )" )
		y<-rbind(y, "" )
		y<-rbind(y, "# JAGS Modules" )
		y<-rbind(y, "load.module('msm') # for matrix exponential, mexp()" )
		y<-rbind(y, "load.module('glm') # for better glm sampler        " )
		y<-rbind(y, "" )
		y<-rbind(y, "# start time                                                         ")
		y<-rbind(y, "start <- Sys.time()                                                  ")
		y<-rbind(y, "" )
		y<-rbind(y, "# initialization/adaptation                                          ")
		# globalenv !!! 
		# y<-rbind(y, "eval(parse(text=paste0(  name, ".ini <- jags.model ( file = mf , data=globalenv(), inits=sL, n.chains = ",chains,", n.adapt=",adapt,", quiet=FALSE )"  )),envir=globalenv() )" )
		# y<-rbind(y, "eval(parse(text=paste0(  name, ".ini <- jags.model ( file = mf , data=globalenv(), n.chains = ",chains,", n.adapt=",adapt,", quiet=FALSE )"  )),envir=globalenv() )" )
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .ini <- jags.model ( file = bugs.file , data=data.env, n.chains=chains, n.adapt=adapt, quiet=FALSE ) )),envir=data.env )" )
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .ini <- jags.model ( file = bugs.file , data=data.env, n.chains=chains, n.adapt=adapt, quiet=FALSE ) )),envir=globalenv())" )
		y<-rbind(y, "ini <- jags.model ( file = bugs.file , data=data.env, n.chains=chains, n.adapt=adapt, quiet=FALSE )" )
		y<-rbind(y, "" )
		y<-rbind(y, "# run                                                                ")		
		# y<-rbind(y, "eval(parse(text=paste0(  model.name, .res <- jags.samples ( ',model.name,'.ini , variable.names=c('A','Q','b'), n.iter=iter, thin=thin, type='trace' , progress.bar = 'none', by=20 )' )), envir=globalenv() )" )
		y<-rbind(y, "res <- jags.samples ( ini, variable.names=c('A','Q','b'), n.iter=iter, thin=thin, type='trace' , progress.bar = 'text', by=20 )" )
		y<-rbind(y, "" )
		y<-rbind(y, "# run time                                                           ")		
		y<-rbind(y, "runtime <- Sys.time() - start                                        ")		
		
		### (over)write relevant variables to environment ###
		# jags.syntax <- x
		# obj <- c( "jags.syntax" )
		# eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )
		
		## create return object
		ret <- list()
		# first entry: engine
		ret$engine <- engine
		# second entry: model.name
		ret$model.name <- model.name
		# third entry: data environment
		ret$data.env <- env
		# fourth entry: environment with original matrices
		ret$par.env <- par.env
		# fifth entry: call
		ret$call <- y
		# sixth entry: syntax
		ret$syntax <- x
		
		
		# return
		return( ret )
}

make.str <- function( y.name ) {
		
		# environment where objects are
		env <- parent.frame()
		
		# dimensions of structure, get from parent env
		dim.y <- eval( parse( text=paste0( "dim(", y.name, ")" ) ), envir=env )

		
		# if structure without dimensions (e.g. vector)
		if( is.null( dim.y ) ) {
				
				# long structure
				y. <- data.frame( matrix( seq( along= eval( parse( text=paste0( "", y.name, "" ) ), envir=env ) ), ncol=1 ) )
				
		} else {
		# if structure with dimensions (e.g. matrix)
		
				# long structure
				y. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.y ), collapse="," ), "),accumulate=FALSE )" )))
				
		}
		y.$par <- apply( y., 1, function ( z ) eval( parse( text= paste0( y.name,"[", paste(z,collapse=","), "]" ) ), envir=env ) )
# browser()
		# tag duplicated free parameters and generate values to set duplicated
		y.$dupl <- as.integer( duplicated(y.$par) & is.na(suppressWarnings(as.numeric(y.$par)))  )
		dupl <- unique( y.$par[ duplicated( y.$par ) & is.na(suppressWarnings(as.numeric(y.$par))) ] )
		if ( length( dupl ) > 0 ) {
				d.l <- sapply( dupl, function( d ){
						y.1 <- y.[ y.$par %in% dupl, ]
						y.1 <- y.1[ !duplicated( y.1$par ), ]
						y.1$val <- apply( y.1, 1, function ( z ) paste0( y.name, "", "[", paste( z[-c(length(z)-1,length(z))], collapse=","), "]" ) )
						return( y.1 )
				}, simplify=FALSE )
				d <- do.call( "rbind", d.l )
				y.cn <- colnames( y. )
				y.$order <- seq(along=rownames(y.))
				y. <- merge( y., d[,c("par","val")], by="par", sort=FALSE, all.x=TRUE )
				y. <- y.[ order( y.$order), ]
				y.$order <- NULL
				y. <- y.[ , c( y.cn, "val" ), drop=FALSE ]
		} else {
				y.$val <- ""
		}
	
		
		# loop over long structure, generate strings
		make.str2 <- function( z ) {
# browser()
				dupl <- z[ length(z)-1 ]
				val <- z[ length(z) ]
				z <- z[ -c(length(z)-2,length(z)-1,length(z)) ]
				
				if ( dupl %in% "0" ){
# browser()				
						# name of parameter
						nam <- paste0( y.name, "[", paste( z, collapse=","), "]" )
			
						# determine if fixed value or freely estimated
						as.num <- suppressWarnings( as.numeric( eval( parse( text=nam ), envir=env ) ) )
						is.fixed <- !is.na( as.num )
		# browser()				
						# operator: <- or ~
						op <- ifelse( is.fixed, " <- ", " ~ " )
						# if fixed, uncomment in jags syntax, because already set in R
						# com <- ifelse( is.fixed, "# ", "" )
						com <- ""
						
						# value
						val <- ifelse( is.fixed, as.character( as.num ), eval( parse( text=paste0( y.name, ".prior", "[", paste( z, collapse=","), "]" ) ), envir=env ) )
						
						# string
						s <- paste0( "    ", com, nam, op, val )
				
				} else {
						
						s <- paste0( "    ",y.name, "", "[", paste( z, collapse=","), "] <- ", val )
						
				}
				
				# return
				return( s )
		}
# browser()
		# s <- apply( y.1[,-ncol(y.1),drop=FALSE], 1, make.str2 )
		s <- apply( y., 1, make.str2 )

		# append some white space (for optical reasons)
		s <- sapply( s, function( x ) paste0( x, paste( rep( " ", 69 - nchar(x) ), collapse="" ) ) )
		
		# as column vector
		s <- matrix( s, ncol=1 )
		
		# return
		return( s )

}

moveTo.par.env <- function( name, env, par.env ){
		if( exists(name,envir=env) ){
				assign( name, get( name, envir=env ), envir=par.env )
				rm( list=name, envir=env )
		}
		TRUE
}


make.inverse.1 <- function( name, dim ){
		
		x<-matrix( "", 1, 1 )
		
		if (dim %in% 2) {
				x<-rbind(x, paste0( "    ## inverse of ",name,"                                           " ) )
				x<-rbind(x, paste0( "    # determinant of ",name,"                                        " ) )
				x<-rbind(x, paste0( "    ",name,".det <- ",name,"[1,1]*",name,"[2,2] - ",name,"[2,1]*",name,"[1,2] " ) )
				x<-rbind(x, paste0( "    # ",name," is invertible if determinant is not 0                          " ) )
				x<-rbind(x, paste0( "    ",name,".invertible <- ifelse( ",name,".det==0, 0, 1 )           " ) )
				x<-rbind(x, paste0( "    # if not invertible, modest mod of determinant                   " ) )
				x<-rbind(x, paste0( "    ",name,".detmod1 ~ dbern(0.5)                                    " ) )
				x<-rbind(x, paste0( "    ",name,".detmod2 <- ifelse( ",name,".invertible==1, 0, (",name,".detmod1-0.5)/500 )     " ) )
				x<-rbind(x, paste0( "    ",name,".det.adj <- ",name,".det + ",name,".detmod2                                     " ) )
				x<-rbind(x, paste0( "    # inverse of ",name,"                                                          " ) )
				x<-rbind(x, paste0( "    ",name,".help[1,1] <- ",name,"[2,2]                                            " ) )
				x<-rbind(x, paste0( "    ",name,".help[2,2] <- ",name,"[1,1]                                            " ) )
				x<-rbind(x, paste0( "    ",name,".help[1,2] <- -1*",name,"[1,2]                                         " ) )
				x<-rbind(x, paste0( "    ",name,".help[2,1] <- -1*",name,"[2,1]                                         " ) )
				x<-rbind(x, paste0( "    ",name,".inv[1:Aw,1:Aw] <- (1/",name,".det.adj * ",name,".help[,])             " ) )
		}
		if (dim %in% 4) {
				x<-rbind(x, paste0( "    ## inverse of ",name,"                                                                                                                                                                                                                                                                    ") )
				x<-rbind(x, paste0( "    # http://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix                                                                                                                                                                                                                       ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,1] <- ",name,"[2,2]*",name,"[3,3]*",name,"[4,4]-",name,"[2,2]*",name,"[3,4]*",name,"[4,3]-",name,"[3,2]*",name,"[2,3]*",name,"[4,4]+",name,"[3,2]*",name,"[2,4]*",name,"[4,3]+",name,"[4,2]*",name,"[2,3]*",name,"[3,4]-",name,"[4,2]*",name,"[2,4]*",name,"[3,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,1] <- -",name,"[2,1]*",name,"[3,3]*",name,"[4,4]+",name,"[2,1]*",name,"[3,4]*",name,"[4,3]+",name,"[3,1]*",name,"[2,3]*",name,"[4,4]-",name,"[3,1]*",name,"[2,4]*",name,"[4,3]-",name,"[4,1]*",name,"[2,3]*",name,"[3,4]+",name,"[4,1]*",name,"[2,4]*",name,"[3,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,1] <- ",name,"[2,1]*",name,"[3,2]*",name,"[4,4]-",name,"[2,1]*",name,"[3,4]*",name,"[4,2]-",name,"[3,1]*",name,"[2,2]*",name,"[4,4]+",name,"[3,1]*",name,"[2,4]*",name,"[4,2]+",name,"[4,1]*",name,"[2,2]*",name,"[3,4]-",name,"[4,1]*",name,"[2,4]*",name,"[3,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,1] <- -",name,"[2,1]*",name,"[3,2]*",name,"[4,3]+",name,"[2,1]*",name,"[3,3]*",name,"[4,2]+",name,"[3,1]*",name,"[2,2]*",name,"[4,3]-",name,"[3,1]*",name,"[2,3]*",name,"[4,2]-",name,"[4,1]*",name,"[2,2]*",name,"[3,3]+",name,"[4,1]*",name,"[2,3]*",name,"[3,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,2] <- -",name,"[1,2]*",name,"[3,3]*",name,"[4,4]+",name,"[1,2]*",name,"[3,4]*",name,"[4,3]+",name,"[3,2]*",name,"[1,3]*",name,"[4,4]-",name,"[3,2]*",name,"[1,4]*",name,"[4,3]-",name,"[4,2]*",name,"[1,3]*",name,"[3,4]+",name,"[4,2]*",name,"[1,4]*",name,"[3,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,2] <- ",name,"[1,1]*",name,"[3,3]*",name,"[4,4]-",name,"[1,1]*",name,"[3,4]*",name,"[4,3]-",name,"[3,1]*",name,"[1,3]*",name,"[4,4]+",name,"[3,1]*",name,"[1,4]*",name,"[4,3]+",name,"[4,1]*",name,"[1,3]*",name,"[3,4]-",name,"[4,1]*",name,"[1,4]*",name,"[3,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,2] <- -",name,"[1,1]*",name,"[3,2]*",name,"[4,4]+",name,"[1,1]*",name,"[3,4]*",name,"[4,2]+",name,"[3,1]*",name,"[1,2]*",name,"[4,4]-",name,"[3,1]*",name,"[1,4]*",name,"[4,2]-",name,"[4,1]*",name,"[1,2]*",name,"[3,4]+",name,"[4,1]*",name,"[1,4]*",name,"[3,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,2] <- ",name,"[1,1]*",name,"[3,2]*",name,"[4,3]-",name,"[1,1]*",name,"[3,3]*",name,"[4,2]-",name,"[3,1]*",name,"[1,2]*",name,"[4,3]+",name,"[3,1]*",name,"[1,3]*",name,"[4,2]+",name,"[4,1]*",name,"[1,2]*",name,"[3,3]-",name,"[4,1]*",name,"[1,3]*",name,"[3,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,3] <- ",name,"[1,2]*",name,"[2,3]*",name,"[4,4]-",name,"[1,2]*",name,"[2,4]*",name,"[4,3]-",name,"[2,2]*",name,"[1,3]*",name,"[4,4]+",name,"[2,2]*",name,"[1,4]*",name,"[4,3]+",name,"[4,2]*",name,"[1,3]*",name,"[2,4]-",name,"[4,2]*",name,"[1,4]*",name,"[2,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,3] <- -",name,"[1,1]*",name,"[2,3]*",name,"[4,4]+",name,"[1,1]*",name,"[2,4]*",name,"[4,3]+",name,"[2,1]*",name,"[1,3]*",name,"[4,4]-",name,"[2,1]*",name,"[1,4]*",name,"[4,3]-",name,"[4,1]*",name,"[1,3]*",name,"[2,4]+",name,"[4,1]*",name,"[1,4]*",name,"[2,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,3] <- ",name,"[1,1]*",name,"[2,2]*",name,"[4,4]-",name,"[1,1]*",name,"[2,4]*",name,"[4,2]-",name,"[2,1]*",name,"[1,2]*",name,"[4,4]+",name,"[2,1]*",name,"[1,4]*",name,"[4,2]+",name,"[4,1]*",name,"[1,2]*",name,"[2,4]-",name,"[4,1]*",name,"[1,4]*",name,"[2,2]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,3] <- -",name,"[1,1]*",name,"[2,2]*",name,"[4,3]+",name,"[1,1]*",name,"[2,3]*",name,"[4,2]+",name,"[2,1]*",name,"[1,2]*",name,"[4,3]-",name,"[2,1]*",name,"[1,3]*",name,"[4,2]-",name,"[4,1]*",name,"[1,2]*",name,"[2,3]+",name,"[4,1]*",name,"[1,3]*",name,"[2,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[1,4] <- -",name,"[1,2]*",name,"[2,3]*",name,"[3,4]+",name,"[1,2]*",name,"[2,4]*",name,"[3,3]+",name,"[2,2]*",name,"[1,3]*",name,"[3,4]-",name,"[2,2]*",name,"[1,4]*",name,"[3,3]-",name,"[3,2]*",name,"[1,3]*",name,"[2,4]+",name,"[3,2]*",name,"[1,4]*",name,"[2,3]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[2,4] <- ",name,"[1,1]*",name,"[2,3]*",name,"[3,4]-",name,"[1,1]*",name,"[2,4]*",name,"[3,3]-",name,"[2,1]*",name,"[1,3]*",name,"[3,4]+",name,"[2,1]*",name,"[1,4]*",name,"[3,3]+",name,"[3,1]*",name,"[1,3]*",name,"[2,4]-",name,"[3,1]*",name,"[1,4]*",name,"[2,3]     ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[3,4] <- -",name,"[1,1]*",name,"[2,2]*",name,"[3,4]+",name,"[1,1]*",name,"[2,4]*",name,"[3,2]+",name,"[2,1]*",name,"[1,2]*",name,"[3,4]-",name,"[2,1]*",name,"[1,4]*",name,"[3,2]-",name,"[3,1]*",name,"[1,2]*",name,"[2,4]+",name,"[3,1]*",name,"[1,4]*",name,"[2,2]    ") )
				x<-rbind(x, paste0( "    ",name,".inv.help[4,4] <- ",name,"[1,1]*",name,"[2,2]*",name,"[3,3]-",name,"[1,1]*",name,"[2,3]*",name,"[3,2]-",name,"[2,1]*",name,"[1,2]*",name,"[3,3]+",name,"[2,1]*",name,"[1,3]*",name,"[3,2]+",name,"[3,1]*",name,"[1,2]*",name,"[2,3]-",name,"[3,1]*",name,"[1,3]*",name,"[2,2]     ") )
				x<-rbind(x, paste0( "    ",name,".det <- ",name,"[1,1]*",name,".inv.help[1,1]+",name,"[1,2]*",name,".inv.help[2,1]+",name,"[1,3]*",name,".inv.help[3,1]+",name,"[1,4]*",name,".inv.help[4,1]                                                                                                                       ") )
				x<-rbind(x, paste0( "    # ",name," is invertible if determinant is not 0                                   ") )
				x<-rbind(x, paste0( "    ",name,".invertible <- ifelse( ",name,".det==0, 0, 1 )                             ") )
				x<-rbind(x, paste0( "    # if not invertible, modest mod of determinant                                     ") )
				x<-rbind(x, paste0( "    ",name,".detmod1 ~ dbern(0.5)                                                      ") )
				x<-rbind(x, paste0( "    ",name,".detmod2 <- ifelse( A.invertible==1, 0, (",name,".detmod1-0.5)/500 )       ") )
				x<-rbind(x, paste0( "    ",name,".det.adj <- ",name,".det + ",name,".detmod2                                ") )
				x<-rbind(x, paste0( "    ",name,".inv <- ",name,".inv.help / ",name,".det.adj                               ") )
		}
		
		x<-rbind(x,"")
		return( x )
}

make.inverse.2 <- function( name, dim ){
		
		### *.prec.replace must exist
		
		x<-matrix( "", 1, 1 )
		
		if (dim %in% 2) {
				if (name %in% "Qt"){
						x<-rbind(x, "                    ## inverse of Qt                                 ")
						x<-rbind(x, "                    # determinant of Qt                              ")
						x<-rbind(x, "                    Qt.det[t,p] <- Qt[1,1,t,p]*Qt[2,2,t,p] - Qt[2,1,t,p]*Qt[1,2,t,p]     ")
						x<-rbind(x, "                    # Qt is invertible if determinant is not 0       ")
						x<-rbind(x, "                    Qt.invertible[t,p] <- ifelse( Qt.det[t,p]==0, 0, 1 )     ")
						x<-rbind(x, "                    # if not invertible, modest mod of determinant   ")
						x<-rbind(x, "                    Qt.detmod1[t,p] ~ dbern(0.5)                        ")
						x<-rbind(x, "                    Qt.detmod2[t,p] <- ifelse( Qt.invertible[t,p]==1, 0, (Qt.detmod1[t,p]-0.5)/500 )     ")
						x<-rbind(x, "                    Qt.det.adj[t,p] <- Qt.det[t,p] + Qt.detmod2[t,p]    ")
						x<-rbind(x, "                    # inverse of Qt                                  ")
						x<-rbind(x, "                    Qt.help[1,1,t,p] <- Qt[2,2,t,p]                  ")
						x<-rbind(x, "                    Qt.help[2,2,t,p] <- Qt[1,1,t,p]                  ")
						x<-rbind(x, "                    Qt.help[1,2,t,p] <- -1*Qt[1,2,t,p]               ")
						x<-rbind(x, "                    Qt.help[2,1,t,p] <- -1*Qt[2,1,t,p]               ")
						x<-rbind(x, "                    Qt.prec.orig[1:Aw,1:Aw,t,p] <- (1/Qt.det.adj[t,p] * Qt.help[,,t,p])     ")
						x<-rbind(x, "                                                                     ")
						x<-rbind(x, "                    # check if Qt.prec.orig is positive definite          ")
						x<-rbind(x, "                    Qt.prec.orig.det[t,p] <- Qt.prec.orig[1,1,t,p]*Qt.prec.orig[2,2,t,p] - Qt.prec.orig[2,1,t,p]*Qt.prec.orig[1,2,t,p]     ")
						x<-rbind(x, "                    Qt.prec.orig.posdef[t,p] <- ifelse(  Qt.prec.orig[1,1,t,p] > 0 && Qt.prec.orig.det[t,p] > 0, 1, 0 )     ")
						x<-rbind(x, "                                                                     ")
						x<-rbind(x, "                    # if Qt.prec.orig is not pos def then replace by a pos def matrix     ")
						x<-rbind(x, "                    Qt.prec[1:2,1:2,t,p] <- ifelse( Qt.prec.orig.posdef[t,p]==1, Qt.prec.orig[,,t,p] , Qt.prec.replace[,]  )     ")
				}
		}
		
		x<-rbind(x,"")
		return( x )
}

