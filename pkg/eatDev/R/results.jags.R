
results.jags <- function ( env, mode ) {
# browser()		
		# require packages
		require( "ggplot2" )
		require( "shinystan" )
		require( "coda" )
		
		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# defaults of burnin
		if (!exists("burnin",mode="numeric")) burnin <- 0
		
		# burnin must be lower than iterations
		if ( burnin >= r$runpar$iter ) {
				cat( paste0( "burnin=", burnin, " is not lower than iter=", r$runpar$iter, "; now set to iter/2=", burnin <- floor( r$runpar$iter/2 ), " \n\n" ) )
		}
		
		# output
		if ( verbose ){
				if ( mode %in% "jags" )	engine.str <- "JAGS" else if ( mode %in% "ctstan" )	engine.str <- "Stan" else engine.str <- ""
				cat( paste0( "Extracting results from ",engine.str," object...", "\n" ) )
				cat( paste0( "   burnin: ", burnin, "\n" ) )
				# cat( paste0( "            iterations: ", iter, "\n" ) )
				# cat( paste0( "                chains: ", chains, "\n" ) )
				# cat( paste0( "     thinning interval: ", thin, "\n" ) )
				cat( paste0( "\n" ) )
				flush.console()
		}
		
		# function to extract parameters and calculate statistics
		# y <- data.frame( rep( 1:length(unique(dl[,"id"])), each=n.latent ), rep(1:1, length(unique(dl[,"id"]))) )
		# T0MEANS.strings <- apply( y, 1, function(z) paste0( "T0MEANS[", paste(z,collapse=",")  ,"]" ) )
		# CINT.strings <- apply( y, 1, function(z) paste0( "CINT[", paste(z,collapse=",")  ,"]" ) )
		# pars <- c(T0MEANS.strings,CINT.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",paste0("manifestmeans_Y",1:n.manifest) )
		# pars <- c(T0MEANS.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",paste0("manifestmeans_Y",1:n.manifest) )
		# pars <- c(T0MEANS.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",iM[,1][ grepl( "^m", iM[,1] ) ] )
		# pars <- c(T0MEANS.strings,CINT.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",iM[,1][ grepl( "^m", iM[,1] ) ] )
		# pars <- c(paste0("output_tip_time_on_",c("T0means_eta1","T0means_eta2","cint_eta1","cint_eta2")),T0MEANS.strings,CINT.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",iM[,1][ grepl( "^m", iM[,1] ) ] )
		#pars <- c(T0MEANS.strings,CINT.strings,"lp__","T0mean_eta1","drift_eta1_eta1","diffusion_eta1_eta1","cint_eta1",iM[,1][ grepl( "^m", iM[,1] ) ] )
		# pars <- c("drift_eta1_eta1","diffusion_eta1_eta1",iM[,1][ grepl( "^m", iM[,1] ) ],"lp__","cint_eta1","T0mean_eta1", add.pars )
		# pars <- c("T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2",paste0("manifestmeans_Y",1:n.manifest) )
		# pars <- c(T0MEANS.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2" )
		# pars <- c(T0MEANS.strings,CINT.strings,"T0mean_eta1","T0mean_eta2","drift_eta1_eta1","drift_eta2_eta2","drift_eta1_eta2","drift_eta2_eta1","diffusion_eta1_eta1","diffusion_eta2_eta2","diffusion_eta2_eta1","cint_eta1","cint_eta2" )
		
		#indvarying[1:(length(c(T0MEANS.strings,CINT.strings)))] <- TRUE
		# indvarying[1:length(c(T0MEANS.strings))] <- TRUE
		
		# make list of all parameters
		get.par.list <- function( m, m.name ){
# browser()
				# data structures to long
				dim.m <- dim( m )
				if( is.null( dim.m ) ) {
						
						# long structure
						m. <- data.frame( matrix( seq( along= m ), ncol=1 ) )
						
				} else {
				# if structure with dimensions (e.g. matrix)
				
						# long structure
						m. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.m ), collapse="," ), "),accumulate=FALSE )" )))

				}
				m.$parameter <- apply( m., 1, function ( z ) eval( parse( text= paste0( "m[", paste(z,collapse=","), "]" ) ) ) )
				# NOT duplicated free parameters
				m. <- m.[ !duplicated(m.$parameter) & is.na(suppressWarnings(as.numeric(m.$parameter))), ]
# browser()
				### for ctstan there musn't be . in the parameter names
				if ( mode %in% "ctstan" ) {
						m.$parameter.mod <- gsub( ".", "", m.$parameter, fixed=TRUE )
				} else if ( mode %in% "jags" ) {
						m.$parameter.mod <- m.$parameter
				} else {
						m.$parameter.mod <- m.$parameter
				}				
				
				# name
				m.$name <- m.name

				# call (how to access parameter)
				if ( mode %in% "jags" ) {
						m.$call <- apply( m.[,colnames(m.)[!colnames(m.) %in% c("parameter","parameter.mod","name")],drop=FALSE], 1, function ( z ) paste0( "r$results$'",m.name,"'[", paste(z,collapse=","), ",,]" ) )
				} else if ( mode %in% "ctstan" ) {
						# if ( !indvarying & !par %in% c("lp__") & !grepl("^eta",par) ){
								# iterations/chains fuer parameter
								m.$call <- paste0( "extract( r$results, pars=paste0('output_hmean_', '",m.$parameter.mod,"'), permuted=FALSE, inc_warmup=TRUE )[,,1]" )
						# } else {
								# x <- extract( fit, pars=par, permuted=FALSE, inc_warmup=TRUE )[,,1]
						# }
				}
				
				
				# sort
				m. <- m.[,c("name","parameter","call")]
				
				# return
				return( m. )
		}
# browser()
		pars.l <- mapply( get.par.list, r$parameters, names( r$parameters ), SIMPLIFY=FALSE )
		pars <- do.call( "rbind", pars.l )
		rownames( pars ) <- seq( along=rownames( pars ) )
		
		### for ctstan there musn't be . in the parameter names
		# if ( mode %in% "ctstan" ) {
				# pars$parameter.mod <- gsub( ".", "", pars$parameter, fixed=TRUE )
		# } else if ( mode %in% "jags" ) {
				# pars$parameter.mod <- pars$parameter
		# } else {
				# pars$parameter.mod <- pars$parameter
		# }
		
		# indvarying <- rep( FALSE, length(pars) )
		# est.l <- try( mapply( extr, pars, SIMPLIFY=FALSE ) )
		extr <- function( z ) {

				if ( verbose ) cat( paste0( "   ", z["parameter"], "\n" ) ); flush.console()
				
				# if ( !indvarying & !par %in% c("lp__") & !grepl("^eta",par) ){
					# iterations/chains fuer parameter
					# x <- extract( fit, pars=paste0("output_hmean_", par), permuted=FALSE, inc_warmup=TRUE )[,,1]
				# } else {
					# x <- extract( fit, pars=par, permuted=FALSE, inc_warmup=TRUE )[,,1]
				# }
# browser()
				# get chains of parameter
				# x <- eval( parse( text=paste0( "r$results$",z["call"],"" ) ) )
				x <- eval( parse( text=paste0( z["call"] ) ) )
			
				# wide ( iteration x chain ) to long
				# data set primarily for iteration plot, that's why complete, inclusive burnin
				# multiple chains -> melt
				# single chain -> manual mod
				if( is.null(dim(x)) ) {
						xl <- matrix( NA, ncol=3, nrow=length(x) )
						xl[,1] <- 1:length(x)
						xl[,2] <- 1
						xl[,3] <- x
						xl <- data.frame( xl )
						colnames( xl ) <- c( "iterations", "chains", "value" )
						x <- dcast( xl, iterations ~ chains, value.var="value" )				
				} else {
						xl <- melt( x )
						colnames( xl ) <- c( "iterations", "chains", "value" )
				}
				# chains to factor
				if( is.numeric( xl$chains ) ) xl$chains <- factor( xl$chains, levels=sort(unique(xl$chains)), labels=paste0( "chain:", sort(unique(xl$chains)) ) )
				
				# delete burnin, original wide data
				x2 <- x[((burnin+1):r$runpar$iter),,drop=FALSE]
				
				### delete outlier chains
				# means after burnin
				mab <- apply( x2, 2, mean )
				del <- abs( mab - mean( mab ) ) > 2*sd( mab )
				if ( any ( del ) ) {
						# aus burnin geloeschten Daten rausnehmen
						x2 <- x2[,!del,drop=FALSE]
						chains <- ncol( x2 )
						
						# aus Iterationsplot-Datensatz die geloeschte Kette hinter burnin rausnehmen, damit mans sieht
						xl <- xl[ !( xl$chains %in% names(del)[del] & xl$iterations > burnin), ]
				}
# browser()
				# Iteration-Plots fuer nicht ind varying
				# if (!indvarying) {
						# Iteration-Plots
						# pdf( file.path( folder.plots, paste0( par, ".pdf" ) ) )
						suppressWarnings(
								pl <- ggplot(xl, aes(x=iterations, y=value, colour=chains)) + geom_point(shape=16, size=1.5, alpha=0.50) +
								scale_colour_hue(l=50) + # Use a slightly darker palette than normal
								geom_smooth(method="loess",
										se=FALSE,    # Don't add shaded confidence region
										fullrange=TRUE) + # Extend regression lines
								geom_vline(xintercept = burnin+0.5) +
								theme_bw() +
								theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
						)
						suppressWarnings(
								ggsave( file.path( plot.dir, paste0( z["parameter"], ".png" ) ), pl, width=2*29.7, height=21, units="cm", dpi=300 )
						)
						# dev.off()
				# }
			
				# mcmc-Objekt bauen
				do <- paste0( "as.mcmc.list( list( ", paste( paste0( " as.mcmc( x2[,",1:r$runpar$chains,"] ) " ), collapse="," ) , " ) )" )
				mcmclist <- eval( parse( text=do ) )

				# shinystan-Objekt bauen
				do <- paste0( "as.shinystan( list( ", paste( paste0( "  as.matrix( data.frame( '",z["parameter"],"' = x2[,",1:r$runpar$chains,"] ) ) " ), collapse="," ) , " ) )" )
				sso <- eval( parse( text=do ) )
				
				psrf.coda <- gelman.diag( mcmclist )
				psrf.ss <- retrieve( sso, "Rhat" )
				effN.coda <- effectiveSize( mcmclist )
				effN.ss <- retrieve( sso, "Neff" )
				# stats <- summary( mcmclist )
				relMCSE.ss <- retrieve( sso, "mcse" ) / retrieve( sso, "sd" )
				
				# return data.frame
				# ret <- data.frame( "variable"=z["parameter"], "value"=unname(stats$statistics["Mean"]), "SD"=unname(stats$statistics["SD"]), "Naive SE"=unname(stats$statistics["Naive SE"]), "Time-series SE"=unname(stats$statistics["Time-series SE"]), "psrf.coda"=unname(psrf.coda$psrf[1,1]), "psrf.UL.coda"=unname(psrf.coda$psrf[1,2]), "psrf.ss"=psrf.ss, "effN.coda"=unname(effN.coda), "effN.ss"=effN.ss, "relMCSE"=relMCSE )
# browser()
				ret <- data.frame( "name"=z["name"], "variable"=z["parameter"], "value"=retrieve(sso,"mean"), "sd"=retrieve(sso,"sd"), "mcse.ss"=retrieve( sso, "mcse" ), "relative.mcse.ss"=relMCSE.ss, "psrf.coda"=unname(psrf.coda$psrf[1,1]), "psrf.UL.coda"=unname(psrf.coda$psrf[1,2]), "psrf.ss"=psrf.ss, "effN.coda"=unname(effN.coda), "effN.ss"=effN.ss, "engine"=engine, stringsAsFactors=FALSE )
				
				# if( chains==1 ) { psrf.est <- psrf.UL <-NA } else { psrf <- gelman.diag( mcmclist ); psrf.est <- unname(psrf$psrf[1,1]); psrf.UL <- unname(psrf$psrf[1,2]) }
				# effN <- effectiveSize( mcmclist )
				# stats <- summary( mcmclist )
				
				# return data.frame
				# ret <- data.frame( "variable"=par, "value"=unname(stats$statistics["Mean"]), "SD"=unname(stats$statistics["SD"]), "Naive SE"=unname(stats$statistics["Naive SE"]), "Time-series SE"=unname(stats$statistics["Time-series SE"]), "psrf.est"=psrf.est, "psrf.UL"=psrf.UL, "effN"=unname(effN) )
				
				return( ret )
		}
		est.l <- apply( pars, 1, extr )
		est <- do.call( "rbind", est.l )
		
		# return
		return( est )
}
