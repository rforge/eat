
log2init <- function ( log.path , out.path = dirname(log.path) , iteration = c("highestLikelihood","last","first") , out.files.suffix = "_INIT" ) {
		
		# kompletter Log-File
		tried <- try ( l <- readLines( log.path ) , silent = TRUE )
		if ( inherits ( tried , "try-error" ) ) stop ( paste ( "could not open file" , log.path ) )

		# wenn es ne connection war, dann für später path ziehen
		if ( !is.character ( log.path ) ) log.path <- summary(log.path)$description
		
		# Indikator für Anfang und Ende von Iteration
		itind <- which ( grepl ( "Iteration =" , l ) )
		enditind <- which ( grepl ( "Maximum change in item parameters was" , l ) )

		# Checks
		check1 <- length(itind) == length(enditind)
		check2 <- all ( itind < enditind )
		check3 <- all ( (enditind-1)-(itind+1) >= 0 )
		check4 <- if ( identical ( as.numeric(sub ( "^([^=]+)=(.*)$" , "\\2" , l[itind] )) , seq ( along = itind ) ) ) FALSE else TRUE
		check5 <- ! identical ( itind , integer(0) )
		check6 <- ! identical ( enditind , integer(0) )
		
		if ( ! all ( check1 , check2 , check3 , check4 , check5 , check6 ) ) {
				warning ( paste ( log.path , "is not as expected. please check. no output is generated" ) )
		} else {
		
				# Iterations
				niter <- length(itind)
				
				# highest Likelihood in iteration?
				# entweder bei komplett durchgelaufenen wirds angegeben
				# ansonsten danach suchen
				# wenn alles fehlschlägt dann die letzte Iteration
				if ( any ( w <- grepl ( "Convergence has occurred but a solution with a higher likelihood was encountered at iteration" , l ) ) ) {
						hl <- as.numeric ( sub ( "^(Convergence has occurred but a solution with a higher likelihood was encountered at iteration\\s+)(\\d+)(\\s.*)$" , "\\2" , l[which(w)] ) )
				} else if ( any ( w <- grepl ( "Deviance=" , l ) ) ) {
						dev <- l[which(w)]
						dev <- na.omit ( as.numeric ( sub ( "^(\\s*Deviance=\\s*)(\\d+\\.\\d+)\\s+.*$" , "\\2" , dev ) ) )
						if ( length (dev) == length ( itind ) ) {
								hl <- which ( dev == min ( dev ) )
						} else hl <- niter
				} else hl <- niter
				
				# welche Iteration soll genommen werden?
				if ( is.character ( iteration ) ) {
						
						it.valid <- c("highestLikelihood","last","first")
						if ( length ( iteration ) > 1 ) iteration <- iteration[1]
						if ( ! iteration %in% it.valid ) stop ("option 'iteration' is invalid")
						iteration <- switch ( which ( iteration %in% it.valid ) , 
													 hl ,
													 niter ,
													 1 )

				} else if ( is.numeric ( iteration ) ) {

						if ( ! iteration %in% 1:niter ) {
								warning ( paste ( "option 'iteration' = " , iteration , " is out of range (1-" , niter , ")\niteration='highestLikelihood' is used instead." , sep = "" ) )
								iteration <- hl
						}
						
				} else stop ("option 'iteration' is invalid")
				
				stopifnot ( is.numeric ( iteration ) )
				stopifnot ( iteration %in% 1:niter )
				
				# parameter holen
				p <- l[(itind[iteration]+1):(enditind[iteration]-1)]
				
				### Covariance
				covstart <- which ( grepl ( "Covariance" , p ) )
				covend <-  which ( grepl ( "Correlation" , p ) )
				if ( ! identical ( covstart , integer(0) ) &
					 ! identical ( covend , integer(0) ) & 
					 (s <- covstart +1) <= (e <- covend -1) ) {
							clines <- p [s:e]
							nrows <- length(clines)
							covs <- na.omit ( as.numeric ( unlist ( strsplit ( clines , "\\s+" ) ) ) )
							if ( ! length(covs) %% nrows == 0 ) {
									cov <- NULL
							} else {
									cov <- matrix ( covs , nrow = nrows )
									if ( ! nrow ( cov ) == ncol ( cov ) ) {
											cov <- NULL
									} else {
											cov <- mapply ( function ( sp , ind ) sp[ind:length(sp)] ,
													data.frame(cov) , 1:nrows )

											cov <- unname ( unlist ( mapply ( function ( sp , x ) {
													mapply ( function ( el , y , x ) {
															paste ( "" , x , y , formatC(el,format = "f" ,digits=5) , sep = "         " )
													} , sp , (x):(length(sp)+x-1) , MoreArgs = list ( x ) )
											} , cov , 1:nrows , SIMPLIFY = FALSE ) ) )
											
									}
							}
				} else {
						cov <- NULL
				}
				if ( is.null ( cov ) ) warning ( paste ( "could not extract covariance matrix from" , log.path ) )
		
				### Regression
				regstart <- which ( grepl ( "Regression Coefficients" , p ) )
				regend <-  which ( grepl ( "Item Parameters" , p ) )
				if ( ! identical ( regstart , integer(0) ) &
					 ! identical ( regend , integer(0) ) & 
					 (s <- regstart +1) <= (e <- regend -1) ) {
							rlines <- p [s:e]
							regs <- na.omit ( as.numeric ( unlist ( strsplit ( rlines , "\\s+" ) ) ) )

							# Anzahl Reg-Vars
							vind <- which ( grepl ( "=>\\s*regression" , l ) )
							if ( ! identical ( vind , integer(0) ) & length(vind)==1 ) {
									vars <- l[vind]
									vars <- sub ( "^([^=]*=>\\s*regression\\s+)(.*)$" , "\\2" , vars )
									nvar <- length( unlist ( strsplit ( vars , "\\s+" ) ) ) + 1
							} else nvar <- NULL
							
							# Checks
							check1 <- length(regs) >= 1
							check2 <- !is.null(nvar)
							if ( check2 ) check3 <- length(regs) %% nvar == 0 else check3 <- FALSE
							
							if (  ! all ( check1 , check2 , check3 ) ) {
									reg <- NULL
							} else {
									# nach Anzahl Variablen splitten
									s <- sapply ( 1:(length(regs)/nvar) , function ( el , mult ) { el*mult-nvar+1 } , nvar )
									e <- sapply ( 1:(length(regs)/nvar) , function ( el , mult ) { el*mult } , nvar ) 
									se <- mapply ( function ( s , e ) s:e , s , e , SIMPLIFY = FALSE ) 
									
									se <- mapply ( function ( se , regs ) regs[se] , se , MoreArgs = list ( regs ) , SIMPLIFY = FALSE )
									
									reg <- unname ( unlist ( mapply ( function ( sp , x ) {
													mapply ( function ( el , y , x ) {
															paste ( "" , x , y , formatC(el,format = "f" ,digits=5) , sep = "         " )
													} , sp , 0:(length(sp)-1) , MoreArgs = list ( x ) )
											} , se , 1:length(se) , SIMPLIFY = FALSE ) ) )
							}
				} else {
						reg <- NULL
				}
				if ( is.null ( reg ) ) warning ( paste ( "could not extract regression coefficients from" , log.path ) )				
				
				### Item Parameters
				istart <- which ( grepl ( "Item Parameters" , p ) )
				iend <-  length(p)
				if ( ! identical ( istart , integer(0) ) &
					 ! identical ( iend , integer(0) ) & 
					 (s <- istart +1) <= (e <- iend ) ) {
							items <- p [s:e]
				}
					
				### out files
				
				bn <- basename ( log.path )
				# gepackte Extension abcutten falls nötig
				cutoff <- c ("bz2","zip")
				bn <- gsub ( paste ( paste("\\." , cutoff , "$" , sep ="") , collapse = "|" ) , "" , bn )
				# "log" abcutten	
				cutoff <- c ("log")
				bn <- gsub ( paste ( paste("\\." , cutoff , "$" , sep ="") , collapse = "|" ) , "" , bn )
				
				# Outfiles bauen
				if ( ! file.exists ( out.path ) ) dir.create ( out.path , recursive = FALSE )
				els <- c("cov","reg","prm")
				do <- paste ( els , '.out <- file.path ( out.path , paste ( bn , out.files.suffix , ".', els , '" , sep = "" ) );' , sep = "")
				eval ( parse ( text = do ) )
				
				# rausschreiben
				temp <- mapply ( function ( w , f ) {
						if ( !is.null ( w ) ) write ( w , f )
				} , list ( cov , reg , items ) , list ( cov.out, reg.out, prm.out ) )
				
		}

}
