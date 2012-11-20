
plotDevianceChange <- function ( path , plot = TRUE , pdf = FALSE , out.path = NULL , extreme.crit = 0.75 ) {
		
		# sofwarespezifische Endungen
		software.ext <- signature ( "log" = "conquest" , "out" = "mplus" )
		
		# alle log files von Conquest / out files von Mplus mit readLines einlesen
		lns <- read.txt ( path , "readLines" , names(software.ext) , simplify = FALSE , recursive = TRUE )

		if ( !is.null ( lns ) ) {
		
				# Warnung bei unplausibler Parameter-Kombi
				if ( !plot & pdf ) {
						warning ( "plot=FALSE and pdf=TRUE are not combinable, pdf is set to FALSE" )
						pdf <- FALSE
				}
			
				# software korrespondierend zu lns
				ext <- sub ( "^.*\\.(\\w+)$" , "\\1" , names(lns) )
				software <- unname ( software.ext [ ext ] )
				
				# Deviance holen
				dc <- mapply ( getDevianceChange , lns , software , SIMPLIFY = FALSE )
				
				# outlier entfernen nach extreme.crit
				.fun1 <- function ( dc , extreme.crit ) {
						if ( length(dc)>1 ) {
								thresh <- extreme.crit * sd ( dc )
								keep <- dc<=thresh
								if ( length ( which ( keep ) ) > 0 ) dc <- dc[keep]
						}
				}
				if ( !is.null (extreme.crit) & is.numeric ( extreme.crit) ) dc <- mapply ( .fun1 , dc , MoreArgs = list ( extreme.crit ) , SIMPLIFY = FALSE )
				
				if ( plot ) {
				
						if ( pdf ) {
								# pdf-Namen setzen
								pdfn <- names ( dc )
								pdfn <- sub ( "\\.\\w+$" , ".pdf" , pdfn )
								# names (dc) <- pdfn
								
								# Sammel-pdf
								if ( is.null ( out.path ) ) {
										sammelpdf <- "deviance_change_plots.pdf"
										if ( file.info(path)$isdir ) sammelpdf.dir <- path else sammelpdf.dir <- dirname ( path )
										sammelpdf <- file.path ( sammelpdf.dir , sammelpdf )
								} else {
										sammelpdf <- out.path
								}
								
								# sammelpdf öffnen
								pdf ( file = sammelpdf ,
									  paper = "a4r" , 
									  width = 10.91 , height = 7.48
									)

						} else pdfn <- sapply ( seq ( along = dc ) , function ( i ) NULL , simplify = FALSE )
						
						# basenames ( "Analysename" ) setzen
						bn <- basename ( sub ( "\\.\\w+$" , "" , names ( dc ) ) )
						
						# isconverged
						# currently only ConQuest
						.fun2 <- function ( file.name , software ) {
								if ( software == "conquest" ) {
										shw <- paste ( sub ( "log$" , "" , file.name ) , "shw" , sep = "" )
										if ( !is.null ( ic <- isConverged ( shw ) ) ) ic <- ic$converged else ic <- FALSE
								} else ic <- FALSE
								return (ic)
						}
						ic <- mapply ( .fun2 , names ( dc ) , software , SIMPLIFY = FALSE )						
						
						# Modell-Information
						# currently only ConQuest
						modelinfo <- mapply ( plotDevianceChange.modelinfo , lns , names ( lns ) , software , SIMPLIFY = FALSE )
						
						# plotten
						.fun4 <- function ( dc , pdfn , bn , ic , modelinfo , pdf ) {

								# einmal auf offenes Sammeldevice plotten
								if ( pdf ) plotDevianceChange.plot ( dc=dc, pdf=pdf, bn=bn, ic=ic, modelinfo=modelinfo )
								# jetzt regulär
								plotDevianceChange.plot ( dc=dc, pdf=pdf, pdfn=pdfn, bn=bn, ic=ic, modelinfo=modelinfo )
						}
						temp <- mapply ( .fun4 , dc , pdfn , bn , ic , modelinfo , MoreArgs = list ( pdf ) )
						
						# sammelpdf schließen
						if ( pdf ) invisible ( dev.off ( ) ) else invisible ( TRUE )
				
				} else return ( dc )
		
		} else {
				warning ( "plot could not be created. check input." )
				invisible ( NULL )
		}
		
}

getDevianceChange <- function ( lns , software ) {
		
		if ( software == "conquest" ) {
		
				# DevChange Lines
				d <- which ( grepl ( "Change in the deviance was" , lns ) )
		
				if ( ! identical ( d , integer(0) ) ) {
					
						# Deviance Change extrahieren
						d <- lns[d]
						ret <- as.numeric ( sub ( "^(.*)\\s+(-{0,1}\\d+\\.\\d+)$" , "\\2" , d ) )
						# Namen ist Iterationsnummer
						names ( ret ) <- seq ( along = ret ) + 1
						
				} else ret <- NULL
		
		} else if ( software == "mplus" ) {
		
				####################
				####### TODO #######
				####################
				
				ret <- NULL
				
		} else ret <- NULL

		return ( ret )
}

plotDevianceChange.plot <- function ( dc , pdf , pdfn=NULL , bn , ic , modelinfo ) {
		
		if ( pdf ) {
				if ( !is.null ( pdfn ) ) {
						pdf ( file = pdfn ,
							  paper = "a4r" , 
							  width = 10.91 , height = 7.48
						)
				}
		} else windows( width = 10.91 , height = 7.48 , rescale = "R" )
		
		### plot ###
		xvals <- as.numeric ( names ( dc ) )
		# maximum auf x Achse
		xm <- ceiling( max(xvals)/10 )*10

		# ticks auf x achse
		# so setzen dass schön 10er
		xt <- NULL;	for ( i in c( 1:30 ) ) xt <- c ( xt , (xm/10) %% i == 0 )
		xt <- max ( which ( xt ) )
		
		# Punktgröße setzen (cex)
		# initial: .85
		# mit jeden 100 Iteration 0.1 runter
		# aber mind. 0.4
		cex <- 0.85 - ( length(dc) / 1000 )
		if ( cex < 0.40 ) cex <- 0.40
		
		plot ( xvals , dc ,
			   type = "o" , 
			   main = paste ( "Deviance Change Plot for" , bn ) ,
			   xlab = "Iteration" ,
			   xlim = c(0,max(xvals)) ,
			   xaxp = c(0,xm,xt) ,
			   ylab = "Deviance Change" ,
			   pch = 20 ,
			   cex = cex ,
			   lwd = 0.75
			   )

		# Linie bei 0
		abline ( a=0 , b=0 )

		# Punkte unter 0 rot
		dcr <- dc[dc<0]
		points( as.numeric ( names ( dcr ) ) , dcr , pch=20, cex = cex , col="red")

		# letzter Punkt grün und doppelt so groß wenn Modell ordentlich konvergiert
		if ( ic ) points( as.numeric ( names(dc)[length(dc)] ) , dc[length(dc)] , pch=20, cex = 2*cex , col="green")
		
		# Modell-Informationen
		if ( ! is.null ( modelinfo ) ) mtext( modelinfo )		
		 
		# device ggf. schließen
		if ( !is.null ( pdfn ) ) dev.off()
		
		# Rückgabe
		invisible ( TRUE )
		
}

plotDevianceChange.modelinfo <- function ( lns , file.name , software ) {
	
		if ( software == "conquest" ) {
		
				### Modellinformationen in Plot schreiben
				# aus Conquest
				w <- which ( grepl ( "\\s+=>\\s*estimate\\s*!" , lns ) )
				if ( ! identical ( w , integer(0) ) ) {
						est <- lns[w]
						info <- unlist ( strsplit ( est , "," ) )
						info <- sub ( "\\s+=>\\s*estimate\\s*!\\s*" , "" , info )
						info <- sub ( ";" , "" , info )
						report <- c ("method","nodes","converge","deviancechange")
						info <- info[ unname ( unlist ( sapply ( report , function ( r , info ) which ( grepl ( r , info ) ) , info ) ) )]
				} else info <- character(0)
			
				# Verbrauchte Zeit aus Erstellung des Log und letzter save des Log
				fi <- file.info(file.name)
				td <- unclass ( fi$mtime - fi$ctime )
				if ( !is.na ( td ) ) {
					if ( td > 0 ) {
							tds <- paste ( "minimal elapsed time:" , formatC ( round ( td , 1 ) , format="f", digits=1) , attr ( td , "units" ) )
							info <- c ( info , tds )
					}
				}
				
				info <- paste ( info , collapse = "   " )
				if ( identical ( info , character(0) ) ) info <- NULL
		
		} else info <- NULL
	
		return ( info )
		
}
