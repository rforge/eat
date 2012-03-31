
plotDevianceChange <- function ( path , plot = TRUE , pdf = FALSE , out.path = NULL , extreme.crit = 0.75 , pdftk.path = NULL ) {

		# auf isdir prüfen, dabei connection "abfangen"
		if ( ! inherits ( path , "connection" ) ) isdir <- file.info(path)$isdir else isdir <- FALSE
		
		# wenn Verzeichnis dann rekursiv alle log files rausziehen
		# wenn kein Verzeichnis dann einfach so weiterpassen
		if ( isdir ) {
				f <- list.files(path = path , 
						   pattern = "\\.log$" , all.files = FALSE,
						   full.names = TRUE, recursive = TRUE,
						   ignore.case = FALSE, include.dirs = FALSE)
		} else f <- list ( path )
		
		# Schleife über Change Plot erstellen
		pdfs <- sapply ( f , .plotDevianceChange , plot = plot , pdf = pdf , out.path = out.path , extreme.crit = extreme.crit , simplify = FALSE )
	
		# wenn pdf == TRUE dann einzel pdfs mit pdftk zusammenmergen
		if ( isdir & pdf ) {
		
				# pdftk default
				if ( is.null ( pdftk.path ) ) pdftk.path <- get.file.from.dir(dr=file.path(.Library,"eat/winexe/pdftk"), ext="exe", vers="newest", crit.level="silent" )
				
				# wenn gefunden weiter
				if ( !is.null ( pdftk.path ) ) {
						
						# outpath defaulten
						if ( is.null ( out.path ) ) out.path <- path						
						
						# out.name
						out.name <- "deviance_change_plots.pdf"
						out.path <- file.path ( path , out.name )
						
						# pdftk
						pdftk.str <- paste ( pdftk.path , " " ,
											 paste ( paste ( '"' , pdfs , '"' , sep="" ) , collapse = " " ) ,
											 ' cat output "' ,
											 out.path , '" dont_ask' , sep = "" )
						
						# run
						system(command=pdftk.str, intern = FALSE,
							   ignore.stdout = FALSE, ignore.stderr = FALSE,
							   wait = FALSE, input = NULL, show.output.on.console = FALSE,
							   minimized = TRUE, invisible = TRUE)
						
				}
		}
		
		# return 
		if ( plot ) {
				invisible ( pdfs )
		} else {
				if ( isdir ) return ( pdfs ) else return ( unlist ( pdfs , recursive = FALSE ) )
		}
		
}

.plotDevianceChange <- function ( log.path , plot , pdf , out.path , extreme.crit ) {
	
		# kompletter Log-File
		tried <- try ( l <- readLines( log.path ) , silent = TRUE )
		if ( inherits ( tried , "try-error" ) ) stop ( paste ( "could not open file" , log.path ) )

		# wenn es ne connection war, dann für später path ziehen
		if ( inherits ( log.path , "connection" ) ) {
				log.path.name <- summary(log.path)$description
				close ( log.path )
				log.path <- log.path.name
		}
		
		# DevChange Lines
		d <- which ( grepl ( "Change in the deviance was" , l ) )
		
		if ( ! identical ( d , integer(0) ) ) {
				
				# Warnung bei unplausibler Parameter-Kombi
				if ( !plot & pdf ) warning ( "plot=FALSE and pdf=TRUE are not combinable, pdf is set to FALSE" )

				### basename
				bn <- basename ( log.path )
				# gepackte Extension abcutten falls nötig
				cutoff <- c ("bz2","zip")
				bn <- gsub ( paste ( paste("\\." , cutoff , "$" , sep ="") , collapse = "|" ) , "" , bn )
				# "log" abcutten	
				cutoff <- c ("log")
				bn <- gsub ( paste ( paste("\\." , cutoff , "$" , sep ="") , collapse = "|" ) , "" , bn )	
				
				# Deviance Change extrahieren
				d <- l[d]
				dv <- as.numeric ( sub ( "^(.*)\\s+(-{0,1}\\d+\\.\\d+)$" , "\\2" , d ) )
				# Namen ist Iterationsnummer
				names ( dv ) <- seq ( along = dv ) + 1
				
				# Check
				if ( length ( d ) != length ( dv ) ) stop ( paste ( "could not extract deviance change from" , log.path , " please check." ) )
				
				# outlier
				if ( !is.null (extreme.crit) & length(dv)>1 ) {
						thresh <- extreme.crit * sd ( dv )
						dv <- dv[dv<=thresh]
				}
	
				if ( plot ) {
						
						# wenn pdf erzeugt werden soll, device öffnen
						# sonst neues Konsolen Plot window
						if ( pdf ) {
								
								# outpath defaulten
								if ( is.null ( out.path ) ) out.path <- dirname(log.path)
								if ( ! file.exists ( out.path ) ) dir.create ( out.path , recursive = TRUE )
								
								# file
								pdf.file = file.path ( out.path , paste( bn , ".pdf" , sep = "" ) ) 
								
								#device öffnen
								pdf ( 	file = pdf.file ,
										paper = "a4r" , 
										width = 10.91 , height = 7.48
									)
						} else windows()
						
						### plot ###
						xvals <- as.numeric ( names ( dv ) )
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
						cex <- 0.85 - ( length(dv) / 1000 )
						if ( cex < 0.40 ) cex <- 0.40
						
						plot ( xvals , dv ,
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
						dvr <- dv[dv<0]
						points( as.numeric ( names ( dvr ) ) , dvr , pch=20, cex = cex , col="red")

						# letzter Punkt grün und doppelt so groß wenn Modell ordentlich konvergiert
						shw <- paste ( sub ( "log$" , "" , log.path ) , "shw" , sep = "" )
						if ( !is.null ( ic <- isConverged ( shw ) ) ) ic <- ic$converged else ic <- FALSE
						if ( ic ) points( as.numeric ( names(dv)[length(dv)] ) , dv[length(dv)] , pch=20, cex = 2*cex , col="green")
						
						### Modellinformationen in Plot schreiben
						# aus Conquest
						w <- which ( grepl ( "\\s+=>\\s*estimate\\s*!" , l ) )
						if ( ! identical ( w , integer(0) ) ) {
								est <- l[w]
								inf1 <- unlist ( strsplit ( est , "," ) )
								inf1 <- sub ( "\\s+=>\\s*estimate\\s*!\\s*" , "" , inf1 )
								inf1 <- sub ( ";" , "" , inf1 )
								report <- c ("method","iter","nodes","converge","deviancechange")
								inf1 <- inf1[ unname ( unlist ( sapply ( report , function ( r , inf1 ) which ( grepl ( r , inf1 ) ) , inf1 ) ) )]
						} else inf1 <- character(0)
					
						# Verbrauchte Zeit aus Erstellung des Log und letzter save des Log
						fi <- file.info(log.path)
						td <- unclass ( fi$mtime - fi$ctime )
						if ( td > 0 ) {
								tds <- paste ( "minimal elapsed time:" , formatC ( round ( td , 1 ) , format="f", digits=1) , attr ( td , "units" ) )
								inf1 <- c ( inf1 , tds )
						}
						
						if (! identical ( inf1 , "character(0)" ) ) mtext( paste ( inf1, collapse = "   " ) )				

						# falls pdf device schließen
						if ( pdf ) {
								dev.off()
								invisible ( pdf.file )
						} else {
								# bei erfolgreichen plot (auf die Konsole)
								invisible ( TRUE )
						}
				
				} else return ( dv )
		
		} else {
				warning ( "plot could not be created. check input." )
				invisible ( NULL )
		}
}
