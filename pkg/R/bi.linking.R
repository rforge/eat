
.bi.linking.link.sirtr <- function ( results , analyse.1 , analyse.2 , method , scale.name ) {

		# bi.linking.name
		bi.linking.name <- paste ( analyse.1 , analyse.2 , sep = "---" )

		### 1. Analyse
		# matr.1 <- sapply ( results[[analyse.1]][[1]][[1]] , "[" , simplify = TRUE )
		# b.list.1 <- unlist ( matr.1 [ which ( rownames ( matr.1 ) == "b" ) , ]	) 
		# b.se.list.1 <- unlist ( matr.1 [ which ( rownames ( matr.1 ) == "b.se" ) , ] )
		# b.df.1 <- data.frame ( "item" = names ( b.list.1 ) , 
						# "itemdiff" = b.list.1 ,
						# stringsAsFactors = FALSE )
		matr.1 <- get.item.par ( results[analyse.1] )
		b.df.1 <- matr.1[,c("item","b")]
		b.list.1 <- matr.1[,"b"]
		names(b.list.1) <- matr.1[,"item"]
		b.se.list.1 <- matr.1[,"b.se"]
		names(b.se.list.1) <- matr.1[,"item"]
		
		### 2. Analyse
		matr.2 <- get.item.par ( results[analyse.2] )
		b.df.2 <- matr.2[,c("item","b")]
		b.list.2 <- matr.2[,"b"]
		names(b.list.2) <- matr.2[,"item"]		
		b.se.list.2 <- matr.2[,"b.se"]
		names(b.se.list.2) <- matr.2[,"item"]		

		# common Items
		intsec <- intersect ( b.df.1$item , b.df.2$item )

		# wenn es keine gemeinsamen Items gibt dann NA zurueckgeben
		if ( ! identical ( intsec , character(0) ) ) {
				
				# linken
				out <- equating.rasch( x = b.df.1 , y = b.df.2 )			 

				# Fehlervermeidung
				if ( ! identical ( colnames ( out$transf.par ) ,
								   c ( "item" , "TransfItempar.Gr1" , "Itempar.Gr2" ) ) ) {
						cat ( paste ( "Rueckgabe von sirtr ist inkonsistent. Bitte checken.\n" ) )
						stop ( )
				}
				
				# Linking Konstante
				res <- unname ( out$B.est[ method ] )

				# DIF
				prikey <- out$transf.par[,1]
				dfr <- data.frame ( 
									   "bi.linking.name" = rep ( bi.linking.name , length ( prikey ) ) ,
									   "analyse.1" = rep ( analyse.1 , length ( prikey ) ) ,
									   "analyse.2" = rep ( analyse.2 , length ( prikey ) ) ,									   
									   # "scale" = rep ( scale.name , length ( prikey ) ) ,									   
									   "item" = prikey ,
									   "b.1" = unname ( b.list.1[prikey] ) ,
									   "b.1.trans" = out$transf.par[,2] ,
									   "b.2" = out$transf.par[,3] ,
									   "b.se.1" =  unname ( b.se.list.1[prikey] ) ,
									   "b.se.2" =  unname ( b.se.list.2[prikey] ) ,
											stringsAsFactors = FALSE )
				
				if ( !is.null ( scale.name ) ) {
						dfr$scale <- rep ( scale.name , length ( prikey ) )
						dfr <- reinsort.col ( dfr , "scale" , "analyse.2" )
				}
			
				# gecentered
				# dfr$b.1.c <- dfr$b.1 - mean ( dfr$b.1 , na.rm = TRUE )
				# dfr$b.2.c <- dfr$b.2 - mean ( dfr$b.2 , na.rm = TRUE )
				# dfr$b.1.c.trans <- dfr$b.1.trans - mean ( dfr$b.2 , na.rm = TRUE )
			
				# gecenterte Schwierigkeit bewerten
				# dfr$b.1.c.eval <- as.character ( rep ( NA , length ( dfr$b.1.c ) ) )
				# dfr$b.1.c.eval [ abs ( dfr$b.1.c ) > 4 ] <- "schlecht"
				# dfr$b.1.c.eval [ abs ( dfr$b.1.c ) > 3 ] <- "kritisch"
				# dfr$b.1.c.eval [ abs ( dfr$b.1.c ) <= 3 ] <- "gut"
				
				# dfr$b.2.c.eval <- as.character ( rep ( NA , length ( dfr$b.2.c ) ) )
				# dfr$b.2.c.eval [ abs ( dfr$b.2.c ) > 4 ] <- "schlecht"
				# dfr$b.2.c.eval [ abs ( dfr$b.2.c ) > 3 ] <- "kritisch"
				# dfr$b.2.c.eval [ abs ( dfr$b.2.c ) <= 3 ] <- "gut"
				
				# DIF berechnen
				dfr$DIF <- dfr$b.1.trans - dfr$b.2
				
				Ncommon <- length ( na.omit ( dfr$DIF ) )
				dfr$Ncommon <- rep ( Ncommon , nrow ( dfr ) )						
				# besser potentielle Fehler hier noch abfangen (teilen durch 0 etc.)
				dfr$DIFc <- dfr$DIF - mean ( dfr$DIF , na.rm = TRUE )
				dfr$DIFc.abs <- abs ( dfr$DIFc )
				dfr$DIFc.abs.top80 <- as.integer ( dfr$DIFc.abs >= 
									  quantile( dfr$DIFc.abs , probs = 0.80, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				dfr$DIFc.abs.top90 <- as.integer ( dfr$DIFc.abs >= 
									  quantile( dfr$DIFc.abs , probs = 0.90, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				dfr$DIFc.abs.top95 <- as.integer ( dfr$DIFc.abs >= 
									  quantile( dfr$DIFc.abs , probs = 0.95, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				dfr$DIF.1 <-      dfr$DIFc / 2
				dfr$DIF.2 <- -1 * dfr$DIFc / 2
				dfr$DIF.e <- sqrt ( dfr$b.se.1^2 + dfr$b.se.2^2 ) 
				dfr$DIFc.t <- dfr$DIFc / dfr$DIF.e
				dfr$DIFc.t.pnorm <- dnorm ( dfr$DIFc.t )
				dfr$DIFc.t.pnorm.1s.90 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.90 ) ) 
				dfr$DIFc.t.pnorm.1s.95 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.95 ) )
				dfr$DIFc.t.pnorm.1s.99 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.99 ) )
				dfr$DIFc.t.pnorm.2s.90 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.95 ) )
				dfr$DIFc.t.pnorm.2s.95 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.975 ) )
				dfr$DIFc.t.pnorm.2s.99 <- as.numeric ( abs ( dfr$DIFc.t ) > qnorm ( 0.995 ) )
				
				# Linking Error
				error <- sqrt ( var ( dfr$DIF , na.rm = TRUE ) / Ncommon )
				
				# return bauen
				ret <- list ( res , error , Ncommon , dfr )
		
		} else {
				
				# return bauen
				ret <- list ( NA , NA , 0 , NULL )
		}

		# benennen
		names ( ret ) <- c ( "B" , "error" , "Ncommon" , "DIF" )
		
		# zu einer Liste damit man fuer return benamen kann
		ret <- list ( ret )
		names ( ret ) <- bi.linking.name
		
		# returnen
		return ( ret )

}

.bi.linking.call <- function ( results , bi.link.matrix , method , scale.name ) {

	ret2 <- mapply ( function ( analyse.1 , analyse.2 , results , method , scale.name ) {
				cat ( paste ( "   Linking von" , analyse.1 , "und" , analyse.2 ) , "\n" )
				flush.console()
				# Referenz : analyse.2 
				# zu linken : analyse.1
				ret <- .bi.linking.link.sirtr ( results , analyse.1 , analyse.2 , method , scale.name )
				# if ( !is.null ( ret ) ) return ( ret ) else {
						# return ( list ( list ( B = NULL ) ) )
				# }
		} , bi.link.matrix[,1] , bi.link.matrix[,2] , MoreArgs = list ( results , method , scale.name ) , SIMPLIFY = FALSE , USE.NAMES = FALSE )	
		
	ret2 <- unlist ( ret2 , recursive = FALSE )
	
	# B-Matrix bauen
	matrix.B.long <- cbind ( bi.link.matrix , "B" = unname ( unlist ( sapply ( ret2 , "[" , 1 ) ) ) , stringsAsFactors = FALSE )		
	matrix.B <- .cast.matrix.long ( matrix.B.long , "B" , bi.link.matrix )
	
	# error-Matrix bauen
	matrix.error.long <- cbind ( bi.link.matrix , "error" = unname ( unlist ( sapply ( ret2 , "[" , 2 ) ) ) , stringsAsFactors = FALSE )		
	matrix.error <- .cast.matrix.long ( matrix.error.long , "error" , bi.link.matrix )

	# Ncommon-Matrix bauen
	matrix.Ncommon.long <- cbind ( bi.link.matrix , "Ncommon" = unname ( unlist ( sapply ( ret2 , "[" , 3 ) ) ) , stringsAsFactors = FALSE )		
	matrix.Ncommon <- .cast.matrix.long ( matrix.Ncommon.long , "Ncommon" , bi.link.matrix )
	
	# DIF Liste bauen
	DIF.namen <- names ( ret2 )
	DIF <- unlist ( sapply ( ret2 , "[" , 4 , simplify = FALSE ) , recursive = FALSE , use.names = FALSE )
	names ( DIF ) <- DIF.namen
	
	# loeschen von NULL aus Liste (wenn nicht gelinkt werden konnte)
	del <- .which.list.element.is.null ( DIF )
	if ( ! identical ( del , integer(0) ) ) DIF[del] <- NULL
	
	# Ergebnisse
	ret <- list ( "B" = matrix.B , "error" = matrix.error , "Ncommon" = matrix.Ncommon , "DIF" = DIF )
	
	# returnen		
	return ( ret )
		
}

.bi.linking <- function ( results , scales=NULL , method , lower.triangle ) {
	
	# mind. 2 Analysen pro Skala vorhanden?
	# wenn nicht werden alle genommen
	welche <- mapply ( function ( scales , res.names ) {
			if ( length ( which ( ! regexpr ( scales , res.names ) == -1 ) ) > 1 ) TRUE else FALSE
	} , scales , MoreArgs = list ( names ( results ) ) , SIMPLIFY = TRUE , USE.NAMES = FALSE )
	scales <- scales [ welche ]
	if ( identical ( scales , character(0) ) ) scales <- NULL


	if ( ! is.null ( scales ) ) {
			
			.fun <- function ( scales , results , method ) {
				namen <- names ( results )
				link.names <- namen [ ! regexpr ( scales , namen ) == -1 ]
				bi.link.matrix <- .expand.grid.triangle ( link.names , lower = lower.triangle )
				cat ( paste ( "Scale:" , scales , "" , "\n" ) )
		
				ret <- .bi.linking.call ( results , bi.link.matrix , method , scales )
			}
			
			ret <- mapply ( .fun , scales , MoreArgs = list ( results , method ) , SIMPLIFY = FALSE )
			
	} else {
			
			link.names <- names ( results )
			bi.link.matrix <- .expand.grid.triangle ( link.names , lower = lower.triangle )
			ret <- list ( .bi.linking.call ( results , bi.link.matrix , method , NULL ) )
			names ( ret ) <- "all"
			return ( ret )
	}
	
	return ( ret )
	
}

bi.linking <- function ( results , folder=NULL , file.name=NULL , method = NULL , lower.triangle = TRUE , scales=NULL ) {

		# Begrueßung
		cat ( "Linking startet...\n" )
		
		# Plausicheck
		if ( length ( results ) < 2 ) stop( "Nicht mind. 2 Analysen zum linken vorhanden." )
		
		# Verzeichnis loeschen und/oder erstellen
		temp <- .del.or.create.folder ( folder )
		
		
		# defaults
		if ( is.null ( method ) ) method <- "Stocking-Lord"
		stopifnot ( any ( method %in% c ( "Mean-Mean" , "Haebara" , "Stocking-Lord" ) ) )

		# Linking durchfuehren
		out <- .bi.linking ( results , scales , method , lower.triangle )
		
		# DIF Datensaetze zusammenmergen und an Struktur ranhaengen
		DIF.all <- .rbind.DIF ( out )
		out <- .DIF.append ( out , DIF.all )
		
		# aggregierten DIF Datensatz bauen und an Struktur ranhaengen
		out <- .agg.DIF ( out )

		# Deskriptives ( mean, ... ) ueber alle Skalen
		kennwerte <- c ( "B" , "error" , "Ncommon" )
		scale.descr <- mapply ( function ( kennwert ) {
				mapply ( function ( scales , kennwert ) {
						mapply ( function ( kennwert ) {
								unname ( unlist ( kennwert ) ) 
						} , scales[[kennwert]] , USE.NAMES = FALSE )
				} , out , MoreArgs = list ( kennwert ) , USE.NAMES = FALSE )
		} , kennwerte , SIMPLIFY = FALSE )
		scale.agg.stats <- mapply ( function ( scd ) {
				scd <- as.vector (scd)
				data.frame ( "min" = min ( scd , na.rm=TRUE ) , "max" = max ( scd , na.rm=TRUE ) , "mean" =  mean ( scd , na.rm=TRUE ) , "sd" = sd(as.vector (scd),na.rm=TRUE) )
		} , scale.descr , SIMPLIFY = FALSE )
		scale.agg.stats.dfr <- data.frame ( cbind ( "B" = unname ( unlist ( scale.agg.stats$B ) ) , "error" = unname ( unlist ( scale.agg.stats$error ) ) , "Ncommon" = unname ( unlist ( scale.agg.stats$Ncommon ) ) ) )
		rownames ( scale.agg.stats.dfr ) <- names ( scale.agg.stats$B )
		
		# wenn folder gesetzt wird Excel rausgeschrieben
		if ( ! is.null ( folder ) ) {
	
				# ueberschreiben falls existiert
				if ( is.null ( file.name ) ) file.name <- "bi.linking.results.xlsx"
				if ( substr ( file.name , ( n <- nchar ( file.name ) ) - 4 , n ) == ( suf <- ".xlsx" ) ) {
						file.name.woext <- substr ( file.name , 1, ( n <- nchar ( file.name ) ) - 4 )
				} else {
						file.name.woext <- file.name
						file.name <- paste ( file.name , suf , sep = "" )
				}

				# scale.agg.stats Rdata
				save ( scale.agg.stats , file = file.path ( folder , paste ( file.name.woext , ".scale.agg.stats.Rdata" , sep = "" ) ) )
				
				# falls extra Excel geschrieben werden muessen
				# file.name.dif <- paste ( substr ( file.name , ( n <- nchar ( file.name ) ) - 4 , n ) , "_DIF.xlsx" , sep="" )
				
				path <- file.path ( folder , file.name )
				if ( file.exists ( path ) ) file.remove ( path )
				# path.dif <- file.path ( folder , file.name.dif )
				# if ( file.exists ( path.dif ) ) file.remove ( path.dif )
				
				cat ( "Ergebnisse werden geschrieben (kann dauern) " ); flush.console () 
				### Excels rausschreiben
				mapply ( function ( out , scale.name , path , file.name , folder , n.scales ) {

					# Ausgabe
					cat ( "." ); flush.console () 

					# zum Berechnen von Kennwerten
					B_ <- na.omit ( unname ( unlist ( out$B ) ) )
					error_ <- na.omit ( unname ( unlist ( out$error ) ) )
					Ncommon_ <- na.omit ( unname ( unlist ( out$Ncommon ) ) )					

					# fuer Ausgabe noch bestimmte Anzahl an Spalten ran
					if ( ncol ( out$B ) < 4 ) { 
								to.cbind <- data.frame ( matrix ( NA , ncol = 4 - ncol ( out$B ) , nrow = nrow ( out$B ) ) ,
								stringsAsFactors = FALSE )
								out$B <- cbind ( out$B , to.cbind )
								out$error <- cbind ( out$error , to.cbind )
								out$Ncommon <- cbind ( out$Ncommon , to.cbind )
					}
	
					# Konstante + Error
					if ( ( ncol ( out$B ) - 4 ) > 0 ) addNA <- rep ( NA , ncol ( out$B ) - 4 ) else addNA <- NULL
					towrite <- rbind ( "B Matrix" = rep ( NA , ncol ( out$B ) ) , out$B , 
									   "B min max mean sd" = c ( min ( B_ ) , max ( B_ ) , mean ( B_ ) , sd ( B_ ) , addNA )  ,
									   "B ALL SCALES min max mean sd" = c ( unname ( unlist ( scale.agg.stats$B ) ) , addNA ) ,
									   "error Matrix" =  rep ( NA , ncol ( out$B ) ) , out$error ,
									   "error min max mean sd" = c ( min ( error_ ) , max ( error_ ) , mean ( error_ ) , sd ( error_ ) , addNA ) ,
									   "error ALL SCALES min max mean sd" = c ( unname ( unlist ( scale.agg.stats$error ) ) , addNA ) ,
									   "Ncommon Matrix" =  rep ( NA , ncol ( out$B ) ) , out$Ncommon ,
									   "Ncommon min max mean sd" = c ( min ( Ncommon_ ) , max ( Ncommon_ ) , mean ( Ncommon_ ) , sd ( Ncommon_ ) , addNA ) ,  									   
									   "Ncommon ALL SCALES min max mean sd" = c ( unname ( unlist ( scale.agg.stats$Ncommon ) ) , addNA ) , 
									   "rownames von 'error' und 'Ncommon' manuell nachbearbeiten!" =  rep ( NA , ncol ( out$B ) ) ,
									   "Spalte(n) ist/sind Referenzgruppe(n)" =  rep ( NA , ncol ( out$B ) )
									   # "2. Analyse ist die Referenzanalyse." =  rep ( NA , ncol ( out$B ) ) 
									   )
										
					if ( file.exists ( path ) ) app <- TRUE else app <- FALSE
					write.xlsx2 ( x = towrite , file = path ,
										   sheetName = ( sn <- scale.name ) ,
										   append = app ,
										   row.names = TRUE )
					
					# Rdata-Files
					B <- out$B
					error <- out$error
					Ncommon <- out$Ncommon
					save ( B , file = file.path ( folder , paste ( file.name.woext , ".B.Rdata" , sep ="" ) ) )
					save ( error , file = file.path ( folder , paste ( file.name.woext , ".error.Rdata" , sep ="" ) ) )
					save ( Ncommon , file = file.path ( folder , paste ( file.name.woext , ".Ncommon.Rdata" , sep ="" ) ) )
					
					# Ausgabe
					cat ( "." ); flush.console () 
					
					# DIF.all
					if ( nrow ( out$DIF$all ) < 5000/n.scales ) {
						if ( file.exists ( path ) ) app <- TRUE else app <- FALSE
						write.xlsx2 ( x = out$DIF$all , file = path ,
											   sheetName = ( sn <- paste ( scale.name , "DIF" , sep = "_" ) ),
											   append = app ,
											   row.names = FALSE )
					} else {
						
						file.name.dif <- paste ( substr ( file.name , 1 , ( n <- nchar ( file.name ) ) - 5 ) , "_" , scale.name , ".xlsx" , sep="" )
						folder.dif <- file.path ( folder, "DIF" )
						if ( ! file.exists ( folder.dif ) ) dir.create ( folder.dif , recursive = TRUE )
						path.dif <- file.path ( folder.dif , file.name.dif )

						mapply ( function ( dfr , dfr.name , scale.name , path.dif , all.dfr.names ) {
								
								# Ausgabe
								cat ( "." ); flush.console () 						

								if ( length ( all.dfr.names ) < 19 ) {
								
										if ( file.exists ( path.dif ) ) app <- TRUE else app <- FALSE
								
								} else {
										path.dif <- paste ( substr ( path.dif , 1 , ( n <- nchar ( path.dif ) ) - 5 ) , "_" , dfr.name , ".xlsx" , sep="" )
										app <- FALSE
								}
								
								write.xlsx2 ( x = dfr , file = path.dif ,
													   sheetName = paste ( "Sheet" , which ( all.dfr.names %in% dfr.name ) ) , 
													   append = app ,
													   row.names = FALSE )
								
						
						} , out$DIF[-which(names(out$DIF) %in% c("all","all.agg"))] ,
							names ( out$DIF )[-which(names(out$DIF) %in% c("all","all.agg"))] ,
							MoreArgs = list ( scale.name , path.dif ,
							names ( out$DIF )[-which(names(out$DIF) %in% c("all","all.agg"))] ) )
							# "all" und "all.agg" nicht mit schreiben
					
					}

					# Rdata
					DIF <- out$DIF$all
					save ( DIF , file = file.path ( folder , paste ( file.name.woext , ".DIF.Rdata" , sep ="" ) ) )
					
					# Ausgabe
					cat ( "." ); flush.console () 
				
					# DIF.agg
					if ( nrow ( out$DIF$all.agg ) < 500/n.scales ) {					
							if ( file.exists ( path ) ) app <- TRUE else app <- FALSE
							path.dif.agg <- path
					} else {
							app <- FALSE
							folder.dif.agg <- file.path ( folder, "DIF_agg" )
							if ( ! file.exists ( folder.dif.agg ) ) dir.create ( folder.dif.agg , recursive = TRUE )
							path.dif.agg <- file.path ( folder.dif.agg , paste ( file.name.woext , "_" , scale.name , "_DIF_agg.xlsx" , sep = "" ) )
					}
					write.xlsx2 ( x = out$DIF$all.agg , file = path.dif.agg ,
										   sheetName = ( sn <- paste ( scale.name , "DIF" , "agg" , sep = "_" ) ) ,
										   append = app ,
										   row.names = FALSE )

					
					# Rdata
					DIF.agg <- out$DIF$all.agg
					save ( DIF.agg , file = file.path ( folder , paste ( file.name.woext , ".DIF.agg.Rdata" , sep ="" ) ) )
					
				
				} , out , names ( out ) , MoreArgs = list ( path , file.name , folder , length ( out ) ) ) ### Ende mapply
				
				cat ( " done\n" ); flush.console ()

				# Rdata
				linking.results <- out
				save ( linking.results , file = file.path ( folder , paste ( file.name.woext , ".linking.results.Rdata" , sep ="" ) ) )
				
		} # Ende if ( ! is.null ( folder ) )
		
		cat ( "Linking fertig.\n" )
		
		return ( out )
		
}

.rbind.DIF <- function ( input ) {
		DIFdfr <- NULL
		for ( scale.name in names ( input ) ) {
				for ( DIFname in names(input[[scale.name]]$DIF) ) {
							
						temp <- NULL
						temp <- input[[scale.name]]$DIF[[DIFname]]
						# new <- cbind ( 
								# "bi.link.name" = rep ( DIFname , nrow ( temp ) ) ,
								# "analyse1" = colnames ( temp )[2] ,
								# "analyse2" = colnames ( temp )[3] ,
								# temp ,
								# stringsAsFactors = FALSE )
						new <- temp
								
						if ( is.null ( DIFdfr ) ) DIFdfr <- new else DIFdfr <- rbind ( DIFdfr , new )
				}
		}
		return ( DIFdfr )
}

.DIF.append <- function ( komplett , add ) {
	for ( scale.name in names ( komplett ) ) {
			
			komplett[[scale.name]]$DIF <- c ( komplett[[scale.name]]$DIF , "all" = list ( add ) )
			
	}
	return ( komplett )
}

.expand.grid.triangle <- function ( v , lower=TRUE ) {

		a <- expand.grid (v,v,stringsAsFactors=FALSE) 
		d <- cbind (a,"value"=v)
		e <- cast ( d , Var1 ~ Var2 , value = "value" )
		if ( lower ) f <- lower.tri ( e ) else f <- upper.tri ( e )
		g <- melt ( f )
		h <- a [ g[,3] , ]
	
}
# TEST
#( .expand.grid.triangle ( c ( 1,2,3 ) , lower = FALSE ) )

.cast.matrix.long <- function ( matrix.long , cast.val , matrix.sorted ) {
		matrix.cast <- as.data.frame ( cast ( matrix.long , Var1 ~ Var2 , value = cast.val ) )
		rownames ( matrix.cast ) <- matrix.cast [ , 1 ]
		matrix.unsorted <- matrix.cast[,-1,drop=FALSE]
	
		# sortieren
		colnamen <- unique ( matrix.sorted[ , 2 ] )
		rownamen <- unique ( matrix.sorted[ , 1 ] )
		matrix.out <- data.frame ( matrix(data = NA, nrow = nrow ( matrix.unsorted ) , ncol = ncol ( matrix.unsorted ) , byrow = FALSE, dimnames = NULL) )
		for ( colname in colnamen ) {
				for ( rowname in rownamen ) {
						matrix.out [ which ( rownamen %in% rowname ) , which ( colnamen %in% colname ) ] <- matrix.unsorted [ rowname , colname ]
				}
		}
		colnames ( matrix.out ) <- colnamen
		rownames ( matrix.out ) <- rownamen
		
		return ( matrix.out )
}

.agg.DIF <- function ( inp ) {

		out <- inp
	
		for ( scle in names ( inp ) ) {
		
				to.agg <- agg <- agg.n.valid <- agg.DIF.vars <- agg.DIF <- agg.p.vars <- 
						agg.p <- NULL
	
				to.agg <- inp [[ scle ]]$DIF$all

				agg.n.valid <- aggregate ( as.numeric ( !is.na ( to.agg$DIFc.abs ) ), by=list( to.agg$item ) , FUN=sum, simplify = TRUE )
				colnames ( agg.n.valid )[2] <- "n.valid"
			
				# mit Vorzeichen
				agg.DIF.vars <- list ( "DIFc.min" = min , "DIFc.max" = max ,
								       "DIFc.mean" = mean ,
								       "DIFc.sd" = sd , "DIFc.sum" = sum ) 
				agg.DIF <- .agg.loop1 ( agg.DIF.vars , to.agg$DIFc , list( to.agg$item ) )

				agg <- merge ( agg.n.valid , agg.DIF )

				# ohne Vorzeichen
				agg.DIF.vars <- list ( "DIFc.abs.min" = min , "DIFc.abs.max" = max ,
								       "DIFc.abs.mean" = mean ,
								       "DIFc.abs.sd" = sd , "DIFc.abs.sum" = sum ) 
				agg.DIF <- .agg.loop1 ( agg.DIF.vars , to.agg$DIFc.abs , list( to.agg$item ) )

				agg <- merge ( agg , agg.DIF )

				
				agg$DIFc.abs.sum.adj <- agg$DIFc.abs.sum + ( rep ( max ( agg$n.valid ) , nrow ( agg ) ) - agg$n.valid ) * agg$DIFc.abs.mean

				agg$DIFc.abs.sum.adj.top80 <- as.integer ( agg$DIFc.abs.sum.adj >= 
									  quantile( agg$DIFc.abs.sum.adj , probs = 0.80, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				agg$DIFc.abs.sum.adj.top90 <- as.integer ( agg$DIFc.abs.sum.adj >= 
									  quantile( agg$DIFc.abs.sum.adj , probs = 0.90, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				agg$DIFc.abs.sum.adj.top95 <- as.integer ( agg$DIFc.abs.sum.adj >= 
									  quantile( agg$DIFc.abs.sum.adj , probs = 0.95, na.rm = TRUE ,
									  names = FALSE , type = 7 ) )
				
				agg.top.vars <- c ( "DIFc.abs.top80", "DIFc.abs.top90" , "DIFc.abs.top95" )
				agg.top <- .agg.loop2 ( agg.top.vars , to.agg , list( to.agg$item ) )				

				agg <- merge ( agg , agg.top )
				agg$DIFc.abs.top80.rel <- agg$DIFc.abs.top80 / agg$n.valid
				agg$DIFc.abs.top90.rel <- agg$DIFc.abs.top90 / agg$n.valid
				agg$DIFc.abs.top95.rel <- agg$DIFc.abs.top95 / agg$n.valid
				
				agg.p.vars <- c ( "DIFc.t.pnorm.1s.90", "DIFc.t.pnorm.1s.95", "DIFc.t.pnorm.1s.99",
								  "DIFc.t.pnorm.2s.90", "DIFc.t.pnorm.2s.95", "DIFc.t.pnorm.2s.99" )
				agg.p <- .agg.loop2 ( agg.p.vars , to.agg , list( to.agg$item ) )

				agg <- merge ( agg , agg.p )

				agg$DIFc.t.pnorm.1s.90.rel <- agg$DIFc.t.pnorm.1s.90 / agg$n.valid
				agg$DIFc.t.pnorm.1s.95.rel <- agg$DIFc.t.pnorm.1s.95 / agg$n.valid
				agg$DIFc.t.pnorm.1s.99.rel <- agg$DIFc.t.pnorm.1s.99 / agg$n.valid
				agg$DIFc.t.pnorm.2s.90.rel <- agg$DIFc.t.pnorm.2s.90 / agg$n.valid
				agg$DIFc.t.pnorm.2s.95.rel <- agg$DIFc.t.pnorm.2s.95 / agg$n.valid
				agg$DIFc.t.pnorm.2s.99.rel <- agg$DIFc.t.pnorm.2s.99 / agg$n.valid
				# agg [ agg == Inf ] <- NA
		
				colnames ( agg )[1] <- "item"
				
				out [[ scle ]]$DIF$all.agg <- agg
		}
	return ( out )
}

.agg.loop1 <- function ( agglist , to.agg , by ) {
		
		.fun <- function ( agglist , name , to.agg , by ) {
				if ( length ( unlist ( by ) ) == length ( unique ( unlist ( by ) ) ) ) na.rm <- FALSE else na.rm <- TRUE
				temp <- aggregate ( x=to.agg , by=by , FUN=agglist, na.rm=na.rm, simplify = TRUE )
				colnames ( temp )[length(by)+1] <- name
				return ( temp )
		}
		
		old <- getOption("warn")
		options ( warn=-1 )
		aggs <- mapply ( .fun , agglist , names ( agglist ) , MoreArgs = list ( to.agg , by ) , SIMPLIFY = FALSE )
		options ( warn=old )
		
		agg <- NULL
		for ( i in seq ( along = aggs ) ) {
				if ( is.null ( agg ) ) agg <- aggs[[i]] else agg <- merge ( agg , aggs[[i]] )
		}
		
		rownames ( agg ) <- rep ( 1:nrow ( agg ) )
		return ( agg )
}

.agg.loop2 <- function ( aggvars , to.agg , by ) {

		.fun <- function ( aggvars , to.agg , by ) {
				if ( length ( unlist ( by ) ) == length ( unique ( unlist ( by ) ) ) ) na.rm <- FALSE else na.rm <- TRUE
				temp <- aggregate ( x=to.agg[,aggvars] , by=by , FUN=sum, na.rm=na.rm, simplify = TRUE )
				colnames ( temp )[length(by)+1] <- aggvars
				return ( temp )
		}
		
		aggs <- mapply ( .fun , aggvars , MoreArgs = list ( to.agg , by ) , SIMPLIFY = FALSE )

		agg <- NULL
		for ( i in seq ( along = aggs ) ) {
				if ( is.null ( agg ) ) agg <- aggs[[i]] else agg <- merge ( agg , aggs[[i]] )
		}
		
		rownames ( agg ) <- rep ( 1:nrow ( agg ) )
		return ( agg )

}


# Laden
# source ( "r:/Nawi/main/07_IT/R/Linking/Source/sirtr_0.7-04.R" )
# source ( "p:/ZKD/ZKDaemon/zkdHelpers_0.4.2.R" )
# source ( "p:/ZKD/development/.automateModels.internal_0.1.0.R" ) ### fuer .which.list.element.is.null, del.or.create.folder
# zkdHelpers_LoadPackage ( "reshape" )
# zkdHelpers_LoadPackage ( "xlsx" )
# load ( "c:/temp/Temp20/results.Rdata" )
# load ( "r:/Nawi/main/13_Normierung_2011/Auswertung/06_Skalierung2/01/_automateModels_/results.Rdata" )
#load ( "r:/Nawi/main/13_Normierung_2011/Auswertung/06_Skalierung2/04/_automateModels_/results.Rdata" )
# results <- results[c(1,2,3,10,11,12,13,14,15,16,17,18,19,20)]
# results <- results[c(1,2,3)]
# results <- results[c(22,28)]
# load ( "r:/Nawi/main/13_Normierung_2011/Auswertung/06_Skalierung2/04/_automateModels_/results.Rdata" )



# c ( "Mean-Mean" , "Haebara" , "Stocking-Lord" )
# c ("BF","BE","CF","CE","PF","PE")

# Achtung , wenn folder existiert, wird dieser GELoeSCHT
# out <- bi.linking ( results , scales = c ("BF") , folder="c:/Temp/Temp22" , file.name = NULL , lower.triangle = TRUE )


# str(out)


