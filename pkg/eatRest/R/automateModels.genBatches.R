# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.genBatches
# Description: Subroutine von automateModels
# Author:    Martin Hecht
# Change Log:
# 2012-01-17 SW/MH
# CHANGED: added option 'all.local.cores' in .automateModels.genBatches
# 0000-00-00 AA
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (f�r Logfile)
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.genBatches <- function ( model.specs , folder.aM , run.mode , n.batches , cores ) {
		
		# Plausichecks
		stopifnot ( run.mode %in% c ( "serial" , "parallel" ) )
		if ( is.null ( n.batches ) ) n.batches <- 1
		stopifnot ( is.numeric ( n.batches ) )
		
		# 23.10.2012 kann aukommentiert werden
		# da bei run.mode "serial" n.batches komplett an Anzahl cores h�ngt
		# if ( run.mode == "serial" & !n.batches == 1 ) {
				### eatTools:::sunk ( paste ( ".automateModels.genBatches: Parameter n.batches = " , n.batches , " in Kombination mit run.mode == 'serial' ist nicht sinnvoll und wird ignoriert.\n" , sep = "" ) )
				# eatTools:::sunk ( paste ( ".automateModels.genBatches: Parameter n.batches = " , n.batches , " in combination with run.mode == 'serial' is not meaningfull and ignored.\n" , sep = "" ) )
				# n.batches <- 1
		# }
		
		stopifnot ( file.access ( folder.aM , mode = 4 ) == 0 )
		
		# batch folder erstellen
		folder.batch <- file.path ( folder.aM , "batches" )
		if ( ! file.exists ( folder.batch ) ) { dir.create ( folder.batch ) }
	
		### Parameter zur batch-Gruppenerzeugung setzen
		n.models <- length ( model.specs$folder )
		
		# wenn run.mode="serial"
		# ggf. auf mehrere lokale Cores verteilen, d.h. so viele batches wie cores
		if ( run.mode == "serial" ) {
				co <- detectCores ( logical = FALSE )
				if ( is.null ( cores ) ) {
						n.batches <- co
				} else {
						stopifnot ( is.numeric ( cores ) )
						if ( cores > co ) cores <- co
						n.batches <- cores
				}
		}
		
		if ( n.batches > n.models ) n.batches <- n.models		
		n.per.group <- ceiling ( n.models / n.batches )
		if ( n.per.group == 0 ) n.per.group <- 1
		indices.vector <- rep ( 1 : n.models )
		
		# (rekursive) Funktion zur (zuf�lligen) Gruppenerzeugung
		smpl <- function ( indices , n.per.group ) {
				
				if ( length ( indices ) > n.per.group ) {
				
						gr <- sample ( indices , n.per.group )
						ret <- c ( list ( gr ) , smpl ( indices [ - which ( indices %in% gr ) ] , n.per.group ) )
				
				} else { ret <- list ( indices ) }
				
			return ( ret ) 
		
		}
		
		# batch-Gruppen-Indices erstellen
		batch.groups.ind <- smpl ( indices.vector , n.per.group )
		
		# batch-Gruppen schreiben
		ret <- mapply ( function ( batch.groups.ind , batch.nr , width.batch.nr , model.specs , folder.batch ) {
				
				# Parameter der aktuellen batch.group setzen
				folder.temp <- model.specs$folder [ batch.groups.ind ]
				analyse.name.temp <- model.specs$analyse.name [ batch.groups.ind ]
				
				# batch Eintr�ge erzeugen
				batch.elements <- mapply ( function ( folder.temp , analyse.name.temp ) {
						
						# f�r Windows backslashes
						if ( ! regexpr ( "Windows" , Sys.getenv()["OS"] ) == -1 ) folder.temp <- gsub ( "/", "\\", folder.temp , fixed=TRUE)
						
						paste ( substr ( folder.temp , 1 , 2 ) , "\n" ,
								'cd "' , folder.temp , '"\n' ,
								"CALL " , paste ( analyse.name.temp , ".bat" , sep = "" )	, sep = "" )
						
				} , folder.temp , analyse.name.temp , SIMPLIFY = FALSE )
				
				# batch name
				batch.name <- paste ( "batch" , formatC ( batch.nr , width = width.batch.nr , flag="0" ) , ".bat" , sep = "" ) 
				batch.file <- file.path ( folder.batch , batch.name )
				
				# batch schreiben
				options ( warn = -1 )
				if ( inherits ( try (  write ( paste ( batch.elements , collapse = "\n" ) , file = batch.file )  
					, silent = TRUE ) , "try-error" ) ) {
						# eatTools:::sunk ( paste ( ".automateModels.genBatches: Fehler beim Speichern von " , batch.file , "\n" , sep = "" ) )
						eatTools:::sunk ( paste ( ".automateModels.genBatches: Error while writing " , batch.file , "\n" , sep = "" ) )
						stop ( )
						ret <- NULL 
				} else ret <- batch.file
		
				# returnen
				return ( ret )
				
		} , batch.groups.ind , rep ( 1 : length ( batch.groups.ind ) ) , 
			MoreArgs = list ( nchar ( as.character ( length ( batch.groups.ind ) ) ) , model.specs , folder.batch ) , SIMPLIFY = FALSE )

		# returnen 
		return ( ret )
		
}



