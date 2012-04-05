# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.datasetTreatment
# .automateModels.datasetTreatment.getcols 
# .automateModels.datasetTreatment.numeric
# .automateModels.datasetTreatment.character
# Description: Subroutinen von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.datasetTreatment.getcols <- function ( dataset , type ) {

		# Funktionsname
		f.  <- ".automateModels.datasetTreatment.getcols"
		f.n <- paste ( f. , ":" , sep = "" )
		
		namen <- names ( which ( unlist ( mapply (  function ( element ) {
												if ( element$type == type ) TRUE else FALSE
												if ( element$type == type ) TRUE else FALSE
										} , dataset$varinfo , SIMPLIFY = FALSE ) ) ) )
		
		ret <- which ( colnames ( dataset$data ) %in% namen )
		
		if ( length ( ret ) != length ( namen ) ) {
				# sunk ( paste ( f.n , "Fehler:\n" , "zkdDataset und zkdVarinfo stimmen bzgl. Anzahl oder Benennung von " , type , "-Variablen nicht überein.\n" , 
							  # "Bitte Datensatz und Varinfo checken.\n" ) )
				sunk ( paste ( f.n , "Error:\n" , "zkdDataset and zkdVarinfo are not congruent concerning number or labels of " , type , "-variables.\n" , 
							  "Please check dataset and varinfo.\n" ) )
				stop ( )
		}

		return ( ret )
		
}

.automateModels.datasetTreatment.numeric <- function ( name , el , dataset ) {

		# Funktionsname
		f.  <- ".automateModels.datasetTreatment.numeric"
		f.n <- paste ( f. , ":" , sep = "" )
		
		if ( ! all ( check <- ( el %in% seq ( along = colnames ( dataset ) ) ) ) ) {
				# sunk ( paste ( f.n , "Fehler:\n" , "Nicht alle in Parameter" , name , "spezifizierten Spalten sind im Datensatz vorhanden.\n" , 
							  # "Nicht im Datensatz vorhanden:" , paste ( el [ which ( !check ) ] , collapse = ", " ) , "\n" ,
							  # "Bitte Datensatz und Parameter" , name , "checken.\n" ) )
				sunk ( paste ( f.n , "Error:\n" , "Not all in parameter" , name , "specified columns are not in the dataset.\n" , 
							  "Not in dataset:" , paste ( el [ which ( !check ) ] , collapse = ", " ) , "\n" ,
							  "Please check dataset and parameter" , name , ".\n" ) )

				stop ( )
		} else { 
				
				el.col <- match ( el , seq ( along = colnames ( dataset ) ) ) 
				
				return ( el.col )
				
		}
}

.automateModels.datasetTreatment.character <- function ( name , el , dataset ) {

		# Funktionsname
		f.  <- ".automateModels.datasetTreatment.character"
		f.n <- paste ( f. , ":" , sep = "" )
						
		if ( ! all ( check <- ( el %in% colnames ( dataset ) ) ) ) {
			# sunk ( paste ( f.n , "Fehler:\n" , name , "=" , paste ( el[!check] , collapse = ", " ) , "\n" ,
							# "ist kein gültiger Variablenname im Datensatz.\n" ) )									
			sunk ( paste ( f.n , "Error:\n" , name , "=" , paste ( el[!check] , collapse = ", " ) , "\n" ,
							"is not a valid column in the dataset.\n" ) )										
			
			stop ( )
		} else {

				el.col <- match ( el , colnames ( dataset ) )
		
				return ( el.col )
		}		

		
}

.automateModels.datasetTreatment <- function ( dataset , id = NULL , context.vars = NULL , items = NULL ) {

		# Funktionsname
		f.  <- ".automateModels.datasetTreatment"
		f.n <- ".automateModels.datasetTreatment:"
		
		### Plausichecks
		# stoppen wenn kein data.frame vorliegt
		stopifnot ( inherits ( dataset , "data.frame" ) | inherits ( dataset$data , "data.frame" ) )
		
		# Zusatzangaben
		stopifnot ( is.null ( id ) | is.character ( id ) | is.numeric ( id ) )
		stopifnot ( is.null ( context.vars ) | is.character ( context.vars ) | is.numeric ( context.vars ) )
		stopifnot ( is.null ( items ) | is.character ( items ) | is.numeric ( items ) )		

		# Initialisierung
		id.col <- cont.cols <- item.cols <- NULL
		
		### entweder ist dataset bereits data.frame oder absplitten von "zkdDataset"
		# wenn data.frame
		if ( is.data.frame ( dataset ) ) {
				
				# stoppen wenn Datensatz keine Spalten
				if ( ncol ( dataset ) < 1 ) {
						# sunk ( paste ( f.n , "Fehler:\n" , "Dataset hat weniger als eine Spalte.\n" ) )
						sunk ( paste ( f.n , "Error:\n" , "Dataset has less than one column.\n" ) )
						stop ( )
				}

				### Behandlung der Zusatzinfos
				
				# wenn character
				if ( is.character ( id ) ) id.col <- .automateModels.datasetTreatment.character ( "id" , id , dataset )
				if ( is.character ( context.vars ) ) cont.cols <- .automateModels.datasetTreatment.character ( "context.vars" , context.vars , dataset )
				if ( is.character ( items ) ) item.cols <- .automateModels.datasetTreatment.character ( "items" , items , dataset )
				
				# wenn numeric
				if ( is.numeric ( id ) ) id.col <- .automateModels.datasetTreatment.numeric ( "id" , id , dataset )
				if ( is.numeric ( context.vars ) ) cont.cols <- .automateModels.datasetTreatment.numeric ( "context.vars" , context.vars , dataset )
				if ( is.numeric ( items ) ) item.cols <- .automateModels.datasetTreatment.numeric ( "items" , items , dataset )
				
				# wenn id NULL: dummy-ID setzen
				if ( is.null ( id ) ) {
					
						dummy <- seq ( along = rownames ( dataset ) )
						dummy.str <- paste ( "id" , formatC ( dummy , width = max ( nchar ( dummy ) ) , flag="0" ) , sep = "" )
						dataset <- cbind ( "id" = dummy.str , dataset , stringsAsFactors = FALSE )
						id.col <- 1
						if ( is.numeric ( cont.cols ) ) cont.cols <- cont.cols + 1
						if ( is.numeric ( item.cols ) ) item.cols <- item.cols + 1
						# sunk ( paste ( f.n , "Info:\n" , "Dataset hat keine ID-Spalte.\n" ,
											# "Es wurde eine Dummy-ID in Spalte 1 erzeugt.\n" ) )				
						sunk ( paste ( f.n , "Info:\n" , "Dataset has no ID-column.\n" ,
											"Dummy-ID in column 1 has been generated.\n" ) )						
				}
				
				# wenn items NULL, per default alle Items außer ID auf Items
				if ( is.null ( items ) ) {

						item.cols <- which ( !colnames ( dataset ) %in% colnames ( dataset )[ c ( id.col , cont.cols ) ] )
						
						# if ( is.null ( cont.cols ) ) ausser.str <- "(außer ID)" else ausser.str <- "(außer ID und Kontext-Variablen)"
						if ( is.null ( cont.cols ) ) ausser.str <- "(except ID)" else ausser.str <- "(except ID and context variables)"
						
						# sunk ( paste ( f.n , "Warnung:\n" , "Parameter items ist nicht spezifiziert.\n" ,
											# "Alle Variablen" , ausser.str , "werden als Items behandelt!\n" ,
											# "Bitte überprüfen, ob dies gewünscht ist.\n" ) )							
						sunk ( paste ( f.n , "Warning:\n" , "Parameter items is not specified.\n" ,
											"All variables" , ausser.str , "are treated as items!\n" ,
											"Pleas check if this is desired.\n" ) )			
									
				}
			
		# wenn zkd-Dataset	
		} else if ( !is.null ( dataset$varinfo ) ) {

				# stoppen wenn Datensatz keine Spalten
				if ( ncol ( dataset$data ) < 1 ) {
						# sunk ( paste ( f.n , "Fehler:\n" , "Dataset hat weniger als eine Spalte.\n" ) )
						sunk ( paste ( f.n , "Error:\n" , "Dataset has less than one column.\n" ) )

						stop ( )
				}
		
				id.col <- .automateModels.datasetTreatment.getcols ( dataset , "ID" )
				
				cont.cols <- .automateModels.datasetTreatment.getcols ( dataset , "CV" )
				
				item.cols <- .automateModels.datasetTreatment.getcols ( dataset , "TI" )	
				
				dataset <- dataset$data
				
				# wenn Zusatzinfos spezifiziert, werden diese ignoriert da überflüssig
				# if ( !is.null ( id ) ) sunk ( paste ( f.n , "Info:\n" , "Dataset ist 'zkd'-Dataset. Parameter id =" , paste ( id , collapse = ", " ) , "wird ignoriert.\n" ) )
				# if ( !is.null ( context.vars ) ) sunk ( paste ( f.n , "Info\n:" , "Dataset ist 'zkd'-Dataset. Parameter context.vars =" , paste ( context.vars , collapse = ", " ) , "wird ignoriert.\n" ) )
				# if ( !is.null ( items ) ) sunk ( paste ( f.n , "Info:\n" , "Dataset ist 'zkd'-Dataset. Parameter items =" , paste ( items , collapse = ", " ) , "wird ignoriert.\n" ) )				
				if ( !is.null ( id ) ) sunk ( paste ( f.n , "Info:\n" , "Dataset is 'zkd'-Dataset. Parameter id =" , paste ( id , collapse = ", " ) , "ignored.\n" ) )
				if ( !is.null ( context.vars ) ) sunk ( paste ( f.n , "Info\n:" , "Dataset is 'zkd'-Dataset. Parameter context.vars =" , paste ( context.vars , collapse = ", " ) , "ignored.\n" ) )
				if ( !is.null ( items ) ) sunk ( paste ( f.n , "Info:\n" , "Dataset is 'zkd'-Dataset. Parameter items =" , paste ( items , collapse = ", " ) , "ignored.\n" ) )				
			
		} else {
				# sunk ( paste ( f.n , "Fehler:\n" , "Dataset ist not 'zkd'-Dataset or data.frame.\n" ) )
				sunk ( paste ( f.n , "Error:\n" , "Dataset is not 'zkd'-Dataset or data.frame.\n" ) )
				stop ( )
		}
	
		### Plausichecks
		stopifnot ( is.numeric ( id.col ) )
		stopifnot ( is.numeric ( cont.cols ) | is.null ( cont.cols ) )
		stopifnot ( is.numeric ( item.cols ) )

		# nur eine ID
		stopifnot ( length ( id.col ) == 1 )

		# alle Variablen abgefrühstückt
		if ( ! length ( colnames ( dataset ) ) == length ( c ( id.col,cont.cols,item.cols ) ) ) {
		
				# sunk ( paste ( f.n , "Fehler:\n" , "Es wurden nicht alle Variablen im Datensatz als ID, Kontext-Variable oder Item typisiert.\n" ,
									# "Bitte Parameter id, context.vars und items auf Konsistenz mit dem Datensatz checken.\n" ) )
				sunk ( paste ( f.n , "Error:\n" , "Not all variables in the dataset are typed as ID, context variable or item.\n" ,
									"Please check consistency of parameter id, context.vars, items and dataset.\n" ) )

				stop ( )		
		
		}

		### names setzen
		id.name <- colnames( dataset )[ id.col ]
		if ( !is.null ( cont.cols ) ) cont.names <- colnames( dataset )[ cont.cols ] else cont.names <- NULL
		item.names <- colnames( dataset )[ item.cols ]
	
		### Datensatz sortieren, wenn nötig
		if ( !identical (  seq ( along = colnames ( dataset ) ) , ( ordnung <- c ( id.col , cont.cols , item.cols ) ) ) ) {
		
				dataset <- dataset [ , ordnung ]
				# sunk ( paste ( f.n , "Info:\n" , "Dataset ist nicht in der Reihenfolge ID, Kontext-Variablen, Items sortiert.\n" ,
									# "Dataset wurde umsortiert.\n" ) )
				sunk ( paste ( f.n , "Info:\n" , "Dataset is not sorted in order ID, Kontext-Variablen, Items.\n" ,
									"Dataset has been re-sorted.\n" ) )

									
		}

		### alles auf character
		fun <- function ( spalte , name , f.n ) {
				if ( ! inherits ( spalte , "character" ) ) {
						
						sunk ( paste ( f.n , "Info: Variable" , name , "has been converted to 'character'."  , "\n" ) )

						as.character ( spalte )
				} else spalte
		}
		dataset <- data.frame ( mapply ( fun , dataset , colnames ( dataset ) , MoreArgs = list ( f.n ) , SIMPLIFY = FALSE ) , stringsAsFactors = FALSE )

		
		# return setzen
		ret <- list ( dataset = dataset , id.name = id.name , cont.names = cont.names , item.names = item.names )

		# returnen
		return ( ret )
		
}

##################################################################################################
# TESTEN von .automateModels.datasetTreatment
##################################################################################################

# [1] data.frame ohne alles
# ( d.1 <- data.frame ( rep (1:2) , rep (3:4) , rep ( 5:6 ) ) )
# .automateModels.datasetTreatment ( d.1 )

# [2] data.frame mit id mittendrin
# ( d.2 <- data.frame ( rep (1:2) , "id" = c ( "id1" , "id2" ) , rep (3:4) , rep ( 5:6 ) , stringsAsFactors = FALSE ) )
# .automateModels.datasetTreatment ( d.2 , id = "id" )
# .automateModels.datasetTreatment ( d.2 , id = 2 )

# [3] data.frame mit context-Variablen
# ( d.3 <- data.frame ( "cont1" = rep ( 8:9 ) , rep (1:2) , "id" = c ( "id1" , "id2" ) , rep (3:4) , rep ( 5:6 ) , "cont2" = rep ( 8:9 ) , stringsAsFactors = FALSE ) )
# .automateModels.datasetTreatment ( d.3 , id = "id" , context.vars = c ("cont1","cont2") )
# .automateModels.datasetTreatment ( d.3 , id = 3 , context.vars = c ( 1 , 6 ) )

# [4] Misspezifikation: Varname nicht in Dataset
# .automateModels.datasetTreatment ( d.3 , id = "435345" , context.vars = c ("adgdg","cont2") )
# .automateModels.datasetTreatment ( d.3 , id = "id" , context.vars = c ("adgdg","cont2") )
# .automateModels.datasetTreatment ( d.3 , id = "id" , context.vars = c ("cont1","cont2") , items = "bla" )

# [5] Misspezifikation: Spalte nicht in Dataset
# .automateModels.datasetTreatment ( d.3 , id = 0 )
# .automateModels.datasetTreatment ( d.3 , id = 3 , context.vars = c ( 10 , 11 ) )
# .automateModels.datasetTreatment ( d.3 , id = 3 , context.vars = c ( 1 , 6 ) , items = c ( 2 , 13 ) )

# [6] Misspezifikation: nicht alle Vars spezifiziert
# .automateModels.datasetTreatment ( d.3 , id = 3 , context.vars = c ( 1 , 6 ) , items = c ( 2 , 4 ) )
# .automateModels.datasetTreatment ( d.3 , id = "id" , context.vars = c ("cont1","cont2") , items = c ( 2 , 4 ) )

# [7] Sortierung wie in Elementen
# ( d.4 <- data.frame ( "cont1" = rep ( 8:9 ) , rep (1:2) , "id" = c ( "id1" , "id2" ) , rep (3:4) , rep ( 5:6 ) , "cont2" = rep ( 9:8 ) , stringsAsFactors = FALSE ) )
# .automateModels.datasetTreatment ( d.4 , id = "id" , context.vars = c ("cont2","cont1") , items = c( "rep.5.6." ,"rep.3.4." , "rep.1.2.") )
# .automateModels.datasetTreatment ( d.4 , id = 3 , context.vars = c ( 6 , 1 ) , items = c( 5 , 4 , 2 ) )
# .automateModels.datasetTreatment ( d.4 , id = 3 , context.vars = c ("cont2","cont1") , items = c( 5 , 4 , 2 ) )

# [8] zkd-Dataset NOT WORKING , mit modernerem zkd-Dataset testen !!!
# load ( "p:/ZKD/04_Beispieldaten/zkdMaster.rdata" )
# str ( zkdDatasetMaster )
# str ( zkdDatasetMaster$varinfo )
# out <- .automateModels.datasetTreatment ( zkdDatasetMaster )

# DEBUGGEN
# library (debug)
# mtrace ( .automateModels.datasetTreatment )
# mtrace ( .automateModels.datasetTreatment.numeric )
# mtrace ( .automateModels.datasetTreatment.getcols )







