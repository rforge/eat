# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.collect.results
# Description: Subroutine von automateModels
# Version: 	0.4.0
# Status: beta
# Release Date: 	2011-11-29
# Author:    Martin Hecht
# Change Log:
# 2011-11-29 SW/MH
# CHANGED: modified results structure in .automateModels.collect.results
# 0000-00-00 AA
#		14.10.2011 MH: Ausgabe auf Englisch
#		14.09.2011 MH: "\n" in sunk-Aufrüfen gelöscht (für optisch schöner)
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.collect.results <- function ( model.specs , additionalSubfolder ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.collect.results"
		f.n <- paste ( f. , ":" , sep = "" )

		sunk ( "\n" )
		sunk ( paste ( f.n , "Results are being read ..." ) )		
		
		ret <- mapply ( function (  folder ,
									item.grouping ,
									analyse.name ,
									p.model.name ,
									data.name ,
									software ,
									dif ,
									group ,
									group.names ,
									done ,
									additionalSubfolder ,
									all.analyse.name ) {
		
				ret <- NULL

				# Modell-Nummer String ( nur für Ausgabe )
				model.nr.str <- paste ( "Model" , which ( all.analyse.name %in% analyse.name ) , "of" , length ( all.analyse.name ) )
				
				if ( done ) {
						
						if ( software == "conquest" ) {
								
								# Einrücken der Ausgabe
								einr <- "           "
								
								# Ausgaben / Checks
								sunk ( paste ( f.n , "Sending" , model.nr.str , "to readConquestOutput ..." ) )
								sunk ( paste ( einr , "jobFolder = " , folder , sep = "" ) )
								if ( is.null ( additionalSubfolder )) aSF_str <- "" else aSF_str <- paste ( additionalSubfolder , collapse = ", " )
								sunk ( paste ( einr , "subFolder = " , aSF_str , sep = "" ) )
								sunk ( paste ( einr , "item.grouping = " , "'data.frame': " , nrow ( item.grouping ) , " obs. of " , ncol ( item.grouping ) , " variables" , sep = "" ) )
								sunk ( paste ( einr , "name.analyse = " , analyse.name , sep = "" ) )
								sunk ( paste ( einr , "p.model.name = " , p.model.name , sep = "" ) )
								sunk ( paste ( einr , "DIF.var = " , dif , sep = "" ) )
								sunk ( paste ( einr , "group.names = " , paste ( group.names , collapse = ", " ) , sep = "" ) )				
								stopifnot ( is.character ( group.names ) | is.null ( group.names ) )																
								sunk ( paste ( einr , "dataName = " , data.name , sep = "" ) )
								
								#sunk ( paste ( einr , "group = " , paste ( group , collapse = ", " ) , sep = "" ) )				
								#stopifnot ( is.character ( group ) | is.null ( group ) )

								# readConquestOutput starten
								ret <- readConquestOutput ( jobFolder = folder ,
															subFolder = additionalSubfolder ,
															item.grouping = item.grouping ,
															name.analyse = analyse.name ,
															p.model.name = p.model.name ,
															DIF.var = dif ,
															group.names = group.names ,
															dataName = data.name
															# , group = group ,
															)
								
						}
			
				} else { 
							# wenn done=FALSE ist Modell getimeouted, wird nicht eingelesen
							sunk ( paste ( f.n , model.nr.str , "will not be read because it's timeouted." ) )
						}
		
				# Rückgabe
				return ( ret )
		
		} , model.specs$folder ,
			model.specs$item.grouping ,
			model.specs$analyse.name ,
			model.specs$p.model.name ,
			model.specs$data.name ,
			model.specs$software ,
			model.specs$dif ,
			model.specs$group ,
			model.specs$group.names ,
			model.specs$done ,
			MoreArgs = list ( additionalSubfolder , model.specs$analyse.name ) , SIMPLIFY = FALSE )
		
		ret <- unlist ( ret , recursive = FALSE )
				
		# Analyse-Namen setzen
		# names ( ret ) <- unlist ( model.specs$analyse.name )

		# wahrscheinlich noch unlisten und dann wieder listen
		#ret <- list ( unlist ( ret , recursive = FALSE ) )
		# name setzen
		#names ( ret ) <- "results"
	
		# returnen
		return ( ret )
		
}

