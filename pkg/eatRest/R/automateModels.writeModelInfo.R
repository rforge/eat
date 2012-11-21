# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.writeModelInfo, .automateModels.writeModelInfo.write
# Description: Subroutine von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-11-29
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgabe auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.writeModelInfo.write <- function ( 
						software ,
						dataset ,
						item.grouping ,
						person.grouping ,
						i.model , 
						p.model , 						
						m.model , 
						dif ,
						regression ,
						anchor ,
						missing.rule ,
						group , 
						group.names ,
						analyse.name ,
						folder
						) {
		
		# file setzen
		filepath <- file.path ( folder , paste ( analyse.name , "__INFO.txt" ) ) 
		
		# löschen wenn existiert
		if ( file.exists ( filepath ) ) file.remove ( filepath )
		
		# Vektor erzeugen
		spalte2 <- 22
		towrite <- as.character()

		towrite <- append ( towrite , c ( "MODEL INFORMATION" , "" , "\n" ) )
	
		towrite <- append ( towrite , temp <- "Model Name:" )
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )		
		towrite <- append ( towrite , analyse.name )		

		towrite <- append ( towrite , temp <- "Dimensions:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , paste ( ncol ( item.grouping ) - 1 , " (" , i.model , ") " , sep = "" ) )	
		towrite <- append ( towrite , temp <- "Dimension Names:" )
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )		
		towrite <- append ( towrite , paste ( colnames ( item.grouping )[-1] , collapse = ", " ) )	

		towrite <- append ( towrite , temp <- "Groups:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , paste ( ncol ( person.grouping ) - 1 , " (" , p.model , ") " , sep = "" ) )	
		towrite <- append ( towrite , temp <- "Group Names:" )
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )		
		towrite <- append ( towrite , paste ( colnames ( person.grouping )[-1] , collapse = ", " ) )	

		towrite <- append ( towrite , temp <- "Measurement Model:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , paste ( m.model , sep = "" ) )	

		towrite <- append ( towrite , temp <- "Software:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , paste ( software , sep = "" ) )	

		if ( is.null ( dif ) ) dif_str <- "none" else dif_str <- paste ( dif , collapse = ", " )
		towrite <- append ( towrite , temp <- "DIF:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , dif_str )
		
		if ( is.null ( regression ) ) regr_str <- "none" else regr_str <- paste ( regression , collapse = ", " )
		towrite <- append ( towrite , temp <- "Regression:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , regr_str )	

		if ( is.null ( anchor ) ) anch_str <- "none" else anch_str <- "yes"
		towrite <- append ( towrite , temp <- "Anchor:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , anch_str )	
		
		if ( is.null ( missing.rule ) ) mr_str <- "none" else {
					mr_str <- paste ( mapply ( function ( mr , mr_name ) {
							paste ( mr_name , "=" , mr ) 
					} , missing.rule , names ( missing.rule ) ) , collapse = ", " )
		}
		towrite <- append ( towrite , temp <- "Missing Rule:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , mr_str )	

		if ( software == "conquest" ) {

				if ( is.null ( group ) ) group_str <- "none" else group_str <- paste ( group , collapse = ", " )
				towrite <- append ( towrite , temp <- "Deskr. Gruppenvar.:")
				towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
				towrite <- append ( towrite , group_str )				
		
				if ( is.null ( group.names ) ) group.names_str <- "none" else group.names_str <- paste ( group.names , collapse = ", " )
				towrite <- append ( towrite , temp <- "Deskriptive Gruppen:")
				towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
				towrite <- append ( towrite , group.names_str )			
		
		}
	
		towrite <- append ( towrite , c( "" , "" , "" ) )		
	
		towrite <- append ( towrite , temp <- "Generated:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , as.character(Sys.time()) )			
	
		towrite <- append ( towrite , temp <- "User:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , unname ( Sys.getenv()["USERNAME"] ) )

		towrite <- append ( towrite , temp <- "Userdomain:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , unname ( Sys.getenv()["USERDOMAIN"] ) )

		towrite <- append ( towrite , temp <- "Computername:")
		towrite <- append ( towrite , paste(rep(" ",(spalte2-nchar(temp))),collapse="") )				
		towrite <- append ( towrite , unname ( Sys.getenv()["COMPUTERNAME"] ) )
		
		# Vektor schreiben
		
		options ( warn = -1 )
		tried <- try (   write ( towrite , file = filepath , ncolumns = 3 , append = TRUE, sep = " ")   , silent=TRUE )
		if ( inherits ( tried , "try-error" ) ) ret <- FALSE else ret <- TRUE
	
		# returnen 
		return ( ret )
}

.automateModels.writeModelInfo <- function ( model.specs ) {

		# eatTools:::sunk ( paste ( ".automateModels.writeModelInfo: Modell-Informationen werden geschrieben ... " , "\n" , sep = "" ) )
		eatTools:::sunk ( paste ( ".automateModels.writeModelInfo: Model informationen are being written ... " , "\n" , sep = "" ) )
		
		check <- mapply ( .automateModels.writeModelInfo.write , 
								software = model.specs$software ,
								dataset = model.specs$dataset ,
								item.grouping = model.specs$item.grouping ,
								person.grouping = model.specs$person.grouping ,
								i.model = model.specs$i.model , 
								p.model = model.specs$p.model , 						
								m.model = model.specs$m.model , 
								dif = model.specs$dif ,
								regression = model.specs$regression ,
								anchor = model.specs$anchor ,
								missing.rule = model.specs$missing.rule ,
								group = model.specs$group , 
								group.names = model.specs$group.names ,
								analyse.name = model.specs$analyse.name ,
								folder = model.specs$folder
						)				

		# Plausicheck
		if ( ! all ( check ) ) {
				# stop ( paste ( "Es konnten für Modell(e) " , paste ( model.specs$analyse.name[ which ( !check ) ] , collapse = ", " ) , " die Modell-Informationen nicht erzeugt/geschrieben werden." , sep="" ) )
				stop ( paste ( "Could not write model information for model(s) " , paste ( model.specs$analyse.name[ which ( !check ) ] , collapse = ", " ) , " ." , sep="" ) )
				ret <- FALSE
		} else { 
				# eatTools:::sunk ( paste ( ".automateModels.writeModelInfo: Modell-Informationen wurden erfolgreich geschrieben." , "\n" , sep = "" ) )
				eatTools:::sunk ( paste ( ".automateModels.writeModelInfo: model information successfully written." , "\n" , sep = "" ) )
				ret <- TRUE
		}

		return ( ret )
		
}

