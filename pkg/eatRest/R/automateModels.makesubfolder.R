# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.makesubfolder
# Description: Subroutine von automateModels
# Version: 	0.5.0
# Status: 
# Release Date: 	2011-09-08
# Author:    Martin Hecht
# Change Log:
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.makesubfolder <- function ( model.specs , folder , subfolder.order , subfolder.mode ) {

	# Reihenfolge der Subfolders
	subfolder.order.default = c ( "i.model" , "p.model" , "m.model" , "software" , "dif" , "regression" , "anchor" )
	if ( is.null ( subfolder.order ) ) subfolder.order <- subfolder.order.default
	
	# Plausicheck subfolder.order
	stopifnot ( all ( subfolder.order %in% subfolder.order.default ) )
	
	# i.model.name und p.model.name reinsortieren
	# (hinter i.model bzw. p.model)
	if ( "i.model" %in% subfolder.order ) subfolder.order <- append ( subfolder.order , "i.model.name" , which ( subfolder.order == "i.model" ) )
	if ( "p.model" %in% subfolder.order ) subfolder.order <- append ( subfolder.order , "p.model.name" , which ( subfolder.order == "p.model" ) )		
	if ( "dif" %in% subfolder.order ) subfolder.order <- append ( subfolder.order , "dif.name" , which ( subfolder.order == "dif" ) )		
	if ( "regression" %in% subfolder.order ) subfolder.order <- append ( subfolder.order , "regression.name" , which ( subfolder.order == "regression" ) )				
	
	# dif / regression / anchor  in  dif.dich / regression.dich / anchor.dich wandeln
	subfolder.order [ which ( subfolder.order == "dif" ) ] <- "dif.dich"
	subfolder.order [ which ( subfolder.order == "regression" ) ] <- "regression.dich"		
	subfolder.order [ which ( subfolder.order == "anchor" ) ] <- "anchor.dich"		
	
	# subfolder.mode Plausichecks
	subfolder.mode.available = c ( "intelligent" , "full" , "none" )
	if ( is.null ( subfolder.mode ) ) 
			subfolder.mode <- subfolder.mode.available[1]
	stopifnot ( subfolder.mode %in% subfolder.mode.available )

	### Analyse-Folder holen & Plausichecks & setzen
	model.specs$folder <- .automateModels.setsubfolder ( model.specs , folder , subfolder.order , subfolder.mode ) 

	### Analyse-Folder auf Platte erstellen
	stopifnot ( .automateModels.createsubfolders ( model.specs$folder , folder ) )
	
	# returnen
	return ( model.specs ) 
			
}
