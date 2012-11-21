# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.remove.from.model.specs
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben aus Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.remove.from.model.specs <- function ( model.specs , delete ) {

		### Funktionsname für Meldungen
		f. <- "automateModels"
		f.n <- paste ( f. , ":" , sep = "" )
	
		# Plausichecks
		stopifnot ( is.numeric ( delete ) )
		stopifnot ( inherits ( model.specs , "list" ) )
	
		ret <- mapply ( function ( ms , delete ) {
				if ( all ( delete %in% seq ( along = ms ) ) ) ms[-delete] else stop ( paste ( "internal error in", f.n , "\n" ) )
		} , model.specs , MoreArgs = list ( delete ) , SIMPLIFY = FALSE )
		
		if ( identical ( ret , list() ) ) {
				# eatTools:::sunk ( paste ( "Alle Modelle sind misspezifiziert und wurden gelöscht.\n" ) )
				eatTools:::sunk ( paste ( "All models are misspecified and deleted.\n" ) )
				# eatTools:::sunk ( paste ( "Skript beendet.\n" ) )
				eatTools:::sunk ( paste ( "Script terminated.\n" ) )
				stop()
		} else {
				# eatTools:::sunk ( paste ( "Modell(e)", paste ( delete , collapse = ", " ) , "ist/sind misspezifiziert und wurde(n) aus Automatisierungsliste gelöscht.\n" ) )
				eatTools:::sunk ( paste ( "Model(s)", paste ( delete , collapse = ", " ) , "is/are misspecified and are removed from automate list.\n" ) )
				names ( ret ) <- names ( model.specs )
		}
					
		# returnen
		return ( ret )
				
}
