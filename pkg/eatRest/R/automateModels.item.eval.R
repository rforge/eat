# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.item.eval
# Description: Subroutine von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgabe auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.item.eval <- function ( results ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.item.eval"
		f.n <- paste ( f. , ":" , sep = "" )

		# Ausgabe
		# st <- paste ( "\n" , f.n , "Items werden bewertet " , sep = "" )
		st <- paste ( "\n" , f.n , " Items are being evaluated " , sep = "" )
		eatTools:::sunk ( "cat(st)" ) 
		
		ret <- mapply ( function ( results ) {
				 eatTools:::sunk ( "cat('.')" )
				 flush.console()
				 unlist ( item.eval ( list ( results ) ) , recursive = FALSE )
		} , results , SIMPLIFY = FALSE )

		# Ausgabe
		eatTools:::sunk ( " done\n" )
		
		# returnen 
		return ( ret )
		
}



