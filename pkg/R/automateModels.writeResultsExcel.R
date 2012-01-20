# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.writeResultsExcel
# Description: Subroutine von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.writeResultsExcel <- function ( results , analyse.name , folder , folder.aM , additional.item.props ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.writeResultsExcel"
		f.n <- paste ( f. , ":" , sep = "" )
		
		# Folder-Vektor mit Analyse-Namen benennen
		names ( folder ) <- analyse.name

		# Ausgabe
		# st <- paste ( "\n" , f.n , " Excels werden geschrieben " , sep = "" )
		st <- paste ( "\n" , f.n , " Excels are being written " , sep = "" )
		sunk ( "cat(st)" ) 

		
		# Gesamt-Excel
		check <- write.results.xlsx ( results , folder.aM , additional.item.props )
		#check <- TRUE

		# Einzel-Excels
		check <- c ( check ,
			mapply ( function ( results , name , folder , additional.item.props ) {
					sunk ( "cat('.')" )
					flush.console()
					results <- list ( results )
					names ( results ) <- name
				
					write.results.xlsx ( results = results , path = unname ( unlist ( folder[ name ] ) ) , additional_itemprops = additional.item.props )
			} , results , names ( results ) , MoreArgs = list ( folder , additional.item.props ) , SIMPLIFY = FALSE )
		)
		
		#stopifnot ( all ( check ) )
		
		# Ausgabe
		sunk ( " done\n" )
		
		# returnen 
		return ( check )
		
}



