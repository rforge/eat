# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.makeanalysename
# Description: Subroutine von automateModels
# Version: 	0.5.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgabe auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.makeanalysename <- function ( model.specs , analyse.name.prefix , analyse.name.elements ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.makeanalysename"
		f.n <- paste ( f. , ":" , sep = "" )

		# Plausicheck
		if ( ! all ( welche <- ( analyse.name.elements %in% ( default <- c ( "scale" , "group" , "dif" , "regression" , "anchor" ) ) ) ) ) {
				# eatTools:::sunk ( paste ( f.n , "Folgende Elemente nicht in 'analyse.name.elements' unbekannt:" , paste ( analyse.name.elements[!welche] , collapse = ", " ) , "\n") )
				# eatTools:::sunk ( paste ( f.n , "Gültige Werte sind:" , paste ( default , collapse = ", " ) , "\n") )
				eatTools:::sunk ( paste ( f.n , "Unknown elements in 'analyse.name.elements':" , paste ( analyse.name.elements[!welche] , collapse = ", " ) , "\n") )
				eatTools:::sunk ( paste ( f.n , "Valid values are:" , paste ( default , collapse = ", " ) , "\n") )
				stop ( )
		}
		
		# analyse.name setzen wenn NULL
		analyse.name <- .automateModels.setanalysename ( model.specs , analyse.name.elements ) 

		# Präfix vorsetzen
		stopifnot ( is.character ( analyse.name.prefix ) | is.null ( analyse.name.prefix ) )
		if ( ! is.null ( analyse.name.prefix ) ) analyse.name <- paste ( analyse.name.prefix , analyse.name , sep = "__" )
		
		# wenn Schrott, dann einfach "analyse" nennen
		.fun <- function ( analyse.name ) {
				if ( nchar ( analyse.name ) == 0 | identical ( analyse.name , character(0) ) | is.null ( analyse.name ) ) analyse.name <- "analysis"
				return ( analyse.name )
		}
		analyse.name <- mapply ( .fun , analyse.name , USE.NAMES = FALSE )
		
		# analyse.name uniquifizieren
		analyse.name <- unname ( mapply ( function(element) {return(element)} , make.unique ( analyse.name , sep="" ) , SIMPLIFY = FALSE ) )
		
		# setzen
		model.specs$analyse.name <- analyse.name

		# returnen
		return ( model.specs )
		
}

