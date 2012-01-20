# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.i.p.model.default
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#			14.10.2011 MH: Ausgaben auf Englisch
# Optimierungsmöglichkeiten:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.i.p.model.default <- function ( item.grouping , person.grouping ) {

		### Defaults anhand von Item- und Personen-Grouping setzen			
		# i.model
		i.model <- mapply ( function ( item.grouping ) {
						if ( ncol ( item.grouping ) > 2 ) return ( "multidim" )
						else if	( ncol ( item.grouping ) == 2 ) return ( "unidim" )
						else stop ( "item.grouping has less than 2 columns" )
					} , item.grouping , SIMPLIFY = FALSE )			
		# p.model
		p.model <- mapply ( function ( person.grouping ) {
						if ( ncol ( person.grouping ) > 2 ) return ( "multigroup" ) 
						else if	( ncol ( person.grouping ) == 2 ) return ( "singlegroup" )
						else stop ( "person.grouping has less than 2 columns" )
					} , person.grouping , SIMPLIFY = FALSE )			

		# return setzen
		ret <- list ( i.model = i.model , p.model = p.model )

		# returnen
		return ( ret )
		
}
