# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.reduce.item.and.person.grouping
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-08-17
# Author:    Martin Hecht
# Change Log:
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.reduce.item.and.person.grouping <- function ( grouping ) {

	mapply ( function ( grouping ) {
	
			stopifnot ( is.data.frame ( grouping ) ) 
			
			keep.items <- apply ( grouping , 1 , function ( zeile ) {
				if ( any ( zeile[-1] == 1 ) ) return ( TRUE ) else return ( FALSE )
			} )

			grouping [ keep.items , , drop = FALSE ]
	
		} , grouping , SIMPLIFY = FALSE )
			
}
