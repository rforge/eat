# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.reduce.person.grouping
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-07-16
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.reduce.person.grouping <- function ( person.grouping , dataset , id.name ) {

	mapply ( function ( person.grouping , dataset , id.name ) {

			stopifnot ( is.data.frame ( person.grouping ) ) 
			stopifnot ( is.data.frame ( dataset ) ) 
			stopifnot ( is.character ( id.name ) )
			stopifnot ( id.name %in% colnames ( dataset ) )
			
			keep.rows <- which ( person.grouping[ , 1 ] %in% dataset [ , id.name ] )

			person.grouping [ keep.rows , , drop = FALSE ]
	
		} , person.grouping , dataset , id.name , SIMPLIFY = FALSE )
			
}
