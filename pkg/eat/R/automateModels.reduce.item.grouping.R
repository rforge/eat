# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.reduce.item.grouping
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-07-16
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.reduce.item.grouping <- function ( item.grouping , dataset ) {

	mapply ( function ( item.grouping , dataset ) {

			stopifnot ( is.data.frame ( item.grouping ) ) 
			stopifnot ( is.data.frame ( dataset ) ) 
			
			keep.rows <- which ( item.grouping[ , 1 ] %in% colnames ( dataset ) )

			item.grouping [ keep.rows , , drop = FALSE ]
	
		} , item.grouping , dataset , SIMPLIFY = FALSE )
			
}
