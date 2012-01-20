# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.handle.global.grouping
# Description: Subroutine von automateModels
# Version: 	0.4.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# Optimierungsmöglichkeiten:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.handle.global.grouping <- function ( item.grouping , person.grouping , select.item.group , select.person.group ) {
		
		
		# Plausicheck
		if ( !is.data.frame ( item.grouping ) & !is.null ( select.item.group ) ) 
				# stop ( paste ( ".automateModels.handle.global.grouping: Die Kombination der Parameter 'item.grouping' 
								# und 'select.item.group' ist nicht zulässig." ) )
				stop ( paste ( ".automateModels.handle.global.grouping: The combination of parameters 'item.grouping' 
								and 'select.item.group' is not feasible." ) )


		if ( !is.data.frame ( person.grouping ) & !is.null ( select.person.group ) ) 
				# stop ( paste ( ".automateModels.handle.global.grouping: Die Kombination der Parameter 'person.grouping' 
								# und 'select.person.group' ist nicht zulässig." ) )
				stop ( paste ( ".automateModels.handle.global.grouping: The combination of parameters 'person.grouping' 
								and 'select.person.group' is not feasible." ) )

								
		if ( ! is.null ( select.item.group ) ) {
					
			# Listen wenn keine Liste
			if ( ! inherits ( select.item.group , "list" ) ) select.item.group <- list ( select.item.group )
		
			# Erzeugen der nicht globalen Item-Grouping-Informationen aus der globalen
			item.grouping <- mapply ( function ( select.item.group , item.grouping ) {
								stopifnot ( all ( select.item.group %in% colnames ( item.grouping ) ) )
								if ( !is.null ( select.item.group ) ) keep.cols <- c ( 1 , which ( colnames ( item.grouping ) %in% select.item.group ) ) else keep.cols <- colnames ( item.grouping ) 
								ret <- item.grouping [ , keep.cols , drop = FALSE ]
								stopifnot ( ( length ( select.item.group ) + 1 ) ==  length ( colnames ( ret ) ) ) 
								return ( ret ) 
						} , select.item.group , MoreArgs = list ( item.grouping ) , SIMPLIFY = FALSE )
	
		}

		if ( ! is.null ( select.person.group ) ) {
					
			# Listen wenn keine Liste
			if ( ! inherits ( select.person.group , "list" ) ) select.person.group <- list ( select.person.group )
		
			# Erzeugen der nicht globalen Person-Grouping-Informationen aus der globalen
			person.grouping <- mapply ( function ( select.person.group , person.grouping ) {
								stopifnot ( all ( select.person.group %in% colnames ( person.grouping ) ) )
								if ( !is.null ( select.person.group ) ) keep.cols <- c ( 1 , which ( colnames ( person.grouping ) %in% select.person.group ) ) else keep.cols <- colnames ( person.grouping )
								ret <- person.grouping [ , keep.cols , drop = FALSE ]
								stopifnot ( ( length ( select.person.group ) + 1 ) ==  length ( colnames ( ret ) ) ) 
								return ( ret ) 
						} , select.person.group , MoreArgs = list ( person.grouping ) , SIMPLIFY = FALSE )
		
		}		
		
		# return setzen
		ret <- list ( item.grouping = item.grouping , person.grouping = person.grouping )

		# returnen
		return ( ret )
		
}
