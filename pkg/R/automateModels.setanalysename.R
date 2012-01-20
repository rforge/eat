# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.setanalysename
# Description: Subroutine von automateModels
# Version: 	0.5.0
# Status: beta
# Release Date: 	2011-08-26
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.setanalysename <- function ( model.specs , analyse.name.elements ) {

		# Plausichecks
		stopifnot ( !is.null ( model.specs$i.model.name ) )
		stopifnot ( !is.null ( model.specs$p.model.name ) )
		stopifnot ( !is.null ( model.specs$dif.name ) )
		stopifnot ( !is.null ( model.specs$regression.name ) )
	
		# Namen, die NULL sind, setzen
		.fun <- function ( analyse.name , i.model.name , p.model.name , dif.name , regression.name , anchor.dich , analyse.name.elements ) {
		
					# Setzungen
					element.delimiter <- "__"
	
					if ( is.null ( analyse.name ) ) {
							
							els <- NULL
							
							for ( name in analyse.name.elements ) {
								if ( name == "scale" & name %in% analyse.name.elements & !is.null( i.model.name ) ) els <- c ( els , i.model.name )
								if ( name == "group" & name %in% analyse.name.elements & !is.null( p.model.name ) ) els <- c ( els , p.model.name )
								if ( name == "dif" & name %in% analyse.name.elements & !is.null( dif.name ) ) els <- c ( els , paste ( "DIF_" , dif.name , sep = "" ) )
								if ( name == "regression" & name %in% analyse.name.elements & !is.null( regression.name ) ) els <- c ( els , paste ( "REG_" , regression.name , sep = "" ) )
								if ( name == "anchor" & name %in% analyse.name.elements & anchor.dich == "anchor" ) els <- c ( els , toupper ( anchor.dich ) )
							}
							
							new.name <- paste ( els , collapse = element.delimiter )
						
							return ( new.name )
	
					} else {
							return ( analyse.name )
					}
			
			}
	
		mapply ( .fun , model.specs$analyse.name , model.specs$i.model.name , model.specs$p.model.name , 
				model.specs$dif.name , model.specs$regression.name , model.specs$anchor.dich , 
				MoreArgs = list ( analyse.name.elements ) , SIMPLIFY=TRUE )

}

