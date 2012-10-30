# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.setsubfolder
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-0?-??
# Author:    Martin Hecht
# Change Log:
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.setsubfolder <- function ( model.specs , folder , subfolder.order , subfolder.mode ) {

	.setsubfolder <- function ( model.specs , ref.num , subfolder.order , subfolder.order.num , keep , subfolder.mode , folder ) {
	
			if ( subfolder.order.num %in% seq ( along = subfolder.order ) ) {
			
					if ( length ( unique ( model.specs[[subfolder.order[subfolder.order.num]]][keep] ) ) == 1 ) {
							
							if ( subfolder.mode == "intelligent" ) ret <- NULL
							else if ( subfolder.mode == "full" ) ret <- unlist (  model.specs[[ subfolder.order[subfolder.order.num] ]][ref.num]  )
					
					} else {
					
							ret <- model.specs[[ subfolder.order[subfolder.order.num] ]][ref.num]
							
							keep <- which ( model.specs[[ subfolder.order[subfolder.order.num] ]]
													%in% model.specs[[ subfolder.order[subfolder.order.num] ]][ref.num] )
				
					}
	
					return ( c ( ret , .setsubfolder ( model.specs , ref.num , subfolder.order , subfolder.order.num + 1 , keep , subfolder.mode , folder ) ) ) 
			}
	}
	
	ret <- mapply ( function ( listelnum , subfolder.mode , folder ) {
							if ( subfolder.mode == "none" ) {
								folder
							} else {
									paste ( folder , 
										paste ( .setsubfolder ( model.specs , 
												listelnum , subfolder.order , 1 , seq ( along = model.specs$i.model ) , subfolder.mode , folder )
											, collapse = "/" )
										, sep = "/" )
									} 

						} ,	seq ( along = model.specs$i.model ) , MoreArgs = list( subfolder.mode, folder ) , SIMPLIFY = FALSE )
			
	# letzten "/" entfernen, das findet automateConquestModel besser
	ret <- mapply ( function ( ret ) {
					if ( regexpr ( "^.*/$" , ret ) == 1 ) substr ( ret , 1 , nchar ( ret ) - 1 ) else ret
			} ,	ret , SIMPLIFY = FALSE )	
	
	# returnen
	return ( ret ) 
			
}
