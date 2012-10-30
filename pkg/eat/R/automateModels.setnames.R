# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.setnames
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-07-16
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.setnames <- function ( model.specs ) {

		### i.model.name und p.model.name (Bezeichnung der Skala/Skalen bzw. Gruppe/Gruppen) setzen
		model.specs$i.model.name <- mapply ( function ( item.grouping ) {
								paste ( colnames ( item.grouping[-1] ) , collapse = "-" )
						} , model.specs$item.grouping , SIMPLIFY=FALSE )
		model.specs$p.model.name <- mapply ( function ( person.grouping ) {
								paste ( colnames ( person.grouping[-1] ) , collapse = "-" )
						} , model.specs$person.grouping , SIMPLIFY=FALSE )		

		### dichotome DIF - "Variable" setzen
		model.specs$dif.dich <- mapply ( function ( dif ) {
								if ( is.null ( dif ) ) return( "no_dif" ) else return( "dif" )
						} , model.specs$dif , SIMPLIFY=FALSE )
		
		### dif.name setzen
		model.specs$dif.name <- mapply ( function ( dif ) {
								if ( is.null ( dif ) ) return( NULL ) else return( paste ( dif , collapse = "_" ) )
						} , model.specs$dif , SIMPLIFY=FALSE )						
						
		### dichotome Regression - "Variable" setzen
		model.specs$regression.dich <- mapply ( function ( regression ) {
								if ( is.null ( regression ) ) return( "no_regression" ) else return( "regression" )
						} , model.specs$regression , SIMPLIFY=FALSE )
		
		### regression.name setzen
		model.specs$regression.name <- mapply ( function ( regression ) {
								if ( is.null ( regression ) ) return( NULL ) else return( paste ( regression , collapse = "_" ) )
						} , model.specs$regression , SIMPLIFY=FALSE )
		
		### dichotome anchor - "Variable" setzen
		model.specs$anchor.dich <- mapply ( function ( dif ) {
								if ( is.null ( dif ) ) return( "no_anchor" ) else return( "anchor" )
						} , model.specs$anchor , SIMPLIFY=FALSE )	
	
	# returnen
	return ( model.specs ) 
			
}
