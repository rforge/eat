# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.grouping.vars.to.grouping
# .gen.indicators
# Description: Subroutine von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.grouping.vars.to.grouping <- function ( dataframe , vars = NULL , include.all = FALSE , prikey.name ) { 

		# Funktionsname für Meldungen
		f. <- ".automateModels.grouping.vars.to.grouping"
		f.n <- paste ( f. , ":" , sep = "" )

		# Defaults
		if ( is.null ( vars ) ) {
				vars <- colnames ( dataframe )
		}
		
		include.all <- rep ( include.all , out.length = length ( vars ) )
		if ( ! length ( vars ) == length ( include.all ) ) {
				eatTools:::sunk ( paste ( f.n , "Error:" , "! length ( vars ) == length ( include.all )" ) )
				stop ( )
		}

		# Checks
		if ( ! length ( prikey.name ) == 1 ) {
				eatTools:::sunk ( paste ( f.n , "Error:" , "! length ( prikey.name ) == 1" ) )
				stop ( )
		}		
		if ( ! all ( c ( prikey.name , vars ) %in% colnames ( dataframe ) ) ) {
				eatTools:::sunk ( paste ( f.n , "Error:" , "! all ( c ( prikey.name , vars ) %in% colnames ( dataframe ) )" ) )
				stop ( )
		}			

		# erzeugen
		grouping <- data.frame (
				mapply ( function ( vars , include.all , dfr ) {
								cats <- sort ( unique ( dfr [ , vars ] ) )
								#cats <- unique ( dfr [ , vars ] )
								ret <- mapply ( function ( cats ) {
										as.numeric ( dfr [ , vars ] == cats )
								} , cats , SIMPLIFY = FALSE )
								names ( ret ) <- paste ( vars , cats , sep = "." )
								if ( include.all ) {
										ret <- c ( ret , rep ( 1 , length ( ret[1] ) ) ) 
										names ( ret )[ length ( ret ) ] <- paste ( vars , ".all" , sep = "" )
								}
								return ( ret )
				} , vars , include.all , MoreArgs = list ( dataframe ) , SIMPLIFY = FALSE , USE.NAMES = FALSE )		
		, stringsAsFactors = FALSE )
	
		grouping <- cbind ( as.character ( dataframe[ , prikey.name ] ) , grouping , stringsAsFactors=FALSE )
		colnames ( grouping )[1] <- prikey.name
		return ( grouping )
		
}

# .automateModels.grouping.vars.to.grouping <- function ( dataset , grouping.vars , grouping.vars.include.all , prikey.name ) {
		# .gen.indicators ( dataset , grouping.vars , grouping.vars.include.all , prikey.name )
# }








