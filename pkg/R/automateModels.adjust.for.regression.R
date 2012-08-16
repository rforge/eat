# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.adjust.for.regression
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-09-08
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.adjust.for.regression <- function ( results ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.adjust.for.regression"
		f.n <- paste ( f. , ":" , sep = "" )

		# Ausgabe
		sunk ( paste ( f.n , "Item difficulty is being centered on person mean 0." ) ) 
		
		.fun3 <- function ( results ) {
				results[[3]][[1]]$pv$pv.mean
		}
		means <- mapply ( .fun3 , results , SIMPLIFY = FALSE , USE.NAMES = FALSE )  
		
		# Hilfsfunktionen
		.fun2 <- function ( itemlist , means ) {
		
				itemlist$b <- itemlist$b - means
				# absb <- abs ( itemlist$b )
				# if ( absb > 4 ) bew <- "schlecht" else if ( absb > 3 ) bew <- "kritisch" else bew <- "gut"
				# itemlist$b.eval <- bew
				return(itemlist)
		}

		.fun1 <- function ( results , means ) {
				neu <- mapply ( .fun2 , results[[1]][[1]] , MoreArgs = list ( means ) , SIMPLIFY = FALSE )
				results[[1]][[1]] <- neu
				return ( results )
		}
		
		# adjustieren
		results2 <- mapply ( .fun1 , results , means , SIMPLIFY = FALSE )
		
		# Ausgabe
		sunk ( " done\n\n" )
		
		# returnen 
		return ( results2 )
		
}



