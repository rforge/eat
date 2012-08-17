# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.adjust.for.regression
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-09-08
# Author:    Martin Hecht
# 2012-08-16 KS
# CHANGED: 
# 0000-00-00 AA
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
		
		analyses <- names(results)
		for(k in seq(along =analyses)) {
			dimensions <- names(results[[k]])
			badj <- itNam <- list()
			for(i in seq(along =dimensions)) {
				badj[[dimensions[i]]] <- unlist(lapply(results[[k]][[dimensions[i]]][[1]][[1]], function(ll) {ll$b})) - results[[k]][[dimensions[i]]][[1]]$descriptives$pv$pv.mean
				itNam[[dimensions[i]]] <- names(results[[k]][[dimensions[i]]][[1]][[1]])
				for(jj in itNam[[dimensions[i]]]) {
					results[[k]][[dimensions[i]]][[1]][[1]][[jj]]$b.adj <- unname(badj[[dimensions[i]]][jj])
				}
			}
		}
		
		# Ausgabe
		sunk ( " done\n\n" )
		
		# returnen 
		return ( results )
		
}



