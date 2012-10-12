# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.adjust.for.regression
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-09-08
# Author:    Martin Hecht, Karoline Sachse
# 2012-08-21 KS
# CHANGED: für Personenseite (PVs) implementiert
# 2012-08-17 KS
# CHANGED: improved efficiency
# 2012-08-16 KS
# CHANGED: für alle dimensionen und analysen implementiert
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
		sunk ( paste ( f.n , "Item parameter estimates and plausible values will be centered on person mean." ) ) 
		res1 <- results
		analyses <- names(results)
		for(k in seq(along =analyses)) {
			dimensions <- names(results[[k]])
			badj <- itNam <- list()
			for(i in seq(along =dimensions)) {
				# item side
				badj[[dimensions[i]]] <- unlist(lapply(results[[k]][[dimensions[i]]][[1]][[1]], function(ll) {ll$b})) - results[[k]][[dimensions[i]]][[1]]$descriptives$pv$pv.mean
				itNam[[dimensions[i]]] <- names(results[[k]][[dimensions[i]]][[1]][[1]])
					make.badj <- function ( item , bad ) {
						i.dummy <- results[[k]][[dimensions[i]]][[1]][[1]][[item]]
						i.dummy[["b.adj"]] <- bad
						return(i.dummy)
					}
				i.dummies <- mapply ( make.badj , itNam[[dimensions[i]]], badj[[dimensions[i]]] , SIMPLIFY = FALSE )
				do <- paste ( "results[[", k, "]][[dimensions[", i, "]]][[1]][[1]]<-list(" , paste ( itNam[[dimensions[i]]], " = i.dummies$" , itNam[[dimensions[i]]], collapse = "," , sep="") , ")" , sep = "" )
				eval ( parse ( text = do ) )
				# person side
				pvadj <- pNam <- list()
				for(j in seq(along=names(results[[k]][[dimensions[i]]][[1]][[2]][[1]]$pv))) {
					pvadj[[dimensions[i]]] <- unlist(lapply(results[[k]][[dimensions[i]]][[1]][[2]], function(ll) {ll$pv[[paste("pv.", j, sep="")]]})) - results[[k]][[dimensions[i]]][[1]]$descriptives$pv$pv.mean
					pNam[[dimensions[i]]] <- names(results[[k]][[dimensions[i]]][[1]][[2]])
						make.pvadj <- function ( pers , pvad ) {
							p.dummy <- results[[k]][[dimensions[i]]][[1]][[2]][[pers]]
							p.dummy$pv[[paste("pv.",j,".adj", sep="")]] <- pvad
							return(p.dummy)
						}
					p.dummies <- mapply ( make.pvadj , pNam[[dimensions[i]]], pvadj[[dimensions[i]]] , SIMPLIFY = FALSE )
					results[[k]][[dimensions[i]]][[1]][[2]]<-p.dummies
				}
			}
		}
						# Schleife raus
						# for(jj in itNam[[dimensions[i]]]) {
					# results[[k]][[dimensions[i]]][[1]][[1]][[jj]]$b.adj <- unname(badj[[dimensions[i]]][jj])
				# }
		# Ausgabe
		sunk ( " done\n\n" )
		
		# returnen 
		return ( results )
		
}



