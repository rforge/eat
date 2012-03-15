# 2011-12-18 MH
# ADDED: option 'xlsx.path' to detect.suppression
# 2011-12-17 MH
# NEW: function detect.suppression
# 0000-00-00 AA

.calc.supression <- function ( pred , preds , dep , d ) {

		# Korrelation predictor mit Kriterium
		cor_pred_c <- cor ( d[,pred] , d[,dep] , use = "pairwise.complete.obs" )
		
		### Multiple Regression, Prädiktor wird durch ander Prädiktoren vorhergesagt
		# additional.preds <- preds [ ! preds %in% pred ]
		# pred.str <- paste ( sapply ( additional.preds , function ( pred ) { paste ( "d[,'" , pred , "']" , sep = "" ) } ) , collapse = " + " )
		lm.str <- paste ( pred , " ~ " , paste ( preds , collapse = " + " ) , sep = "" )
		# Modell
		m <- lm ( formula = lm.str , dat = d )
		
		# vorhergesagte Werte
		pred_fitted <- unname ( m$fitted.values )
		# Korrelation vorhergesagte Werte mit Kriterium
		cor_pred_fitted_c <- cor ( pred_fitted , d[,dep] , use = "pairwise.complete.obs" )
		
		# Multiple Korrelation des Prädiktors mit den anderen Prädiktoren
		r.sq_pred <- summary(m)$r.sq

		# wenn multiple Regression, dann multiple.reg = TRUE
		if ( length ( preds ) > 1 ) multiple.reg = TRUE else multiple.reg = FALSE
		
		# Dataframe bauen
		e <- data.frame ( multiple.reg , "dep" = dep , "pred" = pred , "preds" = paste ( preds , collapse = "+" , sep = "" ) , cor_pred_c , cor_pred_fitted_c , r.sq_pred , stringsAsFactors = FALSE )
		rownames ( e ) <- paste ( e$dep , "~" , e$preds , "|" , e$pred , sep = "" )
		
		# Berechnungen
		e$rterm.minus <- ( e$cor_pred_fitted_c * ( 1 - sqrt ( ( 1 - e$r.sq_pred ) ) ) ) / sqrt ( e$r.sq_pred )
		e$rterm.plus <- ( e$cor_pred_fitted_c * ( 1 + sqrt ( ( 1 - e$r.sq_pred ) ) ) ) / sqrt ( e$r.sq_pred )
		e$rterm.minus.diff <- e$rterm.minus - e$cor_pred_c 
		e$rterm.plus.diff <- e$cor_pred_c - e$rterm.plus
		e$rterm.minus.log <- e$cor_pred_c < e$rterm.minus
		e$rterm.plus.log <- e$cor_pred_c > e$rterm.plus
		e$suppression <- e$rterm.minus.log | e$rterm.plus.log
		
		return ( e )
}

detect.suppression <- function ( dat , dependent , independent , full.return = FALSE , xlsx.path = NULL ) {

		# alle bivariaten Prädiktorkombinationen
		komb <- expand.grid ( independent , independent )
		komb <- komb [ ! komb[,1] == komb[,2] , ]
		
		# als Liste
		komb <- apply ( komb , 1 , function ( zeile ) {
				e1 <- unname ( zeile[1] )
				e2 <- unname ( zeile[2] )
				list ( e1 , e2 )
		} )

		# "Diagonal"-Elemente, also ein Prädiktor vs. alle (multiple Regression)
		.fun1 <- function ( pred , preds ) {
				list ( pred , preds[ !preds %in% pred ] )
		}
		komb <- c ( komb , mapply ( .fun1 , independent , MoreArgs = list ( independent ) , SIMPLIFY = FALSE ) )

		# über Kombinationen Schleife
		.fun2 <- function ( komb , dep , d ) {
				.calc.supression ( komb[[1]] , komb[[2]] , dep , d )
		}
		r <- do.call ( "rbind" , mapply ( .fun2 , komb , MoreArgs = list ( dependent , dat ) , SIMPLIFY = FALSE , USE.NAMES = FALSE ) )

		# wenn angefordert oder wenn Excel geschrieben werden soll, dann die Ergebnis-Matrix bauen
		if ( ( ! full.return ) | ( ! is.null ( xlsx.path ) ) ) {
				### Ergebnis-Matrix
				r2 <- r [ , c ( "pred" , "preds" , "suppression" ) ]
				r2$col <- r2$preds
				# Diagonale mit multiplen Regressionen besetzen
				r2$col[ r$multiple.reg ] <- r2$pred[ r$multiple.reg ]
				# Matrix erzeugen
				r2 <- long2matrix ( r2 [ , c ( "pred" , "col" , "suppression" ) ] , sort = FALSE )
		}
		
		### Excels schreiben
		if ( ! is.null ( xlsx.path ) ) {
				tried1 <- try ( write.xlsx2(  r2 , xlsx.path , sheetName="suppression_matrix", formatTemplate=NULL,
								col.names=TRUE, row.names=TRUE, append=FALSE ) , silent = TRUE )
				if ( inherits ( tried1 , "try-error" ) ) {
						warning ( paste ( "File" , xlsx.path , "could not be written." ) )
				} else {
						tried2 <- try ( write.xlsx2(  r , xlsx.path , sheetName="suppression_data", formatTemplate=NULL,
						col.names=TRUE, row.names=TRUE, append=TRUE ) , silent = TRUE )		
						if ( inherits ( tried1 , "try-error" ) ) warning ( paste ( "Sheet 'suppression_data' could not be appended to " , xlsx.path , "." , sep = "" ) )
				}
		}
		
		# Rückgabe
		if ( full.return ) return ( r ) else return ( r2 )

}







