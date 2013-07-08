
get.tam.effects <- function ( tamObj , tam.seObj = NULL , tam.wleObj = NULL ) {

		# Teildatensätze initialisieren
		dfr1 <- data.frame ( 'Var1' = NA , 'Var2' = NA  , 'type' = NA  , 'random.group' = NA  , 'parameter' = NA  , 'value' = NA )
		dfr1 <- dfr2 <- dfr3 <- dfr4 <- dfr5 <- dfr6 <- dfr7 <- dfr1[-1,,drop = FALSE]
		
		# Items
		for ( r in seq ( along = rownames( tamObj$item ) ) ) {
				dfr1[r,'Var1'] <- as.character ( tamObj$item[r,'item'] )
				dfr1[r,'type'] <- 'fixed'
				dfr1[r,'parameter'] <- 'est'
				dfr1[r,'value'] <- tamObj$item[r,'AXsi_.Cat1']
		}

		# Varianzen
		varcov <- tamObj$variance
		for ( i in seq ( along = varcov ) ) {
				dfr2[i,'Var1'] <- colnames ( varcov ) [i]
				dfr2[i,'type'] <- "random"
				dfr2[i,'random.group'] <- "person"
				dfr2[i,'parameter'] <- "var"
				dfr2[i,'value'] <- varcov[i,i]
		}

		# Personen, EAP
		for ( r in seq ( along = rownames( tamObj$person ) ) ) {
				dfr3[r,'Var1'] <- as.character ( tamObj$person[r,'pid'] )
				dfr3[r,'type'] <- 'fixed'
				dfr3[r,'parameter'] <- 'eap'
				dfr3[r,'value'] <- tamObj$person[r,'EAP']
		}

		# Personen, EAP
		for ( r in seq ( along = rownames( tamObj$person ) ) ) {
				dfr4[r,'Var1'] <- as.character ( tamObj$person[r,'pid'] )
				dfr4[r,'type'] <- 'fixed'
				dfr4[r,'parameter'] <- 'eap.sd'
				dfr4[r,'value'] <- tamObj$person[r,'SD.EAP']
		}
		
		# Item Standardfehler
		if ( ! is.null ( tam.seObj ) ) {
				dfr5 <- data.frame ( 'Var1' = NA , 'Var2' = NA  , 'type' = NA  , 'random.group' = NA  , 'parameter' = NA  , 'value' = NA )
				dfr5 <- dfr5[-1,,drop = FALSE]
				for ( r in seq ( along = rownames( tam.seObj$item ) ) ) {
						dfr5[r,'Var1'] <- as.character ( tam.seObj$xsi[r,"item"] )
						dfr5[r,'type'] <- 'fixed'
						dfr5[r,'parameter'] <- 'se'
						dfr5[r,'value'] <- tam.seObj$xsi[r,"se"]
				}
		} else {
				dfr5 <- NULL
		}
		
		### Model ###
		# EAP Rel
		dfr6[1,'type'] <- 'model'
		dfr6[1,'parameter'] <- 'eap.rel'
		dfr6[1,'value'] <- tamObj$EAP.rel
		# Deviance
		dfr6[2,'type'] <- 'model'
		dfr6[2,'parameter'] <- 'deviance'
		dfr6[2,'value'] <- tamObj$ic$deviance
		# Npars
		dfr6[3,'type'] <- 'model'
		dfr6[3,'parameter'] <- 'Npars'
		dfr6[3,'value'] <- tamObj$ic$Npars
		# AIC
		dfr6[4,'type'] <- 'model'
		dfr6[4,'parameter'] <- 'AIC'
		dfr6[4,'value'] <- tamObj$ic$AIC
		# BIC
		dfr6[5,'type'] <- 'model'
		dfr6[5,'parameter'] <- 'BIC'
		dfr6[5,'value'] <- tamObj$ic$BIC
		
		# WLE
		if ( ! is.null ( tam.wleObj ) ) {
				for ( r in seq ( along = rownames( tam.wleObj ) ) ) {
						dfr7[r,'Var1'] <- as.character ( tam.wleObj[r,"pid"] )
						dfr7[r,'type'] <- 'fixed'
						dfr7[r,'parameter'] <- 'wle'
						dfr7[r,'value'] <- tam.wleObj[r,"theta"]
				}
		} else {
				dfr7 <- NULL
		}		
		# WLE Rel an dfr6 (model ran)
		dfr6[nrow(dfr6)+1,'type'] <- 'model'
		dfr6[nrow(dfr6)+1,'parameter'] <- 'WLE.rel'
		dfr6[nrow(dfr6)+1,'value'] <- tam.wleObj$WLE.rel[1]
		
		# Ergebnisdatensatz zusammenbauen
		dfr.list <- list ( dfr1 , dfr5 , dfr2 , dfr3 , dfr4 , dfr7 , dfr6 )
		dfr <- do.call ( "rbind" , dfr.list )

		return ( dfr )
}
