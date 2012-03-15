
# kombiniert Vektor-Elemente, so dass quasi in Matrix gedacht unteres Triangle (ohne Diagonale ensteht)
combVec <- function ( v ) {
		v <- expand.grid ( v , v , stringsAsFactors = FALSE )
		v <- v [ ! v[,1] == v[,2] , ]
		v <- cbind ( v , "comb" = apply ( v , 1 , function ( v ) paste ( sort ( as.character ( v ) ) , collapse = "|" ) ) , stringsAsFactors = FALSE )
		v <- v [ duplicated ( v$comb ) , ]
}

commonItems <- function ( dat , group.var , na = NA , uncommon = FALSE , simplify = TRUE ) {
			
		# Checks
		stopifnot ( is.data.frame ( dat ) )
		stopifnot ( length ( group.var ) == 1 )
		if ( is.numeric ( group.var ) ) {
				w <- which ( colnames ( dat ) == group.var )
				stopifnot ( ! identical ( w , integer(0) ) )
				group.var <- colnames ( dat ) [ w ]
		} else if ( is.character ( group.var ) ) stopifnot ( group.var %in% colnames ( dat ) ) else stop ( "group.var is not numeric or character" )

		# Gruppen auf Gruppenvariable
		groups <- sort ( unique ( as.character ( dat[,group.var] ) ) )
		
		# nur ab 2 Gruppen weitermachen , sonst NULL zurückgeben	
		if ( length ( unique ( dat[,group.var] ) ) >= 2 ) {
		
				# na in Datensatz auf NA
				if ( ! is.na ( na ) ) {
						# mis.rule <- eval ( parse ( text = paste ( "list(",na,"=NA)" ) ) )
						# dat <- collapseMissings ( dat , mis.rule ) # das hier wäre am besten
						### dat <- collapseMissings ( dat , items = colnames ( dat ) )
# !!! temporäres workaraound, bis collapseMissings überarbeitet		
						dat[dat == na] <- NA
				}
			
				# Gruppenspezifischer Datensatz
				dl <- mapply ( function ( gr , group.var , d ) d[ d[,group.var] == gr , !colnames(d)%in%group.var ] ,
							   groups , MoreArgs = list ( group.var , dat ) , SIMPLIFY = FALSE )
				
				# Datensätze reduzieren
				dl <- mapply ( rmNA , dl , SIMPLIFY = FALSE )
			
				# Gruppen crossen
				gr <- combVec ( groups )
				
				# common / uncommon Items
				.fun <- function ( gr1 , gr2 , dl , uc ) {
						g1 <- dl[[gr1]]
						g2 <- dl[[gr2]]
						
						# common
						is <- intersect ( colnames ( g1 ) , colnames ( g2 ) )
					
						# uncommon
						if ( uc ) {
								uc1 <- colnames ( g1 )[ !colnames(g1) %in% is ]
								uc2 <- colnames ( g2 )[ !colnames(g2) %in% is ]
								is <- list ( is , uc1 , uc2 )
								names ( is ) <- c ( "common" , gr1 , gr2 )
						}
						
						return ( is )
				}
				cI <- mapply ( .fun , gr[,1] , gr[,2] , MoreArgs = list ( dl , uncommon ) , SIMPLIFY = FALSE )
				names ( cI ) <- gr$comb
		
		} else cI <- NULL
		
		# simplifizieren ( nur Vektor mit common ) wenn 2 Gruppen und uncommon FALSE
		if ( length ( cI ) == 1 & !uncommon ) cI <- cI[[1]]
		
		return ( cI )
		
}
