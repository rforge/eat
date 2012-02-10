
commonItems <- function ( data , group.var , missing = NA ) {
			
		# Checks
		stopifnot ( is.data.frame ( data ) )
		stopifnot ( length ( group.var ) == 1 )
		if ( is.numeric ( group.var ) ) {
				w <- which ( colnames ( data ) == group.var )
				stopifnot ( ! identical ( w , integer(0) ) )
				group.var <- colnames ( data ) [ w ]
		} else if ( is.character ( group.var ) ) stopifnot ( group.var %in% colnames ( data ) ) else stop ( "group.var is not numeric or character" )

		# missing in Datensatz auf NA
		if ( ! is.na ( missing ) ) {
				# mis.rule <- eval ( parse ( text = paste ( "list(",missing,"=NA)" ) ) )
				# data <- collapseMissings ( data , mis.rule ) # das hier wäre am besten
				### data <- collapseMissings ( data , item.names = colnames ( data ) )
# !!! temporäres workaraound			
				data[data == missing] <- NA
		}
	
		# Gruppen
		groups <- unique ( as.character ( data[,group.var] ) )
	
		# Gruppenspezifischer Datensatz
		dl <- mapply ( function ( gr , group.var , d ) d[ d[,group.var] == gr , !colnames(d)%in%group.var ] ,
					   groups , MoreArgs = list ( group.var , data ) , SIMPLIFY = FALSE )
		
		# Datensätze reduzieren
		dl <- mapply ( rmNA , dl , SIMPLIFY = FALSE )
		
		# Gruppen crossen
		gr <- expand.grid ( groups , groups , stringsAsFactors = FALSE )
		gr <- gr [ ! gr[,1] == gr[,2] , ]
		gr <- gr [ 1:(nrow(gr)/2) , ]
		
		# commonItems 
		cI <- mapply ( function ( gr1 , gr2 , dl ) intersect ( colnames ( dl[[gr1]] ) , colnames ( dl[[gr2]] ) ) , gr[,1] , gr[,2] , MoreArgs = list ( dl ) , SIMPLIFY = FALSE )
		names ( cI ) <- apply ( gr , 1 , paste , collapse="|" )
		
		return ( cI )
		
}
