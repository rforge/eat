
# q3 ... q3 Dataframe im "matrix format"/wide/item x item
# erste Spalte muss "Dimension" sein
# Funktion gibt Descriptives gesplittet nach "Dimension" aus

q3.descriptives <- function ( q3 ) {

		# checks
		stopifnot ( is.data.frame ( q3 ) )
		stopifnot ( ncol ( q3 ) > 1 )
		if ( !colnames(q3)[1] == "Dimension" ) {
				warning ( "First column in 'q3' is not called 'Dimension', still it is treated as dimensions." )
				colnames(q3)[1] <- "Dimension"
		}
		
		# "Dimension" ggf. auf character
		q3 <- set.col.type ( q3 , list ( "character" = "Dimension" ) )

		# Gruppen
		groups <- unique ( q3$Dimension ) 
		
		# items nach Gruppen splitten
		items <- rownames(q3)
		names(items) <- q3$Dimension
		items <- mapply ( function ( groups , items ) unname(items[names(items)==groups]) , groups , MoreArgs = list ( items ), SIMPLIFY = FALSE )
		
		# q3 Vektor (je Gruppe) holen
		.fun <- function ( items , q3 ) {
				ret <- na.omit ( unname ( do.call ( c , q3[ rownames(q3) %in% items , colnames(q3) %in% items] ) ) )
				attributes(ret) <- NULL
				return(ret)
		}
		q3.group <- mapply ( .fun , items , MoreArgs = list ( q3 ), SIMPLIFY = FALSE )
		
		.fun1 <- function ( q3.group ) {
				describe ( q3.group )
		}
		q3.group.descr <- mapply ( .fun1 , q3.group , SIMPLIFY = FALSE )
		q3.group.descr2 <- data.frame ( do.call("rbind" , q3.group.descr ) , stringsAsFactors = FALSE )
		q3.group.descr2$var <- rownames( q3.group.descr2 )
		colnames(q3.group.descr2)[colnames(q3.group.descr2) == "var"] <- "group"
		
		return ( q3.group.descr2 )
		
}
