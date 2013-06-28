
tabulate.vars <- function ( d ) {

		tabl <- sapply ( d , table , simplify = FALSE )
		if ( is.null ( names ( tabl ) ) ) {
				if ( !is.null ( colnames ( d ) ) ) {
						names ( tabl ) <- colnames ( d )
				} else {
						names ( tabl ) <- seq ( along = tabl )
				}
		}
		
		cat.names <- sort ( unique ( unname ( do.call ( "c" , sapply ( tabl , attr , "dimnames" , simplify = TRUE ) ) ) ) )

		make.table <- function ( tab , tabname , cat.names ) { 
				x <- tab[cat.names]
				attr ( x , "dimnames" ) <- list(cat.names)
				x <- t ( data.frame ( x ) )
				x[ is.na ( x ) ] <- 0
				rownames ( x ) <- tabname
				x <- cbind ( data.frame ( "variable" = tabname , stringsAsFactors = FALSE ) , x )
				return ( x )
		}
		tab.list <- mapply ( make.table , tabl , names ( tabl ) , MoreArgs = list ( cat.names ), SIMPLIFY = FALSE )
		tab <- do.call ( "rbind" , tab.list )
		rownames ( tab ) <- seq ( along = rownames ( tab ) )
		
		return ( tab )
}
