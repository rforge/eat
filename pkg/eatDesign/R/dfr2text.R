
# internal function; makes data.frame to text lines
dfr2text <- function ( dfr , blankRowNames = FALSE ) {

		# Original
		dfo <- dfr
		
		# alles auf Character
		# (nur für nicht "" colnames)
		cona <- colnames ( dfr )[!colnames ( dfr ) %in% ""]
		if ( length ( cona ) > 0 ) {
				do <- paste ( paste ( "if ( !is.character ( dfr$\"" , cona  , "\" ) ) dfr$\"" , cona , "\" <- as.character ( dfr$\"" , cona , "\" )" , sep = "" ) , collapse = "; " )
				eval ( parse ( text = do ) )
		}
	
		# nicht numerisch NAs auf <NA>, andere NA
		dfr[ is.na(dfr) & !is.numeric(dfo) ] <- "<NA>"
		
		# Colnames anpassen
		ma <- sapply ( dfr , function ( sp ) max ( nchar ( sp ) ) )

		f <- function ( nc , na ) {
			
				if ( ( n1 <- nchar ( na ) ) <= nc ) {
						einr <- paste ( rep ( " " , nc - n1 ) , collapse = "" )
						r <- paste ( einr , na , sep = "" )
				} else {
						r <- na
				}
				return ( r )
		}
		newnames <- mapply ( f , ma , names ( ma ) )
		
		colnames ( dfr ) <- newnames
		
		# Rownames
		rom <- max ( sapply ( rownames ( dfr ) , nchar ) )
		rostr <- paste ( rep ( " " , rom ) , collapse = "" )
		
		f2 <- function ( rn , l ) {
				if ( l >= ( n1 <- nchar ( rn ) ) ) {
						einr <- paste ( rep ( " " , l - n1 ) , collapse = "" )
						r <- paste ( rn , einr , sep = "" )
				} else {
						r <- rn
				}
				return ( r )
		}
		rownames(dfr) <- mapply ( f2 , rownames ( dfr ) , MoreArgs = list ( rom ) )
		
		firstline <- paste ( rostr , " " , paste ( colnames ( dfr ) , collapse = " " ) , "\n" , sep = "" )
		
		makeRows <- function ( r ) {
				f <- function ( el , na ) {
						if ( ( n1 <- nchar ( na ) ) >= ( n2 <- nchar ( el ) ) ) {
								einr <- paste ( rep ( " " , n1 - n2 ) , collapse = "" )
								r <- paste ( einr , el , sep = " " )
						} else {
								
								r <- el
						}
						return ( r )
				}
				paste ( mapply ( f , r , names ( r ) ) , collapse = "" )
		}
		x <- apply ( dfr , 1 , makeRows )
		if ( blankRowNames ) {
				rn <- sapply ( rownames ( dfr ) , function ( r ) paste ( rep ( " " , nchar	( r ) ) , collapse = "" ) )
		} else {
				rn <- rownames ( dfr )
		}
		x <- paste ( rn , x , sep = "" )
		x <- paste ( x , collapse = "\n" )
		x <- paste ( firstline , x , "\n" , sep = "" )

		return ( x )
}
