
exploreDesign <- function ( dat , na = NA , id = NULL , itemsPerPerson = TRUE , personsPerItem = TRUE ) {

		# wenn keine Id da
		if ( is.null ( id ) ) {
				dat$row <- as.character ( seq ( along = rownames ( dat ) ) )
				id = "row"
		}
		stopifnot ( any ( colnames ( dat ) %in% id ) )
		
		if ( ! all ( !itemsPerPerson , !personsPerItem ) ) {
		
				# items
				items <- colnames ( dat ) [ ! colnames ( dat ) %in% id ]
				
				# persons
				persons <- dat[,id]
				
				# Items jeder Person
				if ( itemsPerPerson ) {
						rows <- mapply ( function ( p , d , id ) { d [ d[,id] == p , items , drop = FALSE ] } , persons , MoreArgs = list ( dat , id ) , SIMPLIFY = FALSE )
						ret <- items.per.person <- mapply ( function ( row , na ) {
								if ( is.na ( na ) ) w <- sapply ( row , is.na ) else w <- sapply ( row , function ( z ) z == na )
								colnames ( row ) [ !w ]
						} , rows , MoreArgs = list ( na ) , SIMPLIFY = FALSE )
				}
			
				# Personen jedes Items
				if ( personsPerItem ) {
						ret <- persons.per.item <- mapply ( function ( d , idv , na ) {
								if ( is.na ( na ) ) w <- is.na(d) else w <- d == na
								idv[ !w ]
						} , dat [ , items , drop = FALSE ] , MoreArgs = list ( dat [ , id ] , na ) , SIMPLIFY = FALSE )
				}
				
				# Rückgabe
				if ( all ( itemsPerPerson , personsPerItem ) ) ret <- list ( "itemsPerPerson" = items.per.person , "personsPerItem" = persons.per.item ) 
		
		} else ret <- NULL
		
		return ( ret ) 
}
