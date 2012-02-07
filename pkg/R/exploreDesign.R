
exploreDesign <- function ( data , missing = NA , id = NULL , itemsPerPerson = TRUE , personsPerItem = TRUE ) {

		# wenn keine Id da
		if ( is.null ( id ) ) {
				data$row <- as.character ( seq ( along = rownames ( data ) ) )
				id = "row"
		}
		stopifnot ( any ( colnames ( data ) %in% id ) )
		
		if ( ! all ( !itemsPerPerson , !personsPerItem ) ) {
		
				# items
				items <- colnames ( data ) [ ! colnames ( data ) %in% id ]
				
				# persons
				persons <- data[,id]
				
				# Items jeder Person
				if ( itemsPerPerson ) {
						rows <- mapply ( function ( p , d , id ) { d [ d[,id] == p , items , drop = FALSE ] } , persons , MoreArgs = list ( data , id ) , SIMPLIFY = FALSE )
						ret <- items.per.person <- mapply ( function ( row , missing ) {
								if ( is.na ( missing ) ) w <- sapply ( row , is.na ) else w <- sapply ( row , function ( z ) z == missing )
								colnames ( row ) [ !w ]
						} , rows , MoreArgs = list ( missing ) , SIMPLIFY = FALSE )
				}
			
				# Personen jedes Items
				if ( personsPerItem ) {
						ret <- persons.per.item <- mapply ( function ( d , idv , missing ) {
								if ( is.na ( missing ) ) w <- is.na(d) else w <- d == missing
								idv[ !w ]
						} , data [ , items , drop = FALSE ] , MoreArgs = list ( data [ , id ] , missing ) , SIMPLIFY = FALSE )
				}
				
				# Rückgabe
				if ( all ( itemsPerPerson , personsPerItem ) ) ret <- list ( "itemsPerPerson" = items.per.person , "personsPerItem" = persons.per.item ) 
		
		} else ret <- NULL
		
		return ( ret ) 
}
