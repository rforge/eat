
get.person.par <- function ( results ) {

		fun1 <- function ( analyse , analyse.name ) {
				
				fun2 <- function ( dimension , dimension.name , analyse.name ) {
			
				
						fun3 <- function ( gruppe , gruppe.name , analyse.name , dimension.name ) {
			
								person.names <- names(gruppe$person)
								analyse.name.vec <- rep ( analyse.name , length.out = length ( person.names ) )
								dimension.name.vec <- rep ( dimension.name , length.out = length ( person.names ) )
								gruppe.name.vec <- rep ( gruppe.name , length.out = length ( person.names ) )

								fun4 <- function ( person ) {

										fun5 <- function ( element ) {
												if ( !is.null ( element ) ) element else NA
										}
								
										person <- mapply ( fun5 , person , SIMPLIFY = FALSE )
										person <- c ( person , person$pv )
										person <- person [ ! names (person) %in% "pv" ]

										do.call ( "c" , person )
										
								}
								kennwerte <- t ( mapply ( fun4 , gruppe$person ) )
								kennwerte <- cbind ( matrix ( c ( analyse.name.vec , dimension.name.vec , gruppe.name.vec , person.names ) , ncol = 4 ) , kennwerte )
								kennwerte <- data.frame ( kennwerte , stringsAsFactors = FALSE )
								colnames ( kennwerte )[c(1:4)] <- c ( "analysis" , "dimension" , "group" , "person" )
								return ( kennwerte )
						}
						mapply ( fun3 , dimension , names ( dimension ) , MoreArgs = list ( analyse.name , dimension.name ) , SIMPLIFY = FALSE )
				
				}
				mapply ( fun2 , analyse , names( analyse ) , MoreArgs = list ( analyse.name ) , SIMPLIFY = FALSE )

		}
		kennwerte <- mapply ( fun1 , results , names( results ) , SIMPLIFY = FALSE )
		kennwerte <- unlist ( unlist ( kennwerte , recursive = FALSE ) , recursive = FALSE )

		# Einzeldataframes zusammenmergen, die gelistet sind
		personenkennwerte <- do.call ( "rbind.fill" , kennwerte )
		rownames ( personenkennwerte ) <- 1:nrow( personenkennwerte )
		v <- colnames ( personenkennwerte ) [ ! colnames ( personenkennwerte ) %in% c ( "analysis" , "dimension" , "group" , "person" ) ]
		personenkennwerte <- set.col.type ( personenkennwerte , list ( "numeric" = v ) )

		return ( personenkennwerte )
}
