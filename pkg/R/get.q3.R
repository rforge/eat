# 2011-12-16 MH
# FIXED: handling of non-existent q3 information in function get.q3
# 0000-00-00 AA


get.q3 <- function ( results ) {

		r <- results

		.fun1 <- function ( analyse , analyse.name ) {
				
				.fun2 <- function ( dimension , dimension.name , analyse.name ) {
				
						.fun3 <- function ( gruppe , gruppe.name , analyse.name , dimension.name ) {
			
								item.names <- names(gruppe$item)
								analyse.name.vec <- rep ( analyse.name , length.out = length ( item.names ) )
								dimension.name.vec <- rep ( dimension.name , length.out = length ( item.names ) )
								gruppe.name.vec <- rep ( gruppe.name , length.out = length ( item.names ) )

								.fun4 <- function ( item , item.name , dimension.name ) {
									
										q3.names <- names ( item$q3 )
										data.frame ( "dim" = rep ( dimension.name , length ( q3.names ) ) , 
													 "row" = rep ( item.name , length ( q3.names ) ) , 
													 "col" = q3.names , 
													 "val" = unname ( unlist ( item$q3 ) ) ,
													 stringsAsFactors = FALSE )
									
								}
								
								q3.long <- mapply ( .fun4 , gruppe$item , names ( gruppe$item ) , MoreArgs = list ( dimension.name ) , SIMPLIFY = FALSE )

								q3.long <- do.call ( "rbind" , q3.long )

								# wenn data.frame nicht leer
								if ( ! nrow ( q3.long ) == 0 ) {
										# für Duplikate auf verschiedenen Skalen, Skalenname ranmergen
										.fun6 <- function ( row , d ) {
												dim <- d$dim[d$row %in% row]
												if ( ! all ( dim %in% dim[1] ) ) TRUE else FALSE 
										}
										dupl <- unname ( mapply ( .fun6 , q3.long$row , MoreArgs = list ( q3.long ) ) )
										q3.long$row[ dupl ] <- paste ( q3.long$row[ dupl ] , q3.long$dim [ dupl ] , sep = "." )
								}
								
								return ( q3.long )
								
						}
						ret <- mapply ( .fun3 , dimension , names ( dimension ) , MoreArgs = list ( analyse.name , dimension.name ) , SIMPLIFY = FALSE )
						ret <- do.call ( "rbind" , ret )
						
				}
				ret <- mapply ( .fun2 , analyse , names( analyse ) , MoreArgs = list ( analyse.name ) , SIMPLIFY = FALSE )
				ret <- do.call ( "rbind" , ret )
				
		}
		q3.long <- mapply ( .fun1 , r , names ( r ) , SIMPLIFY = FALSE )
	
		# nach wide
		.fun7 <- function ( l ) {
				if ( ! ( nrow ( l ) == 0 ) ) {
						wide <- long2matrix ( l[ , ! colnames ( l ) %in% c ( "dim" ) ] , triangle = "lower")
						item.scale <- l [ ! duplicated ( l$row ) , "dim" ]
						names ( item.scale ) <- l [ ! duplicated ( l$row ) , "row" ]
						wide <- cbind ( "Dimension" = item.scale[ rownames ( wide ) ] , wide )
				} else wide <- NULL
		return ( wide )
		}
		ret <- mapply ( .fun7 , q3.long , SIMPLIFY = FALSE )
		
		return ( ret )
}		
