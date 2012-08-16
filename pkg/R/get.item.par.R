
get.item.par <- function ( results ) {

		fun1 <- function ( analyse , analyse.name ) {
				
				fun2 <- function ( dimension , dimension.name , analyse.name ) {
			
				
						fun3 <- function ( gruppe , gruppe.name , analyse.name , dimension.name ) {
			
								item.names <- names(gruppe$item)
								analyse.name.vec <- rep ( analyse.name , length.out = length ( item.names ) )
								dimension.name.vec <- rep ( dimension.name , length.out = length ( item.names ) )
								gruppe.name.vec <- rep ( gruppe.name , length.out = length ( item.names ) )

								fun4 <- function ( item ) {

										fun5 <- function ( element ) {
												if ( !is.null ( element ) ) element else NA
										}
										item <- mapply ( fun5 , item , SIMPLIFY = FALSE )

										# todo NA zu item$b.adj
										c ( item$n.valid, item$p, item$a, item$b, item$b.adj, item$c, item$d,
											item$b.se,  item$infit, item$infit.ci.lb, item$infit.ci.ub, item$infit.t, item$outfit,
											item$outfit.ci.lb, item$outfit.ci.ub, item$outfit.t, item$pbc,
											item$b.eval, item$infit.eval, item$pbc.eval, item$eval.num, item$eval )

								}
					
								kennwerte <- t ( unname ( mapply ( fun4 , gruppe$item ) ) )
								kennwerte <- cbind ( matrix ( c ( analyse.name.vec , dimension.name.vec , gruppe.name.vec , item.names ) , ncol = 4 ) , kennwerte )
								kennwerte <- data.frame ( kennwerte , stringsAsFactors = FALSE )
								colnames ( kennwerte ) <- c ( "analysis" , "dimension" , "group" , "item" , "n.valid", "p", "a", "b", "b.adj", "c", "d",
															  "b.se", "infit", "infit.ci.lb", "infit.ci.ub", "infit.t", "outfit",
															  "outfit.ci.lb", "outfit.ci.ub", "outfit.t", "pbc",
															  "b.eval", "infit.eval", "pbc.eval", "eval.num", "eval" )
								return ( kennwerte )
						}
						mapply ( fun3 , dimension , names ( dimension ) , MoreArgs = list ( analyse.name , dimension.name ) , SIMPLIFY = FALSE )
				
				}
				mapply ( fun2 , analyse , names( analyse ) , MoreArgs = list ( analyse.name ) , SIMPLIFY = FALSE )

		}
		kennwerte <- mapply ( fun1 , results , names( results ) , SIMPLIFY = FALSE )
		kennwerte <- unlist ( unlist ( kennwerte , recursive = FALSE ) , recursive = FALSE )

		# Einzeldataframes zusammenmergen, die gelistet sind
		itemkennwerte <- do.call ( "rbind" , kennwerte )
		rownames ( itemkennwerte ) <- 1:nrow(itemkennwerte)
		itemkennwerte$n.valid <- as.numeric(itemkennwerte$n.valid)
		itemkennwerte$p <- as.numeric(itemkennwerte$p)
		itemkennwerte$a <- as.numeric(itemkennwerte$a)
		itemkennwerte$b <- as.numeric(itemkennwerte$b)
		itemkennwerte$b.adj <- as.numeric ( itemkennwerte$b.adj )
		itemkennwerte$c <- as.numeric(itemkennwerte$c)
		itemkennwerte$d <- as.numeric(itemkennwerte$d)
		itemkennwerte$b.se <- as.numeric(itemkennwerte$b.se)
		itemkennwerte$infit <- as.numeric(itemkennwerte$infit)
		itemkennwerte$infit.ci.lb <- as.numeric(itemkennwerte$infit.ci.lb)
		itemkennwerte$infit.ci.ub <- as.numeric(itemkennwerte$infit.ci.ub)
		itemkennwerte$infit.t <- as.numeric(itemkennwerte$infit.t)
		itemkennwerte$outfit <- as.numeric(itemkennwerte$outfit)
		itemkennwerte$outfit.ci.lb <- as.numeric(itemkennwerte$outfit.ci.lb)
		itemkennwerte$outfit.ci.ub <- as.numeric(itemkennwerte$outfit.ci.ub)
		itemkennwerte$outfit.t <- as.numeric(itemkennwerte$outfit.t)
		itemkennwerte$pbc <- as.numeric(itemkennwerte$pbc)
		itemkennwerte$b.eval <- as.character(itemkennwerte$b.eval)
		itemkennwerte$infit.eval <- as.character(itemkennwerte$infit.eval)
		itemkennwerte$pbc.eval <- as.character(itemkennwerte$pbc.eval)
		itemkennwerte$eval.num <- as.numeric(itemkennwerte$eval.num)
		itemkennwerte$eval <- as.character(itemkennwerte$eval)
		# todo, weitere numeric Setzungen
		
		return ( itemkennwerte )
}
