
.onAttach <- function(lib, pkg){
	
		eV <- eatVersion()
		
		# Anzahl an "=" bestimmen
		l <- sapply ( cbind ( rownames (eV) , eV , stringsAsFactors = FALSE ) , nchar , simplify = FALSE )
		names ( l )[1] <- ""
		l <- sum ( mapply ( function ( l , n ) max ( c ( l , nchar ( n ) ) ) , l , names ( l ) ) ) + ncol ( eV )
		
		streifen <-  paste ( rep ( "=" , l ) , collapse = "" )
		welcomeMsg <- paste ( "\n" , streifen , "\n" , eatDesign:::dfr2text( eV,blankRowNames = TRUE ) , streifen , "\n" , sep = "" )
		
		packageStartupMessage ( welcomeMsg )

		invisible ( welcomeMsg )
}
