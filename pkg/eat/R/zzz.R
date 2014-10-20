
.onAttach <- function(lib, pkg){
		
		versionString <- "eat version 2.1.10 (2014-10-20) -- \"Ginormous Phoenix\""
		
		eV <- eatVersion()
		
		# Anzahl an "=" bestimmen
		l <- sapply ( cbind ( rownames (eV) , eV , stringsAsFactors = FALSE ) , nchar , simplify = FALSE )
		names ( l )[1] <- ""
		l <- sum ( mapply ( function ( l , n ) max ( c ( l , nchar ( n ) ) ) , l , names ( l ) ) ) + ncol ( eV )
		
		l2 <- max ( c ( l , nchar ( versionString ) ) )
		
		streifen <-  paste ( rep ( "=" , l2 ) , collapse = "" )
		welcomeMsg <- paste ( "\n" , streifen , "\n" , versionString , "\n" , streifen , "\n" , eatDesign:::dfr2text( eV,blankRowNames = TRUE ) , streifen , "\n" , sep = "" )
		
		packageStartupMessage ( welcomeMsg )

		invisible ( welcomeMsg )
}
