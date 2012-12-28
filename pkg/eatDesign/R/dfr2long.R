
# internal; transform a quadratic data frame to long with row/col/value
dfr2long <- function ( qm , lower = TRUE , upper = FALSE , diag = FALSE , use.names = TRUE ) {
		
		# Returnvariable
		d <- NULL
		
		if ( ! all ( !lower , !upper , !diag ) ) {
		
				if ( is.data.frame ( qm ) ) {
				
						if ( nrow ( qm ) > 0 &&  ncol ( qm ) > 0 ) {

						pseudo <- matrix ( 1:(nrow(qm)*ncol(qm)) , nrow = nrow(qm) , ncol = ncol(qm) )
								if ( lower && upper ) {

										ind1 <- pseudo [ lower.tri ( pseudo , diag = diag ) ]
										ind2 <- pseudo [ upper.tri ( pseudo , diag = diag ) ]
										ind <- unique ( c ( ind1 , ind2 ) )
										
										# Diagonale nur einmal
										val1 <- qm[ lower.tri ( qm , diag = diag ) ]
										val2 <- qm[ upper.tri ( qm , diag = FALSE ) ]
										val <- c ( val1 , val2 )
										
								} else if ( lower ) {
										ind <- pseudo [ lower.tri ( pseudo , diag = diag ) ]
										val <- qm[ lower.tri ( qm , diag = diag ) ]
								} else if ( upper ) {
										ind <- pseudo [ upper.tri ( pseudo , diag = diag ) ]
										val <- qm[ upper.tri ( qm , diag = diag ) ]
								}
								
								d <- merge ( rownames ( qm ) , colnames ( qm ) )[ind,]
								eval ( parse ( text = paste ( "d$\"" , colnames(d) , "\"<-as.character(d$\"" , colnames(d) , "\")" , sep = "" ) ) )
								colnames ( d ) <- c ("row","col")

								d$value = val

								rownames ( d ) <- seq ( along = rownames ( d ) )

								if ( !use.names ) {
										d$row <- match ( d$row , rownames ( qm ) )
										d$col <- match ( d$col , colnames ( qm ) )
								}

								# sortieren wär schön
								# speziell im Fall upper=lower=diag=TRUE
								
						}
				}
		}
		
		return ( d )
}
