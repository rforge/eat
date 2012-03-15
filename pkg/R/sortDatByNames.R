sortDatByNames <- function ( dat , col.order = NULL , row.order = NULL , warn = TRUE ) {
	
		# Checks
		stopifnot ( is.data.frame ( dat ) )
		
		ucol <- unique ( colnames ( dat ) )
		urow <- unique ( rownames ( dat ) )
		
		if ( ! ( is.null ( col.order ) & is.null ( row.order ) ) ) {
				
				if ( warn ) {
						if ( ! is.null ( col.order ) ) {
								if ( ! all ( drin <- ucol %in% col.order ) ) {
										warning ( paste ( "not all colnames are specified in 'col.order'. non-specified names (" ,
												  paste ( ucol[!drin] , collapse = ", " ) , ") are put at end of matrix." , sep = "" ) )
								
								}
								if ( ! all ( drin <- col.order %in% ucol ) ) {
										warning ( paste ( "'col.order' contains elements that are not in colnames. These elements (" , paste (
													col.order[!drin] , collapse = ", " ) , ") are ignored." , sep = "" ) )
								}
						}
						if ( ! is.null ( row.order ) ) {				
								if ( ! all ( drin <- urow %in% row.order ) ) {
										warning ( paste ( "not all rownames are specified in 'row.order'. non-specified names (" ,
												  paste ( urow[!drin] , collapse = ", " ) , ") are put at end of matrix." , sep = "" ) )
								
								}
								if ( ! all ( drin <- row.order %in% urow ) ) {
										warning ( paste ( "'row.order' contains elements that are not in rownames. These elements (" , paste (
													row.order[!drin] , collapse = ", " ) , ") are ignored." , sep = "" ) )
								}
						}
				}
		
				if ( ! is.null ( col.order ) ) {
						col.o <- na.omit ( match ( col.order , ucol ) )
						col.o <- c ( col.o , which ( ! seq ( along = colnames ( dat ) ) %in% col.o ) )
						dat <- dat [ , col.o ]
				} 

				if ( ! is.null ( row.order ) ) {
						row.o <- na.omit ( match ( row.order , urow ) )
						row.o <- c ( row.o , which ( ! seq ( along = rownames ( dat ) ) %in% row.o ) )
						dat <- dat [ row.o , ]
				}
		}
		
		return ( dat )
		
}


