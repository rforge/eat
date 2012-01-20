
# 2011-12-23 MH
# NEW: function sort.dfr.by.names
# 0000-00-00 AA

sortDfrByNames <- function ( dfr , col.order = NULL , row.order = NULL , warn = TRUE ) {
	
		# Checks
		stopifnot ( is.data.frame ( dfr ) )
		
		ucol <- unique ( colnames ( dfr ) )
		urow <- unique ( rownames ( dfr ) )
		
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
						col.o <- c ( col.o , which ( ! seq ( along = colnames ( dfr ) ) %in% col.o ) )
						dfr <- dfr [ , col.o ]
				} 

				if ( ! is.null ( row.order ) ) {
						row.o <- na.omit ( match ( row.order , urow ) )
						row.o <- c ( row.o , which ( ! seq ( along = rownames ( dfr ) ) %in% row.o ) )
						dfr <- dfr [ row.o , ]
				}
		}
		
		return ( dfr )
		
}

# dfr <- data.frame ( matrix ( rnorm ( 100 ) , ncol = 10 ) )
# colnames ( dfr ) <- paste ( "X" , 10:1 , sep = "" )
# rownames ( dfr ) <- paste ( "X" , 11:2 , sep = "" )
# dfr

## sort data.frame by 'col.order' and 'row.order'
# sort.dfr.by.names ( dfr , paste ( "X" , 1:10 , sep = "" ) , paste ( "X" , 2:11 , sep = "" ) )

