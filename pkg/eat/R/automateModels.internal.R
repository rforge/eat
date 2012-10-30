# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.internal
# Description: Sammlung von Subroutinen
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.del.or.create.folder <- function ( folder , countdown = 5 , timeout = 20 , crit = c("stop","warn","silent") ) {

		crit <- userSpecifiedList ( crit , c("stop","warn","silent") , el.default = 1 )
		
		if ( ! is.numeric ( countdown ) ) countdown <- 0
		if ( identical ( countdown , integer(0) ) | identical ( countdown , numeric ( 0 ) ) ) countdown <- 0
	
		flush.console()
		if ( file.exists ( folder ) ) {
				# cat ( paste ( folder , "wird GELÖSCHT (Abbruch mit 'Esc') " ) ); flush.console()
				cat ( paste ( folder , "is being DELETED ('Esc' to cancel) " ) ); flush.console()
				Sys.sleep ( 1 )
				
				if ( countdown > 0 ) {
						temp <- mapply ( function ( nr ) {
								cat ( paste ( nr , " " , sep="" ) )
								flush.console ( )
								Sys.sleep ( 1 )
						} , countdown:1 )
				}
				
				cat ( "moeoeoep\n" ); flush.console() 
				# technischer Hinweis: es ist leer machen ( nicht komplett löschen ) 
				unlink ( paste ( folder , "/*" , sep ="" ), recursive = TRUE )
				
		}

		### solange checken bis wirklich leer
		flush.console()
		durchgang <- 0
		waitsek <- 0.5
		timeout.durchgang <- timeout * 1/waitsek
		while ( (! identical ( list.files(path = folder, pattern = NULL, all.files = TRUE,
           full.names = TRUE, recursive = TRUE ,
           ignore.case = FALSE, include.dirs = TRUE) , character(0) )) & durchgang < timeout.durchgang ) {
				if ( durchgang == 0 ) {
						cat ( "Waiting for file system / server " )
						flush.console()
				} else {
						cat ( "." )
						flush.console()
				}
				Sys.sleep ( waitsek )
				durchgang <- durchgang + 1
		}
		cat ( "\n" )
		if ( durchgang==timeout.durchgang ) {
				cat ( paste ( "Process has timed out.\n" , sep = "" ) ); flush.console()
				ret <- FALSE
		} else ret <- TRUE
		if ( durchgang>0 ) cat ( paste ( "And again" , durchgang*waitsek , "seconds verwaited\n" ) ); flush.console()

		if ( ! file.exists ( folder ) ) {
				tried <- try ( 
								dir.create ( folder , recursive = TRUE , showWarnings = FALSE )
							  , silent = TRUE )
				if ( inherits ( tried , "try-error" ) ) {
						cat ( paste ( "Folder" , folder , "could not be created." ) )
						ret <- FALSE
				}
		}

		if ( !ret ) {
				s <- paste ( "Deletion and/or creation of" , folder , "failed. Try again.\n You might wanna check if folder is used/locked by another program.\n" )
				if ( crit == "stop" ) {
						stop ( s , call. = FALSE)
				}
				if ( crit == "warn" ) {
						warning ( s )
				}
		}
		return ( ret )
		
}
# .del.or.create.folder <- function ( folder , countdown = 5 , timeout = 20 ) {
		
		# tried <- try ( .del.or.create.folder.doit ( folder , countdown , timeout ) )
		# if ( inherits ( tried , "try-error" ) ) {
				
				# stop ( )
				# return ( FALSE )
		# } else return ( TRUE )
		
# }

.check.any.items.or.rows.left <- function ( dataset , dont.test.cols.name=NULL , dont.test.rows.name=NULL ) {

		dont.test.cols.name <-  dont.test.cols.name [ which ( dont.test.cols.name %in% colnames ( dataset ) ) ]
		if ( identical ( dont.test.cols.name , character(0) ) ) dont.test.cols.name <- NULL
		if ( ! is.null ( dont.test.cols.name ) ) { 
				dataset.cols <- dataset [ , - which ( colnames ( dataset ) %in% dont.test.cols.name ) , drop=FALSE ]
		} else dataset.cols <- dataset
		if ( ! ( logi.cols <- ncol ( dataset.cols ) == 0 ) ) logi.cols <- FALSE

		dont.test.rows.name <-  dont.test.rows.name [ which (  dont.test.rows.name %in% rownames ( dataset ) ) ]
		if ( identical ( dont.test.rows.name , character(0) ) ) dont.test.rows.name <- NULL
		if ( ! is.null ( dont.test.rows.name ) ) {
				dataset.rows <- dataset [ - which ( rownames ( dataset ) %in% dont.test.rows.name ) , , drop=FALSE ]
		} else dataset.rows <- dataset
		if ( ! ( logi.rows <- nrow ( dataset.rows ) == 0 ) ) logi.rows <- FALSE
		
		if (  !( ret <- any ( c ( logi.cols , logi.rows ) ) )  ) ret <- FALSE
		
		return ( ret )
		
}
### Test
# d <- data.frame ( "v1"=c(1,2,3) , "v2"=c(1,2,3) )
# .check.any.items.or.rows.left ( d )
# .check.any.items.or.rows.left ( d[,-c(1,2), drop=FALSE] )
# .check.any.items.or.rows.left ( d[-c(1,2,3),, drop=FALSE] )
# .check.any.items.or.rows.left ( d[-c(1,2,3),-c(1,2), drop=FALSE] )
# .check.any.items.or.rows.left ( d , dont.test.cols.name=c("v1","v2") )
# .check.any.items.or.rows.left ( d , dont.test.rows.name=c("1","2","3") )
# .check.any.items.or.rows.left ( d , dont.test.cols.name=c("v1","v2") , dont.test.rows.name=c("1","2","3") )

.which.list.element.is.null <- function ( liste ) {

	unname ( which ( mapply ( function ( liste ) {

		if ( is.null ( liste ) ) TRUE else FALSE

	} , liste , SIMPLIFY = TRUE ) ) )

}



