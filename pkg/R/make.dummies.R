
isPseudoNumeric <- function ( x ) {
		if ( is.factor ( x ) ) x <- as.character ( x )
		unname ( asNumericIfPossible ( data.frame ( x , stringsAsFactors=FALSE ), set.numeric=FALSE, transform.factors=FALSE, maintain.factor.scores = FALSE, verbose=FALSE ) )
}
						
make.dummies <- function ( dat , cols , colname.as.prefix = TRUE , delimiter = "." , capitalize = FALSE , nchar = NULL , add = TRUE , sort.into.dat = TRUE ) {
		
		# cols nicht in colnames
		w <- !cols %in% colnames(dat)
		if ( any ( w ) ) {
				warning ( paste ( "Variable(s) " , paste ( cols[w] , collapse = ", " ) , " not in data.frame." , sep = "" ) )
		}
		if ( all ( w ) ) {
				if ( add ) ret <- dat else ret <- NULL
		} else {
		
				# Schleife um Vars
				fun <- function ( col , dat , colname.as.prefix , delimiter , nchar ) {

						# wenn Variable numerisch oder pseudo-numerisch (d.h. nach numerisch wandelbar) ist
						# wird colname.as.prefix = TRUE gesetzt, da Variablen-Namen nicht mit einer Zahl anfangen können,
						# wird von dummy.code ein "X" gesetzt
						# dieses wird entfernt, weil das dann schöner ist

						pseudonumeric <- isPseudoNumeric ( dat[,col] )
						
						if ( is.numeric ( dat[,col] ) | pseudonumeric ) {
								isNum <- TRUE 
								if ( !colname.as.prefix ) {
										warning ( paste ( "Variable " , col , " is numeric or pseudo-numeric. That's why colname.as.prefix is set to TRUE for this variable because it's nicer this way." , sep = "" ) )
										colname.as.prefix = TRUE
								}
						} else isNum <- FALSE
						
						# dummies bilden
						x <- psych::dummy.code ( dat[,col] )
						newcolnames <- colnames ( x )
						x <- data.frame ( x )
						
						
						### Variablen-Namen
			
						# falls numerisch war, noch die hässlichen "X" aus Var-Name raus
						# if ( isNum ) newcolnames <- sub ( "(.*)(\\w)(\\d+$)" , "\\1\\3" , colnames(x) ) else newcolnames <- colnames(x)
						# if ( isNum ) newcolnames <- sub ( "(.*)(\\w)(\\d+$)" , "\\1\\3" , colnames(x) ) else newcolnames <- colnames(x)
						
						# mit Großbuchstaben anfangen
						if ( capitalize ) newcolnames <- capitalize ( newcolnames )
						
						# Stelligkeit
						if ( !is.null ( nchar ) ) {
								if ( nchar >= 1 ) {
										newcolnames <- substr ( newcolnames , 1 , nchar )
										if ( any ( duplicated ( newcolnames ) ) ) newcolnames <- make.unique ( newcolnames )
								}
						}
						
						# Variablen-Namen als Prefix
						if ( colname.as.prefix ) {
								if ( !is.character ( delimiter ) ) delimiter <- ""
								newcolnames <- paste ( col , newcolnames , sep = delimiter )
						}
						
						# setzen
						colnames ( x ) <- newcolnames
						
						# returnen
						return ( x )
				}
				ret1 <- mapply ( fun , cols , MoreArgs = list ( dat , colname.as.prefix , delimiter , nchar ) , SIMPLIFY = FALSE )
				
				# unnamen damit do.call / "cbind" nicht noch den Listennamen an die Colnames rantut
				# entspricht cbind deparse.level = 0 ( was aber irgendwie nicht so einfach an do.call übergebbar ist
				ret1.unnamed <- unname ( ret1 )

				# cbinden
				if ( add ) {
				
						ret <- do.call ( "cbind" , c ( list ( dat ) , ret1.unnamed ) )
			
						# reinsortieren
						if ( sort.into.dat ) {
								
								fun2 <- function ( r , na ) {
										paste ( "ret<-reinsort.col(ret,c(" , 
												paste(paste("'",colnames(r),"'",sep=""),collapse=",") ,
												"),'",na,"')",sep="")
								}
								do <- mapply ( fun2 , ret1 , names(ret1) )
								do <- paste ( do , collapse = ";" )
								eval ( parse ( text = do ) )
						}
				
				} else {
						ret <- do.call ( "cbind" , ret1.unnamed )				
				}
		
		}

		return ( ret )

}

