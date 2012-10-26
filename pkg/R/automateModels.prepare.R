# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.prepare
# Description: Subroutine von automateModels
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-11-14
# Author:    Martin Hecht
# Change Log:
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.prepare <- function ( list1, list2, list2.available, list3, list3.checkType, list4 ) {

		### Plausichecks/Aufbereiten list1 ( item.grouping & person.grouping )

		# Umwandeln in Liste 
		if ( ! inherits ( list1$item.grouping , "list" ) ) item.grouping <- list ( item.grouping )
		if ( ! inherits ( list1$person.grouping , "list" ) ) person.grouping <- list ( person.grouping )
		
		# wenn nicht Data.frame oder Liste stoppen
		temp <- sapply ( list1 , function ( element ) {
					stopifnot ( inherits ( element , "data.frame" ) | inherits ( element , "list" )  )
				} , simplify=FALSE )

		# umwandeln in Liste wenn data.frame (ergibt Liste mit einem data.frame-Element)
		list1 <- sapply ( list1 , function ( element ) {
					if ( inherits ( element , "data.frame" ) ) list ( element ) else element
				} , simplify=FALSE )		
		
		# wenn nicht Data.frames als Listenelemente stoppen
		temp <- sapply ( list1 , function ( element ) {
						temp2 <- sapply( element , function (element2) {
								stopifnot ( inherits ( element2 , "data.frame" ) )
						} , simplify=FALSE )
				} , simplify=FALSE )

		# recyclen wenn ungleiche Länge
		out.length <- max ( length ( list1$item.grouping ) , length ( list1$person.grouping ) )
		list1$item.grouping <- rep ( list1$item.grouping , length.out = out.length )
		list1$person.grouping <- rep ( list1$person.grouping , length.out = out.length )		
				
		# wenn item.grouping und person.grouping nicht gleiche Länge stoppen
		stopifnot ( length ( list1$item.grouping ) == length ( list1$person.grouping ) )

		
		### Plausichecks/Aufbereiten list2 ( m.model , i.model , p.model , software )
	
		# umwandeln in Liste wenn character-Skalar oder NULL
		list2 <- sapply ( list2 , function ( element ) {
					if ( !inherits ( element , "list" ) ) list ( element ) else element
				} , simplify=FALSE )
		
		# anpassen (recyclen oder abschneiden), Referenz: person.grouping/item.grouping
		list2 <- sapply ( list2 , function ( element ) {
					#if (length(list1$person.grouping)>length(element)) { element <- rep(element, (length(list1$person.grouping) %/% length(element)) + 1) }		
					#if (length(list1$person.grouping)<length(element)) { element <- element[seq(along=list1$person.grouping)] }
					element <- rep ( element , length.out = length(list1$person.grouping) )
					stopifnot ( length(element) == length(list1$person.grouping) )					
					return ( element )
				} , simplify=FALSE )	
		
		# wenn nicht zugelassene Einträge als Listenelemente dann stoppen
		temp <- mapply ( function ( element , available) {
					temp2 <- sapply ( element , function (element2) {
							stopifnot ( element2 %in% available )
					} )							
				} , list2 , list2.available , SIMPLIFY=FALSE )	

				
		### Plausichecks/Aufbereiten list3 ( analyse.name , dif , weight , anchor , regression )
		# umwandeln in Liste wenn "skalar"
		list3 <- sapply ( list3 , function ( element ) {
					#if ( inherits ( element , "NULL" ) ) list ( element ) else element
					if ( !inherits ( element , "list" ) ) list ( element ) else element
				} , simplify=FALSE )
		
		# anpassen (recyclen oder abschneiden), Referenz: person.grouping/item.grouping
		list3 <- sapply ( list3 , function ( element ) {
					element <- rep ( element , length.out = length(list1$person.grouping) )
					stopifnot ( length(element) == length(list1$person.grouping) )					
					return ( element )
				} , simplify=FALSE )	

		# wenn nicht zugelassene Einträge als Listenelemente dann stoppen
		temp <- mapply ( function ( element , checkType) {
					temp2 <- sapply ( element , function (element2) {
							stopifnot ( inherits ( element2 , checkType ) | inherits ( element2 , "NULL" ) )
					} )							
				} , list3 , list3.checkType , SIMPLIFY=FALSE )	
		### Plausichecks/Aufbereiten list4 ( alle Elemente die "skalar" schon Listen sind )
		# umwandeln in Liste wenn "skalare" Liste
		list4 <- sapply ( list4 , function ( element ) {
					if ( !inherits ( unlist( element , recursive=FALSE ) , "list" ) ) list ( element ) else element
				} , simplify=FALSE )
		
		# anpassen (recyclen oder abschneiden), Referenz: person.grouping/item.grouping
		list4 <- sapply ( list4 , function ( element ) {
					element <- rep ( element , length.out = length(list1$person.grouping) )
					stopifnot ( length(element) == length(list1$person.grouping) )					
					return ( element )
				} , simplify=FALSE )	

		# wenn nicht zugelassene Einträge als Listenelemente dann stoppen
		temp <- mapply ( function ( element , checkType) {
					temp2 <- sapply ( element , function (element2) {
							stopifnot ( inherits ( element2 , checkType ) | inherits ( element2 , "NULL" ) )
					} )							
				} , list4 , "list" , SIMPLIFY=FALSE )	

		### conquestParameters
		# richtige default "NULL"-Struktur erzeugen wenn nicht gesetzt
		.fun <- function ( cP ) {
				if ( is.null ( cP ) ) ret <- list ( cP ) else ret <- cP
				return ( ret )
		}
		list4$conquestParameters <- mapply ( .fun , list4$conquestParameters , SIMPLIFY = FALSE)
		# Check ob keine falschen Bezeichner
		allowedNames <- c(  "na",
							"compute.fit",
							"model.statement",
							"pathConquest",
							"method",
							"std.err",
							"distribution",
							"n.plausible",
							"set.constraints",
							"nodes",
							"p.nodes",
							"f.nodes",
							"n.iterations",
							"converge",
							"deviancechange",
							"allowAllScoresEverywhere",
							"equivalence.table",
							"use.letters",
							"checkLink",
							"export" )
		
		# check <- unique ( names ( unlist ( list4$conquestParameters ) ) )
		check <- names ( sapply ( list4$conquestParameters , names ) )
		
		if ( ! all ( w <- ( check %in% allowedNames ) ) ) {
				errmes <- paste ( "not available names are used in 'conquestParameters':" , 
									paste ( check[!w] , collapse = ", " ), "\n" )
				stop(errmes)
		}		
		
		return ( c ( list1 , list2 , list3 , list4 ) )
}
