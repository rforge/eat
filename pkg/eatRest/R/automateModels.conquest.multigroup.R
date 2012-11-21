# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.conquest.multigroup
# Description: Subroutine von automateModels
# Version: 	0.4.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.conquest.multigroup <- function ( model.specs , dataset ) {

		# Funktionsname
		f.  <- ".automateModels.conquest.multigroup"
		f.n <- paste ( f. , ":" , sep = "" )

		.fun <- function ( software , person.grouping , p.model ) {
				
				if ( software == "conquest" & ncol ( person.grouping ) > 2 ) {
				
						# Gruppennamen
						group.names.orig <- colnames ( person.grouping )[-1]
						
						# neuer Gruppenname
						group.name.new <- paste ( group.names.orig , collapse = "-" )
			
						# person.grouping zusammenfassen
						# TODO
						# eatTools:::sunk ( paste ( f.n , "VORSICHT: noch nich implementiert ist, Gruppen der gleichen Variable zu mergen\n" ) )
						eatTools:::sunk ( paste ( f.n , "DANGER: Merging several Groups of the same Variable is not yet supported\n" ) )
						
						person.vektor <- apply ( person.grouping[-1] , 1 , function ( zeile ) {
									if ( all ( zeile == 1 ) ) return ( 1 ) else return ( 0 )
						} )
						
						person.grouping.new <- cbind ( person.grouping[1] , person.vektor )
						colnames ( person.grouping.new ) <- c ( colnames ( person.grouping[1] ) , group.name.new )
						# alle mit 0 raus
						rows.del <- which ( ! person.grouping.new[,2] == 1 )
						if ( ! identical ( rows.del , "integer(0)" ) ) person.grouping.new <- person.grouping.new [ - rows.del , ]
						
						### group-Variable(n) aus Gruppennamen extrahieren
						
						#TEST
						#group.names.orig <- c ( "gr.9" , "gr.10" , "sf.Hs" , "sf.Rs" , "s.s.s." , "." , "d." , ".4" , "a..b" )
						
						# Warnung wenn nicht Konvention entsprechender group.name
						group.name.check <- regexpr ( "^[[:alnum:]]+\\.[[:alnum:]]+$", group.names.orig ) == 1
						
						if ( ! all ( group.name.check ) ) {
								string <- paste ( group.names.orig [ !group.name.check ] , collapse = ", " )
								# eatTools:::sunk ( paste ( f.n , "Gruppen-Namen" , string , "entsprechen nicht der Konvention <var>.<cat>\n" ,
													# "und werden deshalb für die Erzeugung des group statement für ConQuest nicht verwendet.\n" ) )
								eatTools:::sunk ( paste ( f.n , "group names" , string , "are not consistent with convention <var>.<cat>\n" ,
													"and are not used to generate group statement for ConQuest.\n" ) )


						}
													
						# group.names Parameter für Conquest
						group.names <- unique ( group.names.orig [ group.name.check ] )
						
						# splitten
						group.name.new.split <- strsplit ( group.names , "." , fixed = TRUE )
						
						# group Parameter für ConQuest
						group <- unique ( sapply ( group.name.new.split , "[" , 1 ) )
						
						# wenn die Gruppenvariable allerdings nicht im Datensatz ist, dann Pustekuchen
						# bzw. TODO die Variable dann hier erzeugen
						if ( ! any ( colnames ( dataset ) %in% group ) ) {
								eatTools:::sunk ( paste ( f.n , "\n" ) )
								# eatTools:::sunk ( paste ( "Variable(n)" , paste ( group , collapse = ", " ) , "ist/sind nicht im Datensatz.\n" ) )
								# eatTools:::sunk ( paste ( "Bitte Datensatz und/oder person.grouping anpassen.\n" ) )								
								eatTools:::sunk ( paste ( "Variable(s)" , paste ( group , collapse = ", " ) , "is/are not in dataset.\n" ) )
								eatTools:::sunk ( paste ( "Please check dataset and/or person.grouping\n" ) )
								stop()
						}

						ret <- list ( person.grouping = list ( person.grouping.new ) , group = list ( group ) , group.names = list ( group.names ) , p.model = list ( "singlegroup" ) ) 
				
				} else {
						
						ret <- list ( person.grouping = list ( person.grouping ) , group = list ( NULL ) , group.names = list ( NULL ) , p.model = list ( p.model ) ) 
				
				}
			
				return ( ret )
			
		}

# library(debug)	
# mtrace(.fun)		
		ret <- mapply ( .fun , model.specs$software , model.specs$person.grouping , model.specs$p.model , SIMPLIFY = FALSE )
		
		# Wiederherstellung der Struktur
		ret <- unlist ( ret , recursive = FALSE )
		ret <- mapply ( function ( element.name , ret ) {
						c ( unname ( unlist ( ret [ which ( names ( ret ) == element.name ) ] , recursive = FALSE ) ) )
				} , unique ( names ( ret ) ) , MoreArgs = list ( ret ) , SIMPLIFY = FALSE )
		
		# neue Parameter in model.specs setzen
		model.specs$person.grouping <- ret$person.grouping
		model.specs$group <- ret$group
		model.specs$group.names <- ret$group.names
		model.specs$p.model <- ret$p.model

		# Rückgabe
		return ( model.specs )
		
}


