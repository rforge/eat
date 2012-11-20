# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.crossModels
# Description: Subroutine von automateModels
# Version: 	0.5.0
# Status: beta
# Release Date: 	2011-12-15
# Author:    Martin Hecht
# 2011-12-15 SW/MH
# ADDED: q3 in .automateModels.crossModels
# 0000-00-00 AA
# Change Log:
#		11.11.2011 MH: conquestParameters hinzugefügt
#						n.plausible rausgenommen (ist Bestandteil von conquestParameters)
#		14.10.2011 MH: Ausgaben auf Englisch
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# Optimierungsmöglichkeiten:
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.crossModels <- function ( model.specs ) {

		ret <- mapply ( function ( item.grouping , person.grouping , m.model , i.model , p.model , software ,
							analyse.name , data.name , q3 , dif , weight , anchor , regression , cross , missing.rule , conquestParameters ) {
		
					if ( !is.null ( cross ) ) {
			
							# kreuzen
							if ( cross == "all" ) {
									expanded <- expand.grid ( item.groups.new = colnames ( item.grouping )[-1] ,
									person.groups.new = colnames( person.grouping )[-1] , stringsAsFactors = TRUE )
									new.i.model <- "unidim"
									new.p.model <- "singlegroup"
							} else if ( cross == "item.groups" ) {
									expanded <- expand.grid ( item.groups.new = colnames ( item.grouping )[-1] ,
									person.groups.new = ",:keep:," , stringsAsFactors = TRUE )
									new.i.model <- "unidim"
									new.p.model <- p.model
							} else if ( cross == "person.groups" ) {
									expanded <- expand.grid ( item.groups.new = ",:keep:," ,
									person.groups.new = colnames( person.grouping )[-1] , stringsAsFactors = TRUE )							
									new.i.model <- i.model
									new.p.model <- "singlegroup"
							} else stop ( paste ( ".automateModels.crossModels: parameter 'cross' (" , cross , ") ist wrongly specified." ) )
							
							# neue Listen-Länge
							length.out <- nrow ( expanded )
							
							### neue Parameter erzeugen
							item.grouping.new <- mapply ( function ( item.groups.new , item.grouping ) {
									if ( item.groups.new == ",:keep:," ) item.grouping
									else item.grouping [ , c ( 1 , which ( colnames ( item.grouping ) %in% item.groups.new ) ) ]
							} , expanded$item.groups.new , MoreArgs = list ( item.grouping ) , SIMPLIFY = FALSE ) 
							
							person.grouping.new <- mapply ( function ( person.groups.new , person.grouping ) {
									if ( person.groups.new == ",:keep:," ) person.grouping
									else person.grouping [ , c ( 1 , which ( colnames ( person.grouping ) %in% person.groups.new ) ) ]
							} , expanded$person.groups.new , MoreArgs = list ( person.grouping ) , SIMPLIFY = FALSE )
							
							m.model.new <- 				rep ( list ( m.model ) , length.out = length.out )
							i.model.new <- 				rep ( list ( new.i.model ) , length.out = length.out )
							p.model.new <- 				rep ( list ( new.p.model ) , length.out = length.out )
							software.new <- 			rep ( list ( software ) , length.out = length.out )
							analyse.name.new <- 		rep ( list ( analyse.name ) , length.out = length.out )
							data.name.new <- 			rep ( list ( data.name ) , length.out = length.out )
							q3.new <- 					rep ( list ( q3 ) , length.out = length.out )
							dif.new <- 					rep ( list ( dif ) , length.out = length.out )
							weight.new <-				rep ( list ( weight ) , length.out = length.out )
							anchor.new <- 				rep ( list ( anchor ) , length.out = length.out )
							regression.new <- 			rep ( list ( regression ) , length.out = length.out )
							cross.new <- 				rep ( list ( cross ) , length.out = length.out )
							missing.rule.new <- 		rep ( list ( missing.rule ) , length.out = length.out )
							conquestParameters.new <- 			rep ( list ( conquestParameters ) , length.out = length.out )
							
							# return erzeugen
							ret <- list (   item.grouping = item.grouping.new ,
											person.grouping = person.grouping.new ,
											m.model = m.model.new ,
											i.model = i.model.new ,
											p.model = p.model.new ,
											software = software.new ,
											analyse.name = analyse.name.new ,
											data.name = data.name.new ,
											q3 = q3.new ,
											dif = dif.new ,
											weight = weight.new ,
											anchor = anchor.new ,
											regression = regression.new ,
											cross = cross.new ,
											missing.rule = missing.rule.new ,
											conquestParameters = conquestParameters.new )							
						
					} else {
							
							# nix machen
							# return erzeugen
							ret <- list (   item.grouping = list ( item.grouping ) ,
											person.grouping = list ( person.grouping ) ,
											m.model = list ( m.model ) ,
											i.model = list ( i.model ) ,
											p.model = list ( p.model ) ,
											software = list ( software ) ,
											analyse.name = list ( analyse.name ) ,
											data.name = list ( data.name ) ,
											q3 = list ( q3 ) ,
											dif = list ( dif ) ,
											weight = list ( weight ) , 
											anchor = list ( anchor ) ,
											regression = list ( regression ) ,
											cross = list ( cross ) ,
											missing.rule = list ( missing.rule ) ,
											conquestParameters = list ( conquestParameters ) )	
					
					}
		
					# returnen 
					return ( ret )
		
		} , model.specs$item.grouping , model.specs$person.grouping , model.specs$m.model , model.specs$i.model , model.specs$p.model , model.specs$software ,
							model.specs$analyse.name , model.specs$data.name , model.specs$q3 , model.specs$dif , model.specs$weight , model.specs$anchor , model.specs$regression , 
							model.specs$cross , model.specs$missing.rule , model.specs$conquestParameters ,SIMPLIFY = FALSE )

		# Wiederherstellung der Struktur
		ret <- unlist ( ret , recursive = FALSE )
		ret <- mapply ( function ( element.name , ret ) {
						c ( unname ( unlist ( ret [ which ( names ( ret ) == element.name ) ] , recursive = FALSE ) ) )
				} , unique ( names ( ret ) ) , MoreArgs = list ( ret ) , SIMPLIFY = FALSE )
	
		# cross droppen, da nicht mehr relevant
		ret [ which ( names ( ret ) == "cross" ) ] <- NULL
		
		# returnen
		return ( ret )
		
}
