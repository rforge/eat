

### ben�tigte Pakete
# library(igraph)

### Class definition of "design" ###
setClass(
	"design" ,
	representation = representation ( 
			definition = "data.frame" ,
			elements = "character" ,
			units = "list" ,
			nunits = "integer" ,
			structure = "data.frame" ,
			structureList = "list" ,
			descriptives = "data.frame" ,
			linkList = "list" ,
			adjacency = "list" ,
			link = "data.frame" ,
			varCovMatrix = "matrix" , 
			designDescriptives = "list"
			) ,
	prototype = prototype ( 
			definition = data.frame() ,
			elements = character() ,
			units = list() ,
			nunits = integer() , 
			structure = data.frame() ,
			structureList = list() ,
			descriptives = data.frame() ,
			linkList = list() ,
			adjacency = list() ,
			link = data.frame() ,
			varCovMatrix = matrix()[FALSE,FALSE] ,
			designDescriptives = list()
			)
)

# defineDesign
# wrapper f�r define.design
# statt den 4 "descriptives" nur 1 descriptives (for sake of simplicity)
defineDesign <- function ( dsgn , def = data.frame() , append = FALSE , descriptives = TRUE , verbose = FALSE ) {
					
					# wenn verbose, sollen Warnmeldungen sofort kommen (dazwischen gemischt werden)
					oldwarn <- options ( "warn" )
					if ( verbose ) {
							options ( "warn" = 1 )
					}
					
					# Check descriptives
					if ( !is.logical ( descriptives ) ) {
							msg2 <- paste ( "Argument 'descriptives' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										    "It's set to FALSE.\n", sep = "" )
							warning ( msg2 , call. = FALSE )
							descriptives <- FALSE
					}					
					
					### Aufruf von defineDesign
					ret <- define.design ( dsgn=dsgn, def=def, append=append, genStructure=descriptives, genDescriptives=descriptives, genLink=descriptives, genVarCovMatrix=descriptives, verbose=verbose )					
					
					# auf altes Warn-Level z�r�cksetzen
					options ( oldwarn )
							
					# return
					return ( ret )
}

define.design <- function ( dsgn , def = data.frame() , append = FALSE , genStructure = TRUE , genDescriptives = TRUE , genLink = TRUE , genVarCovMatrix = TRUE , verbose = FALSE ) {

					# Check verbose
					if ( !is.logical ( verbose ) ) {
							msg2e <- paste ( "Argument 'verbose' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										     "It's set to FALSE.\n", sep = "" )
							warning ( msg2e , call. = FALSE )
							verbose <- FALSE
					}				
					# wenn verbose, sollen Warnmeldungen sofort kommen (dazwischen gemischt werden)
					oldwarn <- options ( "warn" )
					if ( verbose ) {
							options ( "warn" = 1 )
					}

					# Check dsgn
					if ( missing ( dsgn ) || !inherits ( dsgn , "design" ) ) {
							msg1 <- paste ( "Argument 'dsgn' in function 'defineDesign' is not a valid object of class 'design'.\n",
											"A new object of class 'design' will be automatically created.\nTo avoid this, set argument 'dsgn' as a valid object.\n",
											"You can create such an object with dsgn <- new ( \"design\" )\n" , sep = "" )
							# warning ( msg1 , call. = FALSE )
							if ( verbose ) {
									cat ( msg1 )
							}
							rm ( dsgn )
							dsgn <- new ( "design" )
					}
					
					# Check def
					def.is.dfr <- is.data.frame ( def )
					if ( !def.is.dfr ) {
							msg22 <- paste ( "Argument 'def' in function 'defineDesign' is not a data.frame.\n" , sep = "" )
							stop ( msg22 , call. = FALSE )
					}

					# empty.def <- ! ( nrow ( def ) > 0 && ncol ( def ) > 0 ) 
					empty.def <- identical ( def , data.frame() )

					# komplette NA-Spalten raus
					if ( !empty.def ) {
							allna <- sapply ( def , function ( sp ) all ( is.na ( sp ) ) )
							if ( all ( allna ) ) {
									def <- data.frame()
									empty.def <- TRUE
									msg21a <- paste ( "All columns in data frame 'def' are completely NA and are removed.\n" ,
												     "'def' is now an empty data frame.\n" , sep = "" )
									warning ( msg21a , call. = FALSE )
							} else if ( any ( allna ) ) {
									def <- def[,!allna,drop=FALSE]
									msg21b <- paste ( "Columns " , paste ( colnames ( def )[allna] , collapse = ", " ) ,
													  "in data frame 'def' are completely NA and are removed.\n" , sep = "" )
									if ( verbose ) {
											cat ( msg21b )
									}
							}
					}
					
					# Check append
					if ( !is.logical ( append ) ) {
							msg2 <- paste ( "Argument 'append' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										    "It's set to FALSE.\n", sep = "" )
							warning ( msg2 , call. = FALSE )
							append <- FALSE
					}
					# Check genStructure
					if ( !is.logical ( genStructure ) ) {
							msg2b <- paste ( "Argument 'genStructure' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										     "It's set to FALSE.\n", sep = "" )
							warning ( msg2b , call. = FALSE )
							genStructure <- FALSE
					}
					# Check genDescriptives
					if ( !is.logical ( genDescriptives ) ) {
							msg2c <- paste ( "Argument 'genDescriptives' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										     "It's set to FALSE.\n", sep = "" )
							warning ( msg2c , call. = FALSE )
							genDescriptives <- FALSE
					}						
					# Check genLink
					if ( !is.logical ( genLink ) ) {
							msg2d <- paste ( "Argument 'genLink' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										     "It's set to FALSE.\n", sep = "" )
							warning ( msg2d , call. = FALSE )
							genLink <- FALSE
					}							
					# Check genVarCovMatrix
					if ( !is.logical ( genVarCovMatrix ) ) {
							msg2e <- paste ( "Argument 'genVarCovMatrix' in function 'defineDesign' is not logical (either TRUE or FALSE).\n",
										     "It's set to FALSE.\n", sep = "" )
							warning ( msg2e , call. = FALSE )
							genVarCovMatrix <- FALSE
					}							

					
					
					#### Definition setzen
					empty.definition <- identical ( dsgn@definition , data.frame() )
					old.definition <- dsgn@definition
					if ( !empty.def ) {
							# aus colnames "|" entfernen (d.h. mit "." ersetzen)
							# da "|" sp�ter sonst u.U. Komplikationen
							# (ist auch default von data.frame( "x|y" = NA )
							colnames ( def ) <- gsub ( "|" , "." , colnames ( def ) , fixed = TRUE )
					
							# def alles auf character
							do <- paste ( paste ( "if ( !is.character ( def$\"" , colnames ( def )  , "\" ) ) def$\"" , colnames ( def ) , "\" <- as.character ( def$\"" , colnames ( def ) , "\" )" , sep = "" ) , collapse = "; " )
							eval ( parse ( text = do ) )
					
							# checken auf uniqueness in def
							if ( !empty.def ) {
									dupl <- duplicated ( def )
									if ( any ( dupl ) ) {
											msg13 <- paste ( ( l <- length ( dupl[dupl] ) ) ,
															 ifelse ( l == 1 , " case" , " cases" ) ,
															 " in argument 'def' in function 'defineDesign' " ,
															 ifelse ( l == 1 , "is" , "are" ) ,
															 " not unique and will be removed.\n" ,
															 "This might be the case if the \"lowest level\" element in the data is not explicitely specified.\n" , 
															 "You can try to set an id variable for the \"lowest level\", e.g. with   def$lowestLevelID <- as.character ( seq ( along = rownames ( def ) ) )\n"
															, sep = "" )
											warning ( msg13 , call. = FALSE )
											def <- def [ !dupl , ]
									}
							}
					
							# setzen von @definition in dsgn oder appenden
							if ( ! append ) {
									dsgn@definition <- def
									
									# Message
									# if ( verbose ) {
											# msg3 <- "Slot @definition is set to data frame 'def'.\n"
											# if ( empty.def ) msg3 <- paste ( msg3 , "'def' is an empty data frame.\n" , sep = "     " )
											# else msg3 <- paste ( msg3 , paste ( "'def' is a data frame with " , ncol ( def ) , " columns and " , nrow ( def ) , " rows.\n" , sep = "" ) , sep = "     " )
											# cat ( msg3 )
									# }
							} else { # append
									if ( identical ( dsgn@definition , def ) || empty.def ) {
											# msg4a <- "remains unchanged.\n     It's current value is"
									} else {

											# 2 F�lle:
											# wenn die Schnittmenge der colnames leer ist, dann kann merge nicht verwendet werden
											# ansonsten macht merge mit Optionen incomparibles=NA und all=TRUE das was man -- wahrscheinlich -- will
											
											olddef <- dsgn@definition
											adddef <- def
											intsec <- intersect ( colnames ( olddef ) , colnames ( adddef ) )
											# alle Elemente
											els <- unique ( c ( colnames ( olddef ) , colnames ( adddef ) ) )											
											
											if ( identical ( intsec , character(0) ) ) {
													
													### "unverbundene" Designs, Bsp:
													#   items testlets blocks booklets
													# 1 item1      tl1    NA    NA
													# 2 item2      tl1    NA    NA
													# 3 item3      tl2    NA    NA
													# 4 item4      tl2    NA    NA
													# 5   NA        NA   bl1   bo1
													# 6   NA        NA   bl2   bo1

													# fehlende Spalten erg�nzen mit NA
													# olddef
													do <- paste ( paste ( "if ( is.null ( olddef$\"" , els , "\" ) ) olddef$\"" , els , "\" <- rep ( NA , nrow ( olddef ) )" , sep = "" ) , collapse = "; " )
													eval ( parse ( text = do ) )
													if ( ! identical ( colnames ( olddef ) , els ) ) olddef <- olddef[,els,drop=FALSE]

													# adddef
													do <- paste ( paste ( "if ( is.null ( adddef$\"" , els , "\" ) ) adddef$\"" , els , "\" <- rep ( NA , nrow ( adddef ) )" , sep = "" ) , collapse = "; " )
													eval ( parse ( text = do ) )
													if ( ! identical ( colnames ( adddef ) , els ) ) adddef <- adddef[,els,drop=FALSE]											
												
													# rbinden
													newdef <- rbind ( olddef , adddef )
													# Duplikate entfernen
													newdef <- newdef [ !duplicated ( newdef ) , ]
													
											} else {
													# mergen mit Optionen all=TRUE und incomparibles=NA
													# f�r "gleichartige" Datens�tze ist das "rbind"
													newdef <- merge ( olddef , adddef , by = intsec , all = TRUE , incomparables = NA , sort = FALSE )
													
													# Spalten sortieren
													newdef <- newdef [ , els ]
													
													# checken auf uniqueness in newdef
													if ( is.data.frame ( newdef ) ) {
															if ( nrow ( newdef ) > 0 ) {
																	dupl <- duplicated ( newdef )
																	if ( any ( dupl ) ) {
																			msg4b <- paste ( ( l <- length ( dupl[dupl] ) ) ,
																							 ifelse ( l == 1 , " case" , " cases" ) ,
																							 " in new @definition " ,
																							 ifelse ( l == 1 , "is" , "are" ) ,
																							 " not unique and will be removed.\n" ,
																							, sep = "" )
																			if ( verbose ) {
																					cat ( msg4b )
																			}
																			newdef <- newdef [ !dupl , ]
																	}
															}
													}													
											}
									
											# setzen
											dsgn@definition <- newdef

											# msg4a <- "is appended by 'def'.\n     It's new value is"
											
									}
									
									# append Message
									# if ( verbose ) {
											# msg4 <- paste ( "Slot @definition " , msg4a , " a data frame with " , ncol ( dsgn@definition ) , " columns and " , nrow ( dsgn@definition ) , " rows.\n" , sep = "" )
											# cat ( msg4 )
											# if ( exists ( "msg4b" , inherits = FALSE ) ) cat ( paste ( "     " , msg4b ) )
									# }							
							}
					}
					
					# Variable zum Tracken ob neue Definition gesetzt wurde
					if ( identical ( old.definition , dsgn@definition ) ) {
							new.definition <- FALSE
					} else {
							new.definition <- TRUE
					}
					# verbose
					# if ( verbose ) {
							# if ( new.definition ) {
									# if ( append ) {
											# msg4a <- "is updated to"
									# } else {
											# msg4a <- "is set to"
									# }
							# } else {
									# msg4a <- "remains unchanged.\n     It's current value is"
							# }
							
							# definition.string <- paste ( "a data frame with " , ncol ( dsgn@definition ) , " columns and " , nrow ( dsgn@definition ) , " rows" , sep = "" )
							# msg4 <- paste ( "Slot @definition " , msg4a , " " , definition.string , ".\n" , sep = "" )
							# cat ( msg4 )
							# if ( exists ( "msg4b" , inherits = FALSE ) ) cat ( msg4b )
							# if ( exists ( "msg4c" , inherits = FALSE ) ) cat ( msg4c )
					# }	
					# verbose message
					if ( verbose ) catmsg ( "definition" , empty.definition , old.definition , dsgn@definition )		
					
					
					# Checken von @definition
					# auf data.frame checken (c3) ist wahrscheinlich �berfl�ssig, aber trotzdem zur Sicherheit, wer wei�
					# c3 <- is.data.frame ( dsgn@definition )
					# if ( c3 ) c4 <- nrow ( dsgn@definition ) > 0 && ncol ( dsgn@definition ) > 0 else c4 <- FALSE		
					# empty.definition <- ! ( c3 && c4 )
					
					# neues setzen von empty.defintion, da sp�ter gebraucht
					empty.definition <- identical ( dsgn@definition , data.frame() )
					
				
					# setzen von @elements
					empty.elements <- identical ( dsgn@elements , character(0) )
					old.elements <- dsgn@elements
					if ( new.definition ) {
							dsgn@elements <- colnames ( dsgn@definition )
							# if ( empty.elements ) {
									# msg7a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@elements , old.elements ) ) {
											# msg7a <- "is updated to"
									# } else {
											# msg7a <- "remains unchanged.\n     It's current value is"
									# }	
							# }
					} else {
							# msg7a <- "remains unchanged.\n     It's current value is"
					}
					# Variable zum Tracken ob neue elements gesetzt wurde
					# if ( identical ( old.elements , dsgn@elements ) ) {
							# new.elements <- FALSE
					# } else {
							# new.elements <- TRUE
					# }					
					# if ( verbose ) {
							# if ( new.elements ) {
									# if ( !empty.elements ) {
											# msg7a <- "is updated to"
									# } else {
											# msg7a <- "is set to"
									# }
							# } else {
									# msg7a <- "remains unchanged.\n     It's current value is"
							# }							
							# elements.string <- ifelse ( identical ( dsgn@elements , character(0) ) , "character(0)" , paste ( "c(" , paste ( paste ( "\"" , dsgn@elements , "\"" , sep = "" ) , collapse = ", " ) , ")" , sep = "" ) )					
							# msg7 <- paste ( "Slot @elements " , msg7a , " " , elements.string , ".\n" , sep = "" )
							# cat ( msg7 )
					# }					
					# verbose message
					if ( verbose ) catmsg ( "elements" , empty.elements , old.elements , dsgn@elements )				
				
				
					# setzen von @units (unique Units der Elemente)
					empty.units <- identical ( dsgn@units , list() )
					old.units <- dsgn@units
					if ( new.definition ) {
							if ( empty.definition ) {
									dsgn@units <- list()
							} else {
									u <- sapply ( dsgn@definition, function ( sp ) { r<-na.omit ( unique ( sp ) ); attributes(r)<-NULL; return(r) }
												  , simplify = FALSE, USE.NAMES = TRUE )
									dsgn@units <- u
							}
							# if ( empty.units ) {
									# msg8a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@units , old.units ) ) {
											# msg8a <- "is updated to"
									# } else {
											# msg8a <- "remains unchanged.\n     It's current value is"
									# }	
							# }
					} else {
							# msg8a <- "remains unchanged.\n     It's current value is"
					}
					# Variable zum Tracken ob neue units gesetzt wurde					
					# if ( identical ( old.units , dsgn@units ) ) {
							# new.units <- FALSE
					# } else {
							# new.units <- TRUE
					# }			
					# if ( verbose ) {
							# if ( new.units ) {
									# if ( !empty.units ) {
											# msg8a <- "is updated to"
									# } else {
											# msg8a <- "is set to"
									# }
							# } else {
									# msg8a <- "remains unchanged.\n     It's current value is"
							# }						
							# units.string <- ifelse ( identical ( dsgn@units , list() ) , "list()" , paste ( "a list with the names of " , paste ( paste ( u <- sapply ( dsgn@units , length ) , names ( u ) ) , collapse = ", " ) , sep = "" ) )
							# msg8 <- paste ( "Slot @units " , msg8a , " " , units.string , ".\n" , sep = "" )
							# cat ( msg8 )
					# }
					# verbose message
					if ( verbose ) catmsg ( "units" , empty.units , old.units , dsgn@units )

					
					# setzen von @nunits
					empty.nunits <- identical ( dsgn@nunits , integer(0) )
					old.nunits <- dsgn@nunits
					if ( new.definition ) {
							if ( identical ( dsgn@units , list() ) ) {
									dsgn@nunits <- integer()
							} else {
									dsgn@nunits <- sapply ( dsgn@units , length , simplify = TRUE , USE.NAMES = TRUE )
							}
							
							# if ( empty.nunits ) {
									# msg9a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@nunits , old.nunits ) ) {
											# msg9a <- "is updated to"
									# } else {
											# msg9a <- "remains unchanged.\n     It's current value is"
									# }	
							# }
							
					} else {
							# msg9a <- "remains unchanged.\n     It's current value is"
					}
					# Variable zum Tracken ob neue nunits gesetzt wurde					
					# if ( identical ( old.nunits , dsgn@nunits ) ) {
							# new.nunits <- FALSE
					# } else {
							# new.nunits <- TRUE
					# }							
					# if ( verbose ) {
							# if ( new.nunits ) {
									# if ( !empty.nunits ) {
											# msg9a <- "is updated to"
									# } else {
											# msg9a <- "is set to"
									# }
							# } else {
									# msg9a <- "remains unchanged.\n     It's current value is"
							# }					
							# nunits.string <- ifelse ( identical ( dsgn@nunits , integer(0) ) , "integer(0)" , paste ( "an integer vector with number of " , paste ( paste ( names ( dsgn@nunits ) , "=" , dsgn@nunits , sep = "" ) , collapse = ", " ) , sep = "" ) )
							# msg9 <- paste ( "Slot @nunits " , msg9a , " " , nunits.string , ".\n" , sep = "" )
							# cat ( msg9 )
					# }
					# verbose message
					if ( verbose ) catmsg ( "nunits" , empty.nunits , old.nunits , dsgn@nunits )
					
					
					### hier ehemals guessHierarchy ###
					
					
					# genStructure
					# Logik:
					# f�r genStructure=FALSE && new.definition = TRUE && not.empty.structure = TRUE
					# muss structure troztdem neu gesetzt werden,
					# da sich die Definition ge�ndert hat und dadurch die bereits gesetzte Structure u.U. falsch ist
					empty.structure <- identical ( dsgn@structure , data.frame() )
					old.structure <- dsgn@structure
					if ( genStructure || ( !genStructure && new.definition && ! empty.structure ) ) {
							
							if ( length ( dsgn@elements ) > 1 ) {
									
									# leerer Zieldatensatz
									s <- data.frame ( matrix ( as.character(NA) , nrow = ( n <- length ( dsgn@elements ) ) , ncol = n ) , stringsAsFactors = FALSE )
									colnames ( s ) <- rownames ( s ) <- dsgn@elements

									# f�r bestimmtes Paar an Elementen die Beziehung bestimmen
									getStructure <- function ( ro , co , d ) {
											
											# R�ckgabevariable
											r <- as.character()
											
											if ( ro == co || ( any ( ! c(ro,co) %in% colnames ( d ) ) ) ) {
													r <- as.character(NA)
											} else {
											
													# Datensatz auf aktuelle Elemente reduzieren
													d <- d[,c(ro,co),drop=FALSE]
													
													# wenn auf mind. einem der beiden Elemente NA, dann raus
													del <- apply ( d , 1 , function ( z ) any ( is.na ( z ) ) )
													d <- d[ !del , ]
													
													# wenn jetzt keine Zeilen mehr im Datensatz, dann "unconnected"
													if ( nrow ( d ) == 0 ) {
															r <- "unconnected"
													} else {
															
															# unique units
															ro.u <- unique ( d[,ro] )
															co.u <- unique ( d[,co] )
															
															# wie oft ist jede Unit des einen Elements in Unit des zweiten Elemtents
															nOtherUnit <- function ( u , d , sp1 , sp2 ) {
																	
																	r <- integer()
																	d <- d[ d[,sp1] == u , ]
																	r <- length ( unique ( d[,sp2] ) )
																	return ( r )
															}
															nou1 <- mapply ( nOtherUnit , ro.u , MoreArgs = list ( d , ro , co ) , SIMPLIFY = TRUE )
															nou2 <- mapply ( nOtherUnit , co.u , MoreArgs = list ( d , co , ro ) , SIMPLIFY = TRUE )													
														
															# bestimmen der Relation
															if ( all ( nou1 == 1 ) && all ( nou2 == 1 ) ) {
																	r <- "equivalent"
															} else if ( all ( nou2 == 1 ) ) {
																	r <- "nestor"
															} else if ( all ( nou1 == 1 ) ) {
																	r <- "nested"
															} else {
																	# jetzt kanns nur noch gecrossed sein
																	# rausbekommen ob completely oder partly
																	
																	compl1 <- all ( nou1 == length ( co.u ) )
																	compl2 <- all ( nou2 == length ( ro.u ) )
																	
																	if ( compl1 && compl2 ) {
																			r <- "crossedcompletely"
																	} else {
																			r <- "crossedpartially"
																	}
															}
													}
											}
											
											return ( r )
									}
					
									# "Paare", rows und columns
									lowTr <- dfr2long ( s , lower = TRUE , upper = FALSE , diag = FALSE , use.names = TRUE )
									ro <- lowTr$row
									co <- lowTr$col
									
									# f�r Elemente-Kombinationen die Relation holen
									st <- mapply ( getStructure , ro , co , MoreArgs = list ( dsgn@definition ) , SIMPLIFY = TRUE , USE.NAMES = FALSE )
									
									# setzen der Elemente des unteren Dreiecks in Ergebnismatrix
									eval ( parse ( text = paste ( "s[\"", ro , "\",\"" , co , "\"]<-\"" , st , "\"" , sep = "" ) ) )
									
									# spiegeln des lower Triangle ins upper in der Ergebnismatrix
									# dabei auf "nested"/"nestor" achten
									do <- paste ( "s[\"", co , "\",\"" , ro , "\"]<-\"" , st , "\"" , sep = "" )
									do <- gsub ( "nested" , "NESTOR" , do , fixed = TRUE )
									do <- gsub ( "nestor" , "nested" , do , fixed = TRUE )
									do <- gsub ( "NESTOR" , "nestor" , do , fixed = TRUE )
									eval ( parse ( text =  do ) )

									# setzen
									dsgn@structure <- s
							
							} else {
									# wenn nur ein Element, dann noch f�r verbose ne Message
									if ( length ( dsgn@elements ) == 1 ) {
											msg12b <- "     Note: There is just 1 element. That's why no there's no structure of elements.\n"
									}
							
									dsgn@structure <- data.frame()
							}
							
							# if ( empty.structure ) {
									# msg12a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@structure , old.structure ) ) {
											# msg12a <- "is updated to"
									# } else {
											# msg12a <- "remains unchanged.\n     It's current value is"
									# }									
							# }
							
							# noch ne Message wenn trotz !genStructure structure gesetzt wurde
							if ( !genStructure && new.definition && ! empty.structure ) {
									# msg12c <- paste ( "     Although genStructure=FALSE, slot @structure is updated,\n" ,
													  # "     since a new definition has been set. This is to avoid misspecifications.\n"
													  # , sep = "" )
									# msg12a <- "is updated to"
									msg12c <- paste ( "     Although descriptives=FALSE, slot @structure is updated,\n" ,
													  "     since a new definition has been set. This is to avoid misspecifications.\n"
													  , sep = "" )									
							}
							
					} else {
							# msg12a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "structure" , empty.structure , old.structure , dsgn@structure )	
							# structure.string <- paste ( "a data frame with " , ncol ( dsgn@structure ) , " columns and " , nrow ( dsgn@structure ) , " rows" , sep = "" )
							# msg12 <- paste ( "Slot @structure " , msg12a , " " , structure.string , ".\n" , sep = "" )
							# cat ( msg12 )
							if ( exists ( "msg12b" , inherits = FALSE ) ) cat ( msg12b )
							if ( exists ( "msg12c" , inherits = FALSE ) ) cat ( msg12c )
					}			

					
					
					
					# structureList
					# Logik: wenn structure nicht leer, dann setzen
					empty.structure <- identical ( dsgn@structure , data.frame() )
					empty.structureList <- identical ( dsgn@structureList , list() )
					old.structureList <- dsgn@structureList
					if ( !empty.structure ) {
									# Zieldatensatz als lower und upper triangle von @structure im long Format
									# da man wahrscheinlich von der Sortierung her erst upper haben will: 2 schrittig
									s1 <- dfr2long ( dsgn@structure , lower = FALSE , upper = TRUE , diag = FALSE , use.names = TRUE )
									s2 <- dfr2long ( dsgn@structure , lower = TRUE , upper = FALSE , diag = FALSE , use.names = TRUE )
									s <- rbind ( s1 , s2 )
									
									# nestor zu nested
									# if ( any ( ( w <- s$value == "nestor" ) ) )  {
											
											# s[ w , "value" ] <- "nested"
											# temp <- s [ w , "col" ]
											# s[ w , "col" ] <- s [ w , "row" ]
											# s[ w , "row" ] <- temp
											
									# }
									
									# neue Spaltennamen
									colnames ( s ) <- c ( "element1" , "element2" , "structure" )
									
									# StructurList erzeugen
									# (detailierte Liste mit units von Element1 bzgl. Element2)
									makeStructureList <- function ( el1 , el2 , def ) { 
										
											s <- def [ , c ( el1 , el2 ) , drop = FALSE ]
											
											# unique units von element1 bzgl. element2
											f <- function ( el2 , s ) {
													s <- s [ s[,2] == el2 , , drop = FALSE ]
													ret <- na.omit ( unique ( s[,1] ) )
													if ( !is.null( attributes ( ret ) ) ) attributes ( ret ) <- NULL
													return ( ret )
											}
										
											els <- na.omit ( unique ( s[,2] ) )
											if ( !is.null( attributes(els) ) ) attributes ( els ) <- NULL
											sl <- mapply ( f , els , MoreArgs = list ( s ) , SIMPLIFY = FALSE )
											
									}
									structureList <- mapply ( makeStructureList , s$element1 , s$element2 , MoreArgs = list ( dsgn@definition ) , SIMPLIFY = FALSE )

									# Namen setzen
									if ( is.list ( structureList ) ) {
											if ( length ( structureList ) > 0 ) {
													# Namen setzen
													names ( structureList ) <- paste ( s$element1 , "|" , s$element2 , sep = "" )
											} else dsgn@structureList <- list()
									} else dsgn@structureList <- list()
									
									# setzen
									dsgn@structureList <- structureList									
									
									# message
									# if ( empty.structureList ) {
											# msg15a <- "is set to"
									# } else {
											# if ( ! identical ( dsgn@structureList , old.structureList ) ) {
													# msg15a <- "is updated to"
											# } else {
													# msg15a <- "remains unchanged.\n     It's current value is"
											# }
									# }									
									
					} else {
									# msg15a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "structureList" , empty.structureList , old.structureList , dsgn@structureList )
							# structureList.string <- paste ( "a list with " , length ( dsgn@structureList ) , " elements" , sep = "" )
							# msg15 <- paste ( "Slot @structureList " , msg15a , " " , structureList.string , ".\n" , sep = "" )
							# cat ( msg15 )
					}	
										
				
					# genDescriptives
					# Logik: wie f�r genStructure
					empty.descriptives <- identical ( dsgn@descriptives , data.frame() )
					old.descriptives <- dsgn@descriptives
					if ( genDescriptives || ( !genDescriptives && new.definition && ! empty.descriptives ) ) {
							
							if ( identical ( dsgn@structure , data.frame() ) ) {
									# msg14b <- paste ( "Argument 'genDescriptives' in function 'defineDesign' is TRUE,\n" ,
													  # "but slot @structure, which is needed to generate descriptives, is empty.\n" ,
													  # "You can run 'defineDesign' with argument 'genStructure = TRUE' or\n" ,
													  # "update the current design with   dsgn <- updateDesign( dsgn , genStructure = TRUE )\n" ,
													  # "where 'dsgn' is your design object.\n"
													  # , sep = "" )
									# statt Warnung wird jetzt einfach ben�tigte structure besorgt
									# aber nicht gesetzt (um intuitive Konsistenz zu wahren)
									dsgn2 <- update.design ( dsgn = dsgn , genStructure = TRUE , genDescriptives = FALSE , genLink = FALSE , genVarCovMatrix = FALSE , verbose = FALSE )
							} else {
									# aktuelles Design "clonen"
									dsgn2 <- dsgn
							}
									
							if ( ! identical ( dsgn2@structure , data.frame() ) ) {
							
									# Zieldatensatz aus @structureList
									spl <- strsplit ( names ( dsgn2@structureList ) , "|" , fixed = TRUE ) 
									d <- data.frame ( "element1" = sapply( spl , "[" , 1 ) , "element2" = sapply( spl , "[" , 2 ) , stringsAsFactors = FALSE )
									
									# structure noch an d ran
									st <- NULL
									do <- paste ( "st <- c ( st , dsgn2@structure[ \"" , d$element1 , "\" , \"" , d$element2 , "\"] )" , sep = "" ) 
									eval ( parse ( text = do ) )
									d$structure <- st 
									d <- d [ , c ( "element1" , "structure" , "element2" ) ]
									rownames ( d ) <- seq ( along = rownames ( d ) )
									
									# Deskriptives erzeugen
									makeDescr <- function ( l ) {

											le <- sapply ( l , length )
											
											# Descriptives
											min <- min ( le )
											max <- max ( le )
											mean <- mean ( le ) 
											median <- median ( le )
											sd <- sd ( le ) 
											
											# sd macht NA wenn nur ein Element
											# (wahrscheinlich) sinnvoller f�r den hierigen Anwendungsfall, das auch auf 0 zu setzen
											# damit konsistent mit wenn meherere Elemente
											if ( is.na ( sd ) ) sd <- 0
											
											d <- data.frame ( min , max , mean , median , sd )
											
											return ( d )
									}
									descr <- mapply ( makeDescr , dsgn2@structureList , SIMPLIFY = FALSE )
									descr <- do.call ( "rbind" , descr )
									# da gleiche Sortierung, kann direkt ran an d (trotzdem Vorsicht!)
									d <- cbind ( d , descr )

									# setzen
									dsgn@descriptives <- d 
									
									# message
									# if ( empty.descriptives ) {
											# msg14a <- "is set to"
									# } else {
											# if ( ! identical ( dsgn@descriptives , old.descriptives ) ) {
													# msg14a <- "is updated to"
											# } else {
													# msg14a <- "remains unchanged.\n     It's current value is"
											# }
									# }
									
									# noch ne Message wenn trotz !genDescriptives descriptives gesetzt wurde
									if ( !genDescriptives && new.definition && ! empty.descriptives ) {
											# msg14b <- paste ( "     Although genDescriptives=FALSE, slot @descriptives is updated,\n" ,
															  # "     since a new definition has been set. This is to avoid misspecifications.\n"
															  # , sep = "" )
											# msg14a <- "is updated to"
											msg14b <- paste ( "     Although descriptives=FALSE, slot @descriptives is updated,\n" ,
															  "     since a new definition has been set. This is to avoid misspecifications.\n"
															  , sep = "" )											
									}
							} else {
									# msg14a <- "remains unchanged.\n     It's current value is"
							}		
					} else {
							# msg14a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "descriptives" , empty.descriptives , old.descriptives , dsgn@descriptives )								
							# descriptives.string <- paste ( "a data frame with " , ncol ( dsgn@descriptives ) , " columns and " , nrow ( dsgn@descriptives ) , " rows" , sep = "" )
							# msg14 <- paste ( "Slot @descriptives " , msg14a , " " , descriptives.string , ".\n" , sep = "" )
							# cat ( msg14 )
							if ( exists ( "msg14b" , inherits = FALSE ) ) cat ( msg14b )
					}					
					### Ende genDescriptives
					
				
					# erstmal linkList
					# parametrisiert �ber genLink
					# Logik: wie f�r genDescriptives
					empty.linkList <- identical ( dsgn@linkList , list() )
					old.linkList <- dsgn@linkList
					if ( genLink || ( !genLink && new.definition && ! empty.linkList ) ) {
							
							if ( identical ( dsgn@structure , data.frame() ) ) {
								
									dsgn3 <- update.design ( dsgn = dsgn , genStructure = TRUE , genDescriptives = FALSE , genLink = FALSE , genVarCovMatrix = FALSE , verbose = FALSE )
							} else {
									# aktuelles Design "clonen"
									dsgn3 <- dsgn
							}
								
							if ( ! identical ( dsgn3@structure , data.frame() ) ) {
						
									# Zieldatensatz aus @structureList
									spl <- strsplit ( names ( dsgn3@structureList ) , "|" , fixed = TRUE ) 
									d <- data.frame ( "element1" = sapply( spl , "[" , 1 ) , "element2" = sapply( spl , "[" , 2 ) , stringsAsFactors = FALSE )
									
									# structure noch an d ran
									st <- NULL
									do <- paste ( "st <- c ( st , dsgn3@structure[ \"" , d$element1 , "\" , \"" , d$element2 , "\"] )" , sep = "" ) 
									eval ( parse ( text = do ) )
									d$structure <- st 
									d <- d [ , c ( "element1" , "structure" , "element2" ) ]
									rownames ( d ) <- seq ( along = rownames ( d ) )
									
									# Graphs erzeugen
									makeGraphs <- function ( l ) {

											makeCombs <- function ( m ) {
													# jedes Element mit jedem anderen verbinden
													if ( length ( m ) >= 2 ) {
															s <- combn ( sort ( m ) , 2 , simplify = FALSE )
													} else if ( length ( m ) == 1 ) {
															s <- list ( m )
													} else {
															s <- list ( NULL )
													}
													# if ( !is.null(s) ) r <- sapply ( s , paste , collapse = "-" ) else r <- s
													# if ( !is.null(s) ) r <- sapply ( s , paste , collapse = "-" , USE.NAMES = FALSE ) else r <- s
												
													return ( s )
											}
										
											edges <- mapply ( makeCombs , l , SIMPLIFY = FALSE , USE.NAMES = FALSE )
											edges <- do.call ( c , edges )
											# NULL raus
											edges <- edges[!sapply(edges, is.null)]
											# hier k�nnten jetzt auch noch unverbundene Vertices dabei sein,
											# diese rausholen (vs) und sp�ter zum Graph hinzuf�gen
											vl <- sapply ( edges , length ) == 1
											if ( any ( vl ) ) {
													vs <- edges[vl] 
											} else {
													vs <- NULL
											}
											edges <- edges[!vl]
											
											# edges bauen
											edges <- sapply ( edges , paste , collapse = "-" )
							
											# Tabulieren, das sind dann die Gewichte
											if ( ! identical ( edges , list() ) ) {
													ct <- table ( edges )
											} else {
													ct <- NULL
											}
												
											# String bauen f�r graph.formula
											if ( !is.null ( ct ) ) {
													string <- paste ( names ( ct ) , collapse = "," )
											} else {
													string <- NULL
											}

											# wenn es noch unverbundene Vertices gibt, noch mit rin
											if ( !is.null ( vs ) ) {
													add <- unique ( do.call ( c , vs ) )
													add <- paste ( add , collapse = "," )
													if ( !is.null ( string ) ) {
															string <- paste ( string , add , sep = "," )
													} else {
															string <- add
													}
											}
											
											# undirected graph bauen
											do <- paste ( "graph.formula ( " , string , " )" , sep = "" )
											gr <- eval ( parse ( text = do ) )
									
											# edges gewichten (nach Vorkommensh�ufigkeit der edge (paarweiser Link))
											if ( ! is.null ( ct ) ) E(gr)$weight <- unname ( ct )

											# vertices gewichten (nach Vorkommensh�ufigkeit der Unit)
											els <- unname ( do.call ( c , l ) )
											tels <- table ( els )
											if ( ! is.null ( V(gr)$name ) ) V(gr)$weight <- tels [ V(gr)$name ]
											
											return ( gr )
									}

									gr <- mapply ( makeGraphs , dsgn3@structureList , SIMPLIFY = FALSE )
								
									# setzen
									dsgn@linkList <- gr
									
									# message
									# if ( empty.linkList ) {
											# msg16a <- "is set to"
									# } else {
											# if ( ! identical ( dsgn@linkList , old.linkList ) ) {
													# msg16a <- "is updated to"
											# } else {
													# msg16a <- "remains unchanged.\n     It's current value is"
											# }
									# }
									
									# noch ne Message wenn trotz !genLink linkList gesetzt wurde
									if ( !genLink && new.definition && ! empty.linkList ) {
											# msg16b <- paste ( "     Although genLink=FALSE, slot @linkList is updated,\n" ,
															  # "     since a new definition has been set. This is to avoid misspecifications.\n"
															  # , sep = "" )
											# msg16a <- "is updated to"
											msg16b <- paste ( "     Although descriptives=FALSE, slot @linkList is updated,\n" ,
															  "     since a new definition has been set. This is to avoid misspecifications.\n"
															  , sep = "" )											
									}
							} else {
									# msg16a <- "remains unchanged.\n     It's current value is"
							}		
					} else {
							# msg16a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "linkList" , empty.linkList , old.linkList , dsgn@linkList ) 
							# linkList.string <- paste ( "a list with " , length ( dsgn@linkList ) , " elements" , sep = "" )
							# msg16 <- paste ( "Slot @linkList " , msg16a , " " , linkList.string , ".\n" , sep = "" )
							# cat ( msg16 )
							if ( exists ( "msg16b" , inherits = FALSE ) ) cat ( msg16b )

					}					
					### Ende linkList / genLink					
					
					
					# adjacency
					# setzen wenn linkList da ist
					empty.linkList <- identical ( dsgn@linkList , data.frame() )
					empty.adjacency <- identical ( dsgn@adjacency , list() )
					old.adjacency <- dsgn@adjacency
					if ( !empty.linkList ) {						
						
							# �ber linkList adjacency Matrizen erzeugen
							makeLinkDescr <- function ( gr ) {
									get.adjacency( gr )
							}
							al <- mapply ( makeLinkDescr , dsgn@linkList , SIMPLIFY = FALSE )
					
							# setzen
							dsgn@adjacency <- al
							
							# message
							# if ( empty.adjacency ) {
									# msg17a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@adjacency , old.adjacency ) ) {
											# msg17a <- "is updated to"
									# } else {
											# msg17a <- "remains unchanged.\n     It's current value is"
									# }
							# }
							
							# noch ne Message wenn trotz !genLink adjacency gesetzt wurde
							if ( !genLink && new.definition && ! empty.adjacency ) {
									# msg17b <- paste ( "     Although genLink=FALSE, slot @adjacency is updated,\n" ,
													  # "     since a new definition has been set. This is to avoid misspecifications.\n"
													  # , sep = "" )
									# msg17a <- "is updated to"
									msg17b <- paste ( "     Although descriptives=FALSE, slot @adjacency is updated,\n" ,
													  "     since a new definition has been set. This is to avoid misspecifications.\n"
													  , sep = "" )									
							}
					} else {
							# msg17a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "adjacency" , empty.adjacency , old.adjacency , dsgn@adjacency ) 
							# adjacency.string <- paste ( "a list with " , length ( dsgn@adjacency ) , " elements" , sep = "" )
							# msg17 <- paste ( "Slot @adjacency " , msg17a , " " , adjacency.string , ".\n" , sep = "" )
							# cat ( msg17 )
							if ( exists ( "msg17b" , inherits = FALSE ) ) cat ( msg17b )
					}
					### Ende adjacency
					
					
					# link
					# setzen wenn linkList und adjacency da ist
					empty.linkList <- identical ( dsgn@linkList , list() )
					empty.adjacency <- identical ( dsgn@adjacency , list() )
					empty.link <- identical ( dsgn@link , data.frame() )
					old.link <- dsgn@link
					if ( !empty.linkList & !empty.adjacency ) {

							### f�r Zieldatensatz
							if ( ! identical ( dsgn@structure , data.frame() ) ) {
									dsgn4 <- dsgn
							} else if ( exists ( "dsgn3" , inherits = FALSE ) && ! identical ( dsgn3@structure , data.frame() ) ) {
									dsgn4 <- dsgn3
							} else {
									dsgn4 <- new ( "design" )
							}
							
							if ( ! identical ( dsgn4@structure , data.frame() ) ) {
							
									# Zieldatensatz aus @structureList
									spl <- strsplit ( names ( dsgn4@structureList ) , "|" , fixed = TRUE ) 
									d <- data.frame ( "element1" = sapply( spl , "[" , 1 ) , "element2" = sapply( spl , "[" , 2 ) , stringsAsFactors = FALSE )
									
									# structure noch an d ran
									st <- NULL
									do <- paste ( "st <- c ( st , dsgn4@structure[ \"" , d$element1 , "\" , \"" , d$element2 , "\"] )" , sep = "" ) 
									eval ( parse ( text = do ) )
									d$structure <- st 
									d <- d [ , c ( "element1" , "structure" , "element2" ) ]
									rownames ( d ) <- seq ( along = rownames ( d ) )					
							
									# �ber linkList die Descriptives erzeugen
									makeLinkDescr <- function ( gr ) {
									
											ld <- list()
											# Graph descriptives, kann ggf. erweitert werden
											ld$linklength <- average.path.length ( gr )
											if ( is.nan ( ld$linklength ) ) ld$linklength <- NA
											# ld$betweenness <- betweenness ( gr )
											# ld$closeness <- closeness ( gr )
											ld$degree <- degree ( gr )
											# ld$diameter <- diameter ( gr )
											# ld$farthest.nodes <- farthest.nodes ( gr )
											# ld$vcount <- vcount ( gr )
											ld$eweight <- E(gr)$weight
											
											return ( ld )
									
									}
									ld <- mapply ( makeLinkDescr , dsgn@linkList , SIMPLIFY = FALSE )
	
									### link rate 1: Anzahl paarweise verbundener Elemente an allen m�glichen paarweisen Verbindungen
									makeLinkRate1 <- function ( adj ) {							
							
											if ( ! adj@Dim[1] == 0 ) {
													# lower Triangle ohne Diagonale besorgen
													lt <- tril ( adj , -1 )
													# nicht 0 durchz�hlen
													co <- length ( which ( lt@x > 0 ) )
													# alle
													# comax <- lt@Dim[1] * ( lt@Dim[1] - 1 ) / 2
													comax <- choose ( lt@Dim[1] , 2 )
													
													# linkrate1
													linkrate1 <- co / comax
													if ( is.infinite ( linkrate1 ) | is.nan ( linkrate1 ) ) linkrate1 <- NA
											} else {
													linkrate1 <- NA
											}
									
											return ( linkrate1 )
									}
									linkrate1 <- mapply ( makeLinkRate1 , dsgn@adjacency , SIMPLIFY = FALSE )	
									
									
									### link rate 2: Anzahl an realisierten Paaren an Anzahl m�glicher Paare wenn completely crossed
									# makeLinkRate2 <- function ( na , adj , nunits ) {							
							
											# lower Triangle ohne Diagonale besorgen
											# lt <- tril ( adj , -1 )
											# 0 durchz�hlen
											# co <- length ( which ( lt@x > 0 ) )
											# alle
											# comax <- length ( lt@x )
											
											# anderes Element
											# spl <- strsplit ( na , "|" , fixed = TRUE )
											# el2 <- sapply ( spl , "[" , 2 )										
											# comax mit Anzahl units des anderen Elements multiplizieren
											# comax2 <- comax * nunits[el2]
											
											# linkrate2
											# linkrate2 <- co / comax2
											# if ( is.infinite ( linkrate2 ) | is.nan ( linkrate2 ) ) linkrate2 <- NA							
									
											# return ( linkrate2 )
									# }
									# linkrate2 <- mapply ( makeLinkRate2 , names ( dsgn@adjacency ) , dsgn@adjacency , MoreArgs = list ( dsgn@nunits ) , SIMPLIFY = FALSE )
									
									### link rate 2: Anzahl an realisierten Paaren an Anzahl m�glicher Paare wenn completely crossed
									makeLinkRate2 <- function ( na , ld , u ) {

											spl <- strsplit ( na , "|" , fixed = TRUE )
											el1 <- sapply ( spl , "[" , 1 )
											el2 <- sapply ( spl , "[" , 2 )
											
											# m�gliche Paare
											le <- length ( u[[ el1 ]] ) 
											# co <- le * ( le - 1 ) / 2
											co <- choose ( le , 2 )
											# mal Anzahl units der anderen Ebene
											co2 <- co * length ( u[[ el2 ]] )
									
											# realisierte paarweise Links geteilt durch maximal m�gliche
											if ( ! identical ( ld$eweight , integer(0) ) ) {
													linkrate2 <- sum ( ld$eweight ) / co2
											} else {
													linkrate2 <- NA
											}
											if ( is.infinite ( linkrate2 ) | is.nan ( linkrate2 ) ) linkrate2 <- NA
											
											return ( linkrate2 )
									}
									linkrate2 <- mapply ( makeLinkRate2 , names ( ld ) , ld , MoreArgs = list ( dsgn@units ) , SIMPLIFY = FALSE )
								
									### linkstrength: Mittelwert degree
									makeLinkStrength <- function ( ld ) {
											mean ( ld$degree )
									}
									linkstrength <- mapply ( makeLinkStrength , ld , SIMPLIFY = FALSE )							
									
									### linkdispersion: SD degree
									makeLinkDispersion <- function ( ld ) {
											sd ( ld$degree )
									}
									linkdispersion <- mapply ( makeLinkDispersion , ld , SIMPLIFY = FALSE )									
									
									# descriptives in Datensatz auff�llen
									els <- paste ( d$element1 , d$element2 , sep = "|" )
									d$linklength <- do.call ( c , mapply ( function ( ld , w ) unname(ld[[w]]) , ld , MoreArgs = list ( "linklength" ) , SIMPLIFY = FALSE )[els] )
									d$linkrate1 <- do.call ( c , linkrate1[els] )
									d$linkrate2 <- do.call ( c , linkrate2[els] )
									d$linkstrength <- do.call ( c , linkstrength[els] )
									d$linkdispersion <- do.call ( c , linkdispersion[els] )

									# setzen
									dsgn@link <- d
									
									# message
									# if ( empty.link ) {
											# msg18a <- "is set to"
									# } else {
											# if ( ! identical ( dsgn@link , old.link ) ) {
													# msg18a <- "is updated to"
											# } else {
													# msg18a <- "remains unchanged.\n     It's current value is"
											# }
									# }
									
									# noch ne Message wenn trotz !genLink link gesetzt wurde
									if ( !genLink && new.definition && ! empty.link ) {
											# msg18b <- paste ( "     Although genLink=FALSE, slot @link is updated,\n" ,
															  # "     since a new definition has been set. This is to avoid misspecifications.\n"
															  # , sep = "" )
											# msg18a <- "is updated to"
											msg18b <- paste ( "     Although descriptives=FALSE, slot @link is updated,\n" ,
															  "     since a new definition has been set. This is to avoid misspecifications.\n"
															  , sep = "" )											
									}
							
							} else {
									# msg18a <- "remains unchanged.\n     It's current value is"
							}
					} else {
							# msg18a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "link" , empty.link , old.link , dsgn@link ) 
							# link.string <- paste ( "a data frame with " , ncol ( dsgn@link ) , " columns and " , nrow ( dsgn@link ) , " rows" , sep = "" )
							# msg18 <- paste ( "Slot @link " , msg18a , " " , link.string , ".\n" , sep = "" )
							# cat ( msg18 )
							if ( exists ( "msg18b" , inherits = FALSE ) ) cat ( msg18b )
							
					}
					### Ende link

					
					# varCovMatrix
					# setzen wenn definition da ist
					empty.definition <- identical ( dsgn@definition , data.frame() )
					empty.units <- identical ( dsgn@units , list() )
					empty.varCovMatrix <- identical ( dsgn@varCovMatrix , matrix()[FALSE,FALSE] )
					old.varCovMatrix <- dsgn@varCovMatrix
					if ( genVarCovMatrix || ( !genVarCovMatrix && !empty.varCovMatrix && new.definition ) ) {
					
							if ( !empty.definition && !empty.units ) {						

									# Definition
									def <- dsgn@definition
									
									# def nach numerisch (durchz�hlen der Elemente von 1 an)
									do <- paste ( sapply ( colnames ( def ) , function ( na ) paste ( "\"" , na , "\" = match ( def$\"" , na , "\", dsgn@units$\"" , na , "\" ) " , sep = "" ) ) , collapse = " , " )
									do <- paste ( "def2 <- data.frame ( " , do , " ) " , sep = "" )
									def2 <- eval ( parse ( text = do ) )
									
									# matrix mit pairwise.complete.obs (ist das gut?)
									ma <- cov ( def2 , use="pairwise.complete.obs" )
								
									# setzen
									dsgn@varCovMatrix <- ma
									
									# message
									# if ( empty.varCovMatrix ) {
											# msg19a <- "is set to"
									# } else {
											# if ( ! identical ( dsgn@varCovMatrix , old.varCovMatrix ) ) {
													# msg19a <- "is updated to"
											# } else {
													# msg19a <- "remains unchanged.\n     It's current value is"
											# }
									# }
									
									# noch ne Message wenn trotz !genVarCovMatrix varCovMatrix gesetzt wurde
									if ( !genVarCovMatrix && new.definition && ! empty.varCovMatrix ) {
											# msg19b <- paste ( "     Although genVarCovMatrix=FALSE, slot @varCovMatrix is updated,\n" ,
															  # "     since a new definition has been set. This is to avoid misspecifications.\n"
															  # , sep = "" )
											# msg19a <- "is updated to"
											msg19b <- paste ( "     Although descriptives=FALSE, slot @varCovMatrix is updated,\n" ,
															  "     since a new definition has been set. This is to avoid misspecifications.\n"
															  , sep = "" )											
									}
							} else {
									# msg19a <- "remains unchanged.\n     It's current value is"
							}
					}
					if ( verbose ) {
							catmsg ( "varCovMatrix" , empty.varCovMatrix , old.varCovMatrix , dsgn@varCovMatrix ) 
							# varCovMatrix.string <- paste ( "a matrix with " , ncol ( dsgn@varCovMatrix ) , " columns and " , nrow ( dsgn@varCovMatrix ) , " rows" , sep = "" )
							# msg19 <- paste ( "Slot @varCovMatrix " , msg19a , " " , varCovMatrix.string , ".\n" , sep = "" )
							
							# cat ( msg19 )
							if ( exists ( "msg19b" , inherits = FALSE ) ) cat ( msg19b )
					}
					### Ende varCovMatrix					
					
					# designDescriptives
					# einfach zusammensammeln was da ist
					empty.varCovMatrix <- identical ( dsgn@varCovMatrix , matrix()[FALSE,FALSE] )
					empty.designDescriptives <- identical ( dsgn@designDescriptives , list() )
					old.designDescriptives <- dsgn@designDescriptives
					## wenn noch mehr dazu kommt "oder" verkn�pfen
					if ( !empty.varCovMatrix ) {						
							
							# ggf. einzeln abfangen
							if ( !empty.varCovMatrix ) {
						
									# Doptimality berechnen
									solved <- try ( solve ( dsgn@varCovMatrix ) , silent = TRUE )
									if ( ! inherits ( solved , "try-error" ) ) {
											Doptimality <- try ( det ( dsgn@varCovMatrix*solved ) , silent = TRUE )
											if ( inherits ( Doptimality , "try-error" ) ) {
													Doptimality <- as.numeric(NA)
											}
									} else {
											Doptimality <- as.numeric(NA)
									}
									if ( is.na ( Doptimality ) ) {
											msg20b <- "     D-optimality index could not be computed due to not solvable design matrix.\n"
									}
							} else {
									### Message im Prinzip sinnlos da nie eintreten kann, da !empty.varCovMatrix
									# msg20c <- paste ( "     D-optimality index could not be computed because slot @varCovMatrix is empty.\n" , 
									#				    "     run updateDesign ( dsgn = <yourdesignobject> , genVarCovMatrix = TRUE ) to update your design.\n" , sep = "" )
									Doptimality <- as.numeric(NA)
							}
							
							### Datensatz bauen
							# do1 <- "\"Doptimality\" = Doptimality"
							# do <- paste ( "d <- data.frame ( " , paste ( do1 , collapse = "," ) , " , stringsAsFactors = FALSE ) " , sep = "" )
							# eval ( parse ( text = do ) )
							
							### Liste bauen
							do1 <- "\"Doptimality\" = Doptimality"
							do <- paste ( "l <- list ( " , paste ( do1 , collapse = "," ) , " ) " , sep = "" )
							eval ( parse ( text = do ) )
							
							# setzen
							dsgn@designDescriptives <- l
							
							# message
							# if ( empty.designDescriptives ) {
									# msg20a <- "is set to"
							# } else {
									# if ( ! identical ( dsgn@designDescriptives , old.designDescriptives ) ) {
											# msg20a <- "is updated to"
									# } else {
											# msg20a <- "remains unchanged.\n     It's current value is"
									# }
							# }

					} else {
							# msg20a <- "remains unchanged.\n     It's current value is"
					}
					if ( verbose ) {
							catmsg ( "designDescriptives" , empty.designDescriptives , old.designDescriptives , dsgn@designDescriptives ) 
							# designDescriptives.string <- paste ( "a data frame with " , ncol ( dsgn@designDescriptives ) , " columns and " , nrow ( dsgn@designDescriptives ) , " rows" , sep = "" )
							# msg20 <- paste ( "Slot @designDescriptives " , msg20a , " " , designDescriptives.string , ".\n" , sep = "" )
							
							# cat ( msg20 )
							if ( exists ( "msg20b" , inherits = FALSE ) ) cat ( msg20b )
							# if ( exists ( "msg20c" , inherits = FALSE ) ) cat ( msg20c )
					}
					### Ende designDescriptives										

					# auf altes Warn-Level z�r�cksetzen
					options ( oldwarn )
					
					#### Finales Design Objekt zur�ckgeben ####
					return( dsgn )
}

updateDesign <- function ( dsgn , descriptives = TRUE , verbose = FALSE ) {
		dsgn2 <- defineDesign ( dsgn , def = data.frame() , append = TRUE , descriptives = TRUE , verbose = verbose )
}

update.design <- function ( dsgn , genStructure = TRUE , genDescriptives = TRUE , genLink = TRUE , genVarCovMatrix = TRUE , verbose = FALSE ) {
		dsgn2 <- define.design ( dsgn , def = data.frame() , append = TRUE , genStructure = genStructure , genDescriptives = genDescriptives , genLink = genLink , genVarCovMatrix = genVarCovMatrix , verbose = verbose )
}

# "+" 
setMethod ( f = "+" , signature = signature ( e1="design" , e2="design" ) ,
			definition = function ( e1 , e2 ) {
					# wenn auf einem der beiden Objekte "descriptives" o.�. dann auch auf dem neuen
					# ERGAENZEN
					genStructure <- ifelse ( any ( !identical ( e1@structure , data.frame() ) , !identical ( e2@structure , data.frame() ) ) , TRUE , FALSE )
					genDescriptives <- ifelse ( any ( !identical ( e1@descriptives , data.frame() ) , !identical ( e2@descriptives , data.frame() ) ) , TRUE , FALSE )
					genLink <- ifelse ( any ( !identical ( e1@link , data.frame() ) , !identical ( e2@link , data.frame() ) ) , TRUE , FALSE )
					genVarCovMatrix <- ifelse ( any ( !identical ( e1@varCovMatrix , matrix()[FALSE,FALSE] ) , !identical ( e2@varCovMatrix , matrix()[FALSE,FALSE] ) ) , TRUE , FALSE )
					
					# Objekte addieren indem die Definition des 2. Objekts an das 1. appended wird
					n <- define.design ( dsgn = e1 , def = e2@definition , append = TRUE , genStructure = genStructure , genDescriptives = genDescriptives , genLink = genLink , genVarCovMatrix = genVarCovMatrix , verbose = FALSE )
					
					return ( n )
			}
)

# "-" 
setMethod ( f = "-" , signature = signature ( e1="design" , e2="design" ) ,
			definition = function ( e1 , e2 ) {
					
					# R�ckgabevariable
					n <- new ( "design" )
					
					# wenn auf einem der beiden Objekte "descriptives" o.�. dann auch auf dem neuen
					# ERGAENZEN
					genStructure <- ifelse ( any ( !identical ( e1@structure , data.frame() ) , !identical ( e2@structure , data.frame() ) ) , TRUE , FALSE )
					genDescriptives <- ifelse ( any ( !identical ( e1@descriptives , data.frame() ) , !identical ( e2@descriptives , data.frame() ) ) , TRUE , FALSE )
					genLink <- ifelse ( any ( !identical ( e1@link , data.frame() ) , !identical ( e2@link , data.frame() ) ) , TRUE , FALSE )
					genVarCovMatrix <- ifelse ( any ( !identical ( e1@varCovMatrix , matrix()[FALSE,FALSE] ) , !identical ( e2@varCovMatrix , matrix()[FALSE,FALSE] ) ) , TRUE , FALSE )
					
					d1 <- e1@definition
					d2 <- e2@definition
					intsec <- intersect ( colnames ( d1 ) , colnames ( d2 ) )
					# alle Elemente
					els <- unique ( c ( colnames ( d1 ) , colnames ( d2 ) ) )											
					
					if ( all ( colnames ( d1 ) %in% intsec ) ) {
									
							# d2 so ordnen wie d1
							d2 <- d2[ , colnames(d1) , drop = FALSE ]
							d <- rbind ( d1 , d2 )
							
							# Duplikate sind jetzt die Elemente, die entfernt werden m�ssen
							dupl <- d[ duplicated ( d ) , ]
					
							if ( nrow ( dupl ) == 0 ) {
									d <- d1
							} else {
									# Duplikate entfernen, sollte hier eigentlich keine geben, nur zur Sicherheit
									dupl <- dupl [ ! duplicated ( dupl ) , , drop = FALSE ]
									
									# Liste aller Duplikate
									dupl.list <- mapply ( function ( i , d ) sapply(d[i,],c) , 1:nrow(dupl) , MoreArgs = list ( dupl ) , SIMPLIFY = FALSE )
								
									detect <- function ( v , d ) {
											eval ( parse ( text = paste ( paste ( "d$\"",names(v),"\"==\"",v,"\"", sep = "" ) , collapse = " & " ) ) )
									}
									del <- mapply ( detect , dupl.list , MoreArgs = list ( d ) , SIMPLIFY = FALSE )
									del2 <- do.call ( "|" , del )
									
									# delete Duplikate
									d <- d[ !del2 , , drop = FALSE ]
							}
				
					} else {
							d <- d1
					}
					if ( nrow ( d ) > 0 ) {
							rownames ( d ) <- seq ( along = rownames ( d ) )
					} else {
							d <- data.frame()
					}
					
					# neues Objekt setzen
					n <- define.design ( dsgn = n , def = d , append = FALSE , genStructure = genStructure , genDescriptives = genDescriptives , genLink = genLink , genVarCovMatrix = genVarCovMatrix , verbose = FALSE )
					
					return ( n )
			}
)

# show
setMethod ( f = "show" , signature = signature ( object="design" ) ,
			definition = function ( object ) {

					# Definitionen
					einr <- "     "
					
					### Design
					if ( identical ( object@definition , data.frame() ) ) {
							msg <- "Design is empty\n"
					} else {
							msg <- paste (
									"Design contains:\n\n" ,
									paste ( paste ( einr , object@nunits , " " , names ( object@nunits ) , sep = "" ) , collapse = "\n" ) ,
									
									"\n" , sep = "" )
					}
					
					### Structure
					if ( ! identical ( object@structure , data.frame() ) ) {
							msga <- "Design structure:\n\n"

							d <- dfr2long ( object@structure , lower = FALSE , upper = TRUE , diag = FALSE , use.names = TRUE )
							
							genString <- function ( ro , co , va , einr ) {
									
									# R�ckgabevariable
									st <- as.character(NA)

									if ( grepl ( "crossed" , va ) ) {
											st <- paste ( einr , ro , " and " , co , " are " , sub ( "crossed" , "" , va ) , " crossed" , sep = "" )
									}
									if ( va == "nestor" ) {
											st <- paste ( einr , co , " are nested within " , ro , sep = "" )
									}
									if ( va == "nested" ) {
											st <- paste ( einr , ro , " are nested within " , co , sep = "" )
									}
									if ( va == "unconnected" ) {
											st <- paste ( einr , ro , " and " , co , " are unconnected" , sep = "" )
									}
									if ( va == "equivalent" ) {
											st <- paste ( einr , ro , " and " , co , " are equivalent" , sep = "" )
									}									
									if ( is.na ( va ) ) {
											st <- as.character(NA)
									}
									
									return ( st )
							
							}
						
							msgb <- mapply ( genString , d$row , d$col , d$value , MoreArgs = list ( einr ) , SIMPLIFY = TRUE , USE.NAMES = FALSE )
							msgb <- msgb[!is.na(msgb)]
							msgb <- paste ( paste ( msgb , collapse = "\n" ) , "\n" , sep = "" )
							msga <- paste ( msga , msgb , sep = "" )
					
					} else {
							msga <- NULL
					}
					if ( !is.null ( msga ) ) msg <- paste ( msg , msga , sep = "\n" )
					
					### Descriptives
					if ( ! identical ( ( d2 <- object@descriptives ) , data.frame() ) ) {

							# kein Output f�r "nestor", "unconnected", "equivalent", da nicht so interessant
							d2 <- d2[ ! d2$structure %in% c("nestor", "unconnected", "equivalent") , , drop = FALSE ]
							
							if ( nrow ( d2 ) > 0 ) {
									
									msgc <- "Descriptives:\n\n"
									
									genString2 <- function ( ro , co , min , max , mean , sd , median , nlstr , einr ) {
								
											# R�ckgabevariable
											st <- as.character(NA)
										
											st <- paste ( einr , ro , " per " , co , ":  " , nlstr , sep = "" )
											if ( sd == 0 || is.na ( sd ) ) {
													st <- paste ( st , mean , sep = "" )
											} else {
													st <- paste ( st , min , " - " , max , "   M = " , formatC( mean , format = "f", digits = 2 ) , "  Mdn = " , formatC( median , format = "f", digits = 2 ) , "  SD = " , formatC( sd , format = "f", digits = 2 ) , sep = "" )
											}
											
											return ( st )
									
									}
									# zum ausrichten leerzeichen string
									nl <- max ( nchar ( paste ( d2$element1 , d2$element2 , sep = "" ) ) )
									er <- nl - nchar ( paste ( d2$element1 , d2$element2 , sep = "" ) )
									nlstr <- sapply ( er , function ( er ) paste ( rep ( " " , er ) , collapse = "" ) )
									
									msgd <- mapply ( genString2 , d2$element1 , d2$element2 , d2$min , d2$max , d2$mean , d2$sd , d2$median , nlstr , MoreArgs = list ( einr ) , SIMPLIFY = TRUE , USE.NAMES = FALSE )
									msgd <- msgd[!is.na(msgd)]
									msgd <- paste ( paste ( msgd , collapse = "\n" ) , "\n" , sep = "" )
									msgc <- paste ( msgc , msgd , sep = "" )
									
							} else msgc <- NULL
					} else {
							msgc <- NULL
					}
					if ( !is.null ( msgc ) ) msg <- paste ( msg , msgc , sep = "\n" )

					### Link
					if ( ! identical ( ( d3 <- object@link ) , data.frame() ) ) {

							# kein Output f�r "nestor", "unconnected", "equivalent", da nicht so interessant
							d3 <- d3[ ! d3$structure %in% c("nestor", "unconnected", "equivalent") , , drop = FALSE ]
							
							if ( nrow ( d3 ) > 0 ) {
									
									msge <- "Link Descriptives:\n\n"

									# auf Stelligkeit bringen
									makeArity <- function ( r ) {

											linklength <- formatC( r["linklength"] , format = "f", digits = 2 )
											linkrate1 <- formatC( r["linkrate1"] , format = "f", digits = 2 )
											linkrate2 <- formatC( r["linkrate2"] , format = "f", digits = 2 )
											if ( r["linkdispersion"] == 0 ) dig <- 0 else dig <- 2
											linkstrength <- formatC( r["linkstrength"] , format = "f", digits = dig )
											linkdispersion <- formatC( r["linkdispersion"] , format = "f", digits = dig )
											
											d <- data.frame ( linklength , linkrate1 , linkrate2 , linkstrength , linkdispersion , stringsAsFactors = FALSE )
											return ( d )
									}

									d3b <- d3[,4:ncol(d3)]
									linkf <- apply ( d3b , 1 , makeArity )
									linkf <- do.call ( rbind , linkf )									
								
									# Rownames
									na <- paste ( einr , d3$element1 , " linked by " , d3$element2 , "" , sep = "" )
									rownames ( linkf ) <- na 

									msge <- paste ( msge , dfr2text ( linkf ) , sep = "" )
									
							} else msge <- NULL
					} else {
							msge <- NULL
					}
					if ( !is.null ( msge ) ) msg <- paste ( msg , msge , sep = "\n" )					
					
					### varCovMatrix
					if ( ! identical ( ( d4 <- object@varCovMatrix ) , matrix()[FALSE,FALSE] ) ) {
									
							msgf <- "Variance-Covariance Matrix:\n\n"
							
							d4 <- data.frame ( d4 )
							
							# alles auf 2 Stellen
							do <- paste ( "d4$\"" , colnames ( d4 ) , "\"<-" , "formatC(d4$\"" , colnames ( d4 ) , "\", format = \"f\", digits = 2)" , sep = "" )
							eval ( parse ( text = do ) )
							
							# Rownames einr�cken
							rownames ( d4 ) <- paste ( einr , rownames ( d4 ) , sep = "" )
							
							msgf <- paste ( msgf , dfr2text ( d4 ) , sep = "" )

					} else {
							msgf <- NULL
					}
					if ( !is.null ( msgf ) ) msg <- paste ( msg , msgf , sep = "\n" )					
					
					### Design Descriptives
					if ( ! identical ( ( d5 <- object@designDescriptives ) , list() ) ) {
							
							msgg <- "Design Descriptives:\n"
						
							# alles auf 2 Stellen
							# do <- paste ( "if ( is.numeric (d5$" , colnames ( d5 ) , ") ) d5$" , colnames ( d5 ) , "<-" , "formatC(d5$" , colnames ( d5 ) , ", format = \"f\", digits = 2) else d5$" , colnames ( d5 ) , "<- \"NA\"" , sep = "" )
							# eval ( parse ( text = do ) )
		
							if ( is.na ( d5$Doptimality ) ) {
									d5$Doptimality <- "not computable"
							} else {
									d5$Doptimality <- formatC ( d5$Doptimality, format = "f", digits = 2 )
							}
							names ( d5 ) [ names ( d5 ) == "Doptimality" ] <- "D-optimality index"
							
							### Achtung: der Einfachheit halber nen Data.frame bauen
							# funktioniert nur wenn L�nge der Listenelemente jeweils 1
							val <- paste ( paste ( "\"" , sapply ( d5 , "[" , 1 ) , "\"" , sep = "" ) , collapse = " , " )
							do <- paste ( "dfr5 <- data.frame ( " , val , " , stringsAsFactors = FALSE )" )
							eval ( parse ( text = do ) )
							colnames ( dfr5 ) <- rep ( "" , length ( val ) )
							rownames ( dfr5 ) <- paste ( einr , names ( d5 ) , ": " , sep = "" )
							
							msgg <- paste ( msgg , dfr2text ( dfr5 , blankRowNames = FALSE ) , sep = "" )

					} else {
							msgg <- NULL
					}
					if ( !is.null ( msgg ) ) msg <- paste ( msg , msgg , sep = "\n" )						
					
				
					### raushauen
					cat ( msg )
			}
)

catmsg <- function ( slotname , empty , old , new ) {
		isnew <- !identical ( old , new )
		if ( isnew ) {
				if ( !empty ) {
						what <- "is updated to"
				} else {
						what <- "is set to"
				}
		} else {
				what <- "remains unchanged.\n     It's current value is"
		}
	
		val <- NULL
		# integer
		if ( inherits ( new , "integer" ) ) {
				if ( is.null ( names ( new ) ) ) {
						nam <- "n"
				} else {
						nam <- " named"
				}
				val <- ifelse ( identical ( new , integer(0) ) , "integer(0)" , paste ( "a" , nam , " integer vector with " , length ( new ) , " elements", sep = "" ) )
		}
		# character
		if ( inherits ( new , "character" ) ) {
				if ( is.null ( names ( new ) ) ) {
						nam <- ""
				} else {
						nam <- "named"
				}
				val <- ifelse ( identical ( new , character(0) ) , "character(0)" , paste ( "a " , nam , " character vector with " , length ( new ) , " elements", sep = "" ) )
		}
		# list
		if ( inherits ( new , "list" ) ) {
				if ( is.null ( names ( new ) ) ) {
						nam <- ""
				} else {
						nam <- "named"
				}
				val <- ifelse ( identical ( new , list() ) , "list()" , paste ( "a " , nam , " list with " , length ( new ) , " elements", sep = "" ) )
		}
		# data frame
		if ( inherits ( new , "data.frame" ) ) {
				val <- ifelse ( identical ( new , data.frame() ) , "data.frame()" , paste ( "a data frame with " , ncol ( new ) , " columns and " , nrow ( new ) , " rows" , sep = "" ) )
		}		
		# matrix
		if ( inherits ( new , "matrix" ) ) {
				val <- ifelse ( identical ( new , matrix()[FALSE,FALSE] ) , "matrix()[FALSE,FALSE]" , paste ( "a matrix with " , ncol ( new ) , " columns and " , nrow ( new ) , " rows" , sep = "" ) )
		}				
	
		if ( !is.null ( val ) ) {
				string <- paste ( "Slot @" , slotname , " " , what , " " , val , ".\n" , sep = "" )
				cat ( string )
		}
		
}