
# conditional cumulative variance sampling
# data.long: person [,1] item [,2] value [,3]
# optional: th [,4], auf dieser Variable wird versucht Gleichverteilung (bzgl. Anzahl gezogener Personen) herzustellen
# col4.n: n auf das conditional upgesampled wird, wenn skalar dann für alle th gleich, oder Vektor um für jedes th zu setzen
#         bitte nur reale th (nicht die als factor/aber 0 noch drin sind), oder am besten namen, dann ist egal
# include: character Vektor mit Personen, die inkludiert sind
ccv.sampling <- function ( data.long , col4.n = NULL , col4.n.max.adj = TRUE , NperItem = NULL , include = NULL , check = TRUE , verbose = FALSE ) {
		
		# umbenennen
		d <- data.long

		# cond (conditional on column 4) setzen
		if ( ncol ( d ) == 4 ) {
				cond <- TRUE
		} else {
				cond <- FALSE
		}

		# environment mit selektierten Personen
		sel.env <- new.env()
		
		# inkludieren von Personen
		if ( !is.null ( include ) ) {
				sel <- include
		} else {
				sel <- NULL
		}
		assign( "sel" , sel , envir = sel.env )
		assign( "sel.seeds" , NULL , envir = sel.env )
		
		# col4.n Handling
		if ( cond & !is.null ( col4.n ) ) {

				col4.els <- unique ( as.character ( d[,4] ) )
				col4.els <- col4.els [ !is.na ( col4.els ) ]
				
				if ( is.numeric ( col4.n ) ) {
						if ( length ( col4.n ) == 1 ) {
								col4.n.vec <- rep ( col4.n , length ( col4.els ) )
								names ( col4.n.vec ) <- col4.els
						} else {
								if ( !is.null ( names ( col4.n ) ) ) {
										col4.n.vec <- col4.n
										col4.n.vec <- col4.n.vec[ names ( col4.n.vec ) %in% col4.els ]
								} else {
										col4.n.vec <- rep ( col4.n , length.out = length ( col4.els ) )
										names ( col4.n.vec ) <- col4.els
								}
						}
				} else {
						col4.n <- NULL
				}
		} else {
				col4.n <- NULL
		}
		if ( cond & !is.null ( col4.n ) ) {
				# seeds initialisieren
				assign( "col4.n.seeds" , NULL , envir = sel.env )
				
				# wenn bereits Personen vorselektiert sind (include),
				# dann die Anzahl, die noch gesampled werden soll, verringern
				if ( !is.null ( include ) ) {
						d.incl <- d [ d[,1] %in% include , c(1,4) ]
						d.incl2 <- d.incl [ !duplicated ( d.incl[,] ) , ]
						thtab <- table ( d.incl2[,2] )

						f1 <- function ( th , anz , thtab ) {
								anz - thtab[th]
						}
						x <- mapply ( f1 , names ( col4.n.vec ) , col4.n.vec , MoreArgs = list ( thtab ) , SIMPLIFY = FALSE )
						col4.n.vec <- do.call ( "c" , unname ( x ) )
						
				}

				if ( verbose ) {
						msg9 <- paste0 ( "col4.n vector:" )
						cat ( paste0 ( "" , msg9 , "\n" ) )
						print ( col4.n.vec )
						flush.console()
				}

				# wenn irgendwo negativ, dann noch anpassen
				if ( w <- any ( col4.n.vec < 0 ) ) {
						col4.n.vec[!w] <- 0
						
						if ( verbose ) {
								msg9b <- paste0 ( "col4.n vector (adjusted for negative values):" )
								cat ( paste0 ( "" , msg9b , "\n" ) )
								print ( col4.n.vec )
								flush.console()
						}
				}
		}
	
		# wenn conditional, dann die Verteilung anlegen, die dann upgedated wird
		if ( cond ) {
				cond.distr.tab <- table ( d[,4] )
				cond.distr <- as.integer ( cond.distr.tab )
				names ( cond.distr ) <- names ( cond.distr.tab )
				cond.distr <- cond.distr [ cond.distr > 0 ]
				cond.distr[] <- 0
				assign( "cond.distr" , cond.distr , envir = sel.env )
				assign( "cond.distr.seeds" , NULL , envir = sel.env )
		}

		# long data nach item splitten
		d2 <- split ( d , f = list ( d[,2] ) , drop = TRUE ) 
		
		if ( verbose ) {
				msg5 <- paste0 ( "Checking variance on items, if no variance, one person is sampled to ensure variance" )
				cat ( paste0 ( "" , msg5 , "\n" ) )
				flush.console()
		}

		# alle values
		vals.unique.all <- unique ( do.call ( "c" , sapply ( d2 , function ( d ) unique(d[,3]) , simplify = FALSE ) ) )
		vals.unique.all <- sort ( vals.unique.all [ !is.na ( vals.unique.all ) ] )
		
		# über Item-Datensätze schleifen
		loop.items <- function ( d , sel.env , vals.unique.all ) {

				# aktuelles Item, für verbose Ausgaben
				item.cur <- as.character ( unique(d[,2]) )
				
				# bereits selektierte Personen
				already.sel <- get ( "sel" , envir = sel.env )
		
				# Datensatz mit nur bereits selektierten Personen
				if ( ! is.null ( already.sel ) & any ( d[,1] %in% already.sel ) ) {
						d2 <- d[ d[,1] %in% already.sel , , drop = FALSE ]
						vals.unique <- unique ( d2[!is.na(d2[,3]),3] )
						vals.unique <- vals.unique [ !is.na ( vals.unique ) ]
				} else {
						d2 <- NULL
						vals.unique <- NULL
				}
				
				# gibt es bereits Varianz?
				### falls man das ausweiten würde wollen auf polytom
				### und alle Kategorien haben will
				### muss man hier auf nicht auf Varianz sondern, auf alle Kategorien checken!!!
				if ( !is.null ( d2 ) ) {
						var.ok <- check.var ( vals.unique )
				} else {
						var.ok <- FALSE
				}
				
				# wenn es bereits Varianz gibt dann nix tun, ansonsten eine neue Person samplen
				if ( var.ok ) {
						# msg4 <- paste0 ( "   " , item.cur , ": already variance, no sampling necessary" )
						# if ( verbose ) {
								# cat ( paste0 ( "" , msg4 , "\n" ) )
								# flush.console()
						# }						
				} else {
				
						# Datensatz ohne bereits selektierte Personen (potentielle Personen)
						# wenn noch keine Selektion dann alle
						if ( ! is.null ( already.sel ) ) {
								d3 <- d[ !d[,1] %in% already.sel , , drop = FALSE ]
						} else {
								d3 <- d
						}					
					
						# potentielle Personen ermitteln
						if ( ! cond ) {
							
								# wenn vals.unique NULL ist, dann sind alle Personen potentiell relevant
								# wenn vals.unique NULL ist, dann je soll-value ( bei dichotom: 0, 1) eine Person aus allen Personen ziehen
								# dazu mehre Sets machen (liste)
								# wenn nicht nur andere Values samplen
								vals.to.sample <- vals.unique.all[!vals.unique.all %in% vals.unique]
								potential.persons <- sapply ( vals.to.sample , function ( x , d ) as.character ( d[d[,3] %in% x , 1 ] ) , d3 , simplify = FALSE )
								# leere Listenelemente raus
								potential.persons <- potential.persons [ sapply ( potential.persons , length ) > 0 ]
								if ( identical ( potential.persons , list() ) ) {
										potential.persons <- NULL
								}
								
						} else {
						
								# aktuelle cond.distribution
								cond.distr.cur.all <- get ( "cond.distr" , envir = sel.env )
								
								# Abgleich mit aktuellen TH
								cond.distr.cur <- cond.distr.cur.all [ names ( cond.distr.cur.all ) %in% unique ( as.character ( d3[,4] ) ) ]
							
								# randomly durcheinanderbringen, damit Verteilung möglichst gleich
								# ansonsten sind die TH, die weiter hinten stehen potentiell benachteiligt
								seed1 <- runif ( 1 , 1 , 2100000000 )
								set.seed ( seed1 )
								names ( seed1 ) <- item.cur
								cond.distr.cur <- cond.distr.cur [ sample ( names ( cond.distr.cur ) , length ( cond.distr.cur ) ) ]
								assign ( "cond.distr.seeds" , c ( get ( "cond.distr.seeds" , envir = sel.env ) , seed1 ) , envir = sel.env )
								
								# sortieren von klein nach groß
								cond.distr.cur.sorted <- cond.distr.cur [ order ( cond.distr.cur ) ]
								
								# jetzt so lange durchgehen bis mind. eine brauchbare Person dabei
								abbruch <- FALSE
								i <- 1
								i.max <- length ( cond.distr.cur.sorted )
								potential.persons <- character(0)
								while ( !abbruch ) {
										
										sel.rows <- d3[,4] == names ( cond.distr.cur.sorted[i] )
								
										# nur bestimmtes TH selektieren
										d4 <- d3[ d3[,4] == names ( cond.distr.cur.sorted[i] ) , , drop = FALSE ]
										
										# wenn vals.unique NULL ist, dann egal (aus allen Personen, unabhängig vom value, ziehen)
										# wenn nicht nur andere Values samplen
										# if ( is.null ( vals.unique ) ) {
												# potential.persons <- as.character ( d4[,1] )
										# } else {
												# potential.persons <- as.character ( d4[!d4[,3] %in% vals.unique , 1 ] )
										# }
										
										vals.to.sample <- vals.unique.all[!vals.unique.all %in% vals.unique]
										potential.persons <- sapply ( vals.to.sample , function ( x , d ) as.character ( d[d[,3] %in% x , 1 ] ) , d4 , simplify = FALSE )
										# leere Listenelemente raus
										potential.persons <- potential.persons [ sapply ( potential.persons , length ) > 0 ]
										if ( identical ( potential.persons , list() ) ) {
												potential.persons <- NULL
										}

										if ( length ( potential.persons ) > 0 ) {
												abbruch <- TRUE
										} else if ( i == i.max ) {
												abbruch <- TRUE
										} else {
												i <- i + 1
										}
								}
								
								# Verteilung updaten
								if ( length ( potential.persons ) > 0 ) {
										cond.distr.cur.new <- cond.distr.cur.all
										cond.distr.cur.new[names(cond.distr.cur.sorted[i])] <- cond.distr.cur.new[names(cond.distr.cur.sorted[i])] + length ( potential.persons )
										assign( "cond.distr" , cond.distr.cur.new , envir = sel.env )
								}
						}
						
						if ( length ( potential.persons ) > 0 ) {
							
								# eine neue (bzw. mehrere) Personen ziehen
								seed2 <- runif ( 1 , 1 , 2100000000 )
								set.seed ( seed2 )
								names ( seed2 ) <- item.cur
								new.person <- sapply ( potential.persons , sample , 1 )
								assign ( "sel.seeds" , c ( get ( "sel.seeds" , envir = sel.env ) , seed2 ) , envir = sel.env )
								
								# neue Person in Variable sel in sel.env hinzufügen
								assign ( "sel" , c ( already.sel , new.person ) , envir = sel.env )								
						
								# Ausgabe
								msg3 <- paste0 ( "   " , item.cur , ": new person(s) selected to ensure variance: " , paste ( new.person , collapse = ", " ) )
								if ( verbose ) {
										cat ( paste0 ( "" , msg3 , "\n" ) )
										flush.console()
								}
								
						} else {
								msg1 <- paste0 ( "   " , item.cur , ": sampling not possible, no potential persons" , "" )
								if ( verbose ) {
										cat ( paste0 (  msg1 , " (WARNING)" , "\n" ) )
										flush.console()
								} else {
										warning ( msg1 )
								}
						}
				}
				return ( TRUE )
		}
		temp <- mapply ( loop.items , d2 , MoreArgs = list ( sel.env , vals.unique.all ) , SIMPLIFY = FALSE )

		# Datensatz reduzieren
		# persons.selected1 <- get ( "sel" , envir = sel.env )
		# if ( !is.null ( persons.selected1 ) ) {
				# d3 <- d[ d[,1] %in% persons.selected1 , , drop=FALSE ]
				# Ausgabe Anzahl Personen
				# if ( verbose ) {
						# msg12 <- paste0 ( "Number of persons sampled to ensure variance on all items: " , length ( persons.selected1 ) )
						# cat ( paste0 ( "" , msg12 , "\n" ) )
						# flush.console()
				# }	
		# } else {
				# d3 <- NULL
		# }
		
		# ggf. upsampling col4.n.vec wenn gewünscht
		if ( ! is.null ( col4.n ) ) {
				
				# Ausgabe
				# if ( verbose ) {
						# msg11 <- paste0 ( "Upsampling conditional on col4" )
						# cat ( paste0 ( "" , msg11 , "\n" ) )
				# }	

				# bereits selektierte Personen
				already.sel <- get ( "sel" , envir = sel.env )
				
				# Datensatz ohne die bereits selektierten Personen (d.h. mit potentiellen Personen)
				if ( !is.null ( already.sel ) ) {
						d4 <- d[ !d[,1] %in% already.sel , , drop=FALSE ]
				} else {
						d4 <- d
				}
				
				# Splitten nach TH
				d5 <- split ( d4 , f = list ( d4[,4] ) , drop = TRUE )
				
				# Soll/Ist Datensatz
				col4.cur <- get ( "cond.distr" , envir = sel.env )
				soll <- data.frame ( "col4" = names ( col4.n.vec ) , "goal" = col4.n.vec , stringsAsFactors = FALSE )
				ist <- data.frame ( "col4" = names ( col4.cur ) , "current" = col4.cur , stringsAsFactors = FALSE )
				soll <- merge ( soll , ist , by = "col4" , sort = FALSE , all.x = TRUE , all.y = FALSE )
				soll$sample.size <- soll$goal - soll$current
				
				# maximal
				col4.tab.real <- table ( d[!duplicated(d[,c(1,4)]),4] )				
				col4.tab.dfr <- data.frame ( "col4" = names ( col4.tab.real ) , "max" = unname ( as.numeric ( col4.tab.real ) ) , stringsAsFactors = FALSE )
				soll <- merge ( soll , col4.tab.dfr , by = "col4" , sort = FALSE , all.x = TRUE , all.y = FALSE )
				rownames ( soll ) <- seq ( along = rownames ( soll ) )
			
				# sample.size auf maximale sample.size adjustieren
				if ( col4.n.max.adj ) {
						toadj <- soll$sample.size > soll$max
						if ( any ( toadj ) ) {
								soll$sample.size[ toadj ] <- soll$max[ toadj ]
								if ( verbose ) {
										msg15 <- paste0 ( "The desired sample size for some elements was too high. That's why it was adjusted to the maximal possible sample size" )
										cat ( paste0 ( "" , msg15 , "\n" ) )
										print ( soll[ toadj, ] )
								}
						}
				}
				
				# check ob alle sample.size > 0 und < max
				soll.invalid <- soll[soll$sample.size<1 | soll$sample.size>soll$max,,drop=FALSE]
				soll.valid <- soll[soll$sample.size>=1 & soll$sample.size<=soll$max,,drop=FALSE]
				
				# Ausgabe
				if ( nrow ( soll.invalid ) > 0 ) {
						if ( verbose ) {
								msg10 <- paste0 ( "There are col4 elements with non-positive (i.e., 0 or negative) sample.size" )
								cat ( paste0 ( "" , msg10 , "\n" ) )
								print ( soll.invalid )
								flush.console()
						} else {
								warning ( msg10 )
						}
				}
				
				# Upsampling
				if ( nrow ( soll.valid ) > 0 ) {
						sampl1 <- function ( sample.size , col4 , d ) {
								potential.persons <- unique ( as.character ( d[[col4]][,1] ) )
								
								seed3 <- runif ( 1 , 1 , 2100000000 )
								set.seed ( seed3 )
								names ( seed3 ) <- col4
								sel <- sample ( potential.persons , sample.size ) 
								assign ( "col4.n.seeds" , c ( get ( "col4.n.seeds" , envir = sel.env ) , seed3 ) , envir = sel.env )								
								
								return ( sel )
						}
						persons.selected2.l <- mapply ( sampl1 , soll.valid$sample.size , soll.valid$col4 , MoreArgs = list ( d5 ) , SIMPLIFY = FALSE )
						persons.selected2 <- do.call ( "c" , persons.selected2.l )
				} else {
						persons.selected2 <- NULL
				}
		} else {
				persons.selected2 <- NULL
		}
		
		# Ausgabe Anzahl Personen
		persons.selected1 <- get ( "sel" , envir = sel.env )		
		if ( verbose ) {
				msg12 <- paste0 ( "Number of persons sampled to ensure variance on all items: " , length ( persons.selected1 ) )
				cat ( paste0 ( "" , msg12 , "\n" ) )
				flush.console()
		}

		# Ausgabe Anzahl Personen
		if ( !is.null ( persons.selected2 ) ) {
				if ( verbose ) {
						msg13 <- paste0 ( "Number of persons upsampled: " , length ( persons.selected2 ) )
						cat ( paste0 ( "" , msg13 , "\n" ) )
						flush.console()
				}	
		}

		# alle Personen
		persons.selected <- c ( persons.selected1 , persons.selected2 )	
		# check
		if ( ! is.null ( persons.selected1 ) & ! is.null ( persons.selected2 ) ) {
				if ( !identical ( intersect ( persons.selected1 , persons.selected2 ) , character(0) ) ) {
						stop ( "internal error: some upsampled persons are already selected." )
				}
		}
		if ( !is.null ( persons.selected ) ) {
				if ( verbose ) {
						msg8 <- paste0 ( "Final number of persons: " , length ( persons.selected ) )
						cat ( paste0 ( "" , msg8 , "\n" ) )
						flush.console()
				}	
		}

		# Datensatz mit finalem Personensample
		d6a <- d[ d[,1] %in% persons.selected , , drop=FALSE ]

	
		### observations raussamplen um NperItem zu gewährleisten ###
		if ( !is.null ( NperItem ) ) {
				
				# Funktion NperItem
				get.nperitem <- function ( d ) {
						d2 <- d[ !duplicated(d[,]) , ]
						d3 <- split ( d2 , f=d2[,2] )
						sapply ( d3 , nrow )
				}
				# aktuelles N per Item
				nperitem.list <- get.nperitem ( d6a[,c(1,2)] )
				
				# Ausgabe
				cat ( paste0 ( "Before observation downsampling" , "\n" ) )
				cat ( paste0 ( "     N per item: M=" , formatC ( mean ( nperitem.list ) , format = "f" , digits = 2 ) , " SD=" , formatC ( sd ( nperitem.list )  , format = "f" , digits = 2 ) , " min=" , min ( nperitem.list ) , " max=" , max ( nperitem.list ) , "\n" ) )
				cat ( paste0 ( "           goal: " , NperItem , "\n" ) )
				
				# Soll/Ist Datensatz
				nperitem <- data.frame ( "item" = names ( nperitem.list ) , "current" = nperitem.list , stringsAsFactors = FALSE )
				nperitem$goal <- NperItem
				nperitem$sample.size <- nperitem$current - nperitem$goal

				# Environment für zu reduzierenden Datensatz d6
				d6.env <- new.env()
				assign( "d6" , d6a , envir = d6.env )
				assign( "NperItem.seeds" , NULL , envir = d6.env )
				
				# Verteilung von idstud
				# wieviele Observations gibt es je Person
				nperperson.tab <- table ( as.character ( d6a[ !duplicated ( d6a[,c(1,2)] ) , 1 ] ) )
				nperperson <- data.frame ( "idstud" = names ( nperperson.tab ) , "nperperson" = as.integer ( nperperson.tab ) , stringsAsFactors = FALSE )
				assign( "nperperson" , nperperson , envir = d6.env )
				
				# Ausgabe
				cat ( paste0 ( "   N per person: M=" , formatC ( mean ( nperperson$nperperson ) , format = "f" , digits = 2 ) , " SD=" , formatC ( sd ( nperperson$nperperson ) , format = "f" , digits = 2 ) , " min=" , min ( nperperson$nperperson ) , " max=" , max ( nperperson$nperperson ) , "\n" ) )
				flush.console()
				
				# Observations raussamplen
				nperitem.downsample <- function ( item , sample.size , d6.env ) {
						# cat ( paste0 ( item , " " ) )
						# cat ( paste0 ( "." ) )
						# flush.console()
						
						if ( sample.size > 0 ) {
								d <- get ( "d6" , envir = d6.env )
								npp <- get ( "nperperson" , envir = d6.env )
								
								# aktuelles Item
								d2 <- d[d[,2]==item , c(1,2)]
								# zur Sicherheit
								d3 <- d2[ !duplicated(d2[,]) , ]
								
								# die Personen die viele observations haben (max), haben große prob (1) rauszufliegen
								npp$prob <- npp$nperperson/max(npp$nperperson)
								# npp$prob[ npp$prob > 0.99 ] <- 0.99
								# bei unter 2 observations ist die Person nicht zum Deselektieren ziehbar, da diese sonst aus dem Datensatz fliegen würde
								npp$prob[ npp$nperperson < 2 ] <- 0
								
								# Seed
								seed4 <- runif ( 1 , 1 , 2100000000 )
								set.seed ( seed4 )
								names ( seed4 ) <- item
								assign ( "NperItem.seeds" , c ( get ( "NperItem.seeds" , envir = d6.env ) , seed4 ) , envir = d6.env )
						
								# samplen
								x <- as.character(d3[,1])
								probs <- npp$prob
								names ( probs ) <- npp$idstud
								probs <- unname ( probs[x] )
								todelete <- sample( x , sample.size , replace = FALSE, prob = probs )
							
								# Observations aus Datensatz werfen
								d4 <- d[ ! ( d[,1] %in% todelete & d[,2] == item ) , ]
								assign( "d6" , d4 , envir = d6.env )
								
								# Verteilung N per person anpassen
								npp[ npp[,1] %in% todelete, "nperperson" ] <- npp[ npp[,1] %in% todelete, "nperperson" ] - 1
								assign( "nperperson" , npp[,c(1,2)] , envir = d6.env )
						}
						
						return ( TRUE )
				}
				temp <- mapply ( nperitem.downsample , nperitem$item , nperitem$sample.size , MoreArgs = list ( d6.env ) , SIMPLIFY = FALSE )
				
				### Check nach downsampling ###
				
				# downgesampleter Datensatz
				d6 <- get ( "d6" , envir = d6.env )
				
				# aktuelles N per Item
				nperitem.list <- get.nperitem ( d6[,c(1,2)] )
				
				# Ausgabe
				cat ( paste0 ( "After observation downsampling" , "\n" ) )
				cat ( paste0 ( "     N per item: M=" , formatC ( mean ( nperitem.list ) , format = "f" , digits = 2 ) , " SD=" , formatC ( sd ( nperitem.list )  , format = "f" , digits = 2 ) , " min=" , min ( nperitem.list ) , " max=" , max ( nperitem.list ) , "\n" ) )
				
				# Verteilung von idstud
				# wieviele Observations gibt es je Person
				nperperson.tab <- table ( as.character ( d6[ !duplicated ( d6[,c(1,2)] ) , 1 ] ) )
				nperperson <- data.frame ( "idstud" = names ( nperperson.tab ) , "nperperson" = as.integer ( nperperson.tab ) , stringsAsFactors = FALSE )
				
				# Ausgabe
				cat ( paste0 ( "   N per person: M=" , formatC ( mean ( nperperson$nperperson ) , format = "f" , digits = 2 ) , " SD=" , formatC ( sd ( nperperson$nperperson ) , format = "f" , digits = 2 ) , " min=" , min ( nperperson$nperperson ) , " max=" , max ( nperperson$nperperson ) , "\n" ) )
				
		} else {
				d6 <- d6a
		}
		
		# Check Item-Varianz
		if ( check ) {
				d7 <- split ( d6 , f = list ( d6[,2] ) , drop = TRUE ) 
				var.ok.items <- sapply ( d7 , function ( d ) check.var ( d[,3] ) )
				var.ok <- all ( var.ok.items )

				if ( !var.ok ) {
						items.not.var.ok <- names(var.ok.items[!var.ok.items])
						msg2 <- paste0 ( "Sampling failed (no variance) for " , length ( items.not.var.ok ) , " item(s): " , paste ( items.not.var.ok , collapse = ", " ) )
						if ( verbose ) {
								cat ( paste0 ( "WARNING: " , msg2 , "\n" ) )
						} else {
								warning ( msg2 )
						}
				} else {
						msg6 <- paste0 ( "Sampling to ensure variance successfully done. All items have variance." )
						if ( verbose ) {
								cat ( paste0 ( "" , msg6 , "\n" ) )
						}
				}
		} else {
				var.ok <- NA
				msg7 <- paste0 ( "Sampling done without final check if all items have variance. All items should have variance." )
				if ( verbose ) {
						cat ( paste0 ( "" , msg7 , "\n" ) )
				}
		}
		flush.console()		
		
		# Check TH Verteilung
		if ( check & !is.null( col4.n ) ) {
				col4.tab <- table ( d6[!duplicated(d6[,c(1,4)]),4] )
				sum ( col4.tab )

				col4.tab.real <- col4.tab[ names(col4.tab) %in% names(col4.n.vec) ]
				col4.tab.real.sorted <- col4.tab.real [ names(col4.n.vec) ]
				col4.distr.ok <- col4.tab.real == col4.tab.real.sorted
				col4.distr.ok.all <- all ( col4.distr.ok )
				
				if ( ! col4.distr.ok.all ) {
						col4.els.not.col4.distr.ok <- col4.els[!col4.distr.ok]
						msg14 <- paste0 ( "Upsampling failed for col4 element(s): " , paste ( col4.els.not.col4.distr.ok , collapse = ", " ) )
						if ( verbose ) {
								cat ( paste0 ( "WARNING: " , msg14 , "\n" ) )
						} else {
								warning ( msg14 )
						}
				} else {
						msg6 <- paste0 ( "Upsampling to desired col4.n successfully done." )
						if ( verbose ) {
								cat ( paste0 ( "" , msg6 , "\n" ) )
						}		
				}
		} else if ( !is.null( col4.n ) ) {
				col4.distr.ok <- NA
				msg7 <- paste0 ( "Sampling done without final check if desired col4.n has been reached." )
				if ( verbose ) {
						cat ( paste0 ( "" , msg7 , "\n" ) )
				}
		}		
		flush.console()
		
		# Return-Objekt
		ret <- list ( "data.long.sampled" = d6 , "persons.selected" = persons.selected , "seeds1" = get ( "sel.seeds" , envir = sel.env ) )
		if ( check ) ret <- c ( ret , list ( "check.var" = var.ok ) )
		if ( cond ) ret <- c ( ret , list ( "col4.distribution" = get ( "cond.distr" , envir = sel.env ) , "seeds2" = get ( "cond.distr.seeds" , envir = sel.env ) ) )
		if ( !is.null(col4.n) ) ret <- c ( ret , list ( "col4.distribution.upsampled" = col4.tab.real.sorted , "col4.upsampling.plan" = soll , "seeds3" = get ( "col4.n.seeds" , envir = sel.env ) ) )
		if ( !is.null(col4.n) & check ) ret <- c ( ret , list ( "check.col4.n" = col4.distr.ok.all ) )
		if ( !is.null(NperItem) ) ret <- c ( ret , list ( "NperItem.seeds" = get ( "NperItem.seeds" , envir = d6.env ) ) )
		
		# kreiertes Environment zurücksetzen
		rm ( list = ls ( all=TRUE, envir = sel.env ), envir = sel.env )
		if ( exists ( "d6.env" ) ) rm ( list = ls ( all=TRUE, envir = d6.env ), envir = d6.env )
		
		# Rückgabe
		return ( ret )
		
}

check.var <- function ( vals ) {
		if ( any ( duplicated ( vals ) ) ) vals <- unique ( vals )
		vals <- vals[!is.na(vals)]
		var.val <- var ( vals )
		if ( !is.na ( var.val ) ) {
				if ( is.numeric ( var.val ) ) {
						var.ok <- var.val > 0
						if ( !is.logical ( var.ok ) | identical ( var.ok , logical(0) ) ) var.ok <- FALSE
				} else {
						var.ok <- FALSE
				}
		} else {
				var.ok <- FALSE
		}
		return ( var.ok )
}				 
