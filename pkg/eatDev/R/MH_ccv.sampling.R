
# conditional cumulative variance sampling
# data.long: person [,1] item [,2] value [,3]
# optional: th [,4], auf dieser Variable wird versucht Gleichverteilung (bzgl. Anzahl gezogener Personen) herzustellen
# col4.n: n auf das conditional upgesampled wird, wenn skalar dann für alle th gleich, oder Vektor um für jedes th zu setzen
#         bitte nur reale th (nicht die als factor/aber 0 noch drin sind), oder am besten namen, dann ist egal
ccv.sampling <- function ( data.long , col4.n = NULL , check = TRUE , verbose = FALSE ) {
		
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
		assign( "sel" , NULL , envir = sel.env )
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
				if ( verbose ) {
						msg9 <- paste0 ( "col4.n vector:" )
						cat ( paste0 ( "" , msg9 , "\n" ) )
						print ( col4.n.vec )
						flush.console()
				}					
		} else {
				col4.n <- NULL
		}
		if ( cond & !is.null ( col4.n ) ) {
				assign( "col4.n.seeds" , NULL , envir = sel.env )
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

		# long data noch item splitten
		d2 <- split ( d , f = list ( d[,2] ) ) 
		
		if ( verbose ) {
				msg5 <- paste0 ( "Checking variance on items, if no variance, one person is sampled to ensure variance" )
				cat ( paste0 ( "" , msg5 , "\n" ) )
				flush.console()
		}
		
		# über item Datensätze schleifen
		loop.items <- function ( d , sel.env ) {
		
				# aktuelles Item, für verbose Ausgaben
				item.cur <- as.character ( unique(d[,2]) )
			
				# bereits selektierte Personen
				already.sel <- get ( "sel" , envir = sel.env )
				
				# Datensatz mit nur bereits selektierte Personen
				if ( ! is.null ( already.sel ) ) {
						d2 <- d[ d[,1] %in% already.sel , , drop = FALSE ]
						vals.unique <- unique ( d2[!is.na(d2[,3]),3] )
						vals.unique <- vals.unique [ !is.na ( vals.unique ) ]
				} else {
						d2 <- NULL
						vals.unique <- NULL
				}
				
				# gibt es bereits Varianz?
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
						if ( ! is.null ( already.sel ) ) {
								d3 <- d[ !d[,1] %in% already.sel , , drop = FALSE ]
						} else {
								d3 <- d
						}					
					
						# potentielle Personen ermitteln
						if ( ! cond ) {
								
								# wenn vals.unique NULL ist, dann egal (aus allen Personen, unabhängig vom value, ziehen)
								# wenn nicht nur andere Values samplen
								if ( is.null ( vals.unique ) ) {
										potential.persons <- as.character ( d3[,1] )
								} else {
										potential.persons <- as.character ( d3[!d3[,3] %in% vals.unique , 1 ] )
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
								cond.distr.cur <- sample ( cond.distr.cur , length ( cond.distr.cur ) )
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
										if ( is.null ( vals.unique ) ) {
												potential.persons <- as.character ( d4[,1] )
										} else {
												potential.persons <- as.character ( d4[!d4[,3] %in% vals.unique , 1 ] )
										}
										
										if ( ! identical ( potential.persons , character(0) ) ) {
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
										cond.distr.cur.new[names(cond.distr.cur.sorted[i])] <- cond.distr.cur.new[names(cond.distr.cur.sorted[i])] + 1
										assign( "cond.distr" , cond.distr.cur.new , envir = sel.env )
								}
						}
						
						if ( length ( potential.persons ) > 0 ) {
								
								# neue Person
								seed2 <- runif ( 1 , 1 , 2100000000 )
								set.seed ( seed2 )
								names ( seed2 ) <- item.cur
								new.person <- sample ( potential.persons , 1 )
								assign ( "sel.seeds" , c ( get ( "sel.seeds" , envir = sel.env ) , seed2 ) , envir = sel.env )
								
								# neue Person in Variable sel in sel.env hinzufügen
								assign( "sel" , c ( already.sel , new.person ) , envir = sel.env )								
						
								# Ausgabe
								msg3 <- paste0 ( "   " , item.cur , ": new person selected to ensure variance: " , new.person )
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
		temp <- mapply ( loop.items , d2 , MoreArgs = list ( sel.env ) , SIMPLIFY = FALSE )
		
		# Datensatz reduzieren
		persons.selected1 <- get ( "sel" , envir = sel.env )
		if ( !is.null ( persons.selected1 ) ) {
				d3 <- d[ d[,1] %in% persons.selected1 , , drop=FALSE ]
				# Ausgabe Anzahl Personen
				if ( verbose ) {
						msg12 <- paste0 ( "Number of persons sampled to ensure variance on all items: " , length ( persons.selected1 ) )
						cat ( paste0 ( "" , msg12 , "\n" ) )
						flush.console()
				}	
		} else {
				d3 <- NULL
		}
		
		# ggf. upsampling col4.n.vec wenn gewünscht
		if ( ! is.null ( col4.n ) ) {

				# Ausgabe
				# if ( verbose ) {
						# msg11 <- paste0 ( "Upsampling conditional on col4" )
						# cat ( paste0 ( "" , msg11 , "\n" ) )
				# }	
		
				# Datensatz ohne die bereits selektierten Personen (d.h. mit potentiellen Personen)
				if ( !is.null ( persons.selected1 ) ) {
						d4 <- d[ !d[,1] %in% persons.selected1 , , drop=FALSE ]
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
				rownames ( soll ) <- seq ( along = rownames ( soll ) )
				
				# check ob alle sample.size > 0
				soll.invalid <- soll[soll$sample.size<1,,drop=FALSE]
				soll.valid <- soll[soll$sample.size>=1,,drop=FALSE]
				
				# Ausgabe
				if ( nrow ( soll.invalid ) > 0 ) {
						if ( verbose ) {
								msg10 <- paste0 ( "There are col4 elements with negative sample.size (i.e. already too many persons sampled)" )
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

				# check
				if ( ! is.null ( persons.selected2 ) ) {
						if ( !identical ( intersect ( persons.selected1 , persons.selected2 ) , character(0) ) ) {
								stop ( "internal error: some upsampled persons are already selected." )
						}
				}
						
				# finaler Datensatz
				persons.selected <- c ( persons.selected1 , persons.selected2 )
				if ( !is.null ( persons.selected ) ) {
		
						d6 <- d[ d[,1] %in% persons.selected , , drop=FALSE ]
						# Ausgabe Anzahl Personen
						if ( verbose ) {
								msg13 <- paste0 ( "Number of persons upsampled: " , length ( persons.selected2 ) )
								cat ( paste0 ( "" , msg13 , "\n" ) )
								flush.console()
						}	
				} else {
						d6 <- NULL
				}				
				
		} else {
				d6 <- d3
				persons.selected <- persons.selected1
		}
		
		# Ausgabe Anzahl Personen
		if ( verbose ) {
				msg8 <- paste0 ( "Final number of persons: " , length ( persons.selected ) )
				cat ( paste0 ( "" , msg8 , "\n" ) )
				flush.console()
		}	
		
		# Check Item-Varianz
		if ( check ) {
				d7 <- split ( d3 , f = list ( d3[,2] ) ) 
				var.ok <- all ( sapply ( d7 , function ( d ) check.var ( d[,3] ) ) )
				if ( !var.ok ) {
						items <- unique ( as.character ( d[,2] ) )
						items.not.var.ok <- items[!var.ok]
						msg2 <- paste0 ( "Sampling failed (no variance) for item(s): " , paste ( items.not.var.ok , collapse = ", " ) )
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
		
		# kreiertes Environment zurücksetzen
		rm ( list = ls ( all=TRUE, envir = sel.env ), envir = sel.env )
		
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
