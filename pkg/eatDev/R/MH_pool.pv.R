
# pv ... Namen oder Indizes der PV-Variablen im Datensatz; oder regulärer Ausdruck

pool.pv <- function ( d , caseid = NULL , pv = NULL , mode = c ( "cont" , "cat" , "icc" , "glm" ) , split = NULL , pv.split = NULL , new.split.name = "scale" , new.split.sub = NULL , cast.for.total = NULL , split.vars.overall = NULL , jk2.mean.group = NULL , jk2.glm.independent = NULL , jk2.glm.family = NULL , effects = seq ( along = c ( split , pv.split ) ) , quants = c ( 0.05 , 0.10 , 0.25 , 0.75 , 0.90 , 0.95 ) , weight = NULL , jkzone = NULL , jkrep = NULL , clusterid = NULL , include.ALL = TRUE , folder.temp = NULL , overwrite.folder.temp = FALSE , verbose = FALSE ) {
		
		# Return-Objekt
		ret <- NULL
		
		# Packages
		require ( plotrix )
		require ( Hmisc )
		require ( eatRep )
		require ( reshape2 )
		
		# temporärer folder für Sicherheitskopien
		if ( !is.null ( folder.temp ) ) {
				if ( overwrite.folder.temp ) {
						if ( file.exists ( folder.temp ) ) {
								temp <- try ( unlink ( x = folder.temp , recursive = TRUE, force = TRUE ) , silent = TRUE )
								Sys.sleep ( 11 )
						}
				}
				if ( !file.exists ( folder.temp ) ) try ( dir.create ( path = folder.temp, showWarnings = FALSE, recursive = TRUE ) , silent = TRUE )
		}

		# jk2.mean.group.cols
		if ( ! is.null ( jk2.mean.group ) ) {
				jk2.mean.group.cols <- colnames ( d )[ grepl ( jk2.mean.group , colnames ( d ) ) ]
		} else {
				jk2.mean.group.cols <- NULL
		}
	
		# jk2.glm.independent.cols
		if ( ! is.null ( jk2.glm.independent ) ) {
				jk2.glm.independent.cols <- colnames ( d )[ grepl ( paste ( jk2.glm.independent , collapse = "|" ) , colnames ( d ) ) ]
		} else {
				jk2.glm.independent.cols <- NULL
		}
	
		# Factors auf character
		# wenn nicht in jk2.glm.independent
		do <- paste0 ( "if ( is.factor ( d$" , colnames ( d ) , ") & ! '" , colnames ( d ) , "' %in% jk2.glm.independent.cols ) d$" , colnames ( d ) , " <- as.character ( d$" , colnames ( d ) , ")" )
		eval ( parse ( text = do ) )
		
		# Check weight
		if ( !is.null ( weight ) ) {
				if ( ! weight %in% colnames ( d ) ) {
						cat ( paste0 ( "'" , weight , "' is not a valid column name in data set" ) )
						weight <- NULL
				}
		}

		# Check caseid
		if ( is.character ( caseid ) ) {
				if ( !caseid %in% colnames ( d ) ) caseid <- NULL
		} else {
				caseid <- NULL
		}
		if ( is.null ( caseid ) ) {
				d <- cbind ( data.frame ( "caseid" = seq ( along = rownames ( d ) ) , stringsAsFactors = FALSE ) , d )
				caseid <- "caseid"
		}

		# Check clusterid
		if ( is.character ( clusterid ) ) {
				clusterid <- clusterid[ clusterid %in% colnames ( d ) ]
				if ( length ( clusterid ) == 0 ) clusterid <- NULL
		} else {
				clusterid <- NULL
		}
		
		# Check quants (Quantile)
		if ( is.numeric ( quants ) ) {
				quants <- quants[ quants > 0 & quants < 1 ]
				if ( length ( quants ) < 1 ) {
						quants <- NULL				
				}
		} else {
				quants <- NULL
		}
		
		# Check mode
		if ( is.character ( mode ) ) {
				if ( length ( mode ) > 1 ) {
						mode <- mode[1]
				}
				if ( ! mode %in% c ( "cont" , "cat" , "icc" , "glm" ) ) {
						mode <- "cont"
				}
		} else {
				mode <- "cont"
		}
		
		# Check interactions
		### TODO ###

		
		# Bestimmen der PV Spalten
		if ( is.character ( pv ) ) {
				if ( any ( pv %in% colnames ( d ) ) ) {
						pv.vars <- pv [ pv %in% colnames(d) & !pv %in% split ]
				} else {
						pv.vars <- colnames ( d ) [ grepl ( pv , colnames ( d ) ) ]
				}
		} else if ( is.numeric ( pv ) ) {
				pv.vars <- colnames ( d )[ pv [ pv %in% seq ( along = colnames ( d ) ) ] ]
		} else {
				pv.vars <- colnames ( d )[ ! colnames ( d ) %in% c ( caseid , split , weight , jkzone , jkrep , jk2.mean.group.cols , jk2.glm.independent.cols ) ]
		}
		if ( verbose ) cat ( paste0 ( "pv variables: " , paste ( pv.vars , collapse = ", " ) ) , "\n" )

		# split Variablen
		split.vars <- NULL
		if ( !is.null ( split ) ) {
				if ( is.character ( split ) ) {
						split <- split [ split %in% colnames ( d ) ]
						if ( ! identical ( split , character(0) ) ) split.vars <- split
				}
		}


		
		# nur relevantes behalten
		d <- d[ , c ( caseid , pv.vars , split.vars , weight , jkzone , jkrep , clusterid , jk2.mean.group.cols , jk2.glm.independent.cols ) ]

		# ggf. splitten der Variablen
		if ( is.character ( pv.split ) ) {
				if ( any ( colnames ( d ) %in% c ( "time" , "id" ) ) ) {
						msg <- "pv.splitting not possible, because data set contains column 'time' and/or 'id'\nPlease rename."
						if ( verbose ) {
								cat ( paste0 ( "Warning: " , msg ) )
						} else {
								warning ( msg )
						}
						d2 <- d
				} else {
						d2 <- reshape ( data = d ,
										direction = "long" , 
										varying = pv.vars ,
										split = list ( "regexp" = pv.split , "include" = TRUE , "fixed" = FALSE ) 
									 )
						attr ( d2 , "reshapeLong" ) <- NULL
						colnames ( d2 )[ colnames ( d2 ) == "time" ] <- new.split.name
						d2$id <- NULL
						
						split.vars <- c ( split.vars , new.split.name )
						pv.vars <- colnames ( d2 )[ ! colnames ( d2 ) %in% c ( caseid , split.vars , weight , jkzone , jkrep , clusterid , jk2.mean.group.cols , jk2.glm.independent.cols ) ]

						# ersetzen new.split.sub mit "" in Kategorien
						if ( !is.null ( new.split.sub ) ) {
								d2[,new.split.name] <- sub ( new.split.sub , "" , d2[,new.split.name] )
						}
						
						if ( verbose ) {
								cat ( paste0 ( "column names are splitted by '" , new.split.name , "' with values " , paste ( unique ( d2[,new.split.name] ) , collapse = ", " ) , "\n" ) )
								# Ausgabe Kategorien auf new.split.name
								# cat ( paste0 ( "Categories on '" , new.split.name , "': " , paste ( unique ( d2[,new.split.name] ) , collapse = ", " ) , "\n" ) )
								# Ausgabe der neuen pv Variablen
								cat ( paste0 ( "new pv variables: " , paste ( pv.vars , collapse = ", " ) ) , "\n" )		
						}
						
				}
		} else {
				d2 <- d
		}
		
		# vorhandene Stufen auf pv.vars für mode="cat"
		if ( mode %in% "cat" ) {
				stufen <- unique ( do.call ( "c" , sapply ( d2[,pv.vars,drop=FALSE] , function ( s ) sort ( unique ( as.character ( s ) ) ) , simplify = FALSE ) ) )
		} else {
				stufen <- NULL
		}
		
		# Check cast.for.total
		if ( is.character ( cast.for.total ) ) {
				if ( !cast.for.total %in% colnames ( d2 ) ) cast.for.total <- NULL
		} else {
				cast.for.total <- NULL
		}
		if ( length ( cast.for.total ) > 1 ) {
				cast.for.total <- cast.for.total[1]
				msg <- "'cast.for.total' is a character vector of length > 1. multiple 'cast.for.total' variables are not supported. first element ist used.\n"
				if ( verbose ) {
						cat ( msg )
				} else {
						warning ( msg )
				}
		}
		
		# Sortierung auf Split-vars aufheben um später zu setzen
		if ( length ( split.vars ) > 0 ) {
				split.vars.sort.list <- sapply ( d2[,split.vars,drop=FALSE] , function ( sp ) as.character ( c ( NA , unique ( as.character ( sp ) ) ) ) , simplify = FALSE )
		} else {
				split.vars.sort.list <- NULL
		}
	
		# Check split.vars.overall / anpassen split.vars.sort.list
		if ( !is.null ( split.vars.overall ) ) {
				w <- names ( split.vars.overall ) %in% c ( split.vars , new.split.name )
				if ( all ( !w ) ) {
						split.vars.overall <- NULL
				} else {
						split.vars.overall <- split.vars.overall[w]
						do <- paste0 ( "split.vars.sort.list$" , names ( split.vars.overall ) , "[is.na(split.vars.sort.list$" , names ( split.vars.overall ) , ")]<- '" , split.vars.overall , "'" )
						eval ( parse ( text = do ) )
				}
		}	

		# effects
		if ( is.numeric ( effects ) ) {
				if ( length ( effects ) > 0 ) {
						interactions <- max ( seq ( along = split.vars ) )
						effects <- effects [ effects >= 0 & effects <= interactions ]
				} else {
						interactions <- NULL
						effects <- NULL
				}
		} else {
				interactions <- NULL
				effects <- NULL
		}			
	
		# loslegen
		if ( length ( pv.vars ) > 0 ) {
		
				### erstellen aller "Interaktionen" ###
				if ( length ( split.vars ) > 0 ) {
						# Interaktionsvektor
						v <- seq ( along = split.vars )
						# kürzen bzgl. Parameter "interaction"
						v <- v [ v <= interactions ]
						
						f <- function ( nn , vars ) {
								combn ( vars , nn , simplify = FALSE )
						}
						x <- mapply ( f , v , MoreArgs = list ( split.vars ) , SIMPLIFY = FALSE )
						x <- do.call ( "c" , x )
						# sortieren nach Länge
						lx <- sapply ( x , length )
						names ( lx ) <- seq ( along = lx )
						lx <- lx [ order ( lx ) ]
						x <- x [ as.numeric ( names ( lx ) ) ]
						
						# adaptieren für gewünschte Effekte
						len <- sapply ( x , length )
						sel <- sapply ( len , "%in%" , effects )
						x <- x [ sel ]
						
						# tatsächlich relevante split.vars
						# f2 <- function ( x , d ) {
								# uel <- sapply ( d[,x,drop=FALSE] , unique , simplify = FALSE )
								###das jetzt ist voll behindi, aaahhhh, naja
								# y <- NULL
								# for ( i in seq ( along = names ( uel ) ) ) {
										# if ( is.null ( y ) ) y <- uel[i] else y <- merge ( y , uel[i] )
								# }
								# y <- data.frame ( y )
								# nrow ( y )
						# }
						# split.vars.mult <- mapply ( f2 , x , MoreArgs = list ( d2[,split.vars,drop=FALSE] ) , SIMPLIFY = TRUE )
						# split.vars.real <- mapply ( function ( x , mult ) rep ( list ( x ) , mult ) , x , split.vars.mult , SIMPLIFY = FALSE )
						# split.vars.real <- do.call ( "c" , split.vars.real )

						# splitten
						f1 <- function ( x , d2 ) {
								do <- paste0 ( "split ( d2 , f = list ( " , paste ( paste0 ( "d2$'" , x , "'" ) , collapse = "," ) , " ) )" )
								ret <- eval ( parse ( text = do ) )
						}
						d3a <- mapply ( f1 , x , MoreArgs = list ( d2 ) , SIMPLIFY = FALSE )
						d3 <- do.call ( "c" , d3a  )
						
						# split vars real (tatsächlich relevante split.vars)
						f9 <- function ( x , times ) {
								rep ( list ( x ) , times )
						}
						splvr <- mapply ( f9 , x , sapply ( d3a , length ) , SIMPLIFY = FALSE )
						split.vars.real <- unlist ( splvr , recursive = FALSE )						
						
						# gesamt adden
						if ( include.ALL ) {
								d3 <- c ( list ( d2 ) , d3 )
								names ( d3 )[1] <- "ALL"
								split.vars.real <- c ( list ( NULL ) , split.vars.real )
						}
				} else {
						d3 <- list ( d2 )
						names ( d3 )[1] <- "ALL"
						if ( !exists ( "split.vars.real" ) ) split.vars.real <- NULL
						split.vars.real <- c ( list ( NULL ) , split.vars.real )
				}
				
				### "leere" Kombinationen droppen
				not.leer <- ( sapply ( d3 , nrow ) > 0 )
				d4 <- d3[not.leer]
				split.vars.real <- split.vars.real[not.leer]

				### Casten der cast.for.total ###
				casten <- function ( d , cast.for.total , pv.vars , caseid ) {

						# return-Objekt d5
						d5 <- d
						
						if ( cast.for.total %in% colnames ( d ) ) {
								if ( length ( unique ( d[,cast.for.total] ) ) > 1 ) {
										d1 <- d[,!colnames(d)%in%c(pv.vars),drop=FALSE]
										d2 <- d[,c(caseid,cast.for.total,pv.vars),drop=FALSE]

										casten2 <- function ( d , pv.nam , ca , caseid , caseid.nam ) {
												d1 <- data.frame ( d )
												colnames ( d1 ) <- pv.nam
												d2 <- data.frame ( caseid , ca , stringsAsFactors = FALSE )
												dfr <- cbind ( d2, d1 )
												dfr$idstud <- seq ( along = rownames ( dfr ) )
												d3 <- reshape2::dcast ( data = dfr , formula = caseid ~ ca , value.var = pv.nam )
												colnames ( d3 )[!colnames ( d3 )%in%"caseid"] <- paste0 ( pv.nam , colnames ( d3 )[!colnames ( d3 )%in%"caseid"] )
												return ( d3 )
										}
										d3 <- mapply ( casten2 , d2[,!colnames(d2)%in%c(caseid,cast.for.total),drop=FALSE] , colnames(d2)[!colnames(d2)%in%c(caseid,cast.for.total)] , MoreArgs = list ( d2[,cast.for.total] , d2[,caseid] , caseid ) , SIMPLIFY = FALSE )
										# mergen
										d4 <- Reduce(function(x, y) merge(x, y, all=TRUE,by="caseid"),d3,accumulate=FALSE )
										colnames ( d4 )[colnames ( d4 ) == "caseid" ] <- caseid
										# Zusatzvariablen ran
										d5 <- merge ( d1[!duplicated(d1[,caseid]),] , d4 , by = caseid , sort = FALSE , all.y = TRUE )
										# cast.for.total auf NA
										d5[,cast.for.total] <- NA
								} 
						}
						return ( d5 )
				}
				if ( !is.null ( cast.for.total ) ) {
						if ( verbose ) cat ( paste0 ( "variable '" , cast.for.total , "' is (re)casted for overall analyses\n" ) )
						d5 <- mapply ( casten , d4 , MoreArgs = list ( cast.for.total , pv.vars , caseid ) , SIMPLIFY = FALSE )
				} else {
						d5 <- d4
				}
				
				
				### Ausgabe der Gesamtanzahl an Analysen ###
				if ( verbose ) {
						cat ( paste0 ( "Number of analyses: " , length ( d5 ) , "\n" ) )
				}
				# Ausgabe
				cat ( paste0 ( "mode = '" , mode , "'\n" ) )				

				# pv pooling auf Datensätzen
				pool.pv <- function ( d , nam , durchgang , split.vars.real , pv.vars , split.vars , weight , jkzone , jkrep , clusterid , nams , durchgaenge , caseid , split.vars.overall , jk2.mean.group , jk2.mean.group.cols , jk2.glm.independent , jk2.glm.independent.cols , stufen ) {

						# Fortschrittszahlen
						if ( verbose ) {
								cat ( durchgang )
								if ( durchgang%%20 == 0 ) lb <- "\n" else lb <- " "
								if ( is.character ( jkzone ) & is.character ( jkrep ) ) lb <- paste0 ( " " , nam , " (N=" , nrow ( d ) , ") " , paste ( rep ( "=" , abs ( 90 - length ( c ( durchgang , nam , nrow ( d ) ) ) ) ) , collapse = "" ) , "\n" )
								cat ( lb )
								if ( durchgang == max ( durchgaenge ) & !lb=="\n" ) cat ( "\n" )
								flush.console()
						}

					    # Adjustierung pv.vars wenn vorher cast.for.total passiert ist
						if ( ! all ( pv.vars %in% colnames ( d ) ) ) {
								pv.vars <- colnames ( d ) [ colnames ( d ) %in% unique ( unname ( do.call ( "c" , sapply ( pv.vars , function ( x , y ) y[ grepl ( x , y ) ] , colnames ( d ) , simplify = FALSE ) ) ) ) ]
						}
						
						# Standard-Statistiken
						if ( mode %in% "cont" ) {
								mean <- 	 mean ( sapply ( d[,pv.vars,drop=FALSE] , mean , na.rm = TRUE ) )
								sd <- 		 mean ( sapply ( d[,pv.vars,drop=FALSE] , sd , na.rm = TRUE ) )
								se <- 		 mean ( sapply ( d[,pv.vars,drop=FALSE] , std.error , na.rm = TRUE ) )
								ret1 <- data.frame ( mean , sd , se )
								
								if ( !is.null ( weight ) ) {
										mean.weighted <- mean ( mapply ( wtd.mean , d[,pv.vars,drop=FALSE] , MoreArgs = list ( weights = d[,weight] , na.rm = TRUE ) ) )
										sqrtVar.weighted <- sqrt ( mean ( mapply ( wtd.var , d[,pv.vars,drop=FALSE] , MoreArgs = list ( weights = d[,weight] , na.rm = TRUE ) ) ) )
										
										ret1 <- cbind ( ret1 , data.frame ( mean.weighted , sqrtVar.weighted ) )
								}
						} else {
								ret1 <- NULL
						}
						
						### Jackknifen ###
						if ( is.character ( jkzone ) & is.character ( jkrep ) & mode %in% c ( "cont" , "cat" , "glm" ) ) {
								
								# Return-Objekt
								ret3 <- NULL
								
								### Checks ###
								check <- logical(0)
								check[1] <- identical ( sort ( as.numeric ( unique ( d$jkrep ) ) ) , as.numeric ( c(0,1) ) )
								
								if ( ! all ( check ) ) {
										if ( verbose ) {
												cat ( paste0 ( "jackknifing not possible:\n" ) )
												if ( !check[1] ) cat ( paste0 ( "     'jkrep' does not contain both values 0 and 1\n" ) )
										}
								}
								
								### Jackknifen ###
								# mode = "cont"
								if ( mode %in% "cont" ) {
						
										if ( all ( check ) ) {
												
												### jk2.mean ###
												cat ( "jk2.mean\n" )
												if ( is.null ( jk2.mean.group ) ) {
														### Normal ###
														tried <- try ( jk.res <- jk2.mean ( dat = d, ID = caseid, wgt = weight , 
																	 JKZone = jkzone , JKrep = jkrep, 
																	 dependent = list ( pv.vars ) , complete.permutation = "no" ) , silent = FALSE )
												} else {
														### mit Gruppe ###
														# groups nach Faktoren wandeln
														levs <- sort ( unique ( do.call ( "c" , sapply ( d [ , jk2.mean.group.cols ] , unique , simplify = FALSE ) ) ) )
														do <- paste0 ( "d$" , jk2.mean.group.cols , " <- factor ( d$" , jk2.mean.group.cols , " , levels = c(" , paste(paste0 ( "'" , levs , "'" ), collapse = ',' ) , "))" )
														eval ( parse ( text = do ) )
														group.list <- list ( jk2.mean.group.cols )
														names ( group.list ) <- toupper ( jk2.mean.group )
														group.differences.by <- names ( group.list )[1]
														tried <- try ( jk.res <- jk2.mean ( dat = d, ID = caseid, wgt = weight , 
																	 JKZone = jkzone , JKrep = jkrep, 
																	 dependent = list ( pv.vars ) , complete.permutation = "no" , group = group.list , group.differences.by = group.differences.by ) , silent = FALSE )
												}
												
												if ( !is.null ( quants ) ) {
														### jk2.quantile ###
														cat ( "jk2.quantile\n" )
														tried2 <- try ( jk.res2 <- jk2.quantile ( dat = d, ID = caseid, wgt = weight , 
																	 JKZone = jkzone , JKrep = jkrep, 
																	 dependent = list ( pv.vars ) ,
																	 probs = quants ,
																	 complete.permutation = "no" ) , silent = FALSE )
												} else {
														tried2 <- try ( uasdjfojwernhkjjsdfjkl - asdfjasdejkkekk , silent = TRUE )
														jk.res2 <- NULL
												}
										} else {
												tried <- tried2 <- try ( uasdjfojwernhkjjsdfjkl - asdfjasdejkkekk , silent = TRUE )
												jk.res <- jk.res2 <- NULL
										}
								
										if ( !inherits ( tried , "try-error" ) & !is.null ( jk.res ) ) {
												
												if ( !is.null ( jk2.mean.group ) ) {
														group.diff <- attr( jk.res[[1]] , "difference" )
														# Datensatz im wide format machen
														vec <- c ( group.diff$m.pooled , group.diff$se.pooled )
														names ( vec ) <- paste ( rep ( group.diff$vgl , 2 ) , c( rep ( "m.pooled" , nrow ( group.diff ) ) , rep ( "se.pooled" , nrow ( group.diff ) ) ) , sep = "." )
														group.diff.dfr <- data.frame ( NA )
														group.diff.dfr <- group.diff.dfr [,-1,drop=FALSE]
														do <- paste0 ( "group.diff.dfr$'" , names ( vec ) , "' <- " , vec )
														eval ( parse ( text = do ) )
														
														grd2 <- jk.res[[1]][,c(group.differences.by,"N_unweighted","se.N_unweighted","N_weighted","se.N_weighted","mean","se.mean","SD","se.SD")]
														colnames ( grd2 )[1] <- "group"
														vec <- c ( grd2$mean , grd2$se.mean )
														names ( vec ) <- paste ( rep ( grd2$group , 2 ) , c( rep ( "mean" , nrow ( grd2 ) ) , rep ( "se.mean" , nrow ( grd2 ) ) ) , sep = "." )
														grd2 <- data.frame ( NA )
														grd2 <- grd2 [,-1,drop=FALSE]
														do <- paste0 ( "grd2$'" , names ( vec ) , "' <- " , vec )
														eval ( parse ( text = do ) )													
														
														group.diff.dfr <- cbind ( grd2 , group.diff.dfr )
														
												} else {
														group.diff.dfr <- NULL
														
														jk.mean <- unname ( unlist ( jk.res[[1]]$mean )[1] )
														jk.mean.se <- unname ( unlist ( jk.res[[1]]$se.mean )[1] )
														jk.sd <- unname ( unlist ( jk.res[[1]]$SD )[1] )
														jk.sd.se <- unname ( unlist ( jk.res[[1]]$se.SD )[1] )
														ret3a <- data.frame ( jk.mean , jk.mean.se , jk.sd , jk.sd.se )
												
												}
										} else {
												group.diff.dfr <- NULL
												
												jk.mean <- NA
												jk.mean.se <- NA
												jk.sd <- NA
												jk.sd.se <- NA
												ret3a <- data.frame ( jk.mean , jk.mean.se , jk.sd , jk.sd.se )
										}

										if ( !inherits ( tried2 , "try-error" ) & !is.null ( jk.res2 ) ) {
												z <- sapply ( apply ( jk.res2[[1]][,c("m.pooled","se.pooled"),drop=FALSE] , 1 , c ) , c )
												z1 <- paste0 ( "q." , do.call ( "c" , sapply ( jk.res2[[1]]$per.number , rep , 2 , simplify = FALSE ) ) * 100 )
												z2 <- rep ( c ( ".mean" , ".se" ) , nrow ( jk.res2[[1]] ) )
												names ( z ) <- paste0 ( z1 , z2 )
												qd <- data.frame ( NA )
												do <- paste0 ( "qd$" , names ( z ) , " <- " , z )
												eval ( parse ( text = do ) )
												qd <- qd [ , -1 , drop = FALSE ]
										} else {
												z1 <- paste0 ( "q." , do.call ( "c" , sapply ( quants , rep , 2 , simplify = FALSE ) ) * 100 )
												z2 <- rep ( c ( ".mean" , ".se" ) , length ( quants ) )
												z <- paste0 ( z1 , z2 )
												qd <- data.frame ( matrix ( rep ( NA , length ( z ) ) , nrow = 1 ) )
												colnames ( qd ) <- z
										}								
										
										# Return-Datensatz
										if ( !is.null ( jk2.mean.group ) ) {
												ret3 <- group.diff.dfr
										} else {
												ret3 <- cbind ( ret3a , qd )
										}
								
								}
								
								### GLM ###
								# mode = "glm"
								if ( mode == "glm" ) {									
						
										# groups nach Faktoren wandeln
										# levs <- sort ( unique ( do.call ( "c" , sapply ( d [ , jk2.mean.group.cols ] , unique , simplify = FALSE ) ) ) )
										# do <- paste0 ( "d$" , jk2.mean.group.cols , " <- factor ( d$" , jk2.mean.group.cols , " , levels = c(" , paste(paste0 ( "'" , levs , "'" ), collapse = ',' ) , "))" )
										# eval ( parse ( text = do ) )
										# group.list <- list ( jk2.mean.group.cols )
										# names ( group.list ) <- toupper ( jk2.mean.group )
										# group.differences.by <- names ( group.list )[1]
										
										# independent list machen
										independent.list <- sapply ( jk2.glm.independent , function ( x , y ) y[ grepl ( x , y ) ] , jk2.glm.independent.cols , simplify = FALSE )
									
										tried <- try ( jk2.glm ( 
													dat = d,
													ID = caseid,
													wgt = weight, 
													JKZone = jkzone,
													JKrep = jkrep, 
													dependent = list ( pv.vars ),
													complete.permutation = "no",
													independent = independent.list , 
													glm.family = jk2.glm.family
													) , silent = FALSE )
										
										if ( !inherits ( tried , "try-error" ) ) {
												ret2 <- ret3 <- tried[[1]]
												
												vec <- c ( ret3$beta , ret3$se.beta )
												names ( vec ) <- paste ( rep ( ret3$reg , 2 ) , c( rep ( "beta" , nrow ( ret3 ) ) , rep ( "se.beta" , nrow ( ret3 ) ) ) , sep = "." )
												ret3 <- data.frame ( NA )
												ret3 <- ret3 [,-1,drop=FALSE]
												do <- paste0 ( "ret3$'" , names ( vec ) , "' <- " , vec )
												eval ( parse ( text = do ) )									
												
												ret3$N <- ret2$N[1]
												ret3$N.valid <- ret2$N.valid[1]
												ret3$beta <- ret2$se.beta[1]
												ret3$R2 <- ret2$R2[1]
												ret3$se.R2 <- ret2$se.R2[1]
												ret3$R.nagelk <- ret2$R.nagelk[1]
												ret3$se.R.nagelk <- ret2$se.R.nagelk[1]

										} else {
												ret3 <- NULL
										}
								}								
								
								
								### Jackknifen ###
								# mode = "cat"
								
								if ( mode %in% "cat" ) {
								
										if ( all ( check ) ) {
												# temporärer jk2.table Bug-Fix, muss gelabelt sein
												dependent <- list ( "x" = pv.vars )

												### jk2.table ###
												cat ( "jk2.table\n" )
												tried3 <- try ( jk.res3 <- jk2.table ( dat = d, ID = caseid, wgt = weight , 
															 JKZone = jkzone , JKrep = jkrep, 
															 dependent = dependent , complete.permutation = "no" ) , silent = FALSE )
												cat ( "\n" )
										} else {
												tried3 <- try ( uasdjfojwernhkjjsdfjkl - asdfjasdejkkekk , silent = TRUE )
												jk.res3 <- NULL
										}

										# Soll-Datensatz
										z1 <- paste0 ( "ks." , do.call ( "c" , sapply ( stufen , rep , 2 , simplify = FALSE ) ) )
										z2 <- rep ( c ( ".mean" , ".se" ) , length ( stufen ) )
										z <- paste0 ( z1 , z2 )
										td <- data.frame ( matrix ( rep ( as.numeric(NA) , length ( z ) ) , nrow = 1 ) , stringsAsFactors = FALSE )
										colnames ( td ) <- z										

										# Werte setzen
										if ( !inherits ( tried3 , "try-error" ) & !is.null ( jk.res3 ) ) {
												z <- sapply ( apply ( jk.res3[[1]][,c("m.pooled","se.pooled"),drop=FALSE] , 1 , c ) , c )
												z1 <- paste0 ( "ks." , do.call ( "c" , sapply ( as.character( jk.res3[[1]]$suffix ) , rep , 2 , simplify = FALSE ) ) )
												z2 <- rep ( c ( ".mean" , ".se" ) , nrow ( jk.res3[[1]] ) )
												names ( z ) <- paste0 ( z1 , z2 )

												# Werte im Soll-Datensatz setzen
												do <- paste0 ( "td$" , names ( z ) , " <- " , z )
												eval ( parse ( text = do ) )
										}							

										# Return-Datensatz
										ret3 <- td
								}
						
						} else if ( !is.null ( clusterid ) & mode %in% "icc" ) {
						
								### ICC ###
								# mode = "icc"
								if ( mode == "icc" ) {								
										if ( !is.null ( clusterid ) ) {
												cat ( paste0 ( "ICC\n" ) )
												icc <- NULL
												# ICC Kennwerte auswählen (Funktion pool.corr)
												auswahl <- c("r","rse")
												new.nam <- c("ICC","ICC.se")
												for ( cid in clusterid ) {
														cat ( paste0 ( "cluster: " , cid , "\n" ) )
														calc.icc <- function ( pv , cid , weight , d ) {
																cat ( "." )
																d2 <- d[,c(pv,cid,weight),drop=FALSE]
																if ( is.null ( weight ) ) {
																		do <- paste0 ( "tried5 <- try ( icc <- ICC1 ( aov ( ",pv," ~ as.factor(",cid,") , data = d2 ) ) )" )
																} else {
																		do <- paste0 ( "tried5 <- try ( icc <- ICC1 ( aov ( ",pv," ~ as.factor(",cid,") , data = d2 , weights = ",weight," ) ) )" )
																}
																eval ( parse ( text = do ) )
																if ( inherits ( tried5 , "try-error" ) ) icc <- NA
																return ( icc )
														}
														icc.list <- mapply ( calc.icc , pv.vars, MoreArgs = list ( cid , weight , d ) , SIMPLIFY = FALSE )
														icc.vec <- do.call ( "c" , icc.list )
													
														# Poolen
														tried4 <- try ( icc.p <- pool.corr ( icc.vec , N = nrow(d) ) )
														
														if ( !inherits ( tried4 , "try-error" ) ) {
																icc. <- data.frame ( matrix ( icc.p , nrow = 1 ) )
																colnames ( icc. ) <- names ( icc.p )
																# Auswahl
																icc. <- icc.[,auswahl,drop=FALSE]
														} else {
																icc. <- data.frame ( matrix ( rep ( NA , length ( auswahl ) ) , nrow = 1 ) )
														}
														colnames ( icc. ) <- paste0 ( cid , "." , new.nam )
														if ( is.null ( icc ) ) icc <- icc. else icc <- cbind ( icc , icc. )
														cat ( "\n" )
												}
												ret3 <- icc
										} else {
												ret3 <- NULL
										}
								}
								
								
						} else {
								ret3 <- NULL
						}
								

						# falls gewichtet, noch ".weighted" an Variablennamen
						if ( !is.null ( weight ) & !is.null ( ret3 ) ) {
								colnames ( ret3 ) <- paste0 ( colnames ( ret3 ) , ".weighted" )
						}

						# Jackknife-"Statistiken", cluster.N
						if ( !is.null ( ret3 ) ) {
								if ( mode %in% c ( "cont" , "cat" ) ) {
										if ( ! is.null ( jkzone ) ) {
												ret3$jkzone.N <- length ( unique ( d[,jkzone] ) )
										}
								}
								if ( mode %in% "icc" ) {
										if ( !is.null ( clusterid ) ) {
												for ( cid in clusterid ) {
														ret3$cluster.N <- length ( unique ( d[,cid] ) )
														colnames ( ret3 )[ colnames ( ret3 )== "cluster.N" ] <- paste0 ( cid , ".N" )
												}
										}
								}
						}
						
						# Anzahl uniquer jkrep je jkzone
						# do <- paste0 ( "xtab <- xtabs ( ~ " , jkzone , " + " , jkrep , " , data = d )" )
						# eval ( parse ( text = do ) )
						# xtab.u <- apply ( xtab , 1 , function ( z ) length(z[z>0]) )
						# ret3$jkrep.min <- min ( xtab.u )
						# ret3$jkrep.max <- max ( xtab.u )

						# Standard-Deskriptives mit Jackknife Statistiken mergen, bei mode = "cont"
						if ( mode %in% "cont" ) {
								if ( ! is.null ( ret1 ) & ! is.null ( ret3 ) ) {
										ret1 <- cbind ( ret1 , ret3 )
								} else {
										ret1 <- NULL
								}
						} else {
								ret1 <- ret3
						}

						### Split-Vars ###
						ret2 <- d[1,split.vars,drop=FALSE]
						if ( ! all ( colnames(ret2) %in% split.vars.real ) ) {
								do <- paste0 ( "ret2$'" , colnames(ret2)[ !colnames(ret2) %in% split.vars.real ] , "'<-NA" )
								eval ( parse ( text = do ) )
						}

						if ( !is.null ( ret1 ) ) {
								ret <- cbind ( ret2 , ret1 )
						} else {
								ret <- NULL
						}

						### Kombination / Ns ###
						comb.names <- unname ( apply ( ret [ , split.vars , drop = FALSE ] , 1 , function ( z ) paste ( z[!is.na(z)] , collapse = "." ) ) )
						comb.names[comb.names==""] <- "ALL"
						if ( !is.null ( weight ) ) caseid.N.weighted <- sum ( d[,weight] , na.rm = TRUE ) else caseid.N.weighted <- NA
						if ( !is.null ( ret ) ) {
								ret <- cbind ( data.frame ( "combination" = comb.names, "caseid.N" = nrow ( d ) , "caseid.N.weighted" = caseid.N.weighted , "pv.vars.N" = length ( pv.vars ) , stringsAsFactors = FALSE ) , ret )
						}

						# setzen von split var overall Kategorien Namen
						if ( !is.null ( split.vars.overall ) ) {
								do <- paste0 ( "ret$" , names ( split.vars.overall ) , "[is.na(ret$" , names ( split.vars.overall ) , ")] <- '" , split.vars.overall , "'" )
								eval ( parse ( text = do ) )
						}

						# Sicherheitskopie der "Zelle"
						if ( ! is.null ( folder.temp ) ) {
								if ( file.exists ( folder.temp ) ) {
										do <- paste0 ( "try ( save ( ret , file = file.path ( folder.temp , '" , nam , "." , durchgang , ".Rdata' ) ) , silent = TRUE )" )
										eval ( parse ( text = do ) )
								}
						}
						
						return ( ret )
				}

# browser()
# d5 <- d5[c(1030,1031,1032)]
# d5 <- d5[c(1031)]
# d5 <- d5[c(2711,2712,2713)]
# d5 <- d5[c(1141)]
# split.vars.real <- split.vars.real[c(1030,1031,1032)]
# split.vars.real <- split.vars.real[c(1031)]
# split.vars.real <- split.vars.real[c(2711,2712,2713)]
# split.vars.real <- split.vars.real[c(1141)]
		
				pvdescr <- mapply ( pool.pv , d5 , names ( d5 ) , seq ( along = d5 ) , split.vars.real , MoreArgs = list ( pv.vars , split.vars , weight , jkzone , jkrep , clusterid , names ( d5 ) , seq ( along = d5 ) , caseid , split.vars.overall , jk2.mean.group , jk2.mean.group.cols , jk2.glm.independent , jk2.glm.independent.cols , stufen ) , SIMPLIFY = FALSE )
		
				# falls NULL Datensätze, vorsorglich noch raus
				keep <- !sapply(pvdescr, is.null )
				pvdescr <- pvdescr [ keep ]
				split.vars.real <- split.vars.real [ keep ]
				
				### löschen ###
				# table ( sapply ( pvdescr , ncol ) )
				# table ( sapply ( pvdescr , nrow ) )
				# rn <- colnames ( pvdescr[[1]] )
				# prob <- sapply ( pvdescr , function ( el , rn ) identical ( colnames ( el ) , rn ) , rn  )
				# rn2 <- colnames ( pvdescr[[which(!prob)[1]]] )
				
				# Datensätze zusammenbinden
				if ( length ( pvdescr ) > 0 ) {
						pvdescr <- do.call ( "rbind" , pvdescr )
				} else {
						pvdescr <- NULL
				}
				
				# rownames
				if ( !is.null ( pvdescr ) ) {
						rownames ( pvdescr ) <- seq ( along = rownames ( pvdescr ) )
				}
				
				ret <- pvdescr
		}

		# sortieren nach split vars
		# if ( ! is.null ( ret ) & ! is.null ( split.vars.sort.list ) ) {
				# ret <- sort.rows ( ret , split.vars.sort.list )
		# }		

		# Fertig
		if ( verbose ) {
				cat ( "done" )
				cat ( "\n" )
				flush.console()
		}
		
		return ( ret )
}

sort.rows <- function ( d , l ) {

	# ok, behindi jetze
	d2 <- NULL
	for ( i in names ( rev(l) ) ) {
			if ( is.null ( d2 ) ) d2 <- l[i] else d2 <- merge ( d2 , l[i] )
	}
	d2 <- data.frame ( d2 , stringsAsFactors = FALSE )
	colnames ( d2 ) <- rev ( names ( l ) )
	d2 <- d2[,rev(colnames(d2)),drop=FALSE]
	eval ( parse ( text = paste0 ( "d2$" , colnames ( d2 ) , " <- as.character ( d2$" , colnames ( d2 ) , " )" ) ) )	
	
	d3 <- d[,colnames(d2),drop=FALSE]
	
	new.order <- apply ( d3 , 1 , function ( z , d2 ) which ( apply ( d2 , 1 , identical , z ) ) , d2 )
	if ( is.list ( new.order ) ) {
			new.order <- sapply ( new.order , "[" , 1 )
			
			# no <- integer(0)
			# for ( i in seq ( along = new.order ) ) {
					# no <- c ( no , new.order [[i]] )
			# }
			# new.order <- no
	}
	
	names ( new.order ) <- seq ( along = new.order )
	new.order <- sort ( new.order )
	new.order <- as.integer ( names ( new.order ) )
	
	ret <- d[ new.order,,drop=FALSE ]
	rownames ( ret ) <- seq ( along = rownames ( ret ) )
	return ( ret )
	
}


# Kopie aus
# T:\Sebastian\Charite\Funktion.r
# 17.04.2013
### subroutine for combining correlations for multiply imputed data - basiert auf Funktion von Alexander Robitzsch
pool.corr <- function( corrs , N , conf.level = .05){
        fisher.corrs <- 1/2*log( ( 1 + corrs) / ( 1 - corrs ) )                 ### convert correlations to Fisher transformed values
        var.fisher <- rep( 1/(N-3) , length(corrs) )
        if(!exists("pool.scalar"))   {library(mice)}                            ### combination of point estimators according Rubin's formula
        fisher.cor.combine <- pool.scalar( fisher.corrs , var.fisher)
        zr <- fisher.cor.combine$qbar
        zr.se <- sqrt( fisher.cor.combine$t )
        t.zr <- zr / zr.se
        fisher2cor <- function(z){ ( exp(2*z) - 1 )/ ( exp(2*z) + 1 ) }
        res <- c( "r" = fisher2cor(zr)  ,
            "fisher_r" = zr ,
            "fisher_rse" = zr.se ,
            "t" = t.zr  ,
            "p" = 2 * pnorm( abs(t.zr) , lower.tail = FALSE ) ,
             fisher2cor( zr + qnorm( ( 1 - conf.level ) / 2 ) * zr.se ) ,
             fisher2cor( zr - qnorm( ( 1 - conf.level ) / 2 ) * zr.se ) )
            names(res)[6] <- paste( "lower" , round(100*conf.level,2),sep="")
            names(res)[7] <- paste( "upper" , round(100*conf.level,2),sep="")
        res <- c( res , ( res[6] - res[7] ) / ( 2* qnorm( ( 1 - conf.level )/2 ) ) )
        names(res)[8] <- "rse"
        res <- res[ c(1,8,2:7) ]
        res <- round(res, 6)
        return(res) }

