
splitModels <- function ( qMatrix = NULL , person.groups = NULL , split = c ( "qMatrix" , "person.groups" ) , add = NULL , cross = NULL , all.persons = TRUE , all.persons.lab = "all" , person.split.depth = 0:length(person.groups[,-1,drop=FALSE]), full.model.names = TRUE , model.name.elements = c ( "dim" , "group" , "cross" ) , include.var.name = FALSE , env = FALSE , nCores=NULL , GBcore=NULL , verbose = TRUE ) {
		
		# Funktion: person.groups nach person.grouping
		pg2pgr <- function ( x , nam ) {
				d <- x[,1,drop=FALSE]
				eval ( parse ( text = paste0 ( "d$'" , nam , "' <- 1 " ) ) )
				return ( d )
		}

		# wenn kein data.frame, dann ignorieren
		if ( !is.null ( qMatrix ) & !is.data.frame ( qMatrix ) ) {
				qMatrix <- NULL
				warning ( paste0 ( "splitModels: qMatrix is not a data.frame and will be ignored." ) , call. = FALSE )
		}
		if ( !is.null ( person.groups ) & !is.data.frame ( person.groups ) ) {
				person.groups <- NULL
				warning ( paste0 ( "splitModels: person.groups is not a data.frame and will be ignored." ) , call. = FALSE )
		}

		# wenn keine Spalten / Zeilen dann NULL
		if ( !is.null ( qMatrix ) ) {
				if ( nrow ( qMatrix ) %in% 0 | ncol ( qMatrix ) %in% 0 ) {
						warning ( "splitModels: check qMatrix" , call. = FALSE )
						qMatrix <- NULL
				}
		}
		if ( !is.null ( person.groups ) ) {
				if ( nrow ( person.groups ) %in% 0 | ncol ( person.groups ) %in% 0 ) {
						warning ( "splitModels: check person.groups" , call. = FALSE )
						person.groups <- NULL
				}
		}

		# Dimensionen in qMatrix duerfen nur 0/1 haben
		if ( !is.null ( qMatrix ) ) {
				if ( ncol ( qMatrix ) > 1 ) {
						not01 <- ! sapply ( qMatrix[,-1,drop=FALSE] , function ( x ) all ( x %in% c(0,1) ) )
						
						if ( any ( not01 ) ) {
								warning ( paste0 ( "splitModels: column(s) '" , paste ( names (not01)[not01] , collapse = "', '" ) , "' in qMatrix contain elements that are not 0 or 1; this/these column(s) are ignored" ) , call. = FALSE )
								qMatrix <- qMatrix[,colnames(qMatrix)[!colnames(qMatrix) %in% names (not01)[not01]],drop=FALSE]
						}
				}
		}
		
		# wenn nur eine Spalte wird diese als IDs angenommen
		if ( !is.null ( qMatrix ) ) {
				if ( ncol ( qMatrix ) %in% 1 ) {
						warning ( "splitModels: qMatrix contains just one column; this is treated as item names" , call. = FALSE )
						qMatrix$dim <- 1
				}
		}
		if ( !is.null ( person.groups ) ) {
				if ( ncol ( person.groups ) %in% 1 ) {
						warning ( "splitModels: person.groups contains just one column; this is treated as person ids" , call. = FALSE )
						person.groups$group <- all.persons.lab
						all.persons <- FALSE
				}
		}		
		
		# qMatrix und person.groups auf Plausibilitaet checken
		if ( !is.null ( qMatrix ) ) {
				# hat erste Spalte mehr Elemente als alle anderen
				len <- sapply ( qMatrix , function ( x ) length ( unique ( x ) ) )
				len.log <- len < len[1]
				if ( ! all ( len.log[-1] ) ) warning ( paste0 ( "splitModels: first column of qMatrix might not contain item names; please check\n(number of unique elements is smaller than in another column)" ) , call. = FALSE )
		}
		if ( !is.null ( person.groups ) ) {
				# hat erste Spalte mehr Elemente als alle anderen
				len <- sapply ( person.groups , function ( x ) length ( unique ( x ) ) )
				len.log <- len < len[1]
				if ( ! all ( len.log[-1] ) ) warning ( paste0 ( "splitModels: first column of person.groups might not contain person ids; please check\n(number of unique elements is smaller than in another column)" ) , call. = FALSE )
		}
		
		# aus Split die Sachen raus, die nicht da sind
		if ( is.null ( qMatrix ) ) split <- split[!split %in% "qMatrix"]
		if ( is.null ( person.groups ) ) split <- split[!split %in% "person.groups"]

		# all.persons.lab checken ob bereits eine Kategorie in person.groups so heisst
		if ( !is.null ( person.groups ) & all.persons ) {
				cats <- unique ( unname ( do.call ( "c" , sapply ( person.groups[,-1,drop=FALSE] , unique , simplify = FALSE ) ) ) )
				if ( all.persons.lab %in% cats ) {
						# Alternativen checken
						alt <- c ( "all" , "ALL" , "_all_" , "_ALL_" )
						# wenn eine der Alternativen nicht in Kategorien, dann diese setzen
						if ( any ( !alt %in% cats ) ) {
								new.lab <- alt[!alt %in% cats][1]
						} else {
						# solange random erzeugen bis eine noch nicht verwendete Kategorie gefunden
								new.lab <- cats[1]
								while ( new.lab %in% cats ) {
										set.seed ( 1234567 )
										new.lab <- paste ( sample ( letters , 3 , replace = TRUE ) , collapse = "")
								}
						}
						# Warnung
						warning ( paste0 ( "splitModels: '" , all.persons.lab , "' is already a used category in person.groups, it has been changed to '" , new.lab , "'." ) , call. = FALSE )
						# neues Label setzen
						all.persons.lab <- new.lab
				}
		}

		# wenn Faktoren in person.groups, dann sortieren
		# bei Nicht-Faktoren Reihenfolge wie im Datensatz
		colcl <- sapply ( person.groups[,-1,drop=FALSE] , class )
		if ( any ( colcl %in% "factor" ) ) {
				do.order <- paste0 ( "person.groups <- person.groups[order(",paste ( paste0 ( "person.groups$" , names ( colcl[colcl %in% "factor"] ) ) , collapse = "," ),"),]" )
				eval ( parse ( text = do.order ) )
		}
		
		# qMatrix
		if ( "qMatrix" %in% split & !is.null ( qMatrix ) ) {
				# qMatrix mit mehreren Dimensionen zu Liste von item.groupings mit nur einer Dimension
				i <- sapply ( colnames ( qMatrix )[-1] , function ( x , d ) { d <- d[,c(colnames(qMatrix)[1],x)]; d <- d[ d[,x] %in% 1 , ]; return ( d ) } , qMatrix , simplify = FALSE )
		} else if ( ! "qMatrix" %in% split & !is.null ( qMatrix ) ) {
				i <- list ( qMatrix )
		} else {
				i <- list ( qMatrix )
		}

		# benennen wenn das erste Element von i nicht NULL ist
		if ( !is.null ( i[[1]] ) ) {
				names ( i ) <- unname ( sapply ( i , function ( x ) paste ( colnames ( x )[-1] , collapse = "_" ) , USE.NAMES = FALSE ) )
		} else {
				names ( i ) <- ""				
		}
				
		if ( "person.groups" %in% split & !is.null ( person.groups ) ) {
	
				#  Liste mit allen Kategorien aller Gruppen machen
				make.pers.l <- function ( v , x , all.persons , all.persons.lab ) { 
						cats <- unique ( as.character ( v ) )
						if ( all.persons ) cats <- c ( cats , "all" )
						d <- data.frame ( cats , stringsAsFactors = FALSE )
						colnames ( d ) <- x
						return ( d )
				}
				pers.l <- mapply ( make.pers.l , person.groups[,-1,drop=FALSE] , colnames ( person.groups )[-1] , MoreArgs = list ( all.persons , all.persons.lab ) , SIMPLIFY = FALSE )
				
				# jetzt komplettes Kreuzen der Kategorien
				# pers.l reversen fuer schoenere Sortierung der Kategorien
				p <- Reduce(function(x, y) merge(x, y, all=TRUE,by=NULL),rev(pers.l),accumulate=FALSE )
				# Spaltenreihenfolge zurueckaendern
				p <- p[,rev(colnames(p)),drop = FALSE ]
				
				# person.split.depth
				# muss innerhalb 0:length(person.groups[,-1,drop=FALSE]) liegen, alle anderen ignorieren
				if ( is.numeric ( person.split.depth ) ) {
						person.split.depth <- as.integer ( round ( person.split.depth ) )
						person.split.depth <- person.split.depth[person.split.depth %in% 0:length(person.groups[,-1,drop=FALSE])]
						if ( identical ( person.split.depth , integer(0) ) | any ( is.na ( person.split.depth ) ) ) {
								person.split.depth <- 0:length(person.groups[,-1,drop=FALSE])
								warning ( paste0 ( "splitModels: parameter person.split.depth is out of range and will be defaulted to " , person.split.depth[1] , ":" , person.split.depth[length(person.split.depth)] ) , call. = FALSE )
						}
				} else {
						person.split.depth <- 0:length(person.groups[,-1,drop=FALSE])
						if ( !is.null ( person.groups ) ) {
								warning ( paste0 ( "splitModels: parameter person.split.depth is falsely set and will be defaulted to " , person.split.depth[1] , ":" , person.split.depth[length(person.split.depth)] ) , call. = FALSE )						
						}
				}

				# Tiefe berechnen
				depth <- ncol ( p ) - apply ( p , 1 , function ( z , all.persons.lab ) sum ( z %in% all.persons.lab ) , all.persons.lab )
				# alle rausnehmen, die nicht die richtige Tiefe haben
				p <- p[ depth %in% person.split.depth , , drop = FALSE ]
			
				# wenn alle rausgenommen wurden, p2 als list(NULL) setzen
				if ( nrow ( p ) %in% 0 ) {
						p2 <- list(NULL)
				} else {
				
						# person.groups reduzieren/listen
						p2 <- list ()
						f1 <- function ( z , all.persons.lab ) {

								# nur nicht all.persons.lab
								b2 <- !z %in% all.persons.lab
								z2 <- z [ b2 ]
								if ( ! identical ( names ( z2 ) , character(0) ) ) {
										str2 <- paste0 ( "person.groups$" , names ( z2 ) , " %in% '" , z2 , "'" )
								} else {
										str2 <- NULL
								}
								
								# alle all.persons.lab
								# hier NAs loeschen
								b3 <- !b2
								z3 <- z [ b3 ]
								if ( ! identical ( names ( z3 ) , character(0) ) ) {
										str3 <- paste0 ( "! is.na ( person.groups$" , names ( z3 ) , " )" )
								} else {
										str3 <- NULL
								}
				
								paste0 ( "person.groups[ " , paste ( c ( str2 , str3 ) , collapse = " & " ) , ",]" )
						}
						do1 <- apply ( p , 1 , f1 , all.persons.lab )
						do1 <- paste0 ( paste0 ( "p2[[" , seq ( along = do1 ) , "]] <- " ) , do1 ) 
						eval ( parse ( text = do1 ) )
				}
		} else if ( ! "person.groups" %in% split & !is.null ( person.groups ) ) {
				p <- data.frame ( matrix ( rep ( all.persons.lab , ncol ( person.groups ) - 1 ) , ncol = ncol ( person.groups ) - 1 ) , stringsAsFactors = FALSE )
				colnames ( p ) <- colnames ( person.groups )[-1]
				p2 <- list ( person.groups )
		} else {
				p2 <- list ( person.groups )
		}
				
		# wenn das erste Element von p2 nicht NULL ist
		if ( !is.null ( p2[[1]] ) ) {
		
				# benennen
				f2 <- function ( z ) {
						paste ( mapply ( function ( x , y ) paste0 ( x , "." , y ) , names ( z ) , z , USE.NAMES = FALSE ) , collapse = "_" )
				}
				pers.names <- apply ( p , 1 , f2 )			
				names ( p2 ) <- pers.names
				
				# nicht vorhandene Kombinationen droppen
				keep <- sapply ( p2 , nrow ) > 0
				groups.dropped <- names ( keep[!keep] )
				p2 <- p2[keep]
				
				# person.groups nach person.grouping
				p3 <- mapply ( pg2pgr , p2 , names ( p2 ) , SIMPLIFY = FALSE )
				
		} else {
				p3 <- p2
				names ( p3 ) <- ""
		}
		
		# Item- und Personendatensaetze zum spaeteren kreuzen
		i.dfr <- data.frame ( "dim" = names ( i ) , stringsAsFactors = FALSE )
		p.dfr <- data.frame ( "group" = names ( p3 ) , stringsAsFactors = FALSE )
		
		# Abgleich von cross und add
		# cross gewinnt, d.h. wenn in cross, wirds aus add rausgenommen
		if ( !is.null ( cross ) & !is.null ( add ) ) {
				if ( any ( names ( cross ) %in% names ( add ) ) ) add <- add[!names(add) %in% names(cross)]
				if ( length ( add ) < 1 ) add <- NULL
		}
		
		# wenn Elemente von add Listen sind, dann erstes Element jeweils und Warnmeldung
		if ( !is.null ( add ) ) {
				add.is.list <- sapply ( add , is.list )
				if ( any ( add.is.list ) ) {
						warning ( paste0 ( "splitModels: One or more elements of add are lists. Only first element of each list entry is used." ) , call. = FALSE )
						do.unlist <- paste0 ( "add$" , names ( add[add.is.list] ) , " <- add$" , names ( add[add.is.list] ) , "[[1]]" )
						eval ( parse ( text = do.unlist ) )
				}
		}
		
		# aus add Elemente mit Laenge 0 (z.B. NULL) rausnehmen
		if ( !is.null ( add ) ) {
				addlength <- sapply ( add , length )
				if ( any ( addlength < 1 ) ) add <- add[!(addlength < 1)]
				if ( length ( add ) < 1 ) add <- NULL
		}
		
		# aus cross Elemente mit Laenge 0 (z.B. NULL) rausnehmen
		if ( !is.null ( cross ) ) {
				crosslength <- sapply ( cross , length )
				if ( any ( crosslength < 1 ) ) cross <- cross[!(addlength < 1)]
				if ( length ( cross ) < 1 ) cross <- NULL
		}
	
		# environment fuer vektorartige Elemente aus add und cross, die spaeter wieder gesetzt werden
		ac.env <- new.env()
		
		# Elemente von add mit mehreren Elementen (Vektor) verarbeiten
		if ( !is.null ( add ) ) {
				addlen <- sapply ( add , length )
				if ( any ( addlen > 1 ) ) {
						
						# Elemente aufs Environment schieben und add modifizieren
						f6 <- function ( x , y ) {
								# fuer model data.frame nur die Elemente verbinden
								new.nam <- paste ( x , collapse = "." )
								# fuer ac.env, komplett mit Variablen-Name um uniqueness zu gewaehren
								full.new.nam <- paste ( c ( y , x ) , collapse = "." )
								paste0 ( "assign ( '" , full.new.nam , "' , add$" , y , " , env=ac.env ); add$" , y , " <- '" , new.nam , "'" )
						}
						do.2env <- mapply ( f6 , add[addlen > 1] , names ( add[addlen > 1] ) , SIMPLIFY = TRUE )
						eval ( parse ( text = do.2env ) )
				}
		}

		# Elemente von cross mit mehreren Elementen (gelistete Vektoren) verarbeiten
		if ( !is.null ( cross ) ) {
				# Elemente identifizieren, die Liste sind
				cross.is.list <- sapply ( cross , is.list )
				
				if ( any ( cross.is.list ) ) {
						# ueber die Listen-Elemente schleifen
						f9 <- function ( l , lnam ) {
								# Elemente aufs Environment schieben
								f10 <- function ( x , y , nr ) {
										# fuer model data.frame nur die Elemente verbinden
										new.nam <- paste ( x , collapse = "." )
										# fuer ac.env, komplett mit Variablen-Name um uniqueness zu gewaehren
										full.new.nam <- paste ( c ( y , x ) , collapse = "." )
										list ( "do" = paste0 ( "assign ( '" , full.new.nam , "' , cross$" , y , "[[",nr,"]] , env=ac.env )" ) , "new.nam" = new.nam )
								}
								ret <- mapply ( f10 , l , lnam , seq(along=l) , SIMPLIFY = FALSE )
								
								# Rueckgabe
								c ( 
									# ausfuehren Elemente aufs Environment schieben
									sapply ( ret , "[[" , 1 ) ,
									# cross modifizieren
									paste0 ( "cross$" , lnam , " <- c(" , paste ( paste0 ( "'" , unname ( sapply ( ret , "[[" , 2 ) ) , "'") , collapse = "," ) ,")" )
								  )
						}
						do.2env <- do.call ( "c" , mapply ( f9 , cross[cross.is.list] , names ( cross[cross.is.list] ) , SIMPLIFY = FALSE ) )
						eval ( parse ( text = do.2env ) )
				}
		}
		
		### cross Elemente zum reinkreuzen vorbereiten
		if ( !is.null ( cross ) ) {
				cr.l <- mapply ( function ( d , n ) {d <- data.frame ( d , stringsAsFactors = FALSE ); colnames ( d ) <- n; return ( d )} , cross , names ( cross ) , SIMPLIFY = FALSE )
				cr <- Reduce(function(x, y) merge(x, y, all=TRUE,by=NULL),rev(cr.l),accumulate=FALSE )
		} else {
				cr <- NULL
		}

		### add Elemente zum reinkreuzen vorbereiten
		if ( !is.null ( add ) ) {
				ad.l <- mapply ( function ( d , n ) {d <- data.frame ( d , stringsAsFactors = FALSE ); colnames ( d ) <- n; return ( d )} , add , names ( add ) , SIMPLIFY = FALSE )
				ad <- Reduce(function(x, y) merge(x, y, all=TRUE,by=NULL),rev(ad.l),accumulate=FALSE )
		} else {
				ad <- NULL
		}		

		# Modelle
		m.l <- list(cr,ad,p.dfr,i.dfr)
		m.l <- m.l [ ! sapply ( m.l , is.null ) ]
		m <- Reduce(function(x, y) merge(x, y, all=TRUE,by=NULL),m.l,accumulate=FALSE )
		m <- m [ , rev ( colnames ( m ) ) , drop = FALSE ]
		
		# Ausgabe wie viele Modelle generiert werden
		if ( verbose ) {

				# wenn zu viele Modelle werden noch zusaetzlich - gebraucht
				zus <- ""
				# MH 5.12.16: versteh ich nicht was das soll, auskommentiert
				# if ( nrow ( m ) > 31 ) zus <- paste(rep("-", nrow ( m ) - 31 - nchar ( as.character ( nrow ( m ) ) ) ),collapse="")
				out.str <- paste0 ( "-------------------------------",paste(rep("-",nchar ( as.character ( nrow ( m ) ) )),collapse=""),zus,"\nsplitModels: generating " , nrow ( m ) , " models\n" )
				cat ( out.str )
				flush.console()
		}

		# Modellname
		if ( full.model.names & !is.null ( model.name.elements ) ) {

				# Elemente aus denen Modellname gebildet werden soll raussuchen
				fx <- function ( el , add , cross ) {
						if ( el %in% c("add","cross") ) eval ( parse ( text = paste0 ( "names ( ",el," )" ) ) ) else el
				}
				mne <- unname ( do.call ( "c" , sapply ( model.name.elements, fx , add , cross , simplify = FALSE ) ) )
				
				# Datensatz reduzieren
				mn <- m[,colnames ( m ) %in% mne , drop = FALSE ]
				# sortieren
				mn <- mn[,mne,drop=FALSE]
				
				# Namen erzeugen
				f4 <- function ( z ) {
						z <- z[!z %in% ""]
						if ( include.var.name ) z <- paste0 ( names ( z ) , "." , z )
						paste ( gsub ( "\\s" , "" , z ) , collapse = "__" )
				}
				m$model.name <- apply ( mn , 1 , f4 )

				# Modell-Name ggf. unique machen
				if ( any ( duplicated ( m$model.name ) ) ) m$model.name <- make.unique ( m$model.name )
				
				# Subpath erzeugen
				f5 <- function ( z ) {
						z <- z[!z %in% ""]
						if ( include.var.name ) z <- paste0 ( names ( z ) , "." , z )						
						x <- as.character ( c( "." , unname ( gsub ( "\\s" , "" , z ) ) ) )
						return ( eval ( parse ( text = paste0 ( "file.path ( " , paste ( paste0("'",x,"'") , collapse = " , " ) , ") " ) ) ) )
				}
				m$model.subpath <- apply ( mn , 1 , f5 )

				# Subpfade ggf. unique machen
				if ( any ( duplicated ( m$model.subpath ) ) ) m$model.subpath <- make.unique ( m$model.subpath )			
				
		} else {
				m$model.name <- paste0 ( "model" , formatC ( seq ( along = rownames ( m ) ) , format = "fg" , width = nchar ( as.character ( nrow ( m ) ) ) , flag = "0" ) )

				if ( nrow ( m ) > 1 ) {
						m$model.subpath <- file.path ( "." , m$model.name )
				} else {
						m$model.subpath <- "."
				}
		}
				
		# Modellname muss vorhanden sein (sonst geht Listenerstellung schlecht)
		if ( any ( abc <- m$model.name %in% "" ) ) {
				m$model.name[abc] <- paste0 ( "model" , formatC ( seq ( along = abc ) , format = "fg" , width = nchar ( as.character ( length ( abc ) ) ) , flag = "0" ) )
		}
				
		# Modell-Nr (=Listen-Index)
		m$model.no <- as.integer ( seq ( along = rownames ( m ) ) )
		
		# Anzahl Dimensionen
		m$Ndim <- as.integer ( unname ( sapply ( m$dim , function ( dim , i ) length ( i[[dim]] ) - 1 , i ) ) )
		m$Ndim[ m$Ndim < 1 ] <- NA
		
		# Anzahl Gruppen
		m$Ngroup <- as.integer ( unname ( sapply ( m$group , function ( group , p3 ) length ( p3[[group]] ) - 1 , p3 ) ) )
		m$Ngroup[ m$Ngroup < 1 ] <- NA
		
		# Modell-Datensatz Spalten sortieren
		vorn <- c ( "model.no" , "model.name" , "model.subpath" , "dim" , "Ndim" , "group" , "Ngroup" )
		m <- m[,c(vorn,colnames(m)[!colnames(m) %in% vorn]),drop=FALSE]
		
		# Return-Objekt bauen
		r <- list ()
		
		f3 <- function ( z , env , cross , add , include.var.name) {

				# Ausgabe eines Punktes
				if ( verbose ) {
						out.str <- paste0 ( "." )
						cat ( out.str )
						flush.console()
				}
		
				# NULL in abhaengig von env
				if ( env ) {
						NULL.char <- "NULL"
				} else {
						NULL.char <- "list(NULL)"
				}
				
				# NULL setzen wenn nicht da
				if ( z["dim"] %in% "" ) {
						ig <- NULL.char 
						di <- NULL.char
				} else {
						ig <- paste0 ( "i$" , z["dim"] )
						di <- z["dim"]
				}
				if ( z["group"] %in% "" ) {
						pg <- NULL.char
						gr <- NULL.char
				} else {
						pg <- paste0 ( "p3$'" , z["group"], "'" )
						gr <- z["group"]
				}
						
				if ( is.na ( z["Ndim"] ) ) nd <- NULL.char else nd <- z["Ndim"]
				if ( is.na ( z["Ngroup"] ) ) ng <- NULL.char else ng <- z["Ngroup"]
				
				# wenn cross oder add nicht NULL, muessen character eintraege gequotet werden
				# und Typ richtig gemacht
				if ( !is.null ( cross ) | !is.null ( add ) ) {
						notnum <- !sapply ( c ( cross , add ) , is.numeric )
						quotes <- sapply ( notnum , function ( x ) if ( x ) "'" else "" )
						as.vorn <- sapply ( c ( cross , add ) , function ( x ) paste0 ( " as." , class ( x ),"(" ) )
						as.hinten <- ") "
				}
				
				if ( !env ) {
						ret <- 	c ( paste0 ( "r$'" , z["model.name"] , "'$model.no <- as.integer(",z["model.no"],")" ) ,
									paste0 ( "r$'" , z["model.name"] , "'$model.name <- '",z["model.name"],"'" ) ,
									paste0 ( "r$'" , z["model.name"] , "'$model.subpath <- '",z["model.subpath"],"'" ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(di%in%NULL.char,"","["),"['dim']",ifelse(di%in%NULL.char,"","]")," <- ",ifelse(di%in%NULL.char,"","'"),di,ifelse(di%in%NULL.char,"","'") ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(nd%in%NULL.char,"","["),"['Ndim']",ifelse(nd%in%NULL.char,"","]")," <- ",ifelse(nd%in%NULL.char,"","as.integer("),nd,ifelse(nd%in%NULL.char,"",")"),"" ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(gr%in%NULL.char,"","["),"['group']",ifelse(gr%in%NULL.char,"","]")," <- ",ifelse(gr%in%NULL.char,"","'"),gr,ifelse(gr%in%NULL.char,"","'") ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(ng%in%NULL.char,"","["),"['Ngroup']",ifelse(ng%in%NULL.char,"","]")," <- ",ifelse(ng%in%NULL.char,"","as.integer("),ng,ifelse(ng%in%NULL.char,"",")"),"" ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(ig%in%NULL.char,"","["),"['qMatrix']",ifelse(ig%in%NULL.char,"","]")," <- ",ig,"" ) ,
									paste0 ( "r$'" , z["model.name"] , "'",ifelse(pg%in%NULL.char,"","["),"['person.grouping']",ifelse(pg%in%NULL.char,"","]")," <- ",pg,"" ) )
									
						# die Sachen aus cross/add setzen
						if ( !is.null ( cross ) | !is.null ( add ) ) {

								f7 <- function ( x , q , as.vorn , as.hinten , z , include.var.name ) {
										# checken ob im ac.env
										# check.name <- ifelse ( include.var.name , z[x] , paste ( c( x , z[x] ) , collapse = "." ) )
										check.name <- paste ( c( x , z[x] ) , collapse = "." )
										is.in.env <- check.name %in% ls ( ac.env )

										# setzen, entweder aus ac.env oder als Skalar
										if ( is.in.env ) {
												ret <- paste0 ( "r$'" , z["model.name"] , "'$'" , x , "' <- get ( '",check.name,"' , pos = ac.env )" )
										} else {
												ret <- paste0 ( "r$'" , z["model.name"] , "'$'" , x , "' <- ", as.vorn , q , z [ x ] , q , as.hinten )
										}
										return ( ret )
								}
								
								ret <- c (	ret ,
											mapply ( f7 , names ( c ( cross , add ) ) , quotes , as.vorn, as.hinten, MoreArgs = list ( z , include.var.name ) )
										  )
						}
									
				} else {
						ret <-  c (	paste0 ( "r$'" , z["model.name"] , "' <- new.env()" ) ,
									paste0 ( "assign ( 'model.no' , as.integer(" , z["model.no"] , ") , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'model.name' , '" , z["model.name"] , "' , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'model.subpath' , '" , z["model.subpath"] , "' , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'dim' , " , ifelse(di%in%NULL.char,"","'") , di , ifelse(di%in%NULL.char,"","'") , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'Ndim' , " ,ifelse(nd%in%NULL.char,"","as.integer(") , nd , ifelse(nd%in%NULL.char,"",")") , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'group' , " , ifelse(gr%in%NULL.char,"","'") , gr , ifelse(gr%in%NULL.char,"","'") , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'Ngroup' , " , ifelse(ng%in%NULL.char,"","as.integer(") , ng , ifelse(ng%in%NULL.char,"",")") , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'qMatrix' , " , ig , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'person.grouping' , " , pg , " , pos = r$'" , z["model.name"] , "' ) " ) )
	
						# die Sachen aus cross/add setzen
						if ( !is.null ( cross ) | !is.null ( add ) ) {

								f8 <- function ( x , q , as.vorn , as.hinten , z , include.var.name ) {
										# checken ob im ac.env
										# check.name <- ifelse ( include.var.name , z[x] , paste ( c( x , z[x] ) , collapse = "." ) )
										check.name <- paste ( c( x , z[x] ) , collapse = "." )
										is.in.env <- check.name %in% ls ( ac.env )

										# setzen, entweder aus ac.env oder als Skalar
										if ( is.in.env ) {
												# ret <- paste0 ( "r$'" , z["model.name"] , "'$'" , x , "' <- get ( '",check.name,"' , pos = ac.env )" )
												ret <- paste0 ( "assign ( '" , x , "' , get ( '",check.name,"' , pos = ac.env ) , pos = r$'", z["model.name"] , "' ) " )
										} else {
												ret <- paste0 ( "assign ( '" , x , "' , " , as.vorn,  q , z [ x ] , q , as.hinten , " , pos = r$'", z["model.name"] , "' ) " )
										}
										return ( ret )
								}
								
								ret <- c (	ret ,
											mapply ( f8 , names ( c ( cross , add ) ) , quotes , as.vorn, as.hinten, MoreArgs = list ( z , include.var.name ) )
										  )
						}	
	
				}
				return ( ret )
		}
		do3 <- unname ( sapply ( apply ( m , 1 , f3 , env , cross , add , include.var.name ) , c ) )
		eval ( parse ( text = do3 ) )

		# Modell-Dataframe noch an Rueckgabe ranhaengen
		# Leerstrings zu NA
		do.leer <- paste0 ( "m$" , colnames(m) , "[m$", colnames(m) , " %in% ''] <- NA" )
		eval ( parse ( text = do.leer ) )

		# anhaengen
		r <- list ( "models" = m , "models.splitted" = r , "nCores" = chooseCores( cores = nCores, GBcore = GBcore, max.cores = nrow(m) ) )
		
		# Ausgabe auf console
		if ( verbose ) {
				out.str <- paste0 ( "\nsee <returned>$models\nnumber of cores: ",r$nCores,"\n-------------------------------",paste(rep("-",nchar ( as.character ( nrow ( m ) ) )),collapse=""),zus,"\n" )
				cat ( out.str )
				flush.console()
		}
		
		return ( r )
}
