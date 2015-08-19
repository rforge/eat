
### Funktion:
# splatter

### Argumente:
# item.grouping     data.frame, erste Spalte muss item-ID sein, dann Dimensionen als Spalten mit Werten 0/1
# person.groups     data.frame, erste Spalte muss person-ID sein, dann Gruppen-Variablen als Spalten mit Gruppen-Kategorien als Werte
# split             character, was gesplittet werden soll
# all.persons       logical (default: TRUE), je Gruppen-Variable wird eine "alle"-Kategorie hinzugefügt
# all.persons.lab   character, Name der "alle"-Kategorie (nur relevant wenn all.persons TRUE ist)
# env               logical (defautl: FALSE), aendert Rueckgabe

### Rueckgabe:
# Liste mit zwei Eintraegen: 
# [1] data.frame mit Modell-Informationen model.no, model.name, model.subpath, item.grouping, person.grouping
# [2] Liste (Anzahl Listenelemente = Anzahl Modelle)
#     wenn env FALSE: Liste mit 4 Elementen: model.name, model.subpath, item.grouping, person.grouping
#     wenn env TRUE:  Liste mit environments die die 4 Objekte model.name, model.subpath, item.grouping, person.grouping beinhalten

splatter <- function ( item.grouping = NULL , person.groups = NULL , split = c ( "item.grouping" , "person.groups" ) , all.persons = TRUE , all.persons.lab = "all" , env = FALSE , verbose = TRUE ) {

		# potentielle TODOs:
				# item.grouping / person.group checks ob richtige Struktur und Plausibilitaet
		
		# Funktion: person.groups nach person.grouping
		pg2pgr <- function ( x , nam ) {
				d <- x[,1,drop=FALSE]
				eval ( parse ( text = paste0 ( "d$'" , nam , "' <- 1 " ) ) )
				return ( d )
		}

		# wenn kein data.frame, dann ignorieren
		if ( !is.null ( item.grouping ) & !is.data.frame ( item.grouping ) ) {
				item.grouping <- NULL
				warning ( paste0 ( "item.grouping is not a data.frame and will be ignored." ) )
		}
		if ( !is.null ( person.groups ) & !is.data.frame ( person.groups ) ) {
				person.groups <- NULL
				warning ( paste0 ( "person.groups is not a data.frame and will be ignored." ) )
		}
		
		# aus Split die Sachen raus, die nicht da sind
		if ( is.null ( item.grouping ) ) split <- split[!split %in% "item.grouping"]
		if ( is.null ( person.groups ) ) split <- split[!split %in% "person.groups"]

		# all.persons.lab checken ob bereits eine Kategorie in person.groups so heisst
		if ( !is.null ( person.groups ) ) {
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
										new.lab <- paste ( sample ( letters , 3 , replace = TRUE ) , collapse = "")
								}
						}
						# Warnung
						warning ( paste0 ( "'" , all.persons.lab , "' is already a used category in person.groups, it has been changed to '" , new.lab , "'." ) )
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
		
		
		# item.grouping
		if ( "item.grouping" %in% split & !is.null ( item.grouping ) ) {
				# item.grouping mit mehreren Dimensionen zu Liste von item.groupings mit nur einer Dimension
				i <- sapply ( colnames ( item.grouping )[-1] , function ( x , d ) { d <- d[,c(colnames(item.grouping)[1],x)]; d <- d[ d[,x] %in% 1 , ]; return ( d ) } , item.grouping , simplify = FALSE )
		} else if ( ! "item.grouping" %in% split & !is.null ( item.grouping ) ) {
				i <- list ( item.grouping )
		} else {
				i <- list ( item.grouping )
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
		
		### kreuzen 
		i.dfr <- data.frame ( "dim" = names ( i ) , stringsAsFactors = FALSE )
		p.dfr <- data.frame ( "group" = names ( p3 ) , stringsAsFactors = FALSE )

		# Modelle
		m <- merge ( p.dfr , i.dfr , by = NULL , sort = FALSE )
		m <- m [ , c ( "dim" , "group" ) ]

		# Ausgabe wie viele Modelle generiert werden
		if ( verbose ) {
				# wenn zu viele Modelle werden noch zusaetzlich - gebraucht
				zus <- ""
				if ( nrow ( m ) > 28 ) zus <- paste(rep("-", nrow ( m ) - 28 - nchar ( as.character ( nrow ( m ) ) ) ),collapse="")
				out.str <- paste0 ( "----------------------------",paste(rep("-",nchar ( as.character ( nrow ( m ) ) )),collapse=""),zus,"\nsplatter: generating " , nrow ( m ) , " models\n" )
				cat ( out.str )
		}
		
		# Modellname
		f4 <- function ( z ) {
				z <- z[!z %in% ""]
				paste ( z , collapse = "__" )
		}
		m$model.name <- apply ( m , 1 , f4 )
		
		# Modellname muss vorhanden sein (sonst geht Listenerstellung schlecht)
		if ( any ( m$model.name %in% "" ) ) m$model.name <- "model"
		
		# Subpath
		m$model.subpath <- "."
		if ( "item.grouping" %in% split ) m$model.subpath <- file.path ( m$model.subpath , m$dim )
		if ( "person.groups" %in% split ) m$model.subpath <- file.path ( m$model.subpath , m$group )
		
		# Modell-Nr (=Listen-Index)
		m$model.no <- seq ( along = rownames ( m ) )
		
		# Modell-Datensatz Spalten sortieren
		vorn <- c ( "model.no" , "model.name" , "model.subpath" , "dim" , "group" )
		m <- m[,c(vorn,colnames(m)[!colnames(m) %in% vorn])]
		
		# Return-Objekt bauen
		r <- list ()
		
		f3 <- function ( z , env ) {

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
				if ( z["dim"] %in% "" ) ig <- NULL.char else ig <- paste0 ( "i$" , z["dim"] )
				if ( z["group"] %in% "" ) pg <- NULL.char else pg <- paste0 ( "p3$" , z["group"] )
				if ( z["model.name"] %in% "" ) mn <- NULL.char else mn <- z["model.name"]
				if ( z["model.subpath"] %in% "" ) msp <- NULL.char else msp <- z["model.subpath"]
				
				if ( !env ) {
						ret <- 	c ( paste0 ( "r$'" , z["model.name"] , "'$model.name <- '",mn,"'" ) ,
									paste0 ( "r$'" , z["model.name"] , "'$model.subpath <- '",msp,"'" ) ,
									paste0 ( "r$'" , z["model.name"] , "'$item.grouping <- ",ig,"" ) ,
									paste0 ( "r$'" , z["model.name"] , "'$person.grouping <- ",pg,"" ) )
				} else {
						ret <-  c (	paste0 ( "r$'" , z["model.name"] , "' <- new.env()" ) ,
									paste0 ( "assign ( 'model.name' , '" , mn , "' , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'model.subpath' , '" , msp , "' , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'item.grouping' , " , ig , " , pos = r$'" , z["model.name"] , "' ) " ) ,
									paste0 ( "assign ( 'person.grouping' , " , pg , " , pos = r$'" , z["model.name"] , "' ) " ) )
				}
		}
		do3 <- unname ( sapply ( apply ( m , 1 , f3 , env ) , c ) )
		eval ( parse ( text = do3 ) )
		
		# Modell-Dataframe noch an Rueckgabe ranhaengen
		# Leerstrings zu NA
		do.leer <- paste0 ( "m$" , colnames(m) , "[m$", colnames(m) , " %in% ''] <- NA" )
		eval ( parse ( text = do.leer ) )
		# anhaengen
		r <- list ( "models" = m , "models.splitted" = r )
		
		# Ausgabe auf console
		if ( verbose ) {
				out.str <- paste0 ( "\nsee <returned>$models\n----------------------------",paste(rep("-",nchar ( as.character ( nrow ( m ) ) )),collapse=""),zus,"\n" )
				cat ( out.str )
		}
		
		return ( r )
}

### Tests ###
# set.seed ( 1234 )
# item.grouping <- data.frame ( "item" = 1:10 , "dim1" = sample ( c ( 0 , 1 ) , 10 , replace = TRUE ), "dim2" = sample ( c ( 0 , 1 ) , 10 , replace = TRUE ) , stringsAsFactors = FALSE )
# person.groups <- data.frame ( "idstud" = 1:10 , "group1" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ), "group2" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ) , stringsAsFactors = FALSE )

# l1 <- splatter ( item.grouping, person.groups )
# length(l1)

# l1b <- splatter ( item.grouping, person.groups , env = TRUE )
# str(l1b)
# ls ( l1b[[1]] )
# identical ( get ( "item.grouping" , l1b[[2]][[1]] ) , l1[[2]][[1]]$item.grouping )

# l1c <- splatter ( item.grouping, person.groups , all.persons = FALSE )
# length(l1c)

# l2 <- splatter ( item.grouping, person.groups , split = "item.grouping" )
# length(l2)

# l3 <- splatter ( item.grouping, person.groups , split = "person.groups" )
# length(l3)

# l4 <- splatter ( item.grouping, person.groups , split = NULL )
# str(l4)

# l5 <- splatter ( item.grouping = NULL, person.groups=person.groups )
# length(l5)
# str(l5)

# l5b <- splatter ( item.grouping = NULL, person.groups=person.groups, split = NULL )
# str(l5b)

# l5c <- splatter ( item.grouping = NULL, person.groups=person.groups, split = "item.grouping" )
# str(l5c)

# l5d <- splatter ( item.grouping = NULL, person.groups=person.groups, split = "person.groups" )
# str(l5d)

# l6 <- splatter ( item.grouping, person.groups=NULL )
# str(l6)

# l6b <- splatter ( item.grouping, person.groups=NULL, split = NULL)
# str(l6b)

# l6c <- splatter ( item.grouping, person.groups=NULL, split = "person.groups" )
# str(l6c)

# l6d <- splatter ( item.grouping, person.groups=NULL, split = "item.grouping" )
# str(l6d)

# l6e <- splatter ( item.grouping, person.groups=NULL, split = "item.grouping" , env = TRUE )
# str(l6e)
# ls ( l6e[[2]][[1]] )
# get ( "person.grouping" , l6e[[2]][[1]] )
# get ( "item.grouping" , l6e[[2]][[1]] )
# get ( "model.name" , l6e[[2]][[1]] )
# get ( "model.subpath" , l6e[[2]][[1]] )

# l7 <- splatter ( item.grouping=NULL, person.groups=NULL, split = NULL )
# str(l7)

# all.persons.lab checken
# l8 <- splatter ( item.grouping=NULL, person.groups=person.groups , all.persons.lab = "cat1" ) 
# person.groups2 <- data.frame ( "idstud" = 1:10 , "group1" = sample ( c ( "all" , "ALL" ) , 10 , replace = TRUE ), "group2" = sample ( c ( "_all_" , "_ALL_" ) , 10 , replace = TRUE ) , stringsAsFactors = FALSE )
# l9 <- splatter ( item.grouping=NULL, person.groups=person.groups2 ) 

# check factor person.groups
# person.groups3 <- data.frame ( "idstud" = 1:10 , "group1" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ), "group2" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ) )
# l10 <- splatter ( item.grouping=NULL, person.groups=person.groups3 ) 
# set.seed(123456)
# person.groups4 <- data.frame ( "idstud" = 1:10 , "group1" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ), "group2" = sample ( c ( "cat1" , "cat2" ) , 10 , replace = TRUE ) , "group3" = sample ( c ( "cat2" , "cat1" ) , 10 , replace = TRUE ) )
# person.groups4$group1 <- factor ( person.groups4$group1 , levels(person.groups4$group1)[ c(2,1) ] )
# person.groups4$group2 <- factor ( person.groups4$group2 , levels(person.groups4$group2)[ c(2,1) ] )
# person.groups4$group3 <- as.character ( person.groups4$group3 )
# l11 <- splatter ( item.grouping=NULL, person.groups=person.groups4 ) 

# Ausgabe checken
# person.groups5 <- data.frame ( "idstud" = 1:10 , "group1" = sample ( c ( "cat1" , "cat2" ,"cat3","cat4","cat5") , 10 , replace = TRUE ), "group2" = sample ( c ( "cat1" , "cat2" ,"cat3","cat4" ) , 10 , replace = TRUE ) , "group3" = sample ( c ( "cat2" , "cat1" ,"cat3" ) , 10 , replace = TRUE ), "group4" = sample ( c ( "cat2" , "cat1" ,"cat3" ) , 10 , replace = TRUE ) )
# l12 <- splatter ( item.grouping=NULL, person.groups=person.groups5 ) 

