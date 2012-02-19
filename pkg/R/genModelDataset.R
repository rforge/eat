####################################################################################################################
#
# function:
#     genModelDataset ( item.grouping , person.grouping , dataset, id.name ,
#                        keep = NULL , allNAdelete = TRUE )
#
# description:
#     erstellt aus einem Datensatz einen Sub-Datensatz, der gewählte Item- und
#     Personengruppen enthält
#
# arguments:
#     item.grouping (data.frame)   :  Itemnamen (1. Spalte), Gruppe 1-N (1. - N. Spalte, enthält "1"/"0" für
#                                      "Item in n. Gruppe"/"Item nicht in n. Gruppe")
#     person.grouping (data.frame) :  Personen-ID (1. Spalte), Gruppe 1-K (1. - K. Spalte, enthält "1"/"0" für
#                                      "Person in k. Gruppe"/"Person nicht in k. Gruppe")
#     dataset (data.frame)         :  Datensatz mit ID-Variable, HG-Variablen und Items
#	    id.name (string)             : Name der ID-Variable
#	    keep (Vektor mit Spaltennamen) : Variablen die zusätzlich im Datensatz bleiben sollen
#                                     (z.B. dif/regression Variablen)
#     allNAdelete : löschen von Items und Personen, die komplett NA haben, default ist TRUE
#
#  TODO
#	  [1] delete.rowNA.on.items (logical)
#		  löschen von Personen die komplett missing auf Testitems
#	  [2] überprüfen/überlegen ob allNAdelete notwendig zu parametrisieren
#		  an sich leere Spalten müssen immer raus
#		  
#
#
# Version: 	0.3.0
# Imports:
# Published:
# Author:   Karoline Sachse, Martin Hecht
# Status: alpha
# Maintainer:
#
# Change Log:
# 2012-01-25 KS
# CHANGED: print only itemnames that are not in item.grouping
# 2011-12-20 MH
# CHANGED: removeNArows replaced by rmNArows
# 0000-00-00 AA
# 14.10.2011 MH: Ausgaben auf Englisch
# 17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 12.08.2011 MH: drop=FALSE überall wo relevant/wichtig geaddet
#				 .check.any.items.or.rows.left eingefügt
#				 .which.list.element.is.null eingefügt
# 11.08.2011 MH: Items ohne Varianz raus
# 09.08.2011 MH: spezifische missing-rule Datasets integriert
# 09.06.2011 (KS):
#
####################################################################################################################

genModelDataset <- function( item.grouping , person.grouping , mis.rule , datasets , 
							  id.name , keep = NULL , allNAdelete = TRUE  ) {

							  
	vers.nr <- "0.2.0"
	f.n <- paste("genModelDataset_", vers.nr , ":" ,sep ="" )
	# cat( paste ( f.n , "Beginne Erstellen von modellspezifischem Datensatz .\n") )
	cat( paste ( f.n , "Creating model specific dataset .\n") )
	
	# missing-rule spezifischen Datensatz setzen
	dataset <- datasets [[ collapseMissings.create.recode.string ( mis.rule ) ]]
	
	# Plausicheck id.name
	stopifnot ( length ( id.name ) == 1 )
	stopifnot ( is.character ( id.name ) )
	stopifnot ( id.name %in% colnames ( dataset ) ) 


	
	# Plausicheck keep
	if ( !is.null ( keep ) ) stopifnot ( is.character ( keep ) )
	# kein Leistungsitem (was auch in grouping drin ist) und auch nicht die ID Variable über keep reinschmuggeln
	stopifnot ( !any (c(item.grouping[,1], id.name) %in% keep))
	if ( !is.null ( keep ) ) stopifnot ( all (keep %in% colnames(dataset)))
	
	# Plausichecks item.grouping
	stopifnot ( class ( item.grouping ) == "data.frame" )
	stopifnot ( ncol ( item.grouping ) > 1 & nrow ( item.grouping ) > 1 )
	stopifnot ( any (item.grouping[,1] %in% colnames(dataset) ) )
	
	# item.grouping in Format chr num num
	if( any( unname(unlist(lapply(item.grouping, class))) != c("character",  rep("numeric", dim(item.grouping)[2]-1)))) {
		item.grouping <- set.col.type( item.grouping , list ( "character" = names(item.grouping)[1] , "numeric" = names(item.grouping)[2:dim(item.grouping)[2]] ))
		item.grouping[,-1] <- apply ( item.grouping[,-1,drop=FALSE] , 2 , function ( spalte ) {
			if (all(spalte %in% c(1,2))) {spalte <- spalte - 1}
		} )
	}
	
	# Plausicheck auf Dichotomie
	temp <- apply ( item.grouping[,-1,drop=FALSE] , 2 , function ( spalte ) {
			stopifnot ( spalte %in% c(0,1) )
	} )

	# Plausicheck auf unique Bezeichner
	stopifnot ( ! any ( duplicated ( item.grouping[,1] ) ) )
	
	# Plausichecks person.grouping
	stopifnot ( class ( person.grouping ) == "data.frame" )
	stopifnot ( ncol ( person.grouping ) > 1 & nrow ( person.grouping ) > 1 )
	stopifnot ( any (person.grouping[,1] %in% dataset[,id.name] ) )
	
	# Plausicheck auf Dichotomie
	temp <- apply ( person.grouping[,-1,drop=FALSE] , 2 , function ( spalte ) {
			stopifnot ( spalte %in% c(0,1) )
	} )

	# Plausicheck auf unique Bezeichner
	stopifnot ( ! any ( duplicated ( person.grouping[,1] ) ) )

	# Plausichecks data.frame
	stopifnot ( class ( dataset ) == "data.frame" )
	stopifnot ( !identical ( dataset , data.frame() ) )
	
	# Übereinstimmung Items in Gruppierungsinfo und Datensatz
	if ( setequal( item.grouping[,1], names(dataset) )) {
		# cat(paste("genModelDataset_", vers.nr, ": Alle Items aus Gruppierungsinfo in Datenstz enthalten und andersrum. \n", sep="")) } else {
		cat(paste("genModelDataset_", vers.nr, ": All items in grouping info in dataset and vice versa. \n", sep="")) } else {
	  if(length(setdiff(item.grouping[,1], names(dataset))) != 0) {
		  # cat(paste("genModelDataset_", vers.nr, ": Folgende Variablen aus item.grouping nicht in Datensatz: \n", sep=""))
		  cat(paste("genModelDataset_", vers.nr, ": Variables in item.grouping not in dataset: \n", sep=""))
		  cat(paste( paste ( idiff <- setdiff(item.grouping[,1], names(dataset) ) , collapse=" ," ) , "\n", sep=""))
		  ## theoretisch könnte es gut sein, hier diese aus item.grouping zu entfernen
		  ## kann u.U. bugs vermeiden , deshalb rein (bitte jemand checken)
		  item.grouping <-  item.grouping [ ! item.grouping[ , 1] %in% idiff , ]
		  rm ( idiff )
		}
	  
	  if(length(setdiff( ( items <- colnames ( dataset ) [ which ( ! colnames ( dataset ) %in% c ( id.name , keep ) ) ] ), item.grouping[,1])) != 0) {
		  # cat(paste("genModelDataset_", vers.nr, ": Folgende Variablen aus Datensatz nicht in item.grouping: \n", sep=""))
		  # cat(paste("genModelDataset_", vers.nr, ": Variables in dataset not in item.grouping: \n", sep=""))
		  # cat(paste( 
		  idiff <- setdiff(names(dataset), item.grouping[,1]) #), "\n")
		  # kommen wirklich alle Kontextvariablen über keep rein?
		  if ( any ( welche <-  items %in% idiff ) ) {
				# cat ( paste ( f.n , "Folgende Variablen aus Datensatz sind Items und nicht in item.grouping:\n" ) )
				cat ( paste ( f.n , "These variables in dataset are neither in item.grouping nor in context.vars:\n" ) )
				cat ( paste ( items [ welche ] , collapse = ", " ) )
				cat ( "\n" )
			}
		  rm ( items )
  		}
	}
	
	# Übereinstimmung Personen in Gruppierungsinfo und Datensatz
	if ( setequal(dataset[,id.name], person.grouping[,1])) {
      # cat("genModelDataset_", vers.nr, ": Alle Personen aus Gruppierungsinfo in Datensatz enthalten und andersrum. \n", sep="")}  else {
      cat("genModelDataset_", vers.nr, ": All persons in grouping info in dataset and vice versa. \n", sep="")}  else {
      if(length(setdiff(person.grouping[,1], dataset[,id.name])) != 0) {
          # cat(paste("genModelDataset_", vers.nr, ": Folgende Fälle aus person.grouping nicht in Datensatz: \n", sep=""))
          cat(paste("genModelDataset_", vers.nr, ": Cases in person.grouping not in dataset: \n", sep=""))
          cat(paste(  pdiff <- setdiff(person.grouping[,1], names(dataset))  , "\n", sep=""))
		  ## theoretisch könnte es gut sein, hier diese aus person.grouping zu entfernen
		  ## kann u.U. bugs vermeiden , deshalb rein (bitte jemand checken)
		  person.grouping <-  person.grouping [ ! person.grouping[ , 1] %in% pdiff , ]
	  }
	  if(length(setdiff(dataset[,id.name], person.grouping[,1])) != 0) {
          # cat(paste("genModelDataset_", vers.nr, ": Folgende Fälle aus Datensatz nicht in person.grouping: \n", sep=""))
          cat(paste("genModelDataset_", vers.nr, ": Cases in dataset not in person.grouping: \n", sep=""))
          cat(paste(paste(setdiff(dataset[,id.name], person.grouping[,1]),collapse=", "),"\n", sep="")) }
       }

	# zu behaltende Items bestimmen     -->> Warnung, welche Items rausgeworfen werden
	keep.items <- item.grouping [ which ( apply ( item.grouping , 1 , function ( zeile ) {
				if ( any ( zeile[-1] == 1 ) ) return ( TRUE ) else return ( FALSE )
			} ) ) , 1 ]
			
	# manuelle gesetzte Items dazu
	keep.items <- c ( id.name , keep , keep.items )
	keep.items.cols <- which ( colnames ( dataset ) %in% keep.items )

	# "löschen" der nicht benötigten Items
	dataset <- dataset [ , keep.items.cols, drop=FALSE ]
	if ( .check.any.items.or.rows.left ( dataset , c ( id.name, keep ) ) ) return ( NULL )
	
 	# zu behaltende Personen bestimmen
	keep.person <- person.grouping [ which ( apply ( person.grouping , 1 , function ( zeile ) {
				if ( any ( zeile[-1] == 1 ) ) return ( TRUE ) else return ( FALSE )
			} ) ) , 1 ]
	if ( .check.any.items.or.rows.left ( dataset ) ) return ( NULL )
			
	# "löschen" der nicht benötigten Personen
	dataset <- dataset [ dataset[,id.name] %in% keep.person , , drop=FALSE ] 
	
	# all-NA-Check: Items mit komplett NA identifizieren, ggf. löschen
	ind.col <- unname ( which ( colMeans(is.na(dataset)) == 1 ) )

### TEST (SW): Hier geschieht bei mir ein Fehler: Wenn keine Items mit komplett NA, gibt obere Zeile
### ein "named integer(0)" zurück. Folglich wird versucht, 0 Variablen ohne gültige Werte zu löschen,
### was zu einem dataframe mit 0 Spalten führt
### Zu beheben mit

	ind.col <- as.numeric( unname ( which ( colMeans(is.na(dataset)) == 1 ) ) )
	if ( identical ( ind.col , numeric(0) ) ) {
				# cat(paste("genModelDataset_", vers.nr, ": Alle Variablen haben gültige Werte. \n", sep=""))
				cat(paste("genModelDataset_", vers.nr, ": All variables have valid values. \n", sep=""))

### Ende Korrektur


	} else { 		
				# cat( paste("genModelDataset_", vers.nr,  ": Folgende Variablen völlig ohne gültige Werte: \n" ,
				cat( paste("genModelDataset_", vers.nr,  ": Variables completely without valid values: \n" ,
				paste( colnames(dataset)[ind.col] , collapse= ", "), sep="") )
				if ( allNAdelete ) {
						dataset <- dataset [ , -ind.col , drop=FALSE ] 
						# cat ( paste ( "\n Diese" , length ( ind.col ) , "Variablen wurden gelöscht. \n" ) )
						cat ( paste ( "\n These" , length ( ind.col ) , "variables have been deleted. \n" ) )
				}
		}
	if ( .check.any.items.or.rows.left ( dataset , c ( id.name, keep ) ) ) return ( NULL )

	# all-NA-Check: Personen mit komplett NA identifizieren, ggf. löschen
	ind.row <- unname ( which ( rowMeans(is.na(dataset[,-which( colnames(dataset) %in% c(id.name, keep) ) ])) == 1 ) )
	if ( identical ( ind.row , integer(0) ) ) {
				# cat(paste("genModelDataset_", vers.nr, ": Alle Fälle haben gültige Werte. \n", sep=""))
				cat(paste("genModelDataset_", vers.nr, ": All cases have valid values. \n", sep=""))
	} else { 		
				# cat( paste( "genModelDataset_", vers.nr, ": Folgende Fälle völlig ohne gültige Werte (außer ggf. auf keep-Variablen): \n" ,
				cat( paste( "genModelDataset_", vers.nr, ": Cases completely without valid values (excpept maybe in keep-variables): \n" ,
				paste( dataset[ ind.row , id.name ] , collapse= ", "), sep=""))
				if ( allNAdelete ) {

						dataset <- dataset [ -ind.row , ] 
						
						# cat ( paste ( "\n Diese" , length ( ind.row ) , "Fälle wurden gelöscht. \n" ) )
						cat ( paste ( "\n These" , length ( ind.row ) , "cases have been deleted. \n" ) )
				}
			}
	if ( .check.any.items.or.rows.left ( dataset ) ) return ( NULL )
			
	# Variablen ohne Varianz raus
	item.names <- item.grouping [ , 1 ][ which ( item.grouping [ , 1 ] %in% colnames ( dataset ) ) ]
	delete <- NULL
	delete <- unname ( mapply ( function ( spalte , item.names ) {
				
					if ( length ( unique ( na.omit ( spalte ) ) ) == 1 ) TRUE else FALSE
					
			} , dataset[,item.names] , item.names , SIMPLIFY = TRUE ) )
	delete <- as.character ( item.names[ delete ] )
	
	if ( ! length ( delete ) == 0 ) {
			# cat ( paste ( f.n , "Info: Variable(n)" , paste ( delete , collapse = ", " ) , "im Datensatz ist eine Konstante und wird gelöscht.\n" ) ) 				
			cat ( paste ( f.n , "Info: Variable(s)" , paste ( delete , collapse = ", " ) , "in dataset is/are constant and deleted.\n" ) ) 				
			dataset <- dataset [ , - which ( colnames ( dataset ) %in% delete ) , drop=FALSE ]
	}
	if ( .check.any.items.or.rows.left ( dataset , c ( id.name, keep ) ) ) return ( NULL )
	
	# kann sein dass neue NArows, diese löschen
	if (allNAdelete) dataset <- rmNArows ( dataset )
	if ( .check.any.items.or.rows.left ( dataset ) ) return ( NULL )
	
	
	# Ausgabe
	# if(nrow(dataset) > 0) cat(paste("genModelDataset_", vers.nr, ": Datensatz erfolgreich erstellt. \n"))	
	if(nrow(dataset) > 0) cat(paste("genModelDataset_", vers.nr, ": dataset successfully created. \n"))	
	# mittlerweile / in dieser Version obsolet
	stopifnot ( nrow ( dataset ) > 0 )
	stopifnot ( ncol ( dataset ) > 0 )
	
	return ( dataset )
  
}

# TEST
#setwd('c:/temp')
#load('genModelDatasetBsp.Rdata')
# keep <- names(dataset)[2:11]
#keep = c ( "bl" , "sf" )
#ret <- genModelDataset ( item.grouping=item.grouping , person.grouping=person.grouping , dataset=list(dataset) , id.name = "idstud", keep=keep, mis.rule=list ( mvi = 0 , mnr = 0 , mci = 0 , mbd = NA , mir = 0 , mbi = 0 ) )
