# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.create.misrule.datasets
# Description: Subroutine von automateModels
# Version: 	0.4.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: write.txt.dataset geaddet
#					   wenn TRUE wird ascii und Excel geschrieben
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.create.misrule.datasets <- function ( dataset , items , items.list , mis.rule , folder , write.txt.dataset ) {

	# Funktionsname für Meldungen
	f. <- ".automateModels.create.misrule.datasets"
	f.n <- paste ( f. , ":" , sep = "" )

	# Ausgabe
	flush.console()
	# sunk ( paste ( f.n , "missing-rule-spezifische Datensätze werden erzeugt\n" ) ); flush.console()
	sunk ( paste ( f.n , "missing-rule-specific datasets are being generated\n" ) ); flush.console()
	
	# unique missing.rules
	mis.rule.unique <- unique ( mis.rule )

	# unique missing.rules Names
	mis.rule.unique.names <- mapply ( function ( missing.rule ) {
			collapseMissings.create.recode.string ( missing.rule )
	} , mis.rule.unique , SIMPLIFY = FALSE )
	
	# Analysen nach missing.rule belabeln
	names ( items.list ) <- mapply ( function ( missing.rule ) {
			collapseMissings.create.recode.string ( missing.rule )
	} , mis.rule , SIMPLIFY = TRUE )

	# nach missing.rule Items zusammen
	mis.rule.items <- mapply ( function ( missing.rule , items.list ) {
			unique ( unname ( unlist ( items.list[which ( names ( items.list ) == missing.rule )] ) ) )
	} , mis.rule.unique.names , MoreArgs = list ( items.list ) , SIMPLIFY = FALSE )
	names ( mis.rule.items ) <- mis.rule.unique.names

	# Datensätze reduzieren
	datasets.red <- mapply ( function ( missing.rule , mis.rule.items , dataset , items ) {
			delete <- items [ which ( ! items %in% mis.rule.items ) ]
			if ( ! identical ( delete , character(0) ) ) {
					ret <- dataset [ , - which ( colnames ( dataset ) %in% delete ) ]
			} else ret <- dataset
			return ( ret )
	} , mis.rule.unique.names , mis.rule.items , MoreArgs = list ( dataset , items ) , SIMPLIFY = FALSE )	
	names ( datasets.red ) <- mis.rule.unique.names

	# Missings auf modellspezifischen Datensatz rekodieren
	datasets.collapsed <- mapply ( collapseMissings ,
				dat = datasets.red ,
				missing.rule = mis.rule.unique ,
				item.names = mis.rule.items ,
				SIMPLIFY = FALSE )	
	names ( datasets.collapsed ) <- mis.rule.unique.names

	# speichern
	.fun <- function ( ds , ds.name , folder ) {
			ds.name <- gsub ( "'" , "" , ds.name )
			ds.name <- gsub ( ";" , "" , ds.name )	
			ds.name <- gsub ( "=" , " " , ds.name )	
			ds.name <- gsub ( " " , "_" , ds.name )	
			
			# Rdata
			rdata <- file.path ( folder , ( n1 <- paste ( ds.name , ".Rdata" , sep="" ) ) )
			if ( file.exists ( rdata ) ) file.remove ( rdata )
			save ( ds , file = rdata )

			sunk ( paste ( "\t" , n1 , "\n" ) ); flush.console()

			if ( write.txt.dataset ) {
					# ascii
					ascii <- file.path ( folder , ( n2 <- paste ( ds.name , ".txt" , sep="" ) ) )
					if ( file.exists ( ascii ) ) file.remove ( ascii )
					write.table( ds , file = ascii , append = FALSE , quote = FALSE, sep = "\t",
					eol = "\n" , na = "", dec = ".", row.names = FALSE ,
					col.names = TRUE )
					sunk ( paste ( "\t" , n2 , "\n" ) ); flush.console()
					
					# da write.xlsx2 bei größeren Datensätzen crasht, Excel nur bis 5000 raus 
					if ( nrow ( ds ) < 5000 ) {
							xlsx <- file.path ( folder , ( n3 <- paste ( ds.name , ".xlsx" , sep="" ) ) )
							if ( file.exists ( xlsx ) ) file.remove ( xlsx )
							tried <- try ( 
										write.xlsx2 ( ds , file = xlsx ,
														sheetName="Sheet 1", formatTemplate=NULL,
														col.names=TRUE, row.names=FALSE, append=FALSE )
										, silent = TRUE )
							if ( inherits ( tried , "try-error" ) ) {
									# sunk ( paste ( "\t" , n3 , " konnte nicht geschrieben werden.\n" ) ); flush.console()
									sunk ( paste ( "\t" , n3 , " could not be written.\n" ) ); flush.console()
							
							} else {
									sunk ( paste ( "\t" , n3 , "\n" ) ); flush.console()
									}
					}
			}
			
			return ( TRUE )
	}

	temp <- mapply ( .fun , datasets.collapsed , names ( datasets.collapsed ) , folder ) 
	
	# returnen
	return ( datasets.collapsed )
				
}
