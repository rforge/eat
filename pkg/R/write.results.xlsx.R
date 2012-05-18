####################################################################################################################
#
# writeResultsExcel
# schreibt Ergebnisse der Conquest Analysen die durch die Funktion readConquestOutput
#  eingelesen wurden in eine Excel Datei (.xlsx).
#
# Version: 	0.8.0
# Imports:
# Published:
# Author:  Malte Jansen, Christiane Penk, Sebastian Wurster
# Maintainer: Klaus & Nikolaus
#
# 2011-12-21 MH
# FIXED: out.name in write.results.xlsx
# 2011-12-15 MH
# FIXED: write q3 results in write.results.xlsx
# ADDED: gc() in write.results.xlsx
# 2011-12-12 MH
# FIXED: set stable and commented tests/examples in write.results.xlsx
# 0000-00-00 AA
# Change log:
#   26.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#   17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
#	0.2.0 : additional_itemprops eingefügt
#	0.0.4-0.0.5: Selektion und Subskala raus; Analysename und Gruppenname rein (erstmal als Platzhalter)
#   0.0.6 : ZKD-Ergebnisstruktur 1:1 übernommen; b.cent raus
#	0.1.0 : Funktionierende Version für eine Analyse
#	0.2.0 : Funktionierende Version für mehrere Analysen
#   0.3.2 : Personenkennwerte rausschreiben in Extra-Tabelle eingefügt
#   0.3.3 : Personenkennwerte jetzt mit PVs!! (ACHTUNG: Funktioniert nur für fünf PVs, muss ncoh angepasst werden)
#
####################################################################################################################
# Basis: ZKD-Ergebnisliste 
#	- Konzept: p:\ZKD\01_Organisation\Konzepte\ErgebnisStruktur_Konzept_05.xlsx
#	- Output von ReadConquestOutput 
#
#Ziel: 
# Es werden 2 Excel Dateien rausgeschrieben: 
#	1. Ergebnisse für Items mit zwei Tabellenblättern (Itemkennwerte, Kategorietrennschärfe)
#	2. Ergebnisse für Personen
#
#
# Input:
# results = ZKD-Ergebnisliste (Struktur siehe p:\ZKD\01_Organisation\Konzepte\ErgebnisStruktur_Konzept_05.xlsx)
# path = Folder wo die Excel-Tabelle hingespeichert werden soll
#
#Aktueller Stand: Itemkennwerte rausschreiben klappt für eine Analyse
#
# TODO
# Schleife über alle Analysen
# Schleife über alle Personengruppen
# nochmal write.xlsx 2 testen
# Kategorietrennschärfen rausschreiben
###write.xlsx(kategorie.matrix, paste(Pfad, name.analyse, ".xlsx", sep=""), sheetName = "Kategorietrennschaerfe", row.names = FALSE, append = TRUE)
# Personenkennwerte rausschreiben
# NAs als Leerstring statt "NV" in Ergebnistabelle
## 
#####################################################################################################################


write.results.xlsx.i <- function ( results , path , additional_itemprops=NULL ) {


		itemkennwerte  <- get.item.par(results)

		# Name des out-files setzen
		if ( ! ( l <- length ( names ( results ) ) ) == 1 ) out.name <- paste ( "All_" , l , "_analyses_items" , sep="" ) else out.name <- paste ( names ( results ) , "_items" , sep = "" )
		
		### rausschreiben
		# Excel
		write.xlsx2 ( itemkennwerte , file.path ( path , paste( out.name , ".xlsx", sep="") ), sheetName = "items", row.names = TRUE )
			
		# Rdata Frame
		save ( itemkennwerte , file = file.path ( path , paste( out.name , ".Rdata", sep="" ) ) )
		
		rm ( list = ls() )
		gc ( )
		return ( TRUE )
}

##Personenwerte als Extra-tabelle

write.results.xlsx.p <- function ( results , path ) {

		personenkennwerte <- get.person.par(results)

		# todo, weitere numeric Setzungen

		# Name des out-files setzen
		if ( ! ( l <- length ( names ( results ) ) ) == 1 ) out.name <- paste ( "All_" , l , "_analyses_persons" , sep="" ) else out.name <- paste ( names ( results ) , "_persons" , sep = "" )

		### rausschreiben
		# Excel
		write.xlsx2 ( personenkennwerte , file.path ( path , paste( out.name , ".xlsx", sep="") ), sheetName = "persons", row.names = TRUE )
			
		# Rdata Frame
		save ( personenkennwerte , file = file.path ( path , paste( out.name , ".Rdata", sep="" ) ) )
		
		rm ( list = ls() )
		gc ( )
		return ( TRUE )
}

write.results.xlsx.q3 <- function ( results , path ) {

		results.q3 <-  get.q3(results)
		
		# Name des out-files setzen
		if ( ! ( l <- length ( names ( results ) ) ) == 1 ) out.name <- paste ( "All_" , l , "_q3" , sep="" ) else out.name <- paste(names ( results ),"_q3",sep="")

		.fun1 <- function ( el , el.name , results.q3 , out.name , path ) {
				if ( !is.null ( el ) ) {
						# Bestimmung von append
						els.names <- names ( results.q3[ sapply ( results.q3 , function ( e ) { !is.null(e) } ) ] )
						if ( which ( els.names == el.name ) == 1 ) append = FALSE else append = TRUE

						sheetNames <- make.unique ( substr ( els.names , 1 , 20 ) )
						sheetName <- sheetNames[which ( els.names == el.name )]
						write.xlsx2 ( el , file.path ( path , paste( out.name , ".xlsx", sep="") ), sheetName = sheetName , row.names = TRUE , append=append)				
						q3 <- el
						save ( q3 , file = file.path ( path , paste( out.name , ".Rdata", sep="" ) ) )
						
						# q3 descriptives
						q3.descriptives <- q3.descriptives(q3)
						write.xlsx2 ( q3.descriptives , file.path ( path , paste( out.name , "_descriptives.xlsx", sep="") ), sheetName = "q3_descriptives" , row.names = FALSE , append=FALSE)
						save ( q3.descriptives , file = file.path ( path , paste( out.name , "_descriptives.Rdata", sep="" ) ) )
						
				}
				return(TRUE)
		}
		temp <- mapply (.fun1, results.q3 , names ( results.q3 ) , MoreArgs = list ( results.q3 , out.name , path ) )
	
		rm ( list = ls() )
		gc ( )
		return ( TRUE )
		
}

write.results.xlsx <- function ( results , path , additional_itemprops=NULL ) {

		write.results.xlsx.i ( results , path , additional_itemprops=NULL )
		write.results.xlsx.p ( results , path)
		write.results.xlsx.q3 ( results , path )
}



# TESTEN
##Test mit Beispieldaten (eine Analyse)
# load ('p:\\ZKD\\02_Beispieldaten\\BspResults02.Rdata')
# load ("t:\\Nawi\\Bsp27\\_automateModels_\\results.Rdata")	
# library ( xlsx )
# results <- BspResults02
# path <- "P:/ZKD/temp/13_ReadConquestResults"

# library(debug)
# mtrace(write.results.xlsx)
# write.results.xlsx (results, path)






