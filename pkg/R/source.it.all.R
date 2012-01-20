####################################################################################################################
#
# source.it.all
# sourced alle R-Skripte
#
# Version: 	0.2.0 (alpha)
# Depends: 	
# Imports: 	
# Published: 	2011-10-12
# Author: 	Christiane Penk , Martin Hecht
# Maintainer: 	Christiane Penk , Martin Hecht
#
# Change-Log
# 12.10.2011 MH: Option return.stable (logical, default: FALSE) zugefügt
#				 wenn TRUE wird character-Vektor mit stable R-Files zurückgeliefert
#				 Hinweis: Module in develop.modules werden (natürlich) exkludiert
# 31.05.2011 CP: die letzte freigegebene Version aus development (oder einem anderen Verzeichnis) sourcen
# 14.09.2010 MH: source.it.all() hinzugefügt, damit gleich gesourct wird
# 13.09.2010 MH: Entwicklung
#
####################################################################################################################

# in ZKD-Sitzung vom 24. Mai 2011 besprochen: 
# onlystable: nur freigegebene Versionen sourcen (s. Konventionen),
#			 sonst höchste Nummer sourcen
# develop.modules: wenn gesetzt, dann höchste Version (auch unstable)
#			Dateinamen
#			z.B. c( "get.wle" ) --> höchste Version
#			z.B. c( "get.wle_0.0.2" ) --> angegebene Version

# Subfunktion: sortiert Versionen absteigend, so dass höchste Version zurückgegeben wird
.source.it.all.sort <- function ( vect ) {

					# splitten am Punkt
					non.sorted <- cbind ( data.frame ( matrix ( as.numeric ( unlist ( strsplit ( vect , "." , fixed = TRUE ) ) ) , ncol = 3 , byrow = TRUE ) , stringsAsFactors = FALSE ) , vect , stringsAsFactors = FALSE )

					# sortieren
					sorted <- non.sorted [ order ( non.sorted$X1 , non.sorted$X2 , non.sorted$X3 , decreasing = TRUE ) , ]

					# zu sourcende Version 
					vers <- sorted [ 1 , 4 ]

					# returnen
					return ( vers )
}

# Funktion
source.it.all <- function ( folder="p:/ZKD/development" , develop.modules = NULL , return.stable = FALSE ) {
	
	# listet alle Dateien auf, die nach den Konventionen benannt sind
	files <- list.files(folder , all.files=TRUE , pattern = "^.*_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", full.names=FALSE )
	# erster Teil, also part1 (bis zum Unterstrich) wird abgesplittet
	part1 <- unlist ( strsplit(files, split="_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", fixed=FALSE ) )
	# zweiter Teil, also part2 (Versionsnummer) wird abgesplittet
	part2 <- 	unname ( mapply ( function ( files , part1 ) {
			substr(files, nchar(part1)+2 , nchar(files)-2 )
	} , files , part1 ) )
	
	# Plausicheck: Anzahl der Elemente muss in part1 und part2 gleich sein
	stopifnot ( length ( part1 ) == length ( part2 ) )
	
	# initialisieren, Sammlung von zu sourcenden files
	tosource <- NULL	
	
	# unique Part1 (mehrfache Dateinamen werden entfernt [also für die Funktion, von denen mehrere Versionsnummern im Ordner lagen])
	part1.names.unique <- unique ( part1 )
	
	### Handling von develop.modules
	if ( !is.null ( develop.modules ) ) {
			
			# Bezeichnung des ersten Teils der Datei (Funktionsname) abplitten 
			dev.vec.part1 <- unlist ( strsplit(develop.modules, split="_{1}\\d*\\.\\d*\\.\\d*$", fixed=FALSE ) )
			
			# Versionsnummer abplitten
			dev.vec.part2 <- unname ( mapply ( function ( develop.modules , dev.vec.part1 ) { 	substr(develop.modules, nchar(dev.vec.part1)+2 , nchar(develop.modules)-2 )
			} , develop.modules , dev.vec.part1 ) )
		
			# columns die so gesourct werden sollen, wie sie sind (mit Versionsnummer)
			as.is.col <- which ( !dev.vec.part2 == "" )
			tosource <- c ( tosource , develop.modules[ as.is.col ] )

			# wenn direkte Versionsnummer im develop.modules angegeben wurde, dann diese Funktionsnamen aus dem Vektor mit den uniquen Dateinamen entfernen
			if ( ! identical ( as.is.col , integer(0) ) ) part1.names.unique <- part1.names.unique [ - which ( part1.names.unique %in% dev.vec.part1 [ as.is.col ] ) ]
	
	}
	
	# für die restlichen Dateinamen (die nicht direkt gesourct werden sollten über develop.modules) die höchste Versionsnummer finden 
	for ( part1.name in part1.names.unique ) {
			
			# alle Dateinamen mit speziellen Namen (part1.name) finden
			part1.ind <- which ( part1 %in% part1.name )
			
			# dazugehörige Versionsnummer
			part2.temp <- part2 [ part1.ind ]
			
			# wenn ein Dateiname noch im develop.modules ist (allerdings jetzt ohne angegebene Versionsnummer), die höchste development Versionsnummer finden
			if (  part1.name %in% develop.modules ) {
					
					# höchste development Versionsnummer (mit Subfunktion)
					vers <- .source.it.all.sort ( part2.temp )
					
			# wenn Dateiname nicht in develop.modules, dann für die höchste stable Version finden (d.h. 0 hinten und höchste Nummerierung vorn/mitte)
			} else { 
			
					# alle, die eine 0 als letzte Ziffer haben
					alle0 <- part2.temp [ which ( regexpr ( "^\\d*\\.\\d*\\.0$" , part2.temp ) == 1 ) ]

					# sortieren und höchste stable Version bekommen (mit Subfunktion)
					vers <- .source.it.all.sort ( alle0 )
					
			}
			
			# wenn der Versionsvektor nicht leer ist, dann an temporäres tosource ranhängen 
			if ( !is.na ( vers ) ) tosource.temp <- paste ( part1.name , vers , sep = "_" ) else tosource.temp <- NULL
			
			#temporäres tosource an tosource ranhänge
			tosource <- c ( tosource , tosource.temp )
	
	}
	
	# File finden
	tosource <- unname ( mapply ( function ( tosource , files ) {
	
			files [ which ( regexpr ( tosource , files ) == 1 ) ]
	
	} , tosource , MoreArgs = list ( files ) ) )
	
	# return bauen bzw. sourcen
	if ( return.stable ) {
			ret <- tosource
			ret <- ret [ grepl ( "^.*\\d*\\.\\d*\\.0.[r|R]$" , tosource ) ] 
	} else {
	
			# Schleife über tosource
			# TODO schöne Plausichecks/Fehlerhandling
			temp <- mapply ( function ( tosource , folder ) {
			
						# print, schöne Ausgaben
						print ( paste ( "source.it.all: Datei " , tosource , " wurde gesourct." , sep = "" ) )
						
						# sourcen
						source (  file.path ( folder , tosource ) )
			
			} , tosource , MoreArgs = list ( folder ) )
			ret <- NULL
	}
	
	return ( ret )
	
}

# TEST
#source.it.all()
#source.it.all ( develop.modules = c ( "get.plausible" ) )

