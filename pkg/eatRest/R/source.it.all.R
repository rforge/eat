####################################################################################################################
#
# source.it.all
# sourced alle R-Skripte
#
#
# ARGUMENTE:
#	development (logical):			wenn TRUE (default) wird höchste Version geladen
#									wenn FALSE wird höchste stable geladen
#	development.only (logical): 	wenn TRUE werden nur development-Versionen geladen
#									wenn FALSE (default) wird höchste Version (development oder stable geladen)
#	exclude:						character-Vektor mit Modulen, die nicht geladen werden sollen
#
# Version: 		0.4.0
# Depends: 	
# Imports: 	
# Published: 	2012-01-23
# Author: 		Martin Hecht, Christiane Penk
# Maintainer: 	Martin Hecht, Christiane Penk
#
# Change-Log
#		14.11.11 MH: Option exclude hinzugefügt
#
####################################################################################################################

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
source.it.all <- function ( folder="p:/ZKD/development" , use.zkd.conv = TRUE , development = TRUE , development.only = FALSE , exclude = NULL ) {
	
	### Plausichecks
	# sinnlose Kombi von development und development.only ausschliessen
	if ( !development & development.only ) {
			cat ( paste ( "source.it.all: development=FALSE und development.only=TRUE ist nicht sinnvoll.\n" ) )
			stop ()
	}

	# initialisieren, Sammlung von zu sourcenden files
	tosource <- NULL	
	
	# listet alle Dateien auf, die nach den Konventionen benannt sind; oder alle R-files
	if ( use.zkd.conv ) {
			
			files <- list.files(folder , all.files=TRUE , pattern = "^.*_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", full.names=FALSE )
			
			# erster Teil, also part1 (bis zum Unterstrich) wird abgesplittet
			part1 <- unlist ( strsplit(files, split="_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", fixed=FALSE ) )
			
			# zweiter Teil, also part2 (Versionsnummer) wird abgesplittet
			part2 <- unname ( mapply ( function ( files , part1 ) {
					substr(files, nchar(part1)+2 , nchar(files)-2 )
			} , files , part1 ) )
			
			# Plausicheck: Anzahl der Elemente muss in part1 und part2 gleich sein
			stopifnot ( length ( part1 ) == length ( part2 ) )
			
			# unique Part1 (mehrfache Dateinamen werden entfernt [also für die Funktion, von denen mehrere Versionsnummern im Ordner lagen])
			part1.names.unique <- unique ( part1 )
			
			# für die restlichen Dateinamen (die nicht direkt gesourct werden sollten über develop.modules) die höchste Versionsnummer finden 
			for ( part1.name in part1.names.unique ) {
					
					# alle Dateinamen mit speziellen Namen (part1.name) finden
					part1.ind <- which ( part1 %in% part1.name )
					
					# dazugehörige Versionsnummer
					part2.temp <- part2 [ part1.ind ]
					
					# wenn ein Dateiname noch im develop.modules ist (allerdings jetzt ohne angegebene Versionsnummer), die höchste development Versionsnummer finden
					# if (  part1.name %in% develop.modules ) {
							
							# höchste development Versionsnummer (mit Subfunktion)
							# vers <- .source.it.all.sort ( part2.temp )
							
					# wenn Dateiname nicht in develop.modules, dann für die höchste stable Version finden (d.h. 0 hinten und höchste Nummerierung vorn/mitte)
					# } else { 
				
							# alle, die eine 0 als letzte Ziffer haben
							if ( !development ) alle0 <- part2.temp [ which ( regexpr ( "^\\d*\\.\\d*\\.0$" , part2.temp ) == 1 ) ] else alle0 <- part2.temp
				
							# Warnung wenn keine stable (aber eine develop, die nicht gesourct wird wegen !development) Version vorhanden ist
							if ( identical ( alle0 , character(0) ) ) cat ( paste ( 
									"source.it.all: Keine stable Version von" , part1.name , "vorhanden.\n" ) )

							# sortieren und höchste stable Version bekommen (mit Subfunktion)
							vers <- .source.it.all.sort ( alle0 )
							
							# wenn development.only und ne stable rausgesucht wurde diese löschen
							if ( development.only & grepl ( ".*\\.0$" , vers ) ) vers <- NA
							
							
					# }
					
					# wenn der Versionsvektor nicht leer ist, dann an temporäres tosource ranhängen 
					if ( !is.na ( vers ) ) tosource.temp <- paste ( part1.name , vers , sep = "_" ) else tosource.temp <- NULL
					
					#temporäres tosource an tosource ranhänge
					tosource <- c ( tosource , tosource.temp )
			}
	} else {
			tosource <- files <- list.files(folder , all.files=TRUE , pattern = "^.*[r|R]$", full.names=FALSE )
	}
	
	# excluden
	if (!is.null(exclude)) tosource <- tosource [ !tosource %in% exclude ]

	# File finden
	tosource <- unname ( mapply ( function ( tosource , files ) {
	
			files [ which ( regexpr ( tosource , files ) == 1 ) ]
	
	} , tosource , MoreArgs = list ( files ) ) )
	
	# return bauen bzw. sourcen
	# if ( return.stable ) {
			# ret <- tosource
			# ret <- ret [ grepl ( "^.*\\d*\\.\\d*\\.0.[r|R]$" , tosource ) ] 
	# } else {
	
			# Schleife über tosource
			# TODO schöne Plausichecks/Fehlerhandling
			temp <- mapply ( function ( tosource , folder ) {
			
						# print, schöne Ausgaben
						cat ( paste ( "source.it.all: Datei " , tosource , " wurde gesourct.\n" , sep = "" ) )
						
						# sourcen
						source (  file.path ( folder , tosource ) )
			
			} , tosource , MoreArgs = list ( folder ) )
			# ret <- NULL
	# }
	
	# return ( ret )
	
}
