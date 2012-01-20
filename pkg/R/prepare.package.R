# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# prepare.package
# Description: Tool zur Vorbereitung des Package
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-12-07
# Author:    Martin Hecht
# Change Log:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

copy.for.package <- function ( source.folder = "p:/ZKD/development" ,
							   files = NULL ,
							   package.folder = "p:/ZKD/packages" ,
							   package.name = NULL , package.version = NULL ) {

		stopifnot ( ! is.null ( source.folder ) )
		stopifnot ( ! is.null ( files ) )
		stopifnot ( ! is.null ( package.folder ) )
		stopifnot ( ! is.null ( package.name ) )
		stopifnot ( ! is.null ( package.version ) )
		
		# destination folder bauen
		dest.folder <- file.path ( package.folder , package.name , package.version , "R" )
		temp <- .del.or.create.folder ( dest.folder , countdown = 0 )
		
		# alle stable aus development Ordner
		stable <- source.it.all ( source.folder , return.stable = TRUE )

		# files, die ins Package sollen selektieren
		part1 <- unlist ( strsplit( stable , split="_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", fixed=FALSE ) )
		cp <- stable [ part1 %in% files ]
		
		# kopieren
		.fun <- function ( module , source.folder , dest.folder ) {
				from <- file.path ( source.folder , module )
				to <- file.path ( dest.folder , module )
				file.copy ( from , to )		
		}
		ret <- mapply ( .fun , cp , MoreArgs = list ( source.folder , dest.folder ) )
		ret <- names ( ret [ unname ( ret ) ] )
		
		# die gecopied wurden returnen als character-Vektor
		return ( ret )
}


extract.changelog <- function ( path ) {

# path <- "p:/ZKD/development/test_1.0.0.R"		
# path <- "p:/ZKD/development/automateModels.collect.results_0.4.0.R"		
# path <- "p:/ZKD/development/aggregateData_1.2.0.R"		
# path <- "p:/ZKD/development/automateModels.conquest.multigroup_0.4.0.R"		

		zeilen <- scan ( file=path , what="character" , sep="\n" , quiet = TRUE )
		
		# Zeilen mit konventionalisiertem Datum/Autor raussuchen
		datelines <- which ( grepl ( "^[^#]*#*\\s*\\d{4}-\\d{2}-\\d{2}\\s*\\w+.*$" , zeilen ) )
		content1 <- ! identical ( datelines , integer(0) )
		if ( ! content1 ) cat ( paste ( path , ": no content (no date line). Will be ignored.\n" ) )

		if ( content1 ) {
				datelines.min <- min ( datelines )
				datelines.max <- max ( datelines )

				# check ob mehr als eine Dateline da
				content2 <- !( datelines.min == datelines.max )
				if ( ! content2 ) cat ( paste ( path , ": no content (only one date line). Will be ignored.\n" ) )

				# check auf Konventionen letzte Zeile nicht "0000-00-00 AA" enthält
				lastlineOK <- grepl ( "^[^#]*#*\\s*0000-00-00 AA*.*$" , zeilen[datelines.max] )
				if ( ! lastlineOK ) cat ( paste ( path , ": row '0000-00-00 AA' not found. Will be ignored.\n" ) )
		} else {
				content2 <- FALSE
				lastlineOK <- FALSE
		}
		
		flush.console ()
		
		# nur weiter wenn alles ok
		if ( ! all ( content1 , content2 , lastlineOK )  ) {
				ret <- list ( NULL )
		} else {

				# relevante Zeilen
				rel.lines <- zeilen [ datelines.min:(datelines.max) ]
				datel <- which ( grepl ( "^[^#]*#*\\s*\\d{4}-\\d{2}-\\d{2}\\s\\w+.*$" , rel.lines ) )
				
				# über die Zeilen drüber, und Changelog rausextrahieren
				.fun <- function ( datel , rel.lines , all.datel ) {
						next.datel.ind <- which ( all.datel %in% datel ) + 1
						next.datel <- all.datel[ next.datel.ind ]
						author <- sub ( "^[^#]*#*\\s*\\d{4}-\\d{2}-\\d{2}\\s*(\\w+/*\\w+).*$" , "\\1" , rel.lines[datel] )
					
						if ( !is.na ( next.datel ) ) {
								if ( length ( ex.vec <- (datel + 1):(next.datel - 1) ) > 0 ) {
										ret <- rel.lines [ (datel + 1):(next.datel - 1) ]
										
										# Raute rauscleanen und author ran
										.fun2 <- function ( el ) {
												temp <- sub ( "^[^#]*#*\\s*(.*)$" , "\\1" , el )
												temp <- paste ( "[" , author , "] " , temp , sep = "" )
										}
										ret <- sapply ( ret , .fun2 , USE.NAMES = FALSE )
										
								} else {
										ret <- NULL
								}
						
						} else {
								ret <- NULL
						}
						return ( ret )
				}
				ret <- mapply ( .fun , datel , MoreArgs = list ( rel.lines , datel ) , SIMPLIFY = FALSE )
			
				# benennen mit Datum
				names ( ret ) <- sub ( "^[^#]*#*\\s*(\\d{4}-\\d{2}-\\d{2})\\s*\\w+.*$" , "\\1" , rel.lines[datel] )
			
				# NULL Elemente löschen
				ret <- ret[ sapply(ret, function(ii) {!is.null(ii)}) ]
		}
		
		return (ret)
				
}


get.versions <- function ( folder ) {
	
		file.list <- list.files( path = folder, pattern = NULL, all.files = TRUE,
           full.names = TRUE, recursive = TRUE , ignore.case = FALSE, include.dirs = FALSE )
		
		file.list <- file.list [ grepl ( "^.*\\d+\\.\\d+\\.\\d+/DESCRIPTION$" , file.list ) ]

		.fun <- function ( file ) {
				zeilen <- scan ( file=file , what="character" , sep="\n" , quiet = TRUE )
				version <- zeilen [ grepl ( "^Version:.*$" , zeilen ) ]
				version <- sub ( "^Version:[^\\d](\\d+\\.\\d+\\.\\d+)[^\\d]*.*$" , "\\1" , version )
				date <- zeilen [ grepl ( "^Date:.*$" , zeilen ) ]
				date <- sub ( "^Date:[^\\d](\\d{4}-\\d{2}-\\d{2})[^\\d]*.*$" , "\\1" , date )
				ret <- list ( version )
				names ( ret ) <- date
				return ( ret )
		}
		ret <- unlist ( mapply ( .fun , file.list , SIMPLIFY=TRUE , USE.NAMES = FALSE ) )

		return ( ret )	
		
}


create.package.change.log <- function ( source.folder , files , package.folder ,
							   package.name = NULL , package.version = NULL ) {

		cat ( "CHANGELOG is being created...\n" )
		
		.fun1 <- function ( files , source.folder ) {
				chl <- extract.changelog ( file.path ( source.folder , files ) )
		}
		chlog.list <- unlist ( mapply ( .fun1 , files , MoreArgs = list ( source.folder ) , SIMPLIFY = FALSE , USE.NAMES = FALSE ) , recursive = FALSE )
		
		# NULL Elemente löschen
		chlog.list <- chlog.list[ sapply(chlog.list, function(ii) {!is.null(ii)}) ]	

		# package-Versions besorgen
		package.versions <- get.versions ( file.path ( package.folder , package.name ) )

		# Dates
		dates.str <- names ( package.versions )
		dates <- sort ( as.date ( unique ( dates.str ) , "ymd" ) )
		
		# splitten nach Date
		.fun2 <- function ( date , chlog.lines , all.date ) {
				prev.date.ind <- which ( all.date %in% date ) - 1
				prev.date <- all.date[ prev.date.ind ]
		
				if ( which ( all.date %in% date ) == 1 ) {
						ret <- chlog.lines[ as.date ( names ( chlog.list ) , "ymd" ) <= date ]
				} else if ( !is.na ( prev.date ) ) {
								ret <- chlog.lines[ (as.date ( names ( chlog.list ) , "ymd" ) <= date) & 
													(as.date ( names ( chlog.list ) , "ymd" ) > prev.date) ]
						} else {
								ret <- NULL
						}
				
				return ( unlist ( unname ( ret ) ) )
		}
		chlog.version <- mapply ( .fun2 , dates , MoreArgs = list ( chlog.list , dates ) , SIMPLIFY = FALSE )
		names ( chlog.version ) <- paste ( "Version " , package.versions [ dates.str ] , " (" , dates.str , ")" , sep = "" )
	
		# neueste oben
		chlog.version <- chlog.version [ sort ( names ( chlog.version ) , decreasing = TRUE ) ]
		
		### Changelog schreiben
		chlog.file <- file.path ( package.folder , package.name , paste ( package.name , "_" , package.version , "_changelog.txt" , sep = "" ) )

		header <- paste ( package.name , " (Version " , package.version , ") ChangeLog" , sep = "" )
		striche <- paste ( rep ( "-" , length.out = nchar ( header ) ) , collapse = "" )
		write ( striche , file = chlog.file , append = FALSE )		
		write ( header , file = chlog.file , append = TRUE )
		write ( paste ( striche , "\n" , sep = "" ) , file = chlog.file , append = TRUE )
		
		.fun3 <- function ( el , name , chlog.file ) {
				write ( name , file = chlog.file , append = TRUE )				
				write ( paste ( el , collapse = "\n" ) , file = chlog.file , append = TRUE )				
				write ( "" , file = chlog.file , append = TRUE )				
		}
		temp <- mapply ( .fun3 , chlog.version , names ( chlog.version ) , MoreArgs = list ( chlog.file ) )
	
}

## test:
# create.package.change.log( source.folder= "p:/ZKD/development" , files=c("mergeData_0.5.0.R", "automateDataPreparation_0.5.0.R") , package.version = "0.0.3", package.folder ="p:/ZKD/development/package/ZKD" , package.name="ZKD" )
# create.package.change.log( source.folder= "p:/ZKD/development" , files=stable , package.version = "1.4.3", package.folder ="p:/ZKD/packages" , package.name="eat" )
# prepare.package ( source.folder = "p:/ZKD/development" ,  files="mergeData_0.5.0.R", package.folder = "p:/ZKD/development/package" , package.name="ZKD" , package.version = "0.0.3" ) 
# copy.for.package ( source.folder = "p:/ZKD/development" , files = c("mergeData", "automateDataPreparation") ,  package.folder = "p:/ZKD/development/package" ,  package.name = "ZKD" , package.version = "0.0.3" )							

#stable <- source.it.all ( source.folder , return.stable = TRUE )
#part1 <- unlist ( strsplit( stable , split="_{1}\\d*\\.\\d*\\.\\d*\\.[r|R]$", fixed=FALSE ) )
#yyy <- unique(part1)
#copy.for.package ( source.folder = "p:/ZKD/development" , files = yyy ,  package.version = "1.4.3", package.folder ="p:/ZKD/packages" , package.name="eat")							


prepare.package <- function ( source.folder = "p:/ZKD/development" , files , package.folder = "p:/ZKD/packages" , package.name , package.version ) {
		
		# Checks
		stopifnot ( !is.null( files ) )
		stopifnot ( !is.null( package.name ) )
		stopifnot ( !is.null( package.version ) )
		
		# R Files kopieren
		dateien <- copy.for.package ( source.folder = source.folder , files = files , package.folder = package.folder , package.name = package.name , package.version = package.version )

		# DESCRIPTION in x.x.x aktualisieren
		descr.file <- file.path ( package.folder , package.name , "x.x.x" , "DESCRIPTION" )
		if ( ! file.exists ( descr.file ) ) stop ( paste ( descr.file , "not found. Please check!" ) )
		
		zeilen <- scan ( file=descr.file , what="character" , sep="\n" , quiet = TRUE )
		version.ind <- which ( grepl ( "^Version:.*$" , zeilen ) )
		zeilen[version.ind] <- paste ( "Version:" , package.version )
		date.ind <- which ( grepl ( "^Date:.*$" , zeilen ) )
		zeilen[date.ind] <- paste ( "Date:" , Sys.Date() )
		write ( zeilen , file=descr.file )		
		
		# x.x.x ins aktuelle Verzeichnis kopieren
		file.list <- list.files( path = file.path ( package.folder , package.name , "x.x.x" ), pattern = NULL, all.files = FALSE ,
				   full.names = TRUE, recursive = FALSE , ignore.case = FALSE, include.dirs = TRUE )
		sapply ( file.list , file.copy , to=file.path ( package.folder , package.name , package.version ) , recursive = TRUE )

		# automateModels Version/Datum setzen
		aM.file <- list.files ( file.path ( package.folder , package.name , package.version , "R" ) , pattern = "^automateModels_.*$" , full.names = TRUE )
		if ( ! file.exists ( aM.file ) ) stop ( paste ( aM.file , "not found. Please check!" ) )
		zeilen <- scan ( file=aM.file , what="character" , sep="\n" , quiet = TRUE )
		zeile.ind <- which ( grepl ( "^.*---donotdelete---.*$" , zeilen ) )
		zeilen[zeile.ind] <- paste ( "\t\tsunk ( paste ( f.n , 'Version: " , package.version , " (" , Sys.Date() , ")\\n' ) )" , sep ="" )
		write ( zeilen , file=aM.file )				
		
		# Changelog erzeugen
		create.package.change.log ( source.folder = source.folder , files = dateien , package.folder = package.folder , package.name = package.name , package.version = package.version )

}
