# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.runBatches
# Description: Subroutine von automateModels
# Author:    Martin Hecht
# Change Log:
# 2012-01-17 SW/MH
# CHANGED: added option 'all.local.cores' in .automateModels.runBatches 
# 0000-00-00 AA
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.runBatches <- function ( batches , run.mode , all.local.cores ) {
		
		# Funktionsname für Meldungen
		f. <- ".automateModels.runBatches"
		f.n <- paste ( f. , ":" , sep = "" )
		
		# Plausichecks
		stopifnot ( run.mode %in% c ( "serial" , "parallel" ) )
		if ( !length ( batches ) == 1 & run.mode == "serial" & !all.local.cores ) {
				# sunk ( paste ( f.n , " run.mode ist 'serial', aber es existiert mehr als eine Batch-Datei. d.h. somethin' wrong.\n" , sep = "" ) )
				sunk ( paste ( f.n , " run.mode is 'serial' und 'all.local.cores' is FALSE, but more than one batch file exists, that means somethin' wrong.\n" , sep = "" ) )
				stop ( )
		}
		
		# Batch starten wenn run.mode == "serial" , bei "parallel" Prompt an User
		if ( run.mode == "serial" ) {
								
				.fun <- function ( batch ) {
						sunk ( paste ( f.n , " Try sending " , batch , "\n                            to console ... " , sep = "" ) )
						
						rtrn <- system ( paste ( '"', normalizePath( batch ), '"', sep = "" ) ,
										 intern = FALSE ,
										 ignore.stdout = FALSE ,
										 ignore.stderr = FALSE ,
										 wait = FALSE ,
										 input = NULL ,
										 show.output.on.console = FALSE ,
										 minimized = FALSE ,
										 invisible = FALSE )
						
						if ( rtrn == 0 ) {
										sunk ( paste ( "done.\n\n" , sep = "" ) )
										ret <- TRUE
								} else {
										sunk ( paste ( "Error.\n" , sep = "" ) )
										sunk ( paste ( f.n , " " , batch , " could NOT be started.\n" , sep = "" ) )
										stop ( )
										ret <- FALSE
								}
						return(ret)
				}
				ret <- mapply ( .fun , batches )
	
		} else if ( run.mode == "parallel" ) {
				
				# sunk ( paste ( f.n , " Bitte folgende Batch-Dateien MANUELL STARTEN:\n" , sep = "" ) )
				sunk ( paste ( f.n , " Please MANUALLY START these batch file(s):\n" , sep = "" ) )
				muell <- mapply ( function ( batches ) {
						sunk ( paste ( "                            " , batches , "\n" , sep = "" ) )
				} , batches )
				
				ret <- TRUE
				
		} else {
				ret <- FALSE
		}
	
		# returnen 
		return ( ret )
		
}



