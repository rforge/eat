# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sunk
# Description:  evaluiert cmd ("character-expression") und schreibt Ausgabe der expression nach path und Bildschirmausgabe
#				path (character, default NULL): wenn nicht angegeben wird 'sunk.path' in einem parent-Environment gesucht
#						wenn 'sunk.path' in keinem parent-Environment existiert wird nach "getwd()+sunk.txt" geschrieben
#				write (logical, default TRUE): in Datei schreiben
#				console.output (logical, default TRUE): Bildschirmausgabe ja/nein
#				new.file (logical, default FALSE): neuen File anlegen, ansonsten appenden
#				text.on.error (logical, default TRUE): wenn TRUE und Text übergeben wird (erzeugt error) dann wird dieser
#						Text mit der in text.out.method spezifizierten Methode geschrieben
#				text.out.method (character, default "cat"): entweder "cat" oder "print"
#
# 2011-12-22 MH
# OPTIMIZED: function '.find.object' (needed for function 'sunk')
# 0000-00-00 AA
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.find.object <- function ( find.object ) {

		env <- sys.parents ()
		akt.env <- max ( env ) + 1
		
		if ( ! identical ( env , integer(0) ) ) {
			
				.fun <- function ( ev ) {
						eval ( parse ( text = paste ( "exists('" , find.object , "')" , sep = "" ) ) , envir = ev )
				}
				wo <- mapply ( .fun , env , SIMPLIFY = TRUE )
				names ( wo ) <- env
		
				if ( ! identical ( ( woi <- names ( which ( wo ) ) ) , character(0) ) ) {
						woherholen <- max ( as.numeric ( woi ) )
						env <- parent.frame ( akt.env - woherholen )
						attach ( env , warn.conflicts = FALSE )
						eval ( parse ( text = paste ( "ret<-" , find.object , sep = "" ) ) )
						detach ( env )
				} else ret <- NULL
		
		} else ret <- NULL
		return ( ret )
}
# TEST
# x <- "bla" 
# .fun3 <- function ( find.object ) {
		# .find.object ( find.object )
# }
# .fun2 <- function ( find.object ) {
		# .fun3 ( find.object )
# }
# .fun1 <- function ( find.object ) {
		# .fun2 ( find.object )
# }
# .fun1 ( "x" )

sunk <- function ( cmd = NULL , path = NULL , write = TRUE , console.output = TRUE , new.file = FALSE , text.on.error = TRUE , text.out.method = NULL ) {
		
		if ( ! is.character ( cmd ) ) {
				cat ( "sunk: Paramter 'cmd' must be character." )
				stop ( )
		}
		
		# gucken ob sunk.path in irgendwelchen Parent-Envs gesetzt
		if ( is.null ( path ) ) path <- .find.object ( "sunk.path" )
		
		# wenn nicht gefunden defaulten
		if ( is.null ( path ) ) path <- file.path ( getwd () , "sunk.txt" )
		
		if ( file.exists ( path ) & new.file ) {
				file.remove ( path )
				# warten bis gelöscht
				while ( file.exists ( path ) ) {
						Sys.sleep ( 0.01 )
				}
		}
		if ( file.exists ( path ) ) app <- TRUE else app <- FALSE

		# Parent frame attachen
		attach ( parent.frame() , warn.conflicts = FALSE )
		
		# Testen ob Fehler
		# wenn Fehler und text.on.error == TRUE dann print / oder cat davor , sonst stoppen
		tried1 <- try ( parse ( text = cmd ) , silent = TRUE  ) 
		if ( ! inherits ( tried1 , "try-error" ) ) { 
				
				# Sonderfälle abfangen
				# komplett
				sf <- c ( "." )
				# enthaltene Teile
				sfpart <- c ( "cat" , "print" , "\n" , "\t" )
				prob <- unname ( unlist ( mapply ( function ( sfpart , cmd ) {
						grepl ( sfpart , cmd , fixed = TRUE )
				} , sfpart , MoreArgs = list ( cmd ) ) ) )
				
				if ( cmd %in% sf | any ( prob ) ) {
						if ( ! grepl ( "cat" , cmd ) ) cmd <- paste ( "cat('",cmd,"\n')",sep="" ) 
						tried2 <- NULL
				} else {
						tried2 <- try ( eval ( parse ( text = cmd ) ) , silent = TRUE )
				}
		
		} else tried2 <- NULL
		
		if ( inherits ( tried1 , "try-error" ) | inherits ( tried2 , "try-error" ) ) {
		# if ( inherits ( tried1 , "try-error" ) ) {
				if ( text.on.error ) {
						if ( is.null ( text.out.method ) ) text.out.method <- "cat"
						stopifnot ( text.out.method %in% c ( "cat" , "print" ) )
						if ( text.out.method == "cat" ) str.suf <- "\n" else str.suf <- ""
						# Hochkommas ersetzen, crashen sonst das parsen
						cmd <- gsub ( "'" , "\\'" , cmd ,  fixed = TRUE )
						cmd <- paste ( text.out.method , "('" , cmd , str.suf , "')" , sep = "" )
				} else stop ("sunk: Parameter 'cmd' invalid")
		} 
		
		# Konsolen-Output
		if ( console.output & ! is.null ( cmd ) ) {
				if ( grepl ( "^\\s*print\\s*\\(" , cmd ) | grepl ( "^\\s*cat\\s*\\(" , cmd ) ) eval ( parse ( text = cmd ) ) else
							print ( eval ( parse ( text = cmd ) ) )
		}

		# Header erstellen
		if ( ! app ) {
				hdr <- paste ( "|" , as.character(Sys.time()) , "|" , Sys.getenv()[ "USERNAME" ] , "|" , Sys.getenv()[ "COMPUTERNAME" ] , "|" , 
								R.Version()$version.string , Sys.getenv()[ "R_ARCH" ] , "|\n" )
				hdr_ <- paste ( paste ( rep ( "-" , nchar ( hdr ) ) , collapse = "" ) , "\n" , sep = "" )
				hdr <- paste ( hdr_ , hdr , hdr_ , "\n" , sep = "" )
		}


		# schreiben
		if ( write ) {
				sink ( path , append = app )
						if ( exists ( "hdr" ) ) cat ( hdr )
						if ( ! is.null ( cmd ) ) {
									if ( grepl ( "^\\s*print\\s*\\(" , cmd ) | grepl ( "^\\s*cat\\s*\\(" , cmd ) ) eval ( parse ( text = cmd ) ) else
									print ( eval ( parse ( text = cmd ) ) )
						}
						cat ( "" )
				sink(type="message")
				sink()
		}
		
		invisible ( TRUE )
}

# TEST
# sunk.path <- "c:/Temp/Temp019/log.txt"
# sunk( "cat('afdfdsdaff\n')" )
# sunk( "sdafjl sdafjlsdfajl dsfsd" )




