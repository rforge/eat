
.onAttach <- function(lib, pkg){
	packageStartupMessage ( paste ( "\n===========================================\neat version: 1.5.15-128 (2012-08-14)\n              \nThis version is BETA. Use at your own risk.\n===========================================\n" ) )
	install.eat.dependencies ()
	}

install.eat.dependencies <- function ( ) {

		pkgs <- c("car","date","foreign","gdata","reshape","sendmailR","xlsx","R.utils","parallel","psych","ggplot2")

		oldwarn <- getOption ( "warn" )
		options ( warn = -1 )
		ex <- sapply ( pkgs , require , character.only = TRUE , quietly = FALSE , warn.conflicts = FALSE )
		options ( warn = oldwarn )

		inst <- names ( ex [ !ex ] )

		if ( ! identical ( inst , character(0) ) ) {
			
				oldwarn <- getOption ( "warn" )
				options ( warn = 2 )
				tried <- try ( install.packages( inst , repos = c("http://ftp5.gwdg.de/pub/misc/cran","http://ftp.yalwa.org/cran") ) , silent = TRUE )
				options ( warn = oldwarn )
				if ( inherits ( tried , "try-error" ) ) {
						stop ( tried )
				} else {
						ex <- sapply ( inst , require , character.only = TRUE , quietly = FALSE , warn.conflicts = TRUE )
				}

		}

		invisible ( TRUE )
}
