
.onAttach <- function(lib, pkg){
	welcomeMsg <- paste ( "\n===========================================\neat version: 1.9.9-215 (2012-11-20) Last Man Standing\n              \nThis version is BETA. Use at your own risk.\n===========================================\n" )
	packageStartupMessage ( welcomeMsg )
	install.eat.dependencies ()
	invisible ( welcomeMsg )
}

install.eat.dependencies <- function ( ) {

		pkgs <- c("MASS","car","date","foreign","gdata","reshape","sendmailR","xlsx","R.utils","parallel","psych","ggplot2","survey","fmsb")
		if ( grepl ( "OS" , Sys.getenv()["OS"] ) ) pkgs <- pkgs [ !pkgs=="xlsx" ]
		
		oldwarn <- getOption ( "warn" )
		options ( warn = -1 )
		ex <- sapply ( pkgs , require , character.only = TRUE , quietly = FALSE , warn.conflicts = FALSE )
		options ( warn = oldwarn )

		inst <- names ( ex [ !ex ] )

		if ( ! identical ( inst , character(0) ) ) {
			
				oldwarn <- getOption ( "warn" )
				options ( warn = 2 )
				tried <- try ( install.packages( inst , repos = c("http://cran.us.r-project.org") ) , silent = TRUE )
				options ( warn = oldwarn )
				if ( inherits ( tried , "try-error" ) ) {
						stop ( tried )
				} else {
						ex <- sapply ( inst , function(pkg) { try ( require ( pkg , character.only = TRUE , quietly = FALSE , warn.conflicts = TRUE ) ) } )
				}

		}

		invisible ( TRUE )
}
