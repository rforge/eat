
.onAttach <- function(lib, pkg){
	welcomeMsg <- "\n==============================================\n    This eatRep version is deprecated.\n   Latest version can be found on GitHub.\n                   Please use\n> devtools::install_github(\"weirichs/eatRep\")\n          to install latest version.\n==============================================\n" 
	packageStartupMessage ( welcomeMsg )
	#invisible ( welcomeMsg )
}
