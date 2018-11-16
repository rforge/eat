
.onAttach <- function(lib, pkg){
	welcomeMsg <- "\n==============================================\n    This eatPrep version is deprecated.\n   Latest version can be found on GitHub.\n                   Please use\n> devtools::install_github(\"sachseka/eatPrep\")\n          to install latest version.\n==============================================\n" 
	packageStartupMessage ( welcomeMsg )
	#invisible ( welcomeMsg )
}
