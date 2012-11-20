# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.check
# Description: Subroutine von automateModels
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#			14.10.2011 MH: Ausgabe auf Englisch
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.check <- function ( liste , check ) {
		available <- list (
				c( "conquest" , "1pl" , "unidim" , "singlegroup" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif") ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "anchor" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "anchor"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "anchor" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "anchor"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "regression" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "regression"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "regression"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "anchor" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "anchor" , "regression"   ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "anchor" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "anchor" , "regression" )
				# TODO : erweitern
		)
		
		implemented <- list (
				c( "conquest" , "1pl" , "unidim" , "singlegroup" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif") ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "anchor" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "anchor"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "anchor" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "anchor"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "regression" ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "regression"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "regression"  ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "anchor" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "anchor" , "regression"   ) ,
				c( "conquest" , "1pl" , "unidim" , "singlegroup" , "dif" , "anchor" , "regression"  ) ,
				c( "conquest" , "1pl" , "multidim" , "singlegroup" , "dif" , "anchor" , "regression" )
				# TODO : erweitern
		)
				
		if ( check == "available" ) reflist <- available else if ( check == "implemented" )
				reflist <- implemented else stop("internal error in .automateModels.check : 
												  argument 'check' not or wrongly specified")
		
		# für dif, anchor, regression jeweilige Bezeichnung setzen
		liste$dif <- sapply ( liste$dif , function ( element ) {
											if (is.null(element)) NULL else "dif"
									} , simplify=FALSE)
		liste$anchor <- sapply ( liste$anchor , function ( element ) {
											if (is.null(element)) NULL else "anchor"
									} , simplify=FALSE)
		liste$regression <- sapply ( liste$regression , function ( element ) {
											if (is.null(element)) NULL else "regression"
									} , simplify=FALSE)									
		
		
		# check
		ret <- all ( 

				mapply ( function ( m.model , i.model , p.model , software , dif , anchor , regression ) {
				
					if ( any (

								mapply ( function ( reflist , test ) {
										identical ( sort ( test ) , sort ( reflist ) )
								} , reflist , MoreArgs=list( test <- c ( m.model , i.model , p.model , software , dif , anchor , regression ) ) )

							 )	)	{
											return ( TRUE )
									} else {
											print ( paste ( "Model specification" , paste ( test , collapse=", " ) , "is not" , check , "." ) )
											return ( FALSE )
									}
					
				} , liste$m.model , liste$i.model , liste$p.model , liste$software , liste$dif , liste$anchor , liste$regression )
		)  

		return ( ret )
}
