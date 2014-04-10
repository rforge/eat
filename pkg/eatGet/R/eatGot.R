
### Class definition of "eatGot" ###
setClass(
	"eatGot" ,
	representation = representation ( 
			results = "data.frame"
			) ,
	prototype = prototype ( 
			results = data.frame()
			)
)

# show
setMethod ( f = "show" , signature = signature ( object="eatGot" ) ,
			definition = function ( object ) {

					# Definitionen
					einr <- "     "
					
					# Ausgabe-String
					if ( identical ( object@results , data.frame() ) ) {
							msg <- "Results object is empty\n"
					} else {
							msg <- paste0 (
									"Results contain:\n\n" ,
									" ===TODO=== " ,
									"\n" )
					}
				
					# raushauen
					cat ( msg )
			}
)
