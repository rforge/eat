
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
					if (!inherits(try( "model" %in% names(object@results))	, "try-error")) {
						if ( "model" %in% names(object@results) ) {
							modls <- unique(object@results["model"])	
						} 
					}
										
					
					# Ausgabe-String
					if ( identical ( object@results , data.frame() ) ) {
							msg <- "Results object is empty\n"
					} else {
							msg <- paste0 (
									"Results contain:\n\n" ,
									paste(dim(modls)[1], "models:", paste(modls[[1]][1:(dim(modls)[1])], collapse = ", ")),
									"\n\n",
									" === u.v.m. ;) === " ,
									"\n" )
					}
				
					# raushauen
					cat ( msg )
			}
)

# fixef for eatGot
#setMethod ( f = "fixef" , signature = signature ( object="eatGot" ) ,
#			definition = function ( object ) {
#
#					#get.fixef <- function ( lmer.effects, easy.to.difficult = FALSE, withCorrelation = FALSE) {
#					
#					# temporaer
#					easy.to.difficult = FALSE
#					withCorrelation = FALSE
#					
#					checkForReshape()
#					
#					withoutCorr <- object@results[intersect ( which(object@results[,"type"] == "fixed"), which (object@results[,"parameter"] != "correlation" ) ) ,]
#					withoutCorr <- dcast(withoutCorr, Var1~parameter, value.var = "value")[,c("Var1", "est", "se", "z.value", "p.value")]
#					if(easy.to.difficult == TRUE) { withoutCorr[,"est"] <- -1 * withoutCorr[,"est"]}
#					if(withCorrelation == TRUE ) {
#						onlyCorr    <- object@results[intersect ( which (object@results[,"parameter"] == "correlation" ), which(object@results[,"type"] == "fixed")) ,]
#						if(nrow(onlyCorr)>0) {
#						   onlyCorr    <- dcast(onlyCorr, Var1~Var2, value.var = "value")
#						   colnames(onlyCorr)[-1] <- paste("corr", colnames(onlyCorr)[-1], sep="_")
#						   withoutCorr <- merge(withoutCorr, onlyCorr, by = "Var1", all = TRUE)
#						}
#					}
#					return(withoutCorr)
#					
#			}
#)




