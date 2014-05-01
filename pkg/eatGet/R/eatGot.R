
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
			
					testO <- function(O) {
						exists(as.character(substitute(O)))
					}
					
					# Definitionen
					einr <- "     "
					if (testO(names(object@results))) {
						if ( "model" %in% names(object@results) ) {
							modls <- unique(object@results["model"])	
							if ( "source" %in% names(object@results) ) {
								srcs <- lapply(modls[[1]], function(k) {
										unique(subset(object@results, object@results == k)["source"])
									})								
							} 
						} 
					}
										
					
					# Ausgabe-String
					if ( identical ( object@results , data.frame() ) ) {
							cat("Results object is empty\n")
					} else {
						if(testO(modls)) {
							cat("Results contain", paste(dim(modls)[1], "model(s):\n" ))							
									for(i in seq(along=1:dim(modls)[1])) {
										cat(paste0(modls[[1]][i], ": "))
										cat(length(na.omit(subset(object@results, object@results$model == modls[[1]][i])$value)), "parameter estimates ")
										if(testO(srcs)) {
											cat(paste0("(source[s]: ", paste0( unlist(srcs[[i]]), collapse=", " ), ")"),"\n")
										}
									}		
						} else {
							cat("Results contain no model names column\n" )
						}
					}				
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




