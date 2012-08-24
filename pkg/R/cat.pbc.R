# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cat.pbc
# Description: berechnet Kategorientrennschärfen 
# Version: 	0.1.0
# Status: alpha
# Release Date: 2012-06-11, 2012-07-27 implemented in eat
# Author:  Nicole Haag
#
# Change Log:
# 2012-08-23 NA
# ADDED: Argument 'context.vars' in cat.pbc
# 0000-00-00 AA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TO DO: 
## - auch Kategorien mit ausgeben, die keiner gewählt hat
## - Itemprops mit ausgeben (?)
## - Fehlerhandling optimieren

###############################################################################

cat.pbc <- function (datRaw, datRec, idRaw, idRec, context.vars, values, subunits, xlsx = NULL ) {

## datRaw:     unrecoded dataset, must contain only id and test items
## datRec:     the same datset as datRaw, in recoded form
## idRaw:      name or number of id in unrecoded dataset
## idRec:      name or number of id in recoded dataset
## values:     input table values
## subunits:   input table subunits
## xlsx:	     full path of excel to be written
## context.vars: name or column numbers of context vars, must be identical in both datasets!

	# Prüfen, ob IDs in beiden Datensätzen übereinstimmen
	idrec <- datRec [ , idRec ]
	idraw <- datRaw  [ , idRaw ]
	if ( ! setequal(idrec, idraw) ) {
		stop ( "cat.pbc: IDs in datasets are not all identical." ) 
	} else {

		# sort IDs 
		datRec  <- datRec [ order(idrec) , ]
		datRaw   <- datRaw  [ order(idraw) , ]   

    # make inputs
    recodeinfo <- makeInputRecodeData (values = values, subunits = subunits)
    varinfo    <- .makeVarinfoRaw (values, subunits)

#		Kontextvariablen ausschließen
    if(is.numeric(context.vars)) {
      context.vars <- colnames(datRaw)[context.vars]
    }
    keep.varsRaw <- setdiff(colnames(datRaw), context.vars)
    keep.varsRec <- setdiff(colnames(datRec), context.vars)
    
    datRaw <- datRaw[ , match(keep.varsRaw, colnames(datRaw))]
    datRec <- datRec[ , match(keep.varsRec, colnames(datRec))]
    
    if(is.numeric(idRaw)) {
      idRaw <- colnames(datRaw)[idRaw]
    }
    vars <- setdiff(colnames(datRaw), idRec)
		
    # relative Punktzahl
		options(warn=-1)
		rel.score1 <- datRec[ , which( colnames(datRec) %in% subunits$subunitRecoded ) ]
		rel.score1 <- apply(rel.score1, 2, as.numeric ) 
		rel.score <- rowMeans(  rel.score1, na.rm=T )

		# Kategorientrennschärfen berechnen
		dfr1 <- NULL

		for (vv in seq( along = vars ) ){
		#    vv <- 35
			var.vv <- vars [vv]
			dat.vv <- datRaw[ , which( colnames(datRaw) == var.vv  ) ]
			valueTypes <- lapply ( varinfo[[vars[vv]]]$values, "[[", "type")
      valuesToNA <- c( "mbd", "mci", names(valueTypes) [ valueTypes %in% c("mbd", "mci") ] )
			dat.vv [ dat.vv %in% valuesToNA] <- NA
			# Häufigkeitsverteilung der Codes, ohne mbd & mci
			tvv <- table( dat.vv )
			tvv1 <- tvv / sum(tvv)
			kat.vv <- names(tvv1) 
			for (bb in seq ( along = kat.vv) ){
				#                bb <- 2
				l.bb <- c( var.vv  , kat.vv[bb] , sum(tvv) , tvv[bb] , tvv1[bb] , 
				  cor( 1 * ( dat.vv == kat.vv[bb] ) , rel.score , use = "complete.obs" ) , 
				  recodeinfo[[ var.vv ]]$values [[kat.vv[bb]]]
				  )
				dfr1 <- rbind( dfr1 , l.bb )
			}
		}
		dfr1 <- data.frame(dfr1, stringsAsFactors = FALSE)
		colnames(dfr1) <- c( "variable" , "cat" , "n" , "freq" , "freq.rel" , "cat.pbc" , "recodevalue" )
		dfr1 [ , 3:6 ] <- apply(dfr1 [ , 3:6 ], 2, as.numeric ) 
	}
	
	if ( !is.null ( xlsx ) ) {
			try ( write.xlsx(dfr1, file = xlsx, sheetName="cat.pbc", col.names=TRUE, row.names=TRUE, append=FALSE) )
	}	
	
	return ( dfr1 ) 
}