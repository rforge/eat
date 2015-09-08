# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# catPbc
# Description: berechnet Kategorientrennschärfen
# Version: 	0.1.0
# Status: alpha
# Release Date: 2012-06-11, 2012-07-27 implemented in eat
# Author:  Nicole Haag
#
# Change Log:
# 2012-08-28 NH
# ADDED: catPbc provides information about all valid codes
# 2012-08-23 NH
# ADDED: Argument 'context.vars' in catPbc
# 0000-00-00 AA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## TO DO:
## auch mit aggregiertem und bewertetem Datensatz nutzbar machen (Wunsch Alex Roppelt)
## vgl. recodeData(dat=dat, values=inputList$unitRecodings, subunits=inputList$units)

###############################################################################

catPbc <- function(datRaw, datRec, idRaw, idRec, context.vars, values, subunits, xlsx = NULL) {

## datRaw:     unrecoded dataset
## datRec:     the same dataset as datRaw, in recoded form
## idRaw:      name or number of id in unrecoded dataset
## idRec:      name or number of id in recoded dataset
## context.vars: name or column numbers of context vars, must be identical in both datasets!
## values:     input table values
## subunits:   input table subunits
## xlsx:	     full path of excel to be written


	# Prüfen, ob IDs in beiden Datensätzen übereinstimmen
	idrec <- datRec [ , idRec ]
	idraw <- datRaw  [ , idRaw ]
	if ( ! setequal(idrec, idraw) ) {
		stop ( "catPbc: IDs in datasets are not all identical." )
	} else {

		# sort IDs
		datRec  <- datRec [ order(idrec) , ]
		datRaw   <- datRaw  [ order(idraw) , ]
		
		# exclude context vars from subunits and values
  if ( length(context.vars != 0) ) {
    if (any(!is.na (match(context.vars, subunits$subunit)))){
      subunits <- subunits[-which(subunits$subunit %in% context.vars), ]
    }
    if (any(!is.na (match(context.vars, values$subunit)))){
      values   <- values[-which(values$subunit %in% context.vars), ]
    }  
	}

    # make inputs
    recodeinfo <- makeInputRecodeData (values = values, subunits = subunits)
    varinfo    <- .makeVarinfoRaw (values, subunits)

#		Kontextvariablen ausschließen
    if(is.numeric(context.vars)) {
      context.vars <- colnames(datRaw)[context.vars]
    }
    keep.varsRaw <- setdiff(colnames(datRaw), context.vars)
    keep.varsRec <- setdiff(colnames(datRec), c(context.vars, paste(context.vars, "R", sep = "")))

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
			validCodes <- names(valueTypes)  [ valueTypes == "vc" ]
      valuesToNA <- c( "mbd", "mci", names(valueTypes) [ valueTypes %in% c("mbd", "mci") ] )
			dat.vv [ dat.vv %in% valuesToNA] <- NA

			# Häufigkeitsverteilung der Codes, ohne mbd & mci
			tvv <- table( dat.vv )
			kat.vv <- names(tvv)
			additionalCodes <- setdiff(validCodes, kat.vv)
      if (length(additionalCodes) > 0){
        addCodes <- rep(0, length(additionalCodes))
        names(addCodes) <- additionalCodes
        tvv <- c(tvv, addCodes)
        tvv <- tvv[order(names(tvv))]
        kat.vv <- sort(names(tvv))
      }
			tvv1 <- tvv / sum(tvv)

			for (bb in seq ( along = kat.vv) ){
				#                bb <- 3
				l.bb <- c( var.vv  , kat.vv[bb] , sum(tvv) , tvv[bb] , tvv1[bb] ,
				  cor( 1 * ( dat.vv == kat.vv[bb] ) , rel.score , use = "complete.obs" ) ,
				  recodeinfo[[ var.vv ]]$values [[kat.vv[bb]]]
				  )
				dfr1 <- rbind( dfr1 , l.bb )
			}
		}
		dfr1 <- data.frame(dfr1, stringsAsFactors = FALSE)
		colnames(dfr1) <- c( "item" , "cat" , "n" , "freq" , "freq.rel" , "catPbc" , "recodevalue" )
		dfr1 [ , 3:6 ] <- apply(dfr1 [ , 3:6 ], 2, as.numeric )
		dfr2 <- merge(dfr1, subunits[ , c("subunit", "subunitType")], by.x = "item", by.y = "subunit",  all.x = T)

	}

	if ( !is.null(xlsx)) {
			try(write.xlsx(dfr2, file = xlsx, sheetName="catPbc", col.names=TRUE, row.names=TRUE, append=FALSE))
	}

	return(dfr2)
}