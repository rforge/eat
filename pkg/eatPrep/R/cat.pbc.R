catPbc <- function(datRaw, datRec, idRaw, idRec, context.vars = NULL, values, subunits, file.name = NULL, verbose = F) {

	# Pruefen, ob IDs in beiden Datensaetzen uebereinstimmen
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

#		Kontextvariablen ausschliessen
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
	    
    # Variablen mit nur missings ausschliessen
    if(any(colMeans(is.na(datRaw)) == 1)){
      raus <- colnames(datRaw)[colMeans(is.na(datRaw)) == 1]
      datRaw <- datRaw[ , -which(colnames(datRaw) %in% raus)]
      datRec <- datRec[ , -which(colnames(datRec) %in% paste0(raus, "R"))]
    }

    vars <- setdiff(colnames(datRaw), idRec)

    # relative Punktzahl
		options(warn=-1)
		rel.score1 <- datRec[ , which( colnames(datRec) %in% subunits$subunitRecoded ) ]
		rel.score1 <- apply(rel.score1, 2, as.numeric )
		rel.score <- rowMeans(  rel.score1, na.rm=T )

		# Kategorientrennschaerfen berechnen
		dfr1 <- NULL

		for (vv in seq( along = vars ) ){
		#    vv <- 35
			var.vv <- vars [vv]
			dat.vv <- datRaw[ , which( colnames(datRaw) == var.vv  ) ]
			valueTypes <- lapply ( varinfo[[vars[vv]]]$values, "[[", "type")
			validCodes <- names(valueTypes)  [ valueTypes == "vc" ]
      valuesToNA <- c( "mbd", "mci", names(valueTypes) [ valueTypes %in% c("mbd", "mci") ] )
			dat.vv [ dat.vv %in% valuesToNA] <- NA
			
			# check: gibt es im rekodierten Datensatz mnr-Codes?
			datRec.vv <- datRec[ , which( colnames(datRec) == subunits$subunitRecoded[subunits$subunit == var.vv]  ) ]
			if(any(na.omit(datRec.vv) == "mnr")) {
        mnrValues <- names(table(dat.vv[datRec.vv == "mnr" & !is.na(datRec.vv)]))
        dat.vv[datRec.vv == "mnr" & !is.na(datRec.vv)] <- as.numeric(names(which(valueTypes == "mnr")))
        if(verbose == TRUE){
          cat(paste0("Replace mnr-values coded ", paste(mnrValues, collapse = ", "),
              " in datRaw for variable ", var.vv, " with code ", names(which(valueTypes == "mnr")), "\n"))
        }
      }
      
			# Haeufigkeitsverteilung der Codes, ohne mbd & mci
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

	if ( !is.null(file.name)) {
			try(write.csv2(dfr2, file = file.name, col.names=TRUE, row.names=FALSE))
	}

	return(dfr2)
}