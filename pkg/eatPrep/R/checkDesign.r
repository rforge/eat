

checkDesign <- function(dat, booklets, blocks, rotation, sysMis="NA", id="ID", subunits = NULL, verbose = TRUE) {

	funVersion <- "checkDesign 0.0.4"

	if (is.na(match(id, colnames(dat)))) {
		stop(paste(funVersion, " ID variable '", id, "' not found in dataset.", sep = "")) }

	blocks <- eatTools:::set.col.type(blocks, col.type = list ( "character" = names(blocks) ))
	booklets <- eatTools:::set.col.type(booklets, col.type = list ( "character" = names(booklets) ))
	rotation <- eatTools:::set.col.type(rotation, col.type = list ( "character" = names(rotation) ))
	
	if(!is.null(subunits)){
		if(verbose) cat("Use names for recoded subunits.\n")
		if (any(is.na(match(blocks$subunit, subunits$subunit)))){ 
		  if(verbose) cat("Found no names for recoded subunit(s) for subunit(s)" , blocks$subunit[which(is.na(match(blocks$subunit, subunits$subunit)))], 
				"\nThis/Those subunit(s) will be ignored in determining 'mnr'.\n")
		  blocks <- blocks[ - which(is.na(match(blocks$subunit, subunits$subunit))), ]
		}
		blocks$subunit[na.omit(match(subunits$subunit, blocks$subunit))] <- subunits$subunitRecoded[ match(blocks$subunit, subunits$subunit) ]
	}
	
	gibsNich <- setdiff(names(dat),c(id,blocks$subunit))
	if (length(gibsNich) > 0) {
		if(verbose) cat(paste(cat(funVersion, " The following variables are not in info (subunits in blocks) but in dataset. \nThey will be ignored during check: \n"), paste(gibsNich, collapse = ", "), sep = ""), "\n")
		dat <- dat[,-match(gibsNich, names(dat))]
	}
	
	# Welche Items sind in Booklet?
	.subunitsInBooklet <- function(TH) {
		return(unname(unlist(sapply(booklets[which(booklets$booklet == TH),grep("block", names(booklets))], function(BL) {
			return(subset(blocks, blocks$block == BL)$subunit)
		}))))
	}
	
	if(sysMis=="NA") {	
		# sysMis instead of vc (M)
		.patternCheckM <- function(subunit, TH, cases) {		
			cc <- cases[which(is.na(dat[match(cases, dat[,id]),match(subunit, names(dat))]))]
			if(length(cc) > 0) {return(cc)} else {return(FALSE)}
		}		
		# vc instead of sysMis (P)
		.patternCheckP <- function(subunitN, TH, cases) {
			dd <- cases[which(!is.na(dat[match(cases, dat[,id]),match(subunitN, names(dat))]))]
			if(length(dd) > 0) {return(dd)} else {return(FALSE)}
		}
	} else {
		# sysMis instead of vc (M)
		.patternCheckM <- function(subunit, TH, cases) {		
			cc <- cases[which(dat[match(cases, dat[,id]),match(subunit, names(dat))] == sysMis)]
			if(length(cc) > 0) {return(cc)} else {return(FALSE)}
		}	
		# vc instead of sysMis (P)
		.patternCheckP <- function(subunitN, TH, cases) {
			dd <- cases[which(dat[match(cases, dat[,id]),match(subunitN, names(dat))] != sysMis)]
			if(length(dd) > 0) {return(dd)} else {return(FALSE)}
		}
	}

	# Für jedes TH SysmisPattern checken
	.bookletPatternCheck <- function(TH) {
		subunits <- .subunitsInBooklet(TH)
		subunitsN <- setdiff(names(dat), c(subunits, id))
		cases <- rotation[,id][rotation$booklet == TH]
		resList <- list()
		resList[["M"]] <- sapply(subunits, .patternCheckM, TH=TH, cases=cases)
		resList[["P"]] <- sapply(subunitsN, .patternCheckP, TH=TH, cases=cases)
		return(resList)
	}
	
	resL <- lapply(booklets$booklet, .bookletPatternCheck)
	names(resL) <- booklets$booklet

	if(all(unlist(resL) == FALSE)) {
		if(verbose) cat(paste(funVersion, "No deviations from design detected! \n"))
	} else {
		if(verbose) cat(paste(funVersion, "Deviations from design detected! \n"))
		if(!all(unlist(resM <- lapply(resL, function(iz) {iz[["M"]]})) == FALSE)) {
			for(ll in names(resL)) {
				if (any(tt <- unlist(lapply(resM[[ll]], function(gg) gg[1])) != FALSE)) {
					if(verbose) cat(paste(funVersion, "Found for", sum(tt),"variable(s) sysMis instead of valid codes for booklet", ll, ":\n"))
						for(pp in names(resM[[ll]])) {
							if (resM[[ll]][[pp]][1] != FALSE) {
								if(verbose) cat(paste(pp, " (", length(resM[[ll]][[pp]])," case(s): ", paste(resM[[ll]][[pp]], collapse = ", "), ") \n", sep=""))
							}
						}	
				}
			}
		} 
		if(!all(unlist(resP <- lapply(resL, function(iz) {iz[["P"]]})) == FALSE)) {
			for(ll in names(resL)) {
				if (any(tt <- unlist(lapply(resP[[ll]], function(gg) gg[1])) != FALSE)) {
					if(verbose) cat(paste(funVersion, "Found for", sum(tt),"variable(s) valid codes instead of sysMis for booklet", ll, ":\n"))
						for(pp in names(resP[[ll]])) {
							if (resP[[ll]][[pp]][1] != FALSE) {
								if(verbose) cat(paste(pp, " (", length(resP[[ll]][[pp]])," case(s): ", paste(resP[[ll]][[pp]], collapse = ", "), ") \n", sep=""))
							}
						}	
				}
			}
		} 
	}
}