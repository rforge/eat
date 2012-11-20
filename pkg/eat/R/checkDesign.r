

checkDesign <- function(dat, booklets, blocks, rotation, sysMis="NA", id="idstud") {
	
	funVersion <- "checkDesign 0.0.2"

	if (is.na(match(id, colnames(dat)))) {
		stop(paste(funVersion, " ID variable '", id, "' not found in dataset.", sep = "")) }

	# Ausgabe, welche Variablen bei Check ignoriert werden:		
	gibsNich <- setdiff(names(dat),c(id,blocks$subunit))
	if (length(gibsNich) > 0) {
		cat(paste(cat(funVersion, " The following variables are not in info (subunits in blocks) but in dataset. \nThey will be ignored during check: \n"), paste(gibsNich, collapse = ", "), sep = ""), "\n")
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
		cases <- rotation$id[rotation$booklet == TH]
		resList <- list()
		resList[["M"]] <- sapply(subunits, .patternCheckM, TH=TH, cases=cases)
		resList[["P"]] <- sapply(subunitsN, .patternCheckP, TH=TH, cases=cases)
		return(resList)
	}
	
	resL <- lapply(booklets$booklet, .bookletPatternCheck)
	names(resL) <- booklets$booklet

	if(all(unlist(resL) == FALSE)) {
		cat(paste(funVersion, "No deviations from design detected! \n"))
	} else {
		cat(paste(funVersion, "Deviations from design detected! \n"))
		if(!all(unlist(resM <- lapply(resL, function(iz) {iz[["M"]]})) == FALSE)) {
			for(ll in names(resL)) {
				if (!all(unlist(resM[[ll]]) == FALSE)) {
					cat(paste(funVersion, "Found sysMis instead of valid codes for booklet", ll, "for variables:\n"))
						for(pp in names(resM[[ll]])) {
							if (resM[[ll]][[pp]][1] != FALSE) {
								cat(paste(pp, " (cases: ", paste(resM[[ll]][[pp]], collapse = ", "), ") \n", sep=""))
							}
						}	
				}
			}
		} 
		if(!all(unlist(resP <- lapply(resL, function(iz) {iz[["P"]]})) == FALSE)) {
			for(ll in names(resL)) {
				if (!all(unlist(resP[[ll]]) == FALSE)) {
					cat(paste(funVersion, "Found valid codes instead of sysMis for booklet", ll, "for variables:\n"))
						for(pp in names(resP[[ll]])) {
							if (resP[[ll]][[pp]][1] != FALSE) {
								cat(paste(pp, " (cases: ", paste(resP[[ll]][[pp]], collapse = ", "), ") \n", sep=""))
							}
						}	
				}
			}
		} 
	}
}