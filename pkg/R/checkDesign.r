

checkDesign <- function(dat, booklets, blocks, rotation, sysMis="NA", id="idstud") {
	
	funVersion <- "checkDesign 0.0.1"
	
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

	# Für jede Person Sysmispattern checken
	if(sysMis=="NA") {
		.patternCheckM <- function(ID) {
			subunits <- .subunitsInBooklet(rotation$booklet[rotation$id == ID])
			cc <- subunits[which(is.na(dat[match(ID, dat[,id]),match(subunits, names(dat))]))]
			if(length(cc) > 0) {return(cc)} else {return(FALSE)}
		}
		.patternCheckP <- function(ID) {
			subunits <- .subunitsInBooklet(rotation$booklet[rotation$id == ID])
			dd <- names(dat)[-match(c(subunits,id), names(dat))][which(!is.na(dat[match(ID, dat[,id]),-na.omit(match(c(subunits,id), names(dat)))]))]
			if(length(dd) > 0) {return(dd)} else {return(FALSE)}
		}
	} else {
		.patternCheckM <- function(ID) {
			subunits <- .subunitsInBooklet(rotation$booklet[rotation$id == ID])
			cc <- subunits[which(dat[match(ID, dat[,id]),match(subunits, names(dat))] == sysMis)]
			if(length(cc) > 0) {return(cc)} else {return(FALSE)}
		}
		.patternCheckP <- function(ID) {
			subunits <- .subunitsInBooklet(rotation$booklet[rotation$id == ID])
			dd <- names(dat)[-match(c(subunits,id), names(dat))][c(which(dat[match(ID, dat[,id]),-match(c(subunits,id), names(dat))] != sysMis),which(is.na(dat[match(ID, dat[,id]),-match(c(subunits,id), names(dat))])))]
			if(length(dd) > 0) {return(dd)} else {return(FALSE)}
		}
	}
	
	xx <- sapply(rotation$id, .patternCheckM)
	yy <- sapply(rotation$id, .patternCheckP)

	if(all(c(unlist(xx),unlist(yy)) == FALSE)) {
		cat(paste(funVersion, "No deviations from design detected! \n"))
	} else {
		cat(paste(funVersion, "Deviations from design detected! \n"))
		if(!all(unlist(xx) == FALSE)) {
			for(ll in names(xx)) {
				if (xx[[ll]][1] != FALSE) {
					cat(paste(funVersion, "Found sysMis instead of valid codes for ID", ll, "in variables:\n",paste(xx[[ll]], collapse = ", "),"\n"))
				}
			}
		} 
		if(!all(unlist(yy) == FALSE)) {
			for(ll in names(yy)) {
				if (yy[[ll]][1] != FALSE) {
					cat(paste(funVersion, "Found valid codes instead of sysMis for ID", ll, "in variables:\n",paste(yy[[ll]], collapse = ", "),"\n"))
				}
			}
		}	
	}
}