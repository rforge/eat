# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# recodeMnr
# Description: convert mbi to mnr 
# Version: 	0.1.0
# Status: alpha
# Release Date: 2012-08-31
# Author:  Nicole Haag
#
# Change Log:
# 0000-00-00 AA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

###############################################################################

recodeMbiToMnr <- function (dat, id, booklets, blocks, rotation, breaks, nMbi = 2, subunits = NULL){
  
  # check consistency of inputs
  if (nMbi < 1) {
    stop("nMbi needs to be >= 1")
  }
  
  if (is.numeric(id)) {
    id <- colnames(dat)[id]
  }  
  
  if(!is.null(subunits)){
    cat("Use names for recoded subunits.\n")
    if (any(is.na(match(blocks$subunit, subunits$subunit)))){ 
      cat("Found no names for recoded subunit(s) for subunit(s)" , paste(blocks$subunit[which(is.na(match(blocks$subunit, subunits$subunit)))], collapse = ", "), 
            "\nThis/Those subunit(s) will be ignored in determining 'mnr'.\n")
      blocks <- blocks[ - which(is.na(match(blocks$subunit, subunits$subunit))), ]
    }
    blocks$subunit[na.omit(match(subunits$subunit, blocks$subunit))] <- subunits$subunitRecoded[ match(blocks$subunit, subunits$subunit) ]
  }
  
  personsWithoutBooklets <- setdiff(dat[ , id], rotation[, id])
  if (length(personsWithoutBooklets) > 0){
    cat("Found no booklet information for cases ", personsWithoutBooklets, ". No recoding will be done for these cases.\n")
  }

  # prepare dataset
  dat.mis <- as.data.frame(sapply(dat, recode, "'mbi'=1; else=0"))
  dat.mis[ , id] <- dat[, id]
  dat <- merge(dat, rotation, by = id, all.x = T)

  bookletsWithoutPersons <- setdiff(booklets$booklet, dat$booklet)
  if (length(bookletsWithoutPersons) > 0){
    cat("Found no response data for booklets", bookletsWithoutPersons, ".\n")
    booklets <- booklets [ - which(booklets$booklet %in% bookletsWithoutPersons) , ]
  }
  
  booklet.long <- melt(booklets, id.var = "booklet", na.rm = T)
  colnames(booklet.long) <- c("booklet", "blockPosition", "block")
  booklet.long <- booklet.long[ booklet.long$block != "", ]
  booklet.long$block <- paste(booklet.long$block)
  if (length(setdiff(blocks$block, unique(booklet.long$block))) < 0){
    blocks <- blocks[ - which(blocks$block %in% setdiff(blocks$block, unique(booklet.long$block))), ]
  } 
  
  blocksWithoutSubitems <- setdiff(unique(booklet.long$block), blocks$block)
  
  if (length(blocksWithoutSubitems) > 0){
    cat("Found no information about subitems for blocks", blocksWithoutSubitems, ". No recoding will be done for these blocks.\n")
  }
  
  missingSubunits <- setdiff(blocks$subunit, colnames(dat))
  if(length(missingSubunits) > 0 ){
    stop("Found no data for subunits", missingSubunits, ".")
  }

	# create sequences to group blocks together according to breaks	
	nBlocks <- ncol(booklets)-1
	blockBeg <- c(breaks-1, breaks[length(breaks)]+ 1)
	if(blockBeg[1] != 1) 
		blockBeg[1] <- 1 
	if(blockBeg[length(blockBeg)] > nBlocks)
		blockBeg <- blockBeg[- length(blockBeg) ]
	stopifnot(length(blockBeg) == length(breaks)+1)

	blockEnd <- breaks
	if (blockEnd[length(blockEnd)] != ncol(booklets)-1) 
		blockEnd <- c(blockEnd, nBlocks)
	stopifnot(length(blockEnd) == length(breaks)+1)

	blockGrouping <-  mapply(":", blockBeg, blockEnd, SIMPLIFY = F)

  # recode mbi booklet-wise
  bookletNames <- unique(booklet.long$booklet)

  for (bb in bookletNames) {
    # bb <- bookletNames[1]
    dat.ll <- dat[ which(dat$booklet %in% bb) , ]
    dat.mis.ll <- dat.mis[ dat$booklet %in% bb , ]

    # find blocks in booklet
    blocks.bb <- booklet.long[ which(booklet.long$booklet == bb) , ]
    blocks.bb <- blocks.bb$block[ order(blocks.bb$blockPosition) ] 
    
	# group blocks according to breaks
	groupedBlocks <- lapply(seq(along = blockGrouping), function(ii) {blocks.bb[blockGrouping[[ii]]]})

	for (gg in seq(along = groupedBlocks)){
		# gg <- 2
		
		block.bb <- blocks[ which(blocks$block %in% groupedBlocks[[gg]]) , ]
		block.bb$orderme <- match(blocks$block, groupedBlocks[[gg]])[!is.na( match(blocks$block, groupedBlocks[[gg]]))]
		block.bb <- block.bb[order(block.bb$orderme, block.bb$subunitBlockPosition), ]
		dat.bb <- dat.ll[ , block.bb$subunit ]
		dat.mis.bb <- dat.mis.ll[ , block.bb$subunit ]

		## find variables to be recoded
		nBlockSubunits  <- ncol(dat.bb)
		subunitSequence <- colnames(dat.bb)
		mbiList <- apply(dat.mis.bb, 1, function(ii){ which(ii > 0) })
		toRecodeList <- lapply(mbiList, function(xx) {
						toRecode <- NULL
						if ( nBlockSubunits %in% xx & length(xx) != nBlockSubunits ){
						  lastResponse <- max(setdiff(seq(1:(nBlockSubunits - 1)), xx))
						  if (lastResponse < (nBlockSubunits - (nMbi-1))){
							firstRecode <- lastResponse + 1
							toRecode <- subunitSequence[firstRecode:nBlockSubunits]
						  }
						}
					return(toRecode)
					})

		## recode selected variables
		names(toRecodeList) <- dat.ll[ , id]
		for (jj in names(toRecodeList)){
		  if (!is.null(toRecodeList[[jj]])) {
			dat[ dat[ , id] == jj, which(colnames(dat) %in% toRecodeList[[jj]]) ] <- "mnr"
		  # cat( dat[which(dat[ , id]  == jj), id], rev(toRecodeList[[jj]]), "\n")
			dat.mis[ dat.mis[ , id] == jj, which(colnames(dat.mis) %in% toRecodeList[[jj]]) ] <- 2
		  }
		}
	}
}

  
  recodedSubitems <- apply(dat.mis, 1, function(ll) { names(ll)[which(ll == 2)]})
  if (any(sapply(recodedSubitems, length) > 0)){
    names(recodedSubitems) <- dat[ , id]
    recodedSubitems <- recodedSubitems[which(sapply(recodedSubitems, length) > 0)]
    ncolsOutput <- max(sapply(recodedSubitems, length))
    output <- matrix("", length(recodedSubitems), ncolsOutput)
    for (ii in seq(along = recodedSubitems)){
      output[ii, 1:length(recodedSubitems[[ii]]) ] <- sort(recodedSubitems[[ii]])
    }
    output <- data.frame(id = names(recodedSubitems), output, stringsAsFactors = F)
    attr(dat, "recodedSubitems") <- output
    cat("Recoded 'mbi' to 'mnr' for", nrow(output), "cases.\n")
  } else {
    cat("Found no 'mbi' to be recoded to 'mnr' according to the specifications.\n")
  
  }
   
  return(dat[ , - which(colnames(dat) == "booklet")])

}
