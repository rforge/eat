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

recodeMbiToMnr <- function (dat, id, booklets, blocks, rotation, nMbi = 2){
  
  # check consistency of inputs
  if (nMbi < 1) {
    stop("nMbi needs to be >= 1")
  }
  personsWithoutBooklets <- setdiff(dat[ , id], rotation$idstud)
  if (length(personsWithoutBooklets) > 0){
    cat("Found no booklet information for cases ", personsWithoutBooklets, ". No recoding will be done for these cases.\n")
  }

  # prepare dataset
  dat.mis <- as.data.frame(sapply(dat, recode, "'mbi'=1; else=0"))
  dat.mis[ , id] <- dat[, id]
  dat <- merge(dat, rotation, by.x = id, by.y = "idstud", all.x = T)

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
  
  
  blockNames <- unique(booklet.long$block)

  for (bb in blockNames) {
    # bb <- blockNames[1]
    ll <- unique(booklet.long$booklet[which(booklet.long$block  == bb)])
    dat.ll <- dat[ which(dat$booklet %in% ll) , ]
    dat.mis.ll <- dat.mis[ dat$booklet %in% ll , ]

    block.bb <- blocks[ which(blocks$block == bb) , ]
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
        flush.console()
        dat.mis[ dat.mis[ , id] == jj, which(colnames(dat.mis) %in% toRecodeList[[jj]]) ] <- 2
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


