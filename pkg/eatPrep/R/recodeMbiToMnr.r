# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# recodeMnr
# Description: convert mbi to mnr 
# Version: 	0.1.1
# Status: alpha
# Release Date: 2013-01-10
# Author:  Nicole Haag
#
# Change Log:
# 2013-01-10 MH: Anpassungen, siehe markierte Stellen
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

###############################################################################

recodeMbiToMnr <- function (dat, id, rotation.id = NULL, booklets, blocks, rotation, breaks, nMbi = 2, subunits = NULL, verbose = FALSE){
# browser()
  # check consistency of inputs
  if (nMbi < 1) {
    warning("nMbi needs to be >= 1. It's set to 1.")
	nMbi <- 1
  }
  
  if (is.numeric(id)) {
    id <- colnames(dat)[id]
  }  
  
  # MH 10.01.2013
  # rotation.id defaulten
  if ( is.null ( rotation.id ) ) rotation.id <- "booklet"

  
  if(!is.null(subunits)){
    if(verbose) cat("Use names for recoded subunits.\n")
    
	# MH 10.01.2013: leichte strukturelle Anpassungen zur besseren Übersicht
	na <- is.na(match(blocks$subunit, subunits$subunit))
	if (any( na )){ 
      warning("Found no names for recoded subunit(s) for subunit(s) " , paste(blocks$subunit[ na ], collapse = ", "), 
            "\nThis/Those subunit(s) will be ignored in determining 'mnr'.\n")
      blocks <- blocks[ !na , ]
    }

	# blocks$subunit[na.omit(match(subunits$subunit, blocks$subunit))] <- subunits$subunitRecoded[ match(blocks$subunit, subunits$subunit) ]
	# MH 10.01.2013: Fehler: "Anzahl der zu ersetzenden Elemente ist kein Vielfaches der Ersetzungslänge" 
	# leider raff ich die Zeile nicht ganz
	# gehe mal davon aus dass diejenigen subunits in blocks ersetzt werden sollen durch ihren Rekodierungsnamen (wenn es welche gibt)
	rec <- subunits$subunitRecoded
	names ( rec ) <- subunits$subunit
	blocks$subunit[blocks$subunit %in% names(rec)] <- rec[blocks$subunit]
	# allerdings Problem: was ist bei recodeData=FALSE in automateDataPreparation??
  }
  
  personsWithoutBooklets <- setdiff(dat[ , id], rotation[, id])
  if (length(personsWithoutBooklets) > 0){
    warning("Found no booklet information for cases ", personsWithoutBooklets, ". No recoding will be done for these cases.\n")
  }

  # prepare dataset
  dat.mis <- as.data.frame(sapply(dat, recode, "'mbi'=1; else=0"))
  dat.mis[ , id] <- dat[, id]
  
  # MH 10.01.2013
  # "if" ergänzt
  if ( !rotation.id %in% colnames(dat) ) {
		dat <- merge(dat, rotation, by = id, all.x = T)
		del.rotation.id <- TRUE
  } else {
		del.rotation.id <- FALSE
  }

  bookletsWithoutPersons <- setdiff(booklets$booklet, dat[,rotation.id])
  if (length(bookletsWithoutPersons) > 0){
    # MH 10.01.2013: "paste" ergänzt
	warning(paste ( "Found no response data for booklets", paste( bookletsWithoutPersons, collapse = ", " ), ".\n") )
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
    # MH 10.01.2013: "paste" ergänzt
	warning(paste ( "Found no information about subitems for blocks", paste ( blocksWithoutSubitems , collapse = ", " ), ". No recoding will be done for these blocks.\n") )
  }
  
  missingSubunits <- setdiff(blocks$subunit, colnames(dat))
  if(length(missingSubunits) > 0 ){
    # MH 10.01.2013: "Kritikalität" geändert
	# kein stop, sondern warning, und wird aus blocks$subunit rausgenommen
	# "paste" ergänzt
	warning( paste ( "Found no data for subunits", paste ( missingSubunits, collapse = ", " ), ".") )
	blocks <- blocks[!blocks$subunit %in% missingSubunits,]
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

	blockGrouping <-  mapply(":", blockBeg, blockEnd, SIMPLIFY = FALSE)

	
  # recode mbi booklet-wise
  bookletNames <- unique(booklet.long$booklet)

  # MH 10.01.2013: rotation.id statt "booklet" bei dat
  for (bb in bookletNames) {
    # bb <- bookletNames[1]

    dat.ll <- dat[ which(dat[,rotation.id] %in% bb) , ]
    dat.mis.ll <- dat.mis[ dat[,rotation.id] %in% bb , ]

    # find blocks in booklet
    blocks.bb <- booklet.long[ which(booklet.long$booklet == bb) , ]
    blocks.bb <- blocks.bb$block[ order(blocks.bb$blockPosition) ] 
    
	# group blocks according to breaks
	groupedBlocks <- lapply(seq(along = blockGrouping), function(ii) {blocks.bb[blockGrouping[[ii]]]})

	# MH 10.01.2013: 
	# potentiell fehlerproduzierend kann sein
	# dass bei den Datensatzreduktionen "drop=FALSE" fehlt
	# das kann nicht (immer) gut gehen
	# wird entsprechend ergänzt
	
	seqGroupedBlocks <- seq(along = groupedBlocks)
	for (gg in seqGroupedBlocks){
		# gg <- 2

		# MH 10.01.2013:
		# fürs debuggen werden die Schleifendurchläufe ausgegeben
		cat ( paste ( bb , " " , gg , ": " , sep = "" ) )
		flush.console()
		
		block.bb <- blocks[ which(blocks$block %in% groupedBlocks[[gg]]) , ]
		block.bb$orderme <- match(blocks$block, groupedBlocks[[gg]])[!is.na( match(blocks$block, groupedBlocks[[gg]]))]
		block.bb <- block.bb[order(block.bb$orderme, block.bb$subunitBlockPosition), ]
		dat.bb <- dat.ll[ , block.bb$subunit , drop = FALSE ]
		dat.mis.bb <- dat.mis.ll[ , block.bb$subunit , drop = FALSE ]

		## find variables to be recoded0
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
# browser()		
		# MH 10.01.2013:
		# Fehler bei names(toRecodeList) <- dat.ll[ , id]
		# Ergänzunglänge stimmt nicht
		# identifiziert: es kommt vor dass toRecodeList leer ( list() ) ist
		# warum, kA, müsste verifiziert werden, ob dass ein valider Fall ist
		# jetzt hotfixen: einfach abfangen = nix tun
		
		names(toRecodeList) <- dat.ll[ , id]
		
		# MH 11.01.2013
		# NULL aus toRecodeList raus
		toRecodeList <- toRecodeList[ ! unname ( sapply ( toRecodeList , is.null ) ) ]
		
		if ( length ( toRecodeList ) > 0 ) {
			
			# MH 11.01.2013
			cat ( paste ( "mnr for " , length ( toRecodeList ) , " cases.\n" , sep = "" ) )
			
			for (jj in names(toRecodeList)){
			  if (!is.null(toRecodeList[[jj]])) {
				dat[ dat[ , id] == jj, which(colnames(dat) %in% toRecodeList[[jj]]) ] <- "mnr"
			  # cat( dat[which(dat[ , id]  == jj), id], rev(toRecodeList[[jj]]), "\n")
				dat.mis[ dat.mis[ , id] == jj, which(colnames(dat.mis) %in% toRecodeList[[jj]]) ] <- 2
			  }
			}
		
		} else {
			cat ( paste ( "mnr for 0 cases.\n" , sep = "" ) )	
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
    if(verbose) cat("Recoded 'mbi' to 'mnr' for", nrow(output), "cases.\n")
  } else {
    if(verbose) cat("Found no 'mbi' to be recoded to 'mnr' according to the specifications.\n")
  
  }
  
  # MH 10.01.2013
  # falls rotation.id zu dat geaddet wurde, dann wieder löschen
  # ansonsten war die bereits im Datensatz (und bleibt auch drin)
  if ( del.rotation.id ) {
		dat <- dat[ , !colnames(dat) %in% rotation.id ]
  }
  
  # return(dat[ , - which(colnames(dat) == "booklet")])
  return ( dat )
  
}
