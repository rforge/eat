####################################################################################################################
#
# readConquestOutput
# liest von Conquest erzeugte Outputs (plausible values (*.pv), Outputfiles (*.shw)
# 	und Personenparameterfiles (*.wle)) als R-Objekte ein
#
# Version: 	1.3.0
# Imports:
# Published:
# Author:  Sebastian Wurster, Sebastian Weirich
#
#
# Change log:
# 2011-11-29 SW/MH
# CHANGED: modified results structure in readConquestOutput
# 0000-00-00 AA
# 23.09.2011 SW: DIRTY HACK geändert 
# 14.09.2011 SW: n.valid und p-Werte eingebaut
# 26.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 26.08.2011 SW: DIRTY HACK dsc
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#	14.07.2011 MH: pbc ergänzt
#
####################################################################################################################


### DIF.var              ... optional: Name der DIF-Variablen (NICHT Spaltennummer!)

# Aufbau:
# 1. Einlesen der Conquest Dateien
# 2. Definition der Listenbestandteile
# 3. Ergebnisse zu Liste results zusammenfassen

###########
# TO DO
###########
# - Beschreibung der Listeneinträge die die Funktion ausgibt
# - Itemparameter:
#	- Unterscheidung von Gruppen einführen
#	- cat: Unterteilung in Kategorienamen
# 	- dif: Unterteilung in DIF-Kategorie
# - Personenparameter:
# 	-
# - Modellinformationen:
#	-

### TEST ###
# jobFolder <- "P:/ZKD/06_Test"
# subFolder <- list()
# subFolder$data <- "../07_temp"
# name.analyse <- "toast"
# dataName <- "daten_toast.sav"


readConquestOutput <- function (jobFolder, subFolder = NULL, item.grouping, name.analyse, p.model.name = NULL , DIF.var = NULL, group.names = NULL, dataName = NULL) {
	
	funVersion <- "readConquestOutput_1.3.0"
    if (is.null(dataName)) {
        dataName <- paste(name.analyse, ".dat", sep = "")
    }
    if (missing(jobFolder)) {
        cat(paste(funVersion, ": No jobFolder specified.\n", sep = ""))
        stop()
    }
    if (missing(name.analyse)) {
        cat(paste(funVersion, ": No 'name.analyse' specified.\n", sep = ""))
        stop()
    }
	
	# define file names
    if (is.null(subFolder$out)) {
        shwFile     <- file.path(jobFolder, paste(name.analyse, ".shw", sep = ""))
        wleFile     <- file.path(jobFolder, paste(name.analyse, ".wle", sep = ""))
        pvFile      <- file.path(jobFolder, paste(name.analyse, ".pvl", sep = ""))
        itnFile     <- file.path(jobFolder, paste(name.analyse, ".itn", sep = ""))
        dscFile.pv  <- file.path(jobFolder, paste(name.analyse, "_pvl.dsc", sep = ""))
        dscFile.wle <- file.path(jobFolder, paste(name.analyse, "_wle.dsc", sep = ""))
    } else {
        shwFile     <- file.path(jobFolder, subFolder$out, paste(name.analyse, ".shw", sep = ""))
        wleFile     <- file.path(jobFolder, subFolder$out, paste(name.analyse, ".wle", sep = ""))
        pvFile      <- file.path(jobFolder, subFolder$out, paste(name.analyse, ".pvl", sep = ""))
        itnFile     <- file.path(jobFolder, subFolder$out, paste(name.analyse, ".itn", sep = ""))
        dscFile.pv  <- file.path(jobFolder, subFolder$out, paste(name.analyse, "_pvl.dsc", sep = ""))
        dscFile.wle <- file.path(jobFolder, subFolder$out, paste(name.analyse, "_wle.dsc", sep = ""))
    }
	
	
	# get number of Dimensions and IDs from Conquest data set 
    
	dimensions <- getDimensionNames(lab.file = file.path(jobFolder, paste(name.analyse, ".lab", sep = "")), jobFolder = jobFolder, name.analyse = name.analyse)
	nDimensions <- nrow(dimensions)
	
    idCols <- identifyIdCols(file.path(jobFolder, paste(name.analyse, ".cqc", sep = "")))
    	
	if (is.null(subFolder$data)) {
        IDs <- substr(readLines(file.path(jobFolder, dataName)), idCols[1], idCols[2])
    } else {
        IDs <- substr(readLines(file.path(jobFolder, subFolder$data, dataName)), idCols[1], idCols[2])
    }
	
 
	
	# read shw file
    isShw <- file.exists(shwFile)
    dif.term <- "empty"
    if (isShw == TRUE) {
        if (!is.null(DIF.var)) {
            dif.term <- paste("item*", DIF.var, sep = "")
            cat(paste(funVersion, ": ", DIF.var, " was specified as DIF variable. Treat '", dif.term, "' as DIF term.\n", sep = ""))
            shw <- get.shw(file = shwFile, dif.term = dif.term)
        }
        if (is.null(DIF.var)) {
            shw <- get.shw(shwFile)
        }
    } else {
        cat(paste(funVersion, ": Found no .shw file.\n", sep = ""))
    }
	
	# read wle file
    isWle <- file.exists(wleFile)
    if (isWle == TRUE) {
        wle.dat <- get.wle(wleFile)
		if(is.null(wle.dat$ID))   {
		   wle.dat$ID <- IDs
		}	
    } else {
        cat(paste(funVersion, ": Found no .wle file.\n", sep = ""))
    }
    
	# read pv file
	isPv <- file.exists(pvFile)
    if (isPv == TRUE) {
        pv.dat <- get.plausible(pvFile)
		if(is.null(pv.dat$ID))   {
		   pv.dat$ID <- IDs
		   if (nrow(pv.dat) != length(IDs)) {
               stop(paste(funVersion, ": Number of cases in datafile does not correspond with number of cases in pvl file.\n", sep = ""))
			}
		}	
    } else {
        cat(paste(funVersion, ": Found no .pvl file.\n", sep = ""))
    }
    
	# read itn file
	isItn <- file.exists(itnFile)
    if (isItn == TRUE) {
        itn <- get.itn(itnFile)
    } else {
        cat(paste(funVersion, ": Found no .itn file.\n", sep = ""))
    }
  
	# read dsc file for plausible values
	isDescPv <- file.exists(dscFile.pv)
    if (isDescPv == TRUE) {
	    sunk(paste(funVersion,": Read descriptives of plausible values.\n",sep=""))
        dsc.pv <- get.dsc(dscFile.pv)
    } else {
        cat(paste(funVersion, ": Found no .dsc file for plausible values.\n", 
            sep = ""))
    }
    
	# read dsc file for wles
	isDescWle <- file.exists(dscFile.wle)
    if (isDescWle == TRUE) {
        sunk(paste(funVersion,": Read descriptives of WLEs.\n",sep=""))
        dsc.wle <- get.dsc(dscFile.wle)
    } else {
        cat(paste(funVersion, ": Found no .dsc file for WLEs.\n", 
            sep = ""))
    }
    
	
	# check consistency of itn and shw files 
	itn.u <- itn[-which(duplicated(itn$item.name)), ]
    if (!identical(shw$item$item.name, itn.u$item.name)) {
        cat(paste(funVersion, ": Found different items in .itn file and .shw file.\n", sep = ""))
        inShwAndItn <- intersect(shw$item$item.name, itn.u$item.name)
        itnOnly <- setdiff(itn.u$item.name, inShwAndItn)
        if (length(itnOnly) > 0) {
            cat(paste(length(itnOnly), " items are not in .shw file. Remove these items from .itn file:\n", sep = ""))
            cat(paste(paste(itnOnly, collapse = ", "), "\n"))
            itn.u <- itn.u[-match(itnOnly, itn.u$item.name), ]
        }
		
        shwOnly <- setdiff(shw$item$item.name, inShwAndItn)
        if (length(shwOnly) > 0) {
            cat(paste(length(shwOnly), " items are not in .itn file. Remove these items from .shw file.:\n", sep = ""))
            cat(paste(paste(shwOnly, collapse = ", ")), "\n")
            shw$item <- shw$item[-match(shwOnly, shw$item$item.name), ]
        }
    }
	
	# define value for missing coefficients
    mis <- NULL
	
	# -----------------------------------------------------------------------
	# write item output list

	
	nItems <- length(shw$item$item.name)
	allItems <- vector(nItems, mode = "list")
	names(allItems) <- shw$item$item.name
	
	for (i in seq(along = shw$item$item.name)) {
		allItems[[shw$item$item.name[i]]] <- list(
			n.valid      = itn.u$n.valid[i], 
	  p            = itn.u$p[i], 
			a            = mis, 
			b            = shw$item$ESTIMATE[i], 
			b.adj        = mis, 	
	  c            = mis, 
			d            = mis, 
			b.se         = shw$item$ERROR[i], 
			infit        = shw$item$infit[i], 
	  infit.ci.lb  = shw$item$infit.ci.lb[i], 
			infit.ci.ub  = shw$item$infit.ci.ub[i], 
	  infit.t      = shw$item$infit.t[i], 
			outfit       = shw$item$outfit[i], 
	  outfit.ci.lb = shw$item$outfit.ci.lb[i], 
			outfit.ci.ub = shw$item$outfit.ci.ub[i], 
	  outfit.t     = shw$item$outfit.t[i], 
			pbc          = itn.u$pbc[i], 
			b.eval       = mis, 
	  infit.eval   = mis, 
			pbc.eval     = mis, 
			eval.num     = mis, 
	  eval         = mis, 
	  cat = list(
				cat.pbc  = mis, 
				freq     = mis, 
				freq.rel = mis
				),
	  q3 = list(),				
			dif = list(
				dif.est      = shw[[dif.term]]$abs.dif[i], 
		dif.se       = shw[[dif.term]]$ERROR[i], 
				dif.ci.lb.90 = shw[[dif.term]]$ci.lb.90[i], 
		dif.ci.ub.90 = shw[[dif.term]]$ci.ub.90[i], 
				dif.ci.lb.95 = shw[[dif.term]]$ci.lb.95[i], 
		dif.ci.ub.95 = shw[[dif.term]]$ci.ub.95[i], 
				dif.ci.lb.99 = shw[[dif.term]]$ci.lb.99[i], 
		dif.ci.ub.99 = shw[[dif.term]]$ci.ub.99[i], 
				dif.eval     = mis
				)
			)
	}
		

	pvCols <- grep ( "pv", colnames(pv.dat))
	nPV <- max(as.numeric(sapply(strsplit(colnames(pv.dat)[pvCols], "_"), "[[", 2 ) ))
    
	# -----------------------------------------------------------------------
	# write person output list

	# allPerson <- vector( length = nDimensions, mode = "list")
	# names(allPerson) <- dimensions [ , 2]
	
	# for (dd in 1:nDimensions) {
	
		# dimPVcols <- grep( paste ( "pv.", dimensions[dd, 2], sep = ""), colnames(pv.dat))
		
		# nPersons <- length(IDs)
		# dimPerson <- vector(nPersons, mode = "list")
		# names(dimPerson) <- IDs
    	
		# for (j in seq(along = IDs)) {
            # dimPerson[[IDs[j]]] <- list(
			# n.solved = wle.dat[wle.dat$case == j, paste("n.solved.", dd, sep = "")], 
			# n.total  = wle.dat[wle.dat$case == j, paste("n.total.", dd, sep = "")], 
			# wle      = wle.dat[wle.dat$case == j, paste("wle.", dd, sep = "")], 
			# wle.se   = wle.dat[wle.dat$case == j, paste("wle.se.", dd, sep = "")], 
			# eap      = pv.dat[pv.dat$case == j, paste("eap.", dimensions[dd, 2], sep = "")], 
			# eap.se   = pv.dat[pv.dat$case == j, paste("eap.se.", dimensions[dd, 2], sep = "")], 
			# pv       = lapply(pv.dat[j, dimPVcols], function(ii) {ii})
			# )
			##change pv names: remove dimension name
			# names(dimPerson[[IDs[j]]]$pv) <- paste ( "pv.", seq( 1:nPV ), sep = "")
        # }
        # allPerson[[dimensions[dd, 2]]] <- dimPerson
    # }
	
	# Dimensionsschleife
	.fun1 <- function ( dim.name , dim.num , wle.dat , pv.dat ) {
			wle <- cbind ( "ID"=wle.dat [ , "ID" ] ,  wle.dat [ , grepl ( paste ( "\\." , dim.num , "$" , sep = "" ) , colnames ( wle.dat ) ) ] , stringsAsFactors = FALSE )
			eap <- cbind ( "ID"=pv.dat [ , "ID" ] ,  pv.dat [ , grepl ( paste(dim.name,"$",sep="")  , colnames ( pv.dat ) ) ] , stringsAsFactors = FALSE )
			pv <- cbind ( "ID"=pv.dat [ , "ID" ] ,  pv.dat [ , grepl ( paste ( "\\." , dim.name , "_\\d+$" , sep = "" ) , colnames ( pv.dat ) ) ] , stringsAsFactors = FALSE )
		
			# Personenschleife
			.fun2 <- function ( id , wle.dat , pv.dat ) {
					wlel <- as.list( unlist( wle[ wle$ID==id,-1 ] ) )
					names ( wlel ) <- gsub ( paste ( "\\." , dim.num , "$" , sep = "" ) , "" , names ( wlel ) )
					
					eapl <- as.list( unlist( eap[ eap$ID==id, -1 ] ) )
					names ( eapl ) <- c ( "eap" , "eap.se" )
					
					pvl <- as.list ( unlist( pv[ pv$ID==id, -1 ] ) )
				    names ( pvl ) <- paste ( "pv." , gsub ( "^.*(\\d+$)" , "\\1" , names ( pvl ) ) , sep = "" )
					
					pvl <- list ( "pv" = pvl )
					
					do.call ( "c" , list ( wlel , eapl , pvl ) )
					
			}
			ret <- mapply ( .fun2 , wle.dat$ID , MoreArgs = list ( wle.dat , pv.dat ) , SIMPLIFY = FALSE )
			names(ret) <- wle.dat$ID
			return(ret)
	}
	allPerson <- mapply ( .fun1 , dimensions [ , 2] , seq ( along = dimensions [ , 2] ) , MoreArgs = list ( wle.dat , pv.dat ) , SIMPLIFY = FALSE )
	

	### Komplett-Struktur zusammenschustern
	# da bei ConQuest immer nur ein Gruppenname keine Schleife dafür, haha	
	.fun <- function ( dim.vec , dim.names , allItems , p.model.name , allPerson ) {
			ret <- c ( list(allItems [ dim.vec == 1 ]) , allPerson[dim.names] )
			names ( ret ) <- c("item","person")
			ret <- list ( ret )
			names ( ret ) <- p.model.name
			return ( ret )
	}
	alles <- mapply ( .fun , item.grouping[,-1,drop=FALSE] , colnames(item.grouping[,-1,drop=FALSE]) , MoreArgs = list ( allItems , p.model.name , allPerson ) , SIMPLIFY = FALSE )

	alles <- list ( alles )
	names ( alles ) <- name.analyse
	
####### Hier weiter optimieren 
# (Sebastian)	
	
	scales <- vector( nDimensions, mode = "list")
	names(scales) <- dimensions [ , 2]
	
	for (ss in 1:nDimensions)	{
        scales[[dimensions[ss, 2]]] <- list(
			pv     = list(pv.mean = dsc.pv[[1]]$aggregates[grep("Average",dsc.pv[[1]]$aggregates$dimension), "mean"], 
			pv.se  = dsc.pv[[1]]$aggregates[grep("Error", dsc.pv[[1]]$aggregates$dimension), "mean"]), 
			wle    = list(wle.mean = dsc.wle[[1]]$single.values[1, "mean"], 
			wle.se = dsc.wle[[1]]$aggregates[1, "mean"]))
    }

	
    # results <- list(item = item, person = allPerson, scales = scales)
    
# str(results)
	
	return(alles)
}




