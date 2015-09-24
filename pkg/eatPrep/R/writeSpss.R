################## TO DO #######
# Definition von Missings fuer SPSS

#-----------------------------------------------------------------------------------------
autoQuote <- function (x){ 
paste("\"", x, "\"", sep = "")}

writeSpss <- function (dat, values, subunits, units, filedat = "mydata.txt", filesps = "readmydata.sps",
  missing.rule = list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 ), 
  path = getwd(), sep = "\t", dec = ",", verbose = FALSE) {
  
  funVersion <- "writeSpss: "
  
  varinfo <- makeInputCheckData (values, subunits, units)
  
  # reduce varinfo
  varinfo <- varinfo [ match(colnames(dat), names(varinfo)) ]
  
  if (class(dat) != "data.frame") {
    stop (paste(funVersion, "dat must be a data.frame.", sep = ""))
  }
  
  if (!is.null(path)) {
    filedat <- file.path(path, filedat)
    filesps <- file.path(path, filesps)
  }
  
  # treat missings
  dat <- collapseMissings(dat, missing.rule, items=colnames(dat))
    
  zkdWriteForeignSPSS(dat, varinfo, datafile = filedat, codefile = filesps, 
           missing.rule = list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 ),
           varnames = colnames(dat), sep = sep, dec = dec, verbose = verbose)
  if (verbose) {
    cat(paste(funVersion, "Data values written to ", filedat, "\n", sep = ""))
    cat(paste(funVersion, "Syntax file written to ", filesps, "\n", sep = ""))
  }
}

#-----------------------------------------------------------------------------------------

zkdWriteForeignSPSS <- function(dat, varinfo, datafile, codefile, 
  missing.rule = list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 ),
  varnames = NULL, dec = ",", sep = "\t", verbose = FALSE) {
  
  funVersion <- "writeSpss: "
  
  # make vars numeric (if possible)
  dat <- data.frame(asNumericIfPossible(dat, verbose = FALSE))
  eol <- paste(sep, "\n", sep = "")
  
  # write dataset
  write.table(dat, file = datafile, row.names = FALSE,
      col.names = FALSE, sep = sep, dec = dec, quote = FALSE,
      na = "", eol = eol)
      
  # get varlabels from varinfo
  varlabels <- lapply(varinfo, "[[", "label" )
  varlabels <- varlabels [ match(varnames, names(varlabels)) ]
  if ( any( sapply(varlabels, is.null)) ) {
		ind <- which ( sapply(varlabels, is.null ) )
		if (verbose) cat (paste(funVersion, "Found no variable labels for variable(s) ", paste( varnames[ind], collapse = ", "), ".\n", sep = ""))
		varlabels [ ind ] <- ""
		names (varlabels)  [ ind ]  <- varnames [ ind ]
	}
  
  varlabels <- gsub("\n", " ", varlabels)    

  # make SPSS variable format statements
  varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
  dl.varnames <- varnames  
  
  chv <- sapply(dat, is.character)  
  
  # hier eventuell statt 'is.numeric' 'is.real' ?
  num <- which(sapply(dat, is.numeric))  
  lengths <- sapply(dat , function(ll) { if(is.numeric(ll)) { 
                                      max(nchar(round(na.omit(abs(ll)), digits=0)))
                                    } else {
                                      max(nchar(ll))
                                    }
                                  })
  
  # checken, ob Dezimalstellen vorhanden - wenn ja, dann Format auf Fx.2 -> 2 Dezimalstellen werden angezeigt
  decimals <- sapply(dat , function(ll) { if(is.numeric(ll)) { 
                                       max(nchar(na.omit(abs(ll))))
                                    } else {
                                      max(nchar(ll))
                                    }
                                  })
  varsWithDecimals <-  names(which(lengths != decimals))

    
  if (any(lengths > 255L))
    stop("Cannot handle character variables longer than 255\n")  
  
  if (any(chv)) {
    lengths <- paste("(", ifelse(chv, "A", "F"), lengths, ")", sep = "")
  }  else {
    lengths <- paste ( "(F", lengths, ")", sep = "")
  }   
  
  if (! is.null(varsWithDecimals)){  
    lengths[which(dl.varnames %in% varsWithDecimals)] <- gsub(")", ".2)", lengths[which(dl.varnames %in% varsWithDecimals)], fixed = TRUE) 
  }                 
  dl.varnames <- paste(dl.varnames, lengths)
  
  # write codefile
  if (sep == "\t")
      freefield <- " free (TAB)\n"
  if (sep != "\t")
      freefield <- cat(" free (\"", sep, "\")\n", sep = "")
  cat("DATA LIST FILE=", autoQuote(datafile), freefield,
      file = codefile)
  cat(" /", dl.varnames, ".\n\n", file = codefile, append = TRUE,
      fill = 60, labels = " ")
  cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
  cat(" ", paste(varnames, autoQuote(varlabels), "\n"), ".\n",
      file = codefile, append = TRUE)

  # get value labels from varinfo
  valuesToWrite <- lapply(lapply(varinfo, "[[", "values"), names)
  valuesToWrite <- lapply ( valuesToWrite, function (ll) { setdiff(ll, names(missing.rule))}) 
  valuesToWrite <- valuesToWrite [ match(varnames, names(valuesToWrite)) ]
                                                                            
  if (any(sapply(valuesToWrite, length) > 0) ) {
    
    variablesToLabel <- which(sapply(valuesToWrite, length) > 0)
    variablesToLabel <- names(valuesToWrite)[variablesToLabel]   
    
    cat("\nVALUE LABELS\n", file = codefile, append = TRUE)   
    
    for (v in variablesToLabel) {
      cat(" /", v, "\n", file = codefile, append = TRUE)
      valueLabels <- sapply ( varinfo[[v]]$values, "[[", "label")
      valueLabels <- valueLabels[ which(! names(valueLabels) %in% names(missing.rule)) ]
      valueLabels <- gsub("\n", " ", valueLabels)
      if (any(nchar(valueLabels) > 120L)) {
        cat(paste(funVersion, "Value labels for variable", v , "longer than 120 characters. Only the first 120 characters will be used.\n"))
        valueLabels <- substring(valueLabels, 1, 120)
        }   
      cat(paste("  ", names(valueLabels),
      autoQuote(valueLabels),"\n",  sep = " "), file = codefile,
      append = TRUE)
    }
    cat(" .\n", file = codefile, append = TRUE)
  }
  cat("\nEXECUTE.\n", file = codefile, append = TRUE)

}

