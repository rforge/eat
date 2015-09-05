# aggregateData - formerly known as zkdDatasetAggregate
#
# function:    aggregateData(dat, subunits, units, aggregatemissings = "use.default", rename = FALSE, recodedData = TRUE)
#
# Description: rekodiert Datensätze
#
# arguments:
#     dat (data.frame)          ... Datensatz mit ID-Variablen und (mindestens) allen Variablen, die rekodiert werden sollen
#     subunits (data.frame)     ... ZKD-Inputtabelle für Subunits (Subitems), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx
#     units (data.frame)        ... ZKD-Inputtabelle für Units (Items), siehe P:\ZKD\01_Organisation\Konzepte\InputStruktur_Konzept.xlsx
#     aggregatemissings(matrix) ... Wenn default überschrieben werden soll, muss eine Matrix übergeben werden, aus der hervorgeht, was passieren soll, wenn zu aggregierende Subunits Missings haben
#     rename (logical)          ... Sollen Units, die nur ein Subunit haben, mit dem Unit- oder dem Subunit-Namen im aggregierten Datensatz stehen (default = FALSE)
#     recodedData(logical)      ... Soll rekodierter oder Rohdatensatz aggregiert werden (wichtig für Erstellung der Aggregateinfo)
#
# Version: 	1.2.0
# Status: alpha
# Release Date:
# Author:  Nicole Haag, Anna Lenski, Sebastian Weirich
#
# Change Log:
# 2015-03-21 MH: debugged, aggregation rule auf "SUM" defaultet
# 2012-09-05 NH
# CHANGED: removed calls to 'eatTools:::sunk'
# 0000-00-00 AA
#
# * 1.2.0 (2011-11-22, NH): bugfix in aggregatemissings
# * 1.0.0 (2011-11-04, NH): auf ZKD-Inputtabellen angepasst
#              Scoring komplett rausgenommen
#              Option rename eingefügt
# * 0.1.1 (2011-10-08, NH): modularisiert, aggregieren und scoren sind getrennte Funktionen
# * 0.1.0: kein Abbruch bei fehlerhafter Bewertungsvorschrift
# * 0.0.2: Pattern Matching vektorwertig, kosmetische Änderungen (NH)
# * 0.0.4: Aggregierung von Missings variabel nach Definition im Deamon
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### TO DO
# - Pattern aggregation implementieren
# - wenn rename = TRUE: auch nach unrekodiertem Subitemnamen suchen

aggregateData <- function (dat, subunits, units, aggregatemissings = NULL, rename = FALSE, recodedData = TRUE, suppressErr = FALSE, recodeErr = "mci", verbose = FALSE) {

  funVersion <- "aggregateData: "

  if (class(dat) != "data.frame") {
  stop(paste(funVersion, "'dat' is not a data.frame.\n", sep = ""))
  }
  
  if(suppressErr == TRUE){
    if(length(recodeErr != 1)){
      cat("recodeErr does not have a length of 1. err will be recoded to 'mci'.")
      recodeErr <- "mci"
    }
  }

  # make aggregateinfo
  aggregateinfo <- makeInputAggregateData(subunits, units, recodedData = recodedData)

  # reduce aggregateinfo - keep only those units for which at least one subunit is found in dat
  nSubunitsInDat <- lapply(lapply(aggregateinfo, "[[", "subunits"), function(ll) { sum( ll %in% colnames(dat)) })
  aggregateinfo <- aggregateinfo[ which(nSubunitsInDat > 0) ]

  if (length(aggregateinfo) == 0){
	stop("Found none of the specified subitems to aggregate in dataset.\n")
  }

  # define missing aggregation
  if ((is.null(aggregatemissings)|is.data.frame(aggregatemissings)|is.matrix(aggregatemissings)) == FALSE){
	stop("aggregatemissings is neither NULL nor a matrix nor a data.frame.")
  }

  if (is.matrix(aggregatemissings)){
	am <- aggregatemissings
  } else {
	if ( is.data.frame(aggregatemissings)) {
		am <- as.matrix(aggregatemissings[-1])
		dimnames(am) <- list(aggregatemissings[, 1], colnames(aggregatemissings)[-1])
	}
  }
  if ( is.null(aggregatemissings)) {
		am <- matrix(c(
								"vc" , "mvi", "vc" , "mci", "err", "vc" , "vc" , "err",
								"mvi", "mvi", "err", "mci", "err", "err", "err", "err",
								"vc" , "err", "mnr", "mci", "err", "mir", "mnr", "err",
								"mci", "mci", "mci", "mci", "err", "mci", "mci", "err",
								"err", "err", "err", "err", "mbd", "err", "err", "err",
								"vc" , "err", "mir", "mci", "err", "mir", "mir", "err",
								"vc" , "err", "mnr", "mci", "err", "mir", "mbi", "err",
								"err", "err", "err", "err", "err", "err", "err", "err" ),
								nrow = 8, ncol = 8, byrow = TRUE)

		dimnames(am) <-
                list(c("vc" ,"mvi", "mnr", "mci",  "mbd", "mir", "mbi", "err"),
										c("vc" ,"mvi", "mnr", "mci",  "mbd", "mir", "mbi", "err"))
  }

  if(!isSymmetric(am)){
	warning("Matrix used for missing aggregation is not symmetrical. This may lead to unexpected results.")
  }

  # füge eine Spalte und eine Zeile mit "err" an -> egal, was auf "err" trifft, es soll immer "err" rauskommen
  am <- cbind(am, err = "err") ;  am <- rbind(am, err = "err")

  # which subunits should be aggregated?
  unitsToAggregate <- names(aggregateinfo)
  subunitsToAggregate <- unname(unlist(lapply(aggregateinfo, "[[", "subunits")))
  subunitsToKeep      <- setdiff(colnames(dat), subunitsToAggregate)

  # initialisiere aggregierten Datensatz
  datAggregated <- dat[ , subunitsToKeep ]

  ### ACHTUNG: IST EHER EXPERIMENTELL!
  if (rename == TRUE) {
  # prüfen, welche Units nur ein Subunit haben
	if (recodedData == TRUE) {
		oneSubunitUnits <- subunits[subunits$subunitRecoded %in% subunitsToKeep, c("unit", "subunitRecoded")]
		oneSubunitUnits <- oneSubunitUnits [na.omit(match(colnames(dat), oneSubunitUnits$subunitRecoded)), ]
		colnames(datAggregated)[ match(oneSubunitUnits$subunitRecoded, colnames(datAggregated) )] <- oneSubunitUnits$unit
	} else {
		oneSubunitUnits <- subunits[subunits$subunit %in% subunitsToKeep, c("unit", "subunit")]
		oneSubunitUnits <- oneSubunitUnits [na.omit(match(colnames(dat), oneSubunitUnits$subunit)), ]
		colnames(datAggregated)[ match(oneSubunitUnits$subunit, colnames(datAggregated) )] <- oneSubunitUnits$unit
	}
	if(verbose) {cat(paste(funVersion, "Found ", nrow(oneSubunitUnits), " unit(s) with only one subunit in 'dat'. This/these subunit(s) will be renamed to their respective unit name(s).\nUnits ",
          paste(oneSubunitUnits$unit, collapse = ", "), "\n", sep = ""))  }
  }

  # erstelle aggregierten Datensatz der Units, die aggregiert werden
  unitsAggregated <- mapply(aggregateData.aggregate, unitsToAggregate, aggregateinfo, MoreArgs = list(am, dat, verbose = verbose, suppressErr = suppressErr, recodeErr = recodeErr))

 if(!missing(unitsAggregated)){
	datAggregated <- cbind(datAggregated, unitsAggregated, stringsAsFactors = FALSE)
  }

  return(datAggregated)
}

#-----------------------------------------------------------------------------------------------------------
# aggregiert eine einzelne Spalte mit dem bereits vorhandenen Aggregat nach der in aggregatemissings vorgegebenen Vorschrift

.aggmiss <- function ( variable, aggregatedVariable, aggregatemissings) {
	aggregatedVariable <- mapply( function (variable, aggregatedVariable){
			x <- aggregatemissings[ match(aggregatedVariable, rownames(aggregatemissings)) , match(variable, colnames(aggregatemissings))]
		return(x)}, variable, aggregatedVariable)
	return (aggregatedVariable)
}

#-----------------------------------------------------------------------------------------------------------
# aggregiert mehrere Spalten eines Datensatzes nach der in aggregatemissings vorgegebenen Vorschrift

.makeMissingind <- function ( dat, aggregatemissings ) {
	### wenn gültige Werte, setze sie auf "vc"!
	dat <- data.frame(apply(dat, 2, function (ll) {gsub("[[:digit:]]", "vc", ll)}), stringsAsFactors = FALSE)

	# initialisiere missing-Rückgabe
	agg <- dat [ , 1]

	# Missingaggregierung für alle Spalten vornehmen
	for (i in seq(along = dat)) {
		agg <- .aggmiss(dat [ , i] , agg, aggregatemissings)
	}
	return(agg)
}

#-----------------------------------------------------------------------------------------------------------
# findet zu aggregierende Spalten eines Datensatzes und aggregiert sie nach einer vorgegebenen Aggregierungsregel

aggregateData.aggregate <- function(unitName, aggregateinfo, aggregatemissings, dat, verbose = FALSE, suppressErr = suppressErr, recodeErr = recodeErr){

  funVersion <- "aggregateData: "

  unitVars <- aggregateinfo$subunits
  aggRule <- toupper(aggregateinfo$arule)

  # Behandlung der Aggregation Rule
  if ( !exists ( "aggRule" ) ) {
		aggRule <- "SUM"
  }
  if ( is.na ( aggRule ) ) {
		aggRule <- "SUM"
  }
  if (nchar(aggRule) == 0) {
    aggRule <- "SUM"
  }
  if ( !is.character ( aggRule ) ) {
		aggRule <- "SUM"
  }
  # Warnung wenn nicht eine standardmäßige aggRule
  if ( !aggRule %in% c("SUM","MEAN","") ) {
		warning ( paste ( "Unit " , unitName , " has potentially problematic aggregation rule (\"" , aggRule , "\"). Please check.\n" , sep = "" ) )
  }


  ## AGGREGIEREN DER MISSINGS
  
  if(verbose) cat(paste (funVersion, "Aggregate unit ", unitName, "", sep = ""))

  # check: sind alle Subunits vorhanden?
  if (any((unitVars %in% colnames(dat)) == FALSE)) {
  	stop(paste(funVersion, "Subunits", paste(setdiff(unitVars, colnames(dat)), collapse = ", "), "not in 'dat'.\n"))
  }

  # gebe Teildatensatz für i-tes unit
  unitDat        <- dat[ , unitVars]

  # check: Datensatz darf keine NAs enthalten. -> werden in mbd umgewandelt
  na <- which(is.na(unitDat))
    if (length(na) > 0) {
        if(verbose)  cat(paste(funVersion, "Data contains NA values. These values will be converted to 'mbd'.\n", sep = ""))
		unitDat[ is.na(unitDat) ] <- "mbd"
  }

  agg <- .makeMissingind(unitDat, aggregatemissings)

  # initialisiere Rückgabe des aggregierten units
  unitAggregated <- unname(agg)

  options(warn = -1)
  
  ## AGGREGIEREN DER VALIDEN CODES JE NACH REGEL

  # prüfen, ob überhaupt valide Codes in der aggregierten Variable stehen
  unitDat.vc <- unitDat[ unitAggregated == "vc", , drop = FALSE ]
  if ( nrow ( unitDat.vc ) > 0 ) {
		  if( aggRule == "SUM" ) {
			unitAggregated[unitAggregated == "vc"] <- as.character(rowSums(apply(unitDat.vc, 2, as.numeric), na.rm = TRUE))
		  }

		  if( aggRule == "MEAN" ) {
			unitAggregated[unitAggregated == "vc"] <- as.character(rowMeans(apply(unitDat.vc, 2, as.numeric), na.rm = TRUE))
		  }
  }

  # MH 15.03.2015
  # so viele Punkte printen wie Anzahl an Subitems
  if ( verbose ) cat ( paste0 ( paste ( rep ( "." , ncol ( unitDat.vc ) ) , collapse = "" ) , "\n" ) )

 ### wenn agg an irgendeiner Stelle auf "err" gesetzt wird, soll eine Warnung ausgegeben werden
  if(any(agg == "err"))  {
  	cat(paste(funVersion, "Aggregation of missing values for unit ", unitName, " produced 'err' for row(s)", paste(which(agg == "err"), collapse = ", "), ".\n",sep=""))
  }

  if (suppressErr == TRUE) {
#    cat(paste("'err' in unit ", unitName, " will be recoded to 'mci'.\n",sep=""))
    unitAggregated[unitAggregated == "err"] <- recodeErr
  }

  options(warn = 0)

  # pattern aggregation (noch nicht getestet)
  #	if( !aggRule %in% c("SUM","MEAN") )  {
  #	unit.pattern <- apply( unitDat[ unit == "vc" ], 1, paste, collapse = "")
  #	unit [ unit == "vc" ] <- recode(unit.pattern, aggRule)
  #	}

  return(unitAggregated)
}                    