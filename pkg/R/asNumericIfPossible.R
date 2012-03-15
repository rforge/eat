# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# asNumericIfPossible
# Description: 
# Version: 	0.5.0
# Status: beta
# Release Date: 	2011-12-27
# Author:    Sebastian Weirich
# Change Log:
#
# 2011-12-28 SW
# FIXED: asNumericIfPossible() now allows for vectors and logical columns
# 0000-00-00 AA
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### macht Spalten in data.frames numerisch, wenn es geht (oder liefert TRUE-FALSE, ob es geht)
### set.numeric: soll transformierter Datensatz zurückgegeben werden (DEFAULT), oder TRUE-FALSE-Vektor, ob es geht
### transform.factors: sollen auch Faktorvariablen in metrische Variablen transformiert werden?
### maintain.factor.scores: wenn Faktoren auch transformiert werden sollen, sollen ihre Faktorwerte übernommen werden?


asNumericIfPossible <- function(dat, set.numeric=TRUE, transform.factors=FALSE, maintain.factor.scores = TRUE, verbose=TRUE)   {
            funVersion  <- "asNumericIfPossible_0.5.0"
            originWarnLevel <- getOption("warn")
            wasInputVector  <- FALSE
            if(class(dat) != "data.frame" ) {
              if(verbose == TRUE) {cat(paste(funVersion, ": Warning! Argument of 'asNumericIfPossible' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep=""))}
              dat <- data.frame(dat, stringsAsFactors=FALSE)
              wasInputVector <- ifelse(ncol(dat) == 1, TRUE, FALSE)
            }
            currentClasses <- sapply(dat, FUN=function(ii) {class(ii)})
            summaryCurrentClasses <- names(table(currentClasses))
            if ( verbose == TRUE)   {
               cat(paste(funVersion, ": Current data frame consists of following ",length(summaryCurrentClasses), " classe(s):\n    ",sep=""))
               cat(paste(summaryCurrentClasses,collapse=", ")); cat("\n")
            }
            options(warn = -1)                                                  ### zuvor: schalte Warnungen aus!
            numericable <- sapply(dat, FUN=function(ii)   {
                  n.na.old       <- sum(is.na(ii))
                  transformed    <- as.numeric(ii)
                  transformed.factor <- as.numeric(as.character(ii))
                  n.na.new       <- sum(is.na(transformed))
                  n.na.new.factor <- sum(is.na(transformed.factor))
                  ret            <- rbind(ifelse(n.na.old == n.na.new, TRUE, FALSE),ifelse(n.na.old == n.na.new.factor, TRUE, FALSE))
                  if(transform.factors == FALSE)   {
                     if(class(ii) == "factor")   {
                        ret <- rbind(FALSE,FALSE)
                     }
                  }
                  return(ret)})
            options(warn = originWarnLevel)                                     ### danach: schalte Warnungen zurück in Ausgangszustand
            changeVariables <- colnames(dat)[numericable[1,]]
            changeFactorWithIndices   <- NULL
            if(transform.factors == TRUE & maintain.factor.scores == TRUE)   {
               changeFactorWithIndices   <- names(which(sapply(changeVariables,FUN=function(ii) {class(dat[[ii]])=="factor"})))
               changeFactorWithIndices   <- setdiff(changeFactorWithIndices, names(which(numericable[2,] == FALSE)) )
               changeVariables           <- setdiff(changeVariables, changeFactorWithIndices)
            }
            if(length(changeVariables) >0)   {                                  ### hier werden alle Variablen (auch Faktoren, wenn maintain.factor.scores = FALSE) ggf. geändert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dat$'" , ii , "' <- as.numeric(dat$'",ii, "'), silent=TRUE)" , sep = "" ) } , changeVariables  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(length(changeFactorWithIndices) >0)   {                          ### hier werden ausschließlich FAKTOREN, wenn maintain.factor.scores = TRUE, ggf. geändert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dat$'" , ii , "' <- as.numeric(as.character(dat$'",ii, "')), silent=TRUE)" , sep = "" ) } , changeFactorWithIndices  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(set.numeric==FALSE) {return(numericable[1,])}
            if(set.numeric==TRUE)  {
              if(verbose == TRUE)      {
                 if( sum ( numericable[1,] == FALSE ) > 0 )  {
                     cat(paste("Following ",sum ( numericable[1,] == FALSE )," variable(s) won't be transformed:\n    ",sep=""))
                     cat(paste(colnames(dat)[as.numeric(which(numericable[1,] == FALSE))],collapse= ", ")); cat("\n")
                 }
              }
              if(wasInputVector == TRUE) {dat <- unname(unlist(dat))}
              return(dat)
           }
         }
