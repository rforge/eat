.existsBackgroundVariables <- function(dat, variable )  {
                             if(!is.null(variable))  {
            								 if(is.character(variable))  {
            									 misVariable <- setdiff(variable, colnames(dat))
            									 if(length(misVariable)>0) {cat(paste("Can't find ",length(misVariable)," variable(s) in dataset.\n",sep=""))
            									 cat(paste(misVariable,collapse=", ")); cat("\n"); stop()}
            									 varColumn <- match(variable, colnames(dat))
            								 }
            								 if(is.numeric(variable))   {
                                if(ncol(dat) < variable ) {stop("Designated column number exceeds number of columns in dataset.\n")}
                                varColumn <- variable
                             }
                           return(colnames(dat)[varColumn])
            							 }
                             if(is.null(variable)) {return(NULL)}
                             }


facToChar <- function ( dataFrame ) {
             if(!"data.frame" %in% class(dataFrame)) {stop()}
             classes <- which( unlist(lapply(dataFrame,class)) == "factor")
             if(length(classes)>0) {
                for (u in classes) {
                     dataFrame[,u] <- as.character(dataFrame[,u]) }}
             return(dataFrame)}


crop <- function ( x , char = " " ) {
	if ( char %in% c ( "\\" , "+" , "*" , "." , "(" , ")" , "[" , "]" , "{" , "}" , "|" , "^" , "$" ) ) {char <- paste ( "\\" , char , sep = "" ) }
	gsub ( paste ( "^" , char , "+|" , char , "+$" , sep = "" ) , "" , x ) }

							 
halve.string <- function (string, pattern, first = TRUE )  {
    # if(!exists("str_split"))   {library(stringr)}
    n <- 2
    if (length(string) == 0)
        return(matrix(character(), nrow = n, ncol = 1))
    string <- stringr:::check_string(string)
    pattern <- stringr:::check_pattern(pattern, string)
    if (!is.numeric(n) || length(n) != 1) {
        stop("n should be a numeric vector of length 1")
    }
    if (n == Inf) {
        stop("n must be finite", call. = FALSE)
    }
    else if (n == 1) {
        matrix(string, ncol = 1)
    }
    else {
        locations <- stringr:::str_locate_all(string, pattern)
        do.call("rbind", plyr::llply(seq_along(locations), function(i) {
            location <- locations[[i]]
            string <- string[i]
            pieces <- 1
            if ( first == TRUE)  {
                  cut <- t(as.matrix(location[1,]))
            } else {cut <- t(as.matrix(location[nrow(location),])) }
            keep <- invert_match(cut)
            padding <- rep("", n - pieces - 1)
            c(str_sub(string, keep[, 1], keep[, 2]), padding)
        }))
    } }

### Hilfsfunktion, ersetzt table(unlist( ... ))
table.unlist <- function(dataFrame, verbose = TRUE)   {
                # if(!exists("rbind.fill.matrix"))  {library(plyr)}
                # if(class(dataFrame) != "data.frame" ) {stop("Argument of 'table.unlist' has to be of class 'data.frame'.\n")}
                if(class(dataFrame) != "data.frame" ) {
                   if(verbose == TRUE ) {cat(paste("Warning! Argument of 'table.unlist' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep=""))}
                   dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
                }
                column.by.column   <- do.call("rbind.fill.matrix", lapply(dataFrame, FUN=function(ii) {t(table(ii))}) )
                freq.table         <- colSums(column.by.column,na.rm=TRUE)
                return(freq.table)}

wo.sind <- function(a,b,quiet=FALSE)
           {b <- data.frame(1:length(b),b,stringsAsFactors=FALSE)               ### zus‰tzliche Syntaxbefehle sind notwendig, damit die Funktion mit missing values umgehen kann.
            if(sum(which(is.na(a)))>0)     {cat("a contains missing values. \n")}
            if(sum(which(is.na(b[,2])))>0) {cat("b contains missing values. \n")}
            if(length(na.omit(a)) > length(unique(na.omit(a))))     {cat("a contains duplicate elements. \n")}
            if(length(intersect(a,b[,2])) == 0) {cat("No common elements in a and b. \n")}
            if(quiet==FALSE) { if(length(intersect(a,b[,2])) > 0) {if(length(setdiff(a,b[,2]))>0)      {cat("Not all Elemente of a included in b. \n")} } }
            a <- unique(a)
            if(sum(which(is.na(a)))>0)     {a <- a[-which(is.na(a))]}           ### Sofern vorhanden, werden missing values aus a entfernt
            b <- na.omit(b)                                                     ### Sofern vorhanden, werden missing values aus b entfernt; aber: Rangplatz der
            reihe <- NULL                                                       ### der nicht fehlenden Elemente in b bleibt erhalten
            if(length(a)>0) {for (i in 1:length(a))
                                 {reihe <- c(reihe,b[,1][a[i]==b[,2]])}
                             if(quiet==FALSE) { cat(paste("Found",length(reihe),"elements.")); cat("\n") }}
            if(length(a)==0) {cat("No valid values in a.\n")}
            return(reihe)}


as.numeric.if.possible <- function(dataFrame, set.numeric=TRUE, transform.factors=FALSE, maintain.factor.scores = TRUE, verbose=TRUE)   {
            originWarnLevel <- getOption("warn")
            wasInputVector  <- FALSE
            if( !"data.frame" %in% class(dataFrame) ) {
              if(verbose == TRUE )  {cat(paste("Warning! Argument of 'as.numeric.if.possible' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep="")) }
              dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
              wasInputVector <- ifelse(ncol(dataFrame) == 1, TRUE, FALSE)
            }
            currentClasses <- sapply(dataFrame, FUN=function(ii) {class(ii)})
            summaryCurrentClasses <- names(table(currentClasses))
            if(verbose == TRUE )  {
               cat(paste("Current data frame consists of following ",length(summaryCurrentClasses), " classe(s):\n    ",sep=""))
               cat(paste(summaryCurrentClasses,collapse=", ")); cat("\n")
            }
            options(warn = -1)                                                  ### zuvor: schalte Warnungen aus!
            numericable <- sapply(dataFrame, FUN=function(ii)   {
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
            options(warn = originWarnLevel)                                     ### danach: schalte Warnungen wieder in Ausgangszustand!
            changeVariables <- colnames(dataFrame)[numericable[1,]]
            changeFactorWithIndices   <- NULL
            if(transform.factors == TRUE & maintain.factor.scores == TRUE)   {
               changeFactorWithIndices   <- names(which(sapply(changeVariables,FUN=function(ii) {class(dataFrame[[ii]])=="factor"})))
               changeFactorWithIndices   <- setdiff(changeFactorWithIndices, names(which(numericable[2,] == FALSE)) )
               changeVariables           <- setdiff(changeVariables, changeFactorWithIndices)
            }
            if(length(changeVariables) >0)   {                                  ### hier werden alle Variablen (auch Faktoren, wenn maintain.factor.scores = FALSE) ggf. ge‰ndert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dataFrame$'" , ii , "' <- as.numeric(dataFrame$'",ii, "'), silent=TRUE)" , sep = "" ) } , changeVariables  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(length(changeFactorWithIndices) >0)   {                          ### hier werden ausschlieﬂlich FAKTOREN, wenn maintain.factor.scores = TRUE, ggf. ge‰ndert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dataFrame$'" , ii , "' <- as.numeric(as.character(dataFrame$'",ii, "')), silent=TRUE)" , sep = "" ) } , changeFactorWithIndices  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(set.numeric==FALSE) {return(numericable[1,])}
            if(set.numeric==TRUE)  {
              if(verbose == TRUE)      {
                 if( sum ( numericable[1,] == FALSE ) > 0 )  {
                     cat(paste("Following ",sum ( numericable[1,] == FALSE )," variable(s) won't be transformed:\n    ",sep=""))
                     cat(paste(colnames(dataFrame)[as.numeric(which(numericable[1,] == FALSE))],collapse= ", ")); cat("\n")
                 }
              }
              if(wasInputVector == TRUE) {dataFrame <- unname(unlist(dataFrame))}
              return(dataFrame)
           }
         }


make.indikator <- function(variable, name.var = "ind", force.indicators = NULL, separate.missing.indikator = c("no","ifany", "always"), sep = "_" )  {
                  separate.missing.indikator <- match.arg(separate.missing.indikator)
                  t.var <- table(variable, useNA = separate.missing.indikator )
                  if(!is.null(force.indicators))  {
                     additional <- setdiff(as.character(force.indicators), names(t.var))
                     if(length(additional) > 0 )  {
                        add <- rep(0,length(additional))
                        names(add) <- additional
                        t.var      <- c(t.var, add)
                     }
                  }
                  ind.i <- data.frame( variable, lapply(names(t.var), FUN = function(iii) {
                           if(!is.na(iii)) {ind.iii  <- which(variable == iii)}
                           if(is.na(iii))  {ind.iii  <- which(is.na(variable))}
                           ret      <- rep(0, length(variable) )
                           if(length(ind.iii)>0)  {ret[ind.iii] <- 1}
                           if(separate.missing.indikator == "no" )  {
                              if(length(which(is.na(variable))) > 0 ) {
                                 ret[which(is.na(variable))] <- NA
                              }
                           }
                           return(ret)}), stringsAsFactors = FALSE )
                  colnames(ind.i)[-1] <- paste(name.var, names(t.var), sep=sep)
                  return(ind.i)}

remove.non.numeric <- function(string)
                      {if(!is.null(dim(string))) {dimension <- dim(string)}
                       splitt <- strsplit(string,"")
                       options(warn = -1)                                       ### warnungen aus
                       splitt <- unlist ( lapply(splitt, FUN=function(ii) {paste( na.omit(as.numeric(ii)),collapse="")}))
                       options(warn = 0)                                        ### warnungen wieder an
                       if(!is.null(dim(string))) {splitt <- matrix(splitt,dimension[1],dimension[2],byrow = FALSE)}
                       return(splitt)}

### entfernt bestimmtes Pattern aus einem String
remove.pattern     <- function ( string, pattern ) {
                      splitt <- strsplit(string, pattern)
                      ret    <- unlist(lapply(splitt, FUN = function ( y ) { paste(y, collapse="")}))
                      return(ret)}

remove.numeric <- function(string)
                      {if(!is.null(dim(string))) {dimension <- dim(string)}
                       splitt <- strsplit(string,"")
                       options(warn = -1)                                       ### warnungen aus
                       splitt <- lapply(splitt, FUN=function(ii) {
                                 a         <- as.numeric(ii)
                                 change    <- which(!is.na(a))
                                 if(length(change)>0) {ii[change] <- ""}
                                 ii        <- paste(ii, collapse="")
                                 return(ii)
                       })
                       options(warn = 0)                                        ### warnungen wieder an
                       splitt <- unlist(splitt)
                       if(!is.null(dim(string))) {splitt <- matrix(splitt,dimension[1],dimension[2],byrow = FALSE)}
                       return(splitt)}
