crop <- function ( x , char = " " ) {
	if ( char %in% c ( "\\" , "+" , "*" , "." , "(" , ")" , "[" , "]" , "{" , "}" , "|" , "^" , "$" ) ) {char <- paste ( "\\" , char , sep = "" ) }
	gsub ( paste ( "^" , char , "+|" , char , "+$" , sep = "" ) , "" , x ) }
	
.existsBackgroundVariables <- function(dat, variable )  {
                             if(!is.null(variable))  {
            								 if(is.character(variable))  {
            									 misVariable <- setdiff(variable, colnames(dat))
            									 if(length(misVariable)>0) {cat(paste("Can't find ",length(misVariable)," variable(s) in dataset.\n",sep=""))
            									 cat(paste(misVariable,collapse=", ")); cat("\n"); stop()}
            									 varColumn <- match(variable, colnames(dat))
            								 }
            								 if(is.numeric(variable))   {varColumn <- variable}
                                             return(colnames(dat)[varColumn])
            							 }
                             if(is.null(variable)) {return(NULL)}
                             } 
							 
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
        do.call("rbind", plyr:::llply(seq_along(locations), function(i) {
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

table.unlist <- function(dataFrame)   {
                # if(!exists("rbind.fill.matrix"))  {library(reshape)}
                # if(class(dataFrame) != "data.frame" ) {stop("Argument of 'table.unlist' has to be of class 'data.frame'.\n")}
                if(class(dataFrame) != "data.frame" ) {
                   cat(paste("Warning! Argument of 'table.unlist' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep=""))
                   dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
                }
                column.by.column   <- do.call("rbind.fill.matrix", lapply(dataFrame, FUN=function(ii) {t(table(ii))}) )
                freq.table         <- colSums(column.by.column,na.rm=TRUE)
                return(freq.table)}
	