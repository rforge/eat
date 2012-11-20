# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# table.unlist
# Description: replace buggy 'table(unlist( ... ))'
# Version: 	0.2.0
# Status: beta
# Release Date: 	2011-11-23
# Author:    Sebastian Weirich
# Change Log:
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table.unlist <- function(dataFrame)   {
                funVersion <- "table.unlist_0.2.0"
                ## if(!exists("rbind.fill.matrix"))  {library(reshape)}
                ## if(class(dataFrame) != "data.frame" ) {stop("Argument of 'table.unlist' has to be of class 'data.frame'.\n")}
                if(class(dataFrame) != "data.frame" ) {
                   sunk(paste(funVersion,": Warning! Argument of 'table.unlist' has to be of class 'data.frame'.\n    Object will be converted to data.frame.\n",sep=""))
                   dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
                }
                column.by.column   <- do.call("rbind.fill.matrix", lapply(dataFrame, FUN=function(ii) {t(table(ii))}) )
                freq.table         <- colSums(column.by.column,na.rm=TRUE)
                return(freq.table)}


