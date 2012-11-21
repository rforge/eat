####################################################################################################################
#
# get.equ
# liest Conquest-Äquivalenztabellen (*.equ) als R-Objekte ein
#
# Version: 	1.1.0
# Imports:
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
# 1.1.0 (2011-11-23, NH): bugfix: vergessenes dimensions in dimensionLines umbenannt
#
####################################################################################################################

get.equ <- function(file)
           {funVersion     <- "get.equ_1.1.0"
            input          <- scan(file, what="character", sep="\n", quiet=TRUE)
            dimensionLines <- grep("Equivalence Table for", input)
            eatTools:::sunk(paste(funVersion, ": Find ", length(dimensionLines), " dimension(s).\n", sep=""))
            endTab         <- grep("================", input)
            endTab         <- sapply(dimensionLines, FUN=function(ii) {endTab[endTab>ii][1]})
            equTabsList <- lapply(1 : length(dimensionLines), FUN=function(ii) {
                              part <- crop(input[(dimensionLines[ii]+6) : (endTab[ii]-1)])
                              part <- data.frame(matrix(as.numeric(unlist(strsplit(part," +"))), ncol=3, byrow=T), stringsAsFactors=F)
                              colnames(part) <- c("Score", "Estimate", "std.error")
                              return(part)
                           })
            regrModel  <- grep("The regression model", input)
            itemModel  <- grep("The item model", input)
            stopifnot(length(regrModel) == length(itemModel))
            dimensionNames <- unlist( lapply(dimensionLines, FUN=function(ii) {unlist(lapply(strsplit(input[ii], "\\(|)"), FUN=function(iii){iii[length(iii)]}))}) )
            model       <- lapply(1:length(regrModel), FUN=function(ii) {rbind ( crop(gsub("The regression model:", "", input[regrModel[ii]])), crop(gsub("The item model:", "", input[itemModel[ii]])) ) })
            model       <- do.call("data.frame", args=list(model, row.names=c("regression.model", "item.model"), stringsAsFactors=F))
            colnames(model) <- dimensionNames
            equTabsList$model.specs <- model
            names(equTabsList)[1 : length(dimensionLines)] <- dimensionNames
            return(equTabsList)}

