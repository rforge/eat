####################################################################################################################
#
# checkLink
# ueberprueft die Verlinkung der Items untereinander
#
# Version: 	0.1.0
# Depends:  car
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
#
####################################################################################################################

checkLink <- function(dat, na = NA, verbose = TRUE)   {
             if(!is.na(na))  {
               na <- which(is.na(dat))
               if(length(na)>0)  {
                  cat(paste("Warning: '",na,"' was specified to denote 'sysmis' in the data. ",length(na)," 'NA'-values were found in the dataset anyway. \n         Hence, ",na," and 'NA' will be handled as 'sysmis'.\n",sep=""))
               }
               # if(!exists("recode")) {library(car)}
               dat <- as.data.frame(lapply(dat, FUN=function(ii) {recode(ii, paste(na,"= NA",collapse="; ") ) } ) )
             }
             non.missing.cases <- lapply(dat, FUN=function(ii) {which(!is.na(ii))})
             all.cases <- non.missing.cases[[1]]
             i <- 2
             total.abbruch     <- FALSE
             while( (i < length(non.missing.cases) + 1 ) & !total.abbruch )  {
                  if(length( intersect(all.cases,non.missing.cases[[i]])) > 0 )  {
                     all.cases <- unique(c(all.cases, non.missing.cases[[i]] ) )
                  }  else   {
                     overlap        <- FALSE
                     remain.columns <- length(non.missing.cases) + 1 - i
                     ii             <- 1
                     while (overlap == FALSE & ii < remain.columns )  {
                           non.missing.cases <- non.missing.cases[c(setdiff(1:length(non.missing.cases),i),i)]
                          if(length( intersect(all.cases,non.missing.cases[[i]])) > 0 ) {overlap <- TRUE}
                           ii <- ii + 1
                     }
                     if (overlap == FALSE) {total.abbruch <- TRUE}
                     if (overlap == TRUE)  {all.cases <- unique(c(all.cases, non.missing.cases[[i]] ) ) }
                  }
                  i <- i + 1
             }
             if (length(all.cases) != nrow(dat))   {
                if (verbose == TRUE) {cat("WARNING! Dataset is not completely linked.\n") }
                return(FALSE)
             }
             if (length(all.cases) == nrow(dat))   {
                if (verbose == TRUE) {cat("Dataset is completely linked.\n") }
                return(TRUE)
             }  }