####################################################################################################################
#
# get.wle
# liest Conquest-Personenparameterfiles (*.wle) als R-Objekte ein
# Funktion eignet sich sowohl f�r Dateien, die WLEs als auch MLEs enthalten
#
# Version: 	1.3.0
# Depends: gdata
# Imports:
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
# Change log:
# 2011-12-28 SW
# CHANGED: use asNumericIfPossible() in get.wle()
# 2011-11-30 SW
# FIXED: problem with multidimensional WLEs and first cases without answers re-repaired
# 0000-00-00 AA
# 30.11.2011, SW: problem with multidimensional WLEs and first cases without answers re-repaired
#                 (conventions in Version, 1.1.0, Archiv)
# 25.11.2011, SW: 'cat' durch 'eatTools:::sunk' ersetzt
# 03.10.2011, NH:  zu 1.0.0 (Benennungen Input und Output ge�ndert, entsprechen jetzt ZKD-Konvention
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 30.12.2010, SW: korrekte Benennung der Spalten auch bei WLE-Dateien 
#                 mit integrierter ID
# 20.12.2010, SW: Funktion erm�glicht auch das Auslesen von WLE-Dateien 
#                 mit integrierter ID
#
####################################################################################################################

get.wle <- function (file) {
			   funVersion <- "get.wle_1.3.0"
               input <- readLines (file)
               input <- crop(input)
               input <- strsplit(input, " +")
               n.spalten <- max ( sapply(input,FUN=function(ii){ length(ii) }) )
               input <- data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow=F), stringsAsFactors=F)
			   if ( n.spalten %% 2 == 0 ) mk <- colnames (input)[-2] else mk <- colnames (input)
			   input <- set.col.type ( dat=input , col.type=list("numeric.if.possible"=mk) )
			   col.min.na <- which( rowSums(is.na(input)) == min(rowSums(is.na(input))))[1]### Zeile mit den am wenigsten fehlenden Elementen
               col.numeric <- which ( sapply(input, FUN=function(ii) {class(ii)}) == "numeric" )
			   col.real.numbers <- na.omit(unlist ( lapply (col.numeric , FUN= function(ii) { ifelse(input[col.min.na,ii] == round(input[col.min.na,ii]), NA, ii)}) ) )
               cat(paste(funVersion,": Found valid WLEs of ", nrow(na.omit(input))," person(s) for ", length(col.real.numbers)/2, " dimension(s).\n",sep=""))
               namen.1 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("n.solved","n.total")}))
               namen.2 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("wle","wle.se")}))
               namen.1 <- paste(namen.1,rep(1:(length(namen.1)/2),each=2),sep=".")# obere Zeile: benenne nun!
               namen.2 <- paste(namen.2,rep(1:(length(namen.2)/2),each=2),sep=".")
               namen   <- c(namen.1,namen.2)
               colnames(input)[1:2] <- c("case","ID")
               if(length(col.real.numbers) > 0 ) {colnames(input)[(ncol(input)- length(namen)+1): ncol(input)] <- namen }
               return(input)}
               
