####################################################################################################################
#
# get.wle
# liest Conquest-Personenparameterfiles (*.wle) als R-Objekte ein
# Funktion eignet sich sowohl für Dateien, die WLEs als auch MLEs enthalten
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
# CHANGED: use as.numeric.if.possible() in get.wle()
# 2011-11-30 SW
# FIXED: problem with multidimensional WLEs and first cases without answers re-repaired
# 0000-00-00 AA
# 30.11.2011, SW: problem with multidimensional WLEs and first cases without answers re-repaired
#                 (conventions in Version, 1.1.0, Archiv)
# 25.11.2011, SW: 'cat' durch 'sunk' ersetzt
# 03.10.2011, NH:  zu 1.0.0 (Benennungen Input und Output geändert, entsprechen jetzt ZKD-Konvention
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 30.12.2010, SW: korrekte Benennung der Spalten auch bei WLE-Dateien 
#                 mit integrierter ID
# 20.12.2010, SW: Funktion ermöglicht auch das Auslesen von WLE-Dateien 
#                 mit integrierter ID
#
####################################################################################################################

get.wle <- function (file) {
               funVersion <- "get.wle_1.3.0"
               input <- readLines (file)
               input <- crop(input)
               input <- strsplit(input, " +")
               n.spalten <- max ( sapply(input,FUN=function(ii){ length(ii) }) )
               n.wle <- (n.spalten-1) / 4                                       ### Spaltenanzahl sollte ganzzahlig sein.
               input <- as.numeric.if.possible(data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow=F), stringsAsFactors=F), set.numeric = TRUE, verbose = FALSE)
               col.min.na <- which( rowSums(is.na(input)) == min(rowSums(is.na(input))))[1]### Zeile mit den am wenigsten fehlenden Elementen
               col.real.numbers <- which(input[col.min.na,] != round(input[col.min.na,],digits=0))
               cat(paste(funVersion,": Found valid WLEs of ", nrow(na.omit(input))," person(s) for ", length(col.real.numbers)/2, " dimension(s).\n",sep=""))
               namen.1 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("n.solved","n.total")}))
               namen.2 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("wle","wle.se")}))
               namen.1 <- paste(namen.1,rep(1:(length(namen.1)/2),each=2),sep=".")# obere Zeile: benenne nun!
               namen.2 <- paste(namen.2,rep(1:(length(namen.2)/2),each=2),sep=".")
               namen   <- c(namen.1,namen.2)
               colnames(input)[1:2] <- c("case","ID")
               if(length(col.real.numbers) == 1 ) {colnames(input)[(ncol(input)- length(namen)+1): ncol(input)] <- namen }
               return(input)}
               
