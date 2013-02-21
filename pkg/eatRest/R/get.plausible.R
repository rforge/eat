####################################################################################################################
#
# get.plausible
# liest von Conquest erzeugte plausible values (*.pv) als R-Objekte ein
#
# Version: 	2.5.0
# Depends: reshape
# Imports: reshape
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
#
# Change log:
#
# 2011-12-05 SW
# FIXED: get.plausible() now reads files with negatives values < -10
# 0000-00-00 AA
#
#
# 25.11.2011, SW: "cat" durch "eatTools:::sunk" ersetzt
# * zu 1.0.2 (2011-10-04, NH): bugfix: kann jetzt wieder ein- und mehrdimensionale Skalierungen lesen
# * zu 1.0.1 (2011-10-03, NH): liest Dimensionsnamen aus lab file, wenn lab file im Arbeitsordner liegt.
#
# 20.10.2011 SW/MH: auf development gesetzt da buggy
# 20.10.2011 MH: library statement auskommentiert
# 12.10.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 14.01.2011, SW: Funktion übernimmt nun auch IDs, sofern sie im von Conquest
#                 erzeugten pv-File enthalten sind.
#
####################################################################################################################

get.plausible <- function (file, verbose = FALSE) {
                    input           <- scan(file, what = "character", sep = "\n", quiet = TRUE)
                    input           <- crop(gsub("-"," -",input) )
                    input           <- strsplit(input," +")                     ### Untere Zeile gibt die maximale Spaltenanzahl
                    n.spalten       <- max ( sapply(input,FUN=function(ii){ length(ii) }) )
                    input           <- data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow=F), stringsAsFactors=F)
                    pv.pro.person   <- sum (input[-1,1]==1:(nrow(input)-1) )    ### Problem: wieviele PVs gibt es pro Person? Kann nicht suchen, ob erste Ziffer ganzzahlig, denn das kommt manchmal auch bei Zeile 7/8 vor, wenn entsprechende Werte = 0.0000
                    n.person        <- nrow(input)/(pv.pro.person+3)            ### Anzahl an PVs pro Person wird bestimmt anhand der Übereinstimmung der ersten Spalte mit aufsteigenden 1,2,3,4...
                    weg             <- c(1, as.numeric( sapply(1:n.person,FUN=function(ii){((pv.pro.person+3)*ii-1):((pv.pro.person+3)*ii+1)}) ) )
                    cases           <- input[(1:n.person)*(pv.pro.person+3)-(pv.pro.person+2),1:2]
                    input.sel       <- input[-weg,]
                    n.dim           <- dim(input.sel)[2]-1                      ### Anzahl der Dimensionen
                    if(verbose == TRUE) {
                       cat(paste("Found",n.person,"person(s) and",n.dim,"dimension(s).\n"))
                       cat(paste("Found",pv.pro.person,"plausible values for each person and each dimension.\n"))
                    }
                    ID              <- input[  (pv.pro.person + 3) *  (1:n.person) - (pv.pro.person + 2) ,2]
                    ### suche ggf. Dimensionsnamen im Lab.file
	                  lab.file        <- gsub ( "\\.[^\\.]+$" , ".lab", file )
	                  dimNames        <- NULL
                    if(!file.exists(lab.file)) {
                       if(verbose == TRUE) { cat(paste("Expected label file '",lab.file,"' was not found. Dimension(s) will be labeled by default as 'dim'.\n",sep="")) }
                    }  else {
                       dimNames <- getDimnamesFromLabfile (lab.file = lab.file )
                       stopifnot(length(dimNames) == 0 | length(dimNames) == n.dim )
    	              }
	                  if (is.null(dimNames)) {
                        dimNames <- paste("dim.",1:n.dim,sep="")
                    }
                    colnames(input.sel) <- c("PV.Nr", dimNames)
                    input.sel[,1]   <- gsub( " ", "0", formatC(input.sel[,1],width = max(nchar(input.sel[,1]))))
                    input.sel$ID    <- rep(ID, each = pv.pro.person)
                    is.na.ID        <- FALSE
                    if(is.na(input.sel$ID[1])) {                                ### wenn keine ID im PV-File, wird hier eine erzeugt (Fall-Nr), da sonst reshapen misslingt
                       is.na.ID        <- TRUE                                  ### Die ID wird später wieder gelöscht. Um das machen zu können, wird Indikatorvariable erzeugt, die sagt, ob ID fehlend war.
                       input.sel$ID    <- rep( 1: n.person, each = pv.pro.person)
                    }
                    input.melt      <- melt(input.sel, id = c("ID", "PV.Nr") , stringsAsFactors = FALSE)
                    input.wide      <- data.frame( case = gsub(" ", "0",formatC(as.character(1:n.person),width = nchar(n.person))) , cast(input.melt, ... ~ variable + PV.Nr) , stringsAsFactors = FALSE)
                    colnames(input.wide)[-c(1:2)] <- paste("pv.", rep(dimNames, each = pv.pro.person), "_", rep(1:pv.pro.person, n.dim), sep="")
                    weg.eap         <- (1:n.person)*(pv.pro.person+3) - (pv.pro.person+2)
                    input.eap    <- input[setdiff(weg,weg.eap),]                ### nimm EAPs und deren Standardfehler und hänge sie an Datensatz - all rows that have not been used before
                    input.eap    <- na.omit(input.eap[,-ncol(input.eap),drop=F])### find EAPs and posterior standard deviations
                    stopifnot(ncol(input.eap) ==  n.dim)
                    input.eap    <- lapply(1:n.dim, FUN=function(ii) {matrix(unlist(as.numeric(input.eap[,ii])), ncol=2,byrow=T)})
                    input.eap    <- do.call("data.frame",input.eap)
                    colnames(input.eap) <- paste(rep(c("eap","eap.se"),n.dim), rep(dimNames, each = 2), sep = ".")
                    PV           <- data.frame(input.wide,input.eap, stringsAsFactors = FALSE)
                    numericColumns <- setdiff(1:ncol(PV), grep("^ID$", colnames(PV))) ### alle außer ID-Spalte sollen numerisch werden
                    if(is.na.ID == TRUE) {PV$ID <- NA}
                    for (ii in numericColumns) {PV[,ii] <- as.numeric(as.character(PV[,ii]))  }
                    return(PV)}

