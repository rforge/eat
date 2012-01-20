####################################################################################################################
#
# read.cqc
# liest von Conquest benutzte Datensätze zurück nach R
#
# Version: 	0.2.0
# Depends: sirt.r von Alex
# Imports:
# Published:
# Author:  Sebastian Weirich
# Maintainer:
#
# Change-Log
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#
####################################################################################################################


read.cqc <- function(dat, cqc, lab)                                             ### liest Conquest-Datenfiles ein. Benötigt eine erweiterte read.fwf2 aus "sirt.r" von Alex (in "Funktion.rsy" integriert).    
            {labels.ii <- NULL; let <- NULL                                     ### cqc- und lab-Argument ist optional. Wenn es nicht definiert wird, versucht die Funktion, das cqc-File über den identischen 
             # if(!exists("read.fwf2")) {source("P:/ZKD/stable/latest/sirtr.r")}### Dateinamen wie das Datenfile (nur mit anderem Suffix) einzulesen. Das Labelfile wird dann aus dem Syntaxfile einzulesen versucht 
             Name   <- paste(unlist( lapply( strsplit(dat, "\\."), function(ll) {ll[-length(ll)]} ) ),collapse=".")
             input  <- scan(dat,what="character",sep="\n",quiet=TRUE)           ### obere Zeile: das mit dem paste etc. ist, damit auch Dateinamen, die mehr als einen Punkt enthalten, erst nach dem letzten Punkt getrennt werden
             if(!missing(cqc)) {syntax <- scan(cqc,what="character",sep="\n",quiet=TRUE) }
             if(missing(cqc))  {syntax <- scan(paste(Name,".cqc",sep=""),what="character",sep="\n",quiet=TRUE) }
             if(!missing(lab)) {label  <- read.table(lab,header=T,stringsAsFactors=F)
                                labels.ii <- label[,2] }
             if(missing(lab)) {ind <- grep("label", syntax)[1]
                                if(length(ind)==0) {cat("Konnte keine Labels identifizieren.\n")}
                                if(length(ind)==1) {ind.1 <- grep("let name",syntax)                             ### enthält Syntax ein "let name"-statement?
                                                    if(length(ind.1)>0) {let <- gsub(";", "", unlist( lapply( strsplit(syntax[ind.1],"=") , function(ll) {ll[length(ll)]} ) ))
                                                                         let <- gsub(" +","",let)}
                                                    ind <- syntax[ind]
                                                    lab <- unlist( lapply( strsplit(ind, " "), function(ll) {ll[length(ll)]} ) )
                                                    suffix <- unlist( lapply( strsplit(lab, "\\."), function(ll) {ll[length(ll)]} ) )
                                                    suffix <- gsub(" +$","", gsub(";", "" , suffix))
                                                    prefix <- ifelse(is.null(let), unlist( lapply( strsplit(lab, "\\."), function(ll) {ll[1]} ) ), let)
                                                    lab <- paste(prefix , suffix, sep=".")
                                                    label  <- read.table(lab,header=T,stringsAsFactors=F)
                                                    labels.ii <- label[,2]}}
             weite <- syntax[c( grep("format", syntax), grep("Format", syntax), grep("FORMAT", syntax))]
             weite <- gsub(";","",weite)
             weite <- strsplit(weite, " +")
             resp  <- c(grep("respons", weite[[1]]),grep("Respons", weite[[1]]))### wo beginnen responses?
             options(warn = -1)                                                 ### zuvor: schalte Warnungen aus!
             numerisch <- unlist( lapply( 1:length(weite[[1]]), function(ll) {as.numeric(substr(weite[[1]][ll],1,1))} ) )
             options(warn = 0)                                                  ### danach: schalte Warnungen wieder an!
             numerisch <- which(!is.na(numerisch))
             resp  <- which(numerisch>resp)
             beginn.ende <- lapply(1:length(numerisch), FUN=function(ii) {unlist(strsplit(weite[[1]][numerisch[ii]],"-"))})
             zwischenraeume <- unlist( lapply(1:(length(beginn.ende)-1), FUN=function(ii) {setdiff( as.numeric(beginn.ende[[ii]][length(beginn.ende[[ii]])]) : as.numeric(beginn.ende[[ii+1]][1]), c(as.numeric(beginn.ende[[ii]][length(beginn.ende[[ii]])]),as.numeric(beginn.ende[[ii+1]][1])))}))
             weg <- NULL                                                        ### obere Zeile: bedeutet, daß die ID etwa von der 1. bis zur 10. Spalte geht, die nächste Variable 
             if(length(zwischenraeume)>0) {weg <- zwischenraeume}               ### aber erst in der 12. Spalte beginnt. Die 11. Spalte müßte dann gelöscht werden!  
             startpunkt  <- as.numeric(beginn.ende[[1]][1])
             if(startpunkt!=1) {input <- substring(input,startpunkt)}
             beginn.ende <- lapply(1:length(beginn.ende), FUN=function(ii){1+diff( c(as.numeric(beginn.ende[[ii]][1]), as.numeric(beginn.ende[[ii]][length(beginn.ende[[ii]])])))    })
             if(as.numeric(beginn.ende[[resp]]) != length(labels.ii)) {cat("Warnung: Anzahl von Variablen in Labels-Datei weicht von definierter Anzahl in cqc-Datei ab.\n")}
             beginn.ende[[resp]] <- rep(1, beginn.ende[[resp]])
             beginn.ende <- unlist(beginn.ende)
             namen.1     <- weite[[1]][numerisch-1]
             daten <- read.fiwifo(dat,format=beginn.ende, columns.to.delete=weg)
             colnames(daten)[1:length(namen.1)] <- namen.1
             colnames(daten)[(ncol(daten)-length(labels.ii) +1 ):length(labels.ii)] <- labels.ii
             return(daten)}
            