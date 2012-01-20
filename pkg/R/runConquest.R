####################################################################################################################
#
# runConquest ALPHA
# startet Conquest-Lauf                              
#
# Version: 	0.1.0 
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change-Log
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 
####################################################################################################################

runConquest <- function(jobFolder, inputfile,pathConquest,subFolder=NULL)
                  {setwd(jobFolder)
                   #if(missing(pfad.console))                                    ### wird kein Pfad für Konsole angegeben, wird in zwei Standardpfaden gesucht (zuhause und Büro)
                   #  {pfad.console <- "N:/iqb/"                                 ### Untere Zeile: Wenn console.exe nicht in angegebenem Verzeichnis, wird in Alternativverzeichnis gesucht
                   #   if(file.access(file.path(pfad.console,name.console), mode = 0)== -1)
                   #                               {pfad.console <- "C:/Programme/ConQuest/" } }
                   input <- scan(inputfile,what="character",sep="\n",quiet=TRUE)### 25. Januar 2011: Falsche Berechnung durchgeführt. Kontrollinstanzen sind notwendig.
                   weite <- input[c( grep("format", input), grep("Format", input), grep("FORMAT", input))]
                   options(warn = -1)                                           ### untere Zeile: identifiziere die größte Zahl in dieser Zeile!
                   foo   <- sapply(1:nchar(weite), FUN=function(ii){as.numeric(substr(weite,ii,ii))})
                   foo[is.na(foo)] <- " "
                   foo   <- paste(foo, collapse="")                             ### untere Zeile: so viele Spalten sollte es im Datenfile geben!
                   foo   <- na.omit(as.numeric(unlist(strsplit(foo, " +"))),na.rm=T)
                   options(warn = 0)                                            ### untere Zeile: identifiziere Datenfile
                   dat.file <- input[c( grep("datafile", input), grep("Datafile", input), grep("DATAFILE", input))]
                   dat.file.suff <- unlist(strsplit(dat.file,"\\."))
                   dat.file.suff <- dat.file.suff[length(dat.file.suff)]
                   dat.file.suff <- gsub(";","",dat.file.suff)
                   dat.file.suff <- gsub(" +$","",dat.file.suff)
                  if(length(grep("%.", dat.file)) == 1) {dat.file <- unlist(strsplit(inputfile,"\\."))
                                                          dat.file <- paste(dat.file[1:(length(dat.file)-1)],collapse=".")
                                                          dat.file <- paste(dat.file,dat.file.suff,sep=".")}
                   if(length(grep("%.", dat.file)) == 0) {dat.file <- unlist(strsplit(dat.file," "))
                                                          dat.file <- dat.file[length(dat.file)]
                                                          dat.file <- gsub(";","",dat.file)
                                                          dat.file <- gsub(" +$","",dat.file)}
                   lab.file <- input[c( grep("labels", input), grep("Labels", input), grep("LABELS", input))]
                   lab.file.suff <- unlist(strsplit(lab.file,"\\."))
                   lab.file.suff <- lab.file.suff[length(lab.file.suff)]
                   lab.file.suff <- gsub(";","",lab.file.suff)
                   lab.file.suff <- gsub(" +$","",lab.file.suff)
                   if(length(grep("%.", lab.file)) == 1) {lab.file <- unlist(strsplit(inputfile,"\\."))
                                                          lab.file <- paste(lab.file[1:(length(lab.file)-1)],collapse=".")
                                                          lab.file <- paste(lab.file,lab.file.suff,sep=".")}    
                   if(length(grep("%.", lab.file)) == 0) {lab.file <- unlist(strsplit(lab.file," "))
                                                          lab.file <- lab.file[length(lab.file)]
                                                          lab.file <- gsub(";","",lab.file)
                                                          lab.file <- gsub(" +$","",lab.file)}
                   if(!is.null(subFolder$data)) {test <- readLines(file.path(getwd(),subFolder$data,dat.file) )} else { test <- readLines(dat.file) }                                 ### Check: hat der Resultdatensatz eine einheitliche Spaltenanzahl? Muß unbedingt sein!
                   stopifnot(length(table(nchar(test)))==1)
                   if(as.numeric(names(table(nchar(test))) )  != max(foo)) {cat("Serious problem: Number of Columns in data file does not seem to match specified column numbers in cqc file. Please check!\n")}
                   ## lab  <- read.table(lab.file, sep=" ", header=T, stringsAsFactors=F)
                   n.items <- diff(foo)                                         ### obere Zeile: lese Labeldatei ein!
                   ## n.items <- n.items[length(n.items)] + 1
                   
                   ### Test entfernt: Wenn Dimensioanen in labelfiole mit drinstehen, stimmt es nicht mehr überein
                   
                   ## if(nrow(lab) != n.items) {cat("Serious problem: Number of items does not seem to correspond with number of columns in data file. Please check!\n")}
                   #weite <- input[c( grep("score", input), grep("Score", input), grep("SCORE", input))]          ### Checke nun weiterhin, ob nicht nur Itemanzahl im Format-Statement mit Labelfile übereinstimmt, sondern auch Itemanzahl im Score-Statement
                   #options(warn = -1)                                           ### untere Zeile: identifiziere die größte Zahl in dieser Zeile!
                   #foo   <- sapply(1:nchar(weite), FUN=function(ii){as.numeric(substr(weite,ii,ii))})
                   #foo[is.na(foo)] <- " "
                   #foo   <- paste(foo, collapse="")                             ### untere Zeile: so viele Spalten sollte es im Datenfile geben!
                   #foo   <- na.omit(as.numeric(unlist(strsplit(foo, " +"))),na.rm=T)
                   #options(warn = 0)                                            ### untere Zeile: identifiziere Datenfile
                   #n.items.2 <- diff(foo)[length(diff(foo))] + 1
                   #if(n.items.2 != n.items) {cat("Serious problem: Number of items in Format-Statement does not seem to correspond with number of items in score-statement. Please check!\n")}
                   #if(n.items.2 != nrow(lab)) {cat("Serious problem: Number of items in score-Statement does not seem to correspond with number of items in label file. Please check!\n")}
                   ### print( paste(pfad.console,name.console," ",inputfile,sep="") )
                   system(paste(file.path(pathConquest)," ",inputfile,sep=""), intern=T) }
                   ### geht nicht in R.2.12: system(paste(file.path(pfad.console,name.console)," ",inputfile,sep=""), show.output.on.console=F) 
                   ### print("ok")

