### fileName          ... Pfad und Dateiname der SPSS-Datei
### boundary          ... minimale Belegung, ab der Recodierung erfolgen soll
### saveFolder        ... Pfad, wo die Ergebnisdateien (csv-datei und syntaxdatei) abgelegt werden sollen
### nameListe         ... Dateiname fuer csv-datei
### nameSyntax        ... Dateiname fuer Sytaxdatei
#   foo <- fdz(fileName = "n:/Dropbox/Projekte/2014/Aleks/StEG_Datensatz_Eltern_IZA.sav", saveFolder = "n:/Archiv/temp", nameListe = "liste1.csv", nameSyntax = "syntax1.txt")
#   foo <- fdz(fileName = "q:/BT2016/BT/50_Daten/03_Aufbereitet/06_Gesamtdatensatz/BS_LV_Primar_2016_Matchingvorlaeufig_09_erweiterteGadsversion.sav", saveFolder = "N:/archiv/temp", nameListe = "liste2.csv", nameSyntax = "syntax2.txt", exclude = exclude)
fdz <- function ( fileName, boundary = 5, saveFolder = NA, nameListe = NULL, nameSyntax = NULL, exclude = NULL) {
       cat("\nRead data set with labels ... \n"); flush.console()
       mitLab <- read.spss ( fileName, to.data.frame = FALSE, use.value.labels = TRUE, use.missings = TRUE)
       varLab <- attr(mitLab, "variable.labels")
       mitLab <- data.frame ( mitLab )
       cat("\nRead data set without labels ... \n"); flush.console()
       ohnLab <- data.frame ( read.spss ( fileName, to.data.frame = FALSE, use.value.labels = FALSE, use.missings = TRUE))
       cat("\nRead missing definition ... \n"); flush.console()
       mitMis <- data.frame ( read.spss ( fileName, to.data.frame = FALSE, use.value.labels = TRUE, use.missings = FALSE) )
       skala  <- sapply(ohnLab, class)
       tab    <- lapply(mitLab, FUN = function (y ) { table(as.character(y)) } )
       nKatOM <- sapply(tab, FUN = function ( y ) { length(y)})                 ### Anzahl Kategorien (ohne Missingkategorien)
       nValid <- sapply(tab, FUN = function ( y ) { sum(y)})                    ### untere Zeile: Kategorien mit Haeufigkeit kleiner gleich 5, aber groesser als 0!
       freq5  <- sapply(tab, FUN = function ( y ) { length(which(y < (boundary + 1) & y > 0 ))>0 })
       grenze <- ifelse(nrow(mitLab) < 100, nrow(mitLab)/2, 100)
       existVarLab <- nchar(varLab)>0
       existValLab <- unlist(lapply(ohnLab, FUN = function ( yy ) { !is.null(attr(yy, "value.labels"))}))
       liste  <- data.frame ( variable = names(varLab), varLab = unlist(varLab), existVarLab = existVarLab, existValLab = existValLab, skala = unlist(skala), nKatOhneMissings = nKatOM, nValid = nValid, nKl5 = freq5, makeAnonymous = FALSE, recodeToNumeric = FALSE, exclude = FALSE)
       if (!is.null(exclude) ) {
           chk <- setdiff(exclude,liste[,"variable"])
           if ( length(chk)>0) {
              cat(paste0("Warning: Variables '",paste(chk, collapse="', '"), "' from the 'exclude' argument are not available in the data set and will be ignored.\n"))
           }
           liste[na.omit(match(exclude, liste[,"variable"])),"exclude"] <- TRUE
       }
       recode1<- intersect ( which ( liste[,"exclude"] == FALSE), intersect( intersect( which(freq5==TRUE), which ( nKatOM < grenze)), which(liste[,"skala"] %in% c("numeric", "integer" ))))
       recode2<- intersect ( which ( liste[,"exclude"] == FALSE), setdiff ( 1:nrow(liste), which(liste[,"skala"] %in% c("numeric", "integer" ))))
       if (length(recode1)==0 && length(recode2)==0) {
            cat("\nNo recoding necessary.\n")
            return(liste)
       }  else  {
            if(!is.na(saveFolder)) {
                if(dir.exists(saveFolder) == FALSE) {                           ### das Verzeichnis aber nicht existiert, wird es jetzt erzeugt
                   cat(paste("Warning: Specified folder '",saveFolder,"' does not exist. Create folder ... \n",sep="")); flush.console()
                   dir.create(saveFolder, recursive = TRUE)
                }
            }
       }
       snipp1 <- snipp2 <- NULL                                                 ### initialisieren
       if ( length(recode1)>0) {
            cat(paste0("\n   ",length(recode1), " numeric variables with category size <= ",boundary," will be recoded anonymously.\n")); flush.console()
            liste[recode1, "makeAnonymous"] <- TRUE
            toRec <- liste[which(liste[,"makeAnonymous"]==TRUE),]
            snipp1<- unlist(by(toRec, INDICES = toRec[,"variable"], FUN = function ( tr ) {
                     werte <- table(ohnLab[, as.character(tr[["variable"]]) ] ) ### hier: Variablenweise!
                     werteM<- table(mitMis[, as.character(tr[["variable"]]) ] )
                     unter6<- werte[which(werte < (boundary + 1) )]
                     unter6<- data.frame ( Nummer = 1:length(unter6), kategorie = names(unter6), belegung = unter6)
                     aufb  <- do.call("rbind", by(data = unter6, INDICES = unter6[,"Nummer"], FUN = function ( z ) {
                              matchU <- match( as.character(z[["kategorie"]]), names(werte))
                              matchW <- matchU                                  ### hier: werteweise (je Variable)
                              toNA   <- FALSE
                              while ( sum(werte[matchU:matchW])< (boundary + 1) & toNA == FALSE ) { if( (matchW+2)< length(werte))  { matchW <- matchW+1}  else { toNA <- TRUE } }
                              inkl   <- names(werte[matchU:matchW])[-1]         ### wenn zwei Werte mit weniger als 5 Belegungen direkt benachbart sind, muss ggf. nicht
                              if(length(inkl) == 0 ) { inkl <- NA}              ### zweimal recodiert werden, falls beide Kategorien zusammen mehr als 5 Belegungen haben
                              z      <- data.frame ( Nummer = z[["Nummer"]], kategorie = z[["kategorie"]], inkludiert = inkl, toNA = toNA)
                              return(z)}))
                     weg   <- which(aufb[,"kategorie"] %in% aufb[,"inkludiert"])
                     if(length(weg)>0) { aufb <- aufb[-weg,]}
                     if(length(which( aufb[,"toNA"] == TRUE))>0) {              ### Fuer alle, die zu "NA" recodiert werden, muss nur ein Recodierungsstatement, nicht mehrere verfasst werden
                        minNum <- min(aufb[which( aufb[,"toNA"] == TRUE),"Nummer"])
                        aufb[which( aufb[,"toNA"] == TRUE),"Nummer"] <- minNum
                     }
                     misLab<- setdiff(names(werteM), names(werte))              ### Missinglabels fuer Variable
                     recSt1<- c("RECODE", as.character(tr[["variable"]]) )      ### erster Teil des Recodierungsstatements
                     recSt2<- unlist(by(data = aufb, INDICES = aufb[,"Nummer"], FUN = function ( r ) {
                              if(r[1,"toNA"] == FALSE) {
                                 newValue<- r[1,"kategorie"]
                                 recStat1<- paste("(",as.numeric(as.character(r[1,"kategorie"])), " THRU ", max(as.numeric(as.character(r[,"inkludiert"]))), " = ", newValue, ")",sep="")
                              }  else  {
                                 allVal  <- sort(unique(na.omit(c(as.numeric(as.character(r[,"kategorie"])), as.numeric(as.character(r[,"inkludiert"]))))))
                                 if ( length(allVal)>1) {
                                      recStat1<- paste("(",allVal[1], " THRU ", allVal[length(allVal)], " = SYSMIS )",sep="")
                                 }  else  {
                                      recStat1<- paste("(",allVal[1], " = SYSMIS )",sep="")
                                 }
                              }
                              return(recStat1)}))                               ### Syntaxgenerierung: "c:\Users\weirichs\Dropbox\IQB\Projekte\Aleks\Aufbereitung_SUFs_StEG_ak_Elterndaten.sps"
                     recSt3<- c("(ELSE = COPY)", "INTO", paste( as.character(tr[["variable"]]), "_FDZ.\n", sep=""), paste("VARIABLE LABELS ", paste( as.character(tr[["variable"]]), "_FDZ", sep="")," '", varLab[[as.character(tr[["variable"]])]], " (FDZ)'.", sep=""))
                     recSt4<- unlist(by(data = aufb, INDICES = aufb[,"Nummer"], FUN = function ( r ) {
                              if(r[1,"toNA"] == FALSE) {
                                 newValue<- r[1,"kategorie"]
                                 recStat <- paste("VALUE LABELS ", paste( as.character(tr[["variable"]]), "_FDZ ", sep=""), newValue, " 'von ",as.numeric(as.character(r[1,"kategorie"]))," bis ",max(as.numeric(as.character(r[,"inkludiert"])))," (zur Anonymisierung aggregiert (FDZ))'",sep="")
                                 if(length(misLab)>0) {
                                   recStat <- paste(recStat, " ", paste(misLab, paste("'missingLabel_",misLab,"'",sep=""), collapse = " "), ".", sep="")
                                 } else {
                                   recStat <- paste(recStat, ".",sep="")
                                 }
                              }  else  {
                                 recStat <- NULL
                              }
                              return(recStat)}))
                     recSt5<- c ( paste(c("VARIABLE LEVEL ", paste( as.character(tr[["variable"]]), "_FDZ ", sep=""), "(ORDINAL)."), sep="", collapse=""),
                              paste("FORMATS ", paste( as.character(tr[["variable"]]), "_FDZ ", sep=""), "(F3.0).", sep="", collapse=""))
                     if(length(misLab)>0) {
                        recSt5 <- c(recSt5, paste("MISSING VALUES ", paste( as.character(tr[["variable"]]), "_FDZ ", sep=""), "(", paste(misLab, collapse=", "),").",sep="", collapse=""))
                     }
                     recSt6<- "EXECUTE.\n\n"
                     recSt <- c(recSt1, recSt2, recSt3, recSt4, recSt5, recSt6)
                     return(recSt)}))
       }
    ### jetzt werden die nicht-numerischen Variablen zu numerisch umcodiert
       if ( length(recode2)>0) {                                                ### untere zeile: missingwerte auslesen
            cat("\nRead missing definition for character variables ... \n"); flush.console()
            b     <- read_sav(fileName)
            d     <- extract_labels(rawDat = b, type = "SPSS")                  ### achtung, der liest hier bei character-variablen manchmal die missings nicht korrekt aus
            cat(paste0("\n   Recode ",length(recode2), " non-numeric variables into numeric variables.\n")); flush.console()
            liste[recode2, "recodeToNumeric"] <- TRUE
            toRec <- liste[which(liste[,"recodeToNumeric"]==TRUE),]
            snipp2<- unlist(by(toRec, INDICES = toRec[,"variable"], FUN = function ( tr ) {
                     werte <- crop(names(table(ohnLab[, as.character(tr[["variable"]]) ] )))
                     if ( length(setdiff(werte, ""))==0 ) {
                          recSt <- NULL
                     }  else  {
                          miss  <- d[which(d[,"varName"] == as.character(tr[["variable"]])),]
                          wom   <- setdiff ( setdiff (werte, miss[,"value"]), "")## werte ohne missings
                          recSt1<- c("RECODE", as.character(tr[["variable"]]) ) ### erster Teil des Recodierungsstatements
                          oldnew<- data.frame ( old = wom, new = 1:length(wom), stringsAsFactors = FALSE)
                          recSt2<- unlist(by ( data = oldnew, INDICES = oldnew[,"old"], FUN = function (z) { paste0("(",z[["old"]], " = ", z[["new"]], ")") }))
                          recSt3<- c("INTO" , paste0(as.character(tr[["variable"]]), "_FDZ."))
                          recSt4<- paste0("VARIABLE LABELS ", as.character(tr[["variable"]]), "_FDZ '",as.character(tr[["varLab"]]), "'.")
                          recSt5<- paste0("VALUE LABELS ", as.character(tr[["variable"]]), "_FDZ '", paste(oldnew[,"new"], paste0("'",oldnew[,"old"], "'") , collapse=" "), ".")
                          recSt6<- paste0("VARIABLE LEVEL ", as.character(tr[["variable"]]), "_FDZ (NOMINAL).")
                          recSt7<- paste0("FORMATS ", as.character(tr[["variable"]]), "_FDZ (F8.0).")
                          recSt8<- NULL                                         ### initialisieren
                          if(length(na.omit(miss[,"value"]))>0) {
                             recSt8<- paste0("MISSING VALUES ", as.character(tr[["variable"]]), "_FDZ (", paste(na.omit(miss[,"value"]), collapse = ", "), ").")
                          }
                          recSt9<- "EXECUTE."
                          recSt <- c(recSt1, recSt2, recSt3, recSt4, recSt5, recSt6, recSt7, recSt8, recSt9)
                     }
                     return(recSt)}))
       }
       snipp  <- c(snipp1, snipp2)
       if(is.null(nameSyntax)) { nameSyntax <- "syntaxbaustein.txt" }
       if(!is.na(saveFolder)) { write(snipp,  file.path(crop(saveFolder,"/"), nameSyntax)) }
       if(is.null(nameListe))  { nameListe <- "Liste_komplett.csv"}
       write.csv2(liste, file.path(crop(saveFolder,"/"), nameListe), na="")
       return(snipp)  }