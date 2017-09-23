generate.replicates <- function ( dat, ID, wgt = NULL, PSU, repInd, type )   {
                       if(type %in% c("JK2", "BRR")) { stopifnot(length(PSU) == 1 & length(repInd) == 1 ) } 
                       if(type  == "JK1" ) { if(!is.null(repInd))  { 
                          cat("'repInd' is ignored for 'type = JK1'.\n")
                          repInd <- NULL
                       }  } 
                       allVars     <- list(ID = ID, wgt = wgt, PSU = PSU, repInd = repInd)
                       all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                       dat.i       <- dat[,unlist(all.Names)]
                       if(type %in% c("JK2", "BRR")) { if( !all( names(table(dat.i[,all.Names[["repInd"]]])) == c(0,1)) ) {stop("Only 0 and 1 are allowed for repInd variable.\n")} }
                       zonen       <- names(table(as.character(dat.i[,all.Names[["PSU"]]]) ) )   
                       cat(paste("Create ",length(zonen)," replicate weights according to ",type," procedure.\n",sep=""))
                       missings    <- sapply(dat.i, FUN = function (ii) {length(which(is.na(ii)))})
                       if(!all(missings == 0)) {
                           mis.vars <- paste(names(missings)[which(missings != 0)], collapse = ", ")
                           stop(paste("Found missing value(s) in variable(s) ", mis.vars,".\n",sep=""))
                       }                                                        ### untere Zeile: in JK-2 werden die gewichte nur in der entsprechenden PSU _fuer ein Replicate_ geaendert 
                       reps <- data.frame ( lapply(zonen , FUN = function(ii) { ### in BRR werden die Gewichte in allen Zonen _fuer ein Replicate_ geaendert!
                               rep.ii <- dat.i[,all.Names[["wgt"]]]             ### in JK-1 werden die Gewichte nur in der entsprechenden PSU _fuer alle Replicates_ geaendert 
                               if(type == "JK2")  { rep.ii[dat.i[,all.Names[["PSU"]]] == ii ] <- ifelse(dat.i[ dat.i[,all.Names[["PSU"]]] == ii ,all.Names[["repInd"]]] == 1, 0, 2 * rep.ii[dat.i[,all.Names[["PSU"]]] == ii ] ) }
                               if(type == "BRR")  { rep.ii <- ifelse(dat.i[ ,all.Names[["repInd"]]] == 1, 0, 2 * rep.ii ) }
                               if(type == "JK1")  { 
                                  rep.ii[ which ( dat.i[,all.Names[["PSU"]]] == ii) ] <- 0
                                  rep.ii[ which ( dat.i[,all.Names[["PSU"]]] != ii) ] <- rep.ii[ which ( dat.i[,all.Names[["PSU"]]] != ii) ] *  ( sum(dat.i[,all.Names[["wgt"]]]) / sum (rep.ii))
                               }
                               return(rep.ii) }), stringsAsFactors = FALSE)
                       colnames(reps) <- paste(all.Names[["wgt"]], 1:ncol(reps), sep="_")
                       ret            <- data.frame(dat.i[,all.Names[["ID"]],drop=FALSE], reps, stringsAsFactors = FALSE)
                       attr(ret, "n.replicates") <- length(zonen)
                       return(ret) }

### Wrapper: ruft "eatRep()" mit selektiven Argumenten auf 
jk2.mean <- function(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"),
            PSU = NULL, repInd = NULL, repWgt = NULL, nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups),
            group.differences.by = NULL, group.delimiter = "_", trend = NULL, linkErr = NULL, dependent, na.rm = FALSE, doCheck = TRUE) {
            eatRep(datL =datL, ID=ID , wgt = wgt, type=type, PSU = PSU, repInd = repInd, repWgt = repWgt, toCall = "mean",
                   nest = nest, imp = imp, groups = groups, group.splits = group.splits, group.differences.by = group.differences.by, 
                   trend = trend, linkErr = linkErr, dependent = dependent, group.delimiter=group.delimiter, na.rm=na.rm, doCheck=doCheck)}

### Wrapper: ruft "eatRep()" mit selektiven Argumenten auf 
jk2.table<- function(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"),
            PSU = NULL, repInd = NULL, repWgt = NULL, nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups),
            group.differences.by = NULL, chiSquare = FALSE, correct = TRUE, group.delimiter = "_", trend = NULL, linkErr = NULL, dependent , separate.missing.indicator = FALSE,
            na.rm=FALSE, expected.values = NULL, doCheck = TRUE ) { 
            if ( !is.null(group.differences.by) & chiSquare == FALSE) { 
               chk <- eatRep(datL =datL, ID=ID , wgt = wgt, type=type, PSU = PSU, repInd = repInd, repWgt = repWgt, toCall = "table",
                      nest = nest, imp = imp, groups = groups, group.splits = group.splits, group.differences.by = group.differences.by, correct = correct, 
                      trend = trend, linkErr = linkErr, dependent = dependent, group.delimiter=group.delimiter, separate.missing.indicator=separate.missing.indicator, 
                      expected.values=expected.values, na.rm=na.rm, doCheck=doCheck, onlyCheck= TRUE)
    ### missing handling muss vorneweg geschehen
               isNa<- which ( is.na ( datL[, chk[["dependent"]] ] ))
               if ( length ( isNa ) > 0 ) { 
                    cat(paste0("Warning: Found ",length(isNa)," missing values in dependent variable '",chk[["dependent"]],"'.\n"))
                    if ( separate.missing.indicator == TRUE ) { 
                         stopifnot ( length( intersect ( "missing" , names(table(datL[, chk[["dependent"]] ])) )) == 0 )
                         datL[isNa, chk[["dependent"]] ] <- "missing"
                    }  else  { 
                         if ( na.rm == FALSE ) { stop("If no separate missing indicator is used ('separate.missing.indicator == FALSE'), 'na.rm' must be TRUE if missing values occur.\n")}
                         datL <- datL[-isNa,]
                    }     
               }                                                                ### Ende des missing handlings
               frml<- as.formula ( paste("~ ",chk[["dependent"]]," - 1",sep="") )
               datL[, chk[["dependent"]] ] <- as.character( datL[, chk[["dependent"]] ] )
               matr<- data.frame ( model.matrix ( frml, data = datL) )
               datL<- data.frame ( datL,  matr)
               ret <- do.call("rbind", lapply ( colnames(matr), FUN = function ( dpd ) { 
                      res <- jk2.mean ( datL = datL, ID=chk[["ID"]], wgt=chk[["wgt"]], type = type, PSU = chk[["PSU"]], repInd = chk[["repInd"]], repWgt = repWgt, 
                                        nest = chk[["nest"]], imp = chk[["imp"]], groups = chk[["group"]], group.splits = group.splits, group.differences.by=group.differences.by, 
                                        group.delimiter =group.delimiter, trend = chk[["trend"]], linkErr = chk[["linkErr"]], dependent = dpd, na.rm=na.rm,doCheck = doCheck) 
                      res <- res[which(res[,"parameter"] %in% c("mean", "meanGroupDiff", "wholePopDiff")),]
                      prm <- remove.pattern(string = as.character(res[,"depVar"]), pattern = chk[["dependent"]])
                      ind <- which(res[,"parameter"] %in% c("meanGroupDiff", "wholePopDiff"))
                      if ( length ( ind ) > 0 ) { prm[ind] <- paste ( res[ind,"parameter"], prm[ind], sep=".") }
                      res[,"parameter"] <- prm
                      res[,"depVar"] <- chk[["dependent"]]
                      return(res) } ) )
            }  else  { 
               ret <- eatRep(datL =datL, ID=ID , wgt = wgt, type=type, PSU = PSU, repInd = repInd, repWgt = repWgt, toCall = "table",
                      nest = nest, imp = imp, groups = groups, group.splits = group.splits, group.differences.by = group.differences.by, correct = correct, 
                      trend = trend, linkErr = linkErr, dependent = dependent, group.delimiter=group.delimiter, separate.missing.indicator=separate.missing.indicator, 
                      expected.values=expected.values, na.rm=na.rm, doCheck=doCheck)
            }
            return(ret)}       
            
### Wrapper: ruft "eatRep()" mit selektiven Argumenten auf 
jk2.quantile<- function(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"),
            PSU = NULL, repInd = NULL, repWgt = NULL, nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups),
            group.delimiter = "_", trend = NULL, linkErr = NULL, dependent, probs = seq(0, 1, 0.25),  na.rm = FALSE,
            nBoot = NULL, bootMethod = c("wSampling","wQuantiles") , doCheck = TRUE)  { 
            bootMethod <- match.arg ( bootMethod )
            eatRep(datL =datL, ID=ID , wgt = wgt, type=type, PSU = PSU, repInd = repInd, repWgt = repWgt, toCall = "quantile",
                   nest = nest, imp = imp, groups = groups, group.splits = group.splits, trend = trend, linkErr = linkErr, dependent = dependent,
                   group.delimiter=group.delimiter, probs=probs, na.rm=na.rm, nBoot=nBoot, bootMethod=bootMethod, doCheck=doCheck)}


### Wrapper: ruft "eatRep()" mit selektiven Argumenten auf 
jk2.glm  <- function(datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"),
            PSU = NULL, repInd = NULL, repWgt = NULL, nest=NULL, imp=NULL, groups = NULL, group.splits = length(groups), group.delimiter = "_",
            group.differences.by.wholePop = FALSE, trend = NULL, linkErr = NULL, formula, family=gaussian, forceSingularityTreatment = FALSE, glmTransformation = c("none", "sdY"), doCheck = TRUE, na.rm = FALSE ) { 
            if ( group.differences.by.wholePop == TRUE ) { group.differences.by <- "wholePop" } else {group.differences.by <- NULL}
            eatRep(datL =datL, ID=ID , wgt = wgt, type=type, PSU = PSU, repInd = repInd, repWgt = repWgt, toCall = "glm",
                   nest = nest, imp = imp, groups = groups, group.splits = group.splits, group.differences.by = group.differences.by , trend = trend, linkErr = linkErr, 
                   formula=formula, family=family, forceSingularityTreatment=forceSingularityTreatment, glmTransformation = glmTransformation,
                   group.delimiter=group.delimiter, na.rm=na.rm, doCheck=doCheck)}

### Funktion ist nicht user-level, sondern wird von jk2.mean, jk2.table, jk2.quantile, jk2.glm mit entsprechenden Argumenten aufgerufen
eatRep <- function (datL, ID, wgt = NULL, type = c("JK1", "JK2", "BRR"), PSU = NULL, repInd = NULL, repWgt = NULL, nest=NULL, imp=NULL, 
          toCall = c("mean", "table", "quantile", "glm"), groups = NULL, group.splits = length(groups), group.differences.by = NULL, 
          group.delimiter = "_", trend = NULL, linkErr = NULL, dependent, na.rm = FALSE, forcePooling = TRUE, boundary = 3, doCheck = TRUE,
          separate.missing.indicator = FALSE, expected.values = NULL, probs = NULL, nBoot = NULL, bootMethod = NULL, formula=NULL, family=NULL, 
          forceSingularityTreatment = FALSE, glmTransformation = c("none", "sdY"), correct, onlyCheck = FALSE)    {
          # if(!exists("rbind.fill"))   {library(plyr)}
          checkForPackage (namePackage = "reshape", targetPackage = "eatRep")
          toCall<- match.arg(toCall)
          type  <- match.arg(arg = toupper(type), choices = c("JK1", "JK2", "BRR"))
          glmTransformation <- match.arg(glmTransformation)
          if(forceSingularityTreatment == FALSE & glmTransformation != "none") { 
             cat("'forceSingularityTreatment' was set to 'FALSE'. Please note that 'glmTransformation' is only possible if 'forceSingularityTreatment' is 'TRUE'.\n"); flush.console()
          }
          if(toCall == "glm") {                                                 ### fuer glm muessen abhaengge und unabhaengige Variablen aus Formel extrahiert werden
             dependent  <- as.character(formula)[2]
             independent<- unlist ( sapply( strsplit(as.character(formula)[3], "\\*|\\:|\\+|-|\\(|\\)|\\^")[[1]], crop ) )
             independent<- intersect(independent, colnames(datL))
           }  else { independent <- NULL}
          if ( "wholePop" %in% group.differences.by ) {                         ### bloeder schlechter Hotfix
               group.differences.by <-  setdiff ( group.differences.by, "wholePop")
               if(length(group.differences.by) == 0) { group.differences.by <- NULL }
               wp <- TRUE 
          }  else { wp <- FALSE } 
          if(is.null(groups))  {groups <- "wholeGroup"; datL[,"wholeGroup"] <- 1}## Hotfix 2 
          allVar<- list(ID = ID, wgt = wgt, PSU = PSU, repInd = repInd, repWgt = repWgt, nest=nest, imp=imp, group = groups, trend=trend, linkErr = linkErr, group.differences.by=group.differences.by, dependent = dependent, independent=independent)
          allNam<- lapply(allVar, FUN=function(ii) {.existsBackgroundVariables(dat = datL, variable=ii)})
          na    <- c("isClear", "N_weightedValid", "N_weighted",  "wgtOne")
          naGr  <- c("wholePop", "group", "depVar", "modus", "parameter", "coefficient", "value", "linkErr")
          naInd <- c("(Intercept)", "Ncases", "Nvalid", "R2",  "R2nagel", "linkErr")       
          naGr1 <- which ( allNam[["group"]] %in% naGr )                        ### hier kuenftig besser: "verbotene" Variablennamen sollen automatisch umbenannt werden!
          if(length(naGr1)>0)  {cat("Error: Following name(s) of grouping variables in data set are deemed to be unsuitable due to danger of confusion with result structure:\n"); cat(paste(allNam[["group"]][naGr1], collapse=", ")); cat("\nPlease rename these variable(s).\n"); stop() }
          naInd1<- which ( allNam[["independent"]] %in% naInd )
          if(length(naInd1)>0) {cat("Error: Following name(s) of independent variables in data set are deemed to be unsuitable due to danger of confusion with result structure:\n"); cat(paste(allNam[["independent"]][naInd1], collapse=", ")); cat("\nPlease rename these variable(s).\n"); stop() }
          na2   <- which ( unlist(allNam) %in% na )
          if(length(na2)>0) {cat("Error: Following variable name(s) in data set are deemed to be unsuitable due to danger of confusion with result structure:\n"); cat(paste(unlist(allNam)[na2], collapse=", ")); cat("\nPlease rename these variable(s).\n"); stop() }
    ### Achtung: wenn Funktion nur zum checken genutzt wird, endet sie hier
          if ( onlyCheck == TRUE ) { 
              ret <- allNam
          }  else  { 
    ### wie in 'defineModel': Funktion ruft sich selber auf, wenn Trend bestimmt werden soll und/oder Differenzen zur Gesamtpopulation bestimmt werden sollen.
    ### aeussere Schleife: Trend. check: only two groups in trend variable
              if(!is.null(allNam[["trend"]])) {                                 ### Achtung: hier wenn Trendberechnung geschehen soll 
    ### check: nur zwei Gruppen in Trendvariablen?
                  lev <- sort ( unique(datL[,allNam[["trend"]]]))
                  if(length(lev) != 2) {stop(paste(length(lev), " levels ('",paste(lev, collapse="', '"),"') found for the 'trend' variable '",allNam[["trend"]],"'. 2 levels are allowed.\n",sep=""))}
    ### check: alle Kombinationen von faktoren in beiden datensaetzen? 
                  if (!is.null(allNam[["group"]])) {  
                       foo <- lapply(allNam[["group"]], FUN = function ( gr ) { 
                              ch <- by(data = datL, INDICES = datL[,allNam[["trend"]]], FUN = function ( subdat ) { table(subdat[,gr]) }, simplify = FALSE )
                              if ( !all ( names(ch[[1]]) == names(ch[[2]]))) { stop(paste("Error in grouping variable '",gr,"': Levels do not match. Levels in trend group '",names(ch)[1],"': \n    ", paste(names(ch[[1]]), collapse = ", "),"\nLevels in trend group '",names(ch)[2],"': \n    ", paste(names(ch[[2]]), collapse = ", "),sep="")) } } )
                  }            
    ### repliziere Analyse zweimal, d.h. fuer beide Trendgruppen ... hier single core 
                  if ( "single" == "single" ) { 
                        resT<- by ( data = datL, INDICES = datL[,allNam[["trend"]]], FUN = function ( subdat ) { 
                               if(wp==TRUE) { gdb <- c( group.differences.by, "wholePop") } else { gdb <- group.differences.by }
                               cat(paste("\nTrend group: '",subdat[1,allNam[["trend"]] ], "'\n",sep=""))  
                               do    <- paste ( "foo <- eatRep ( ", paste(names(formals(eatRep)), recode(names(formals(eatRep)), "'trend'='NULL'; 'datL'='subdat'; 'group.differences.by'='gdb'"), sep =" = ", collapse = ", "), ")",sep="")
                               eval(parse(text=do))
                               foo[,allNam[["trend"]]] <- subdat[1,allNam[["trend"]] ]
                               return(foo)}, simplify = FALSE) 
                  }  else {                                                     ### jetzt multicore
                        # if(!exists("detectCores"))   {library(parallel)}
                        spl <- by ( data = datL, INDICES = datL[,allNam[["trend"]]], FUN = function ( subdat ) { return(subdat)}, simplify = FALSE)
                        doIt<- function (laufnummer,  ... ) { 
                               if(!exists("jk2.mean"))  { library(eatRep) }
                               # if(!exists("jk2.mean")) {source("c:/Users/weirichs/Dropbox/R/Funktion.rsy")}
                               cat(paste("\nTrend group '",spl[[laufnummer]][1,allNam[["trend"]] ], "'\n",sep=""))
                               if(wp==TRUE) { gdb <- c( group.differences.by, "wholePop") } else { gdb <- group.differences.by }
                               do    <- paste ( "txt <- capture.output ( foo <- eatRep ( ", paste(names(formals(eatRep)), recode(names(formals(eatRep)), "'trend'='NULL'; 'datL'='spl[[laufnummer]]'; 'group.differences.by'='gdb'"), sep =" = ", collapse = ", "), "))",sep="")
                               eval(parse(text=do))
                               foo[,allNam[["trend"]]] <- spl[[laufnummer]][1,allNam[["trend"]] ]
                               return(list ( res=foo, txt=txt)) }
                        cl  <- makeCluster(2, type = "SOCK")
                        mods<- clusterApply(cl = cl, x = 1:length(spl), fun = doIt)
                        stopCluster(cl)
                        resT<- list( mods[[1]][["res"]], mods[[2]][["res"]])
                        cat(c(mods[[1]][["txt"]], mods[[2]][["txt"]]), sep="\n")
                  }      
    ### 'resT' aufbereiten, Linkingfehler ranmatchen, Trend berechnen ... wenn es keinen linkingfehler gibt, wird er auf 0 gesetzt 
                  ch3  <- data.frame ( do.call("rbind", lapply(resT, dim) ))
                  ch3  <- lapply(ch3, FUN = function ( y ) { if ( length(unique(y)) != 1) {stop("error 1")} } )
                  if (!all(colnames(resT[[1]]) == colnames(resT[[2]]))) {stop("error 2")}
                  resT1<- dcast ( rbind(resT[[1]], resT[[2]]) , as.formula ( paste ( " ... ~ coefficient + ",allNam[["trend"]],sep="") ) )
                  resT1[,"est_trend"]<- resT1[,paste("est_",lev[2],sep="")] - resT1[,paste("est_",lev[1],sep="")]
    ### Linkingfehler mergen ... kompliziert! ... 
                  if ( is.null ( allNam[["linkErr"]] ) ) { 
                       le <- 0
                  }  else  { 
                       if ( length( unique(datL[, allNam[["linkErr"]]])) == 1 ) {## Achtung, krass kompliziert: wenn der Linkingfehler nicht fuer alle Zeilen gleich ist,
                            le <- unique(datL[, allNam[["linkErr"]]])           ### muss er innerhalb jeder Gruppe von 'parameter' gleich sein
                       }  else  {                                               ### 'leF' = linking error frame
                            nle<- length(unique(datL[, allNam[["linkErr"]]]))   ### Anzahl der Linkfehler ... wenn das ungleich der Anzahl der Kategorien ist ... dann wurde jk2.mean ueber den Wrapper mit 'chiSquare == FALSE' aufgerufen
                            if ( nle == length(unique ( datL[,allNam[["dependent"]] ])) ) { 
                                 leF<- do.call("rbind", by(data = datL, INDICES = datL[,allNam[["dependent"]] ], FUN = function ( z ) { 
                                       if ( !length(unique(z[, allNam[["linkErr"]] ] )) == 1 ) {
    ### obere Zeile darf, wenn ueberhaupt, nur dann TRUE sein, wenn 'jk2.table' ueber 'chiSquare == FALSE' mit dem 'jk2.mean'-Wrapper aufgerufen wurde ... doofes Problem: hat man eine kategoriale 
    ### Variable und erzeugt die Haeufigkeitstabelle, indem man dichotome Indikatoren generiert und fuer diese den Mittelwert bestimmt, wird der nicht mittransformiert 
                                            stopifnot(length(unique(datL[,allNam[["dependent"]]])) == 2)
                                            stopifnot(all(sort(unique(datL[,allNam[["dependent"]]])) == 0:1 ))
                                            stopifnot(length(unique(datL[,allNam[["linkErr"]]])) == 2)
                                            lef1<- datL[ which(datL[,allNam[["dependent"]]] == 1) ,allNam[["linkErr"]] ]
                                            if ( length(unique(lef1)) != 1) {
                                                 print ( unique(datL[,c(allNam[["trend"]], allNam[["dependent"]], allNam[["linkErr"]])]) , row.names = FALSE)
                                                 stop("Linking error is not unique between trend groups.\n")
                                            }
                                            lef1<- unique(lef1)
                                       }  else  { 
                                            lef1<- unique(z[,allNam[["linkErr"]]])
                                       }     
                                       return ( data.frame ( parameter = unique ( z[,allNam[["dependent"]]]), lef = lef1) ) } ))
                            }  else  {           
                                 stopifnot(all(sort(unique(datL[,allNam[["dependent"]]])) == 0:1 ))
                                 z  <- datL[which(datL[,allNam[["dependent"]]] == 1 ) ,]
                                 stopifnot(length(unique(z[,allNam[["linkErr"]]]))==1)
                                 le <- unique(z[,allNam[["linkErr"]]])
                            }
    ### wenn 'toCall' != 'mean', muessen Linkingfehler auf andere Art und Weise ans Rueckgabeobjekt drangehangen werden 
    ### das wird jetzt nach und nach dazu programmiert ... erstmal fuer 'table'; 'quantile' und 'glm' folgen spaeter
                            if ( toCall != "mean") {                            
                                 if ( toCall == "table" ) {                     ### untere Zeile: es muss genau einen gemeinsamen Spaltennamen in 'leF' und 'resT1' geben
                                     stopifnot ( length ( intersect ( colnames ( leF ), colnames(resT1))) == 1 ) 
                                     stopifnot ( ncol ( leF) == 2 ) 
                                     byCol <- intersect ( colnames ( leF ), colnames( resT1))
                                     stopifnot ( all ( unique ( sort ( leF[,byCol])) == unique ( sort ( resT1[,byCol]))))
                                     resT1 <- merge ( resT1, leF, by = byCol, all = TRUE)
                                 }  else  { 
                                     stop("weiter machen...")
                                 }   
                            }       
                       }     
                  }                  
                  if ( toCall == "mean") { 
                       resT1[,"se_trend"] <- sqrt(resT1[,paste("se_",lev[2],sep="")]^2 + resT1[,paste("se_",lev[1],sep="")]^2 + le^2)
                  }
                  if ( toCall == "table") { 
                       leCol <- setdiff ( colnames(leF), byCol ) 
                       resT1[,"se_trend"] <- sqrt(resT1[,paste("se_",lev[2],sep="")]^2 + resT1[,paste("se_",lev[1],sep="")]^2 + resT1[, leCol ]^2)
                       resT1 <- resT1[,-match(leCol, colnames(resT1))]
                  }
                  if ( !toCall %in% c("mean", "table") ) {
                        cat ( paste0("No method for incorporating linking error into '",toCall,"' analyses defined yet. Linking error will be ignored so far. \n"))
                        le <- 0
                        resT1[,"se_trend"] <- sqrt(resT1[,paste("se_",lev[2],sep="")]^2 + resT1[,paste("se_",lev[1],sep="")]^2 + le^2)
                  }      
                  resT2<- melt ( resT1, measure.vars = c(paste("est_",lev[1], sep=""), paste("est_",lev[2], sep=""), paste("se_",lev[1], sep=""), paste("se_",lev[2], sep=""), "est_trend", "se_trend"), na.rm = FALSE)
                  resT2<- data.frame ( resT2[,-match("variable", colnames(resT2))], as.numeric.if.possible (colsplit ( string = as.character(resT2[,"variable"]), pattern = "_", names = c("coefficient", allNam[["trend"]])), verbose = FALSE))
                  return(resT2)
              }  else {       
    ### innere Schleife: 'wholePop'. Erstmal ein paar checks ...
                 if ( wp==TRUE & is.null(groups) ) {
                      cat(paste("'groups' are NULL. Hence, no group differences can be estimated.\n",sep=""))
                 }
                 if ( wp==TRUE & !toCall %in% c("mean", "glm") ) {
                      cat(paste("Estimation of group differences with respect to whole population ('wholePop') only applies for the 'jk2.mean', 'jk2.glm' and 'jk2.table' with 'chiSquare == FALSE'.\n",sep=""))
                 }
    ### hier ruft die Funktion sich selber auf, wenn 'wholePop' in 'group.differences.by' enthalten ist
                 if ( wp==TRUE & !is.null(groups) & toCall %in% c("mean", "glm") ) {
                       if ( !all(group.splits == length(groups)) ) {
                            cat(paste("Warning: 'group.splits' has to equal 'length(groups)', if 'group.differences.by' contains 'wholePop'. Set 'group.splits' to ",length(groups),".\n",sep=""))
                            group.splits <- length(groups)
                       }
    ### erster Schritt: nur Subgruppen. Modellaufruf generieren
                       do    <- paste ( "resG <- eatRep ( ", paste(names(formals(eatRep)), names(formals(eatRep)), sep =" = ", collapse = ", "), ")",sep="")
                       eval(parse(text=do))
    ### zweiter Schritt: nur Gesamtpopulation, groups = intersect(groups, group.differences.by) ... bloeder Hotfix
                       if ( length( groups ) == 1 & wp == TRUE & length(group.differences.by) > 0 ) {
                            newGrp <- setdiff ( groups, group.differences.by)
                            if ( length( newGrp ) == 0 ) { newGrp <- NULL }
                       }  else  {      
                            newGrp<- intersect(groups, group.differences.by)
                       }     
                       do    <- paste ( "resA <- eatRep ( ", paste(names(formals(eatRep)), recode(names(formals(eatRep)), "'groups'='newGrp'; 'group.differences.by'='newGrp'"), sep =" = ", collapse = ", "), ")",sep="")
                       eval(parse(text=do))
                       resKom<- do.call("rbind", by(data = resG, INDICES = resG[, allNam[["group"]] ], FUN = function ( x ) {
                                x1 <- dcast(x, ... ~ coefficient, value.var = "value")
                                x2 <- dcast(resA, ... ~ coefficient, value.var = "value")
                                com<- setdiff (intersect(colnames(x1), colnames(x2)), c("est", "se", "modus", "depVar", "group"))
                                x3 <- merge(x1, x2, by = com, all = FALSE)
                                if ( nrow ( x3 ) > 0 ) { 
                                      stopifnot ( nrow(x3) == nrow(x1))
                                      x3[,"wholePopDiff_est"] <- x3[,"est.x"] - x3[,"est.y"]
                                      x3[,"wholePopDiff_se"]  <- sqrt(x3[,"se.x"]^2 + x3[,"se.y"]^2)
    ### ACHTUNG: wenn toCall == "mean", dann werden hier nur die Mittelwertsdifferenzen ausgegeben (nicht die Differenzen in Varianzen, Standardabweichungen, etc.)
                                      if ( toCall == "mean") { 
                                           x4 <- melt ( x3[which(x3[,"parameter"] %in% c("mean", "meanGroupDiff") ), ], id.vars = "group.x", measure.vars = c("wholePopDiff_est", "wholePopDiff_se"))
                                           x4 <- data.frame ( x4, colsplit(string = as.character(x4[,"variable"]), pattern = "_", names = c("parameter", "coefficient")))
                                           x5 <- x[1:nrow(x4),]
                                           x5[,"parameter"] <- x4[,"parameter"]; x5[,"coefficient"] <- x4[,"coefficient"]; x5[,"value"] <- x4[,"value"]
                                      }
    ### ACHTUNG: wenn toCall == "glm", dann werden nur die Differenzen fuer Intercept und alle Regressoren ausgegeben (nicht die Differenzen Ncases, NcasesValid, R2 ...)
                                      if ( toCall == "glm") {
                                           x4 <- melt ( x3[-which(x3[,"parameter"] %in% c("Ncases", "Nvalid", "R2", "R2nagel") ), ], id.vars = c("group.x","parameter"), measure.vars = c("wholePopDiff_est", "wholePopDiff_se"))
                                           foo<- colsplit(string = as.character(x4[,"variable"]), pattern = "_", names = c("parameter", "coefficient"))
                                           foo[,"parameter"] <- paste("wholePopDiff", x4[,"parameter"], sep="__")
                                           x5 <- data.frame ( group = x4[,"group.x"], depVar = x[1,"depVar"], modus = x[1,"modus"], parameter = foo[,"parameter"], coefficient = foo[,"coefficient"], value = x4[,"value"])
                                           add<- setdiff ( colnames(x), colnames(x5))
                                           if ( length(add)>0) { x5 <- data.frame ( x5, x[1,add, drop=FALSE] ) }
                                      }
                                      x  <- rbind ( x, x5)
                                }      
                                return(x)}))
                       add   <- setdiff ( colnames(resG), colnames(resA))
                       if(length(add)>0) {
                          stopifnot ( length(add) == 1)
                          resA[,add] <- paste("all_",add,sep="")
                       }
                       wg    <- match("wholeGroup", colnames(resA))
                       if(is.na(wg)) {resKom<- rbind.fill(resKom, resA)} else {resKom<- rbind.fill(resKom, resA[,-wg])}
                       return(resKom)
                 }  else  {
    ### obere Zeile: Ende der inneren Schleife
                       if( length( setdiff ( allNam[["group.differences.by"]],allNam[["group"]])) != 0) {stop("Variable in 'group.differences.by' must be included in 'groups'.\n")}
                       if(toCall == "glm") {.GlobalEnv$glm.family <- family}    ### Hotfix!
    ### Anzahl der Analysen definieren ueber den 'super splitter' und Analysen einzeln (ueber 'lapply') starten
                       toAppl<- superSplitter(group = allNam[["group"]], group.splits = group.splits, group.differences.by = allNam[["group.differences.by"]], group.delimiter = group.delimiter , dependent=allNam[["dependent"]] )
                       cat(paste(length(toAppl)," analyse(s) overall according to: 'group.splits = ",paste(group.splits, collapse = " ") ,"'.", sep=""))
    ### Achtung: wenn keine Gruppen und/oder Nests und/oder Imputationen spezifiziert sind, erzeuge Variablen mit Werten gleich 1, damit by() funktioniert!
                       if( is.null(allNam[["imp"]]) )  { datL[,"imp"] <- 1; allNam[["imp"]] <- "imp" } else { stopifnot(length(allNam[["imp"]]) == 1 ); datL[,allNam[["imp"]]] <- as.character(datL[,allNam[["imp"]]])}
                       if( is.null(allNam[["wgt"]]) )  { datL[,"wgtOne"] <- 1; allNam[["wgt"]] <- "wgtOne" } else { 
                           stopifnot(length(allNam[["wgt"]]) == 1 ) 
                           if ( !class(datL[,allNam[["wgt"]]]) %in% c("numeric", "integer") ) { stop ( paste("Error: 'wgt' variable '",allNam[["wgt"]],"' of class '",class(datL[,allNam[["wgt"]]]),"' has to be numeric.\n",sep="")) }
                           isMis <- which(is.na(datL[,allNam[["wgt"]]]))
                           isZero<- which ( datL[,allNam[["wgt"]]] == 0 ) 
                           if(length(isMis)>0) { stop (paste ( "Error: Found ",length(isMis)," missing values in the weight variable '",allNam[["wgt"]],"'.\n",sep="")) }
                           if(length(isZero)>0) { cat (paste ( "\nWarning: Found ",length(isZero)," zero weights in the weight variable '",allNam[["wgt"]],"'.\n",sep="")) }
                       }
                       if(!is.null(allNam[["nest"]]))  { 
                           stopifnot(length(allNam[["nest"]]) == 1 ) 
                           datL[,allNam[["nest"]]] <- as.character(datL[,allNam[["nest"]]])
                           cat(paste("\nAssume nested structure with ", length(table(datL[,allNam[["nest"]]]))," nests and ",length(table(datL[,allNam[["imp"]]]))," imputations in each nest. This will result in ",length(table(datL[,allNam[["nest"]]]))," x ",length(table(datL[,allNam[["imp"]]]))," = ",length(table(datL[,allNam[["nest"]]]))*length(table(datL[,allNam[["imp"]]]))," imputation replicates.\n",sep=""))
                       }  else  { cat("\nAssume unnested structure with ",length(table(datL[,allNam[["imp"]]]))," imputations.\n",sep="")}
                       datL[,"isClear"] <- TRUE
                       if( is.null(allNam[["nest"]]) ) { datL[,"nest"]  <- 1; allNam[["nest"]]  <- "nest" }
                       if(!is.null(allNam[["group"]])) {                        ### untere Zeile: das, damit leere Gruppen nicht ueber by() mit geschleift werden, wie es passiert, wenn Gruppen als Faktoren definiert sind
                           for ( jj in allNam[["group"]] )  { datL[,jj] <- as.character(datL[,jj]) } 
                       }
    ### check: abhaengige Var. numerisch?
                       if(toCall %in% c("mean", "quantile", "glm")) {
                          if(!class(datL[,allNam[["dependent"]]]) %in% c("integer", "numeric")) { 
                              cat(paste0("Warning: Dependent variable has to be of class 'integer' or 'numeric'.\n         '",allNam[["dependent"]],"' of class '",class(datL[,allNam[["dependent"]]]),"' will be transformed to numeric.\n"))
                              datL[,allNam[["dependent"]]] <- as.numeric(as.character(datL[,allNam[["dependent"]]]))
                          }                                                                                                
                       }
    ### wenn Replicates bereits uebergeben, muss PSU und repInd NULL sein
                       if(!is.null(repWgt) ) { 
                          if ( !is.null(allNam[["PSU"]]) | !is.null(allNam[["repInd"]]) ) { 
                              cat("Warning: Arguments 'PSU' and 'repInd' are expected to be NULL if replicate weights are already defined (via 'repWgt').\n    'PSU' and 'repInd' will be ignored.\n")
                          }    
                       }
    ### replicates erzeugen (nur einmal fuer alle Analysen, und nur wenn 'repWgt' NULL ist)
                       if(!is.null(allNam[["repWgt"]]))  {                       
                          repA <- data.frame ( datL[,allNam[["ID"]], drop=FALSE], datL[,allNam[["repWgt"]] ])
                          repA <- repA[!duplicated(repA[,allNam[["ID"]]]),]
                       }  else  { 
                          if(!is.null(allNam[["PSU"]]))  { 
                              repW <- datL[!duplicated(datL[,allNam[["ID"]]]),]
                              repA <- generate.replicates(dat = repW, ID = allNam[["ID"]], wgt = allNam[["wgt"]], PSU = allNam[["PSU"]], repInd = allNam[["repInd"]], type=type )
                              rm(repW)
                          }  else  { repA <- NULL}
                       }   
                       if(is.null(repA)) {doJK <- FALSE }  else {doJK <- TRUE}   
    ### splitten nach super splitter
                       allRes<- do.call("rbind.fill", lapply( names(toAppl), FUN = function ( gr ) {
                           if(toCall %in% c("mean", "table"))  { allNam[["group.differences.by"]] <- attr(toAppl[[gr]], "group.differences.by") } 
                           if( nchar(gr) == 0 ){ datL[,"dummyGroup"] <- "wholeGroup" ; allNam[["group"]] <- "dummyGroup" } else {allNam[["group"]] <- toAppl[[gr]] }
    ### check: Missings duerfen nur in abhaengiger Variable auftreten!          ### obere Zeile: problematisch!! "allNam" wird hier in jedem Schleifendurchlauf ueberschrieben -- nicht so superschoen!
                           noMis <- unlist ( c ( allNam[-match(c("group", "dependent"), names(allNam))], toAppl[gr]) )
                           miss  <- which ( sapply(datL[,noMis], FUN = function (uu) {length(which(is.na(uu)))}) > 0 )
                           if(length(miss)>0) { cat(paste("Warning! Unexpected missings in variable(s) ",paste(names(miss), collapse=", "),".\n",sep=""))}
    ### check: gleichviele Imputationen je Nest und Gruppe?
                           if(doCheck == TRUE) {
                              impNes<- table ( by(data = datL, INDICES = datL[, c(allNam[["nest"]], toAppl[[gr]]) ], FUN = function ( x ) { length(table(as.character(x[,allNam[["imp"]]])))}, simplify = FALSE) )
                              if(length(impNes) != 1 ) {cat("Warning: Number of imputations differ across nests and/or groups!"); print(impNes)}
    ### check: gleichviele PSUs je nest?
                              if(!is.null(allNam[["PSU"]]))  { 
                                  psuNes<- table ( by(data = datL, INDICES = datL[,allNam[["nest"]]], FUN = function ( x ) { length(table(as.character(x[,allNam[["PSU"]]])))}, simplify = FALSE) )
                                  if(length(psuNes) != 1 ) {cat("Warning: Number of PSUs differ across nests!"); print(psuNes)}
                              }    
    ### check: sind fuer jede Gruppe alle Faktorstufen in allen nests und allen imputationen vorhanden? z.B. nicht in einer Imputation nur Jungen
                              impNes<- by(data = datL, INDICES = datL[, c(allNam[["nest"]], allNam[["imp"]]) ], FUN = function ( x ) {
                                       if(length(x[,allNam[["ID"]]]) != length(unique(x[,allNam[["ID"]]])))  { 
                                          cat(paste(" W A R N I N G !  '",allNam[["ID"]],"' variable is not unique within nests and imputations. Analysis will be most likely biased!\n",sep=""))
                                       }   
                                       if( length(toAppl[[gr]])>0) { ret <- lapply( toAppl[[gr]], FUN = function ( y ) {table(x[,y])}) } else {ret <- 1}
                                       return(ret) }, simplify = FALSE)
                              impNes<- data.frame ( do.call("rbind", lapply(impNes, FUN = function ( x ) { unlist(lapply(x, FUN = length)) })) )
                              if ( !all ( sapply(impNes, FUN = function ( x ) { length(table(x)) } ) == 1) ) { cat("Warning: Number of units in at least one group differs across imputations!\n")}
    ### Achtung!! jetzt der check, der in der alten Version ueber 'checkData' gemacht wurde!
                              datL  <- do.call("rbind", by(data = datL, INDICES = datL[,c( allNam[["group"]], allNam[["nest"]], allNam[["imp"]])], FUN = function ( sub.dat ) {
                                       if(!is.null(allNam[["PSU"]])) {
                                           nJkZones <- length(table(as.character(sub.dat[,allNam[["PSU"]]])))
                                           if(nJkZones<2)  { 
                                              cat("Warning! Found group with less than 2 PSUs. Please check your data!\n"); flush.console()
                                              sub.dat[,"isClear"] <- FALSE 
                                           }
                                       }                                        ### untere Zeile: prueft; es darf GAR KEINE Missings geben 
                                       if( (toCall == "table" & separate.missing.indicator == FALSE) | (toCall %in% c("mean", "quantile", "glm") & na.rm==FALSE ) )  {    
                                          nObserved <- length(which(is.na(sub.dat[, allNam[["dependent"]]])))
                                           if(nObserved>0) { 
                                              if ( toCall %in% c("mean", "quantile", "glm") ) { 
                                                   cat("Warning! Found unexpected missing data in dependent variable for at least one group. Execution haltered. Please check your data!\n"); flush.console()
                                                   sub.dat[,"isClear"] <- FALSE 
                                              }  else  { 
                                                   cat("Warning! Found unexpected missing data in dependent variable for at least one group although 'separate.missing.indicator' was set to 'FALSE'. \n    Sure that this is intended? Try to continue execution ... \n"); flush.console()
                                              }     
                                           }
                                       }                                        ### untere Zeile: prueft; es darf NICHT ALLES missing sein
                                       if ( toCall %in% c("mean", "quantile", "glm") & na.rm==TRUE) { 
                                           nMissing <- length(which(is.na(sub.dat[, allNam[["dependent"]]])))
                                           if(nMissing == nrow(sub.dat))  { 
                                              cat("Warning! Some groups without any observed data. Please check your data!\n"); flush.console()
                                              sub.dat[,"isClear"] <- FALSE 
                                           }
                                       }
                                       return(sub.dat)}))    
                              ok    <- table(datL[,"isClear"])
                              if(length(ok) > 1 ) { cat ( paste( ok[which(names(ok)=="FALSE")] , " of ", nrow(datL), " cases removed from analysis due to inconsistent data.\n",sep="")) }
                           }
    ### nur fuer jk2.table(): "expected.values" aufbereiten ... 
                           if(toCall=="table") {
                              misInd <- which(is.na(datL[,allNam[["dependent"]]]))
                              if(separate.missing.indicator == TRUE) {
                                 if(length(misInd)>0) { datL[misInd,allNam[["dependent"]]] <- "<NA>"}
                              }  else {    
                                 if(length(misInd)>0) { 
                                    cat(paste("Warning: No seperate missing categorie was chosen. ", length(misInd), " missings were found anyhow for ",allNam[["dependent"]],". Missings will be deteted from the data.\n",sep=""))
                                    if(length(misInd) == nrow(datL)) {stop()}
                                    datL <- datL[-misInd,]
                                 }
                              }      
                              expected.values <- sort(unique(c(expected.values, names(table(datL[,allNam[["dependent"]]]))))) 
                           }   
    ### nun wird der Datensatz zuerst nach Nests und je Nest nach Imputationen geteilt 
                           anaA<- do.call("rbind", by(data = datL, INDICES = datL[,"isClear"], FUN = function ( datL1 ) {
                                  if(datL1[1,"isClear"] == TRUE) {              ### nur fuer isClear==TRUE werden Analysen gemacht
                                     ana <- do.call("rbind", by(data = datL1, INDICES = datL1[,allNam[["nest"]]], FUN = function ( datN ) {
                                            anaI <- do.call("rbind", by(data = datN, INDICES = datN[,allNam[["imp"]]], FUN = function ( datI ) {
    ### nun muss die Funktion (mean, table, glm ... ) und die Methode (JK2, BRR, oder konventionell) definiert werden
                                                    if( toCall == "mean" ) {    ### hier wird an die meanfunktion uebergeben
                                                        if ( doJK == TRUE ) {
                                                            ana.i <- jackknife.mean (dat.i = datI , allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter, type=type, repA=repA)
                                                        }  else  { 
                                                            ana.i <- conv.mean (dat.i = datI , allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter)
                                                        }
                                                    }
                                                    if( toCall == "table" ) {                
                                                        if ( doJK == TRUE ) {
                                                            ana.i <- jackknife.table ( dat.i = datI , allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter, type=type, repA=repA, separate.missing.indicator = separate.missing.indicator, expected.values=expected.values)
                                                        }  else  { 
                                                            ana.i <- conv.table (dat.i = datI , allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter, separate.missing.indicator = separate.missing.indicator, correct=correct, expected.values=expected.values)
                                                        }
                                                    }
                                                    if( toCall == "quantile" ) {                
                                                        if ( doJK == TRUE ) {
                                                            ana.i <- jackknife.quantile (dat.i = datI, allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter, type=type, repA=repA, probs=probs)
                                                        }  else  { 
                                                            ana.i <- conv.quantile (dat.i = datI, allNam=allNam, na.rm=na.rm, group.delimiter=group.delimiter, probs=probs, nBoot=nBoot,bootMethod=bootMethod)
                                                        }
                                                    }
                                                    if( toCall == "glm" ) {                
    ### initiate checks specifically for regression models
                                                        doChek<- checkRegression ( dat = datI, allNam=allNam)
                                                        ana.i <- jackknife.glm ( dat.i = datI , allNam=allNam, formula=formula, forceSingularityTreatment=forceSingularityTreatment, glmTransformation=glmTransformation, na.rm=na.rm, group.delimiter=group.delimiter, type=type, repA=repA)
                                                    }
                                                    ana.i <- data.frame ( ana.i, datI[1,c(allNam[["nest"]], allNam[["imp"]]),drop=FALSE], stringsAsFactors = FALSE)
                                                    return(ana.i)}))
                                            return(anaI)}))
    ### es wird nur gepoolt, wenn es mehr als eine Imputation gibt!
                                     if( length(table(ana[,allNam[["imp"]]])) > 1 ) { 
                                         retList <- jk2.pool ( datLong = ana, allNam=allNam, forceSingularityTreatment = forceSingularityTreatment)
                                     }  else  { retList <- ana[,-match(c(allNam[["nest"]], allNam[["imp"]]), colnames(ana))] }
    ### hier die dummy-Ergebnisstruktur erzeugen (noch nicht schoen, kann man vielleicht auch lassen ... )
                                  }  else  {                   
                                     retList <- NULL                           
                                  }
                                  return(retList)}))
                           if( "dummyGroup" %in% colnames(anaA) )  { anaA <- anaA[,-match("dummyGroup", colnames(anaA))] }
                           return(anaA)}))
                       rownames(allRes) <- NULL;  cat("\n")
                       return(allRes) }}} }

### vergleicht Trends zwischen Gruppen 
compareTrends <- function ( resultFrame ) { 
          tv  <- which ( sapply (resultFrame, FUN = function ( x ) { length( grep("^trend$", x) > 0) }) > 0 ) 
    ### trendvariable finden
          if ( length( tv ) == 0 ) { stop ("Cannot found any trend variable.\n")}
          if ( length( tv ) > 1  ) { stop (paste ("Found more than one trend variable: '",paste(names(tv), collapse = "', '"),"'.\n",sep=""))}
          sel <- resultFrame[which(resultFrame[,names(tv)] == "trend"),]
    ### Trends nach allen Parametern getrennt vergleichen 
          tr  <- do.call("rbind", by ( data = sel, INDICES = sel[,"parameter"], FUN = function ( prm ) { 
                 if ( length (unique(prm[,"group"])) < 2 ) { 
                      vgl <- NULL 
                 }  else  {      
                      spl <- data.frame ( combn(unique(prm[,"group"]),2), stringsAsFactors = FALSE)
                      vgl <- do.call("rbind", lapply ( spl, FUN = function ( gr ) { 
                             prms<- prm[which(prm[,"group"] %in% gr),]
                             estD<- diff ( prms[which(prms[,"coefficient"] == "est"),"value"] ) 
                             seD <- sqrt ( sum(prms[which(prms[,"coefficient"] == "se"),"value"]^2) ) 
                             ret <- data.frame ( group = paste ("compareTrend=",gr[1],"|__|",gr[2],sep=""), prms[1:2,c("depVar", "modus", "parameter")], coefficient = c("est", "se"), value = c(estD, seD))
                             return(ret)}))
                 }            
                 return(vgl) }))  
          return(tr)}           
          
checkRegression <- function ( dat, allNam ) {
                   ch <- lapply( allNam[["independent"]], FUN = function ( i ) {
                         isKonst <- length(unique(dat[,i]))
                         if ( isKonst == 1) {
                              cat(paste("Warning: predictor '",i,"' is constant. Please check your data.\n",sep=""))
                         }
                         if ( class ( dat[,i] ) == "character" ) {
                              cat(paste("Warning: predictor '",i,"' has class 'character'. Please check your data.\n",sep=""))
                         }
                         if ( class ( dat[,i] ) %in% c("character", "factor") ) {
                              if ( isKonst > 15 ) {
                                   cat(paste("Warning: predictor '",i,"' of class '",class ( dat[,i] ),"' has ",isKonst," levels. Please check whether this is intended.\n",sep=""))
                              }
                         } })   }                                               ### keine Rueckgabe

conv.quantile      <- function ( dat.i , allNam, na.rm, group.delimiter, repA, probs, nBoot,bootMethod  ) {
                      ret  <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,allNam[["group"]]], FUN = function ( sub.dat) {
                              if( all(sub.dat[,allNam[["wgt"]]] == 1) )  {      ### alle Gewichte sind 1 bzw. gleich
                                 ret   <- hdquantile(x = sub.dat[,allNam[["dependent"]]], se = TRUE, probs = probs,na.rm=na.rm )
                                 ret   <- data.frame (group = paste(sub.dat[1,allNam[["group"]],drop=FALSE], collapse=group.delimiter), depVar = allNam[["group"]], modus = "noch_leer", parameter = rep(names(ret),2), coefficient = rep(c("est","se"),each=length(ret)),value = c(ret,attr(ret,"se")),sub.dat[1,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE)
                              } else {                                          ### wenn Gewichte gefordert, koennen SEs ueber Bootstrap bestimmt werden
                                 if(!is.null(nBoot)) {
                                     if(nBoot<5) {nBoot <- 5}
                                     if(bootMethod == "wQuantiles") {           ### Variante 1
                                         x     <- sub.dat[,allNam[["dependent"]]]
                                         ret   <- boot(data = x, statistic = function ( x, i) {wtd.quantile(x = x[i], weights = sub.dat[,allNam[["wgt"]]], probs = probs,na.rm=na.rm )}, R=nBoot)
                                         ret   <- data.frame (group = paste(sub.dat[1,allNam[["group"]],drop=FALSE], collapse=group.delimiter), depVar = allNam[["group"]], modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(ret$t0, sapply(data.frame(ret$t), sd)), sub.dat[1,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE)
                                     } else {                                   ### Variante 2
                                         ret   <- do.call("rbind", lapply(1:nBoot, FUN = function (b){
                                                  y   <- sample(x = sub.dat[,allNam[["dependent"]]], size = length(sub.dat[,allNam[["dependent"]]]), replace = TRUE, prob = sub.dat[,allNam[["wgt"]]]/sum(sub.dat[,allNam[["wgt"]]]))
                                                  ret <- hdquantile(x = y, se = FALSE, probs = probs,na.rm=na.rm )
                                                  return(ret)}))
                                         ret   <- data.frame (group = paste(sub.dat[1,allNam[["group"]],drop=FALSE], collapse=group.delimiter), depVar = allNam[["group"]], modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(wtd.quantile(x = sub.dat[,allNam[["dependent"]]], weights = sub.dat[,allNam[["wgt"]]], probs = probs,na.rm=na.rm ), sapply(data.frame(ret),sd)) , sub.dat[1,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE)
                                     }
                                 } else {
                                     ret   <- wtd.quantile(x = sub.dat[,allNam[["dependent"]]], weights = sub.dat[,allNam[["wgt"]]], probs = probs,na.rm=na.rm )
                                     ret   <- data.frame (group = paste(sub.dat[1,allNam[["group"]],drop=FALSE], collapse=group.delimiter), depVar = allNam[["group"]], modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(ret, rep(NA, length(probs))) , sub.dat[1,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE)
                                 }
                              }
                              return(ret)}))
                      return(facToChar(ret))}


jackknife.quantile <- function ( dat.i , allNam, na.rm, type, repA, probs, group.delimiter) {
                      cat("."); flush.console()
                #      if(!exists("svrepdesign"))      {library(survey)}
                      typeS          <- recode(type, "'JK2'='JKn'")             ### typeS steht fuer type_Survey
                      design         <- svrepdesign(data = dat.i[,c(allNam[["group"]], allNam[["dependent"]]) ], weights = dat.i[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = repA[match(dat.i[,allNam[["ID"]]], repA[,allNam[["ID"]]] ),-1,drop = FALSE], combined.weights = TRUE, mse = TRUE)
                      formel         <- as.formula(paste("~ ",allNam[["dependent"]], sep = "") )
                      quantile.imp   <- svyby(formula = formel, by = as.formula(paste("~", paste(allNam[["group"]], collapse = " + "))), design = design, FUN = svyquantile, quantiles = probs, return.replicates = TRUE, na.rm = na.rm)
                      molt           <- melt(data=quantile.imp, id.vars=allNam[["group"]], na.rm=TRUE)
                      molt[,"parameter"]   <- remove.non.numeric(as.character(molt[,"variable"]))
                      recString      <- paste("'",names(table(molt[,"parameter"])) , "' = '" , as.character(probs), "'" ,sep = "", collapse="; ")
                      molt[,"parameter"]   <- recode(molt[,"parameter"], recString)
                      molt[,"coefficient"] <- recode(remove.numeric(as.character(molt[,"variable"])), "'V'='est'")
                      return(facToChar(data.frame ( group = apply(molt[,allNam[["group"]],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), depVar = allNam[["group"]], modus = "noch_leer", molt[,c("parameter", "coefficient", "value", allNam[["group"]])], stringsAsFactors = FALSE))) }


conv.table      <- function ( dat.i , allNam, na.rm, group.delimiter, separate.missing.indicator , correct, expected.values  ) {
                   table.cast <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,allNam[["group"]]], FUN = function ( sub.dat) {
                                 prefix <- data.frame(sub.dat[1,allNam[["group"]], drop=FALSE], row.names = NULL, stringsAsFactors = FALSE )
                                 foo    <- make.indikator(variable = sub.dat[,allNam[["dependent"]]], name.var = "ind", force.indicators =expected.values, separate.missing.indikator = ifelse(separate.missing.indicator==TRUE, "always","no"))
                                 if(all(dat.i[,allNam[["wgt"]]] == 1)) {ret    <- data.frame ( prefix , desk(foo[,-1, drop = FALSE],na.rm=TRUE)[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )
                                 } else { ret    <- data.frame ( prefix , desk(foo[,-1, drop = FALSE], p.weights = sub.dat[,allNam[["wgt"]]],na.rm=TRUE)[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )}
                                 ret[,"parameter"] <- substring(rownames(ret),5)
                                 return(ret)}) )
                   if(!is.null(allNam[["group.differences.by"]]))   {
                      m            <- table.cast
                      m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[allNam[["group"]]], collapse = "."))})
                      m$all.group  <- 1
                      res.group    <- tempR <- setdiff(allNam[["group"]], allNam[["group.differences.by"]])
                      if(length(res.group) == 0 ) {res.group <- "all.group"} 
                      difs         <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                      if(length(tempR)>0) { 
                                         datSel    <- merge(dat.i, iii[!duplicated(iii[,res.group]),res.group,drop=FALSE], by = res.group, all = FALSE)
                                      }  else  { 
                                         datSel    <- dat.i
                                      }
                                      tbl    <- table(datSel[,c(allNam[["group.differences.by"]], allNam[["dependent"]])])
                                      chisq  <- chisq.test(tbl, correct = correct)  
                                      scumm  <- iii[!duplicated(iii[,res.group]),res.group,drop = FALSE]
                                      group  <- paste( paste( colnames(scumm), as.character(scumm[1,]), sep="="), sep="", collapse = ", ")
                                      dif.iii<- data.frame(group = group, parameter = "chiSquareTest", coefficient = c("chi2","df","pValue"), value = c(chisq[["statistic"]],chisq[["parameter"]],chisq[["p.value"]]) , stringsAsFactors = FALSE )
                                      return(dif.iii)}))}                       ### siehe http://www.vassarstats.net/dist2.html   und   http://onlinestatbook.com/2/tests_of_means/difference_means.html
                   ret        <- melt(table.cast, measure.vars = c("Mittelwert", "std.err"), na.rm=TRUE)
                   ret[,"coefficient"] <- recode(ret[,"variable"], "'Mittelwert'='est'; 'std.err'='se'")
                   ret        <- data.frame ( group = apply(ret[,allNam[["group"]],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), depVar = allNam[["dependent"]], modus = "noch_leer", ret[,c("coefficient", "parameter")], value = ret[,"value"], ret[,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE) 
                   if(!is.null(allNam[["group.differences.by"]]))   {return(facToChar(rbind.fill(ret,difs)))} else {return(facToChar(ret))}}                   


jackknife.table <- function ( dat.i , allNam, na.rm, group.delimiter, type, repA, separate.missing.indicator, expected.values) { 
                   cat("."); flush.console()
                   dat.i[,allNam[["dependent"]]] <- factor(dat.i[,allNam[["dependent"]]], levels = expected.values)
                   # if(!exists("svrepdesign"))      {library(survey)}
                   typeS     <- recode(type, "'JK2'='JKn'")
                   design    <- svrepdesign(data = dat.i[,c(allNam[["group"]], allNam[["dependent"]])], weights = dat.i[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = repA[match(dat.i[,allNam[["ID"]]], repA[,allNam[["ID"]]] ),-1,drop = FALSE], combined.weights = TRUE, mse = TRUE)
                   formel    <- as.formula(paste("~factor(",allNam[["dependent"]],", levels = expected.values)",sep=""))
                   means     <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(allNam[["group"]]), collapse = " + "))), design = design, FUN = svymean, deff = FALSE, return.replicates = TRUE)
                   cols      <- match(paste("factor(",allNam[["dependent"]],", levels = expected.values)",expected.values,sep=""), colnames(means))
                   colnames(means)[cols] <- paste("est",expected.values, sep="____________")
                   cols.se   <- grep("^se[[:digit:]]{1,5}$", colnames(means) )
                   stopifnot(length(cols) == length(cols.se))
                   colnames(means)[cols.se] <- paste("se____________", expected.values, sep="")
                   molt      <- melt(data=means, id.vars=allNam[["group"]], na.rm=TRUE)
                   splits    <- data.frame ( do.call("rbind", strsplit(as.character(molt[,"variable"]),"____________")), stringsAsFactors = FALSE)
                   colnames(splits) <- c("coefficient", "parameter")
                   ret       <- data.frame ( group = apply(molt[,allNam[["group"]],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), depVar = allNam[["dependent"]], modus = "noch_leer", splits, value = molt[,"value"], molt[,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE) 
                   if(!is.null(allNam[["group.differences.by"]]))   {
                      m            <- ret
                      m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[allNam[["group"]]], collapse = "."))})
                      m$all.group  <- 1
                      res.group    <- tempR <- setdiff(allNam[["group"]], allNam[["group.differences.by"]])
                      if(length(res.group) == 0 ) {res.group <- "all.group"} 
                      difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                        if(length(tempR)>0) { 
                                           datSel    <- merge(dat.i, iii[!duplicated(iii[,res.group]),res.group,drop=FALSE], by = res.group, all = FALSE)
                                        }  else  { 
                                           datSel    <- dat.i
                                        }
                                        designSel <- svrepdesign(data = datSel[,c(allNam[["group.differences.by"]], allNam[["dependent"]])], weights = datSel[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = repA[match(datSel[,allNam[["ID"]]], repA[,allNam[["ID"]]] ),-1,drop = FALSE], combined.weights = TRUE, mse = TRUE)
                                        formel    <- as.formula(paste("~", allNam[["group.differences.by"]] ,"+",allNam[["dependent"]],sep=""))
                                        tbl       <- svychisq(formula = formel, design = designSel, statistic = "Chisq")
                                        scumm     <- iii[!duplicated(iii[,res.group]),res.group,drop = FALSE]
                                        group     <- paste( paste( colnames(scumm), as.character(scumm[1,]), sep="="), sep="", collapse = ", ")
                                        dif.iii   <- data.frame(group = group, parameter = "chiSquareTest", coefficient = c("chi2","df","pValue"), value = c(tbl[["statistic"]],tbl[["parameter"]],tbl[["p.value"]]) , stringsAsFactors = FALSE )
                                        return(dif.iii)                         ### siehe http://www.vassarstats.net/dist2.html
                      } )) }                                                    ### http://onlinestatbook.com/2/tests_of_means/difference_means.html
                   if(!is.null(allNam[["group.differences.by"]]))   {return(facToChar(rbind.fill(ret,difs)))} else {return(facToChar(ret))}}                   


conv.mean      <- function (dat.i , allNam, na.rm, group.delimiter) {
                  deskr    <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,allNam[["group"]]], FUN = function ( sub.dat) {
                              prefix <- sub.dat[1,allNam[["group"]], drop=FALSE]
                              if ( all(sub.dat[,allNam[["wgt"]]] == 1) )  { useWGT <- NULL}  else  {useWGT <- sub.dat[,allNam[["wgt"]]] }
                              ret    <- data.frame ( nValidUnweighted = length(na.omit(sub.dat[, allNam[["dependent"]] ])), prefix, desk(sub.dat[, allNam[["dependent"]] ], p.weights = useWGT, na.rm=na.rm)[,c("N", "N.valid", "Mittelwert", "std.err", "Varianz", "Streuung")], stringsAsFactors = FALSE)
                              names(ret) <- c( "nValidUnweighted", allNam[["group"]] , "Ncases", "NcasesValid", "mean", "se.mean", "var","sd")
                              return(ret)}))
                  if(!is.null(allNam[["group.differences.by"]]))   {
                     nCat <- table(as.character(dat.i[,allNam[["group.differences.by"]]]))
                     if ( length(nCat) < 2 ) { 
                          cat(paste("Warning: Grouping variable '", allNam[["group.differences.by"]], "'only has one category within imputation and/or nest. Group differences cannot be computed. Skip computation.\n",sep="")) 
                     }  else  {
                        m            <- deskr
                        m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[allNam[["group"]]], collapse = "."))})
                        m$all.group  <- 1
                        res.group    <- tempR <- setdiff(allNam[["group"]], allNam[["group.differences.by"]])
                        if(length(res.group) == 0 ) {res.group <- "all.group"} 
                        kontraste      <- expand.grid(1:length(table(m[,allNam[["group.differences.by"]]])), 1:length(table(m[,allNam[["group.differences.by"]]])))
                        weg            <- which(apply(kontraste, 1, FUN = function ( x ) {x[1] >= x[2]}))
                        kontraste      <- kontraste[-weg,]
                        recodeString   <- paste("'", 1:length(table(m[,allNam[["group.differences.by"]]])),"' = '", names(table(m[,allNam[["group.differences.by"]]])),"'", sep = "", collapse = "; ")
                        kontraste      <- data.frame ( lapply(kontraste, FUN = function ( x ) {recode(x, recodeString)}), stringsAsFactors = FALSE )
                        difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                          ret <- do.call("rbind", apply(kontraste, 1, FUN = function ( k ) {
                                                 if ( sum ( k %in% iii[,allNam[["group.differences.by"]]]) != length(k) ) { 
                                                    cat(paste("Warning: cannot compute contrasts for 'group.differences.by = ",allNam[["group.differences.by"]],"'.\n",sep="")); flush.console()
                                                    return(NULL)
                                                 }  else  {   
                                                    vgl.iii   <- iii[iii[,allNam[["group.differences.by"]]] %in% k ,]
                                                    true.diff <- diff(vgl.iii[,"mean"])
                                                    scumm     <- sapply(vgl.iii[,res.group,drop = FALSE], as.character)
                                                    group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                                    dummy     <- do.call("cbind", lapply ( allNam[["group"]], FUN = function ( gg ) { 
                                                                 ret <- data.frame ( paste ( unique(vgl.iii[,gg]), collapse = ".vs."))
                                                                 colnames(ret) <- gg
                                                                 return(ret)}))
                                                    dif.iii   <- data.frame(dummy, group = paste(group, paste(k, collapse = ".vs."),sep="____"), parameter = "meanGroupDiff", coefficient = c("est","se"), value = c(true.diff, sqrt( sum(vgl.iii[,"sd"]^2 / vgl.iii[,"nValidUnweighted"]) )) , stringsAsFactors = FALSE )
                                                    return(dif.iii)             ### siehe http://www.vassarstats.net/dist2.html
                                                 } }))                          ### http://onlinestatbook.com/2/tests_of_means/difference_means.html
                                          return(ret)})) 
                     }
                  }                                           
                  deskrR   <- melt(data = deskr, id.vars = allNam[["group"]], measure.vars = setdiff(colnames(deskr), c("nValidUnweighted",allNam[["group"]]) ), na.rm=TRUE)
                  deskrR[,"coefficient"] <- recode(deskrR[,"variable"], "'se.mean'='se';else='est'")
                  deskrR[,"parameter"]   <- gsub("se.mean","mean",deskrR[,"variable"])
                  deskrR   <- data.frame ( group = apply(deskrR[,allNam[["group"]],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), depVar = allNam[["dependent"]], deskrR[,c( "parameter", "coefficient", "value", allNam[["group"]])], stringsAsFactors = FALSE)
                  if(!is.null(allNam[["group.differences.by"]]))   {
                      if ( length (nCat ) >1 ) {
                           return(facToChar(rbind.fill(deskrR,difs)))
                      }   else  {
                           return(facToChar(deskrR))
                      }
                  }  else {
                      return(facToChar(deskrR))
                  } }    


jackknife.mean <- function (dat.i , allNam, na.rm, group.delimiter, type, repA) {
          cat("."); flush.console()
          dat.i[,"N_weighted"]      <- 1
          dat.i[,"N_weightedValid"] <- 1
          if( length(which(is.na(dat.i[,allNam[["dependent"]]]))) > 0 ) { dat.i[which(is.na(dat.i[,allNam[["dependent"]]])), "N_weightedValid" ] <- 0 }
          typeS<- recode(type, "'JK2'='JKn'")
          # if(!exists("svrepdesign"))      {library(survey)}
          repl <- repA[ match(dat.i[,allNam[["ID"]]], repA[,allNam[["ID"]]]),]
          des  <- svrepdesign(data = dat.i[,c(allNam[["group"]], allNam[["dependent"]], "N_weighted", "N_weightedValid")], weights = dat.i[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = repl[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
          rets <- data.frame ( target = c("Ncases", "NcasesValid", "mean", "var"), FunctionToCall = c("svytotal","svytotal","svymean","svyvar"), formelToCall = c("paste(\"~ \", \"N_weighted\",sep=\"\")","paste(\"~ \", \"N_weightedValid\",sep=\"\")","paste(\"~ \",allNam[[\"dependent\"]], sep = \"\")","paste(\"~ \",allNam[[\"dependent\"]], sep = \"\")"), naAction = c("FALSE","TRUE","na.rm","na.rm"), stringsAsFactors = FALSE)
          ret  <- apply(rets, 1, FUN = function ( toCall ) {                    ### svyby wird dreimal aufgerufen ...
                  do   <- paste(" res <- svyby(formula = as.formula(",toCall[["formelToCall"]],"), by = as.formula(paste(\"~\", paste(allNam[[\"group\"]], collapse = \" + \"))), design = des, FUN = ",toCall[["FunctionToCall"]],",na.rm=",toCall[["naAction"]],", deff = FALSE, return.replicates = TRUE)",sep="")
                  eval(parse(text=do))
                  resL <- melt( data = res, id.vars = allNam[["group"]], variable.name = "coefficient" , na.rm=TRUE)
                  stopifnot(length(table(resL[,"coefficient"])) == 2)
                  resL[,"coefficient"] <- recode(resL[,"coefficient"], "'se'='se'; else ='est'")
                  resL[,"parameter"]   <- toCall[["target"]]
                  attr(resL, "original") <- res
                  return(resL)})
          sds  <- do.call("rbind", by(data = dat.i, INDICES =  dat.i[,allNam[["group"]]], FUN = function (uu) {
                  namen   <- uu[1, allNam[["group"]], drop=FALSE]
                  sub.rep <- repl[ match(uu[,allNam[["ID"]]], repl[,allNam[["ID"]]] ) ,  ]
                  des.uu  <- svrepdesign(data = uu[,c(allNam[["group"]], allNam[["dependent"]])], weights = uu[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = sub.rep[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                  var.uu  <- svyvar(x = as.formula(paste("~",allNam[["dependent"]],sep="")), design = des.uu, deff = FALSE, return.replicates = TRUE, na.rm = na.rm)
                  ret     <- data.frame(namen, est = as.numeric(sqrt(coef(var.uu))), se =  as.numeric(sqrt(vcov(var.uu)/(4*coef(var.uu)))), stringsAsFactors = FALSE )
                  return(ret)}) )
          sds  <- data.frame ( melt(data = sds, id.vars = allNam[["group"]], variable.name = "coefficient" , na.rm=TRUE), parameter = "sd", stringsAsFactors = FALSE)
          resAl<- rbind(do.call("rbind",ret), sds)
          resAl<- data.frame ( group = apply(resAl[,allNam[["group"]],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), depVar = allNam[["dependent"]], resAl[,c("parameter","coefficient","value",allNam[["group"]])] , stringsAsFactors = FALSE)
          if(!is.null(allNam[["group.differences.by"]]))   {
             nCat <- table(as.character(dat.i[,allNam[["group.differences.by"]]]))
             if ( length(nCat) < 2 ) { 
                  cat(paste("Warning: Grouping variable '", allNam[["group.differences.by"]], "'only has one category within imputation and/or nest. Group differences cannot be computed. Skip computation.\n",sep="")) 
             }  else  {
                m    <- attr(ret[[ which(rets[,"target"] == "mean") ]], "original")
                m$comb.group <- apply(m, 1, FUN = function (ii) {crop(paste( ii[allNam[["group"]]], collapse = "."))})
                repl1<- data.frame(t(attr(attr(ret[[which(rets[,"target"] == "mean")]], "original"), "replicates")), stringsAsFactors = FALSE )
                colnames(repl1) <- replCols <- paste("replNum", 1:ncol(repl1), sep="")
                repl1[,"comb.group"] <- rownames(repl1)
                m    <- merge(m, repl1, by = "comb.group" )
                m$all.group    <- 1
                res.group      <- tempR  <- setdiff(allNam[["group"]], allNam[["group.differences.by"]])
                if(length(res.group) == 0 ) {res.group <- "all.group"}
                kontraste      <- expand.grid(1:length(table(m[,allNam[["group.differences.by"]]])), 1:length(table(m[,allNam[["group.differences.by"]]])))
                weg            <- which(apply(kontraste, 1, FUN = function ( x ) {x[1] >= x[2]}))
                if(length(weg)>0) {kontraste      <- kontraste[-weg,]}
                recodeString   <- paste("'", 1:length(table(m[,allNam[["group.differences.by"]]])),"' = '", names(table(m[,allNam[["group.differences.by"]]])),"'", sep = "", collapse = "; ")
                kontraste      <- data.frame ( lapply(kontraste, FUN = function ( x ) {recode(x, recodeString)}), stringsAsFactors = FALSE )
                difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                  ret <- do.call("rbind", apply(kontraste, 1, FUN = function ( k ) { 
                                         if ( sum ( k %in% iii[,allNam[["group.differences.by"]]]) != length(k) ) { 
                                              cat(paste("Warning: cannot compute contrasts for 'group.differences.by = ",allNam[["group.differences.by"]],"'.\n",sep="")); flush.console()
                                              return(NULL)                      ### Quelle fuer dieses Vorgehen: 
                                         } else {                               ### Mail SW an ZKD, 07.11.2012, 17.54 Uhr, "in Absprache mit Dirk"
                                              vgl.iii   <- iii[iii[,allNam[["group.differences.by"]]] %in% k ,]
                                              true.diff <- diff(vgl.iii[,which(colnames(vgl.iii) %in% allNam[["dependent"]])])
                                              other.diffs <- apply(vgl.iii[,replCols], 2, diff)
                                              scumm     <- sapply(vgl.iii[,res.group,drop = FALSE], as.character)
                                              group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                              dummy     <- do.call("cbind", lapply ( allNam[["group"]], FUN = function ( gg ) { 
                                                           ret <- data.frame ( paste ( unique(vgl.iii[,gg]), collapse = ".vs."))
                                                           colnames(ret) <- gg
                                                           return(ret)}))
                                              dif.iii   <- data.frame(dummy, group = group, vgl = paste(k, collapse = ".vs."), dif = true.diff, se =  sqrt(sum((true.diff - other.diffs)^2)), stringsAsFactors = FALSE )
                                              return(dif.iii)
                                         } }))
                                  return(ret)}))
                difsL<- data.frame ( depVar = allNam[["dependent"]], melt(data = difs, measure.vars = c("dif", "se") , variable.name = "coefficient" , na.rm=TRUE), parameter = "meanGroupDiff", stringsAsFactors = FALSE)
                difsL[,"coefficient"] <- recode(difsL[,"coefficient"], "'se'='se'; else = 'est'")
                difsL[,"group"] <- apply(difsL[,c("group","vgl")],1,FUN = function (z) {paste(z,collapse="____")})
                resAl<- rbind(resAl,difsL[,-match("vgl", colnames(difsL))])  
             }
          }      
          return(facToChar(resAl)) }
          

### Hilfsfunktion fuer jk2.glm()
jackknife.glm <- function (dat.i , allNam, formula, forceSingularityTreatment, glmTransformation, na.rm, group.delimiter, type, repA ) {
                 cat("."); flush.console()
                 sub.ana <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,allNam[["group"]]], FUN = function (sub.dat) {
                            glm.ii <- test <- glm(formula = formula, data = sub.dat, family = glm.family)
                            singular       <- names(glm.ii$coefficients)[which(is.na(glm.ii$coefficients))]
                            if(!is.null(repA)) {
                                # if(!exists("svrepdesign"))      {library(survey)}
                                typeS          <- recode(type, "'JK2'='JKn'")
                                design         <- svrepdesign(data = sub.dat[,c(allNam[["group"]], allNam[["independent"]], allNam[["dependent"]]) ], weights = sub.dat[,allNam[["wgt"]]], type=typeS, scale = 1, rscales = 1, repweights = repA[match(sub.dat[,allNam[["ID"]]], repA[,allNam[["ID"]]] ),-1,drop = FALSE], combined.weights = TRUE, mse = TRUE)
                                if(length(singular) == 0 & forceSingularityTreatment == FALSE ) {
                                   glm.ii      <- svyglm(formula = formula, design = design, return.replicates = FALSE, family = glm.family)
                                }
                            }
                            r.squared      <- data.frame ( r.squared = var(glm.ii$fitted.values)/var(glm.ii$y) , N = nrow(sub.dat) , N.valid = length(glm.ii$fitted.values) )
                            r.nagelkerke   <- NagelkerkeR2(glm.ii)
                            summaryGlm     <- summary(glm.ii)
    ### AIC, deviance etc. nur auslesen, wenn linkfunktion NICHT 'identity'
                            if( class(glm.family) == "family" ) { 
                                if (  length(grep("binomial", crop(capture.output(glm.family)))) > 0 ) {
                                      res.bl <- data.frame ( group=paste(sub.dat[1,allNam[["group"]]], collapse=group.delimiter), depVar =allNam[["dependent"]],modus = "noch_leer", parameter = c(rep(c("Ncases","Nvalid",names(glm.ii$coefficients)),2),"R2","R2nagel", "deviance", "null.deviance", "AIC", "df.residual", "df.null"),
                                                coefficient = c(rep(c("est","se"),each=2+length(names(glm.ii$coefficients))),rep("est", 7)),
                                                value=c(r.squared[["N"]],r.squared[["N.valid"]],glm.ii$coefficient,NA,NA,summaryGlm$coef[,2],r.squared[["r.squared"]],r.nagelkerke[["R2"]], test$deviance, test$null.deviance, test$aic, test$df.residual, test$df.null),sub.dat[1,allNam[["group"]], drop=FALSE], stringsAsFactors = FALSE)
                                }   else  { 
                                      res.bl <- data.frame ( group=paste(sub.dat[1,allNam[["group"]]], collapse=group.delimiter), depVar =allNam[["dependent"]],modus = "noch_leer", parameter = c(rep(c("Ncases","Nvalid",names(glm.ii$coefficients)),2),"R2","R2nagel"),
                                                coefficient = c(rep(c("est","se"),each=2+length(names(glm.ii$coefficients))),rep("est", 2)),
                                                value=c(r.squared[["N"]],r.squared[["N.valid"]],glm.ii$coefficient,NA,NA,summaryGlm$coef[,2],r.squared[["r.squared"]],r.nagelkerke[["R2"]]),sub.dat[1,allNam[["group"]], drop=FALSE], stringsAsFactors = FALSE)
                                }
                            }  else  {     
                                res.bl <- data.frame ( group=paste(sub.dat[1,allNam[["group"]]], collapse=group.delimiter), depVar =allNam[["dependent"]],modus = "noch_leer", parameter = c(rep(c("Ncases","Nvalid",names(glm.ii$coefficients)),2),"R2","R2nagel"),
                                          coefficient = c(rep(c("est","se"),each=2+length(names(glm.ii$coefficients))),rep("est", 2)),
                                          value=c(r.squared[["N"]],r.squared[["N.valid"]],glm.ii$coefficient,NA,NA,summaryGlm$coef[,2],r.squared[["r.squared"]],r.nagelkerke[["R2"]]),sub.dat[1,allNam[["group"]], drop=FALSE], stringsAsFactors = FALSE)
                            }
                            if(!is.null(repA)) {                                ### jetzt kommt die Behandlung, wenn zusaetzlich zu JK noch singularitaet auftritt. das ueberschreibt nun die bisherige "res.bl"
                                if(length(which(is.na(glm.ii$coefficients))) > 0 ) {
                                   cat(paste("Singularity problem in regression estimation for ", length(singular)," coefficient(s): ",paste(singular, collapse = ", "),". Try workaround ... \n", sep = "")); flush.console()
                                }
                                if(forceSingularityTreatment == TRUE ) {
                                   cat("Compute coefficients in the expectation of singularities ... \n"); flush.console()
                                }
                                if(length(singular) > 0 | forceSingularityTreatment == TRUE ) {
                                   stopifnot(length(as.character(formula)) == 3 )
                                   formelNew  <- paste ( as.character(formula)[2] ," ~ ",as.character(formula)[3],sep="")
                                   cat("Unidentified bug with Nagelkerkes r^2 in singularity treatment. No r^2 is computed.\n")
                                   if ( glmTransformation == "none" )  {string     <- paste("resRoh <- data.frame( withReplicates(design, quote(getOutputIfSingular(glm(formula = ",formelNew,", weights=.weights, family = ",glm.family$family,"(link=\"", glm.family$link,"\"))))), stringsAsFactors = FALSE)",sep="")}
                                   if ( glmTransformation == "sdY" )   {string     <- paste("resRoh <- data.frame( withReplicates(design, quote(getOutputIfSingularT1(glm(formula = ",formelNew,", weights=.weights, family = ",glm.family$family,"(link=\"", glm.family$link,"\"))))), stringsAsFactors = FALSE)",sep="")}
                                   eval ( parse ( text = string ) )
                                   # rownames(resRoh) <- gsub("N", "Ncases", rownames(resRoh))
                                   index      <- which(nchar(rownames(resRoh)) == 0)                  
                                   if(length(index)>0) { for ( j in 1:length(index)) { rownames(resRoh)[index[j]] <- paste("dummyPar",j,sep="")}}
                                   res.bl     <- data.frame ( group=paste(sub.dat[1,allNam[["group"]]], collapse=group.delimiter), depVar =allNam[["dependent"]],modus = "noch_leer", parameter = rep(rownames(resRoh), 2), coefficient = c(rep("est", nrow(resRoh)), rep("se", nrow(resRoh))),
                                                 value = c(resRoh[,"theta"], resRoh[,"SE"]), sub.dat[1,allNam[["group"]], drop=FALSE], stringsAsFactors = FALSE)
                                   weg        <- intersect ( which(res.bl[,"coefficient"] == "se") , which(res.bl[,"value"] == 0) )
                                   if(length(weg)>0) { res.bl[weg,"value"] <- NA}
                                }
                            }
                            return(res.bl) }))
                 return(sub.ana) }
          

dM <- function ( object, omitTerms = c("mean","sd","var", "Ncases","NcasesValid", "meanGroupDiff", "se","est"), digits = 3 ) {
             if ( is.null ( object ) ) { ret <- NULL } else { 
                  checkForPackage (namePackage = "reshape", targetPackage = "eatRep")
                  groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
                  origOmit  <- c("mean","sd","var", "Ncases","NcasesValid", "meanGroupDiff", "se","est")
                  if(length(omitTerms) == length(origOmit)) { if ( all (omitTerms == origOmit )) { omitTerms <- NULL } }
                  if (!is.null(omitTerms)) { 
                      weg    <- unlist(lapply ( omitTerms , FUN = function ( x ) { match.arg(arg = x, choices = origOmit) } ))
                      wegC   <- unique(unlist(lapply(object[,c("parameter", "coefficient")], FUN = function ( x ) { which(x %in% weg)})))
                      if(length(wegC)>0) { object <- object[-wegC,]}
                  }    
                  ret <- merge ( dcast(object, depVar + group ~ parameter + coefficient, value.var = "value"), object[ !duplicated(object[,c("group",groupCols), drop=FALSE]),c("group",groupCols), drop=FALSE], by = "group", all.x = TRUE, all.y = FALSE)
                  whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
                  if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(ret[,i], digits = digits)}}
             }     
             return(ret)}


superSplitter <- function ( group=NULL, group.splits = length(group), group.differences.by = NULL, group.delimiter = "_" , dependent )  {
             group        <- as.list(group); names(group) <- unlist(group)
             if(max(group.splits)> length(group)) {group.splits[which(group.splits>length(group))] <- length(group)}
             group.splits <- unique(group.splits)
             superSplitti <- unlist(lapply(group.splits, FUN = function ( x ) {
                             spl <- combn(names(group),x)
                             if(class(spl) == "matrix") { spl <- as.list(data.frame(spl))} else {spl <- list(spl)}
                             spl <- unlist(lapply(spl, FUN = function ( y ) { paste(as.character(unlist(y)), collapse="________")}))
                             return(spl)}))
             superSplitti <- strsplit(superSplitti, "________")
             namen        <- unlist(lapply(superSplitti, FUN = function ( y ) { paste(y, collapse=group.delimiter)}))
             superSplitti <- lapply(superSplitti, FUN = function ( y ) {
                             if(!is.null(group.differences.by)) {if( group.differences.by %in% y ) { attr(y,"group.differences.by") <- group.differences.by}}
                             return(y)})
             names(superSplitti) <- namen
             return(superSplitti)}


checkForPackage <- function (namePackage, targetPackage) {
        if(paste("package:",namePackage,sep="") %in% search() ) {
           cat(paste("Warning: Package '",namePackage,"' is attached. Functions in package '",targetPackage,"' conflict with '",namePackage,"'in some way.\n  '",namePackage,"' therefore will be detached now. \n", sep=""))
           eval(parse(text=paste("detach(\"package:",namePackage,"\", force = TRUE)",sep=""))) } }


dT <- function ( object, reshapeFormula = depVar + group ~ parameter + coefficient, seOmit = FALSE, roundDigits = 1, percent = TRUE) {
             checkForPackage (namePackage = "reshape", targetPackage = "eatRep")
             groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
             if (seOmit == TRUE) { object <- object[-grep("se", object[,"coefficient"]),]}
             ret <- merge ( dcast(data = object[which(object[,"parameter"] != "chiSquareTest"),], formula = reshapeFormula, value.var = "value"), object[ !duplicated(object[,c("group",groupCols), drop=FALSE]) ,c("group",groupCols), drop=FALSE], by = "group", all.x = TRUE, all.y = FALSE)
             whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
             if(percent == TRUE) {multip <- 100 } else { multip <- 1 }
             if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(multip*ret[,i], digits = roundDigits)}}
             return(ret)}

dQ <- function ( object, seOmit = FALSE) {
             groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
             if (seOmit == TRUE) { object <- object[-grep("se", object[,"coefficient"]),]}
             ret <- merge ( dcast(data = object, formula = depVar + group ~ parameter + coefficient, value.var = "value"), object[,c("group",groupCols)], by = "group", all.x = TRUE, all.y = FALSE)
             ret <- ret[!duplicated(ret[,"group"]),c("depVar",groupCols, setdiff(colnames(ret), c("depVar","group", groupCols)))]
             whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
             if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(ret[,i], digits = 3)}}
             return(ret)}

dG <- function ( object , analyses = NULL, digits = 3 ) {
            checkForPackage (namePackage = "reshape", targetPackage = "eatRep")
            splitData <- by ( data = object, INDICES = object[,c("group", "depVar")], FUN = function ( spl ) {return(spl)})
			      if(is.null(analyses)) {analyses <- 1:length(splitData)}
            retResults<- list()   
            for ( i in analyses) {
                 spl    <- splitData[[i]]
                 weg1   <- which ( spl[,"parameter"] %in% c("Ncases","Nvalid","R2","R2nagel", "deviance", "df.null", "df.residual", "null.deviance", "AIC"))
                 weg2   <- grep("wholePopDiff",spl[,"parameter"])
                 ret    <- dcast(spl[-c(weg1, weg2),], parameter~coefficient)
                 ret[,"t.value"] <- ret[,"est"] / ret[,"se"]
                 df     <- spl[ spl[,"parameter"] == "Nvalid" & spl[,"coefficient"] == "est"  ,"value"] - nrow(ret)
                 ret[,"p.value"] <- 2*(1-pt( q = abs(ret[,"t.value"]), df = df ))
                 retNR  <- ret
                 ret    <- data.frame ( lapply(ret, FUN = function ( y ) {if(class(y)=="numeric") {y <- round(y, digits = digits)}; return(y)}), stringsAsFactors = FALSE)
                 groupNamen <- setdiff(colnames(spl), c("group","depVar","modus", "parameter", "coefficient","value"))
                 cat ( paste( "            groups: ", paste( groupNamen, unlist(lapply(spl[1,groupNamen], as.character)), sep=" = ", collapse = "; "),"\n",sep=""))
                 cat ( paste( "dependent Variable: ", as.character(spl[1,"depVar"]), "\n \n", sep=""))
                 print(ret)
                 r2     <- spl[ spl[,"parameter"] == "R2" ,"value"]
                 r2nagel<- spl[ spl[,"parameter"] == "R2nagel" ,"value"]
                 cat(paste("\n            R-squared: ",round(r2[1],digits = digits),"; SE(R-squared): ",round(r2[2],digits = digits),"\n",sep=""))
                 cat(paste  ("Nagelkerkes R-squared: ",round(r2nagel[1],digits = digits),"; SE(Nagelkerkes R-squared): ",round(r2nagel[2],digits = digits),"\n",sep=""))
                 for ( prms in c("deviance", "null.deviance", "df.null", "df.residual", "AIC")) {
                       if ( prms %in% spl[,"parameter"] ) {
                            cat(paste  ( paste(rep(" ", times = 21 - nchar(prms)),collapse=""),prms,": ", round ( spl[intersect(which(spl[,"parameter"] == prms), which(spl[,"coefficient"] == "est")),"value"],digits = digits), "\n", sep=""))
                       }
                 }
                 nn     <- spl[ spl[,"parameter"] == "Nvalid" & spl[,"coefficient"] =="est" ,"value"]
                 cat(paste( round(nn, digits = 2), " observations and ",round(df,digits = 2), " degrees of freedom.",sep="")); cat("\n")
                 if(i != max(analyses)) { cat("------------------------------------------------------------------\n") }
                 retResults[[i]] <- data.frame ( groups = paste( groupNamen, unlist(lapply(spl[1,groupNamen], as.character)), sep=" = ", collapse = "; "), spl[1,groupNamen], depVar = spl[1,"depVar"], retNR, r2 = r2[1], r2nagel = r2nagel[1], df = df)
            }
            return(retResults)}

### Hilfsfunktion fuer jk2.glm() wenn Regression singulaere Terme enthaelt
### Achtung: negelkerke wird irgendwie falsch berechnet, keine Ahnung woran es liegt, wird ausgeschlossen
getOutputIfSingular <- function ( glmRes ) {
                       coefs <- na.omit(coef(glmRes))
                       rnagel<- unlist(NagelkerkeR2(glmRes))
                       names(rnagel) <- c("Nvalid", "R2nagel")
                       rnagel<- rnagel[1]
                       coefs <- c(coefs, R2 = var(glmRes$fitted.values)/var(glmRes$y), rnagel)
                       return(coefs)}

### wie oben, nur werden hier lineare Transformationen der Regressionskoeffizienten erlaubt
### unstandardisierte Koeffizienten werden genutzt, um den logit vorherzusagen
### jede person hat auf dieser logitvariablen einen wert
### standardabweichung der vorhergesagten logits + (pi^2) / 3 wird genutzt, um varianz der latenten variable zu bestimmen
### unstandardisierten logits werden durch varianz der lat. variablen geteilt, um standardisierte zu erhalten   
### Achtung: negelkerke wird irgendwie falsch berechnet, keine Ahnung woran es liegt, wird ausgeschlossen
getOutputIfSingularT1<- function ( glmRes) {
                       coefs <- na.omit(coef(glmRes))
                       pred  <- sd ( glmRes$linear.predictors ) +  (pi^2)/3
                       coefs <- coefs/pred
                       rnagel<- unlist(NagelkerkeR2(glmRes))
                       names(rnagel) <- c("Nvalid", "R2nagel")
                       rnagel<- rnagel[1]
                       coefs <- c(coefs, R2 = var(glmRes$fitted.values)/var(glmRes$y), rnagel)
                       return(coefs)}
                       

desk <- function(variable,na=NA, p.weights = NULL, na.rm = FALSE) {
         variable <- as.numeric.if.possible( data.frame(as.matrix(variable),stringsAsFactors = FALSE), verbose = FALSE )
         if(!is.null(p.weights)) {
             Mis.weight <- FALSE
             stopifnot( length(p.weights) == nrow(variable) )
             } else { Mis.weight <- TRUE}
         onlyMis  <- sapply(variable, FUN = function ( y ) { all( is.na(y) ) } )
         if(sum(onlyMis)>0) {
            cat("Folgende Variablen wurden aufgrund durchgehend fehlender oder nicht-numerischer Werte ausgeschlossen: \n")
            cat(paste(colnames(variable)[which(onlyMis)], collapse = ", ")); cat("\n")
            variable <- variable[, -which(onlyMis), drop = FALSE ]
         }
         weg      <- which(!sapply(variable, class) %in% c("numeric", "integer"))
         if(length(weg) == ncol(variable)) {stop("No numeric variable(s).\n")}
         if(length(weg)>0) {
            cat(paste("Following ",length(weg)," non-numeric variable(s) will be ignored: ", paste(colnames(variable)[weg], collapse = ", "), "\n", sep=""))
            variable <- variable[,-weg]
         }
         ret      <- do.call("rbind", lapply(variable, FUN = function ( y ) {
                     if(Mis.weight == TRUE ) {
                        Summe      <- sum(y, na.rm = na.rm)
                        Mittelwert <- mean(y, na.rm = na.rm)
                        Varianz    <- var(y, na.rm = na.rm)
                        N          <- length(y)
                        N.valid    <- length(na.omit(y)) }
                     if(Mis.weight == FALSE ) {
                        Summe <- sum( y * p.weights )
                        Mittelwert <- wtd.mean(x = y, weights = p.weights, na.rm = na.rm)
                        Varianz    <- wtd.var(x = y, weights = p.weights, na.rm = na.rm)
                        N          <- sum(p.weights)
                        N.valid    <- sum(p.weights[which(!is.na(y))]) }
                     dataFrame <- data.frame ( N = N, N.valid = N.valid, Missing = length(y) - length(na.omit(y)), Minimum = min(y, na.rm = na.rm), Maximum = max(y, na.rm = na.rm), Summe = Summe, Mittelwert = Mittelwert, std.err = sd(y, na.rm = na.rm) / sqrt(length(na.omit(y))), sig = ifelse(length(table(y))==1, NA, t.test(x = y)$p.value), Median = median(y, na.rm = na.rm), Streuung = sqrt(Varianz), Varianz = Varianz , stringsAsFactors = FALSE )
                     return(dataFrame)} ))
         rownames(ret) <- colnames(variable)
         return(ret)}
