getResults <- function ( runModelObj ) {
            if(runModelObj$software == "conquest") {
               return ( getConquestResults (path = runModelObj$dir, analysis.name=runModelObj$analysis.name, model.name=runModelObj$model.name, qMatrix=runModelObj$qMatrix))
            }
            if(runModelObj$software == "tam") {
               return(NULL)
            } }


runModel <- function(defineModelObj, show.output.on.console = FALSE, show.dos.console = TRUE, wait = TRUE) {
            if(defineModelObj$software == "conquest") {
               oldPfad <- getwd()
               setwd(defineModelObj$dir)
               system(paste(defineModelObj$conquest.folder," ",defineModelObj$input,sep=""),invisible=!show.dos.console,show.output.on.console=show.output.on.console, wait=wait)
               setwd(oldPfad)                                                   ### untere Zeile: Rueckgabeobjekt definieren: Conquest
               return ( defineModelObj )
            }
            if(defineModelObj$software == "tam") {                              ### exportiere alle Objekte aus defineModelObj in environment
               for ( i in names( defineModelObj )) { assign(i, defineModelObj[[i]]) }
               if(!exists("tam.mml"))       {library(TAM, quietly = TRUE)}      ### March, 2, 2013: fuer's erste ohne DIF, ohne polytome Items, ohne mehrgruppenanalyse, ohne 2PL
               if(!is.null(anchor)) {
                   stopifnot(ncol(anchor) == 2 )                                ### Untere Zeile: Wichtig! Sicherstellen, dass Reihenfolge der Items in Anker-Statement
                   notInData   <- setdiff(anchor[,1], all.Names[["variablen"]])
                   if(length(notInData)>0)  {
                      cat(paste("Found following ", length(notInData)," item(s) in anchor list which are not in the data:\n",sep=""))
                      cat(paste(notInData, collapse = ", ")); cat("\n")
                      cat("Delete missing item(s) from anchor list.\n")
                      anchor <- anchor[-match(notInData, anchor[,1]),]
                   }
                   anchor[,1]    <- match(as.character(anchor[,1]), all.Names[["variablen"]])
               }
               if(length( all.Names[["HG.var"]])>0)     { Y <- daten[,all.Names[["HG.var"]], drop=FALSE] } else { Y <- NULL }
               if(length( all.Names[["weight.var"]])>0) { wgt <- as.vector(daten[,all.Names[["weight.var"]]])} else {wgt <- NULL}
               stopifnot(all(qMatrix[,1] == all.Names[["variablen"]]))
               if(length(all.Names[["DIF.var"]]) == 0 ) {
                  if( irtmodel %in% c("1PL", "PCM", "PCM2", "RSM") ) {
                      mod     <- tam.mml(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = list(progress = progress, maxiter = n.iterations, increment.factor=increment.factor , fac.oldxsi=fac.oldxsi))
                  }
                  if( irtmodel %in% c("2PL", "GPCM", "2PL.groups", "GPCM.design", "3PL") )  {
                      if(!is.null(est.slopegroups))  {
                          weg1            <- setdiff(all.Names[["variablen"]], est.slopegroups[,1])
                          if(length(weg1)>0) {stop("Items in dataset which are not defined in design matrix for item groups with common slopes ('est.slopegroups').\n")}
                          weg2            <- setdiff(est.slopegroups[,1], all.Names[["variablen"]])
                          if(length(weg2)>0) {
                             cat(paste("Following ",length(weg2), " Items in design matrix for item groups with common slopes ('est.slopegroups') which are not in dataset:\n",sep=""))
                             cat("   "); cat(paste(weg2, collapse=", ")); cat("\n")
                             cat("Remove these item(s) from design matrix.\n")
                             est.slopegroups <- est.slopegroups[-match(weg2,est.slopegroups[,1]),]
                          }
                          est.slopegroups <- est.slopegroups[match(all.Names[["variablen"]], est.slopegroups[,1]),2]
                      }
                      if( irtmodel == "3PL") {
                          if(is.null(guessMat)) {
                             cat("No matrix for guessing parameters defined. Assume unique guessing parameter for each item.\n")
                             guessMat     <- data.frame ( item = all.Names[["variablen"]], guessingGroup = 1:length(all.Names[["variablen"]]), stringsAsFactors = FALSE)
                          } else {
                            weg1          <- setdiff(all.Names[["variablen"]], guessMat[,1])
                            if(length(weg1)>0) {cat(paste(length(weg1), " item(s) in dataset which are not defined in guessing matrix. No guessing parameter will be estimated for these/this item(s).\n",sep="")) }
                            weg2          <- setdiff(guessMat[,1], all.Names[["variablen"]])
                            if(length(weg2)>0) {
                               cat(paste(length(weg2), " item(s) in guessing matrix missing in dataset. Remove these items from guessing matrix.\n",sep=""))
                               guessMat   <- guessMat[-match( weg2, guessMat[,1])  ,]
                            }
                          }
                          gues <- guessMat[ match( all.Names[["variablen"]], guessMat[,1]) , "guessingGroup"]
                          gues[which(is.na(gues))] <- 0
                          mod  <- tam.mml.3pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, pweights = wgt, est.guess =gues, control = list(progress = progress, maxiter = n.iterations, increment.factor=increment.factor , fac.oldxsi=fac.oldxsi))
                      }  else { mod     <- tam.mml.2pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, est.slopegroups=est.slopegroups,pweights = wgt, control = list(progress = progress, maxiter = n.iterations, increment.factor=increment.factor , fac.oldxsi=fac.oldxsi)) }
                  }
               } else {
                 assign(paste("DIF_",all.Names[["DIF.var"]],sep="") , as.data.frame (daten[,all.Names[["DIF.var"]]]) )
                 formel   <- as.formula(paste("~item - ",paste("DIF_",all.Names[["DIF.var"]],sep="")," + item * ",paste("DIF_",all.Names[["DIF.var"]],sep=""),sep=""))
                 facetten <- as.data.frame (daten[,all.Names[["DIF.var"]]])
                 colnames(facetten) <- paste("DIF_",all.Names[["DIF.var"]],sep="")
                 mod      <- tam.mml.mfr(resp = daten[,all.Names[["variablen"]]], facets = facetten, formulaA = formel, pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = list(progress = progress, maxiter = n.iterations, increment.factor=increment.factor , fac.oldxsi=fac.oldxsi))
               }
               return(mod)  }  }


defineModel <- function(dat, items, id, irtmodel = c("1PL", "2PL", "PCM", "PCM2", "RSM", "GPCM", "2PL.groups", "GPCM.design", "3PL"),
               qMatrix=NULL, DIF.var=NULL, HG.var=NULL, group.var=NULL, weight.var=NULL, anchor = NULL, check.for.linking = TRUE,
               remove.no.answers = TRUE, remove.missing.items = TRUE, remove.constant.items = TRUE, verbose=TRUE,
               software = c("conquest","lme4", "tam"), dir = NULL, analysis.name, model.statement = "item",  compute.fit = TRUE,
               n.plausible=5, seed = NULL, conquest.folder=NULL,constraints=c("cases","none","items"),std.err=c("quick","full","none"),
               distribution=c("normal","discrete"), method=c("gauss", "quadrature", "montecarlo"), n.iterations=2000,nodes=NULL, p.nodes=2000,
               f.nodes=2000,converge=0.001,deviancechange=0.0001, equivalence.table=c("wle","mle","NULL"), use.letters=FALSE, allowAllScoresEverywhere = TRUE,
               guessMat = NULL, est.slopegroups = NULL, progress = FALSE, increment.factor=1 , fac.oldxsi=0,
               export = list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE) )   {
                  if(!"data.frame" %in% class(dat) ) { cat("Convert 'dat' to a data.frame.\n"); dat <- data.frame ( dat, stringsAsFactors = FALSE)}
                  irtmodel <- match.arg(irtmodel)
                  software <- match.arg(software)
                  if(software == "conquest") {
                     original.options <- options("scipen")                      ### lese Option für Anzahl der Nachkommastellen
                     options(scipen = 20)                                       ### setze Option für Anzahl der Nachkommastellen
                     if(missing(analysis.name)) {stop("'analysis.name' not specified.\n") }   }
                  if(length(model.statement)!=1)            {stop("'model.statement' has to be of length 1.\n")}
                  if(class(model.statement)!="character")   {stop("'model.statement' has to be of class 'character'.\n")}
                  if(missing(dat)) {stop("No dataset specified.\n") }           ### 11.04.2014: nutzt Hilfsfunktionen von jk2.mean etc.
                  allVars     <- list(ID = id, variablen=items, DIF.var=DIF.var, HG.var=HG.var, group.var=group.var, weight.var=weight.var)
                  all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                  doppelt     <- which(duplicated(dat[,all.Names[["ID"]]]))
                  if(length(doppelt)>0)  {stop(paste( length(doppelt) , " duplicate IDs found!",sep=""))}
                  dir <- crop(dir,"/")
     ### Sektion 'explizite Variablennamen ggf. aendern' ###
                  subsNam <- .substituteSigns(dat=dat, variable=unlist(all.Names[-c(1:2)]))
                  if(software == "conquest") {                                  ### Conquest erlaubt keine gross geschriebenen und expliziten Variablennamen, die ein "." oder "_" enthalten
                     if(!all(subsNam$old == subsNam$new)) {
                        sn     <- subsNam[which( subsNam$old != subsNam$new),]
                        cat("Conquest neither allows '.', '-' and '_' now upper case letters in explicit variable names. Delete signs from variables names for explicit variables.\n"); flush.console()
                        recStr <- paste("'",sn[,"old"] , "' = '" , sn[,"new"], "'" ,sep = "", collapse="; ")
                        colnames(dat) <- car::recode(colnames(dat), recStr)
                        all.Names     <- lapply(all.Names, FUN = function ( y ) { car::recode(y, recStr) })
                        if(model.statement != "item") {
                           cat("    Remove deleted signs from variables names for explicit variables also in the model statement. Please check afterwards for consistency!\n")
                           model.statement <- gsub(sn[,"old"], sn[,"new"], model.statement)
                        }
                     }
                     if("item" %in% unlist(all.Names[-c(1:2)])) { stop("Conquest does not allow labelling explicit variable(s) with 'Item' or 'item'.\n") }
                 }                                                              ### untere Zeilen: Dif-Variablen und Testitems dürfen sich nicht überschneiden
                  if(length(intersect(all.Names$DIF.var, all.Names$variablen))>0)    {stop("Test items and DIF variable have to be mutually exclusive.\n")}
                  if(length(intersect(all.Names$weight.var, all.Names$variablen))>0) {stop("Test items and weighting variable have to be mutually exclusive.\n")}
                  if(length(intersect(all.Names$HG.var, all.Names$variablen))>0)     {stop("Test items and HG variable have to be mutually exclusive.\n")}
                  if(length(intersect(all.Names$group.var, all.Names$variablen))>0)  {stop("Test items and group variable have to be mutually exclusive.\n")}
     ### Sektion 'Q matrix ggf. erstellen und auf Konsistenz zu sich selbst und zu den Daten pruefen' ###
                 if(is.null(qMatrix)) { qMatrix <- data.frame ( Item = all.Names$variablen, Dim1 = 1, stringsAsFactors = FALSE) } else {
                     qMatrix <- checkQmatrixConsistency(qMatrix)                ### pruefe Konsistenz der q-matrix
                     notInDat<- setdiff(qMatrix[,1], all.Names$variablen)
                     notInQ  <- setdiff( all.Names$variablen , qMatrix[,1])
                     if(length(notInDat)>0) {
                        cat(paste("Following ", length(notInDat)," item(s) missed in data frame will removed from Q matrix: \n    ",paste(notInDat,collapse=", "),"\n",sep=""))
                        qMatrix <- qMatrix[-match(notInDat, qMatrix[,1]),]
                     }
                     if(length(notInQ)>0) {
                        cat(paste("Following ", length(notInQ)," item(s) missed in Q matrix will removed from data: \n    ",paste(notInQ,collapse=", "),"\n",sep=""))
                     }
                     all.Names$variablen <- qMatrix[,1]  } ;   flush.console()  ### Wichtig! Sicherstellen, dass Reihenfolge der Items in Q-Matrix mit Reihenfolge der Items im Data.frame uebereinstimmt!
     ### Sektion 'Alle Items auf einfache Konsistenz pruefen' ###
                  namen.items.weg <- NULL
                  is.NaN <- do.call("cbind", lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function (uu) { is.nan(uu) } ) )
                  if(sum(is.NaN) > 0 ) {dat[is.NaN] <- NA}                      ### Wandle NaN in NA, falls es welche gibt
                  n.werte <- lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN=function(ii) {table(ii)})
                  onlyHomogenBezeichner <- lapply(n.werte, FUN = function (zz) {### geprueft werden Testitems: Keine Werte? konstant? nicht dichotom?
                             zahl <- grep("[[:digit:]]", names(zz))
                             buch <- grep("[[:alpha:]]", names(zz))
                             ret  <- (length(zahl) == length(zz) & length(buch) == 0 ) | (length(zahl) == 0 & length(buch) == length(zz) )
                             return(ret)})
                  noHomogenBezeichner   <- which(onlyHomogenBezeichner == FALSE)
                  datasetBezeichner     <- unique(unlist(lapply(n.werte, names)))
                  zahl                  <- grep("[[:digit:]]", datasetBezeichner )
                  buch                  <- grep("[[:alpha:]]", datasetBezeichner )
                  ret                   <- (length(zahl) == length(datasetBezeichner) & length(buch) == 0 ) | (length(zahl) == 0 & length(buch) == length(datasetBezeichner) )
                  options(warn = -1)                                            ### zuvor: schalte Warnungen aus!
                  only.null.eins        <- unlist( lapply(n.werte, FUN=function(ii) {all( names(ii) == c("0","1") ) }) )
                  options(warn = 0)                                             ### danach: schalte Warnungen wieder an!
                  n.werte <- sapply(n.werte, FUN=function(ii) {length(ii)})
                  n.mis   <- which(n.werte == 0)
                  if(length(n.mis) >0) {cat(paste("Serious warning: ",length(n.mis)," testitems(s) without any values.\n",sep=""))
                                        if(verbose == TRUE) {cat(paste("    ", paste(names(n.mis), collapse=", "), "\n", sep=""))}
                                        if(remove.missing.items == TRUE) {
                                           cat(paste("Remove ",length(n.mis)," variable(s) due to solely missing values.\n",sep=""))
                                           namen.items.weg <- c(namen.items.weg, names(n.mis))}}
                  n.constant <- which(n.werte == 1)
                  if(length(n.constant) >0) {cat(paste("Warning: ",length(n.constant)," testitems(s) are constants.\n",sep=""))
                                             if(verbose == TRUE) {foo <- lapply(names(n.constant),FUN=function(ii) {cat(paste(ii,": ",names(table(dat[,ii])),sep="")); cat("\n")})}
                                             if(remove.constant.items == TRUE) {
                                                cat(paste("Remove ",length(n.constant)," variable(s) due to solely constant values.\n",sep=""))
                                                namen.items.weg <- c(namen.items.weg, names(n.constant))}}
                  n.rasch   <- which( !only.null.eins )
                  if(length(n.rasch) >0 )   {cat(paste("Warning: ",length(n.rasch)," variable(s) are not strictly dichotomous with 0/1.\n",sep=""))
                                             for (ii in 1:length(n.rasch))  {
                                                  max.nchar <-  max(nchar(names(table(dat[,names(n.rasch)[ii]]))))
                                                  if(max.nchar>1) {cat(paste("Arity of variable",names(n.rasch)[ii],"exceeds 1.\n"))}
                                                  if(verbose == TRUE) {cat(paste(names(n.rasch)[ii],": ", paste( names(table(dat[,names(n.rasch)[ii]])),collapse=", "),"\n",sep=""))}}
                                             cat("Expect a rating scale model or partial credit model.\n")
                                             if(model.statement == "item")
                                               {cat("WARNING: Sure you want to use 'model statement = item' even when items are not dichotomous?\n")} }
                  if(length(noHomogenBezeichner)>0) {
                     stop(paste("Item(s) ",paste(names(noHomogenBezeichner), collapse=", ")," with mixed response identifier (numeric and string).\n",sep=""))}
                  if(ret == FALSE ) {
                     stop("Itemdata with inconsistant response identifier (numeric and string).\n")}
     ### Sektion 'Hintergrundvariablen auf Konsistenz zu sich selbst und zu den Itemdaten pruefen'. Ausserdem Stelligkeit (Anzahl der benoetigten character) fuer jede Variable herausfinden ###
                  weg.dif <- NULL; weg.hg <- NULL; weg.weight <- NULL; weg.group <- NULL
                  if(length(all.Names$HG.var)>0)    {
                     hg.info <- lapply(all.Names$HG.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="HG", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                     for ( i in 1:length(hg.info)) { dat[, hg.info[[i]]$varname ] <- hg.info[[i]]$x }
                     weg.hg  <- unique(unlist(lapply(hg.info, FUN = function ( y ) {y$weg})))
                     if(length(weg.hg)>0)                                       ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                       {cat(paste("Remove ",length(weg.hg)," cases with missings on at least one HG variable.\n",sep=""))}
                  }
                  if(length(all.Names$group.var)>0)  {
                     group.info <- lapply(all.Names$group.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="group", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                     for ( i in 1:length(group.info)) { dat[, group.info[[i]]$varname ] <- group.info[[i]]$x }
                     weg.group  <- unique(unlist(lapply(group.info, FUN = function ( y ) {y$weg})))
                     if(length(weg.group)>0)                                    ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {cat(paste("Remove ",length(weg.group)," cases with missings on group variable.\n",sep=""))}
                  }
                  if(length(all.Names$DIF.var)>0)  {
                     dif.info <- lapply(all.Names$DIF.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="DIF", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                     for ( i in 1:length(dif.info)) { dat[, dif.info[[i]]$varname ] <- dif.info[[i]]$x }
                     weg.dif  <- unique(unlist(lapply(dif.info, FUN = function ( y ) {y$weg})))
                     if(length(weg.dif)>0)                                      ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {cat(paste("Remove ",length(weg.dif)," cases with missings on DIF variable.\n",sep=""))}
                  }
                  if(length(all.Names$weight.var)>0)  {
                     if(length(all.Names$weight.var)!=1) {stop("Use only one weight variable.")}
                     weight.info <- lapply(all.Names$weight.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="weight", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                     for ( i in 1:length(weight.info)) { dat[, weight.info[[i]]$varname ] <- weight.info[[i]]$x }
                     weg.weight  <- unique(unlist(lapply(weight.info, FUN = function ( y ) {y$weg})))
                     if(length(weg.weight)>0)                                   ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {cat(paste("Remove ",length(weg.weight)," cases with missings on weight variable.\n",sep=""))}
                  }                                                             ### untere Zeile, Achtung: group- und DIF- bzw. group- und HG-Variablen dürfen sich überschneiden!
                  namen.all.hg <- unique(c(all.Names$HG.var,all.Names$group.var,all.Names$DIF.var,all.Names$weight.var))
                  if(length(namen.all.hg)>0) {all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(dat[,ii]))))})} else {all.hg.char <- NULL}
                  weg.all <- unique(c(weg.dif, weg.hg, weg.weight, weg.group))
                  if(length(weg.all)>0) {
                     cat(paste("Remove",length(weg.all),"case(s) overall due to missings on at least one explicit variable.\n"))
                     dat   <- dat[-weg.all,]
                  }
     ### Sektion 'Itemdatensatz zusammenbauen' (fuer Conquest ggf. mit Buchstaben statt Ziffern) ###
                  if(length(namen.items.weg)>0)  {
                     cat(paste("Remove ",length(unique(namen.items.weg))," test item(s) overall.\n",sep=""))
                     all.Names$variablen <- setdiff(all.Names$variablen, unique(namen.items.weg) )
                     qMatrix             <- qMatrix[match(all.Names$variablen, qMatrix[,1]),]
                  }
     ### Sektion 'Personen ohne gueltige Werte identifizieren und ggf. loeschen' ###
                  datL  <- reshape2::melt(data = dat, id.vars = unique(unlist(all.Names[-match("variablen", names(all.Names))])), measure.vars = all.Names[["variablen"]], na.rm=TRUE)
                  weg   <- setdiff(dat[,all.Names[["ID"]]], unique(datL[,all.Names[["ID"]]]))
                  if(length(weg)>0)   {                                         ### identifiziere Faelle mit ausschließlich missings
                     cat(paste("Found ",length(weg)," cases with missings on all items.\n",sep=""))
                     if( remove.no.answers == TRUE)  {cat("Cases with missings on all items will be deleted.\n"); dat <- dat[-match(weg,dat[,all.Names[["ID"]]] ) ,]  }
                     if( remove.no.answers == FALSE) {cat("Cases with missings on all items will be kept.\n")}}
     ### Sektion 'Verlinkung pruefen' ###
                  if(check.for.linking == TRUE) {                               ### Dies geschieht auf dem nutzrspezifisch reduzierten/selektierten Datensatz
                     linkNaKeep <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = FALSE, verbose = FALSE )
                     linkNaOmit <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = TRUE, verbose = FALSE )
                     if(linkNaKeep == FALSE & linkNaOmit == FALSE ) {cat("WARNING! Dataset is NOT completely linked (even if cases with missings on all items are removed).\n")}
                     if(linkNaKeep == FALSE & linkNaOmit == TRUE )  {cat("Note: Dataset is not completely linked. This is probably only due to missings on all cases.\n")}
                     if(linkNaKeep == TRUE )                        {cat("Dataset is completely linked.\n")}
                  }
     ### Sektion 'Datensaetze softwarespezifisch aufbereiten: Conquest' ###
                  if ( software == "conquest" )   {                             ### untere Zeile: wieviele character muss ich fuer jedes Item reservieren?
                      var.char <- sapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN=function(ii) {max(nchar(as.character(na.omit(ii))))})
                      no.number <- setdiff(1:length(var.char), grep("[[:digit:]]",var.char))
                      if(length(no.number)>0) {var.char[no.number] <- 1}        ### -Inf steht dort, wo nur missings sind, hier soll die Characterbreite auf 1 gesetzt sein
                      if(use.letters == TRUE)   {                               ### sollen Buchstaben statt Ziffern beutzt werden? Dann erfolgt hier Recodierung.
                         rec.statement <- paste(0:25,"='",LETTERS,"'",sep="",collapse="; ")
                         for (i in all.Names[["variablen"]])  {                 ### Warum erst hier? Weil Prüfungen (auf Dichotomität etc. vorher stattfinden sollen)
                              dat[,i] <- car::recode(dat[,i], rec.statement)}
                         var.char <- rep(1,length(all.Names[["variablen"]]))}   ### var.char muß nun neu geschrieben werden, da nun alles wieder einstellig ist!
                  }
                  daten    <- data.frame(ID=as.character(dat[,all.Names[["ID"]]]), dat[,namen.all.hg, drop = FALSE], dat[,all.Names[["variablen"]], drop = FALSE], stringsAsFactors = FALSE)
                  daten$ID <- gsub ( " ", "0", formatC(daten$ID, width=max(as.numeric(names(table(nchar(daten$ID)))))) )
                  if ( software == "conquest" )   {
                      fixed.width <- c(as.numeric(names(table(nchar(daten[,"ID"])))), all.hg.char, rep(max(var.char),length(var.char)))
                      gdata::write.fwf(daten , file.path(dir,paste(analysis.name,".dat",sep="")), colnames = FALSE,rownames = FALSE, sep="",quote = FALSE,na=".", width=fixed.width)
                      test <- readLines(paste(dir,"/",analysis.name,".dat",sep=""))
                      stopifnot(length(table(nchar(test)))==1)                  ### Check: hat der Resultdatensatz eine einheitliche Spaltenanzahl? Muß unbedingt sein!
                      lab <- data.frame(1:length(all.Names[["variablen"]]), all.Names[["variablen"]], stringsAsFactors = FALSE)
                      colnames(lab) <- c("===>","item")                         ### schreibe Labels!
                      write.table(lab,file.path(dir,paste(analysis.name,".lab",sep="")),col.names = TRUE,row.names = FALSE, dec = ",", sep = " ", quote = FALSE)
                      if(!is.null(conquest.folder))     {
                         batch <- paste( normalize.path(conquest.folder),paste(analysis.name,".cqc",sep=""), sep=" ")
                         write(batch, file.path(dir,paste(analysis.name,".bat",sep="")))}
                      foo <- gen.syntax(Name=analysis.name, daten=daten, all.Names = all.Names, namen.all.hg = namen.all.hg, all.hg.char = all.hg.char, var.char= max(var.char), model=qMatrix, ANKER=anchor, pfad=dir, n.plausible=n.plausible, compute.fit = compute.fit,
                                        constraints=constraints, std.err=std.err, distribution=distribution, method=method, n.iterations=n.iterations, nodes=nodes, p.nodes=p.nodes, f.nodes=f.nodes, converge=converge,deviancechange=deviancechange, equivalence.table=equivalence.table, use.letters=use.letters, model.statement=model.statement, conquest.folder = conquest.folder, allowAllScoresEverywhere = allowAllScoresEverywhere, seed = seed, export = export)
                      if(!is.null(anchor))  { foo <- anker (lab.file = file.path(dir, paste(analysis.name,"lab",sep=".")), prm = anchor) }
     ### Sektion 'Rueckgabeobjekt bauen', hier fuer Conquest                    ### setze Optionen wieder in Ausgangszustand
                      options(scipen = original.options); flush.console()       ### Achtung: setze Konsolenpfade in Hochkommas, da andernfalls keine Leerzeichen in den Ordner- bzw. Dateinamen erlaubt sind!
                      return ( list ( software = software, input = paste("\"", file.path(dir, paste(analysis.name,"cqc",sep=".")), "\"", sep=""), conquest.folder = paste("\"", conquest.folder, "\"", sep=""), dir=dir, analysis.name=analysis.name, model.name = analysis.name, qMatrix=qMatrix ) )  }
     ### Sektion 'Rueckgabeobjekt fuer tam'
                  if ( software == "tam" )   {
                      return ( list ( software = software, qMatrix=qMatrix, anchor=anchor,  all.Names=all.Names, daten=daten, irtmodel=irtmodel, progress = progress, n.iterations = n.iterations, increment.factor=increment.factor , fac.oldxsi=fac.oldxsi ,
                                est.slopegroups = est.slopegroups, guessMat=guessMat))    }
   }


### Hilfsfunktionen für prep.conquest
.checkContextVars <- function(x, varname, type, itemdaten)   {
                     if(missing(varname))  {varname <- "ohne Namen"}
                     if(class(x) != "numeric")  {                               ### ist Variable numerisch?
                        if (type == "weight") {stop(paste(type, " variable has to be 'numeric' necessarily. Automatic transformation is not recommended. Please transform by yourself.\n",sep=""))}
                        cat(paste(type, " variable has to be 'numeric'. Variable '",varname,"' of class '",class(x),"' will be transformed to 'numeric'.\n",sep=""))
                        x <- unlist(as.numeric.if.possible(dataFrame = data.frame(x, stringsAsFactors = FALSE), transform.factors = TRUE, maintain.factor.scores = FALSE, verbose=FALSE))
                        if(class(x) != "numeric")  {                            ### erst wenn as.numeric.if.possible fehlschlägt, wird mit Gewalt numerisch gemacht, denn für Conquest MUSS es numerisch sein
                           x <- as.numeric(as.factor(x))
                        }
                        cat(paste("    '", varname, "' was converted into numeric variable of ",length(table(x))," categories. Please check whether this was intended.\n",sep=""))
                        if(length(table(x)) < 12 ) { cat(paste("    Values of '", varname, "' are: ",paste(names(table(x)), collapse = ", "),"\n",sep=""))}
                     }
                     mis     <- length(table(x))
                     if(mis == 0 )  {stop(paste("Error: ",type," Variable '",varname,"' without any values.",sep=""))}
                     if(mis == 1 )  {stop(paste("Error: ",type," Variable '",varname,"' is a constant.",sep=""))}
                     if(type == "DIF" | type == "group") {if(mis > 10)   {cat(paste("Serious warning: ",type," Variable '",varname,"' with more than 10 categories. Recommend recoding. \n",sep=""))}}
                     char    <- max(nchar(as.character(na.omit(x))))
                     weg     <- which(is.na(x))
                     if(length(weg) > 0 ) {cat(paste("Warning: Found ",length(weg)," cases with missing on ",type," variable '",varname,"'. Conquest probably will collapse unless cases are not deleted.\n",sep=""))}
                     if(type == "DIF" ) {
                                   if(mis > 2 )   {cat(paste(type, " Variable '",varname,"' does not seem to be dichotomous.\n",sep=""))}
                                   n.werte <- lapply(itemdaten, FUN=function(iii){by(iii, INDICES=list(x), FUN=table)})
                                   completeMissingGroupwise <- data.frame(t(sapply(n.werte, function(ll){lapply(ll, FUN = function (uu) { length(uu[uu>0])}  )})), stringsAsFactors = FALSE)
                                   for (iii in seq(along=completeMissingGroupwise)) {
                                        missingCat.i <- which(completeMissingGroupwise[,iii] == 0)
                                        if(length(missingCat.i) > 0) {
                                           cat(paste("Warning: Following items with no values in ",type," variable '",varname,"', group ",iii,": \n",sep=""))
                                           cat(paste(rownames(completeMissingGroupwise)[missingCat.i],collapse=", ")); cat("\n")
                                        }
                                        constantCat.i <- which(completeMissingGroupwise[,iii] == 1)
                                        if(length(constantCat.i) > 0) {
                                           cat(paste("Warning: Following items are constants in ",type," variable '",varname,"', group ",iii,":\n",sep=""))
                                           cat(paste(rownames(completeMissingGroupwise)[constantCat.i],collapse=", ")); cat("\n")
                                        }
                                   }
                     }
                     return(list(x = x, char = char, weg = weg, varname=varname))}


.existsBackgroundVariables <- function(dat, variable )  {
                             if(!is.null(variable[1]))  {
            								 if(is.factor(variable))    {
            								    v  <- as.character(variable)
            								    rN <- remove.numeric(v)
            								    if(all (nchar(rN) == 0 ) ) { variable <- as.numeric(v) } else { variable <- as.character(variable)}
            								 }
                             if(is.character(variable))  {
            									 misVariable <- setdiff(variable, colnames(dat))
            									 if(length(misVariable)>0) {cat(paste("Can't find ",length(misVariable)," variable(s) in dataset.\n",sep=""))
            									 cat(paste(misVariable,collapse=", ")); cat("\n"); stop()}
            									 varColumn <- match(variable, colnames(dat))
            								 }
            								 if(is.numeric(variable))   {
                                if(ncol(dat) < max(variable) ) {stop("Designated column number exceeds number of columns in dataset.\n")}
                                varColumn <- variable
                             }
                           return(colnames(dat)[varColumn])
            							 }  else { return(NULL)}
                             }


.substituteSigns <- function(dat, variable ) {
                    if(!is.null(variable)) {
           					   variableNew <- tolower(gsub("_|\\.|-", "", variable))
                       cols        <- match(variable, colnames(dat))
           					   return(data.frame(cols=cols, old=variable,new=variableNew, stringsAsFactors = FALSE))
           					}
                    if(is.null(variable)) {return(data.frame(old=TRUE,new=TRUE))}
                    }


checkQmatrixConsistency <-  function(qmat) {  
             if(class(qmat) != "data.frame")    { qmat     <- data.frame(qmat, stringsAsFactors = FALSE)}
             if(class(qmat[,1]) != "character") { qmat[,1] <- as.character(qmat[,1])}
             werte <- table.unlist(qmat[,-1,drop=FALSE], useNA="always")
             if(length(setdiff( names(werte) , c("0","1", "NA")))<0) {stop("Q matrix must not contain entries except '0' and '1'.\n")}
             if(werte[match("NA", names(werte))] > 0) {stop("Missing values in Q matrix.\n")}
             doppel<- which(duplicated(qmat[,1]))
             if(length(doppel)>0) {
                cat("Found duplicated elements in the item id column of the q matrix. Duplicated elements will be removed.\n")
                chk  <- table(qmat[,1])
                chk  <- chk[which(chk > 1)]
                chkL <- lapply(names(chk), FUN = function ( ch ) { 
                        qChk <- qmat[which(qmat[,1] == ch),]
                        pste <- apply(qChk, 1, FUN = function ( x ) { paste(x[-1], collapse="")})
                        if( !all ( pste == pste[1] )) { cat("Inconsistent q matrix.\n"); stop()}
                        })
             }           
             zeilen<- apply(qmat, 1, FUN = function ( y ) { all ( names(table(y[-1])) == "0")  })
             weg   <- which(zeilen == TRUE)
             if(length(weg)>0) { 
                cat(paste("Note: Following ",length(weg)," item(s) in Q matrix do not belong to any dimension. Delete these item(s) from Q matrix.\n",sep=""))
                cat("    "); cat(paste(qmat[weg,1],collapse=", ")); cat("\n")
                qmat  <- qmat[-weg,]
             }
             return(qmat)}   


checkLink <- function(dataFrame, remove.non.responser = FALSE, sysmis = NA, verbose = TRUE)   {
             if(!is.na(sysmis))  {
               na <- which(is.na(dataFrame))
               if(length(na)>0)  {
                  cat(paste("Warning: '",sysmis,"' was specified to denote 'sysmis' in the data. ",length(na)," 'NA'-values were found in the dataset anyway. \n         Hence, ",sysmis," and 'NA' will be handled as 'sysmis'.\n",sep=""))
               }
               dataFrame <- as.data.frame(lapply(dataFrame, FUN=function(ii) {car::recode(ii, paste(sysmis,"= NA", collapse = "; ") ) } ) )
             }
             if ( remove.non.responser == TRUE ) {
                na <- which( rowSums(is.na(dataFrame)) == ncol ( dataFrame ) )
                if(length(na)>0) {
                   dataFrame <- dataFrame[-na,]
                   if(verbose == TRUE ) {cat(paste("Remove ",length(na)," cases with missing on all items.\n", sep = ""))}
                }
             }
             non.missing.cases <- lapply(dataFrame, FUN=function(ii) {which(!is.na(ii))})
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
             if (length(all.cases) != nrow(dataFrame))   {
                if (verbose == TRUE) {cat("WARNING! Dataset is not completely linked.\n") }
                return(FALSE)
             }
             if (length(all.cases) == nrow(dataFrame))   {
                if (verbose == TRUE) {cat("Dataset is completely linked.\n") }
                return(TRUE)
             }  }


getConquestResults<- function(path, analysis.name, model.name, qMatrix) {
         allFiles <- list.files(path=path, pattern = analysis.name, recursive = FALSE)
         qL       <- reshape2::melt(qMatrix, id.vars = colnames(qMatrix)[1], na.rm=TRUE)
         qL       <- qL[which(qL[,"value"] != 0 ) , ]
         varName  <- colnames(qMatrix)[1]
         ret      <- NULL                                                       ### Rueckgabeobjekt initialisieren
    ### Sektion 'Itemparameter auslesen' (shw)
         shwFile  <- paste(analysis.name, "shw", sep=".")
         if (!shwFile %in% allFiles) {
             cat("Cannot find Conquest showfile.\n")
         } else {
             shw  <- get.shw( file.path(path, shwFile) )                        ### Untere Zeile: Dimensionen analog zu Bezeichnung in Q Matrix benennen
             if(is.null( dim(shw$cov.structure) )) {from <- NA} else { from <- shw$cov.structure[-ncol(shw$cov.structure),1]}
             altN <- data.frame ( nr = 1:(ncol(qMatrix)-1), pv = paste("dim", 1:(ncol(qMatrix)-1),sep="."), from = from ,  to = colnames(qMatrix)[-1], stringsAsFactors = FALSE)
             shw1 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],shw$item[,"item"]),"variable"], par = "est",  derived.par = NA, value = as.numeric(shw$item[,"ESTIMATE"]), stringsAsFactors = FALSE)
             shw2 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items",group = qL[match(qL[,varName],shw$item[,"item"]),"variable"], par = "est",  derived.par = "se", value = as.numeric(shw$item[,"ERROR"]), stringsAsFactors = FALSE)
             toOff<- shw2[ which(is.na(shw2[,"value"])), "var1"]
             if(length(toOff)>0) {
                shw1[match(toOff, shw1[,"var1"]), "par"] <- "offset"
                shw2  <- shw2[-which(is.na(shw2[,"value"])),] }                 ### entferne Zeilen aus shw2, die in der "value"-Spalte NA haben
             ret  <- rbind(ret, shw1, shw2)                                     ### Rueckgabeobjekt befuellen, danach infit auslesen
             ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],shw$item[,"item"]),"variable"], par = "est",  derived.par = "infit", value = as.numeric(shw$item[,"MNSQ.1"]), stringsAsFactors = FALSE))
             if(length(shw) > 4 )  {                                            ### ggf. Parameter zusaetzlicher Conquest-Terme einlesen
                read  <- 2 : (length(shw) - 3)                                  ### Diese Terme muessen eingelesen werden
                for ( i in names(shw)[read] ) {
                     cols <- unlist(isLetter(i))                                ### versuche Spalte(n) zu identifizieren
                     if( !all(cols %in% colnames(shw[[i]])) ) {
                         cat(paste("Cannot identify variable identifier for term '",i,"' in file '",shwFile,"'. Skip procedure.\n",sep=""))
                     }  else  {
                         if(length(cols) == 1 ) {var1 <- paste( cols, shw[[i]][,cols],sep="_") } else { var1 <- unlist(apply(shw[[i]][,cols], MARGIN=1, FUN = function ( y ) {
                            paste ( unlist(lapply ( 1:length(y), FUN = function ( yy ) { paste(names(y)[yy], y[yy],sep="_")})), sep="", collapse = "_X_")  }))}
                         if(ncol(qMatrix) != 2 ){
                            cat(paste("Warning: Cannot identify the group the term '",i,"' in file '",shwFile,"' belongs to. Insert 'NA' to the 'group' column.\n",sep=""))
                            gr <- NA
                         }  else { gr <- colnames(qMatrix)[2]}
                         shwE <- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = NA, value = shw[[i]][,"ESTIMATE"], stringsAsFactors = FALSE)
                         shwE2<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "infit", value = shw[[i]][,"MNSQ.1"], stringsAsFactors = FALSE)
                         shwSE<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "se", value = shw[[i]][,"ERROR"], stringsAsFactors = FALSE)
                         toOff<- shwSE[ which(is.na(shwSE[,"value"])), "var1"]
                         if(length(toOff)>0) {
                            shwE[match(toOff, shwE[,"var1"]), "par"] <- "offset"
                            shwSE <- shwSE[-which(is.na(shwSE[,"value"])),] }
                         ret  <- rbind(ret, shwE, shwE2, shwSE)
                     }}}
    ### Sektion 'Populationsparameter auslesen' (shw)
             if(ncol(qMatrix) == 2) {                                           ### eindimensionaler Fall
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = colnames(qMatrix)[2], var2 = NA , type = "distrpar", indicator.group = NA, group = "persons", par = "var",  derived.par = NA, value = shw$cov.structure, stringsAsFactors = FALSE))
             }  else  {                                                         ### mehrdimensional
                stopifnot(nrow(shw$cov.structure) == ncol(qMatrix))             ### (Residual-)Varianzen und (Residual-)Korrelationen der lat. Dimensionen
                shw$cov.structure[-nrow(shw$cov.structure),1] <- colnames(qMatrix)[-1]
                cov1 <- shw$cov.structure[,-1]
                cov1[upper.tri(shw$cov.structure[,-1])] <- NA
                cov1 <- data.frame ( shw$cov.structure[,1,drop=FALSE], cov1, stringsAsFactors = FALSE)
                colnames(cov1)[-1] <- cov1[-nrow(cov1),1]
                cov2 <- facToChar( dataFrame = reshape2::melt(cov1[-nrow(cov1),], id.vars = colnames(cov1)[1], na.rm=TRUE))
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = c(colnames(qMatrix)[-1], cov2[,1]), var2 = c(rep(NA, ncol(qMatrix)-1), cov2[,2]) , type = "random", indicator.group = NA, group = "persons", par = c(rep("var",ncol(qMatrix)-1), rep("correlation", nrow(cov2))) ,  derived.par = NA, value = unlist(c(cov1[nrow(cov1),-1], cov2[,3])) , stringsAsFactors = FALSE))
             }
    ### Sektion 'Regressionsparameter auslesen' (shw)
             if(nrow(shw$regression)>1) {
                reg  <- shw$regression                                          ### untere Zeile: Dimensionen analog zu Q matrix umbenennen
                if(!is.null( dim(shw$cov.structure) )) {
                   for ( i in 1:nrow(altN)) { colnames(reg) <- gsub(altN[i,"from"], altN[i,"to"], colnames(reg))}
                }  else  {
                   index  <- grep("_$", colnames(reg))
                   colnames(reg)[index] <- paste(colnames(reg)[index], altN[,"to"], sep="")
                }
                regL <- reshape2::melt(reg, id.vars = colnames(reg)[1], measure.vars = colnames(reg)[-c(1, ncol(reg))], na.rm=TRUE)
                foo  <- data.frame ( do.call("rbind", strsplit(as.character(regL[,"variable"]), "_")), stringsAsFactors = FALSE)
                colnames(foo) <- c("par", "group")
                foo[,"derived.par"] <- car::recode(foo[,"par"], "'error'='se'; else = NA")
                foo[,"par"] <- "est"
                regL <- data.frame ( regL[,-match("variable", colnames(regL)), drop=FALSE], foo, stringsAsFactors = FALSE)
                regL[,"reg.var"] <- car::recode(regL[,"reg.var"], "'CONSTANT'='(Intercept)'")
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = regL[,"reg.var"], var2 = NA , type = "regcoef", indicator.group = NA, group = regL[,"group"], par = regL[,"par"],  derived.par = regL[,"derived.par"], value = regL[,"value"] , stringsAsFactors = FALSE))
             }
    ### Sektion 'Modellindizes auslesen' (shw)
             ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = NA, var2 = NA , type = "model", indicator.group = NA, group = NA, par = c("deviance", "Npar"),  derived.par = NA, value = shw$final.deviance , stringsAsFactors = FALSE))
         }                                                                      ### schliesst die Bedingung 'shw file vorhanden'
    ### Sektion 'Personenparameter auslesen' (wle)
         wleFile  <- paste(analysis.name, "wle", sep=".")
         if (!wleFile %in% allFiles) {
             cat("Cannot find Conquest WLE file.\n")
         } else {
             wle  <- get.wle( file.path(path, wleFile) )
             for ( i in 1:nrow(altN)) { colnames(wle) <- gsub(  paste(".",altN[i,"nr"],"$",sep=""), paste("_", altN[i,"to"],sep="") , colnames(wle))}
             wleL <- reshape2::melt(wle, id.vars = "ID", measure.vars = colnames(wle)[-c(1:2)], na.rm=TRUE)
             foo  <- data.frame ( do.call("rbind", strsplit(as.character(wleL[,"variable"]), "_")), stringsAsFactors = FALSE)
             colnames(foo) <- c("par", "group")
             foo[,"derived.par"] <- car::recode(foo[,"par"], "'wle'='est'; 'std.wle'='se'; else=NA")
             foo[,"par"]         <- car::recode(foo[,"par"], "'wle'='wle'; 'std.wle'='wle'; 'n.solved'='NitemsSolved'; 'n.total'='NitemsTotal'")
             wleL <- data.frame ( wleL[,-match("variable", colnames(wleL)), drop=FALSE], foo, stringsAsFactors = FALSE)
             ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = wleL[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = wleL[,"group"], par = wleL[,"par"],  derived.par = wleL[,"derived.par"], value = wleL[,"value"] , stringsAsFactors = FALSE))
         }
    ### Sektion 'Personenparameter auslesen' (PVs)
         pvFile   <- paste(analysis.name, "pvl", sep=".")
         if (!pvFile %in% allFiles) {
             cat("Cannot find Conquest PV file.\n")
         } else {
             pv   <- get.plausible( file.path(path, pvFile), forConquestResults = TRUE )
             rec  <- paste("'",altN[,"pv"] , "' = '" , altN[,"to"], "'" ,sep = "", collapse="; ")
             pv$pvLong[,"variable"] <- car::recode( pv$pvLong[,"variable"], rec)
             ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = pv$pvLong[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = pv$pvLong[,"variable"], par = "pv",  derived.par = paste("pv", as.numeric(pv$pvLong[,"PV.Nr"]),sep=""), value = as.numeric(pv$pvLong[,"value"]) , stringsAsFactors = FALSE))
             eaps <- reshape2::melt ( data.frame ( pv$pvWide[,"ID", drop=FALSE], pv$eap, stringsAsFactors = FALSE), id.vars = "ID", na.rm=TRUE)
             foo  <- data.frame ( do.call("rbind", strsplit(as.character(eaps[,"variable"]), "_")), stringsAsFactors = FALSE)
             colnames(foo) <- c("par", "group")
             foo[,"derived.par"] <- car::recode(foo[,"par"], "'eap'='est'; 'se.eap'='se'; else=NA")
             foo[,"par"]         <- "eap"
             foo[,"group"]       <- car::recode(tolower(foo[,"group"]), rec)
             ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = eaps[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = foo[,"group"], par = "eap",  derived.par = foo[,"derived.par"], value = eaps[,"value"] , stringsAsFactors = FALSE))
         }
         return(ret)}


table.unlist <- function(dataFrame, verbose = TRUE, useNA = c("no","ifany", "always"))   {
                useNA  <- match.arg(useNA)
                if(!exists("rbind.fill.matrix"))  {library(plyr)}
                # if(class(dataFrame) != "data.frame" ) {stop("Argument of 'table.unlist' has to be of class 'data.frame'.\n")}
                if(class(dataFrame) != "data.frame" ) {
                   if(verbose == TRUE ) {cat(paste("Warning! Argument of 'table.unlist' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep=""))}
                   dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
                }
                column.by.column   <- do.call("rbind.fill.matrix", lapply(dataFrame, FUN=function(ii) {
                                      tab        <- table(ii, useNA = useNA)
                                      names(tab) <- car::recode(names(tab), "NA='NA'")
                                      return(t(tab))}))
                freq.table         <- colSums(column.by.column,na.rm=TRUE)
                return(freq.table)}

as.numeric.if.possible <- function(dataFrame, set.numeric=TRUE, transform.factors=FALSE, maintain.factor.scores = TRUE, verbose=TRUE)   {
            originWarnLevel <- getOption("warn")
            wasInputVector  <- FALSE
            if( !"data.frame" %in% class(dataFrame) ) {
              if(verbose == TRUE )  {cat(paste("Warning! Argument of 'as.numeric.if.possible' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep="")) }
              dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
              wasInputVector <- ifelse(ncol(dataFrame) == 1, TRUE, FALSE)
            }
            currentClasses <- sapply(dataFrame, FUN=function(ii) {class(ii)})
            summaryCurrentClasses <- names(table(currentClasses))
            if(verbose == TRUE )  {
               cat(paste("Current data frame consists of following ",length(summaryCurrentClasses), " classe(s):\n    ",sep=""))
               cat(paste(summaryCurrentClasses,collapse=", ")); cat("\n")
            }
            options(warn = -1)                                                  ### zuvor: schalte Warnungen aus!
            numericable <- sapply(dataFrame, FUN=function(ii)   {
                  n.na.old       <- sum(is.na(ii))
                  transformed    <- as.numeric(ii)
                  transformed.factor <- as.numeric(as.character(ii))
                  n.na.new       <- sum(is.na(transformed))
                  n.na.new.factor <- sum(is.na(transformed.factor))
                  ret            <- rbind(ifelse(n.na.old == n.na.new, TRUE, FALSE),ifelse(n.na.old == n.na.new.factor, TRUE, FALSE))
                  if(transform.factors == FALSE)   {
                     if(class(ii) == "factor")   {
                        ret <- rbind(FALSE,FALSE)
                     }
                  }
                  return(ret)})
            options(warn = originWarnLevel)                                     ### danach: schalte Warnungen wieder in Ausgangszustand!
            changeVariables <- colnames(dataFrame)[numericable[1,]]
            changeFactorWithIndices   <- NULL
            if(transform.factors == TRUE & maintain.factor.scores == TRUE)   {
               changeFactorWithIndices   <- names(which(sapply(changeVariables,FUN=function(ii) {class(dataFrame[[ii]])=="factor"})))
               changeFactorWithIndices   <- setdiff(changeFactorWithIndices, names(which(numericable[2,] == FALSE)) )
               changeVariables           <- setdiff(changeVariables, changeFactorWithIndices)
            }
            if(length(changeVariables) >0)   {                                  ### hier werden alle Variablen (auch Faktoren, wenn maintain.factor.scores = FALSE) ggf. geändert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dataFrame$'" , ii , "' <- as.numeric(dataFrame$'",ii, "'), silent=TRUE)" , sep = "" ) } , changeVariables  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(length(changeFactorWithIndices) >0)   {                          ### hier werden ausschließlich FAKTOREN, wenn maintain.factor.scores = TRUE, ggf. geändert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dataFrame$'" , ii , "' <- as.numeric(as.character(dataFrame$'",ii, "')), silent=TRUE)" , sep = "" ) } , changeFactorWithIndices  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(set.numeric==FALSE) {return(numericable[1,])}
            if(set.numeric==TRUE)  {
              if(verbose == TRUE)      {
                 if( sum ( numericable[1,] == FALSE ) > 0 )  {
                     cat(paste("Following ",sum ( numericable[1,] == FALSE )," variable(s) won't be transformed:\n    ",sep=""))
                     cat(paste(colnames(dataFrame)[as.numeric(which(numericable[1,] == FALSE))],collapse= ", ")); cat("\n")
                 }
              }
              if(wasInputVector == TRUE) {dataFrame <- unname(unlist(dataFrame))}
              return(dataFrame)
           }
         }
