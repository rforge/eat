getResults <- function ( runModelObj, overwrite = FALSE, abs.dif.bound = 0.6, sig.dif.bound = 0.3, p.value = 0.9, simplify = TRUE ) { 
            if( "runMultiple" %in% class(runModelObj)) {                        ### Mehrmodellfall
               res <- lapply( runModelObj, FUN = function ( r ) { 
                      ret <- getResults (runModelObj = r, overwrite = overwrite, abs.dif.bound = abs.dif.bound, sig.dif.bound = sig.dif.bound, p.value = p.value, simplify = simplify ) 
                      att <- list ( list ( model.name = ret[1,"model"], all.Names = attr(ret, "all.Names"), dif.settings = attr(ret, "dif.settings") ))
                      stopifnot ( length(unique(ret[1,"model"])) == 1 )         ### grosser scheiss: baue Hilfsobjekt fuer Attribute (intern notwendige Zusatzinformationen) separat zusammen
                      return(list ( ret = ret, att = att))})                    ### schlimmer Code, darf nie jemand sehen!!
               att <- lapply(res, FUN = function ( yy ) { return ( yy[["att"]] ) } )
               res <- do.call("rbind", lapply ( res, FUN = function ( yy ) { return ( yy[["ret"]] ) } ) ) 
               attr(res, "att") <- att
               class(res) <- c("data.frame", "multipleResults")
               return(res)
            }  else {                                                           ### Einmodellfall 
               isTa  <- FALSE                                                   
               if( "runConquest" %in% class(runModelObj) ) {                    ### wurde mit Conquest gerechnet?
                    res <- getConquestResults (path = runModelObj$dir, analysis.name=runModelObj$analysis.name, model.name=runModelObj$model.name, qMatrix=runModelObj$qMatrix, all.Names = runModelObj$all.Names, abs.dif.bound = abs.dif.bound, sig.dif.bound = sig.dif.bound, p.value = p.value, simplify = simplify, deskRes = runModelObj$deskRes, discrim = runModelObj$discrim)
                    dir <- runModelObj[["dir"]]
                    name<- runModelObj[["analysis.name"]]
                    attr(res, "all.Names") <- runModelObj[["all.Names"]]
               }  else  {                                                       ### Alternativ: es wurde mit TAM gerechnet 
                    isTa<- TRUE                                                 ### logisches Argument: wurde mit Tam gerechnet?
                    res <- getTamResults (runModelObj=runModelObj)
                    dir <- attr(runModelObj, "dir")
                    name<- attr(runModelObj, "analysis.name")
                    attr(res, "all.Names") <- attr(runModelObj, "all.Names")
               }
               attr(res, "dif.settings")   <- list (abs.dif.bound = abs.dif.bound, sig.dif.bound = sig.dif.bound, p.value = p.value, simplify = simplify) 
               if(!is.null(dir)) { 
                  if(isTa == TRUE)          { save(runModelObj, file = file.path(dir, paste(name, "_tamObject.rda",sep="")))} 
                  if(isTa == FALSE ) { 
                      if ( attr(res, "available")[["itn"]] == TRUE | attr(res, "available")[["itn"]] == TRUE ) { 
                          item<- itemFromRes ( res )                            ### obere Zeile: wenn mit TAM gerechnet wurde, tam Objekt speichern 
                          if ( file.exists(file.path(dir, paste(name, "_items.csv",sep=""))) & overwrite == FALSE) { 
                               cat(paste("Item results cannot be saved, file ",  file.path(dir, paste(name, "_items.csv",sep=""))," already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                          }  else  { 
                               write.csv2(item, file.path(dir, paste(name, "_items.csv",sep="")), na="", row.names = FALSE)
                          }    
                      }
                  }        
                  if (isTa == FALSE ) { 
                      if ( attr(res, "available")[["wle"]] == TRUE & attr(res, "available")[["pv"]] == TRUE) { 
                           wle <- wleFromRes(res)                                        ### untere Zeilen: speichere wunschgemaess alle Personenparameter in einer Tabelle im Wideformat
                           wleL<- melt ( wle, id.vars = c("ID", "dimension"), measure.vars = c("wle_est", "wle_se"), na.rm = TRUE)
                           wleW<- dcast ( wleL, ID~dimension+variable, value.var = "value" )
                           pv  <- pvFromRes(res)
                           pvL <- melt ( pv, id.vars = c("ID", "dimension"), na.rm = TRUE)
                           pvW <- dcast ( pvL, ID~dimension+variable, value.var = "value" )
                           eap <- eapFromRes(res)
                           eapL<- melt ( eap, id.vars = c("ID", "dimension"), measure.vars = c("EAP", "SE.EAP"), na.rm = TRUE)
                           eapW<- dcast ( eapL, ID~dimension+variable, value.var = "value" )
                           allP<- merge(eapW, pvW, by = "ID", all=TRUE)
                           allP<- merge(allP, wleW, by = "ID", all=TRUE)
                           if ( file.exists(file.path(dir, paste(name, "_persons.csv",sep=""))) & overwrite == FALSE) { 
                                cat(paste("Person estimates cannot be saved, file ",  file.path(dir, paste(name, "_persons.csv",sep=""))," already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                           }  else  { 
                                write.csv2(allP, file.path(dir, paste(name, "_persons.csv",sep="")), na="", row.names = FALSE)
                           }    
                      }
                  }
                  if ( file.exists(file.path(dir, paste(name, "_resultsAll.csv",sep=""))) & overwrite == FALSE) { 
                       cat(paste("Results cannot be saved, file ",  file.path(dir, paste(name, "_resultsAll.csv",sep=""))," already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                  }  else  { 
                       write.csv2(res, file.path(dir, paste(name, "_resultsAll.csv",sep="")), na="", row.names = FALSE)
                  }    
               }   
               return(res)
               }}   

runModel <- function(defineModelObj, show.output.on.console = FALSE, show.dos.console = TRUE, wait = TRUE) {
            if ("defineMultiple" %in% class( defineModelObj ) ) {               ### erstmal fuer den Multimodellfall
                res <- lapply(defineModelObj, FUN = function ( r ) { 
                       ret <- runModel ( defineModelObj = r, show.output.on.console = show.output.on.console, show.dos.console = show.dos.console, wait = wait)
                       return(ret)})
                class(res) <- c("runMultiple", "list")
                return(res)
            } else {    
                if("defineConquest" %in% class(defineModelObj)) {               ### ab hier fuer den single model Fall 
                   oldPfad <- getwd()
                   setwd(defineModelObj$dir)
                   options(warn = -1)
                   system(paste(defineModelObj$conquest.folder," ",defineModelObj$input,sep=""),invisible=!show.dos.console,show.output.on.console=show.output.on.console, wait=wait) 
                   options(warn = 0)
                   setwd(oldPfad)                                                   ### untere Zeile: Rueckgabeobjekt definieren: Conquest
                   class(defineModelObj) <- c("runConquest", "list")
                   return ( defineModelObj )
                }
                if("defineTam" %in% class(defineModelObj)) {                        ### exportiere alle Objekte aus defineModelObj in environment 
                   for ( i in names( defineModelObj )) { assign(i, defineModelObj[[i]]) } 
                   if ( show.output.on.console == TRUE ) { control$progress <- TRUE } 
#                   if(!exists("tam.mml"))       {library(TAM, quietly = TRUE)}      ### March, 2, 2013: fuer's erste ohne DIF, ohne polytome Items, ohne mehrgruppenanalyse, ohne 2PL
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
                          mod     <- tam.mml(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = control)
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
                              mod  <- tam.mml.3pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, pweights = wgt, est.guess =gues, control = control)
                          }  else { mod     <- tam.mml.2pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, est.slopegroups=est.slopegroups,pweights = wgt, control = control) }
                      }
                   } else {
                     assign(paste("DIF_",all.Names[["DIF.var"]],sep="") , as.data.frame (daten[,all.Names[["DIF.var"]]]) )
                     formel   <- as.formula(paste("~item - ",paste("DIF_",all.Names[["DIF.var"]],sep="")," + item * ",paste("DIF_",all.Names[["DIF.var"]],sep=""),sep=""))
                     facetten <- as.data.frame (daten[,all.Names[["DIF.var"]]])
                     colnames(facetten) <- paste("DIF_",all.Names[["DIF.var"]],sep="")
                     mod      <- tam.mml.mfr(resp = daten[,all.Names[["variablen"]]], facets = facetten, formulaA = formel, pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = control)
                   }
                   attr(mod, "qMatrix")      <- defineModelObj[["qMatrix"]]     ### hier werden fuer 'tam' zusaetzliche Objekte als Attribute an das Rueckgabeobjekt angehangen
                   attr(mod, "n.plausible")  <- defineModelObj[["n.plausible"]] ### Grund: Rueckgabeobjekt soll weitgehend beibehalten werden, damit alle 'tam'-Funktionen, die darauf aufsetzen, lauffaehig sind
                   attr(mod, "dir")          <- defineModelObj[["dir"]] 
                   attr(mod, "analysis.name")<- defineModelObj[["analysis.name"]] 
                   attr(mod, "all.Names")    <- defineModelObj[["all.Names"]] 
                   attr(mod, "deskRes")      <- defineModelObj[["deskRes"]] 
                   attr(mod, "discrim")      <- defineModelObj[["discrim"]] 
                   attr(mod, "irtmodel")     <- defineModelObj[["irtmodel"]] 
                   return(mod)  }  }   }


defineModel <- function(dat, items, id, splittedModels = NULL, irtmodel = c("1PL", "2PL", "PCM", "PCM2", "RSM", "GPCM", "2PL.groups", "GPCM.design", "3PL"),
               qMatrix=NULL, DIF.var=NULL, HG.var=NULL, group.var=NULL, weight.var=NULL, anchor = NULL, check.for.linking = TRUE, boundary = 6, 
               remove.boundary = FALSE, remove.no.answers = TRUE, remove.missing.items = TRUE, remove.constant.items = TRUE, remove.failures = FALSE, 
               remove.vars.DIF.missing = TRUE, remove.vars.DIF.constant = TRUE, verbose=TRUE, software = c("conquest","lme4", "tam"), dir = NULL, 
               analysis.name, withDescriptives = TRUE, model.statement = "item",  compute.fit = TRUE, n.plausible=5, seed = NULL, conquest.folder=NULL,
               constraints=c("cases","none","items"),std.err=c("quick","full","none"), distribution=c("normal","discrete"), 
               method=c("gauss", "quadrature", "montecarlo"), n.iterations=2000,nodes=NULL, p.nodes=2000, f.nodes=2000,converge=0.001,deviancechange=0.0001, 
               equivalence.table=c("wle","mle","NULL"), use.letters=FALSE, allowAllScoresEverywhere = TRUE, guessMat = NULL, est.slopegroups = NULL, 
               progress = FALSE, increment.factor=1 , fac.oldxsi=0, export = list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE) )   {
                  checkForPackage (namePackage = "eatRest", targetPackage = "eatModel")
                  if(!"data.frame" %in% class(dat) ) { cat("Convert 'dat' to a data.frame.\n"); dat <- data.frame ( dat, stringsAsFactors = FALSE)}
     ### Sektion 'multiple models handling': jedes Modell einzeln von 'defineModel' aufbereiten lassen 
     ### Hier wird jetzt erstmal nur die bescheuerte Liste aus 'splitModels' aufbereitet (wenn der Nutzer sie verhunzt hat)
                  if(!is.null(splittedModels)) {
                     if(length(splittedModels) == 2 & !is.null(splittedModels[[1]]) &  length(nrow( splittedModels[[1]]) > 0)>0 ) { 
                        if(nrow(splittedModels[[1]])>0) { 
                           mods   <- intersect(splittedModels[["models"]][,"model.no"], unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]})))
                        }  else  { 
                           mods <- unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]})) 
                        }
                     }  else  { 
                        mods <- unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]})) 
                     }
                     if(length(mods) == 0) { stop("Inconsistent model specification in 'splittedModels'.\n") } else { if(verbose == TRUE) { cat(paste("\nSpecification of 'qMatrix' and 'person.groups' results in ",length(mods)," model(s).\n",sep="")) } }
     ### Jetzt wird die aufbereitete Liste aus 'splitModels' abgearbeitet 
     ### ACHTUNG: Argumente in 'splittedModels' ueberschreiben default- und vom Nutzer gesetzte Argumente in 'defineModel'!
                     models <- lapply( mods, FUN = function ( m ) { 
                               matchL <- match(m, unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) { l[["model.no"]] } )))
                               if(!is.null(splittedModels[["models.splitted"]][[matchL]][["qMatrix"]])) {
                                  itemMis<- setdiff ( splittedModels[["models.splitted"]][[matchL]][["qMatrix"]][,1], colnames(dat))
                                  if( length ( itemMis ) > 0) {
                                      cat(paste( "Warning: ",length(itemMis) ," from ",nrow(splittedModels[["models.splitted"]][[matchL]][["qMatrix"]])," items not found in data.\n",sep=""))
                                  }
                                  itemSel<- intersect ( splittedModels[["models.splitted"]][[matchL]][["qMatrix"]][,1], colnames(dat))
                                  qMatrix<- splittedModels[["models.splitted"]][[matchL]][["qMatrix"]]
                               }  else  {
                                  if(is.null(items)) { stop(paste("Model no. ",m," ('",splittedModels[["models.splitted"]][[matchL]][["model.name"]],"'): no items defined.\n",sep=""))}
                                  itemSel<- items
                               }
     ### Personen im Datensatz selektieren: Achtung: wenn keine Personen in "person.grouping", nimm alle!
                               if(!is.null(splittedModels[["models.splitted"]][[matchL]][["person.grouping"]])) {
                                  persMis<- setdiff ( splittedModels[["models.splitted"]][[matchL]][["person.grouping"]][,1], dat[,id])
                                  if( length ( persMis ) > 0) {
                                      cat(paste( "Warning: ",length(persMis) ," from ",nrow(splittedModels[["models.splitted"]][[matchL]][["person.grouping"]])," persons not found in data.\n",sep=""))
                                  }
                                  persons<- intersect ( splittedModels[["models.splitted"]][[matchL]][["person.grouping"]][,1], dat[,id])
                                  datSel <- dat[match(persons, dat[,id]),]
                               }  else  { datSel <- dat }
     ### Unterverzeichnisse definieren
                               if(is.null(dir)) { dirI <- NULL }  else  { dirI   <- file.path(dir, substring(splittedModels[["models.splitted"]][[matchL]][["model.subpath"]],3)) }
                               nameI  <- splittedModels[["models.splitted"]][[matchL]][["model.name"]]
                               if(is.null (qMatrix)) { nDim <- 1 } else { nDim <- ncol(qMatrix)-1 }
     ### Aufruf von 'defineModel' generieren
                               overwr1<- data.frame ( arg = c("dat", "items", "qMatrix", "analysis.name", "dir", "splittedModels"), val = c("datSel", "itemSel", "qMatrix", "nameI", "dirI", "NULL"), stringsAsFactors = FALSE)
                               notNull<- which ( unlist(lapply(splittedModels[["models.splitted"]][[matchL]], is.null)) == FALSE ) 
                               overwr2<- setdiff ( intersect ( names(formals(defineModel)), names ( notNull)), "qMatrix")
                               if(length(overwr2)>0) {                          ### obere Zeile: qMatrix ist oben schon definiert, wird hier NICHT aus der Liste durchgeschleift 
                                  overwr3 <- data.frame ( arg = overwr2, val = paste("splittedModels[[\"models.splitted\"]][[",matchL,"]][[\"",overwr2,"\"]]",sep=""), stringsAsFactors = FALSE)
                                  overwr1 <- rbind ( overwr1, overwr3)
                                  overwr3[,"eval"] <- unlist(lapply(paste(overwr3[,"val"],sep=""), FUN = function ( l ) { eval(parse(text = l))}))
                               }  else  { overwr3 <- NULL }  
                               default<- setdiff ( names(formals(defineModel)), overwr1[,"arg"])
                               overwr1<- rbind ( overwr1, data.frame ( arg = default, val = default, stringsAsFactors = FALSE) )
                               toCall <- paste( overwr1[,"arg"], overwr1[,"val"], sep=" = ", collapse=", ")
                               toCall <- paste("defineModel ( ", toCall, ")", sep="")
     ### sprechende Ausgaben, wenn verbose == TRUE
                               overwr3<- rbind.fill ( data.frame ( arg = c("Model name", "Number of items", "Number of persons", "Number of dimensions"), eval = c(splittedModels[["models.splitted"]][[matchL]][["model.name"]],length(itemSel), nrow(datSel) , nDim), stringsAsFactors = FALSE)  , overwr3)
                               overwr3["leerz"] <- max (nchar(overwr3[,"arg"])) - nchar(overwr3[,"arg"]) + 1
                               txt    <- apply(overwr3, MARGIN = 1, FUN = function ( j ) { paste("\n    ", j[["arg"]], ":", paste(rep(" ", times = j[["leerz"]]), sep="", collapse=""), j[["eval"]], sep="")})
                               nDots  <- max(nchar(overwr3[,"arg"])) + max(nchar(overwr3[,"eval"])) + 6
                               if(verbose == TRUE ) {
                                  cat(paste("\n\n",paste(rep("=",times = nDots), sep="", collapse=""),"\nModel No. ",m, paste(txt,sep="", collapse=""), "\n",paste(rep("=",times = nDots), sep="", collapse=""),"\n\n", sep=""))
                               }   
                               ret    <- eval(parse(text=toCall))
                               return(ret)                                                             
                               })
                  class(models) <- c("defineMultiple", "list")
                  return(models)                                                ### Das ist die Rueckgabe fuer den Mehrmodellfall
                  }  else  { 
     ### ACHTUNG: hier beginnt jetzt der 'single model Fall' von 'defineModel' ### 
                     irtmodel <- match.arg(irtmodel)
                     software <- match.arg(software)
                     method   <- match.arg(method)
                     if(software == "conquest") {
                        original.options <- options("scipen")                   ### lese Option für Anzahl der Nachkommastellen
                        options(scipen = 20)                                    ### setze Option für Anzahl der Nachkommastellen
                        if(missing(analysis.name)) {stop("Please specify 'analysis.name' or use 'software = \"tam\"'\n")} 
                     }  else  { 
                        if(missing(analysis.name)) {analysis.name <- NULL} 
                     }   
                     if(length(model.statement)!=1)            {stop("'model.statement' has to be of length 1.\n")}
                     if(class(model.statement)!="character")   {stop("'model.statement' has to be of class 'character'.\n")}
                     if(missing(dat))   {stop("No dataset specified.\n") }      ### 11.04.2014: nutzt Hilfsfunktionen von jk2.mean etc.
                     if(is.null(items)) {stop("Argument 'items' must not be NULL.\n",sep="")}
                     allVars     <- list(ID = id, variablen=items, DIF.var=DIF.var, HG.var=HG.var, group.var=group.var, weight.var=weight.var)
                     all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                     doppelt     <- which(duplicated(dat[,all.Names[["ID"]]]))
                     if(length(doppelt)>0)  {stop(paste( length(doppelt) , " duplicate IDs found!",sep=""))}
                     if(!is.null(dir)) {                                        ### Sofern ein verzeichnis angegeben wurde (nicht NULL), 
                        dir         <- crop(dir,"/")                            ### das Verzeichnis aber nicht existiert, wird es jetzt erzeugt
                        if(dir.exists(dir) == FALSE) { 
                           cat(paste("Warning: Specified folder '",dir,"' does not exist. Create folder ... \n",sep="")); flush.console()
                           dir.create(dir, recursive = TRUE)
                        }
                     }      
     ### pruefen, ob es Personen gibt, die weniger als <boundary> items gesehen haben (muss VOR den Konsistenzpruefungen geschehen)
                     datL.valid  <- melt(dat, id.vars = all.Names[["ID"]], meaure.vars = all.Names[["variablen"]], na.rm=TRUE)
                     nValid      <- table(datL.valid[,all.Names[["ID"]]])
                     inval       <- nValid[which(nValid<boundary)]
                     if(length(inval)>0) { 
                        cat(paste( length(inval), " subject(s) with less than ",boundary," valid item responses: ", paste(names(inval),inval,sep=": ", collapse="; "),"\n",sep=""))
                        if(remove.boundary==TRUE) { 
                           cat(paste("subjects with less than ",boundary," valid responses will be removed.\n    Caution! This can result in loosing some items likewise.\n",sep="") )
                           weg <- match(names(inval), dat[,all.Names[["ID"]]])
                           stopifnot(length(which(is.na(weg))) == 0 ) ; flush.console()
                           dat <- dat[-weg,]
                        }
                     }                    
     ### Sektion 'explizite Variablennamen ggf. aendern' ###
                     subsNam <- .substituteSigns(dat=dat, variable=unlist(all.Names[-c(1:2)]))
                     if(software == "conquest") {                               ### Conquest erlaubt keine gross geschriebenen und expliziten Variablennamen, die ein "." oder "_" enthalten
                        if(!all(subsNam$old == subsNam$new)) {
                           sn     <- subsNam[which( subsNam$old != subsNam$new),]
                           cat("Conquest neither allows '.', '-' and '_' nor upper case letters in explicit variable names. Delete signs from variables names for explicit variables.\n"); flush.console()
                           recStr <- paste("'",sn[,"old"] , "' = '" , sn[,"new"], "'" ,sep = "", collapse="; ")
                           colnames(dat) <- recode(colnames(dat), recStr)
                           all.Names     <- lapply(all.Names, FUN = function ( y ) { recode(y, recStr) })
                           if(model.statement != "item") {
                              cat("    Remove deleted signs from variables names for explicit variables also in the model statement. Please check afterwards for consistency!\n")
                              for ( uu in 1:nrow(sn))  {model.statement <- gsub(sn[uu,"old"], sn[uu,"new"], model.statement)}
                           }
                        }
                        if("item" %in% unlist(all.Names[-c(1:2)])) { stop("Conquest does not allow labelling explicit variable(s) with 'Item' or 'item'.\n") }
                     }                                                          ### untere Zeilen: Dif-Variablen und Testitems dürfen sich nicht überschneiden
                     if(length(intersect(all.Names$DIF.var, all.Names$variablen))>0)    {stop("Test items and DIF variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$weight.var, all.Names$variablen))>0) {stop("Test items and weighting variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$HG.var, all.Names$variablen))>0)     {stop("Test items and HG variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$group.var, all.Names$variablen))>0)  {stop("Test items and group variable have to be mutually exclusive.\n")}
     ### Sektion 'Q matrix ggf. erstellen und auf Konsistenz zu sich selbst und zu den Daten pruefen' ###
                     if(is.null(qMatrix)) { qMatrix <- data.frame ( Item = all.Names$variablen, Dim1 = 1, stringsAsFactors = FALSE) } else {
                         qMatrix <- checkQmatrixConsistency(qMatrix)            ### pruefe Konsistenz der q-matrix
                         notInDat<- setdiff(qMatrix[,1], all.Names$variablen)
                         notInQ  <- setdiff( all.Names$variablen , qMatrix[,1])
                         if(length(notInDat)>0) {
                            cat(paste("Following ", length(notInDat)," item(s) missed in data frame will removed from Q matrix: \n    ",paste(notInDat,collapse=", "),"\n",sep=""))
                            qMatrix <- qMatrix[-match(notInDat, qMatrix[,1]),]
                            if(nrow(qMatrix) == 0) { stop("No common items in Q matrix and data.\n")}
                         }
                         if(length(notInQ)>0) {
                            cat(paste("Following ", length(notInQ)," item(s) missed in Q matrix will removed from data: \n    ",paste(notInQ,collapse=", "),"\n",sep=""))
                         }
                         all.Names$variablen <- qMatrix[,1]  } ;   flush.console()# Wichtig! Sicherstellen, dass Reihenfolge der Items in Q-Matrix mit Reihenfolge der Items im Data.frame uebereinstimmt!
     ### Sektion 'Alle Items auf einfache Konsistenz pruefen' ###
                      namen.items.weg <- NULL
                      is.NaN <- do.call("cbind", lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function (uu) { is.nan(uu) } ) )
                      if(sum(is.NaN) > 0 ) {                                    ### Wandle NaN in NA, falls es welche gibt
                         cat(paste("Found ",sum(is.NaN)," 'NaN' values in the data. Convert 'NaN' to 'NA'.\n",sep=""))
                         for ( j in all.Names[["variablen"]]) { 
                               weg <- which ( is.nan(dat[,j] ))
                               if(length(weg)>0) {  dat[weg,j] <- NA }
                         }
                      }         
                      n.werte <- table.unlist(dat[,all.Names[["variablen"]], drop = FALSE]) 
                      zahl    <- grep("[[:digit:]]", names(n.werte))            ### sind das alles Ziffern? (auch wenn die Spalten als "character" klassifiziert sind
                      noZahl  <- setdiff(1:length(n.werte), zahl)
                      if (length( zahl ) == 0 )  { stop("Please use numeric values for item responses.\n")}
                      if (length( noZahl ) > 0 ) { 
                          cat(paste(" W A R N I N G !  Found ",sum(n.werte[noZahl])," non-numeric values in the item responses. These values will be treated as missing responses!\n",sep="")) }
                      klasse  <- unlist( lapply(dat[,all.Names[["variablen"]], drop = FALSE], class) ) 
                      if( "character" %in% klasse | "factor" %in% klasse | "logical" %in% klasse ) { 
                          cat(paste(" W A R N I N G !  Found unexpected class type(s) in item response columns: ",paste(setdiff(klasse, c("numeric", "integer")), collapse = ", "), "\n",sep=""))
                          cat("                  All item columns will be transformed to be 'numeric'. Recommend to edit your data manually prior to analysis.\n")
                          for ( uu in all.Names[["variablen"]] ) { dat[,uu] <- as.numeric(dat[,uu])}
                      }    
                      values  <- lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function ( ii ) { table(ii)})
                      isDichot<- unlist(lapply(values, FUN = function ( vv ) { identical(c("0","1"), names(vv)) }))
                      n.werte <- sapply(values, FUN=function(ii) {length(ii)})
                      n.mis   <- which(n.werte == 0)
                      if(length(n.mis) >0) {                                    ### identifiziere Items ohne jegliche gueltige Werte
                         cat(paste("Serious warning: ",length(n.mis)," testitems(s) without any values.\n",sep=""))
                         if(verbose == TRUE) {cat(paste("    ", paste(names(n.mis), collapse=", "), "\n", sep=""))}
                         if(remove.missing.items == TRUE) {
                         cat(paste("Remove ",length(n.mis)," variable(s) due to solely missing values.\n",sep=""))
                         namen.items.weg <- c(namen.items.weg, names(n.mis))}
                      }   
                      constant <- which(n.werte == 1)
                      if(length(constant) >0) {                                 ### identifiziere konstante Items (Items ohne Varianz)
                         cat(paste("Warning: ",length(constant)," testitems(s) are constants.\n",sep=""))
                         if(verbose == TRUE) {foo <- lapply(names(constant),FUN=function(ii) {cat(paste(ii,": ",names(table(dat[,ii])),sep="")); cat("\n")})}
                         if(remove.constant.items == TRUE) {
                         cat(paste("Remove ",length(constant)," variable(s) due to solely constant values.\n",sep=""))
                         namen.items.weg <- c(namen.items.weg, names(constant))}
                      }                                                         ### identifiziere alle Items, die nicht dichotom (="ND") sind 
                      n.rasch   <- which( !isDichot )                           ### (aber nicht die, die bereits wegen konstanter Werte aussortiert wurden!)
                      if(length(n.rasch) >0 )   {                               ### also polytome Items oder Items, die mit 1/2 anstatt 0/1 kodiert sind
                         valND <- values[ which(names(values) %in% names(n.rasch)) ]
                         valND <- valND[which(sapply(valND, length) > 1)]
                         if(length(valND)>0) { 
                            cat(paste("Warning: ",length(valND)," variable(s) are not strictly dichotomous with 0/1.\n",sep=""))
                            for (ii in 1:length(valND))  {
                                 max.nchar <-  max(nchar(names(table(dat[,names(valND)[ii]]))))
                                 if(max.nchar>1) {
                                    cat(paste("Arity of variable",names(valND)[ii],"exceeds 1.\n"))
                                 }
                                 if(verbose == TRUE) {
                                    cat(paste(names(valND)[ii],": ", paste( names(table(dat[,names(valND)[ii]])),collapse=", "),"\n",sep=""))
                                 }
                            }
                            cat("Expect a rating scale model or partial credit model.\n")
                            if(model.statement == "item") {
                               cat("WARNING: Sure you want to use 'model statement = item' even when items are not dichotomous?\n")
                            } 
                         }
                      }   
     ### Sektion 'Hintergrundvariablen auf Konsistenz zu sich selbst und zu den Itemdaten pruefen'. Ausserdem Stelligkeit (Anzahl der benoetigten character) fuer jede Variable herausfinden ###
                      weg.dif <- NULL; weg.hg <- NULL; weg.weight <- NULL; weg.group <- NULL
                      if(length(all.Names$HG.var)>0)    {
                         hg.info <- lapply(all.Names[["HG.var"]], FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="HG", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE], suppressAbort = TRUE )})
                         for ( i in 1:length(hg.info)) { dat[, hg.info[[i]]$varname ] <- hg.info[[i]]$x }
                         wegVar  <- unlist(lapply(hg.info, FUN = function ( uu ) { uu[["toRemove"]] }))
                         if(length(wegVar)>0) { all.Names[["HG.var"]] <- setdiff ( all.Names[["HG.var"]], wegVar) }
                         weg.hg  <- unique(unlist(lapply(hg.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.hg)>0) {                                 ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                            cat(paste("Remove ",length(weg.hg)," cases with missings on at least one HG variable.\n",sep=""))}
                      }
                      if(length(all.Names$group.var)>0)  {
                         group.info <- lapply(all.Names$group.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="group", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         for ( i in 1:length(group.info)) { dat[, group.info[[i]]$varname ] <- group.info[[i]]$x }
                         weg.group  <- unique(unlist(lapply(group.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.group)>0)                                ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.group)," cases with missings on group variable.\n",sep=""))}
                      }
                      if(length(all.Names$DIF.var)>0)  {
                         dif.info <- lapply(all.Names$DIF.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="DIF", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         if ( remove.vars.DIF.missing == TRUE ) {
                              for ( uu in 1:length(dif.info)) { if (length(dif.info[[uu]]$wegDifMis) >0) { 
                                    cat(paste("Remove item(s) which only have missing values in at least one group of DIF variable '",dif.info[[uu]]$varname,"'.\n", sep=""))
                                    namen.items.weg <- c(namen.items.weg,dif.info[[uu]]$wegDifMis) } }
                         }
                         if ( remove.vars.DIF.constant == TRUE ) {
                              for ( uu in 1:length(dif.info)) { if (length(dif.info[[uu]]$wegDifConst) >0) { 
                                    cat(paste("Remove item(s) which are constant in at least one group of DIF variable '",dif.info[[uu]]$varname,"'.\n",sep=""))
                                    namen.items.weg <- c(namen.items.weg,dif.info[[uu]]$wegDifConst) } }
                         }
                         for ( i in 1:length(dif.info)) { dat[, dif.info[[i]]$varname ] <- dif.info[[i]]$x }
                         weg.dif  <- unique(unlist(lapply(dif.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.dif)>0)                                  ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.dif)," cases with missings on DIF variable.\n",sep=""))}
                      }
                      if(length(all.Names$weight.var)>0)  {
                         if(length(all.Names$weight.var)!=1) {stop("Use only one weight variable.")}
                         weight.info <- lapply(all.Names$weight.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="weight", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         for ( i in 1:length(weight.info)) { dat[, weight.info[[i]]$varname ] <- weight.info[[i]]$x }
                         weg.weight  <- unique(unlist(lapply(weight.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.weight)>0)                               ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.weight)," cases with missings on weight variable.\n",sep=""))}
                      }                                                         ### untere Zeile, Achtung: group- und DIF- bzw. group- und HG-Variablen dürfen sich überschneiden!
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
                      if(inherits(try(datL  <- melt(data = dat, id.vars = unique(unlist(all.Names[-match("variablen", names(all.Names))])), measure.vars = all.Names[["variablen"]], na.rm=TRUE)  ),"try-error"))  {
                         cat("W A R N I N G ! ! !   Error in melting for unknown reasons. Try workaround.\n"); flush.console()
                         allHG <- setdiff(unique(unlist(all.Names[-match("variablen", names(all.Names))])), all.Names[["ID"]] )
                         stopifnot(length(allHG)>0)                             ### dies ist ein Workaround, wenn "melt" fehltschlaegt (Fehler nicht reproduzierbar)
                         datL  <- melt(data = dat, id.vars = all.Names[["ID"]], measure.vars = all.Names[["variablen"]], na.rm=TRUE)
                         datL  <- merge(datL, dat[,unique(unlist(all.Names[-match("variablen", names(all.Names))]))], by = all.Names[["ID"]], all=TRUE)
                      }   
                      weg   <- setdiff(dat[,all.Names[["ID"]]], unique(datL[,all.Names[["ID"]]]))
                      if(length(weg)>0)   {                                     ### identifiziere Faelle mit ausschließlich missings
                         cat(paste("Found ",length(weg)," cases with missings on all items.\n",sep=""))
                         if( remove.no.answers == TRUE)  {cat("Cases with missings on all items will be deleted.\n"); dat <- dat[-match(weg,dat[,all.Names[["ID"]]] ) ,]  }
                         if( remove.no.answers == FALSE) {cat("Cases with missings on all items will be kept.\n")}}
     ### Sektion 'Summenscores fuer Personen pruefen' ###
                      datW  <- dcast(datL, as.formula(paste("variable~",all.Names[["ID"]],sep="")), value.var = "value")
                      means <- colMeans(datW[,-1], na.rm=TRUE)
                      minMea<- mean(sapply(datW[,-1], min, na.rm=TRUE), na.rm=TRUE)
                      maxMea<- mean(sapply(datW[,-1], max, na.rm=TRUE), na.rm=TRUE)
                      allFal<- which ( sapply ( means, FUN = function ( xx ) { isTRUE(all.equal( xx, minMea ))}))
                      if(length(allFal)>0) { 
                         cat(paste( length(allFal), " subject(s) have a sum score of ",minMea," which seems to be the lowest possible test score. Hence, they do not solve any item:\n   ", paste(names(allFal), " (",minMea,"/",nValid[allFal],")",sep="",collapse=", "),"\n",sep=""))
                         if (remove.failures == TRUE)  { 
                             cat("   Remove subjects without any correct response.\n"); flush.console()
                             weg <- na.omit(match(names(allFal), dat[,all.Names[["ID"]]]))
                             dat <- dat[-weg,] } 
                      }
                      allTru<- which ( sapply ( means, FUN = function ( xx ) { isTRUE(all.equal( xx, maxMea ))}))
                      if(length(allTru)>0) { cat(paste( length(allTru), " subject(s) solved each item: ", paste(names(allTru), " (",nValid[allTru],"/",nValid[allTru],")",sep="", collapse=", "),"\n",sep=""))}
     ### Sektion 'Verlinkung pruefen' ###
                      if(check.for.linking == TRUE) {                           ### Dies geschieht auf dem nutzerspezifisch reduzierten/selektierten Datensatz
                         linkNaKeep <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = FALSE, verbose = FALSE )
                         linkNaOmit <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = TRUE, verbose = FALSE )
                         if(linkNaKeep == FALSE & linkNaOmit == FALSE ) {cat("WARNING! Dataset is NOT completely linked (even if cases with missings on all items are removed).\n")}
                         if(linkNaKeep == FALSE & linkNaOmit == TRUE )  {cat("Note: Dataset is not completely linked. This is probably only due to missings on all cases.\n")}
                         if(linkNaKeep == TRUE )                        {cat("Dataset is completely linked.\n")}
                      }
     ### Sektion 'Anpassung der Methode (gauss, monte carlo) und der Nodes'             
                      if(method == "montecarlo")  {
                        if(nodes < 100 ) {
    				               cat(paste("Warning: Due to user specification, only ",nodes," nodes are used for '",method,"' estimation. Please note or re-specify your analysis.\n",sep=""))
    				            }
                        if(is.null(nodes) )   {
                          cat(paste("'",method,"' has been chosen for estimation method. Number of nodes was not explicitly specified. Set nodes to 1000.\n",sep=""))
    				              if(software == "conquest") {nodes <- 1000}
      		                if(software == "tam" )     {nodes <- 0; snodes <- 1000; QMC <- TRUE}
    				            }  else  { if(software == "tam" )     {snodes <- nodes; nodes <- 0; QMC <- TRUE} } 
    			           }
    			           if(method != "montecarlo") {
                        if ( is.null(nodes) )   {
                             cat(paste("Number of nodes was not explicitly specified. Set nodes to 20 for method '",method,"'.\n",sep=""))
    				                 if ( software == "conquest" ) { nodes <- 20 } 
    				                 if ( software == "tam" )      { nodes <- seq(-6,6,len=20); snodes <- 0; QMC <- FALSE } 
    				            }  else { 
     				                 if ( software == "tam" )      { nodes <- seq(-6,6,len=nodes); snodes <- 0; QMC <- FALSE }
                        }       
    				            if ( !is.null(seed)) {
                              if ( software == "conquest" ) {  cat("Warning! 'seed'-Parameter is appropriate only in Monte Carlo estimation method. (see conquest manual, p. 225) Recommend to set 'seed' to NULL.\n") }
                        }
    			           }
     ### Sektion 'Datensaetze softwarespezifisch aufbereiten: Conquest' ###
                      if ( software == "conquest" )   {                         ### untere Zeile: wieviele character muss ich fuer jedes Item reservieren?
                          var.char <- sapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN=function(ii) {max(nchar(as.character(na.omit(ii))))})
                          no.number <- setdiff(1:length(var.char), grep("[[:digit:]]",var.char))
                          if(length(no.number)>0) {var.char[no.number] <- 1}    ### -Inf steht dort, wo nur missings sind, hier soll die Characterbreite auf 1 gesetzt sein
                          if(use.letters == TRUE)   {                           ### sollen Buchstaben statt Ziffern beutzt werden? Dann erfolgt hier Recodierung.
                             rec.statement <- paste(0:25,"='",LETTERS,"'",sep="",collapse="; ")
                             for (i in all.Names[["variablen"]])  {             ### Warum erst hier? Weil Prüfungen (auf Dichotomität etc. vorher stattfinden sollen)
                                  dat[,i] <- recode(dat[,i], rec.statement)}
                             var.char <- rep(1,length(all.Names[["variablen"]]))}## var.char muß nun neu geschrieben werden, da nun alles wieder einstellig ist!
                      }
     ### Sektion 'deskriptive Ergebnisse berechnen und durchschleifen' ###
                      daten    <- data.frame(ID=as.character(dat[,all.Names[["ID"]]]), dat[,namen.all.hg, drop = FALSE], dat[,all.Names[["variablen"]], drop = FALSE], stringsAsFactors = FALSE)
                      if ( withDescriptives == TRUE ) { 
                          deskRes <- desk.irt(daten = daten, itemspalten = match(all.Names[["variablen"]], colnames(daten)), percent = TRUE)
                          discrim <- item.diskrim(daten,match(all.Names[["variablen"]], colnames(daten)))
                      }  else  {
                         deskRes <- NULL 
                         discrim <- NULL
                      }   
                      if ( software == "conquest" )   {
                          daten$ID <- gsub ( " ", "0", formatC(daten$ID, width=max(as.numeric(names(table(nchar(daten$ID)))))) )
                          fixed.width <- c(as.numeric(names(table(nchar(daten[,"ID"])))), all.hg.char, rep(max(var.char),length(var.char)))
                          write.fwf(daten , file.path(dir,paste(analysis.name,".dat",sep="")), colnames = FALSE,rownames = FALSE, sep="",quote = FALSE,na=".", width=fixed.width)
                          test <- readLines(paste(dir,"/",analysis.name,".dat",sep=""))
                          stopifnot(length(table(nchar(test)))==1)              ### Check: hat der Resultdatensatz eine einheitliche Spaltenanzahl? Muß unbedingt sein!
                          lab <- data.frame(1:length(all.Names[["variablen"]]), all.Names[["variablen"]], stringsAsFactors = FALSE)
                          colnames(lab) <- c("===>","item")                     ### schreibe Labels!
                          write.table(lab,file.path(dir,paste(analysis.name,".lab",sep="")),col.names = TRUE,row.names = FALSE, dec = ",", sep = " ", quote = FALSE)
                          if(!is.null(conquest.folder))     {
                             batch <- paste( normalize.path(conquest.folder),paste(analysis.name,".cqc",sep=""), sep=" ")
                             write(batch, file.path(dir,paste(analysis.name,".bat",sep="")))}
                          foo <- gen.syntax(Name=analysis.name, daten=daten, all.Names = all.Names, namen.all.hg = namen.all.hg, all.hg.char = all.hg.char, var.char= max(var.char), model=qMatrix, ANKER=anchor, pfad=dir, n.plausible=n.plausible, compute.fit = compute.fit,
                                            constraints=constraints, std.err=std.err, distribution=distribution, method=method, n.iterations=n.iterations, nodes=nodes, p.nodes=p.nodes, f.nodes=f.nodes, converge=converge,deviancechange=deviancechange, equivalence.table=equivalence.table, use.letters=use.letters, model.statement=model.statement, conquest.folder = conquest.folder, allowAllScoresEverywhere = allowAllScoresEverywhere, seed = seed, export = export)
                          if(!is.null(anchor))  { 
                             ankFrame <- anker (lab = lab, prm = anchor) 
                             write.table(ankFrame, file.path(dir,paste(analysis.name,".ank",sep="")) ,sep=" ", col.names = FALSE, row.names = FALSE, quote = FALSE)
                          }
     ### wenn Conquest gewaehlt, dann ggf. Logfile umbenennen falls bereits (unter demselben namen existiert) existiert
                          if(file.exists( file.path ( dir,  paste(analysis.name,".log",sep=""))) )  {
                             cat(paste("Found existing log file '",paste(analysis.name,".log",sep=""), "' in folder '",dir,"'\nConquest analysis will overwrite log file. Original log file will be saved as '",paste(analysis.name,"_old.log'\n",sep=""),sep=""))
                             do <- file.rename(from = file.path(dir, paste(analysis.name,".log",sep="")), to = file.path(dir, paste(analysis.name,"_old.log",sep="")))
                          }
     ### Sektion 'Rueckgabeobjekt bauen', hier fuer Conquest                    ### setze Optionen wieder in Ausgangszustand
                          options(scipen = original.options); flush.console()   ### Achtung: setze Konsolenpfade in Hochkommas, da andernfalls keine Leerzeichen in den Ordner- bzw. Dateinamen erlaubt sind!
                          ret <- list ( software = software, input = paste("\"", file.path(dir, paste(analysis.name,"cqc",sep=".")), "\"", sep=""), conquest.folder = paste("\"", conquest.folder, "\"", sep=""), dir=dir, analysis.name=analysis.name, model.name = analysis.name, qMatrix=qMatrix, all.Names=all.Names, deskRes = deskRes, discrim = discrim)
                          class(ret) <-  c("defineConquest", "list")
                          return ( ret )  }
     ### Sektion 'Rueckgabeobjekt fuer tam'
                      if ( software == "tam" )   {
                          control <- list ( nodes = nodes , snodes = snodes , QMC=QMC, convD = deviancechange ,conv = converge , convM = .0001 , Msteps = 4 , maxiter = n.iterations, max.increment = 1 , 
                                     min.variance = .001 , progress = progress , ridge=0 , seed = seed , xsi.start0=FALSE,  increment.factor=increment.factor , fac.oldxsi= fac.oldxsi) 
                          ret     <- list ( software = software, qMatrix=qMatrix, anchor=anchor,  all.Names=all.Names, daten=daten, irtmodel=irtmodel, est.slopegroups = est.slopegroups, guessMat=guessMat, control = control, n.plausible=n.plausible, dir = dir, analysis.name=analysis.name, deskRes = deskRes, discrim = discrim)
                          class(ret) <-  c("defineTam", "list")
                          return ( ret )    }   }  }


### Hilfsfunktionen fuer prep.conquest
.checkContextVars <- function(x, varname, type, itemdaten, suppressAbort = FALSE)   {
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
                     toRemove<- NULL
                     mis     <- length(table(x))
                     if(mis == 0 )  {
                        if ( suppressAbort == FALSE ) { 
                             stop(paste("Error: ",type," Variable '",varname,"' without any values.",sep=""))
                        }  else  { 
                             cat(paste("Warning: ",type," Variable '",varname,"' without any values. '",varname,"' will be removed.\n",sep=""))
                             toRemove <- varname
                        }
                     }        
                     if(mis == 1 )  {
                        if ( suppressAbort == FALSE ) { 
                             stop(paste("Error: ",type," Variable '",varname,"' is a constant.",sep=""))
                        }  else  { 
                             cat(paste("Warning: ",type," Variable '",varname,"' is a constant. '",varname,"' will be removed.\n",sep=""))
                             toRemove <- varname
                        }
                     }        
                     if(type == "DIF" | type == "group") {if(mis > 10)   {cat(paste("Serious warning: ",type," Variable '",varname,"' with more than 10 categories. Recommend recoding. \n",sep=""))}}
                     wegDifMis <- NULL; wegDifConst <- NULL; char <- 1; weg <- which(is.na(1:12))
                     if ( is.null(toRemove)) {
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
                                                wegDifMis <- c(wegDifMis, rownames(completeMissingGroupwise)[missingCat.i] )
                                                cat(paste(rownames(completeMissingGroupwise)[missingCat.i],collapse=", ")); cat("\n")
                                             }
                                             constantCat.i <- which(completeMissingGroupwise[,iii] == 1)
                                             if(length(constantCat.i) > 0) {
                                                cat(paste("Warning: Following items are constants in ",type," variable '",varname,"', group ",iii,":\n",sep=""))
                                                wegDifConst <- c(wegDifConst, rownames(completeMissingGroupwise)[constantCat.i] )
                                                cat(paste(rownames(completeMissingGroupwise)[constantCat.i],collapse=", ")); cat("\n")
                                             }
                                        }
                          }
                     }     
                     return(list(x = x, char = char, weg = weg, varname=varname, wegDifMis = wegDifMis, wegDifConst = wegDifConst, toRemove = toRemove))}


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
               dataFrame <- as.data.frame(lapply(dataFrame, FUN=function(ii) {recode(ii, paste(sysmis,"= NA", collapse = "; ") ) } ) )
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

converged<- function (dir, logFile) { 
            logF  <- scan(file = file.path ( dir, logFile ), what="character",sep="\n",quiet=TRUE)
            last  <- logF[length(logF)] 
            if ( !crop(last) == "=>quit;" ) { 
               if ( length( grep("quit;" , last)) == 0 ) { 
                   cat(paste("Warning: Model seems not to have converged. Log file unexpectedly finishs with '",last,"'.\nReading in model output might fail.\n", sep=""))
                   isConv <- FALSE
               }  else  { 
                   isConv <- TRUE   
               }    
            }  else  {       
            isConv <- TRUE
            }
            return(isConv)}

getConquestResults<- function(path, analysis.name, model.name, qMatrix, all.Names, abs.dif.bound , sig.dif.bound, p.value, simplify, deskRes, discrim) {
         allFiles <- list.files(path=path, pattern = analysis.name, recursive = FALSE)
         qL       <- melt(qMatrix, id.vars = colnames(qMatrix)[1], variable.name = "dimensionName", na.rm=TRUE)
         qL       <- qL[which(qL[,"value"] != 0 ) , ]
         varName  <- colnames(qMatrix)[1]
         ret      <- NULL                                                       ### Rueckgabeobjekt initialisieren
    ### Sektion 'Konvergenz pruefen' (log)
         logFile  <- paste(analysis.name, "log", sep=".")
         isConv   <- converged ( dir = path, logFile = logFile )
    ### Sektion 'Itemparameter auslesen' (itn)
         itnFile  <- paste(analysis.name, "itn", sep=".")
         if (!itnFile %in% allFiles) {
             cat("Cannot find Conquest itn-file.\n")
         } else {
             itn  <- get.itn( file.path(path, itnFile) )
             allID<- c("dif.name", "dif.value", "item.name", "Label")
             drin <- allID[which(allID %in% colnames(itn))]
             # itn1 <- data.frame ( model = model.name, source = "conquest", var1 = itnS[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],itnS[,"item.name"]),"dimensionName"], par = "itemP",  derived.par = NA, value = as.numeric(itnS[,"item.p"]), stringsAsFactors = FALSE)
             # itn2 <- data.frame ( model = model.name, source = "conquest", var1 = itnS[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],itnS[,"item.name"]),"dimensionName"], par = "Nvalid",  derived.par = NA, value = as.numeric(itnS[,"n.valid"]), stringsAsFactors = FALSE)
             itnL <- melt(itn, id.vars = drin, measure.vars = "pt.bis", value.name = "ptBis", na.rm=TRUE)
             both <- merge(qL, itnL, by.x = colnames(qMatrix)[1], by.y = "item.name", all=TRUE)
             drin2<- setdiff ( drin, "item.name")
             both["var2"] <- apply(X = both, MARGIN = 1, FUN = function ( zeile ) { paste( names ( zeile[drin2]), zeile[drin2], sep="=", collapse= ", ") })
             itn3 <- data.frame ( model = model.name, source = "conquest", var1 = both[,colnames(qMatrix)[1]], var2 = NA , type = "fixed", indicator.group = "items", group = both[,"dimensionName"], par = "ptBis",  derived.par = both[,"var2"], value = as.numeric(both[,"ptBis"]), stringsAsFactors = FALSE)
             ret  <- rbind(ret, itn3)
         }
    ### Sektion 'Itemparameter auslesen' (shw)
         shwFile  <- paste(analysis.name, "shw", sep=".")
         if (!shwFile %in% allFiles) {
             cat("Cannot find Conquest showfile.\n")
         } else {
             shw  <- get.shw( file = file.path(path, shwFile) )                 ### Untere Zeile: Dimensionen analog zu Bezeichnung in Q Matrix benennen
             if(is.null( dim(shw$cov.structure) )) {from <- NA} else { from <- shw$cov.structure[-ncol(shw$cov.structure),1]}
             altN <- data.frame ( nr = 1:(ncol(qMatrix)-1), pv = paste("dim", 1:(ncol(qMatrix)-1),sep="."), from = from ,  to = colnames(qMatrix)[-1], stringsAsFactors = FALSE)
             shw[["item"]]  <- merge(shw[["item"]], qL[,-match("value", colnames(qL))], by.x = "item", by.y = colnames(qMatrix)[1], all=TRUE)
             shw1 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = NA, value = as.numeric(shw$item[,"ESTIMATE"]), stringsAsFactors = FALSE)
             shw2 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items",group = shw$item[,"dimensionName"], par = "est",  derived.par = "se", value = as.numeric(shw$item[,"ERROR"]), stringsAsFactors = FALSE)
             toOff<- shw2[ which(is.na(shw2[,"value"])), "var1"]
             if(length(toOff)>0) {
                shw1[match(toOff, shw1[,"var1"]), "par"] <- "offset"
                shw2  <- shw2[-which(is.na(shw2[,"value"])),] }                 ### entferne Zeilen aus shw2, die in der "value"-Spalte NA haben
             if(!is.null ( deskRes ) ) {
                deskR<- merge(deskRes, qL[,-match("value", colnames(qL))], by.x = "item.name", by.y = colnames(qMatrix)[1], all=TRUE)
                shw3 <- data.frame ( model = model.name, source = "conquest", var1 = deskR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,"item.p"], stringsAsFactors = FALSE)
                shw4 <- data.frame ( model = model.name, source = "conquest", var1 = deskR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "Nvalid",  derived.par = NA, value = deskR[,"valid"], stringsAsFactors = FALSE)
             }  else  {
                shw3 <- NULL
                shw4 <- NULL
             }
             if( !is.null(discrim) )  {
                 discR<- merge(discrim, qL[,-match("value", colnames(qL))], by.x = "item.name", by.y = colnames(qMatrix)[1], all=TRUE)
                 shw5 <- data.frame ( model = model.name, source = "conquest", var1 = discR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = discR[,"dimensionName"], par = "itemDiscrim",  derived.par = NA, value = discR[,"item.diskrim"], stringsAsFactors = FALSE)
             }  else  {
                 shw5 <- NULL
             }
             ret  <- rbind(ret, shw1, shw2, shw3, shw4, shw5)                   ### Rueckgabeobjekt befuellen, danach infit auslesen
             ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = "infit", value = as.numeric(shw$item[,"MNSQ.1"]), stringsAsFactors = FALSE),
                                data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = "outfit", value = as.numeric(shw$item[,"MNSQ"]), stringsAsFactors = FALSE) )
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
                         shwE3<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "outfit", value = shw[[i]][,"MNSQ"], stringsAsFactors = FALSE)
                         shwSE<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "se", value = shw[[i]][,"ERROR"], stringsAsFactors = FALSE)
                         toOff<- shwSE[ which(is.na(shwSE[,"value"])), "var1"]
                         if(length(toOff)>0) {
                            shwE[match(toOff, shwE[,"var1"]), "par"] <- "offset"
                            shwSE <- shwSE[-which(is.na(shwSE[,"value"])),] }
                         ret  <- rbind(ret, shwE, shwE2, shwE3, shwSE)
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
                cov2 <- facToChar( dataFrame = melt(cov1[-nrow(cov1),], id.vars = colnames(cov1)[1], na.rm=TRUE))
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
                regL <- melt(reg, id.vars = colnames(reg)[1], measure.vars = colnames(reg)[-c(1, ncol(reg))], na.rm=TRUE)
                foo  <- data.frame ( do.call("rbind", strsplit(as.character(regL[,"variable"]), "_")), stringsAsFactors = FALSE)
                colnames(foo) <- c("par", "group")
                foo[,"derived.par"] <- recode(foo[,"par"], "'error'='se'; else = NA")
                foo[,"par"] <- "est"
                regL <- data.frame ( regL[,-match("variable", colnames(regL)), drop=FALSE], foo, stringsAsFactors = FALSE)
                regL[,"reg.var"] <- recode(regL[,"reg.var"], "'CONSTANT'='(Intercept)'")
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
             wleL <- melt(wle, id.vars = "ID", measure.vars = colnames(wle)[-c(1:2)], na.rm=TRUE)
             foo  <- data.frame ( do.call("rbind", strsplit(as.character(wleL[,"variable"]), "_")), stringsAsFactors = FALSE)
             colnames(foo) <- c("par", "group")
             foo[,"derived.par"] <- recode(foo[,"par"], "'wle'='est'; 'std.wle'='se'; else=NA")
             foo[,"par"]         <- recode(foo[,"par"], "'wle'='wle'; 'std.wle'='wle'; 'n.solved'='NitemsSolved'; 'n.total'='NitemsTotal'")
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
             pv$pvLong[,"variable"] <- recode( pv$pvLong[,"variable"], rec)
             ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = pv$pvLong[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = pv$pvLong[,"variable"], par = "pv",  derived.par = paste("pv", as.numeric(pv$pvLong[,"PV.Nr"]),sep=""), value = as.numeric(pv$pvLong[,"value"]) , stringsAsFactors = FALSE))
             eaps <- melt ( data.frame ( pv$pvWide[,"ID", drop=FALSE], pv$eap, stringsAsFactors = FALSE), id.vars = "ID", na.rm=TRUE)
             foo  <- data.frame ( do.call("rbind", strsplit(as.character(eaps[,"variable"]), "_")), stringsAsFactors = FALSE)
             colnames(foo) <- c("par", "group")
             foo[,"derived.par"] <- recode(foo[,"par"], "'eap'='est'; 'se.eap'='se'; else=NA")
             foo[,"par"]         <- "eap"
             foo[,"group"]       <- recode(tolower(foo[,"group"]), rec)
             ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = eaps[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = foo[,"group"], par = "eap",  derived.par = foo[,"derived.par"], value = eaps[,"value"] , stringsAsFactors = FALSE))
         }
         attr(ret, "isConverged") <- isConv
         attr(ret, "available")   <- list ( itn = itnFile %in% allFiles, shw = shwFile %in% allFiles, wle = wleFile %in% allFiles, pv = pvFile %in% allFiles)
         return(ret)}

getTamResults     <- function(runModelObj) {
         regr     <- tam.se(runModelObj)
         qMatrix  <- attr(runModelObj, "qMatrix")
         qL       <- melt(qMatrix, id.vars = colnames(qMatrix)[1], variable.name = "dimensionName", na.rm=TRUE)
         qL       <- qL[which(qL[,"value"] != 0 ) , ]
         varName  <- colnames(qMatrix)[1]
         ret      <- NULL                                                       ### Rueckgabeobjekt initialisieren
    ### Sektion 'Itemparameter auslesen' (shw)
         xsis <- merge(data.frame ( item = rownames(runModelObj[["xsi"]]), runModelObj[["xsi"]]), qL[,-match("value", colnames(qL))],  by.x = "item", by.y = colnames(qMatrix)[1], all = TRUE)
         shw1 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = xsis[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = xsis[,"dimensionName"], par = "est",  derived.par = NA, value = xsis[,"xsi"], stringsAsFactors = FALSE)
         shw2 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = xsis[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = xsis[,"dimensionName"], par = "est",  derived.par = "se", value = xsis[,"se.xsi"], stringsAsFactors = FALSE)
         toOff<- shw2[ which(shw2[,"value"] == 0 ), "var1"]
         if(length(toOff)>0) {
            shw1[match(toOff, shw1[,"var1"]), "par"] <- "offset"
            shw2  <- shw2[-which(shw2[,"value"] == 0 ),] }                      ### entferne Zeilen aus shw2, die in der "value"-Spalte NA haben, danach: p-Werte einfuegen
        ### nValid und p-werte aus tam auszulesen, misslingt wenn DIF modelle gerechnet werden ... werden statt dessen aus Itemdaten ausgelesen
        # shw3 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(runModelObj[["item"]][,"item"]), var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],runModelObj[["item"]][,"item"]),"dimensionName"], par = "itemP",  derived.par = NA, value = runModelObj[["item"]][,"M"], stringsAsFactors = FALSE)
        # shw4 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(runModelObj[["item"]][,"item"]), var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],runModelObj[["item"]][,"item"]),"dimensionName"], par = "Nvalid",  derived.par = NA, value = runModelObj[["item"]][,"N"], stringsAsFactors = FALSE)
         if(!is.null ( attr(runModelObj, "deskRes") ) ) {
            deskR<- merge(attr(runModelObj, "deskRes"), qL[,-match("value", colnames(qL))],  by.x = "item.name", by.y = colnames(qMatrix)[1], all = TRUE)
            shw3 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(deskR[,"item.name"]), var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,"item.p"], stringsAsFactors = FALSE)
            shw4 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(deskR[,"item.name"]), var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "Nvalid",  derived.par = NA, value = deskR[,"valid"], stringsAsFactors = FALSE)
         }  else  {
            shw3 <- NULL
            shw4 <- NULL
         }
         if(!is.null ( attr(runModelObj, "discrim") ) ) {
            discR<- merge( attr(runModelObj, "discrim") , qL[,-match("value", colnames(qL))],  by.x = "item.name", by.y = colnames(qMatrix)[1], all = TRUE)
            shw5 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = discR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = discR[,"dimensionName"], par = "itemDiscrim",  derived.par = NA, value = discR[,"item.diskrim"], stringsAsFactors = FALSE)
         }  else  {
            shw5 <- NULL
         }                                                                      ### untere Zeile: Diskriminationsparameter im 2pl-Fall auslesen
         if(attr(runModelObj, "irtmodel") %in% c("2PL", "2PL.groups", "GPCM", "3PL")) {
            shw6 <- do.call("rbind", lapply (  1 : length ( colnames( qMatrix ) [-1] ) , FUN = function ( dims ) {
                    shw6D <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = regr[["B"]][,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = colnames(qMatrix)[dims+1], par = "estSlope",  derived.par = NA, value = regr[["B"]][,paste("B.Cat1.Dim",dims,sep="")], stringsAsFactors = FALSE)
                    shw6se<- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = regr[["B"]][,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = colnames(qMatrix)[dims+1], par = "estSlope",  derived.par = "se", value = regr[["B"]][,paste("se.B.Cat1.Dim",dims,sep="")], stringsAsFactors = FALSE)
                    return(rbind(shw6D, shw6se)) }))
         }  else  {
            shw6 <- NULL
         }
         ret  <- rbind(ret, shw1, shw2, shw3, shw4, shw5, shw6)                 ### Rueckgabeobjekt befuellen, danach infit auslesen
         infit<- tam.fit(runModelObj, progress=FALSE)                           ### Achtung: wenn DIF-Analyse, dann misslingt untere Zeile: Workarond!
    ### DIF-Parameter umbenennen, so dass es konsistent zu "getConquestResults" ist
         if(inherits(try( ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],rownames(runModelObj[["xsi"]])),"dimensionName"], par = "est",  derived.par = "infit", value = infit$itemfit[,"Infit"], stringsAsFactors = FALSE),
                                             data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],rownames(runModelObj[["xsi"]])),"dimensionName"], par = "est",  derived.par = "outfit", value = infit$itemfit[,"Outfit"], stringsAsFactors = FALSE)) , silent = TRUE),"try-error"))  {
            ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = NA, par = "est",  derived.par = "infit", value = infit$itemfit[,"Infit"], stringsAsFactors = FALSE),
                               data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = NA, par = "est",  derived.par = "outfit", value = infit$itemfit[,"Outfit"], stringsAsFactors = FALSE) )
            ret[,"toMerge"] <- halve.string(ret[,"var1"], ":", first=TRUE)[,1]
            ret  <- merge(ret, qL[,c("Item", "dimensionName")], by.x = "toMerge", by.y = "Item", all.x = TRUE)
            ret  <- ret[,-match(c("group", "toMerge"), colnames(ret))]
            colnames(ret) <- recode(colnames(ret), "'dimensionName'='group'")
            indD5<- setdiff( 1:nrow(ret), grep(":DIF", ret[,"var1"]))
            indD5<- setdiff( ret[indD5,"var1"], qL[,"Item"])
            to   <- remove.pattern(string = indD5, pattern = "DIF_")
            to1  <- remove.numeric(to)
            to2  <- remove.non.numeric(to)
            indD5<- data.frame ( from = indD5, to = paste(to1, to2, sep="_"), stringsAsFactors = FALSE)
            recSt<- paste("'",indD5[,"from"] , "' = '" , indD5[,"to"],"'", collapse="; ",sep="")
            indD <- grep(":DIF", ret[,"var1"])
            indD2<- halve.string(ret[indD,"var1"], pattern = ":DIF_", first=TRUE)
            indD3<- remove.numeric(indD2[,2])
            indD4<- remove.non.numeric(indD2[,2])
            ret[indD,"var1"] <- paste("item_", indD2[,1], "_X_", paste(indD3, indD4, sep="_"), sep="")
            ret[,"var1"]     <- recode(ret[,"var1"], recSt)
         }
    ### Sektion 'Populationsparameter auslesen' (shw)
         if(ncol(qMatrix) == 2) {                                               ### eindimensionaler Fall
            ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = colnames(qMatrix)[2], var2 = NA , type = "distrpar", indicator.group = NA, group = "persons", par = "var",  derived.par = NA, value = runModelObj[["variance"]][1,1] , stringsAsFactors = FALSE))
         }  else  {                                                             ### mehrdimensional: (Residual-)Varianzen und (Residual-)Korrelationen der lat. Dimensionen
            cov1 <- runModelObj[["variance"]]
            colnames(cov1) <- colnames(qMatrix)[-1]
            rownames(cov1) <- colnames(qMatrix)[-1]
            cor1 <- cov2cor(cov1)
            for (ii in 1:nrow(cor1))   {                                        ### loesche alles oberhalb der Hauptdiagonalen
                 cor1[ii,ii:ncol(cor1)] <- NA}
            cor1 <- melt(cor1, measure.vars = colnames(cor1), na.rm=TRUE)
            vars <- diag(cov1)
            ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = c(names(vars),as.character(cor1[,"Var1"])) , var2 = c(rep(NA, length(vars)), as.character(cor1[,"Var2"])) , type = "random", indicator.group = NA, group = "persons", par = c(rep("var",length(vars)), rep("correlation", nrow(cor1))) ,  derived.par = NA, value = c(unlist(vars), cor1[,"value"]), stringsAsFactors = FALSE))
         }
    ### Sektion 'Regressionsparameter auslesen' (shw)
         if( !isTRUE(all.equal ( dim(runModelObj$beta) , c(1,1))))  {           ### wird nur gemacht, wenns auch Regressionsparameter gibt
             regr <- data.frame ( reg.var = rownames(regr$beta), regr$beta, stringsAsFactors = FALSE)
             regr <- melt(regr, id.vars = "reg.var", na.rm=TRUE)
             regr2<- data.frame ( par = "est", derived.par = recode(unlist(lapply(strsplit(as.character(regr[,"variable"]),"\\."), FUN = function ( l ) {l[1]})), "'se'='se'; else=NA"), group = colnames(qMatrix)[as.numeric(remove.pattern( string = unlist(lapply(strsplit(as.character(regr[,"variable"]),"\\."), FUN = function ( l ) {l[2]})), pattern = "Dim")) + 1], regr, stringsAsFactors = FALSE)
             ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = regr2[,"reg.var"], var2 = NA , type = "regcoef", indicator.group = NA, group = regr2[,"group"], par = regr2[,"par"],  derived.par = regr2[,"derived.par"], value = regr2[,"value"] , stringsAsFactors = FALSE))
         }
    ### Sektion 'Modellindizes auslesen' (shw)
             ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = NA, var2 = NA , type = "model", indicator.group = NA, group = NA, par = c("deviance", "Npar", "AIC", "BIC"), derived.par = NA, value = unlist(runModelObj[["ic"]][c("deviance", "Npars", "AIC", "BIC")]), stringsAsFactors = FALSE))
    ### Sektion 'Personenparameter auslesen' (wle)
         wle  <- tam.wle(runModelObj, progress = FALSE)                         ### Achtung: im eindimensionalen Fall enthalten die Spaltennamen keine Benennung der Dimension
         eind1<- ncol(wle) == 7
         if(eind1 == TRUE) {
            cols1<- grep("theta$", colnames(wle))
            cols2<- grep("error$", colnames(wle))
            stopifnot(length(cols1) ==1); stopifnot(length(cols2) ==1)
            colnames(wle)[c(cols1,cols2)] <- paste(colnames(wle)[c(cols1,cols2)], ".Dim1", sep="")
         }
         weg1 <- grep("WLE.rel", colnames(wle))
         wleL <- melt(wle, id.vars = "pid", measure.vars = colnames(wle)[-c(1:2,weg1)], na.rm=TRUE)
         wleL[,"group"] <- colnames(qMatrix)[as.numeric(remove.pattern(string = unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[2]})), pattern = "Dim"))+1]
         wleL[,"par"]   <- recode(unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[1]})), "'PersonScores'='NitemsSolved'; 'PersonMax'='NitemsTotal'; 'theta'='wle'; 'error'='wle'")
         wleL[,"derived.par"] <- recode(unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[1]})), "'theta'='est'; 'error'='se';else=NA")
         ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = wleL[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = wleL[,"group"], par = wleL[,"par"],  derived.par = wleL[,"derived.par"], value = wleL[,"value"] , stringsAsFactors = FALSE))
    ### Sektion 'Personenparameter auslesen' (PVs)
         pv   <- tam.pv(runModelObj, nplausible = attr(runModelObj, "n.plausible"), normal.approx = TRUE, ntheta = 2000, np.adj = 20 )$pv
         pvL  <- melt(pv, id.vars = "pid", na.rm=TRUE)
         pvL[,"PV.Nr"] <- as.numeric(remove.pattern(string = unlist(lapply(strsplit(as.character(pvL[,"variable"]),"\\."), FUN = function (l) {l[1]})), pattern = "PV"))
         pvL[,"group"] <- colnames(qMatrix)[as.numeric(remove.pattern(string = unlist(lapply(strsplit(as.character(pvL[,"variable"]),"\\."), FUN = function (l) {l[2]})), pattern = "Dim"))+1]
         ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = pvL[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = pvL[,"group"], par = "pv",  derived.par = paste("pv", pvL[,"PV.Nr"],sep=""), value = pvL[,"value"] , stringsAsFactors = FALSE))
         eaps <- runModelObj[["person"]]                                        ### Achtung: im eindimensionalen Fall enthalten die Spaltennamen keine Benennung der Dimension
         eind1<- ncol(eaps) == 7                                                ### (uneinheitlich zu pvs, wo es immer eine Benennung gibt.)
         #eind2<- length(grep(".Dim1$", colnames(eaps))) == 0                   ### Im eindimensionalen Fall muss Benennung ergaenzt werden
         #stopifnot ( eind1 == eind2)                                           ### zur Sicherheit werden hier zwei Indikatoren fuer Eindimensionalitaet genutzt. Fehlermeldung bei Widerspruch
         if(eind1 == TRUE) {                                                    ### ggf. muss diese Passage nach Release neuerer TAM-Versionen korrigiert werden
            cols <- grep("EAP$", colnames(eaps))
            stopifnot(length(cols) == 2)
            colnames(eaps)[cols] <- paste(colnames(eaps)[cols], ".Dim1", sep="")
         }
         eaps <- melt(eaps, id.vars = "pid", measure.vars = grep("EAP", colnames(eaps)), na.rm=TRUE)
         eaps[,"group"] <- colnames(qMatrix)[as.numeric(remove.pattern ( string = halve.string(string = as.character(eaps[,"variable"]), pattern = "\\.", first = FALSE)[,"X2"], pattern = "Dim"))+1]
         eaps[,"par"]   <- "est"
         eaps[grep("^SD.",as.character(eaps[,"variable"])),"par"]   <- "se"
         ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = eaps[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = eaps[,"group"], par = "eap", derived.par = eaps[,"par"], value = eaps[,"value"] , stringsAsFactors = FALSE))
         return(ret)}


### Extraktorfunktionen
eapFromRes <- function ( resultsObj ) { 
          sel  <- resultsObj[intersect( which(resultsObj[,"par"] == "eap"),which(resultsObj[,"indicator.group"] == "persons")),]
          sel  <- do.call("rbind", by(sel, INDICES = sel[,"group"], FUN = function ( gr ) { 
                  res  <- dcast ( gr , var1~derived.par, value.var = "value")
                  colnames(res) <- c("ID", "EAP", "SE.EAP")                     ### Spaeter eventuell die Original-ID durchschleifen?
                  res  <- data.frame ( res[,"ID", drop=FALSE], dimension = as.character(gr[1,"group"]), res[,-1,drop=FALSE], stringsAsFactors = FALSE)
                  return(res)}))
          return(sel)}        

pvFromRes  <- function ( resultsObj ) { 
          sel  <- resultsObj[intersect( which(resultsObj[,"par"] == "pv"),which(resultsObj[,"indicator.group"] == "persons")),]
          sel  <- do.call("rbind", by(sel, INDICES = sel[,"group"], FUN = function ( gr ) { 
                  res  <- dcast ( gr , var1~derived.par, value.var = "value")
                  colnames(res)[1] <- "ID"                                      ### Spaeter eventuell die Original-ID durchschleifen?
                  res  <- data.frame ( res[,"ID", drop=FALSE], dimension = as.character(gr[1,"group"]), res[,-1,drop=FALSE], stringsAsFactors = FALSE)
                  return(res)}))
          return(sel)}        

itemFromRes<- function ( resultsObj ) { 
          if( "multipleResults" %in% class(resultsObj)) {                       ### Mehrmodellfall
              selM  <- do.call("rbind", by(data = resultsObj, INDICES = resultsObj[,"model"], FUN = function ( mr ) { 
                       toMatch <- which ( unlist ( lapply ( attr(resultsObj, "att"), FUN = function ( aa ) { aa[[1]][["model.name"]] == mr[1,"model"] } ) )  == TRUE ) 
                       stopifnot(length(toMatch) == 1 )
                       attr(mr, "all.Names")    <- attr(resultsObj, "att")[[toMatch]][[1]][["all.Names"]]
                       attr(mr, "dif.settings") <- attr(resultsObj, "att")[[toMatch]][[1]][["dif.settings"]]
                       class(mr) <- "data.frame"
                       res     <- itemFromRes ( mr ) 
                       return(res) }))
          }  else  { 
             sel  <- resultsObj[intersect( which(resultsObj[,"par"] %in% c("est", "estSlope", "Nvalid", "itemP", "ptBis", "itemDiscrim", "offset")),which(resultsObj[,"indicator.group"] == "items")),]
        ### separate selektion wenn es DIF gibt
             if ( length(attr(resultsObj, "all.Names")[["DIF.var"]])>0) { 
                 itemList <- do.call("rbind", lapply ( attr(resultsObj, "all.Names")[["variablen"]], FUN = function ( v ) { 
                             ind <- grep( v, sel[,"var1"])
                             it  <- unique ( sel[ind,"var1"])
                             if(length(it)>3) {
                                cat(paste("Warning! DIF variable '",attr(resultsObj, "all.Names")[["DIF.var"]],"' seems to have more than two categories. To date, this is not supported by 'eatModel'.\n",sep=""))
                             }
                             item<- it [ which(it %in% v) ]
                             dif <- sort ( setdiff( it, item ))[1]
                             weg <- setdiff ( setdiff ( it, item) , dif ) 
                             return ( data.frame ( item = item, dif = dif, weg = weg , stringsAsFactors = FALSE) ) })) 
                 weg      <- wo.sind ( itemList[,"weg"], sel[,"var1"], quiet = TRUE)
                 forDif   <- wo.sind ( itemList[,"dif"], sel[,"var1"], quiet = TRUE)
                 stopifnot(length( intersect(weg, forDif)) == 0 ) 
                 selForDif<- sel[forDif, ]             
                 sel      <- sel[-c(weg, forDif) , ] 
                 selDIF   <- do.call("rbind", by(selForDif, INDICES = selForDif[,"group"], FUN = function ( gr ) { 
                             res  <- dcast ( gr , var1~par+derived.par, value.var = "value")
                             mat  <- lapply( attr(resultsObj, "all.Names")[["variablen"]], FUN = function ( v ) { grep(v, res[,"var1"])})
                             stopifnot (  all ( sapply(mat, length) == 1) ) 
                             res[unlist(mat),"item"]  <- attr(resultsObj, "all.Names")[["variablen"]]
                             colnames(res) <- recode ( colnames(res) , "'est_infit'='infitDif'; 'est_se'='seDif'; 'est_NA'='estDif'")
                             res[,"absDif"]<- abs ( res[,"estDif"]  * 2 ) 
                             res[,paste("CI__", attr(resultsObj, "dif.settings")[["p.value"]] ,"__lb",sep="")] <- res[,"absDif"] - 2*abs(qnorm(0.5*(1-attr(resultsObj, "dif.settings")[["p.value"]]))) * res[,"seDif"]
                             res[,paste("CI__", attr(resultsObj, "dif.settings")[["p.value"]] ,"__ub",sep="")] <- res[,"absDif"] + 2*abs(qnorm(0.5*(1-attr(resultsObj, "dif.settings")[["p.value"]]))) * res[,"seDif"]
                             res[,"difIndex"] <- apply(res[,c("absDif", "CI__0.9__lb", "CI__0.9__ub")], MARGIN = 1, FUN = function ( d ) { 
                                                 check <- all ( !is.na(d) ) 
                                                 if(check == TRUE) { 
                                                    crit1 <- d[["absDif"]] > attr(resultsObj, "dif.settings")[["abs.dif.bound"]]
                                                    crit2 <- !all ( sort ( c ( d[["CI__0.9__lb"]], attr(resultsObj, "dif.settings")[["sig.dif.bound"]] , d[["CI__0.9__ub"]]), index.return = TRUE)$ix == 1:3 )
                                                    if ( crit1 == TRUE & crit2 == TRUE) { res <- 1 }  else { res <- 0}
                                                 }  else  { 
                                                    res <- NA
                                                 }   
                                                 return(res)})
                             return(res)}))                    
             }                
             sel  <- do.call("rbind", by(sel, INDICES = sel[,"group"], FUN = function ( gr ) { 
                     res  <- dcast ( gr , var1~par+derived.par, value.var = "value")
                     colnames(res) <- recode ( colnames(res) , "'var1'='item'; 'est_infit'='infit'; 'est_outfit'='outfit'; 'est_se'='se'; 'est_NA'='est'; 'estSlope_se'='seSlope'; 'estSlope_NA'='estSlope'; 'offset_NA'='estOffset'; 'Nvalid_NA'='Nvalid'; 'ptBis_NA'='ptBis'; 'itemP_NA'='itemP'; 'itemDiscrim_NA'='itemDiscrim'")
                     cols <- c("Nvalid", "itemP", "itemDiscrim", "est", "estOffset", "se", "estSlope", "seSlope", "infit","outfit", "ptBis")
                     drin1<- which(cols %in% colnames(res))
                     drin2<- grep("ptBis_", colnames(res))
                     res  <- data.frame ( res[,"item", drop=FALSE], dimension = as.character(gr[1,"group"]), res[,c(cols[drin1], colnames(res)[drin2])], stringsAsFactors = FALSE)
                     return(res)}))
             if ( length(attr(resultsObj, "all.Names")[["DIF.var"]])>0) { 
                 ciCo<- colnames(selDIF)[grep("^CI__", colnames(selDIF))]
                 sel <- merge(sel, selDIF[,c("item", "estDif", "seDif", "infitDif", "absDif", ciCo, "difIndex")], by="item", all=TRUE)
             }    
             return(sel)
          }
          return (selM )}           
          
wleFromRes <- function ( resultsObj ) { 
          sel  <- resultsObj[intersect( which(resultsObj[,"par"] %in% c("wle","NitemsSolved", "NitemsTotal")),which(resultsObj[,"indicator.group"] == "persons")),]
          sel  <- do.call("rbind", by(sel, INDICES = sel[,"group"], FUN = function ( gr ) { 
                  res  <- dcast ( gr , var1~par+derived.par, value.var = "value")
                  colnames(res) <- recode ( colnames(res) , "'var1'='ID'; 'NitemsSolved_NA'='NitemsSolved'; 'NitemsTotal_NA'='NitemsTotal'")
                  res  <- data.frame ( res[,"ID", drop=FALSE], dimension = as.character(gr[1,"group"]), res[,-1,drop=FALSE], stringsAsFactors = FALSE)
                  return(res)}))
          return(sel)} 
          
          
prepJack <- function ( resultsObj , arrangeDependentVar = c("singleFrame", "multipleFrames"), modelAsGroup = TRUE ) {
            arrangeDependentVar <- match.arg(arrangeDependentVar)
            noMod <- which(is.na(resultsObj[,"model"]))
            if(length(noMod)>0) {
               cat("Warning: missing values in 'model' column of 'resultsObj'. Output might be invalid.\n")
            }
            sel   <- resultsObj[ which(resultsObj[,"par"] == "pv") ,]
            depVar<- by(sel, INDICES = as.character(sel[,"group"]), FUN = function ( dv ) {
                     nameDep <- paste("value", dv[1,"group"],sep="_")
                     res     <- data.frame ( idstud = dv[,"var1"], subgroup = dv[,"model"], imp = dv[,"derived.par"], platzhalter = dv[,"value"])
                     colnames(res) <- gsub("platzhalter", nameDep, colnames(res))
                     return(res)})
            if (arrangeDependentVar == "singleFrame") {
                if ( length ( depVar ) > 1 ) { 
                     foo <- depVar[[1]]
                     for ( i in 2:length(depVar)) {
                          foo <- merge(foo, depVar[[i]], all = TRUE)
                     }
                     depVar <- foo
                }  else  { 
                     depVar <- depVar[[1]]
                }     
            }
            return(depVar)}
          

table.unlist <- function(dataFrame, verbose = TRUE, useNA = c("no","ifany", "always"))   {
                useNA<- match.arg(useNA)
                # if(class(dataFrame) != "data.frame" ) {stop("Argument of 'table.unlist' has to be of class 'data.frame'.\n")}
                if(class(dataFrame) != "data.frame" ) {
                   if(verbose == TRUE ) {cat(paste("Warning! Argument of 'table.unlist' has to be of class 'data.frame'. Object will be converted to data.frame.\n",sep=""))}
                   dataFrame <- data.frame(dataFrame, stringsAsFactors=FALSE)
                }
                dLong<- melt(dataFrame, measure.vars = colnames(dataFrame), na.rm=FALSE)
                freqT<- table(dLong[,"value"], useNA = useNA)
                names(freqT) <- recode(names(freqT), "NA='NA'")
                return(freqT)}

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
            if(length(changeVariables) >0)   {                                  ### hier werden alle Variablen (auch Faktoren, wenn maintain.factor.scores = FALSE) ggf. geaendert
               do <- paste ( mapply ( function ( ii ) { paste ( "try(dataFrame$'" , ii , "' <- as.numeric(dataFrame$'",ii, "'), silent=TRUE)" , sep = "" ) } , changeVariables  ) , collapse = ";" )
               eval ( parse ( text = do ) )
            }
            if(length(changeFactorWithIndices) >0)   {                          ### hier werden ausschliesslich FAKTOREN, wenn maintain.factor.scores = TRUE, ggf. geaendert
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

get.plausible <- function(file, quiet = FALSE, forConquestResults = FALSE)  {   ### untere Zeile: hier beginnt Einlesen fuer Plausible Values aus Conquest
                 checkForPackage (namePackage = "reshape", targetPackage = "eatRep")                                              
                 input           <- scan(file,what="character",sep="\n",quiet=TRUE)
                 input           <- strsplit(crop(gsub("-"," -",input) ) ," +")# Untere Zeile gibt die maximale Spaltenanzahl
                 n.spalten       <- max ( sapply(input,FUN=function(ii){ length(ii) }) )
                 input           <- data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow=F), stringsAsFactors=F)
                 pv.pro.person   <- sum (input[-1,1]==1:(nrow(input)-1) )       ### Problem: wieviele PVs gibt es pro Person? Kann nicht suchen, ob erste Ziffer ganzzahlig, denn das kommt manchmal auch bei Zeile 7/8 vor, wenn entsprechende Werte = 0.0000
                 n.person        <- nrow(input)/(pv.pro.person+3)               ### Anzahl an PVs pro Person wird bestimmt anhand der uebereinstimmung der ersten Spalte mit aufsteigenden 1,2,3,4...
                 weg             <- c(1, as.numeric( sapply(1:n.person,FUN=function(ii){((pv.pro.person+3)*ii-1):((pv.pro.person+3)*ii+1)}) ) )
                 cases           <- input[(1:n.person)*(pv.pro.person+3)-(pv.pro.person+2),1:2]
                 input.sel       <- input[-weg,]
                 n.dim <- dim(input.sel)[2]-1                                   ### Anzahl der Dimensionen
                 if(quiet == FALSE) {cat(paste(n.person,"persons and",n.dim,"dimensions(s) found.\n"))
                               cat(paste(pv.pro.person,"plausible values were drawn for each person on each dimension.\n"))}
                 ID              <- input[  (pv.pro.person + 3) *  (1:n.person) - (pv.pro.person + 2) ,2]
                 colnames(input.sel) <- c("PV.Nr", paste("dim.",1:(ncol(input.sel)-1),sep=""))
                 input.sel[,1]   <- gsub( " ", "0", formatC(input.sel[,1],width = max(nchar(input.sel[,1]))))
                 input.sel$ID    <- rep(ID, each = pv.pro.person)
                 is.na.ID        <- FALSE
                 if(is.na(input.sel$ID[1])) {                                   ### wenn keine ID im PV-File, wird hier eine erzeugt (Fall-Nr), da sonst reshapen misslingt
                    is.na.ID        <- TRUE                                     ### Die ID wird spaeter wieder geloescht. Um das machen zu koennen, wird Indikatorvariable erzeugt, die sagt, ob ID fehlend war.
                    input.sel$ID    <- rep( 1: n.person, each = pv.pro.person)
                 }
                 input.melt      <- melt(input.sel, id.vars = c("ID", "PV.Nr") , stringsAsFactors = FALSE)
                 input.wide      <- data.frame( case = gsub(" ", "0",formatC(as.character(1:n.person),width = nchar(n.person))) , dcast(input.melt, ... ~ variable + PV.Nr) , stringsAsFactors = FALSE)
                 colnames(input.wide)[-c(1:2)] <- paste("pv.", paste( rep(1:pv.pro.person,n.dim), rep(1:n.dim, each = pv.pro.person), sep = "."), sep = "")
                 weg.eap         <- (1:n.person)*(pv.pro.person+3) - (pv.pro.person+2)
                 input.eap    <- input[setdiff(weg,weg.eap),]                   ### nimm EAPs und deren Standardfehler und haenge sie an Datensatz - all rows that have not been used before
                 input.eap    <- na.omit(input.eap[,-ncol(input.eap),drop=FALSE])## find EAPs and posterior standard deviations 
                 stopifnot(ncol(input.eap) ==  n.dim)
                 input.eap    <- lapply(1:n.dim, FUN=function(ii) {matrix(unlist(as.numeric(input.eap[,ii])), ncol=2,byrow=T)})
                 input.eap    <- do.call("data.frame",input.eap)
                 colnames(input.eap) <- paste(rep(c("eap","se.eap"),n.dim), rep(paste("Dim",1:n.dim,sep="."),each=2),sep="_")  
                 PV           <- data.frame(input.wide,input.eap, stringsAsFactors = FALSE)
                 numericColumns <- grep("pv.|eap_|case",colnames(PV))
                 if(is.na.ID == TRUE) {PV$ID <- NA}
                 for (ii in numericColumns) {PV[,ii] <- as.numeric(as.character(PV[,ii]))  }
                 if(  forConquestResults == TRUE ) {
                      return(list ( pvWide = PV, pvLong = input.melt, eap = input.eap))
                 }  else { 
                 return(PV)}}
                 
checkForPackage <- function (namePackage, targetPackage) {
        if(paste("package:",namePackage,sep="") %in% search() ) {
           cat(paste("Warning: Package '",namePackage,"' is attached. Functions in package '",targetPackage,"' conflict with '",namePackage,"'in some way.\n  '",namePackage,"' therefore will be detached now. \n", sep=""))
           eval(parse(text=paste("detach(\"package:",namePackage,"\", force = TRUE)",sep=""))) } }

get.wle <- function(file)      {                                                ### alte und neue Version der Funktion: neue beginnt dort, wo if(n.wle != round(n.wle))
            input <- scan(file, what = "character", sep = "\n", quiet = TRUE)   ### in neuer Funktion wird nicht mehr der relative Anteil geloester Aufgaben angegeben
            input <- crop(input)                                                ### hauptsaechlich das Problem: wie benenne ich die Spalten korrekt?
            input <- strsplit(input," +")                                       ### loescht erste und letzte Leerzeichen einer Stringkette (benoetigt Paket "gregmisc")
            n.spalten <- max ( sapply(input,FUN=function(ii){ length(ii) }) )   ### Untere Zeile gibt die maximale Spaltenanzahl: Dies minus eins und dann geteilt durch 4 ergibt Anzahl an WLEs
            n.wle <- (n.spalten-1) / 4                                          ### Spaltenanzahl sollte ganzzahlig sein.
            input <- as.numeric.if.possible(data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow = FALSE), stringsAsFactors = FALSE), set.numeric = TRUE, verbose = FALSE)
            if(n.wle == round(n.wle))
              {cat(paste("Found valid WLEs of ", nrow(na.omit(input))," persons for ", n.wle, " dimension(s).\n",sep=""))
               spalten <- unlist( lapply(1:n.wle,FUN=function(ii){c(2*ii,2*ii+1,2*ii+1,2*n.wle+2*ii,2*n.wle+2*ii+1,2*n.wle+2*ii)  }) )
               input   <- data.frame(input[,c(1,spalten)], stringsAsFactors=FALSE)# Obere Zeile: Ein paar Spalten werden zweimal ausgegeben, in diese werden spaeter die "Rel.Freq" und transformierte WLEs eingetragen
               for (i in 1:n.wle) {input[,6*i-2] <- input[,6*i-4] / input[,6*i-3]## trage "Rel.Freq" ein! Untere Zeile: rechne in PISA-Metrik um!
                                   input[,6*i+1] <- input[,6*i-1] / sd(input[,6*i-1], na.rm = TRUE) * 100 + 500}
               colnames(input) <- c("case",as.character(sapply(1:n.wle,FUN=function(ii){paste(c("n.solved","n.total","per.solved","wle","std.wle","wle.500"),ii,sep=".")})))
               weg.damit <- grep("500", colnames(input))                        ### loesche Spalten mit WLE.500 (falsch)
               input     <- input[,-weg.damit]}
            if(n.wle != round(n.wle)) 
              {col.min.na  <- which( rowSums(is.na(input)) == min(rowSums(is.na(input))))[1]### Zeile mit den am wenigsten fehlenden Elementen
               col.numeric <- which ( sapply(input, FUN=function(ii) {class(ii)}) == "numeric" )
               col.real.numbers <- na.omit(unlist ( lapply (col.numeric , FUN= function(ii) { ifelse(input[col.min.na,ii] == round(input[col.min.na,ii]), NA, ii)}) ) )
               cat(paste("Found valid WLEs of ", nrow(na.omit(input))," person(s) for ", length(col.real.numbers)/2, " dimension(s).\n",sep=""))
               namen.1 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("n.solved","n.total")}))
               namen.2 <- as.vector( sapply(1:(length(col.real.numbers)/2),FUN=function(ii){c("wle","std.wle")}))
               namen.1 <- paste(namen.1,rep(1:(length(namen.1)/2),each=2),sep=".")# obere Zeile: benenne nun!
               namen.2 <- paste(namen.2,rep(1:(length(namen.2)/2),each=2),sep=".")
               namen   <- c(namen.1,namen.2)
               colnames(input)[1:2] <- c("case","ID")
               if(length(col.real.numbers) > 0) {colnames(input)[(ncol(input)- length(namen)+1): ncol(input)] <- namen} }
            return(input)}

get.shw <- function(file, dif.term, split.dif = TRUE, abs.dif.bound = 0.6, sig.dif.bound = 0.3, p.value = 0.9) {
            options(warn = -1)                                                  ### warnungen aus
            all.output <- list();   all.terms <- NULL                           ### "dif.term" muß nur angegeben werden, wenn DIF-Analysen geschehen sollen.
            input.all <- scan(file,what="character",sep="\n",quiet=TRUE)        ### ginge auch mit:   input <- readLines(file)
            rowToFind <- c("Final Deviance","Total number of estimated parameters")
            rowToFind <- sapply(rowToFind, FUN = function(ii) {                 ### Find the rows indicated in "rowToFind"
                         row.ii <- grep(ii,input.all)                           ### get the parameter of desired rows
                         stopifnot(length(row.ii) == 1)
                         row.ii <- as.numeric(unlist(lapply (strsplit(input.all[row.ii], " +"), FUN=function(ll) {ll[length(ll)]}) ))
                         return(row.ii)})
            ind <- grep("TERM",input.all)                                       ### Wieviele Tabellen gibt es einzulesen?
            grenzen <- grep("An asterisk",input.all)
            if(length(ind)==0) {stop(paste("No TERM-statement found in file ",file,".\n",sep=""))}
            for (i in 1:length(ind)) { 
                 term <- input.all[ind[i]];  steps <- NULL
                 doppelpunkt <- which( sapply(1:nchar(term),FUN=function(ii){u <- substr(term,ii,ii); b <- u==":"  }) )
                 term <- substr(term,doppelpunkt+2,nchar(term))
                 cat(paste("Found TERM ",i,": '",term,"' \n",sep=""))
                 all.terms <- c(all.terms,term)                                 ### Dies dient nur dazu, hinterher die Liste mit ausgelesenen Tabellen beschriften zu können.
                 bereich <- (ind[i]+6) : (grenzen[i] -2)                        ### Dies der Bereich, der ausgewählt werden muß
                 namen   <- c("No.", strsplit(input.all[bereich[1]-2]," +")[[1]][-1])
                 namen   <- gsub("\\^","",namen)
                 index   <- grep("CI",namen)                                    ### Wenn ein "CI" als Spaltenname erscheint, müssen daraus im R-Dataframe zwei Spalten werden!
                 if(length(index) > 0)  {
                    for (ii in 1:length(index)) {
                         namen  <- c(namen[1:index[ii]], "CI",namen[(index[ii]+1):length(namen)] )}}
                 input.sel  <- crop( input.all[bereich] )                       ### Textfile wird reduziert, und voranstehende und abschließende Leerzeichen werden entfernt
                 input.sel  <- gsub("\\(|)|,"," ",input.sel)                    ### entferne Klammern und Kommas (wenn's welche gibt)
                 input.sel  <- gsub("\\*    ", "  NA", input.sel)               ### hier: gefährlich: wenn mittendrin Werte fehlen, würde stringsplit eine unterschiedliche Anzahl Elemente je Zeile finden
                 foo        <- strsplit(input.sel," +")                         ### und die fehlenden Elemente stets ans Ende setzen. Fatal!
                 maxColumns <- max(sapply(foo, FUN=function(ii){ length(ii)}))  ### Gefahr 2: '*' bezeichnet fixierte Parameter, die keinen Standardfehlöer haben. Manchmal steht aber trotzdem einer da (z.B. in DIF). Ersetzung soll nur stattfinden, wenn mehr als vier Leerzeichen hinterher
                 nDifferentColumns <- length( table(sapply(foo, FUN=function(ii){ length(ii)  })))
                 maxColumns <- which( sapply(foo, FUN=function(ii){ length(ii) == maxColumns  }) ) [1]
                 ### untere Zeile: WICHTIG! wo stehen in der Zeile mit den meisten nicht fehlenden Werten Leerzeichen?
                 foo.2      <- which( sapply(1:nchar(input.sel[maxColumns]),FUN=function(ii){u <- substr(input.sel[maxColumns],ii,ii); b <- u==" "  }) )
                 foo.3      <- diff(foo.2)                                      ### zeige die Position des letzten Leerzeichens vor einem Nicht-Leerzeichen
                 foo.3      <- foo.2[foo.3 !=1]                                 ### suche nun in jeder Zeile von input.sel: ist das Zeichen zwei Stellen nach foo.3 ein Leerzeichen? Wenn ja: NA!
                 ESTIMATE   <- which( sapply(1:nchar(input.all[ind[i] + 4] ),FUN=function(ii){u <- substr(input.all[ind[i] + 4],ii,ii+7); b <- u=="ESTIMATE"  }) )
                 foo.3      <- foo.3[foo.3>(ESTIMATE-3)]                        ### Achtung: das alles soll aber nur für Spalten beginnen, die hinter ESTIMATE stehen! (mißrät sonst für Produktterme, z.B. "item*sex")
                 if(nDifferentColumns>1) {
                    if(length(foo.3)>0) {                                       ### Und nochmal: das soll NUR geschehen, wenn es in mindestens einer Zeile nicht die vollständige (=maximale) Anzahl von Elementen gibt!
                       for (ii in 1:length(input.sel)) {                        ### also wenn nDifferentColumns größer als EINS ist (kleiner darf es nicht sein)
                            for (iii in 1:length(foo.3)) {
                                 if(substr( input.sel[ii], foo.3[iii] + 2 , foo.3[iii] + 2 ) == " ") {input.sel[ii] <- paste(substr(input.sel[ii],1,foo.3[iii]), "NA", substring(input.sel[ii],foo.3[iii]+3) , sep="")}}}}
                    if(length(foo.3)==0) {cat(paste("There seem to be no values in any columns behind 'ESTIMATE'. Check outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))}}
                 input.sel <- strsplit(input.sel," +")
                 if(length(input.sel[[1]]) == 0 ) {cat(paste("There seem to be no valid values associated with term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                                                   all.terms <- all.terms[-i]}
                 if(length(input.sel[[1]]) > 0 ) {
                    referenzlaenge <- max (sapply( input.sel, FUN=function(ii ){  length(ii)    }) )
                    if(referenzlaenge < length(namen) ) {cat(paste("Several columns seem to be empty for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \nOutputfile may be corrupted. Please check!\n",sep=""))
                                                         referenzlaenge <- length(namen)}
                    if(referenzlaenge > length(namen) ) {
                       if(referenzlaenge == length(namen) + 1) {
                          cat(paste("There seem to be one more column than columns names. Expect missing column name before 'ESTIMATE'. \nCheck outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                          ind.name <- which(namen == "ESTIMATE")
                          namen    <- c(namen[1:ind.name-1], "add.column",namen[ind.name:length(namen)])}
                       if(referenzlaenge >  length(namen) + 1) {
                          cat(paste("There seem to be more columns than names for it. Check outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                          namen <- c(namen, rep("add.column",referenzlaenge-length(namen) )) }}
                    input.sel <- t(sapply(input.sel, FUN=function(ii){ c(ii, rep(NA,referenzlaenge-length(ii))) }))
                    colnames(input.sel) <- namen                                ### untere Zeile: entferne eventuelle Sternchen und wandle in Dataframe um!
                    input.sel <- as.numeric.if.possible(data.frame( gsub("\\*","",input.sel), stringsAsFactors = FALSE), set.numeric = TRUE, verbose = FALSE)
                    results.sel <- data.frame(input.sel,filename=file,stringsAsFactors = FALSE)
                    if(is.na(as.numeric(results.sel$ESTIMATE[1]))) {cat(paste("'ESTIMATE' column in Outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"' does not seem to be a numeric value. Please check!\n",sep=""))}
                    if(!missing(dif.term)) {                                    ### Der absolute DIF-Wert ist 2 * "Betrag des Gruppenunterschieds". Fuer DIF muessen ZWEI Kriterien erfuellt sein:
                       if(all.terms[length(all.terms)] == dif.term) {           ### Der absolute DIF-Wert muss groesser als 'abs.dif.bound' (z.B. 0.6) und zugleich signifikant groesser als 'sig.dif.bound' (z.B. 0.3) sein
                          cat(paste("Treat '",all.terms[length(all.terms)],"' as DIF TERM.\n",sep=""))
                          results.sel <- data.frame(results.sel,abs.dif = 2*results.sel$ESTIMATE,stringsAsFactors=FALSE)
                          konfNiveau  <- round(100*p.value)                     ### Das bedeutet, für Werte groesser 0.6 darf 0.3 NICHT im 90 bzw. 95%-Konfidenzintervall liegen. Nur dann haben wir DIF!
                          results.sel[,paste("KI.",konfNiveau,".u",sep="")] <- results.sel$abs.dif-2*abs(qnorm(0.5*(1-p.value)))*results.sel$ERROR  
                          results.sel[,paste("KI.",konfNiveau,".o",sep="")] <- results.sel$abs.dif+2*abs(qnorm(0.5*(1-p.value)))*results.sel$ERROR  
                          results.sel[,paste("sig.",konfNiveau,sep="")] <- ifelse(abs(results.sel[,"abs.dif"])>abs.dif.bound & abs(results.sel[,paste("KI.",konfNiveau,".u",sep="")])>sig.dif.bound & abs(results.sel[,paste("KI.",konfNiveau,".o",sep="")])>sig.dif.bound,1,0)
                          results.sel$filename <- file
                          if(split.dif==TRUE) {results.sel <- results.sel[1:(dim(results.sel)[1]/2),]
                                               if(dim(results.sel)[1]!=dim(results.sel)[1]) {cat("Warning: missing variables in DIF table.\n")}}}}
                 all.output[[i]] <- results.sel}}
              if(!missing(dif.term)) {if(sum(all.terms==dif.term)==0) {cat(paste("Term declarated as DIF: '",dif.term,"' was not found in file: '",file,"'. \n",sep=""))  }}
              names(all.output) <- all.terms
              ### ggf. Regressionsparameter einlesen!
            	regrStart <- grep("REGRESSION COEFFICIENTS", input.all) + 2
              isRegression <- length(regrStart) > 0
            	if ( isRegression)   {
                  regrEnd <- grep("An asterisk next", input.all)
              		regrEnd <- regrEnd[which(regrEnd > regrStart)][1] - 2
              		regrInput <- crop(input.all[regrStart:regrEnd])
              		zeileDimensions <- grep("Regression Variable",input.all)
                  stopifnot(length(zeileDimensions) ==1)
              		nameDimensions  <- unlist(strsplit(input.all[zeileDimensions], "  +"))[-1]
              		regrRows <- grep("CONSTANT",input.all)
                  regrRows <- regrRows[regrRows<=regrEnd][1]
              		regrNamen <- unlist(lapply(strsplit(input.all[regrRows:regrEnd],"  +"), FUN=function(ii) {unlist(ii)[1]} ))
                  regrInputSel <- crop(input.all[regrRows:regrEnd])
                  regrInputSel <- gsub("\\(","",regrInputSel)
              		regrInputSel <- gsub(")","",regrInputSel)
              		regrInputSel <- gsub("\\*","  NA",regrInputSel)
              		regrInputSel <- unlist( strsplit(regrInputSel," +") )
              		nDimensions  <- (length(  regrInputSel ) / length(regrNamen) - 1 )/2
                  cat(paste("Finde ",nDimensions," Dimension(en): ",paste(nameDimensions,collapse=", "),"\n",sep=""))
                  cat(paste("Finde ",length(regrNamen)-1," Regressor(en).\n",sep=""))
                  regrInputSel <- data.frame(matrix(regrInputSel, ncol=2*nDimensions+1, byrow=T),stringsAsFactors=F)
                  for (ii in 2:ncol(regrInputSel))  {regrInputSel[,ii] <- as.numeric(regrInputSel[,ii])}
                  colnames(regrInputSel) <- c("reg.var", paste(rep(c("coef","error"),nDimensions), rep(nameDimensions,each=2),sep="_") )
                  regrInputSel$filename <- file
              		all.output$regression <- regrInputSel
              }
              ### Kovarianz-/ Korrelationsmatrix einlesen: schwierig, also Trennen nach ein- vs. mehrdimensional. Eindimensional: zweimal "-----" zwischen Beginn und Ende des COVARIANCE-Statements
              korStart <- grep("COVARIANCE/CORRELATION MATRIX", input.all)
              korEnd   <- grep("An asterisk next", input.all)
              korEnd   <- min(korEnd[korEnd > korStart])
              korStriche <- grep("-----",input.all)
              korStriche <- korStriche[korStriche > korStart & korStriche < korEnd]
              if(length(korStriche) == 2) {                                     ### eindimensional!
                 varRow    <- grep("Variance", input.all)
                 variance  <- as.numeric( unlist( lapply(strsplit(input.all[varRow]," +"), FUN=function(ll) {ll[length(ll)]}) ) )
                 names(variance) <- "variance"
                 all.output$cov.structure <- variance
              }
              if(length(korStriche) > 2) {                                      ### mehrdimensional!
                 bereich     <- input.all[ (min(korStriche) + 1) : (max(korStriche) - 1 ) ]
                 bereich     <- bereich[ -grep("----",bereich)]
                 bereich     <- strsplit(crop(bereich),"  +")
                 for (ii in 2:(length(bereich)-1) )  {
                     if(ii <= length(bereich[[ii]]) )  {
                        bereich[[ii]] <- c(bereich[[ii]][1:(ii-1)], NA, bereich[[ii]][ii:length(bereich[[ii]])])
                     }
                     if(ii > length(bereich[[ii]]) )  {
                        bereich[[ii]] <- c(bereich[[ii]][1:(ii-1)], NA)
                     }
                 }
                 bereich.data.frame <- as.numeric.if.possible(data.frame(do.call("rbind", bereich[-1]),stringsAsFactors=FALSE), verbose = FALSE)
                 colnames(bereich.data.frame) <- bereich[[1]]
                 all.output$cov.structure <- bereich.data.frame
              }
            all.output$final.deviance <- rowToFind
            options(warn = 0)                                                   ### warnungen wieder an
            return(all.output)}
                 
                 
get.prm <- function(file)   {
            input <- scan(file,what="character",sep="\n",quiet=TRUE)
            input <- strsplit( gsub("\\\t"," ",crop(input)), "/\\*")            ### Hier ist es wichtig, gsub() anstelle von sub() zu verwenden! sub() loescht nur das erste Tabulatorzeichen
            ret   <- data.frame ( do.call("rbind", strsplit( crop(unlist(lapply(input, FUN = function ( l ) {l[1]}))), " +")), stringsAsFactors = FALSE)
            nameI <- crop(remove.pattern ( crop( crop(unlist(lapply(input, FUN = function ( l ) {l[length(l)]}))), char = "item"), pattern = "\\*/"))
            ret   <- data.frame ( Case= as.numeric(ret[,1]), item = nameI, parameter= as.numeric(ret[,2]) ,stringsAsFactors = FALSE)
            return(ret)}

get.itn <- function(file)  {
            input <- scan(file, what = "character", sep="\n", quiet = TRUE)
            ind.1 <- grep("==========",input)
            items <- grep( "item:", input )
            diff.last <- ind.1[length(ind.1)-1] - items[length(items)] + 4
            items <- cbind(1:length(items),items,c(diff(items),diff.last))      ### dort wo diff(items) != 13 , ist das entsprechende Item partial credit. (Fuer das letzte Item ist das komplizierter, da length(diff(items))<length(items).    )
            ind.2 <- gregexpr(":", input[items[,2]])                            ### Folgende Zeilen dienen dazu zu pruefen, ob DIFs in der Tabelle vorkommen oder nicht (falls ja, dann gibt es zwei Doppelpunkte pro input[items[,2]]
            ind.3 <- unlist(ind.2)                                              ### Dann ist ind.3 auch doppelt so lang wie ind.2, weil jedes Element aus ind.2 ein Vektor mit zwei Elementen ist
            ind.3 <- matrix(ind.3,length(ind.2),byrow=T)
            item.namen <- substr(input[items[,2]], ind.3[,dim(ind.3)[2]]+1+nchar(as.character(items[,1])),100)
            item.namen <- gsub(" ","",item.namen)                               ### Leider funktioniert gsub() nicht fuer Klammern, da diese fuer regular expression reserviert sind, aber...
            item.namen <- gsub("\\)","",item.namen); item.namen <- gsub("\\(","",item.namen)              
            if(dim(ind.3)[2]>1)                                                 ### kommen DIFs din vor? Ja, falls Bedingung TRUE
              {stopifnot(length(table(ind.3[,1]))==1)                           ### sollte 1 sein; da es immer dieselbe DIF-Variable mit ergo derselben Zeichenlaenge ist.
               dif.name <- rep(substr(input[items[,2]], 1, ind.3[,1]-1),(items[,3]-11))                          ### Auslesen der Variablennamen fuer DIF
               dif.value <- rep(as.numeric(substr(input[items[,2]], ind.3[,1]+1, ind.3[,1]+1)),(items[,3]-11))}  ### Auslesen des Wertes der DIF-Variablen
            zeilen <- list(); reihe <- NULL                                     ### Was geschieht oben? Die DIF-Variable wird fuer Item repetiert, und zwar zweimal, wenn es ein normales, dreimal, wenn es ein partial credit-Item ist. Die entsprechende Information steht in items[,3]; vgl.: rep(1:4,1:4)
            for (i in 1:dim(items)[1])                                          ### finde die Zeilen fuer jedes Item
                {zeilen[[i]] <- (items[i,2]+7) : (items[i,2]+ (items[i,3]-5) )  ### kein partial credit: beginne sieben Zeilen unter "item:" und ende bei acht Zeilen (= 13-5) unter "item:". Fuer partial credit, ende items[i,3]-5 Zeilen unter "items:"
                 cases       <- gsub("NA ","NA",input[zeilen[[i]]])             ### Untere Zeile: Korrektur, wenn die zwei Datenzeilen leere felder enthalten (NA wird nachtraeglich eingetragen)
                 cases <- gsub("_BIG_ ","NA",cases)
                 cases <- gsub("_BIG_","NA",cases)
                 if(length(table(sapply(1:length(cases),FUN=function(ii){length(unlist(strsplit(cases[ii]," +"))) }) ) )>1 )
                   {cases <- gsub("          ","    NA    ",cases)}             ### Perfekt! ueberall dort, wo zehn Leerzeichen infolge stehen, muss eine Auslassung sein! Hier wird ein Ersetzung gemacht!
                 cases       <- data.frame( matrix ( unlist( strsplit(crop(gsub(" +"," ", cases))," ") ), nrow=length(zeilen[[i]]),byrow=TRUE ) , stringsAsFactors=FALSE)
                 ind         <- grep("\\)",cases[1,]); cases[,ind] <- gsub("\\)","",cases[,ind] )
                 cases       <- data.frame(cases[,1:(ind-1)],matrix(unlist(strsplit(cases[,6],"\\(")),nrow=length(zeilen[[i]]),byrow=T),cases[,-c(1:ind)],stringsAsFactors=F)
                 for(jj in 1:ncol(cases)) {cases[,jj] <- as.numeric(cases[,jj])}
                 colnames(cases) <- c("Label","Score","Abs.Freq","Rel.Freq","pt.bis","t.value","p.value",paste(rep(c("PV1.Avg.","PV1.SD."),((ncol(cases)-7)/2) ),rep(1:((ncol(cases)-7)/2),each=2),sep=""))
                 threshold.zeile   <- input[items[i,2]+2]; threshold <- NULL; delta <- NULL
                 bereich <- ifelse( (items[i,3]-12)<1,1,(items[i,3]-12))        ### Sicherheitsbedingung, falls Variable nur eine Kategorie hat
                 if((items[i,3]-12)<1) {cat(paste("Item",i,"hat nur eine Antwortkategorie.\n"))}
                 for (j in 1: bereich )
                     {threshold  <- c(threshold ,as.numeric(substr(threshold.zeile,  6*j+16,6*j+21)))
                      delta      <- c(delta,     as.numeric(substr(input[items[i,2]+3],6*j+13,6*j+18)))}
                 while(length(threshold) < nrow(cases)) {threshold <- c(threshold,NA)}
                 while(length(delta) < nrow(cases)) {delta <- c(delta,NA)}
                 item.p <- NA                                                   ### Manchmal kann kein p-wert bestimmt werden. Wenn doch, wird das NA ueberschrieben
                 valid.p <- which(is.na(cases$Score))
                 if(length(valid.p) == 0)
                    {item.p <- cases[which(cases$Score == max(cases$Score)),"Abs.Freq"] / sum(cases$Abs.Freq)}
                 sub.reihe   <- data.frame(item.nr=i, item.name=item.namen[i], cases[,1:2], n.valid = sum(cases$Abs.Freq), cases[,3:4], item.p = item.p, diskrim=as.numeric(substr(input[items[i,2]+1],45,55)),cases[,-c(1:4)], threshold, delta, stringsAsFactors=F)
                 reihe <- rbind(reihe,sub.reihe)}
             if(dim(ind.3)[2]>1)
               {reihe <- data.frame(dif.name,dif.value,reihe,stringsAsFactors=FALSE)}
             return(reihe)}
                 
get.dsc <- function(file) {
            input     <- scan(file,what="character",sep="\n",quiet=TRUE)
            n.gruppen    <- grep("Group: ",input)
            gruppennamen <- unlist( lapply( strsplit(input[n.gruppen]," ") , function(ll) {paste(ll[-1],collapse=" ")} ) )
            cat(paste("Found ",length(n.gruppen)," group(s) in ",file,".\n",sep=""))
            trenner.1 <- grep("------------------",input)
            trenner.2 <- grep("\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.",input)
            stopifnot(length(trenner.1) == length(trenner.2))
            daten     <- lapply(1:(length(trenner.1)/2), FUN=function(ii) {
                 dat <- strsplit(input[(trenner.1[2*ii]+1):(trenner.2[2*ii-1]-1)]," +")
                 dat <- data.frame(matrix(unlist(lapply(dat, FUN=function(iii) {  c(paste(iii[1:(length(iii)-4)],collapse=" "),iii[-c(1:(length(iii)-4))])  })), ncol=5,byrow=T) , stringsAsFactors=F)
                 dat <- data.frame(group.name = gruppennamen[ii], dat, stringsAsFactors = FALSE)
                 colnames(dat) <- c("group.name","dimension","N","mean","std.dev","variance")
                 for (iii in 3:ncol(dat)) {dat[,iii] <- as.numeric(dat[,iii])}
                 desc <- strsplit(input[(trenner.2[2*ii-1]+1):(trenner.2[2*ii]-1)]," +")
                 desc <- data.frame(matrix(unlist(lapply(desc, FUN=function(iii) {  c(paste(iii[1:(length(iii)-3)],collapse=" "),iii[-c(1:(length(iii)-3))])  })), ncol=4,byrow=T) , stringsAsFactors=F)
                 colnames(desc) <- c("dimension","mean","std.dev","variance")
                 for (iii in 2:ncol(desc)) {desc[,iii] <- as.numeric(desc[,iii])}
                 dat.list <- list( single.values=dat, aggregates=desc)
                 return(dat.list) } )
            names(daten) <- gruppennamen
            n.dim        <- names(table(unlist(lapply(1:length(daten), FUN=function(ii) {length( grep("Error", daten[[ii]]$aggregates$dimension))}) ) ))
            stopifnot(length(n.dim)==1)
            cat(paste("Found ",n.dim," dimension(s) in ",file,".\n",sep=""))
            return(daten)}
                 
get.equ <- function(file)  {
            input       <- scan(file,what="character",sep="\n",quiet = TRUE)
            dimensionen <- grep("Equivalence Table for",input)
            cat(paste("Finde ",length(dimensionen), " Dimension(en).\n",sep=""))
            ende        <- grep("================",input)
            ende        <- sapply(dimensionen, FUN=function(ii) {ende[ende>ii][1]})
            tabellen    <- lapply(1:length(dimensionen), FUN=function(ii)
                           {part <- crop(input[(dimensionen[ii]+6):(ende[ii]-1)])
                            part <- data.frame(matrix(as.numeric(unlist(strsplit(part," +"))),ncol=3,byrow = TRUE),stringsAsFactors = FALSE)
                            colnames(part) <- c("Score","Estimate","std.error")
                            return(part)})
            regr.model  <- grep("The regression model",input)
            item.model  <- grep("The item model",input)
            stopifnot(length(regr.model) == length(item.model))
            name.dimensionen <- unlist( lapply(dimensionen,FUN=function(ii) {unlist(lapply(strsplit(input[ii], "\\(|)"),FUN=function(iii){iii[length(iii)]}))}) )
            model       <- lapply(1:length(regr.model), FUN=function(ii) {rbind ( crop(gsub("The regression model:","",input[regr.model[ii]])), crop(gsub("The item model:","",input[item.model[ii]])) ) })
            model       <- do.call("data.frame",args=list(model,row.names=c("regression.model","item.model"),stringsAsFactors = FALSE))
            colnames(model) <- name.dimensionen
            tabellen$model.specs <- model
            names(tabellen)[1:length(dimensionen)] <- name.dimensionen
            return(tabellen)}
                 

### as.numeric(remove.non.numeric) gives "NA" instead of empty "" in case of no numbers in n-th string
### Bsp.: remove.non.numeric(c("5","  h89 kj.9","2-4h","aags"))
remove.non.numeric <- function(string)
                      {if(!is.null(dim(string))) {dimension <- dim(string)}
                       splitt <- strsplit(string,"")
                       options(warn = -1)                                       ### warnungen aus
                       splitt <- unlist ( lapply(splitt, FUN=function(ii) {paste( na.omit(as.numeric(ii)),collapse="")}))
                       options(warn = 0)                                        ### warnungen wieder an
                       if(!is.null(dim(string))) {splitt <- matrix(splitt,dimension[1],dimension[2],byrow = FALSE)}
                       return(splitt)}


remove.numeric <- function(string)
                      {if(!is.null(dim(string))) {dimension <- dim(string)}
                       splitt <- strsplit(string,"")
                       options(warn = -1)                                       ### warnungen aus
                       splitt <- lapply(splitt, FUN=function(ii) {
                                 a         <- as.numeric(ii)
                                 change    <- which(!is.na(a))
                                 if(length(change)>0) {ii[change] <- ""}
                                 ii        <- paste(ii, collapse="")
                                 return(ii)
                       })
                       options(warn = 0)                                        ### warnungen wieder an
                       splitt <- unlist(splitt)
                       if(!is.null(dim(string))) {splitt <- matrix(splitt,dimension[1],dimension[2],byrow = FALSE)}
                       return(splitt)}

crop <- function ( x , char = " " ) {
	if ( char %in% c ( "\\" , "+" , "*" , "." , "(" , ")" , "[" , "]" , "{" , "}" , "|" , "^" , "$" ) ) {char <- paste ( "\\" , char , sep = "" ) }
	gsub ( paste ( "^" , char , "+|" , char , "+$" , sep = "" ) , "" , x ) }

normalize.path <- function(string)
                  {string <- gsub("//","/",string)
                   string <- gsub("/","//",string)
                   string <- gsub("//","\\\\",string)
                   return(string)}
                 
gen.syntax     <- function(Name,daten, all.Names, namen.all.hg = NULL, all.hg.char = NULL, var.char, model = NULL, ANKER, constraints=c("cases","none","items"), pfad=NULL, Title=NULL,n.plausible=5,std.err=c("quick","full","none"), compute.fit ,
                           distribution=c("normal","discrete"), method=c("gauss", "quadrature", "montecarlo"), n.iterations=200, nodes=NULL, p.nodes=2000, f.nodes=2000, converge=0.001,deviancechange=0.0001, equivalence.table=c("wle","mle","NULL"), use.letters=use.letters, model.statement=model.statement, conquest.folder = NULL, allowAllScoresEverywhere,
                           seed , export = list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE) )  {
                   if(is.null(ANKER)) {ANKER <- FALSE} else {ANKER <- TRUE}
                   export.default <- list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE)
                   mustersyntax <- c("title = ####hier.title.einfuegen####;",
                   "export logfile >> ####hier.name.einfuegen####.log;",
                   "datafile ####hier.name.einfuegen####.dat;",
                   "Format pid ####hier.id.einfuegen####",
                   "group",
                   "codes ####hier.erlaubte.codes.einfuegen####;",
                   "labels  << ####hier.name.einfuegen####.lab;",
                   "import anchor_parameters << ####hier.name.einfuegen####.ank;",
                   "caseweight",
                   "set constraints=####hier.constraints.einfuegen####;",
                   "set warnings=no,update=yes,n_plausible=####hier.anzahl.pv.einfuegen####,p_nodes=####hier.anzahl.p.nodes.einfuegen####,f_nodes=####hier.anzahl.f.nodes.einfuegen####;",
                   "set seed=####hier.seed.einfuegen####;",
                   "export par    >> ####hier.name.einfuegen####.prm;",
                   "regression",
                   "model ####hier.model.statement.einfuegen####;",
                   "estimate ! fit=####hier.fitberechnen.einfuegen####,method=####hier.method.einfuegen####,iter=####hier.anzahl.iterations.einfuegen####,nodes=####hier.anzahl.nodes.einfuegen####,converge=####hier.converge.einfuegen####,deviancechange=####hier.deviancechange.einfuegen####,stderr=####hier.std.err.einfuegen####,distribution=####hier.distribution.einfuegen####;",
                   "Itanal >> ####hier.name.einfuegen####.itn;",
                   "show cases! estimates=latent >> ####hier.name.einfuegen####.pvl;",
                   "show cases! estimate=wle >> ####hier.name.einfuegen####.wle;",
                   "equivalence ####hier.equivalence.table.einfuegen#### >> ####hier.name.einfuegen####.equ;",
                   "show >> ####hier.name.einfuegen####.shw;",
                   "export history >> ####hier.name.einfuegen####.his;",
									 "export covariance >> ####hier.name.einfuegen####.cov;",
									 "export reg_coefficients >> ####hier.name.einfuegen####.reg;",
									 "export designmatrix >> ####hier.name.einfuegen####.mat;",
                   "put >> ####hier.name.einfuegen####.cqs;  /* export systemfile */",
                   "descriptives !estimates=pv >> ####hier.name.einfuegen####_pvl.dsc;",
                   "descriptives !estimates=wle >> ####hier.name.einfuegen####_wle.dsc;",
                   "quit;")
                   if(is.null(Title))   {                                       ### wenn kein Titel gesetzt, erstelle ihn aus Sys.getenv()
                      all.inf  <- Sys.getenv()
                      Title    <- paste("Analysis name: ",Name, ", User: ",all.inf["USERNAME"],", Computername: ",all.inf["COMPUTERNAME"],", ", R.version$version.string , ", Time: ",date(),sep="")}
                   converge <- paste("0",substring(as.character(converge+1),2),sep="")
                   deviancechange <- paste("0",substring(as.character(deviancechange+1),2),sep="")
                   syntax    <- gsub("####hier.title.einfuegen####",Title,mustersyntax)
                   if(is.null(n.plausible))   {n.plausible <- 0}  ; if(is.na(n.plausible))     {n.plausible <- 0}
                   if(n.plausible == 0 )     {                                  ### wenn Anzahl PVs = 0 oder NULL, loesche Statement; andernfalls: setze Anzahl zu ziehender PVs ein!
                      syntax    <- gsub("n_plausible=####hier.anzahl.pv.einfuegen####,","",syntax) } else {
                      syntax    <- gsub("####hier.anzahl.pv.einfuegen####",n.plausible,syntax)
                   }
                   syntax    <- gsub("####hier.name.einfuegen####",Name,syntax)
                   ID.char   <- max(as.numeric(names(table(nchar(daten[,"ID"])))))
                   syntax    <- gsub("####hier.id.einfuegen####",paste("1-",as.character(ID.char)," ",sep="" ) ,syntax)
                   syntax    <- gsub("####hier.anzahl.iterations.einfuegen####",n.iterations,syntax)
                   syntax    <- gsub("####hier.anzahl.p.nodes.einfuegen####",p.nodes,syntax)
                   syntax    <- gsub("####hier.anzahl.f.nodes.einfuegen####",f.nodes,syntax)
                   syntax    <- gsub("####hier.converge.einfuegen####",converge,syntax)
                   syntax    <- gsub("####hier.deviancechange.einfuegen####",deviancechange,syntax)
                   if(!is.null(seed)) {syntax    <- gsub("####hier.seed.einfuegen####",seed,syntax)}
                   syntax    <- gsub("####hier.constraints.einfuegen####",match.arg(constraints),syntax)
                   compute.fit  <- if(compute.fit == TRUE ) compute.fit <- "yes" else compute.fit <- "no"
                   syntax    <- gsub("####hier.fitberechnen.einfuegen####",compute.fit,syntax)
                   syntax    <- gsub("####hier.anzahl.nodes.einfuegen####",nodes,syntax)
                   syntax    <- gsub("####hier.std.err.einfuegen####",match.arg(std.err),syntax)
                   syntax    <- gsub("####hier.distribution.einfuegen####",match.arg(distribution),syntax)
                   syntax    <- gsub("####hier.equivalence.table.einfuegen####",match.arg(equivalence.table),syntax)
                   syntax    <- gsub("####hier.model.statement.einfuegen####",tolower(model.statement),syntax)
                   erlaubte.codes <- paste(gsub("_","",sort(gsub(" ","_",formatC(names(table.unlist(daten[, all.Names[["variablen"]], drop = FALSE ])),width=var.char)),decreasing=TRUE)),collapse=",")
                   syntax    <- gsub("####hier.erlaubte.codes.einfuegen####",erlaubte.codes, syntax )
                   ind       <- grep("Format pid",syntax)
                   beginn    <- NULL                                            ### setze "beginn" auf NULL. Wenn DIF-Variablen spezifiziert sind, wird "beginn" bereits
                   if(length(namen.all.hg)>0)    {                              ### untere Zeile: wieviele "character" haben Hintergrundvariablen?
                     all.hg.char.kontroll <- all.hg.char
                     all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(daten[,ii]))))})
                     stopifnot(all(all.hg.char == all.hg.char.kontroll))        ### Trage nun die Spalten in das Format-Statement ein: Fuer ALLE expliziten Variablen
                     for (ii in 1:length(namen.all.hg))  {
                          if(is.null(beginn)) {beginn <- ID.char+1}
                          ende   <- beginn-1+all.hg.char[ii]
                          if (beginn != ende) {syntax[ind] <- paste(syntax[ind],namen.all.hg[ii], " ", beginn,"-",ende," ",sep="")}
                          if (beginn == ende) {syntax[ind] <- paste(syntax[ind],namen.all.hg[ii], " ", beginn," ",sep="")}
                          beginn  <- ende+1 }
                   }
                   if(length(all.Names[["DIF.var"]])>0)   {                     ### in folgender Schleife überschrieben und dann in der Schleife "if(!is.null(HG.var))" ergänzt, nicht neu geschrieben
                      if(model.statement != "item") {
                        cat(paste("Caution! DIF variable was specified. Expected model statement is: 'item - ",tolower(all.Names[["DIF.var"]])," + item*",tolower(all.Names[["DIF.var"]]),"'.\n",sep=""))
                        cat(paste("However, '",tolower(model.statement),"' will used as 'model statement' to accomplish your will.\n",sep=""))
                      }
                      if(model.statement == "item") {
                         ind.model <- grep("model item", syntax)                ### Aendere model statement
                         stopifnot(length(ind.model)==1)
                         syntax[ind.model] <- paste("model item - ",paste(tolower(all.Names[["DIF.var"]]),collapse=" - ") ," + ", paste("item*",tolower(all.Names[["DIF.var"]]),collapse=" + "), ";",sep="")
                      }
                   }
                   if(length(all.Names[["HG.var"]])>0)  {
                      ind.2   <- grep("^regression$",syntax)
                      syntax[ind.2] <- paste(crop(paste( c(syntax[ind.2], tolower(all.Names[["HG.var"]])), collapse=" ")),";",sep="")
                      if(method == "gauss") {cat("Warning: Gaussian quadrature is only available for models without latent regressors.\n")
                                             cat("         Use 'Bock-Aiken quadrature' for estimation.\n")
                                             method <- "quadrature"} }          ### method muß "quadrature" oder "montecarlo" sein
                   syntax    <- gsub("####hier.method.einfuegen####",method,syntax)
                   if(length(all.Names[["weight.var"]])>0)  {                   ### Method wird erst hier gesetzt, weil sie davon abhaengt, ob es ein HG-Modell gibt
                      ind.4   <- grep("caseweight",syntax)
                      syntax[ind.4] <- paste( syntax[ind.4], " ", tolower(all.Names[["weight.var"]]),";",sep="") }
                   if(length(all.Names[["group.var"]])>0) {
                       ind.3   <- grep("^group$",syntax)
                       stopifnot(length(ind.3) == 1)
                       syntax[ind.3] <- paste(crop(paste( c(syntax[ind.3], tolower(all.Names[["group.var"]])), collapse=" ")),";",sep="")
                       ### gebe gruppenspezifische Descriptives
                       add.syntax.pv  <- as.vector(sapply(all.Names[["group.var"]], FUN=function(ii) {paste("descriptives !estimates=pv, group=",tolower(ii)," >> ", Name,"_",tolower(ii),"_pvl.dsc;",sep="")} ))
                       add.syntax.wle <- as.vector(sapply(all.Names[["group.var"]], FUN=function(ii) {paste("descriptives !estimates=wle, group=",tolower(ii)," >> ", Name,"_",tolower(ii),"_wle.dsc;",sep="")} ))
                       ind.3    <- grep("quit",syntax)
                       stopifnot(length(ind.3)==1)
                       syntax   <- c(syntax[1:(ind.3-1)],add.syntax.pv, add.syntax.wle, syntax[ind.3:length(syntax)]) }
                   if(is.null(beginn)) {beginn <- ID.char+1}
                   syntax[ind] <- paste(syntax[ind], "responses ",beginn,"-",beginn-1+var.char*ncol(data.frame(daten[,all.Names[["variablen"]]],stringsAsFactors = FALSE)),";",sep="")
                   if(var.char>1)  {                                            ### Items haben mehr als eine Spalte Stelligkeit (Conquest-Handbuch, S.177)
                      syntax[ind] <- paste(gsub(";","",syntax[ind]), " (a",var.char,");",sep="")}
                   score.statement <- .writeScoreStatementMultidim (data=daten, itemCols=all.Names[["variablen"]], qmatrix=model, columnItemNames = 1 ,use.letters=use.letters, allowAllScoresEverywhere = allowAllScoresEverywhere )
                   expected.nodes  <- nodes^(ncol(model)-1)
                   if(expected.nodes>3500 & method != "montecarlo") {cat(paste("Specified model probably will use ",expected.nodes," nodes. Choosen method ",method," may not appropriate. Recommend to use 'montecarlo' instead.\n",sep=""))}
                   ind <- grep("labels ",syntax)
                   stopifnot(length(ind)==1)
                   syntax <- c(syntax[1:ind],score.statement,syntax[(ind+1):length(syntax)])
                   if(length(all.Names[["HG.var"]])==0) {                       ### wenn kein HG-model, loesche entsprechende Syntaxzeilen
                      ind.2 <- grep("^regression$",syntax)
                      stopifnot(length(ind.2)==1)
                      syntax <- syntax[-ind.2]
                      ind.3 <- grep("export reg_coefficients",syntax)
                      stopifnot(length(ind.3)==1)
                      syntax <- syntax[-ind.3] }
                   if(length(all.Names[["group.var"]]) ==0) {                   ### wenn keine Gruppen definiert, loesche Statement
                      ind.3 <- grep("^group$",syntax)
                      stopifnot(length(ind.3)==1)
                      syntax <- syntax[-ind.3]}
                   if(length(all.Names[["weight.var"]]) ==0) {                  ### wenn keine Gewichte definiert, loesche Statement
                      ind.4 <- grep("^caseweight$",syntax)
                      stopifnot(length(ind.4)==1)
                      syntax <- syntax[-ind.4]}
                   if(match.arg(equivalence.table) == "NULL") {                 ### wenn keine Equivalence-Statement definiert, loesche Zeile
                      ind.5   <- grep("^equivalence",syntax)
                      stopifnot(length(ind.5)==1)
                      syntax <- syntax[-ind.5]}
                   if(is.null(seed)) {                                          ### wenn keine seed-Statement definiert, loesche Zeile
                      ind.7   <- grep("^set seed",syntax)
                      stopifnot(length(ind.7)==1)
                      syntax <- syntax[-ind.7]}
                   if(n.plausible == 0)     {                                   ### wenn Anzahl PVs = 0 oder NULL, loesche Statement
                      ind.6   <- grep("^show cases! estimates=latent", syntax)
                      stopifnot(length(ind.6) == 1)
                      syntax  <- syntax[-ind.6]}
                   if(ANKER == FALSE)  {ind.2 <- grep("anchor_parameter",syntax)### wenn keine ANKER gesetzt, loesche entsprechende Syntaxzeile
                                        syntax <- syntax[-ind.2]}
                   if(ANKER == TRUE)   {ind.2 <- grep("^set constraints",syntax)### wenn ANKER gesetzt, setze constraints auf "none"
                                        if(match.arg(constraints) != "none") { cat("Anchorparameter were defined. Set constraints to 'none'.\n")}
                                        syntax[ind.2]  <- "set constraints=none;"}
                   classes.export <- sapply(export, FUN = function(ii) {class(ii)})
                   if(!all(classes.export == "logical"))  {stop("All list elements of argument 'export' have to be of class 'logical'.\n")}
                   export <- as.list(userSpecifiedList ( l = export, l.default = export.default ))
                   weg <- names(export[which(export == FALSE)])
                   if(length(weg)>0)    {                                       ### hier wird, was nicht exportiert werden soll, aus Syntax geloescht.
                      for (ii in seq(along=weg) ) {
                           ind.x <- grep(paste("export ", weg[ii], sep=""), syntax)
                           stopifnot(length(ind.x) == 1)
                           syntax <- syntax[-ind.x]}}
                   if(export["history"] == TRUE)  {
                      if(!is.null(conquest.folder))  {
                         cq.version <- getConquestVersion( path.conquest = conquest.folder, path.temp = pfad)
                         if(cq.version < as.date("1Jan2007") )   {
   									      ind.3 <- grep("^export history",syntax)               ### wenn Conquest aelter als 2007, soll history geloescht werden,
                           stopifnot(length(ind.3) == 1 )                       ### auch dann, wenn der Benutzer History ausgeben will
                           syntax <- syntax[-ind.3]
                         }
                      }
                      if(is.null(conquest.folder)) {cat("Warning! Conquest folder was not specified. Unable to detect Conquest version. When you propose to use 2005 version,\nhistory statement will invoke to crash Conquest analysis. Please remove history statement manually if you work with 2005 version.\n")} }
                   write(syntax,file.path(pfad,paste(Name,".cqc",sep="")),sep="\n")}
                 
anker <- function(lab, prm )  {                                
                  stopifnot(ncol(prm)==2); stopifnot(ncol(lab)==2)
                  colnames(prm) <- c("item","parameter")
                  ind <- intersect(lab[,"item"],prm[,"item"])
                  if(length(ind) == 0) {stop("No common items found in 'lab.file' and 'prm.file'.\n")}
                  if(length(ind) > 0)  {cat(paste(length(ind), " common items found in 'lab.file' and 'prm.file'.\n",sep="")) }
                  res <- merge(lab, prm, by = "item", sort = FALSE, all = FALSE)
                  res <- data.frame(res[sort(res[,2],decreasing=FALSE,index.return=TRUE)$ix,], stringsAsFactors = FALSE)[,-1]
                  stopifnot(nrow(res) == length(ind))
                  return(res)}
                 
remove.pattern     <- function ( string, pattern ) {
                      splitt <- strsplit(string, pattern)
                      ret    <- unlist(lapply(splitt, FUN = function ( y ) { paste(y, collapse="")}))
                      return(ret)}

isLetter <- function ( string ) { 
            splt <- strsplit(string, "")
            isL  <- lapply(splt, FUN = function ( x ) { 
                    ind <- which ( x %in% c( letters , LETTERS )) 
                    x[setdiff(1:length(x),ind)] <- " " 
                    x <- crop(paste(x, sep="", collapse=""))
                    x <- unlist ( strsplit(x, " +") ) 
                    return(x)  } )
            return(isL)}        


facToChar <- function ( dataFrame, from = "factor", to = "character" ) {
             if(!"data.frame" %in% class(dataFrame)) {stop()}
             classes <- which( unlist(lapply(dataFrame,class)) == from)
             if(length(classes)>0) {
                for (u in classes) { eval(parse(text=paste("dataFrame[,u] <- as.",to,"(dataFrame[,u])",sep="") )) }}
             return(dataFrame)}
                 
                 
.writeScoreStatementMultidim <- function(data, itemCols, qmatrix, columnItemNames = 1 ,columnsDimensions = -1, use.letters=use.letters , allowAllScoresEverywhere) {
            n.dim      <- (1:ncol(qmatrix) )[-columnItemNames]                  ### diese Spalten bezeichnen Dimensionen. untere Zeile: Items, die auf keiner Dimension laden, werden bereits in prep.conquest entfernt. hier nur check
            stopifnot(length( which( rowSums(qmatrix[,n.dim,drop = FALSE]) == 0))==0)
      	    if(length(setdiff(names(table.unlist(qmatrix[,-1, drop = FALSE])), c("0","1"))) > 0 )  {
               cat("Found unequal factor loadings for at least one dimension. This will result in a 2PL model.\n")
               for (u in 2:ncol(qmatrix)) {qmatrix[,u] <- as.character(round(qmatrix[,u], digits = 3))}
            }                                                                   ### obere Zeile: Identifiziere Items mit Trennschärfe ungleich 1.
            stopifnot(all(qmatrix[,1] == itemCols))                             ### untere Zeile: Items im Datensatz, aber nicht in Q-Matrix? wird bereits in prep.conquest behandelt
            cat(paste("Q matrix specifies ",length(n.dim)," dimension(s).\n",sep=""))
            stopifnot(length(setdiff(colnames(data[,itemCols]),  qmatrix[,columnItemNames]) )==0)
            unique.patter <- qmatrix[which(!duplicated(do.call("paste", qmatrix[,-1, drop = FALSE] ))), -1, drop = FALSE]
            colnames(unique.patter) <- paste("Var",1:ncol(unique.patter), sep="")## obere Zeile: Finde alle uniquen Pattern in qmatrix! Jedes unique Pattern muss in Conquest einzeln adressiert werden!
            score.matrix  <- data.frame(score=1, unique.patter, matrix(NA, nrow= nrow(unique.patter), ncol=length(itemCols), dimnames=list(NULL, paste("X",1:length(itemCols),sep=""))),stringsAsFactors = FALSE)
            scoreColumns <- grep("^Var",colnames(score.matrix))
            for (i in 1:length(itemCols))  {                                    ### gebe alle Items auf den jeweiligen Dimensionen
               qmatrix.i    <- qmatrix[qmatrix[,columnItemNames] == itemCols[i],]## auf welcher Dimension laedt Variable i? Untere Zeile: in diese Zeile von score.matrix muß ich variable i eintragen
               matchRow     <- which(sapply ( 1:nrow(score.matrix) , function(ii) {all ( as.numeric(qmatrix.i[,n.dim]) == as.numeric(score.matrix[ii,scoreColumns])) }))
               stopifnot(length(matchRow) == 1)
               matchColumn  <- min(which(is.na(score.matrix[matchRow,])))       ### in welche spalte von Score.matrix muß ich variable i eintragen?
               stopifnot(length(matchColumn) == 1)
               score.matrix[matchRow,matchColumn] <- i
		        }
            rowsToDelete <- which(is.na(score.matrix[, max(scoreColumns) + 1])) ### welche Zeilen in Score.matrix koennen geloescht werden?
            if(length(rowsToDelete)>0) {score.matrix <- score.matrix[-rowsToDelete, ]}
            for (ii in 1:nrow(score.matrix)) {score.matrix[,ii] <- as.character(score.matrix[,ii])}
            score.matrix <- fromMinToMax(dat = data[,itemCols, drop = FALSE], score.matrix = score.matrix, qmatrix = qmatrix, allowAllScoresEverywhere = allowAllScoresEverywhere, use.letters = use.letters)
            kollapse <- lapply(1:nrow(score.matrix), FUN=function(ii) {na.omit(as.numeric(score.matrix[ii,-c(1,scoreColumns)]))})
            kollapse.diff   <- lapply(kollapse,FUN=function(ii) {c(diff(ii),1000)})
            kollapse.ascend <- lapply(kollapse.diff, FUN=function(ii) {unique(c(0, which(ii!=1)))})
            kollapse.string <- list()
            for (a in 1:length(kollapse.ascend))  {
                string   <- list()
                for (i in 2:length(kollapse.ascend[[a]]))   {
                    string.i <- unique( c(kollapse[[a]][kollapse.ascend[[a]][i-1]+1], kollapse[[a]][kollapse.ascend[[a]][i]]))
                    string.i <- ifelse(length(string.i) == 2,paste(string.i[1],"-",string.i[2],sep=""),as.character(string.i))
                    string[[i]] <- string.i
				        }
                string <- paste(unlist(string),collapse=", ")
                kollapse.string[[a]] <- string
			      }
            ### Prüfung, ob "tranformation" des score-statements ok ist
            control <- lapply(kollapse.string,FUN=function(ii) {eval(parse(text=paste("c(",gsub("-",":",ii),")",sep="")))})
            if (!all(unlist(lapply(1:length(control), FUN=function(ii) {all(kollapse[[ii]] == control[[ii]])})))) {
                cat("Error in creating score statement.\n")
			      }
            score.matrix <- data.frame(prefix="score",score.matrix[,c(1,scoreColumns)],items="! items(",kollapse.string=unlist(kollapse.string),suffix=");",stringsAsFactors=F)
            score.statement <- sapply(1:nrow(score.matrix), FUN=function(ii) { paste(score.matrix[ii,],collapse=" ")})
            return(score.statement) }
                 
fromMinToMax <- function(dat, score.matrix, qmatrix, allowAllScoresEverywhere, use.letters)    {
                all.values <- alply(as.matrix(score.matrix), .margins = 1, .fun = function(ii) {names(table.unlist(dat[,na.omit(as.numeric(ii[grep("^X", names(ii))])), drop = FALSE]))  })
                if ( allowAllScoresEverywhere == TRUE ) {                       ### obere Zeile: WICHTIG: "alply" ersetzt "apply"! http://stackoverflow.com/questions/6241236/force-apply-to-return-a-list
                    all.values <- lapply(all.values, FUN = function(ii) {sort(as.numeric.if.possible(unique( unlist ( all.values ) ), verbose = FALSE ) ) } )
                }
                if(use.letters == TRUE )  {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(LETTERS[which(LETTERS == ii[1]) : which(LETTERS == ii[length(ii)])], collapse=" "),")") } ) ) }
                if(use.letters == FALSE ) {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(ii[1] : ii[length(ii)],collapse = " "),")")  } ) ) }
                scoring <- unlist( lapply( minMaxRawdata , FUN = function(ii) { paste("(", paste( 0 : (length(unlist(strsplit(ii, " ")))-3), collapse = " "),")")}) )
                stopifnot(length(scoring) == length( minMaxRawdata ) )
                stopifnot(length(scoring) == nrow(score.matrix ) )
                options(warn = -1)                                              ### warnungen aus
                for (i in 1:nrow(score.matrix))    {
                    score.matrix$score[i] <- minMaxRawdata[i]
                    targetColumns         <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) != 0 ) )
                    stopifnot(length(targetColumns) > 0 )
                    score.matrix[i,targetColumns]  <- unlist(lapply(score.matrix[i,targetColumns], FUN = function ( y ) {paste( "(", paste(as.numeric(y) * na.omit(as.numeric(unlist(strsplit(scoring[i]," ")))), collapse = " "), ")")}))
                    nonTargetColumns      <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) == 0 ) )
                    if ( length ( nonTargetColumns ) > 0 )    {
                       score.matrix[i,nonTargetColumns]  <- "()"
                    }
                }
                options(warn = 0)                                               ### warnungen wieder an
                return(score.matrix)}
                 
userSpecifiedList <- function ( l, l.default ) {
		if ( !is.null ( names ( l ) ) ) {
				names ( l ) <- match.arg ( names(l) , names(l.default) , several.ok = TRUE )
		} else {
        if(length(l) > length(l.default) )  {
           stop("Length of user-specified list with more elements than default list.\n")
        }
				names ( l ) <- names ( l.default )[seq(along=l)]
		}
		if ( length(l) < length(l.default) ) {
				l <- c ( l , l.default )
				l <- l[!duplicated(names(l))]
				l <- l[match ( names (l) , names(l.default) )]
		}
		return(l)}
                 
getConquestVersion <- function ( path.conquest , path.temp , asDate = TRUE ) {
    wd <- path.temp
		f <- file.path ( wd , "delete.cqc" )
		write ( "quit;" , f )
		f <- normalizePath ( f )
		path.conquest <- normalizePath ( path.conquest )
		cmd <- paste ( "\"", path.conquest, "\" \"", f , "\"" , sep ="")
		r <- NULL
		ow <- getOption ( "warn" )
		options ( warn = -1 )
		try ( r <- system ( command = cmd , intern = TRUE ) , silent = TRUE )
		options ( warn = ow )
		file.remove ( f )
		if ( !is.null ( r ) ) {
				r <- r[1]
				r <- sub ( "ConQuest build: " , "" , r )
				r <- gsub ( "\\s+" , "-" , r )
				if ( asDate ) r <- as.date(r)
		}
		return (r)
}

halve.string <- function (string, pattern, first = TRUE )  {
     allSplit<- strsplit(x = string, split = pattern)
     if(first==TRUE)  { 
        ret <- as.matrix(data.frame(X1 = unlist(lapply(allSplit, FUN = function (l) { l[1]})), 
               X2 = unlist(lapply(allSplit, FUN = function (l) {
                    if(length(l) == 1 )  { ret <- NA } else { ret <- paste(l[-1], collapse=pattern)}
                    return(ret)})), stringsAsFactors = FALSE))
     }  else  {
        ret <- as.matrix(data.frame(X1 = unlist(lapply(allSplit, FUN = function (l) {paste(l[1:(length(l)-1)], collapse=pattern)})), 
               X2 = unlist(lapply(allSplit, FUN = function (l) { 
                    if(length(l)==1) { ret <- NA}  else { ret <- l[length(l)]}
                    return(ret)})), stringsAsFactors = FALSE))
     }
     return(ret)}           
     
     
desk.irt <- function(daten, itemspalten, na=NA,percent=FALSE,reduce=TRUE,codebook=list(datei=NULL,item=NULL,value=NULL,lab=NULL, komp=NULL), quiet = FALSE ) {
            if( !"data.frame" %in% class(daten) ) {stop("'daten' must be of class 'data.frame'.\n")}
             if(!missing(itemspalten)) {daten <- daten[,itemspalten,drop=FALSE]}
             if (is.na(na[1])==FALSE) {                                         ### wenn spezifiziert, werden hier missings recodiert
                 recode.statement <- paste(na,"= NA",collapse="; ")
                 daten            <- data.frame(sapply(daten,FUN=function(ii) {recode(ii,recode.statement)}),stringsAsFactors=FALSE)
             }
             specific.codes <- lapply(daten,function(ii){NULL})                 ### definiert ggf. Spezifische Codes, nach denen je Variable gesucht werden soll
             if(!is.null(codebook$datei) & !is.null(codebook$value))  {
               specific.codes <- lapply(as.list(colnames(daten)), FUN=function(ii) {
                                 codebook$datei[codebook$datei[,codebook$item] == ii,c(codebook$item,codebook$value)] } )
               kein.eintrag   <- which(sapply(specific.codes,FUN=function(ii) {nrow(ii)==0}))
               if(length(kein.eintrag)>0)  {cat(paste(length(kein.eintrag)," item(s) missing in codebook:\n",sep=""))
                                            cat(paste(colnames(daten)[kein.eintrag],collapse=", ")); cat("\n")}
             }
             results        <- lapply(1:ncol(daten), FUN=function(ii) {
                               res.i <- table.muster(vektor=daten[,ii], mustervektor=specific.codes[[ii]]$value)
                               namen.res.i <- names(res.i)
                               if(length(res.i)==0) {
                                  if(quiet == FALSE ) { cat(paste("Item '",colnames(daten)[ii],"' without any valid values.\n",sep=""))}
                                  res.i <- 0
                                  namen.res.i <- NA}
                               Label <- NA
                               KB <- NA
                               if(!is.null(codebook$lab))  {Label <- codebook$datei[codebook$datei[,codebook$item] == colnames(daten)[ii],codebook$lab]}
                               if(!is.null(codebook$komp)) {KB    <- codebook$datei[codebook$datei[,codebook$item] == colnames(daten)[ii],codebook$komp]}
                               res.i <- data.frame(item.nr = ii, item.name = colnames(daten)[ii], Label = Label, KB = KB, cases = length(daten[,ii]),Missing=sum(is.na(daten[,ii])),valid=sum(!is.na(daten[,ii])),Codes=namen.res.i,Abs.Freq=as.numeric(res.i),Rel.Freq=as.numeric(res.i)/sum(!is.na(daten[,ii])), item.p=mean(na.omit(daten[,ii])), stringsAsFactors=FALSE)
             })
             results        <- do.call("rbind",results)
             if(reduce == TRUE)  {results <- results[which(results$Codes == 1),]}
             if(percent == TRUE) {results$Rel.Freq <- 100 * results$Rel.Freq}
             return(results)}    
             
table.muster <- function(vektor, mustervektor = NULL, weights, na.rm = TRUE, useNA = c("no", "ifany", "always")) {
               if(length(mustervektor)>0) {
                  additional.values <- na.omit( setdiff(vektor,mustervektor))
                  if( length( additional.values ) > 0 )   {
                     cat("Warning: Found additional values not defined in 'mustervektor':\n")
                     cat(paste(additional.values,collapse=", ")); cat("\n")
                     cat("Concatenate additional values to 'mustervektor'.\n")
                     mustervektor <- c(mustervektor, additional.values)
                  }
                  notInData <- setdiff(mustervektor, vektor)
                  if ( length(notInData) > 0) { 
                       vektor <- factor(vektor, levels = as.character(mustervektor))
                  }
                }       
                  if(missing(weights)) {
                     Table <- table(vektor, useNA =useNA )
                  }  else  { 
                     if ( length(notInData) > 0) { 
                          vektor <- c(vektor, notInData)
                          weights<- c(weights, rep(0, length(notInData)))
                     }     
                     Table <- wtd.table(x = vektor, weights = weights, na.rm =na.rm  )$sum.of.weights
                  }
                return(Table)}
              
wo.sind <- function(a,b,quiet=FALSE) {
            b <- data.frame(1:length(b),b,stringsAsFactors=FALSE)               ### zusätzliche Syntaxbefehle sind notwendig, damit die Funktion mit missing values umgehen kann.
            if(sum(which(is.na(a)))>0)     {cat("a contains missing values. \n")}
            if(sum(which(is.na(b[,2])))>0) {cat("b contains missing values. \n")}
            if(length(na.omit(a)) > length(unique(na.omit(a))))     {cat("a contains duplicate elements. \n")}
            if(length(intersect(a,b[,2])) == 0) {
               cat("No common elements in a and b. \n")
               reihe <- NULL
            }  else {    
               if(quiet==FALSE) { if(length(intersect(a,b[,2])) > 0) {if(length(setdiff(a,b[,2]))>0)      {cat("Not all Elemente of a included in b. \n")} } }
               a <- na.omit(unique(a))                                          ### Sofern vorhanden, werden missing values aus a entfernt
               b <- na.omit(b)                                                  ### Sofern vorhanden, werden missing values aus b entfernt; aber: Rangplatz der
               if(length(a)>0) {                                                ### der nicht fehlenden Elemente in b bleibt erhalten
                  reihe <- b[ which(b[,2] %in% a)  ,1]
                  if(quiet==FALSE) { cat(paste("Found",length(reihe),"elements.\n")) }
               }   
               if(length(a)==0) {cat("No valid values in a.\n")}
            }   
            return(reihe)}
              
              
### angelehnt an das Skript von Alexander Robitzsch, "R_Skalierung.odt", Seite 11
item.diskrim <- function(daten, itemspalten, na = NA, streng = TRUE)
                {if(!missing(itemspalten))  {daten <- daten[,itemspalten]}
                 if(is.null(dim(daten))) {daten <- as.matrix(daten)
                                          colnames(daten) <- "variable"}
                 namen <- colnames(daten)
                 if (is.na(na[1])==FALSE) {for (i in na)
                                               {daten[daten==i] <- NA  }}
                 daten <- data.frame(daten, stringsAsFactors = FALSE)           ### Trennschärfe ist eigentlich Korrelation des Items mit dem Summenscore ohne dieses Item. Dieses macht die Option "streng = T"; die andere berechnet Korrelation mit Summenscore einschließlich dieses Items
                 daten <- data.frame(sapply(daten, FUN = function (uu ) {as.numeric(uu)}))
                 if(streng == TRUE)  {trennsch  <- sapply(colnames(daten), FUN = function(i) {cor(daten[,i],rowMeans(daten[,-match(i, colnames(daten)), drop = FALSE],na.rm = TRUE) ,use = "complete.obs")})}
                 if(streng == FALSE) {trennsch  <- sapply(daten, FUN = function(i) {cor(i,rowMeans(daten,na.rm = TRUE) ,use="complete.obs")})}
                 trennsch  <- data.frame(item.name=colnames(daten),item.diskrim = trennsch,stringsAsFactors = FALSE)
                 return(trennsch)}
              