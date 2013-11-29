generate.replicates <- function ( dat, ID, wgt = NULL, JKZone, JKrep )      {
                       stopifnot(length(JKZone) == 1 & length(JKrep) == 1 )
                       allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep)
                       all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                       dat.i       <- dat[,unlist(all.Names)]
                       colnames(dat.i) <- names(all.Names)
                       if( !all( names(table(dat.i$JKrep)) == c(0,1)) ) {stop("Only 0 and 1 are allowed for JKrep variable.\n")}
                       zonen       <- names(table(dat.i$JKZone) )
                       cat(paste("Create ",length(zonen)," replicate weights.\n",sep=""))
                       missings    <- sapply(dat.i, FUN = function (ii) {length(which(is.na(ii)))})
                       if(!all(missings == 0)) {
                           mis.vars <- paste(names(missings)[which(missings != 0)], collapse = ", ")
                           stop(paste("Found missing value(s) in variable(s) ", mis.vars,".\n",sep=""))
                       }
                       reps <- data.frame ( lapply(zonen , FUN = function(ii) {
                               rep.ii <- dat.i[,"wgt"]
                               rep.ii[dat.i[,"JKZone"] == ii ] <- ifelse(dat.i[ dat.i[,"JKZone"] == ii ,"JKrep"] == 1, 0, 2 * rep.ii[dat.i[,"JKZone"] == ii ] )
                               return(rep.ii) }), stringsAsFactors = FALSE)
                       colnames(reps) <- paste(wgt, 1:ncol(reps), sep="_")
                       ret            <- data.frame(ID = dat.i[,"ID",drop=FALSE], reps, stringsAsFactors = FALSE)
                       attr(ret, "n.replicates") <- length(zonen)
                       return(ret) }


setCatParameter <- function ( dat ) {
                   if(!is.null(attr(dat, "logFile")) ) {
                       file   <- attr(dat, "logFile")
                       append <- TRUE
                   } else {
                     file   <- ""
                     append <- FALSE  }
                   return(list(file = file, append = append))}


adjustDependentForNested <- function ( dependent, complete.permutation, group ) {
            if(sum(sapply(dependent, is.list)) > 0)   {
                if(complete.permutation != "nothing") {cat("Warning: 'complete.permutation' should be set to 'nothing' if a nested structure is implemented.\n")}
                foo       <- checkNest ( group = group, dependent = dependent )
                dependent <- lapply(dependent, FUN = function (d) {
                             if(is.list(d) ) {
                                unlistD                 <- unlist(d)
                                attr(unlistD, "nested") <- d
                             } else { unlistD <- d }
                             return(unlistD) })
            }
            return(dependent)}


### dat               ... Datensatz als data.frame
### ID                ... ID als Spalte oder Spaltenname
### group             ... optional: Liste aus Gruppierungsvariablen, ggf. imputiert, Bsp.:
###                       group = list(EGP = c("EGP1", "EGP2", "EGP3", "EGP4", "EGP5"))
###                       Bei mehr als einer Gruppierungsvariable wird kreuztabuliert
### dependent         ... Liste der abhängigen variablen, ggf. imputiert, Bsp.:
###                       dependent = list(Lesen = paste("Lesen", gsub(" ", "0", formatC(1:15, width = 2)), sep = ""))
### group.differences.by  optional: nach welcher Variable sollen Gruppendifferenzen bestimmt werden? Variable muss eine der Variablen in group sein!
###                       angegeben werden muss dabei der Name des Listenelementes, sofern es sich um eine imputierte Variable handelt
###                       Bsp.: group = list(BL = c("bl1", "bl2", "bl3", "bl4", "bl5"), geschlecht = "sex"), group.differences.by = "geschlecht". Hier werden in jedem BL Geschlechtsunterschiede bestimmt.
### complete.permutation  Wenn Anzahl der Imputationen von unabhängiger und abhängiger Variable variiert
jk2.mean <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(), group.splits = length(groups), group.differences.by = NULL, group.delimiter = "_", dependent = list(), na.rm = FALSE, complete.permutation = c("nothing", "groups", "all"), forcePooling = TRUE, boundary = 3, doCheck = TRUE)    {
            complete.permutation <- match.arg ( complete.permutation )
            if(length(groups)>0) {
               toLapply <- superSplitter(group = groups, group.splits = group.splits, group.differences.by = group.differences.by, group.delimiter = group.delimiter , dependent=dependent )
               for ( u in unlist(groups) ) {dat[,u] <- as.character(dat[,u])}
            } else {toLapply <- list(schleife1 <- list())}
            # if(!exists("rbind.fill"))     {library(plyr)}
            allAnalyses <- do.call("rbind.fill", lapply(toLapply, FUN = function ( group ) {
                nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
                JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = forcePooling )
                if(!is.null(JK$JK)) {
                    forcePooling  <- JK$forcePooling
                    dependent     <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                    catPrms       <- setCatParameter(dat)
                    if(!is.null(attr(group,"group.differences.by"))) {
                       if(!attr(group,"group.differences.by") %in% names(nam[["groupSet"]])) {stop()} }
                    wgtNULL       <- FALSE
                    if(is.null(wgt))   {
                       cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                       dat$weight_one <- 1
                       wgt            <- "weight_one"
                       wgtNULL        <- TRUE
                    }
                    if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )}
                    if(length(nam[["groupSet"]]) == 0) {
                       cat("No group(s) specified. Analyses will be computed only for the whole sample.\n", file=catPrms$file, append=catPrms$append)
                       dat$wholeGroup <- "wholeGroup"
                       nam[["groupSet"]]  <- list(wholeGroup = "wholeGroup")
                    }
                    allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(nam[["groupSet"]]), dependent = unlist(dependent) )
                    all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                    dat.i       <- dat[,unlist(all.Names), drop = FALSE]
                    missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
                    ### Missings duerfen nur in abhaengiger Variable auftreten!
                    kritisch    <- setdiff( names(missings[missings!=0]) , unlist(dependent ) )
                    if(length(kritisch)>0) {stop(paste("Found NAs in variable(s) ",paste(kritisch, collapse = ", "), "\n",sep = "") )}
                    ### Anhaengige Variable muss numerisch sein
                    kritisch    <- sapply(dat.i[,unlist(dependent), drop = FALSE], FUN = function (uu) {class(uu)})
                    kritisch    <- unique(setdiff(kritisch, c("numeric", "integer")))
                    if(length(kritisch)>0) {cat(paste("WARNING: Found non-numeric variable classes in dependent variables: ",paste(unique(kritisch),collapse = ", "),". Analysis may fail.\n",sep=""))}
                    cat(paste("Found ",length(nam[["groupSet"]])," grouping variable(s).\n",sep=""), file=catPrms$file, append=catPrms$append)
                    .checkGroupConsistency(dat = dat, group = nam[["groupSet"]])
                    cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""), file=catPrms$file, append=catPrms$append)
                    if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(nam[["groupSet"]], stringsAsFactors = FALSE))} else { group <- nam[["groupSet"]]}
                    names.dependent <- names(nam[["dependentSet"]])
					          if(doCheck == TRUE) {
						           isCheck    <- lapply(names.dependent, FUN = function ( depN ) {
									                   workbook   <- .generateWorkbook(group = nam$groupSet, dep = dependent[[depN]], complete.permutation.all = (complete.permutation == "all") )
                                     isCheckDep <- checkData( dat=dat, ID=ID, wgt=wgt, JKZone=NULL, JKrep=NULL, group=nam$groupSet, dependent=nam$dependentSet, depN=depN, workbook=workbook, na.rm=na.rm, separate.missing.indikator = NA)
									                   return(isCheckDep)})
					          names(isCheck) <- names.dependent
					          }
                    analysis    <- do.call("rbind", lapply(names.dependent, FUN = function ( depN ) {
                                   if(doCheck == TRUE ) {dat.iC          <- merge(dat.i, isCheck[[depN]], by = ID, all = TRUE)} else { dat.iC <- dat.i; dat.iC[,"allCheck"] <- "TRUE" }
                                   analysisChecked <- do.call("rbind", by(data = dat.iC, INDICES = dat.iC[,"allCheck"], FUN = function ( dat.i ) {
                                       dep      <- dependent[[depN]]
                                       workbook <- .generateWorkbook(group = nam$groupSet, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                                       if( dat.i[1,"allCheck"] == "TRUE") {
                                           cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                                           if(JK$JK == TRUE)   {ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.mean, dat.i = dat.i, dep = dep, replicates = replicates, ID = ID, wgt = wgt, na.rm = na.rm , group.differences.by = attr(group,"group.differences.by"), workbook = workbook,group.delimiter=group.delimiter,nam=nam)}
                                           if(JK$JK == FALSE)  {ana <- apply(workbook$workbook, MARGIN = 1, FUN = conv.mean, dat.i = dat.i, dep = dep, ID = ID, wgt = wgt, na.rm = na.rm , group.differences.by = attr(group,"group.differences.by"), workbook = workbook,group.delimiter=group.delimiter,nam=nam)}
                                           struktur   <- makeNestedStruktur ( dep=dep, ana=ana, catPrms=catPrms )
                                           if(length(ana)>1)    {                       ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                              stopifnot(length(ana) == nrow(workbook$workbook))
                                              for ( u in 1:length(ana)) { ana[[u]][,"dep"] <- workbook$workbook[u,"dep"]}
                                              anaL    <- merge( do.call("rbind", ana), struktur, by = "dep", all = TRUE)
                                              retList <- jk2.pool ( datLong = anaL, groupNames = names(nam[["groupSet"]]) )
                                           } else { retList <- ana[[1]]}
                                       } else {
                                           uniKomb <- unique(as.vector(unlist(apply(workbook$workbook, MARGIN = 1, FUN = function ( imp ) { foo <- apply(dat.i[,imp[-length(imp)],drop=FALSE],1,FUN = function ( uk ) {paste(uk,collapse=group.delimiter)})}))))
                                           retList <- do.call("rbind", lapply(uniKomb, FUN = function ( uk ) { data.frame ( group = uk, expand.grid(list(parameter = c("mean", "Ncases", "sd", "var"), coefficient = c("est","se")),stringsAsFactors = FALSE), value=NA,matrix(unlist(strsplit(uk,split=group.delimiter)),nrow=1,dimnames=list(NULL,names(nam$groupSet))), stringsAsFactors = FALSE)}))
                                       }
                                       return(retList)}))
                                   if(wgtNULL==TRUE)    { m1 <- "unweighted" } else { m1 <- "weighted"}
                                   if(is.null(JKZone))  { m2 <- NULL } else { m2 <- "jk2."}
                                   retList  <- data.frame ( group = analysisChecked[,"group"], depVar = depN, modus = paste(m2,m1,sep="",collapse=""), analysisChecked[,c("parameter","coefficient","value")], analysisChecked[,names(group),drop=FALSE], stringsAsFactors = FALSE, row.names = NULL)
                                   if(!is.na(match("wholeGroup", colnames(retList)))) { retList <- retList[,-match("wholeGroup", colnames(retList)),drop=FALSE]}
                                   return(retList)
                    }))
                } else {
                    analysis <- NULL
                }
                return(analysis)}))
			      class(allAnalyses) <- c("data.frame", "jk2.mean")
            return(allAnalyses)}

summary.jk2.mean <- function ( object, omitTerms = c("mean","sd","var", "Ncases","NcasesValid", "meanGroupDiff", "se","est") ) {
             groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
             origOmit  <- c("mean","sd","var", "Ncases","NcasesValid", "meanGroupDiff", "se","est")
             if(length(omitTerms) == length(origOmit)) { if ( all (omitTerms == origOmit )) { omitTerms <- NULL } }
             if (!is.null(omitTerms)) {
                 weg    <- unlist(lapply ( omitTerms , FUN = function ( x ) { match.arg(arg = x, choices = origOmit) } ))
                 wegC   <- unique(unlist(lapply(object[,c("parameter", "coefficient")], FUN = function ( x ) { which(x %in% weg)})))
                 if(length(wegC)>0) { object <- object[-wegC,]}
             }
             ret <- merge ( reshape2::dcast(object, depVar + group ~ parameter + coefficient, value.var = "value"), object[,c("group",groupCols)], by = "group", all.x = TRUE, all.y = FALSE)
             if( "meanGroupDiff" %in% omitTerms | !"meanGroupDiff" %in% names(table(object[,"parameter"])) ) {
                 ret <- ret[!duplicated(ret[,"group"]),c("depVar",groupCols, setdiff(colnames(ret), c("depVar","group", groupCols)))]
             } else {
                 ret <- ret[!duplicated(ret[,"group"]),c("depVar","group", setdiff(colnames(ret), c("depVar", "group", groupCols)))]
             }
             whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
             if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(ret[,i], digits = 3)}}
             return(ret)}


superSplitter <- function ( group = list(), group.splits = length(group), group.differences.by = NULL, group.delimiter = "_" , dependent )  {
            if(max(group.splits)> length(group)) {group.splits[which(group.splits>length(group))] <- length(group)}
            group.splits <- unique(group.splits)
            nam          <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
            superSplitti <- unlist(lapply(group.splits, FUN = function ( x ) {
                            spl <- combinat::combn(names(nam$groupSet),x)
                            if(class(spl) == "matrix") { spl <- as.list(data.frame(spl))} else {spl <- list(spl)}
                            spl <- unlist(lapply(spl, FUN = function ( y ) { paste(as.character(unlist(y)), collapse="________")}))
                            return(spl)}))
            superSplitti <- strsplit(superSplitti, "________")
            namen        <- unlist(lapply(superSplitti, FUN = function ( y ) { paste(y, collapse=group.delimiter)}))
            superSplitti <- lapply(superSplitti, FUN = function ( y ) {
                            ret <- nam$groupSet[y]
                            if(!is.null(group.differences.by)) {if( group.differences.by %in% y ) { attr(ret,"group.differences.by") <- group.differences.by}}
                            return(ret)})
            names(superSplitti) <- namen
            return(superSplitti)}


### Hilfsfunktion fuer jk2 ...
checkData <- function ( dat, ID, wgt, JKZone, JKrep, group, independent = NULL, dependent, depN , workbook, na.rm , separate.missing.indikator) {
             resCheck <- do.call("cbind", apply(workbook$workbook, 1, FUN = function ( imp ) {
                         grouping    <- setdiff(as.character(imp[-length(imp)]), unlist(independent))
						 resCheckImp <- do.call("rbind", by(data = dat, INDICES = dat[,grouping], FUN = function ( sub.dat) {
                                        if(!is.null(JKZone)) {nJkZones <- length(table(as.character(sub.dat[,JKZone]))) > 2} else {nJkZones <- TRUE}
                                        if(is.na(separate.missing.indikator)) {nObserved <- TRUE} else {
                                           if(separate.missing.indikator[[depN]] == FALSE)  {
                                              nObserved <- length(which(!is.na(sub.dat[,as.character(imp[length(imp)])]))) > 0
                                           } else { nObserved <- TRUE}
                                        }
                                        if(is.na(na.rm)) { nMissing <- TRUE} else {
                                           if (na.rm == FALSE ) { nMissing <- length(which(is.na(sub.dat[,as.character(imp[length(imp)])])))==0}
                                           if (na.rm == TRUE )  { nMissing <- length(which(is.na(sub.dat[,as.character(imp[length(imp)])]))) < nrow(sub.dat)}
                                        }
                                        return(data.frame ( sub.dat[,ID,drop=FALSE], checkOk = all(c(nJkZones,nObserved,nMissing)), stringsAsFactors = FALSE))}))
                         return(resCheckImp)}))
             ret      <- resCheck[,c(1,2* (1:((ncol(resCheck))/2)))]
             colnames(ret)[1] <- ID
             return(data.frame(ret[,ID,drop=FALSE], allCheck = as.character( rowSums(ret[,-1,drop=FALSE])==(ncol(ret)-1) ), stringsAsFactors = FALSE))}


### separate.missing.indikator ... Soll eine separate Kategorie für missings definiert werden?
### expected.values            ... optional (und empfohlen): Vorgabe für erwartete Werte, vgl. "table.muster"
###                                kann entweder eine benannte Liste sein, mit Namen wie in "dependent", oder ein einfacher character Vektor, dann werden diese Vorgaben für alle abhängigen Variablen übernommen
###                                bleibt "expected.values" leer, dann wird es automatisch mit den Werten der Variablen in ihrer Gesamtheit belegt!
jk2.table <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(), group.splits = length(groups), group.delimiter = "_", dependent = list(), separate.missing.indikator = FALSE, expected.values = list(), complete.permutation = c("nothing", "groups", "all"), doCheck = TRUE )    {
             complete.permutation <- match.arg ( complete.permutation )
             if(length(groups)>0) {
                toLapply <- superSplitter(group = groups, group.splits = group.splits, group.delimiter = group.delimiter , dependent=dependent )
                for ( u in unlist(groups) ) {dat[,u] <- as.character(dat[,u])}
             } else {toLapply <- list(schleife1 <- list())}
             # if(!exists("rbind.fill"))     {library(plyr)}
             allAnalyses <- do.call("rbind.fill", lapply(toLapply, FUN = function ( group ) {
                  nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
                  JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE )
                  if(!is.null(JK$JK)) {
                      dependent            <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                      catPrms              <- setCatParameter(dat)
                      wgtNULL       <- FALSE
                      if(is.null(wgt))   {
                         cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                         dat$weight_one <- 1
                         wgt <- "weight_one"
                         wgtNULL <- TRUE
                      }
                      if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )} else { replicates <-  NULL }
                      if(length(nam[["groupSet"]]) == 0) {
                         cat("No group(s) specified. Analyses will be computed only for the whole sample.\n", file=catPrms$file, append=catPrms$append)
                         dat$wholeGroup <- "wholeGroup"
                         nam[["groupSet"]]  <- list(wholeGroup = "wholeGroup")
                      }
                      allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(nam[["groupSet"]]), dependent = unlist(dependent) )
                      all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                      dat.i       <- dat[,unlist(all.Names)]                    ### Missings duerfen nur in abhaengiger Variable auftreten!
                      missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
                      kritisch    <- setdiff( names(missings[missings!=0]) , unlist(dependent ) )
                      if(length(kritisch)>0) {stop(paste("Found NAs in variable(s) ",paste(kritisch, collapse = ", "), "\n",sep = "") )}
                      cat(paste("Found ",length(nam[["groupSet"]])," grouping variable(s).\n",sep=""), file=catPrms$file, append=catPrms$append)
                      .checkGroupConsistency(dat = dat, group = nam[["groupSet"]])
                      cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""), file=catPrms$file, append=catPrms$append)
                      flush.console()
                      if(length(expected.values)>0) {
                         if(class(expected.values) == "character")  {
                            expected.values <- lapply(dependent, FUN = function (uu) {expected.values})
                            names(expected.values) <- names(dependent)
                         } else {
                           if(class(expected.values) == "list")  {
                              if(length(expected.values)==1 & length(dependent)>1) {
                                 expected.values <- rep(expected.values,length(dependent))
                              }
                              if(is.null(names(expected.values))) {
                                 names(expected.values) <- names(dependent)
                              } else {
                                if ( !all(names(expected.values) == names(dependent)))  {
                                   names(expected.values) <- names(dependent)
                                }
                              }
                           }
                         }
                      }
                      if(length(expected.values) == 0) {
                          expected.values <- lapply(dependent, FUN = function (ii ) {unique(unlist(lapply(ii, FUN = function (iii) {names(table(dat[,iii]))})))})
                      }
                      if(class(separate.missing.indikator) == "logical")   {
                         separate.missing.indikator <- lapply(dependent, FUN = function (uu) {separate.missing.indikator})
                      }
                      names.dependent <- names(nam[["dependentSet"]])
                      dependent   <- lapply(names(dependent), FUN = function(zz) {
                                     ret  <- dependent[[zz]]
                                     attr(ret, "expected") <- expected.values[[zz]]
                                     attr(ret, "separate.missing") <- separate.missing.indikator[[zz]]
                                     return(ret)
                      })
                      names(dependent) <- names.dependent
                      if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(nam[["groupSet"]], stringsAsFactors = FALSE))} else { group <- nam[["groupSet"]] }
					            if(doCheck == TRUE) {
						             isCheck     <- lapply(names.dependent, FUN = function ( depN ) {
							  		                    workbook   <- .generateWorkbook(group = nam$groupSet, dep = dependent[[depN]], complete.permutation.all = (complete.permutation == "all") )
									                      isCheckDep <- checkData( dat=dat, ID=ID, wgt=wgt, JKZone=NULL, JKrep=NULL, group=nam$groupSet, dependent=nam$dependentSet, depN=depN, workbook=workbook, na.rm=NA, separate.missing.indikator = separate.missing.indikator)
									                      return(isCheckDep)})
					               names(isCheck) <- names.dependent
					            }
                      analysis    <- do.call("rbind", lapply(names.dependent, FUN = function ( depN ) {
                                     if(doCheck == TRUE ) {dat.iC          <- merge(dat.i, isCheck[[depN]], by = ID, all = TRUE)} else { dat.iC <- dat.i; dat.iC[,"allCheck"] <- "TRUE" }
                                     analysisChecked <- do.call("rbind", by(data = dat.iC, INDICES = dat.iC[,"allCheck"], FUN = function ( dat.i ) {
                                          dep      <- dependent[[depN]]
                                          workbook <- .generateWorkbook(group = nam$groupSet, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                                          if( dat.i[1,"allCheck"] == "TRUE") {
                                              cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                                              if(!is.null(replicates)) {ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.table, dat.i = dat.i, dep = dep , depN=depN, replicates = replicates, ID = ID, wgt = wgt, catPrms = catPrms)}
                                              if(is.null(replicates))  {ana <- apply(workbook$workbook, MARGIN = 1, FUN = conv.table, dat.i = dat.i, dep = dep , depN=depN, ID = ID, wgt = wgt, catPrms = catPrms, separate.missing.indikator = separate.missing.indikator)}
                                              struktur   <- makeNestedStruktur ( dep=dep, ana=ana, catPrms=catPrms )
                                              if(length(ana)>1)    {            ### Es wird nur gepoolt, wenn es mehr als eine Imputation gegeben hat
                                                 stopifnot(length(ana) == nrow(workbook$workbook))
                                                 for ( a in 1 : length(ana) ) {ana[[a]][,"dep"] <- workbook$workbook[a,"dep"] }
                                                 anaL         <- merge( do.call("rbind", ana), struktur, by = "dep", all = TRUE)
                                                 pooled       <- jk2.pool ( datLong = anaL, groupNames = names(nam[["groupSet"]]) )
                                                 pooled       <- data.frame ( group = pooled[,"group"], depVar=ana[[1]][,"depVar"], modus = ana[[1]][,"modus"], pooled[,c("coefficient","parameter","value",names(nam[["groupSet"]]))], stringsAsFactors = FALSE, row.names = NULL)
                                              } else { pooled <- ana[[1]] }
                                           } else {
                                           uniKomb <- unique(as.vector(unlist(apply(workbook$workbook, MARGIN = 1, FUN = function ( imp ) { foo <- apply(dat.i[,imp[-length(imp)],drop=FALSE],1,FUN = function ( uk ) {paste(uk,collapse=group.delimiter)})}))))
                                           pooled  <- do.call("rbind", lapply(uniKomb, FUN = function ( uk ) {data.frame ( group = uk, depVar = depN, modus = "noch_leer", expand.grid(list(coefficient = c("est","se"), parameter = expected.values[[depN]]), stringsAsFactors = FALSE), value=NA,matrix(unlist(strsplit(uk,split=group.delimiter)),nrow=1,dimnames=list(NULL,names(nam$groupSet))), stringsAsFactors = FALSE, row.names = NULL)}))
                                           }
                                           return(pooled)}))
                                     analysisChecked[,"group"] <- apply(analysisChecked[,names(nam[["groupSet"]]),drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)})
                                     if(wgtNULL==TRUE)   { m1 <- "unweighted" } else { m1 <- "weighted"}
                                     if(is.null(JKZone)) { m2 <- NULL } else { m2 <- "jk2."}
                                     analysisChecked[,"modus"] <- paste(m2,m1,sep="",collapse="")
                                     if(!is.na(match("wholeGroup", colnames(analysisChecked)))) { analysisChecked <- analysisChecked[,-match("wholeGroup", colnames(analysisChecked)),drop=FALSE]}
                                    return(analysisChecked)
                      }) )
                  } else {
                      analysis <- NULL
                  }
                  return(analysis) } ))
       class ( allAnalyses) <- c("data.frame", "jk2.table")
			 return (allAnalyses) }

summary.jk2.table <- function ( object, reshapeFormula = depVar + group ~ parameter + coefficient, seOmit = FALSE) {
             groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
             if (seOmit == TRUE) { object <- object[-grep("se", object[,"coefficient"]),]}
             ret <- merge ( reshape2::dcast(data = object, formula = reshapeFormula, value.var = "value"), object[,c("group",groupCols)], by = "group", all.x = TRUE, all.y = FALSE)
             ret <- ret[!duplicated(ret[,"group"]),c("depVar",groupCols, setdiff(colnames(ret), c("depVar","group", groupCols)))]
             whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
             if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(ret[,i], digits = 3)}}
             return(ret)}


jk2.quantile <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(), group.splits = length(groups), group.delimiter = "_", dependent = list(), probs = seq(0, 1, 0.25),  na.rm = FALSE, complete.permutation = c("nothing", "groups", "all"), nBoot = NULL, bootMethod = c("wSampling","wQuantiles") , doCheck = TRUE)    {
            bootMethod           <- match.arg ( bootMethod )
            complete.permutation <- match.arg ( complete.permutation )
            if(length(groups)>0) {
               toLapply <- superSplitter(group = groups, group.splits = group.splits, group.delimiter = group.delimiter , dependent=dependent )
               for ( u in unlist(groups) ) {dat[,u] <- as.character(dat[,u])}
            } else {toLapply <- list(schleife1 <- list())}
            # if(!exists("rbind.fill"))     {library(plyr)}
            allAnalyses <- do.call("rbind.fill", lapply(toLapply, FUN = function ( group ) {
                  nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
                  JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE )
                  if(!is.null(JK$JK)) {
                      dependent            <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                      catPrms              <- setCatParameter(dat)
                      wgtNULL              <- FALSE
                      if(is.null(wgt))   {
                         cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                         dat$weight_one <- 1
                         wgt <- "weight_one"
                         wgtNULL <- TRUE
                      }
                      if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )}
                      if(length(nam[["groupSet"]]) == 0) {
                         cat("No group(s) specified. Analyses will be computed only for the whole sample.\n",file=catPrms$file, append=catPrms$append)
                         dat$wholeGroup <- "wholeGroup"
                         nam[["groupSet"]] <- list(wholeGroup = "wholeGroup")
                      }
                      allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(nam[["groupSet"]]), dependent = unlist(dependent) )
                      all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                      dat.i       <- dat[,unlist(all.Names)]
                      missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
                      if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
                      cat(paste("Found ",length(nam[["groupSet"]])," grouping variable(s).\n",sep=""),file=catPrms$file, append=catPrms$append)
                      .checkGroupConsistency(dat = dat, group = nam[["groupSet"]])
                      groupsize   <- sapply(nam[["groupSet"]], FUN = function (iii ) {length(iii)})
                      cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""),file=catPrms$file, append=catPrms$append)
                      if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(nam[["groupSet"]], stringsAsFactors = FALSE))} else { group <- nam[["groupSet"]]}
                      names.dependent <- names(nam[["dependentSet"]])
     					        if(doCheck == TRUE) {
						             isCheck     <- lapply(names.dependent, FUN = function ( depN ) {
									                      workbook   <- .generateWorkbook(group = nam$groupSet, dep = dependent[[depN]], complete.permutation.all = (complete.permutation == "all") )
                 									      isCheckDep <- checkData( dat=dat, ID=ID, wgt=wgt, JKZone=NULL, JKrep=NULL, group=nam$groupSet, dependent=nam$dependentSet, depN=depN, workbook=workbook, na.rm=na.rm, separate.missing.indikator = NA)
                 									      return(isCheckDep)})
					            names(isCheck) <- names.dependent
					            }
                      analysis    <- do.call("rbind", lapply(names.dependent, FUN = function ( depN ) {
                                     if(doCheck == TRUE ) {dat.iC          <- merge(dat.i, isCheck[[depN]], by = ID, all = TRUE)} else { dat.iC <- dat.i; dat.iC[,"allCheck"] <- "TRUE" }
                                     analysisChecked <- do.call("rbind", by(data = dat.iC, INDICES = dat.iC[,"allCheck"], FUN = function ( dat.i ) {
                                           dep      <- dependent[[depN]]
                                           workbook <- .generateWorkbook(group = group, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                                               if( dat.i[1,"allCheck"] == "TRUE") {
                                               cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                                               if(JK$JK == TRUE )  {ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.quantile, dat.i = dat.i, dep=dep, replicates = replicates, ID = ID, wgt = wgt, probs = probs,na.rm=na.rm)}
                                               if(JK$JK == FALSE ) {ana <- apply(workbook$workbook, MARGIN = 1, FUN = conv.quantile, dat.i = dat.i, dep=dep, ID = ID, wgt = wgt, probs = probs,na.rm=na.rm,wgtNULL =wgtNULL,nBoot=nBoot,bootMethod=bootMethod )}
                                               struktur   <- makeNestedStruktur ( dep=dep, ana=ana, catPrms=catPrms )
                                               if(length(ana)>1)    {           ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                                  stopifnot(length(ana) == nrow(workbook$workbook))
                                                  for ( a in 1 : length(ana) ) {ana[[a]][,"dep"] <- workbook$workbook[a,"dep"] }
                                                  anaL    <- merge( do.call("rbind", ana), struktur, by = "dep", all = TRUE)
                                                  pooled  <- jk2.pool ( datLong = anaL, groupNames = names(nam[["groupSet"]]) )
                                                  pooled  <- data.frame ( group = pooled[,"group"], depVar = ana[[1]][,"depVar"], modus = ana[[1]][,"modus"], pooled[,c("parameter","coefficient","value", names(nam[["groupSet"]]) )], stringsAsFactors = FALSE)
                                               } else { pooled <- ana[[1]] }
                                           } else {
                                               uniKomb <- unique(as.vector(unlist(apply(workbook$workbook, MARGIN = 1, FUN = function ( imp ) { foo <- apply(dat.i[,imp[-length(imp)],drop=FALSE],1,FUN = function ( uk ) {paste(uk,collapse=group.delimiter)})}))))
                                               pooled  <- do.call("rbind", lapply(uniKomb, FUN = function ( uk ) {data.frame ( group = uk, depVar = depN, modus = "noch_leer", expand.grid(list(coefficient = c("est","se"), parameter = probs), stringsAsFactors = FALSE), value=NA,matrix(unlist(strsplit(uk,split=group.delimiter)),nrow=1,dimnames=list(NULL,names(nam$groupSet))), stringsAsFactors = FALSE)}))
                                           }
                                           return(pooled)}))
                                     if(wgtNULL==TRUE)   { m1 <- "unweighted" } else { m1 <- "weighted"}
                                     if(is.null(JKZone)) { m2 <- NULL } else { m2 <- "jk2."}
                                     analysisChecked[,"modus"] <- paste(m2,m1,sep="",collapse="")
                                     if(!is.na(match("wholeGroup", colnames(analysisChecked)))) { analysisChecked <- analysisChecked[,-match("wholeGroup", colnames(analysisChecked)),drop=FALSE]}
                                     analysisChecked[,"depVar"] <- depN
                                     if (length(intersect( names(group), colnames(analysisChecked))) > 0 ) { analysisChecked[,"group"] <- apply(analysisChecked[,names(group),drop=FALSE], MARGIN=1,FUN = function ( b ) { paste(b, collapse=group.delimiter)}) } else { analysisChecked[,"group"] <- "wholeGroup"}
                                    return(analysisChecked)
                      }) )
                  } else {
                      analysis <- NULL
                  }
                  return(analysis) } ))
             class(allAnalyses) <- c("data.frame", "jk2.quantile")
             return (allAnalyses) }

summary.jk2.quantile <- function ( object, seOmit = FALSE) {
             groupCols <- setdiff(colnames(object), c("group", "depVar", "modus", "parameter", "coefficient", "value"))
             if (seOmit == TRUE) { object <- object[-grep("se", object[,"coefficient"]),]}
             ret <- merge ( reshape2::dcast(data = object, formula = depVar + group ~ parameter + coefficient, value.var = "value"), object[,c("group",groupCols)], by = "group", all.x = TRUE, all.y = FALSE)
             ret <- ret[!duplicated(ret[,"group"]),c("depVar",groupCols, setdiff(colnames(ret), c("depVar","group", groupCols)))]
             whichIsNumeric <- as.numeric(which ( sapply(ret, class) == "numeric"))
             if(length(whichIsNumeric)>0) {  for ( i in whichIsNumeric) {ret[,i] <- round(ret[,i], digits = 3)}}
             return(ret)}


jk2.glm <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, groups = list(), group.splits = length(groups), group.delimiter = "_", independent = list(), reg.statement = NULL, dependent = list(), complete.permutation = c("nothing", "groups", "independent", "all") , glm.family, forceSingularityTreatment = FALSE, doCheck = TRUE )    {
            complete.permutation <- match.arg ( complete.permutation )
            if(length(groups)>0) {
               toLapply <- superSplitter(group = groups, group.splits = group.splits, group.delimiter = group.delimiter , dependent=dependent )
               for ( u in unlist(groups) ) {dat[,u] <- as.character(dat[,u])}
            } else {toLapply <- list(schleife1 <- list())}
            # if(!exists("rbind.fill"))     {library(plyr)}
            allAnalyses <- do.call("rbind.fill", lapply(toLapply, FUN = function ( group ) {
                nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = independent  ) )
                JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE  )
                if(!is.null(JK$JK)) {
                    dependent            <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                    catPrms              <- setCatParameter(dat)
                    .GlobalEnv$glm.family <- glm.family                         ### Hotfix!
                    if(!is.null(reg.statement)) {
                        if( !all ( unlist(lapply(names(nam[["independentSet"]]), FUN = function (u) {grep(u, reg.statement)})) == 1) )  {
                             stop("Regression statement contains variables not incorporated in independent variables list.\n")
                        }
                        regSplit <- unlist ( sapply( strsplit(reg.statement, "\\*|\\:|\\+|-")[[1]], crop ) )
                        if( !all ( unlist(lapply(regSplit, FUN = function (u) {length(grep(u, names(nam[["independentSet"]]))) })) > 0) )  {
                             stop("Regression statement contains variables not incorporated in independent variables list.\n")
                        }
                    }
                    wgtNULL <- FALSE
                    if(is.null(wgt))   {
                       cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                       dat$weight_one <- 1
                       wgt <- "weight_one"
                       wgtNULL <- TRUE
                    }  else  {
                       if(JK$JK == FALSE) {cat("I'm sorry! Use of weights is not supported without jackknifing. Weights will be ignored. It's the fault of 'glm()'.\n")}
                    }
                    if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )} else { replicates <- NULL }
                    if(length(nam[["groupSet"]]) == 0) {
                       cat("No group(s) specified. Analyses will be computed only for the whole sample.\n",file=catPrms$file, append=catPrms$append)
                       dat$wholeGroup <- "wholeGroup"
                       nam[["groupSet"]] <- list(wholeGroup = "wholeGroup")
                    }
                    allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(nam[["groupSet"]]), independent = unlist(nam[["independentSet"]]), dependent = unlist(dependent) )
                    all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                    dat.i       <- dat[,unlist(all.Names)]
                    missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
                    if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
                    cat(paste("Found ",length(nam[["groupSet"]])," grouping variable(s).\n",sep=""),file=catPrms$file, append=catPrms$append)
                    .checkGroupConsistency(dat = dat, group = nam[["groupSet"]])
                    cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""))
                    if ( complete.permutation == "groups" )      {group       <- as.list(expand.grid(nam[["groupSet"]], stringsAsFactors = FALSE))} else { group <- nam[["groupSet"]]}
                    if ( complete.permutation == "independent" ) {independent <- as.list(expand.grid(nam[["independentSet"]], stringsAsFactors = FALSE))} else { independent <- nam[["independentSet"]]}
                    pre.workbook   <- c(group, independent)
                    names.dependent <- names(nam[["dependentSet"]])
					if(doCheck == TRUE) {
						isCheck     <- lapply(names.dependent, FUN = function ( depN ) {
									   workbook   <- .generateWorkbook(group = pre.workbook, dep = dependent[[depN]], complete.permutation.all = (complete.permutation == "all") )
									   isCheckDep <- checkData( dat=dat, ID=ID, wgt=wgt, JKZone=NULL, JKrep=NULL, group=nam$groupSet, independent = nam$independentSet, dependent=nam$dependentSet, depN=depN, workbook=workbook, na.rm=FALSE, separate.missing.indikator = NA)
									   return(isCheckDep)})
					    names(isCheck) <- names.dependent
					}
                    analysis    <- do.call("rbind", lapply(names.dependent, FUN = function ( depN ) {
                                   if(doCheck == TRUE ) {dat.iC          <- merge(dat.i, isCheck[[depN]], by = ID, all = TRUE)} else { dat.iC <- dat.i; dat.iC[,"allCheck"] <- "TRUE" }
                                   analysisChecked <- do.call("rbind", by(data = dat.iC, INDICES = dat.iC[,"allCheck"], FUN = function ( dat.i ) {
                                       dep      <- dependent[[depN]]
                                       workbook <- .generateWorkbook(group = pre.workbook, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                                       if( dat.i[1,"allCheck"] == "TRUE") {
                                           cat(paste("Use ",nrow(workbook$workbook)," replications overall.\n",sep=""))
                                           ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.glm, group = group, reg.statement = reg.statement, glm.family = glm.family, independent = independent, dat.i = dat.i, replicates = replicates, ID = ID, wgt = wgt, forceSingularityTreatment = forceSingularityTreatment,nam=nam,depN=depN )
                                           names(ana) <- workbook$workbook[,"dep"]
                                           struktur   <- makeNestedStruktur ( dep=dep, ana=ana, catPrms=catPrms )
                                           if(length(ana)>1)    {               ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                              stopifnot(length(ana) == nrow(workbook$workbook))
                                              for ( u in 1:length(ana)) { ana[[u]][,"dep"] <- workbook$workbook[u,"dep"]}
                                              anaL    <- merge( do.call("rbind", ana), struktur, by = "dep", all = TRUE)
                                              retList <- jk2.pool ( datLong = anaL, groupNames = names(nam[["groupSet"]]) )
                                           } else { retList <- ana[[1]]}
                                       } else {
                                           uniKomb <- unique(as.vector(unlist(apply(workbook$workbook, MARGIN = 1, FUN = function ( imp ) {foo <- apply(dat.i[,setdiff(imp,c(imp[length(imp)],unlist(nam[["independentSet"]]))),drop=FALSE],1,FUN = function ( uk ) {paste(uk,collapse=group.delimiter)})}))))
                                           retList <- do.call("rbind", lapply(uniKomb, FUN = function ( uk ) { data.frame ( group = uk, expand.grid(list(parameter = c("(Intercept)", names(nam[["independentSet"]]), "Ncases", "Nvalid", "R2","R2nagel"), coefficient = c("est","se")),stringsAsFactors = FALSE), value=NA,matrix(unlist(strsplit(uk,split=group.delimiter)),nrow=1,dimnames=list(NULL,names(nam$groupSet))), stringsAsFactors = FALSE)}))
                                       }
                                       return(retList)}))
                                   if(wgtNULL==TRUE)    { m1 <- "unweighted" } else { m1 <- "weighted"}
                                   if(is.null(JKZone))  { m2 <- NULL } else { m2 <- "jk2."}
                                   retList  <- data.frame ( group = apply(analysisChecked,1,FUN = function ( z ) { paste(z[names(nam[["groupSet"]])],collapse=group.delimiter)}), depVar = depN, modus = paste(m2,m1,sep="",collapse=""), analysisChecked[,c("parameter","coefficient","value")], analysisChecked[,names(nam[["groupSet"]]),drop=FALSE], stringsAsFactors = FALSE, row.names = NULL)
                                   if(!is.na(match("wholeGroup", colnames(retList)))) { retList <- retList[,-match("wholeGroup", colnames(retList)),drop=FALSE]}
                                   return(retList)
                    }))
                } else {
                    analysis <- NULL
                }
            return(analysis)}))
            class(allAnalyses) <- c("data.frame", "jk2.glm")
            return(allAnalyses)}


summary.jk2.glm <- function ( object , analyses = NULL ) {
            splitData <- by ( data = object, INDICES = object[,c("group", "depVar")], FUN = function ( spl ) {return(spl)})
			      if(is.null(analyses)) {analyses <- 1:length(splitData)}
            for ( i in analyses) {
                 spl    <- splitData[[i]]
                 split2 <- spl[,"parameter"] %in% c("Ncases","Nvalid","R2","R2nagel")
                 ret    <- reshape2::dcast(spl[!split2,], parameter~coefficient)
                 ret[,"t.value"] <- ret[,"est"] / ret[,"se"]
                 df     <- spl[ spl[,"parameter"] == "Ncases" & spl[,"coefficient"] == "est"  ,"value"] - nrow(ret)
                 ret[,"p.value"] <- 2*(1-pt( q = abs(ret[,"t.value"]), df = df ))
                 ret    <- data.frame ( lapply(ret, FUN = function ( y ) {if(class(y)=="numeric") {y <- round(y, digits = 3)}; return(y)}), stringsAsFactors = FALSE)
                 groupNamen <- setdiff(colnames(spl), c("group","depVar","modus", "parameter", "coefficient","value"))
                 cat ( paste( "            groups: ", paste( groupNamen, as.vector(spl[1,groupNamen]), sep=" = ", collapse = "; "),"\n",sep=""))
                 cat ( paste( "dependent Variable: ", spl[1,"depVar"], "\n \n", sep=""))
                 print(ret)
                 r2     <- spl[ spl[,"parameter"] == "R2" ,"value"]
                 r2nagel<- spl[ spl[,"parameter"] == "R2nagel" ,"value"]
                 cat(paste("\n            R-squared: ",round(r2[1],digits = 3),"; SE(R-squared): ",round(r2[2],digits = 3),"\n",sep=""))
                 cat(paste  ("Nagelkerkes R-squared: ",round(r2nagel[1],digits = 3),"; SE(Nagelkerkes R-squared): ",round(r2nagel[2],digits = 3),"\n",sep=""))
                 nn     <- spl[ spl[,"parameter"] == "Nvalid" & spl[,"coefficient"] =="est" ,"value"]
                 cat(paste(nn," observations and ",df, " degrees of freedom.",sep="")); cat("\n")
                 if(i != max(analyses)) { cat("------------------------------------------------------------------\n") }
            }}


doJK <- function ( dat, JKZone , JKrep, forcePooling ) {
        if(is.null(JKZone) | is.null(JKrep))   {
           cat("No jacknifing variables. Assume no cluster structure.\n")
           if(forcePooling == TRUE) {
              # cat("'forcePooling' is set to 'FALSE' without a cluster structure. This is to avoid biased standard errors for variances and standard deviations.\n")
              forcePooling  <- FALSE
           }
           JK  <- FALSE
        } else {
           nZonen <- length(table(dat[,JKZone]))
           if( nZonen < 2 ) {                                                   ### Plausibilitaets-check: gibt es mehr als nur eine JK-Zone?
              cat(paste("Error: Found only ", nZonen," jackknifing zone. Abort analysis.\n",sep=""))
              JK <- NULL
           } else {
              JK <- TRUE
           }
        }
        return(list(JK = JK, forcePooling = forcePooling))}


.checkAndCreateGroupList <- function ( dat, ID, wgt, JKZone, JKrep, group , dependent, independent = NULL, use.cores)   {
             groupImps    <- lapply(group, length)
             groupImps1   <- which(groupImps != 1)
             if(length(group)>0)      {cat(paste("Found ",length(group)," group(s).\n",sep="")) }
             if(length(groupImps1)>0) {cat(paste(length(groupImps1)," group(s) with more than one imputation. For this groups no multicore option is available.\n",sep=""))}
             depImp       <- lapply(dependent, length)
             if(length(group) == 0 & length(dependent) == 1) {stop("Only one analysis and no groups selected. Please use singlecore version of this function.\n")}
             if(all(groupImps != 1) & length(dependent) == 1) {stop("Only one analysis and only group(s) with more than one imputation found. Please use singlecore version of this function.\n")}
             allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(group), dependent = unlist(dependent), independent = unlist(independent) )
             all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
             .checkGroupConsistency(dat = dat, group = group)
             useGroup    <- names(group)[which(groupImps == 1)]
             groupNames  <- unlist(group[useGroup])
             if(length(groupNames) > 0) {for (u in groupNames) {dat[,u] <- as.character(dat[,u])}}
             groupCategories <- lapply(groupNames, FUN = function (u) {names(table(dat[,u]))})
             groupCategories$dependent <- names(dependent)
             groupList       <- expand.grid(groupCategories,stringsAsFactors = FALSE)
             groupList       <- data.frame(Nummer = 1:nrow(groupList), groupList, stringsAsFactors = FALSE )
             if(use.cores == 1 ) {stop("Not useful to operate on 1 core in multicore option. Please use single core version of this function or choose a more fancy machine. \n")}
             restGroup       <- group[names(group)[which(groupImps != 1)]]
             if(length(restGroup)== 0) {restGroup <- list() }
             splitGroup      <- group[names(group)[which(groupImps == 1)]]
             if(length(splitGroup)== 0) {splitGroup <- list() }
             cat(paste( use.cores , " useable cores and ", nrow(groupList), " analyses to be parallelized found.\n",sep=""))
             if(use.cores > nrow(groupList))   {
                cat(paste("Will use only ", nrow(groupList), " cores.\n",sep=""))
                use.cores <- nrow(groupList)
             }
             return(list ( groupList = groupList, splitGroup = splitGroup, restGroup=restGroup, use.cores = use.cores )  )}


assignNames <- function ( x = list ( groupSet = NULL, dependentSet = NULL, independentSet = NULL  ) ) {
               ret <- lapply(names(x), FUN = function (y) {
                      if ( length( names ( x[[y]] ) ) == 0 & length( x[[y]] ) > 0 )  {
                              names ( x[[y]] ) <- unlist(x[[y]])[1:length(x[[y]])]
                      }  else  {
                           leerName <- which( nchar ( names ( x[[y]] ) ) == 0 )
                           if(length(leerName) > 0 )  {
                              names(x[[y]])[leerName] <- paste (y, 1:length(leerName),sep="")
                           }
                      }
                      return(x[[y]])})
               names(ret) <- names(x)
               return(ret)}


### Hilfsfunktion für die jk2-Dinger
.checkGroupConsistency <- function (dat, group)   {
       consistent <- lapply(group, FUN = function (ii) {
                     if(length(ii) > 1) {
                        first <- names(table(as.character(dat[,ii[1]])))
                        cons  <- lapply(dat[,ii], FUN = function (iii) {
                                 actual <- names(table(as.character(iii)))
                                 stopifnot(all ( actual == first ) )
                        })
                        }})}


### prueft die genestete Struktur
checkNest <- function(group, dependent, independent = NULL) {
             if(length(group)>0 & length(independent)>0) {
                if( length( setdiff( as.numeric(names(table(c(unlist(sapply(group,length)), unlist(sapply(independent,length)) ) ))), 1 ) ) > 1 )  {
                    cat("Warning: Nesting structure does not seem to be appropriate. Number of imputations for group variables differs from number of imputations for independent variable.\n")
                }
             }
             ld        <- sapply(dependent, FUN = function (x ) {ifelse(is.list(x), length(x), 1)})
             sapply( list(group, independent ), FUN = function ( x ) {
                     if(length(x) > 0 )  {
                        lg     <- sapply(x, length)
                        ret    <- lapply(ld, FUN = function (y) {y / lg})
                        falsch <- which(!sapply(ret, FUN = function (r) { all( r == round (r) ) }))
                        if(length(falsch)>0) {
                           cat(paste("Warning! Nesting structure does not seem to be appropriate for dependent variable(s) ",paste(falsch, collapse=", "),". Standard errors may be not trustworthy!\n", sep=""))
                        }
                     } } )  }


.generateWorkbook <- function (group, dep, complete.permutation.all ) {
                     group.origin <- group
                     if(complete.permutation.all == TRUE ) {
                        group$dep <- dep
                        workbook  <- expand.grid(group, stringsAsFactors = FALSE)
                        }  else  {
                        if(is.null(attr(dep, "nested")) )    {
                        max.elements <- max ( c( unlist(lapply(group, length)) , length(dep) ) )
                        group$dep    <- dep
                        group        <- lapply(group, FUN = function (uu) {rep(uu, times = max.elements)[1:max.elements]})
                        workbook     <- data.frame(group, stringsAsFactors = FALSE)
                        }  else  {
                        workbook     <- NULL
                        for ( m in 1 : length(attr(dep, "nested")) )   {
                              n.nested <- sapply(group.origin, length)
                              mm       <- sapply(n.nested, FUN = function ( nn ) { ifelse(nn >= m, m, nn ) } )
                              frame1   <- sapply( names(n.nested), FUN = function ( ii ) { group.origin[[ii]][mm[[ii]]] })
                              ret      <- data.frame ( t(frame1), dep = attr(dep, "nested")[[m]], stringsAsFactors = FALSE )
                              workbook <- rbind(workbook, ret)
                        }
                       }
                     }
                     return(list(workbook = workbook, group = group, group.origin = group.origin ) ) }


makeNestedStruktur <- function ( dep, ana, catPrms ) {
                      if(!is.null(attr(dep, "nested")))   {
                          if(length(ana)>1)    {cat("\nPooling Standard errors. Underlying nested structure is assumed.\n", file=catPrms$file, append=catPrms$append)}
                          struktur <- attr(dep, "nested")
                          if(is.null(names(struktur)) )   { names(struktur) <- paste("n",gsub(" ","0",formatC(1:length(struktur), width = nchar(length(struktur)))), sep="") }
                          struktur <- do.call("rbind", lapply( names(struktur), FUN = function ( zz ) { data.frame ( nesting = zz, dep = struktur[[zz]], stringsAsFactors = FALSE)}))
                          }  else  {struktur <- data.frame ( nesting = "n1", dep=dep, stringsAsFactors = FALSE)
                              if(length(ana)>1)    {cat("\nPooling Standard errors. Assume no nested structure.\n", file=catPrms$file, append=catPrms$append)}
                          }
                       return(struktur)}


jackknife.glm <- function (imp, group, reg.statement, glm.family, independent, dat.i , replicates , ID , wgt, forceSingularityTreatment,nam,depN ) {
                 group.names       <- as.character(imp[names(group)])
                 independent.names <- setdiff(as.character(imp), group.names)
                 independent.names <- setdiff(independent.names, as.character(imp[["dep"]]) )
                 cat("."); flush.console()
                 stopifnot(length(imp[["dep"]]) == 1 ); stopifnot(length(independent.names) == length( independent) )
                 sub.ana <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,group.names], FUN = function (sub.dat) {
                            for (aa in 1:length(independent)) {                 ### Hotfix: alle Imputationen einer Variablen je Nestungsebene muessen gleich heissen, sonst mißlingt das Poolen
                                 stopifnot(  independent.names[aa] %in% independent[[aa]] )
                                 if( !names(independent)[aa] %in% colnames ( sub.dat ) ) {
                                    sub.dat[, names(independent)[aa] ] <- sub.dat[, independent.names[aa] ]
                                    imp       <- gsub(independent.names[aa] , names(independent)[aa] , imp )
                                 }
                            }
                            formel         <- as.formula(paste(imp[["dep"]],"~", paste( imp[names(independent)], collapse = " + "), sep = ""))
                            if(!is.null(reg.statement))  {
                                formel     <- as.formula(paste(imp[["dep"]],"~", reg.statement))
                            }
                            glm.ii         <- glm(formula = formel, data = sub.dat, family = glm.family)
                            singular       <- names(glm.ii$coefficients)[which(is.na(glm.ii$coefficients))]
                            group.values   <- data.frame(matrix(sapply(sub.dat[,as.character(imp[names(group)]), drop = FALSE], FUN = function(uu) {names(table(as.character(uu)))}), nrow = 1), stringsAsFactors = FALSE )
                            colnames(group.values) <- names(group)
                            if(!is.null(replicates)) {
                                # if(!exists("svrepdesign"))      {library(survey)}
                                sub.replicates <- replicates[replicates[,"ID"] %in% sub.dat[,ID] ,-1, drop = FALSE]
                                design         <- survey::svrepdesign(data = sub.dat[,as.character(imp)], weights = sub.dat[,wgt], type="JKn", scale = 1, rscales = 1, repweights = sub.replicates, combined.weights = TRUE, mse = TRUE)
                                if(length(singular) == 0 & forceSingularityTreatment == FALSE ) {
                                   glm.ii      <- survey::svyglm(formula = formel, design = design, return.replicates = FALSE, family = glm.family)
                                }
                            }
                            r.squared      <- data.frame ( r.squared = var(glm.ii$fitted.values)/var(glm.ii$y) , N = nrow(sub.dat) , N.valid = length(glm.ii$fitted.values) )
                            r.nagelkerke   <- fmsb::NagelkerkeR2(glm.ii)
                            summaryGlm     <- summary(glm.ii)
                            res.bl         <- data.frame ( group="noch_leer", depVar =depN,modus = "noch_leer", parameter = c(rep(c("Ncases","Nvalid",names(glm.ii$coefficients)),2),"R2","R2nagel"),
                                              coefficient = c(rep(c("est","se"),each=2+length(names(glm.ii$coefficients))),"est","est"),
                                              value=c(r.squared[["N"]],r.squared[["N.valid"]],glm.ii$coefficient,NA,NA,summaryGlm$coef[,2],r.squared[["r.squared"]],r.nagelkerke[["R2"]]),group.values, stringsAsFactors = FALSE)
                            if(!is.null(replicates)) {                          ### jetzt kommt die Behandlung, wenn zusaetzlich zu JK noch singularitaet auftritt. das ueberschreibt nun die bisherige "res.bl"
                                if(length(which(is.na(glm.ii$coefficients))) > 0 ) {
                                   cat(paste("Singularity problem in regression estimation for ", length(singular)," coefficient(s): ",paste(singular, collapse = ", "),". Try workaround ... \n", sep = "")); flush.console()
                                }
                                if(forceSingularityTreatment == TRUE ) {
                                   cat("Compute coefficients in the expectation of singularities ... \n"); flush.console()
                                }
                                if(length(singular) > 0 | forceSingularityTreatment == TRUE ) {
                                   # if(!exists("NagelkerkeR2")) {library(fmsb)}
                                   if(!is.null(reg.statement)) { formel <- paste(imp[["dep"]],"~", reg.statement) } else {formel <- paste(imp[["dep"]],"~", paste( imp[names(independent)], collapse = " + "), sep = "")}
                                   string     <- paste("resRoh <- data.frame( withReplicates(design, quote(getOutputIfSingular(glm(formula = ",formel,", weights=.weights, family = ",glm.family$family,"(link=\"", glm.family$link,"\"))))), stringsAsFactors = FALSE)",sep="")
                                   eval ( parse ( text = string ) )
                                   # res.bl     <- data.frame ( group.values, reg = rownames( resRoh)[-((nrow(resRoh)-2) :nrow(resRoh))], resRoh[-((nrow(resRoh)-2) :nrow(resRoh)),], r.squared = resRoh[nrow(resRoh)-2,"Estimate"], N = nrow(sub.dat), N.valid = NA, r.nagelkerke = resRoh[nrow(resRoh),"Estimate"], stringsAsFactors = FALSE )
                                }
                            }
                            attr(res.bl, "singularities") <- singular           ### [1,] 0.075734 0.0143
                            return(res.bl) }))
                 return(sub.ana) }


### Hilfsfunktion fuer jk2.glm() wenn Regression singulaere Terme enthaelt
getOutputIfSingular <- function ( glmRes ) {
                       coefs <- na.omit(coef(glmRes))
                       coefs <- c(coefs, var(glmRes$fitted.values)/var(glmRes$y), unlist(fmsb::NagelkerkeR2(glmRes)))
                       return(coefs)}


conv.quantile      <- function ( imp, dat.i, dep, ID, wgt , probs, na.rm, wgtNULL, nBoot,bootMethod=bootMethod  ) {
                      # if(!exists("wtd.quantile"))    {library(Hmisc)}
                      ret  <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                              if(wgtNULL == TRUE) {
                                 ret   <- Hmisc::hdquantile(x = sub.dat[,imp[["dep"]]], se = TRUE, probs = probs,na.rm=na.rm )
                                 ret   <- data.frame (group="noch_leer", depVar = "noch_leer", modus = "noch_leer", parameter = rep(names(ret),2), coefficient = rep(c("est","se"),each=length(ret)),value = c(ret,attr(ret,"se")),sub.dat[1,as.character(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE)
                              } else {                                          ### wenn Gewichte gefordert, koennen SEs ueber Bootstrap bestimmt werden
                                 if(!is.null(nBoot)) {
                                     if(nBoot<5) {nBoot <- 5}
                                     if(bootMethod == "wQuantiles") {           ### Variante 1
                                         x     <- sub.dat[,imp[["dep"]]]
                                         ret   <- boot::boot(data = x, statistic = function ( x, i) {Hmisc::wtd.quantile(x = x[i], weights = sub.dat[,wgt], probs = probs,na.rm=na.rm )}, R=nBoot)
                                         ret   <- data.frame (group="noch_leer", depVar = "noch_leer", modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(ret$t0, sapply(data.frame(ret$t), sd)), sub.dat[1,as.character(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE)
                                     } else {                                   ### Variante 2
                                         ret   <- do.call("rbind", lapply(1:nBoot, FUN = function (b){
                                                  y   <- sample(x = sub.dat[,imp[["dep"]]], size = length(sub.dat[,imp[["dep"]]]), replace = TRUE, prob = sub.dat[,wgt]/sum(sub.dat[,wgt]))
                                                  ret <- Hmisc::hdquantile(x = y, se = FALSE, probs = probs,na.rm=na.rm )
                                                  return(ret)}))
                                         ret   <- data.frame (group="noch_leer", depVar = "noch_leer", modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(Hmisc::wtd.quantile(x = sub.dat[,imp[["dep"]]], weights = sub.dat[,wgt], probs = probs,na.rm=na.rm ), sapply(data.frame(ret),sd)) , sub.dat[1,as.character(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE)
                                     }
                                 } else {
                                     ret   <- Hmisc::wtd.quantile(x = sub.dat[,imp[["dep"]]], weights = sub.dat[,wgt], probs = probs,na.rm=na.rm )
                                     ret   <- data.frame (group="noch_leer", depVar = "noch_leer", modus = "noch_leer", parameter = rep(as.character(probs),2), coefficient = rep(c("est","se"),each=length(probs)), value = c(ret, rep(NA, length(probs))) , sub.dat[1,as.character(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE)
                                 }
                              }
                              return(ret)}))
                      if(!all(as.character(imp[-length(imp)]) == names(imp[-length(imp)])))  {colnames(ret) <- car::recode(colnames(ret), paste("'",as.character(imp[-length(imp)]) , "' = '" , names(imp[-length(imp)]), "'" ,sep = "", collapse="; ") )}
                      return(facToChar(ret))}


jackknife.quantile <- function ( imp, dat.i, dep, replicates , ID, wgt , probs, na.rm ) {
                      cat("."); flush.console()
                      # if(!exists("svrepdesign"))      {library(survey)}
                      design         <- survey::svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep) ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                      formel         <- as.formula(paste("~ ",imp[["dep"]], sep = "") )
                      quantile.imp   <- survey::svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svyquantile, quantiles = probs, return.replicates = TRUE, na.rm = na.rm)
                      molt           <- reshape2::melt(data=quantile.imp, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                      molt[,"parameter"]   <- remove.non.numeric(as.character(molt[,"variable"]))
                      recString      <- paste("'",names(table(molt[,"parameter"])) , "' = '" , as.character(probs), "'" ,sep = "", collapse="; ")
                      molt[,"parameter"]   <- car::recode(molt[,"parameter"], recString)
                      molt[,"coefficient"] <- car::recode(remove.numeric(as.character(molt[,"variable"])), "'V'='est'")
                      if(!all(as.character(imp[-length(imp)]) == names(imp[-length(imp)])))  {colnames(molt) <- car::recode(colnames(molt), paste("'",as.character(imp[-length(imp)]) , "' = '" , names(imp[-length(imp)]), "'" ,sep = "", collapse="; ") )}
                      return(facToChar(data.frame ( group = "noch_leer", depVar = "noch_leer", modus = "noch_leer", molt[,c("parameter", "coefficient", "value", names(imp)[-length(imp)])], stringsAsFactors = FALSE))) }


conv.table      <- function ( imp, dat.i, dep,  depN, ID , wgt , catPrms, separate.missing.indikator  ) {
                   stopifnot(length(imp[["dep"]]) == 1 )
                   stopifnot( !is.null( attr(dep,"expected") ))
                   table.cast <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                                 prefix <- data.frame(sub.dat[1,as.character(imp[-length(imp)]), drop=FALSE], row.names = NULL, stringsAsFactors = FALSE )
                                 foo    <- make.indikator(variable = sub.dat[,as.character(imp[length(imp)])], name.var = "ind", force.indicators =attr(dep,"expected"), separate.missing.indikator = ifelse(attr(dep, "separate.missing")==TRUE, "always","no"))
                                 if(all(dat.i[,wgt] == 1)) {ret    <- data.frame ( prefix , desk(foo[,-1, drop = FALSE],na.rm=TRUE)[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )
                                 } else { ret    <- data.frame ( prefix , desk(foo[,-1, drop = FALSE], p.weights = sub.dat[,wgt],na.rm=TRUE)[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )}
                                 ret[,"parameter"] <- substring(rownames(ret),5)
                                 return(ret)}) )
                   ret        <- reshape2::melt(table.cast, measure.vars = c("Mittelwert", "std.err"), na.rm=TRUE)
                   ret[,"coefficient"] <- car::recode(ret[,"variable"], "'Mittelwert'='est'; 'std.err'='se'")
                   recodeStr  <- paste("'",as.character(imp[-length(imp)]) , "' = '" , names(imp[-length(imp)]), "'" ,sep = "", collapse="; ")
                   if(!all(as.character(imp[-length(imp)]) == names(imp[-length(imp)])))  {colnames(ret) <- car::recode(colnames(ret), recodeStr)}
                   ret        <- facToChar ( data.frame ( group = "noch_leer", depVar = depN, modus = "noch_leer", ret[,c("coefficient", "parameter")], value = ret[,"value"], ret[,names(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE) )
                   return(ret)}


jackknife.table <- function ( imp, dat.i, dep,  depN, replicates , ID , wgt , catPrms  ) {
                   cat("."); flush.console()
                   stopifnot(length(imp[["dep"]]) == 1 )
                   stopifnot( !is.null( attr(dep,"expected") ))
                   dat.ana  <- dat.i
                   if(attr(dep, "separate.missing") == TRUE ) {
                      original.levels <- sort(c(attr(dep,"expected"), "missing"))
                      weg             <- which(is.na(dat.ana[,imp[["dep"]]]))
                      if ( length(weg)>0) {
                           dat.ana[weg,imp[["dep"]]] <- "missing"
                      }
                   }  else {
                      original.levels <- sort(attr(dep,"expected"))
                      weg             <- which(is.na(dat.ana[,imp[["dep"]]]))
                      if(length(weg)>0) {
                         cat(paste(" Warning: No seperate missing categorie was chosen. ", length(weg), " missings were found anyhow for variable ",imp[["dep"]],". Missings will be deteted from the data.\n",sep=""), file=catPrms$file, append=catPrms$append)
                         dat.ana   <- dat.ana[-weg,]
                      }
                   }
                   n.levels  <- table(original.levels)
                   n.double  <- which(n.levels > 1 )
                   if(length(n.double)>0) {
                      cat(paste("    Warning! ",length(n.double), " multiple level indicators of dependent variable found:\n",sep=""), file=catPrms$file, append=catPrms$append)
                      cat("    "); print(n.double)
                      if(append == TRUE) { cat(paste(n.double, "\n",sep=""), file=catPrms$file, append=catPrms$append)  }
                      cat("    Double level indicators will be merged to one common indicator.\n", file=catPrms$file, append=catPrms$append)
                      original.levels <- unique(original.levels)
                   }
                   dat.ana$what.I.want <- factor(dat.ana[,imp[["dep"]]], levels = original.levels)
                   # if(!exists("svrepdesign"))      {library(survey)}
                   design    <- survey::svrepdesign(data = dat.ana[,c(as.character(imp[-length(imp)]), "what.I.want")], weights = dat.ana[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[match(dat.ana[,ID], replicates[,"ID"], ),-1,drop = FALSE], combined.weights = TRUE, mse = TRUE)
                   means     <- survey::svyby(formula = ~factor(what.I.want, levels = original.levels), by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svymean, deff = FALSE, return.replicates = TRUE)
                   cols      <- match(paste("factor(what.I.want, levels = original.levels)",original.levels,sep=""), colnames(means))
                   colnames(means)[cols] <- paste("est",original.levels, sep="____________")
                   cols.se   <- grep("^se[[:digit:]]{1,5}$", colnames(means) )
                   stopifnot(length(cols) == length(cols.se))
                   colnames(means)[cols.se] <- paste("se____________", original.levels, sep="")
                   molt      <- reshape2::melt(data=means, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                   recodeStr <- paste("'",as.character(imp[-length(imp)]) , "' = '" , names(imp[-length(imp)]), "'" ,sep = "", collapse="; ")
                   if(!all(as.character(imp[-length(imp)]) == names(imp[-length(imp)])))  {colnames(molt) <- car::recode(colnames(molt), recodeStr)}
                   splits    <- data.frame ( do.call("rbind", strsplit(as.character(molt[,"variable"]),"____________")), stringsAsFactors = FALSE)
                   colnames(splits) <- c("coefficient", "parameter")
                   ret       <- facToChar( data.frame ( group = "noch_leer", depVar = depN, modus = "noch_leer", splits, value = molt[,"value"], molt[,names(imp[-length(imp)]),drop=FALSE], stringsAsFactors = FALSE) )
                   return(ret)}


conv.mean      <- function (imp, dat.i , dep, ID, wgt, na.rm, group.differences.by, workbook,group.delimiter,nam  )   {
                  deskr    <- do.call("rbind", by(data = dat.i[,c(as.character(imp[-length(imp)]), dep, wgt)], INDICES = dat.i[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                              prefix <- sub.dat[1,as.character(imp[-length(imp)]), drop=FALSE]
                              if(is.null(wgt)) useWGT <- NULL else useWGT <- sub.dat[,wgt]
                              ret    <- data.frame ( nValidUnweightd = length(na.omit(sub.dat[, imp[["dep"]] ])), prefix, desk(sub.dat[, imp[["dep"]] ], p.weights = useWGT, na.rm=na.rm)[,c("N", "N.valid", "Mittelwert", "std.err", "Varianz", "Streuung")], stringsAsFactors = FALSE)
                              names(ret) <- c( "nValidUnweightd", names(imp[-length(imp)]) , "Ncases", "NcasesValid", "mean", "se.mean", "var","sd")
                              return(ret)}))
                  if(!is.null(group.differences.by))   {
                      m            <- deskr
                      m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[names(imp[-length(imp)])], collapse = "."))})
                      m$all.group  <- 1
                      res.group    <- tempR <- setdiff(names(workbook$group.origin), group.differences.by)
                      if(length(res.group) == 0 ) {res.group <- "all.group"} else {res.group <- names(imp[-length(imp)][res.group])}
                      kontraste      <- expand.grid(1:length(table(m[,group.differences.by])), 1:length(table(m[,group.differences.by])))
                      weg            <- which(apply(kontraste, 1, FUN = function ( x ) {x[1] >= x[2]}))
                      kontraste      <- kontraste[-weg,]
                      recodeString   <- paste("'", 1:length(table(m[,group.differences.by])),"' = '", names(table(m[,group.differences.by])),"'", sep = "", collapse = "; ")
                      kontraste      <- data.frame ( lapply(kontraste, FUN = function ( x ) {car::recode(x, recodeString)}), stringsAsFactors = FALSE )
                      difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                        ret <- do.call("rbind", apply(kontraste, 1, FUN = function ( k ) {
                                               vgl.iii   <- iii[iii[,group.differences.by] %in% k ,]
                                               true.diff <- diff(vgl.iii[,"mean"])
                                               scumm     <- sapply(vgl.iii[,res.group,drop = FALSE], as.character)
                                               group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                               dummy     <- unlist(lapply(nam[["groupSet"]], FUN = function (l){l[1]}))
                                               dummyVar  <- expand.grid(lapply(dummy, FUN = function (d ) { as.character(unique(dat.i[,d]))}))
                                               dummyVar  <- data.frame(lapply(dummyVar, FUN = function ( xx ) { xx <- NA}))
                                               dif.iii   <- data.frame(group = paste(group, paste(k, collapse = ".vs."),sep="____"), parameter = "meanGroupDiff", coefficient = c("est","se"), value = c(true.diff, sqrt( sum(vgl.iii[,"sd"]^2 / vgl.iii[,"nValidUnweightd"]) )) , dummyVar, stringsAsFactors = FALSE )
                                               return(dif.iii)}))               ### siehe http://www.vassarstats.net/dist2.html
                                        return(ret)})) }                        ### http://onlinestatbook.com/2/tests_of_means/difference_means.html
                  deskrR   <- reshape2::melt(data = deskr, id.vars = names(imp)[-length(imp)], measure.vars = setdiff(colnames(deskr), c("nValidUnweightd",names(imp)[-length(imp)]) ), na.rm=TRUE)
                  deskrR[,"coefficient"] <- car::recode(deskrR[,"variable"], "'se.mean'='se';else='est'")
                  deskrR[,"parameter"]   <- gsub("se.mean","mean",deskrR[,"variable"])
                  deskrR   <- data.frame ( group = apply(deskrR[,names(imp)[-length(imp)],drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), deskrR[,c( "parameter", "coefficient", "value", names(imp)[-length(imp)])], stringsAsFactors = FALSE)
                  if(!is.null(group.differences.by))   {return(facToChar(rbind(deskrR,difs)))} else {return(facToChar(deskrR))}}


jackknife.mean <- function (imp, dat.i , dep, replicates , ID, wgt, na.rm, group.differences.by, workbook,group.delimiter,nam  )   {
                  cat("."); flush.console()
                  dat.i[,"N_weighted"]      <- 1
                  dat.i[,"N_weightedValid"] <- 1
                  if( length(which(is.na(dat.i[,imp[["dep"]]]))) ) { dat.i[which(is.na(dat.i[,imp[["dep"]]])), "N_weightedValid" ] <- 0 }
                  # if(!exists("svrepdesign"))      {library(survey)}
                  design         <- survey::svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep,"N_weighted","N_weightedValid") ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                  rets           <- data.frame ( target = c("Ncases", "NcasesValid", "mean", "var"), FunctionToCall = c("svytotal","svytotal","svymean","svyvar"), formelToCall = c("paste(\"~ \", \"N_weighted\",sep=\"\")","paste(\"~ \", \"N_weightedValid\",sep=\"\")","paste(\"~ \",imp[[\"dep\"]], sep = \"\")","paste(\"~ \",imp[[\"dep\"]], sep = \"\")"), naAction = c("FALSE","TRUE","na.rm","na.rm"), stringsAsFactors = FALSE)
                  ret            <- apply(rets, 1, FUN = function ( toCall ) {  ### svyby wird dreimal aufgerufen ...
                                    do   <- paste(" res <- survey::svyby(formula = as.formula(",toCall[["formelToCall"]],"), by = as.formula(paste(\"~\", paste(as.character(imp[-length(imp)]), collapse = \" + \"))), design = design, FUN = ",toCall[["FunctionToCall"]],",na.rm=",toCall[["naAction"]],", deff = FALSE, return.replicates = TRUE)",sep="")
                                    eval(parse(text=do))
                                    resL <- reshape2::melt( data = res, id.vars = as.character(imp[-length(imp)]), variable.name = "coefficient" , na.rm=TRUE)
                                    stopifnot(length(table(resL[,"coefficient"])) == 2)
                                    resL[,"coefficient"] <- car::recode(resL[,"coefficient"], "'se'='se'; else ='est'")
                                    resL[,"parameter"]   <- toCall[["target"]]
                                    attr(resL, "original") <- res
                                    return(resL)})
                  sds            <- do.call("rbind", by(data = dat.i, INDICES =  dat.i[,as.character(imp[-length(imp)])], FUN = function (uu) {
                                    namen          <- sapply(uu[,as.character(imp[-length(imp)]), drop = FALSE], FUN = function ( yy ) {
                                                      retu <- table(yy)
                                                      return(names(retu)[retu>0]) })
                                    sub.replicates <- replicates[ match(uu[,ID], replicates[,"ID"] ) ,  ]
                                    design.uu      <- survey::svrepdesign(data = uu[ ,c(as.character(imp[-length(imp)]), dep)], weights = uu[,wgt], type="JKn", scale = 1, rscales = 1, repweights = sub.replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                                    var.uu         <- survey::svyvar(x = as.formula(paste("~",imp[["dep"]],sep="")), design = design.uu, deff = FALSE, return.replicates = TRUE, na.rm = na.rm)
                                    ret            <- data.frame(t(namen), est = as.numeric(sqrt(coef(var.uu))), se =  as.numeric(sqrt(vcov(var.uu)/(4*coef(var.uu)))), stringsAsFactors = FALSE )
                                    return(ret)}) )
                  sds             <- data.frame ( reshape2::melt(data = sds, id.vars = as.character(imp[-length(imp)]), variable.name = "coefficient" , na.rm=TRUE), parameter = "sd", stringsAsFactors = FALSE)
                  resultsAll      <- rbind(do.call("rbind",ret), sds)
                  resultsAll      <- data.frame ( group = apply(resultsAll[,as.character(imp[-length(imp)]),drop=FALSE],1,FUN = function (z) {paste(z,collapse=group.delimiter)}), resultsAll[,c("parameter","coefficient","value",as.character(imp[-length(imp)]))] , stringsAsFactors = FALSE)
                  recodeStr       <- paste("'",as.character(imp[-length(imp)]) , "' = '" , names(imp[-length(imp)]), "'" ,sep = "", collapse="; ")
                  if(!all(as.character(imp[-length(imp)]) == names(imp[-length(imp)])))  {colnames(resultsAll) <- car::recode(colnames(resultsAll), recodeStr)}
                  if(!is.null(group.differences.by))   {
                      m            <- attr(ret[[ which(rets[,"target"] == "mean") ]], "original")
                      m$comb.group <- apply(m, 1, FUN = function (ii) {crop(paste( ii[as.character(imp[-length(imp)])], collapse = "."))})
                      repl         <- data.frame(t(attr(attr(ret[[which(rets[,"target"] == "mean")]], "original"), "replicates")), stringsAsFactors = FALSE )
                      repl[,"comb.group"] <- rownames(repl)
                      m              <- merge(m, repl, by = "comb.group" )
                      m$all.group    <- 1
                      res.group      <- tempR  <- setdiff(names(workbook$group.origin), group.differences.by)
                      if(length(res.group) == 0 ) {res.group <- "all.group"} else {res.group <- as.character(imp[-length(imp)][res.group])}
                      kontraste      <- expand.grid(1:length(table(m[,imp[[group.differences.by]]])), 1:length(table(m[,imp[[group.differences.by]]])))
                      weg            <- which(apply(kontraste, 1, FUN = function ( x ) {x[1] >= x[2]}))
                      kontraste      <- kontraste[-weg,]
                      recodeString   <- paste("'", 1:length(table(m[,imp[[group.differences.by]]])),"' = '", names(table(m[,imp[[group.differences.by]]])),"'", sep = "", collapse = "; ")
                      kontraste      <- data.frame ( lapply(kontraste, FUN = function ( x ) {car::recode(x, recodeString)}), stringsAsFactors = FALSE )
                      difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                        ret <- do.call("rbind", apply(kontraste, 1, FUN = function ( k ) {
                                               vgl.iii   <- iii[iii[,imp[[group.differences.by]]] %in% k ,]
                                               true.diff <- diff(vgl.iii[,which(colnames(vgl.iii) %in% dep)])
                                               cols      <- grep("^X[[:digit:]]{1,3}$", colnames(vgl.iii) )
                                               other.diffs <- apply(vgl.iii[,cols], 2, diff)
                                               scumm     <- sapply(vgl.iii[,res.group,drop = FALSE], as.character)
                                               if ( length(tempR)>0) {colnames(scumm) <- names(imp[-length(imp)])[match( colnames(scumm), imp[-length(imp)])]}
                                               group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                               dif.iii   <- data.frame(group = group, vgl = paste(k, collapse = ".vs."), dif = true.diff, se =  sqrt(sum((true.diff - other.diffs)^2)), stringsAsFactors = FALSE )
                                               return(dif.iii)}))
                                        return(ret)}))
                      difsL          <- data.frame ( reshape2::melt(data = difs, measure.vars = c("dif", "se") , variable.name = "coefficient" , na.rm=TRUE), parameter = "meanGroupDiff", stringsAsFactors = FALSE)
                      difsL[,"coefficient"] <- car::recode(difsL[,"coefficient"], "'se'='se'; else = 'est'")
                      dummy      <- unlist(lapply(nam[["groupSet"]], FUN = function (l){l[1]}))
                      dummyVar   <- expand.grid(lapply(dummy, FUN = function (d ) { as.character(unique(dat.i[,d]))}))
                      dummyVar   <- data.frame(lapply(dummyVar, FUN = function ( xx ) { xx <- NA}))
                      difsL      <- data.frame ( group = apply(difsL[,c("group","vgl")],1,FUN = function (z) {paste(z,collapse="____")}), difsL[,c("parameter","coefficient", "value")], dummyVar, stringsAsFactors = FALSE)
                      resultsAll <- rbind(resultsAll,difsL)
                  }
                  return(facToChar(resultsAll)) }


### Wiewohl die Funktion "median" bereits existiert, soll ihre Arbeitsweise nochmals demonstriert werden.
### Zuerst muß unterschieden werden, ob die Variable eine gerade oder ungerade Anzahl von Elementen hat.
### Dies kann man tun, indem man die Anzahl der Elemente halbiert und diese Zahl mit dem Befehl "round"
### rundet. Ist die gerundete Zahl mit der ungerundeten identisch, hat die Variable eine gerade Anzahl von
### Elementen.
### Als nächstes muß die Variable mit dem "sort"-Befehl sortiert werden. Bei einer ungeraden Anzahl von
### Elementen ist der Median der Wert Nummer (n+1)/2 dieser sortierten Variablen.
### Bei einer geraden Anzahl von Elementen ist der Median der Mittelwert des n/2 und (n+2)/2 Elements die-
### ser sortierten Variablen. --- 15. Mai 2007
### (Die Funktion "show.median" ist seit dem 2. April 2008 Bestandteil der Funktion "desk".)
desk <- function(variable,na=NA, p.weights = NULL, na.rm = FALSE) {
         variable <- as.numeric.if.possible( data.frame(as.matrix(variable),stringsAsFactors = FALSE), verbose = FALSE )
         if(!is.null(p.weights)) {
             Mis.weight <- FALSE
             stopifnot( length(p.weights) == nrow(variable) )
             # if(!exists("wtd.mean"))      {library(Hmisc)}
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
                        Mittelwert <- Hmisc::wtd.mean(x = y, weights = p.weights, na.rm = na.rm)
                        Varianz    <- Hmisc::wtd.var(y, na.rm = na.rm)
                        N          <- sum(p.weights)
                        N.valid    <- sum(p.weights[which(!is.na(y))]) }
                     dataFrame <- data.frame ( N = N, N.valid = N.valid, Missing = length(y) - length(na.omit(y)), Minimum = min(y, na.rm = na.rm), Maximum = max(y, na.rm = na.rm), Summe = Summe, Mittelwert = Mittelwert, std.err = sd(y, na.rm = na.rm) / sqrt(length(na.omit(y))), sig = ifelse(length(table(y))==1, NA, t.test(x = y)$p.value), Median = median(y, na.rm = na.rm), Streuung = sqrt(Varianz), Varianz = Varianz , stringsAsFactors = FALSE )
                     return(dataFrame)} ))
         rownames(ret) <- colnames(variable)
         return(ret)}
