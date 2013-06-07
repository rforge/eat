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
### complete.permutation  Wenn Anzahl der Imputationen von unabhängiger und abhängiger Variable varriiert
jk2.mean <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), group.differences.by = NULL, dependent = list(), na.rm = FALSE, complete.permutation = c("nothing", "groups", "all"), forcePooling = TRUE, boundary = 3)    {
            nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
            JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = forcePooling )
            if(!is.null(JK$JK)) {
                forcePooling <- JK$forcePooling
                complete.permutation <- match.arg ( complete.permutation )
               # if(!exists("melt"))       {library(reshape2)}
                dependent     <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                catPrms       <- setCatParameter(dat)
                if(!is.null(group.differences.by)) {
                   if(!group.differences.by %in% names(nam[["groupSet"]])) {stop()} }
                if(is.null(wgt))   {
                   cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                   dat$weight_one <- 1
                   wgt <- "weight_one"
                }
              #  if(!exists("svrepdesign"))      {library(survey)}
                if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )}
                if(length(nam[["groupSet"]]) == 0) {
                   cat("No group(s) specified. Analyses will be computed only for the whole sample.\n", file=catPrms$file, append=catPrms$append)
                   dat$whole_group <- "whole_group"
                   nam[["groupSet"]]  <- list(whole_group = "whole_group")
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
                analysis    <- lapply(dependent, FUN = function ( dep ) {
                               workbook <- .generateWorkbook(group = group, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                               cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                               if(JK$JK == TRUE)   {ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.mean, dat.i = dat.i, dep = dep, replicates = replicates, ID = ID, wgt = wgt, na.rm = na.rm , group.differences.by = group.differences.by, workbook = workbook)}
                               if(JK$JK == FALSE)  {ana <- apply(workbook$workbook, MARGIN = 1, FUN = conv.mean, dat.i = dat.i, dep = dep, ID = ID, wgt = wgt, na.rm = na.rm , group.differences.by = group.differences.by, workbook = workbook)}
                               if(!is.null(attr(dep, "nested")))   {
                                   if(length(ana)>1)    {cat("\nPooling Standard errors. Underlying nested structure is assumed.\n", file=catPrms$file, append=catPrms$append)}
								   struktur <- attr(dep, "nested")
                                   if(is.null(names(struktur)) )   { names(struktur) <- paste("n",gsub(" ","0",formatC(1:length(struktur), width = nchar(length(struktur)))), sep="") }
                               }  else  {struktur <- list(n1 = dep, n2 = NULL) 
						                 if(length(ana)>1)    {cat("\nPooling Standard errors. Assume no nested structure.\n", file=catPrms$file, append=catPrms$append)}	   
							   }
                               if(length(ana)>1)    {                               ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                  anaL      <- data.frame( do.call("rbind", lapply(ana, FUN = function (a) {reshape2::melt(data = a, measure.vars = intersect(dep, colnames(a)) , na.rm=TRUE)}) ), nesting = factor(NA , levels = names(struktur)), stringsAsFactors = FALSE )
                                  for ( x in 1 : length(struktur)) {if(!is.null(struktur[[x]])) {anaL[wo.sind(struktur[[x]], anaL[,"variable"], quiet = TRUE),"nesting"] <- names(struktur)[x]}  }
                                  expect.ses <- c("se.N_weighted", "se.mean", "se.variance", "se.N_unweighted", "se.SD")
                                  seColumns  <- match(expect.ses, colnames(anaL))
                                  stopifnot(length(seColumns) == length(expect.ses) & length(na.omit(seColumns)) == length(expect.ses))
                                  for ( x in seColumns)    {                        ### ersetze ggf. missings in Standardfehlern
                                        mis <- which(is.na(anaL[,x]))
                                        if(length(mis)>0) {
                                          # cat(paste("Unexpected missings in standard errors of ",colnames(anaL)[x],".\n", sep = ""), file=catPrms$file, append=catPrms$append)
                                           if(length(mis) > boundary & forcePooling == TRUE) {
                                            #  cat(paste("Amount of missings in standard errors of ",colnames(anaL)[x]," exceeds 'boundary' criteria. \n    I will not pool the results. Increase 'boundary' criteria at your own risk.\n", sep = ""), file=catPrms$file, append=catPrms$append)
                                           }
                                           if(length(mis) <= boundary & forcePooling == TRUE)    {
                                              cat("Replace missing standard errors by '0'.\n", file=catPrms$file, append=catPrms$append)
                                              anaL[mis ,x] <- 0
                                           }
                                        }
                                  }
                                  retList <- by(data = anaL, INDICES = anaL[, names(workbook$group.origin)], FUN = function ( u ) {
                                             nested  <- by ( data = u, INDICES = u[,"nesting"], FUN = function ( uu ) { return(uu)}) })
                                  pooled  <- do.call("rbind", lapply(retList, FUN = function ( p ) {
                                             N.unweighted <- pool.means ( m = lapply(p, FUN = function ( pp ) { pp[,"N_unweighted"]}), se = lapply(p, FUN = function ( pp ) { pp[,"se.N_unweighted"]}) )$summary[c("m.pooled","se.pooled")]
                                             N.weighted   <- pool.means ( m = lapply(p, FUN = function ( pp ) { pp[,"N_weighted"]}), se = lapply(p, FUN = function ( pp ) { pp[,"se.N_weighted"]}) )$summary[c("m.pooled","se.pooled")]
                                             m   <- pool.means ( m = lapply(p, FUN = function ( pp ) { pp[,"value"]}), se = lapply(p, FUN = function ( pp ) { pp[,"se.mean"]}) )$summary[c("m.pooled","se.pooled")]
                                             SD  <- pool.means ( m = lapply(p, FUN = function ( pp ) { pp[,"SD"]}), se = lapply(p, FUN = function ( pp ) { pp[,"se.SD"]}) )$summary[c("m.pooled","se.pooled")]
                                             VAR <- pool.means ( m = lapply(p, FUN = function ( pp ) { pp[,"variance"]}), se = lapply(p, FUN = function ( pp ) { pp[,"se.variance"]}) )$summary[c("m.pooled","se.pooled")]
                                             ret <- data.frame( p[[1]][1,names(workbook$group.origin), drop = FALSE], N.unweighted, N.weighted, m, SD, VAR, stringsAsFactors = FALSE )
                                             colnames(ret) <- c ( names(workbook$group.origin), "N_unweighted", "se.N_unweighted", "N_weighted", "se.N_weighted", "mean", "se.mean", "SD", "se.SD", "variance", "se.variance" )
                                             return(ret) }))
                               } else { cat("\n"); pooled <- ana[[1]] }
                               pooled.dif <- NULL
                               if(!is.null(attr(ana[[1]], "difs")))   {
                                   if(length(ana) == 1) {pooled.dif <- attr(ana[[1]], "difs")}
                                   if(length(ana) > 1)  {
                                      dif.frame    <- do.call("rbind", lapply(ana, FUN = function ( iii ) {reshape2::melt(attr(iii, "difs"), id.vars = "group")}))
                                      stopifnot(length(table(dif.frame[,"variable"])) == 2 ); stopifnot ( all ( names ( table (dif.frame[,"variable"])) == c("dif", "se")))
                                      pooled.dif   <- do.call("rbind", by(data = dif.frame, INDICES= dif.frame[,"group"], FUN = function ( iii ) {
                                                      d <- pool.means(m = iii[ iii[,"variable"] == "dif" , "value"] , se = iii[ iii[,"variable"] == "se" , "value"])$summary[c("m.pooled","se.pooled")]
                                                      return(d)}) )
                                   }
                               }
                               attr(pooled, "difference")     <- pooled.dif
                               attr(pooled, "unpooledList")   <- ana
                               if(length(ana)>1)  {attr(pooled, "unpooledFrame")  <- anaL}
                               return(pooled)
                })
            } else {
                analysis <- NULL
            }
        return(analysis)}


### multicore version of jk2.mean()
jk2.mean.M <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), dependent = list(), na.rm = FALSE, complete.permutation = c("nothing", "groups", "all"), forcePooling = TRUE, boundary = 3, multicoreOptions = list(n.cores = NULL, GBcore = NULL, tempFolder = NULL, nameLogfile = NULL) )    {
             if(is.null(multicoreOptions[["nameLogfile"]])) { multicoreOptions[["nameLogfile"]] <- "analyse.log" }
             beginn               <- Sys.time()
             complete.permutation <- match.arg ( complete.permutation )
             use.cores    <- chooseCores(cores = multicoreOptions[["n.cores"]], GBcore = multicoreOptions[["GBcore"]])
             groupList    <- .checkAndCreateGroupList ( dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = group , dependent = dependent, use.cores = use.cores)
             logFile      <- .manageMulticoreLogfile  (tempFolder = multicoreOptions[["tempFolder"]], nameLogfile = multicoreOptions[["nameLogfile"]])
             flush.console()                                                    ### Output der Console wird in multicore Verfahren nicht ausgegeben und muss in einer Datei zwischengespeichert werden
             mach.es <- function ( laufnummer, ...  ) {
                        if(!exists("jk2.mean"))    {library(eatRep)}
                        datS             <- dat                                 ### initialisiere Datensatz
                        if(length(groupList[["splitGroup"]])>0)    {
                           for (u in names(groupList[["splitGroup"]]))    {     ### selektiere Datensatz
                                datS        <- datS[datS[,groupList[["splitGroup"]][[u]]] == groupList[["groupList"]][laufnummer,u] , ]
                           }
                        }
                        attr(datS, "logFile") <- logFile
                        dep              <- dependent[groupList[["groupList"]][laufnummer,"dependent"]]
                        mod              <- jk2.mean(dat = datS , ID=ID , wgt=wgt , JKZone=JKZone , JKrep=JKrep , group=groupList[["restGroup"]], group.differences.by=NULL, dependent=dep , na.rm= na.rm, complete.permutation=complete.permutation, forcePooling=forcePooling, boundary = boundary)
                        if(length(groupList[["splitGroup"]])>0)  {
                           mod           <- lapply(mod, FUN = function (m) {
                                            groupNames <- groupList[["groupList"]][laufnummer,c(-1,-ncol(groupList[["groupList"]])), drop=FALSE]
                                            for (uu in 1:ncol(groupNames)) {
                                                 m[[colnames(groupNames)[uu]]] <- groupNames[1,uu]
                                            }
                                            return(m) })
                        }
                        return(mod)}
             cl <- makeCluster(groupList[["use.cores"]], type = "SOCK")
             counts  <- clusterApply(cl = cl, x = 1:nrow(groupList[["groupList"]]), fun = mach.es, tempFolder = multicoreOptions[["tempFolder"]], dat , ID , wgt , restGroup = groupList[["restGroup"]], JKZone , JKrep , dependent , na.rm, complete.permutation , forcePooling , boundary, groupList = groupList[["groupList"]], logFile = logFile )
             stopCluster(cl)
             ende    <- Sys.time()
             cat("Analysis finished: "); print(ende-beginn)
             return(counts) }


### separate.missing.indikator ... Soll eine separate Kategorie für missings definiert werden?
### expected.values            ... optional (und empfohlen): Vorgabe für erwartete Werte, vgl. "table.muster"
###                                kann entweder eine benannte Liste sein, mit Namen wie in "dependent", oder ein einfacher character Vektor, dann werden diese Vorgaben für alle abhängigen Variablen übernommen
###                                bleibt "expected.values" leer, dann wird es automatisch mit den Werten der Variablen in ihrer Gesamtheit belegt!
jk2.table <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), dependent = list(), separate.missing.indikator = FALSE, expected.values = list(), complete.permutation = c("nothing", "groups", "all") )    {
             nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
            JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE )
            if(!is.null(JK$JK)) {
                complete.permutation <- match.arg ( complete.permutation )
                dependent           <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                catPrms              <- setCatParameter(dat)
               # if(!exists("svrepdesign"))      {library(survey)}
                if(is.null(wgt))   {
                   cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                   dat$weight_one <- 1
                   wgt <- "weight_one"
                }
                if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )} else { replicates <-  NULL }
                if(length(nam[["groupSet"]]) == 0) {
                   cat("No group(s) specified. Analyses will be computed only for the whole sample.\n", file=catPrms$file, append=catPrms$append)
                   dat$whole_group <- "whole_group"
                   nam[["groupSet"]]  <- list(whole_group = "whole_group")
                }
                allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(nam[["groupSet"]]), dependent = unlist(dependent) )
                all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                dat.i       <- dat[,unlist(all.Names)]
                missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
                ### Missings duerfen nur in abhaengiger Variable auftreten!
                kritisch    <- setdiff( names(missings[missings!=0]) , unlist(dependent ) )
                if(length(kritisch)>0) {stop(paste("Found NAs in variable(s) ",paste(kritisch, collapse = ", "), "\n",sep = "") )}
                cat(paste("Found ",length(nam[["groupSet"]])," grouping variable(s).\n",sep=""), file=catPrms$file, append=catPrms$append)
                .checkGroupConsistency(dat = dat, group = nam[["groupSet"]])
                cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""), file=catPrms$file, append=catPrms$append)
                flush.console()
                if(length(expected.values)>0) {
                   if(class(expected.values) == "character")  {
                      expected.values <- lapply(dependent, FUN = function (uu) {expected.values})
                   }
                }
                if(length(expected.values) == 0) {
                    expected.values <- lapply(dependent, FUN = function (ii ) {names(table.unlist(dat[,ii, drop = FALSE]))})
                }
                if(class(separate.missing.indikator) == "logical")   {
                   separate.missing.indikator <- lapply(dependent, FUN = function (uu) {separate.missing.indikator})
                }
                names.dependent <- names(dependent)
                dependent   <- lapply(names(dependent), FUN = function(zz) {
                               ret  <- dependent[[zz]]
                               attr(ret, "expected") <- expected.values[[zz]]
                               attr(ret, "separate.missing") <- separate.missing.indikator[[zz]]
                               return(ret)
                })
                names(dependent) <- names.dependent
                if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(nam[["groupSet"]], stringsAsFactors = FALSE))} else { group <- nam[["groupSet"]] }
                analysis    <- lapply(dependent, FUN = function ( dep ) {
                               workbook <- .generateWorkbook(group = group, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                               cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                               ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.table, dat.i = dat.i, dep = dep , replicates = replicates, ID = ID, wgt = wgt, catPrms = catPrms)
                               # names(ana)             <- workbook$workbook[,"dep"]; cat("\n")
                               for ( a in 1 : length(ana) ) {ana[[a]][,"dep_imp"] <- workbook$workbook[a,"dep"] }
                               if(!is.null(attr(dep, "nested")))   {
                                   if(length(ana)>1)    {cat("\nPooling Standard errors. Underlying nested structure is assumed.\n", file=catPrms$file, append=catPrms$append)}
                                   struktur <- attr(dep, "nested")
                                   if(is.null(names(struktur)) )   { names(struktur) <- paste("n",gsub(" ","0",formatC(1:length(struktur), width = nchar(length(struktur)))), sep="") }
                               }  else  {struktur <- list(n1 = dep, n2 = NULL)
                                         if(length(ana)>1)    {cat("\nPooling Standard errors. Assume no nested structure.\n", file=catPrms$file, append=catPrms$append)}
                               }
                               if(length(ana)>1)    {                               ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                  if(length(workbook$group.origin)>0) {
                                     ana    <- lapply(ana, FUN = function( a ) {
                                               for ( aa in 1:length(workbook$group.origin)) { colnames(a) <- gsub( workbook$group.origin[[aa]][which(workbook$group.origin[[aa]] %in% colnames(a))], names(workbook$group.origin)[aa], colnames(a) )}
                                               return(a)})
                                  }
                                  anaL      <- do.call("rbind", ana)
                                #   anaL[,"dep_imp"] <- halve.string(string = rownames(anaL), pattern = "\\.", first = FALSE )[,1]
                                  pooled    <- do.call("rbind", by(data = anaL, INDICES = anaL[,c(names(workbook$group.origin), "suffix")], FUN = function ( p ) {
                                               m   <- lapply(struktur, FUN = function (ss) {p[p[,"dep_imp"] %in% ss , "mittelmean"]})
                                               se  <- lapply(struktur, FUN = function (ss) {p[p[,"dep_imp"] %in% ss , "se"]})
                                               ret <- data.frame( p[1,c(names(workbook$group.origin), "suffix")], pool.means(m = m, se = se)$summary[c("m.pooled","se.pooled")] , stringsAsFactors = FALSE ) }))
                               } else { pooled <- ana[[1]] }
                               return(pooled)
                })
            } else {
                analysis <- NULL
            }
        return(analysis)}


### multicore version of jk2.table()
jk2.table.M <- function(dat, ID, wgt = NULL,JKZone = NULL, JKrep = NULL, group = list(), dependent = list(), separate.missing.indikator = FALSE, expected.values = list(), complete.permutation = c("nothing", "groups", "all"), multicoreOptions = list(n.cores = NULL, GBcore = NULL, tempFolder = NULL, nameLogfile = NULL))    {
             if(is.null(multicoreOptions[["nameLogfile"]])) { multicoreOptions[["nameLogfile"]] <- "analyse.log" }
             beginn               <- Sys.time()
             complete.permutation <- match.arg ( complete.permutation )
             use.cores    <- chooseCores(cores = multicoreOptions[["n.cores"]], GBcore = multicoreOptions[["GBcore"]])
             groupList    <- .checkAndCreateGroupList ( dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = group , dependent = dependent, use.cores = use.cores)
             logFile      <- .manageMulticoreLogfile  (tempFolder = multicoreOptions[["tempFolder"]], nameLogfile = multicoreOptions[["nameLogfile"]])
             flush.console()
             mach.es <- function ( laufnummer, ...  ) {
                        if(!exists("jk2.mean"))    {library(eatRep)}
                        datS             <- dat                                 ### initialisiere Datensatz
                        if(length(groupList[["splitGroup"]])>0)    {
                           for (u in names(groupList[["splitGroup"]]))    {     ### selektiere Datensatz
                                datS        <- datS[datS[,groupList[["splitGroup"]][[u]]] == groupList[["groupList"]][laufnummer,u] , ]
                           }
                        }
                        attr(datS, "logFile") <- logFile
                        dep              <- dependent[groupList[["groupList"]][laufnummer,"dependent"]]
                        mod              <- jk2.table(dat = datS , ID=ID , wgt=wgt , JKZone=JKZone , JKrep=JKrep , group=groupList[["restGroup"]], dependent=dep ,  separate.missing.indikator=separate.missing.indikator, expected.values=expected.values, complete.permutation=complete.permutation)
                        if(length(groupList[["splitGroup"]])>0)  {
                           mod           <- lapply(mod, FUN = function (m) {
                                            groupNames <- groupList[["groupList"]][laufnummer,c(-1,-ncol(groupList[["groupList"]])), drop=FALSE]
                                            for (uu in 1:ncol(groupNames)) {
                                                 m[[colnames(groupNames)[uu]]] <- groupNames[1,uu]
                                            }
                                            return(m) })
                        }
                        return(mod)}
             cl <- makeCluster(groupList[["use.cores"]], type = "SOCK")
             counts  <- clusterApply(cl = cl, x = 1:nrow(groupList[["groupList"]]), fun = mach.es, tempFolder = multicoreOptions[["tempFolder"]], dat , ID , wgt , restGroup = groupList[["restGroup"]], JKZone , JKrep , dependent , separate.missing.indikator, expected.values, complete.permutation , groupList = groupList[["groupList"]], logFile )
             stopCluster(cl)
             ende    <- Sys.time()
             cat("Analysis finished: "); print(ende-beginn)
             return(counts) }


jk2.quantile <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), dependent = list(), probs = seq(0, 1, 0.25),  complete.permutation = c("nothing", "groups", "all") )    {
            nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = NULL  ) )
            JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE )
            if(!is.null(JK$JK)) {
                complete.permutation <- match.arg ( complete.permutation )
                dependent            <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                catPrms              <- setCatParameter(dat)
               # if(!exists("svrepdesign"))      {library(survey)}
                if(is.null(wgt))   {
                   cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                   dat$weight_one <- 1
                   wgt <- "weight_one"
                }
                if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )}
                if(length(nam[["groupSet"]]) == 0) {
                   cat("No group(s) specified. Analyses will be computed only for the whole sample.\n",file=catPrms$file, append=catPrms$append)
                   dat$whole_group <- "whole_group"
                   nam[["groupSet"]]           <- list(whole_group = "whole_group")
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
                analysis    <- lapply(dependent, FUN = function ( dep ) {
                               workbook <- .generateWorkbook(group = group, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                               cat(paste("Use ",nrow(workbook$workbook)," replication(s) overall.\n",sep=""), file=catPrms$file, append=catPrms$append)
                               if(JK$JK == TRUE )  {ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.quantile, dat.i = dat.i, dep=dep, replicates = replicates, ID = ID, wgt = wgt, probs = probs)}
                               if(JK$JK == FALSE ) {ana <- apply(workbook$workbook, MARGIN = 1, FUN = conv.quantile, dat.i = dat.i, dep=dep, ID = ID, wgt = wgt, probs = probs)}
                               for ( a in 1 : length(ana) ) {ana[[a]][,"dep_imp"] <- workbook$workbook[a,"dep"] }
                               if(!is.null(attr(dep, "nested")))   {
                                   if(length(ana)>1)    {cat("\nPooling Standard errors. Underlying nested structure is assumed.\n", file=catPrms$file, append=catPrms$append)}
                                   struktur <- attr(dep, "nested")
                                   if(is.null(names(struktur)) )   { names(struktur) <- paste("n",gsub(" ","0",formatC(1:length(struktur), width = nchar(length(struktur)))), sep="") }
                               }  else  {struktur <- list(n1 = dep, n2 = NULL)
                                         if(length(ana)>1)    {cat("\nPooling Standard errors. Assume no nested structure.\n", file=catPrms$file, append=catPrms$append)}
                               }
                               if(length(ana)>1)    {                               ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                  cat("\nPooling Standard errors.\n",file=catPrms$file, append=catPrms$append)
                                  if(length(workbook$group.origin)>0) {
                                     ana    <- lapply(ana, FUN = function( a ) {
                                               for ( aa in 1:length(workbook$group.origin)) { colnames(a) <- gsub( workbook$group.origin[[aa]][which(workbook$group.origin[[aa]] %in% colnames(a))], names(workbook$group.origin)[aa], colnames(a) )}
                                               return(a)})
                                  }
                                  anaL      <- do.call("rbind", ana)
                                  pooled    <- do.call("rbind", by(data = anaL, INDICES = anaL[,c(names(workbook$group.origin), "per.number")], FUN = function ( p ) {
                                               m   <- lapply(struktur, FUN = function (ss) {p[p[,"dep_imp"] %in% ss , "V"]})
                                               se  <- lapply(struktur, FUN = function (ss) {p[p[,"dep_imp"] %in% ss , "se"]})
                                               ret <- data.frame( p[1,c(names(workbook$group.origin), "per.number")], pool.means(m = m, se = se)$summary[c("m.pooled","se.pooled")] , stringsAsFactors = FALSE ) }))
                               } else { cat("\n"); pooled <- ana[[1]] }
                               return(pooled)
                })
            } else {
                analysis <- NULL
            }
        return(analysis)}


jk2.quantile.M <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), dependent = list(), probs = seq(0, 1, 0.25),  complete.permutation = c("nothing", "groups", "all"), multicoreOptions = list(n.cores = NULL, GBcore = NULL, tempFolder = NULL, nameLogfile = NULL))    {
             if(is.null(multicoreOptions[["nameLogfile"]])) { multicoreOptions[["nameLogfile"]] <- "analyse.log" }
             beginn               <- Sys.time()
             complete.permutation <- match.arg ( complete.permutation )
             use.cores    <- chooseCores(cores = multicoreOptions[["n.cores"]], GBcore = multicoreOptions[["GBcore"]])
             groupList    <- .checkAndCreateGroupList ( dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = group , dependent = dependent, use.cores = use.cores)
             logFile      <- .manageMulticoreLogfile  (tempFolder = multicoreOptions[["tempFolder"]], nameLogfile = multicoreOptions[["nameLogfile"]])
             flush.console()
             mach.es <- function ( laufnummer, ...  ) {
                        if(!exists("jk2.mean"))    {library(eatRep)}
                        datS             <- dat                                 ### initialisiere Datensatz
                        if(length(groupList[["splitGroup"]])>0)    {
                           for (u in names(groupList[["splitGroup"]]))    {     ### selektiere Datensatz
                                datS        <- datS[datS[,groupList[["splitGroup"]][[u]]] == groupList[["groupList"]][laufnummer,u] , ]
                           }
                        }
                        attr(datS, "logFile") <- logFile
                        dep              <- dependent[groupList[["groupList"]][laufnummer,"dependent"]]
                        mod              <- jk2.quantile(dat = datS , ID=ID , wgt=wgt , JKZone=JKZone , JKrep=JKrep , group=groupList[["restGroup"]], dependent=dep ,  probs = probs, complete.permutation=complete.permutation)
                        if(length(groupList[["splitGroup"]])>0)  {
                           mod           <- lapply(mod, FUN = function (m) {
                                            groupNames <- groupList[["groupList"]][laufnummer,c(-1,-ncol(groupList[["groupList"]])), drop=FALSE]
                                            for (uu in 1:ncol(groupNames)) {
                                                 m[[colnames(groupNames)[uu]]] <- groupNames[1,uu]
                                            }
                                            return(m) })
                        }
                        return(mod)}
             cl <- makeCluster(groupList[["use.cores"]], type = "SOCK")
             counts  <- clusterApply(cl = cl, x = 1:nrow(groupList[["groupList"]]), fun = mach.es, tempFolder = multicoreOptions[["tempFolder"]], dat , ID , wgt , restGroup = groupList[["restGroup"]], JKZone , JKrep , dependent , probs, complete.permutation , groupList = groupList[["groupList"]], logFile )
             stopCluster(cl)
             ende    <- Sys.time()
             cat("Analysis finished: "); print(ende-beginn)
             return(counts) }


jk2.glm <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), independent = list(), reg.statement = NULL, dependent = list(), complete.permutation = c("nothing", "groups", "independent", "all") , glm.family, forceSingularityTreatment = FALSE)    {
            nam  <- assignNames (x = list ( groupSet = group, dependentSet = dependent, independentSet = independent  ) )
            JK   <- doJK( dat = dat, JKZone = JKZone , JKrep = JKrep, forcePooling = FALSE )
            if(!is.null(JK$JK)) {
                complete.permutation <- match.arg ( complete.permutation )
                dependent            <- adjustDependentForNested ( dependent = nam[["dependentSet"]], complete.permutation = complete.permutation, group = nam[["groupSet"]] )
                catPrms              <- setCatParameter(dat)
                .GlobalEnv$glm.family <- glm.family                                 ### Hotfix!
               # if(!exists("NagelkerkeR2"))     {library(fmsb)}
               # if(!exists("svrepdesign"))      {library(survey)}
                if(!is.null(reg.statement)) {
                    if( !all ( unlist(lapply(names(nam[["independentSet"]]), FUN = function (u) {grep(u, reg.statement)})) == 1) )  {
                         stop("Regression statement contains variables not incorporated in independent variables list.\n")
                    }
                    regSplit <- unlist ( sapply( strsplit(reg.statement, "\\*|\\:|\\+|-")[[1]], crop ) )
                    if( !all ( unlist(lapply(regSplit, FUN = function (u) {length(grep(u, names(nam[["independentSet"]]))) })) > 0) )  {
                         stop("Regression statement contains variables not incorporated in independent variables list.\n")
                    }
                }
                if(is.null(wgt))   {
                   cat("No weights specified. Use weight of 1 for each case.\n",file=catPrms$file, append=catPrms$append)
                   dat$weight_one <- 1
                   wgt <- "weight_one"
                }
                if(JK$JK == TRUE )  {replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )} else {replicates <- NULL }
                if(length(nam[["groupSet"]]) == 0) {
                   cat("No group(s) specified. Analyses will be computed only for the whole sample.\n",file=catPrms$file, append=catPrms$append)
                   dat$whole_group <- "whole_group"
                   nam[["groupSet"]] <- list(whole_group = "whole_group")
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
                analysis    <- lapply(dependent, FUN = function ( dep ) {
                               workbook <- .generateWorkbook(group = pre.workbook, dep = dep, complete.permutation.all = (complete.permutation == "all") )
                               cat(paste("Use ",nrow(workbook$workbook)," replications overall.\n",sep=""))
                               ana <- apply(workbook$workbook, MARGIN = 1, FUN = jackknife.glm, group = group, reg.statement = reg.statement, glm.family = glm.family, independent = independent, dat.i = dat.i, replicates = replicates, ID = ID, wgt = wgt, forceSingularityTreatment = forceSingularityTreatment )
                               names(ana)          <- workbook$workbook[,"dep"]
                               if(!is.null(attr(dep, "nested")))   {
                                   if(length(ana)>1)    {cat("\nPooling Standard errors. Underlying nested structure is assumed.\n", file=catPrms$file, append=catPrms$append)}
                                   struktur <- attr(dep, "nested")
                                   if(is.null(names(struktur)) )   { names(struktur) <- paste("n",gsub(" ","0",formatC(1:length(struktur), width = nchar(length(struktur)))), sep="") }
                               }  else  {struktur <- list(n1 = dep, n2 = NULL)
                                         if(length(ana)>1)    {cat("\nPooling Standard errors. assume no nested structure.\n", file=catPrms$file, append=catPrms$append)}
                               }
                               if(length(ana)>1)    {                               ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                                  cat("\nPooling Standard errors.\n",file=catPrms$file, append=catPrms$append)
                                  ana       <- lapply(names(ana), FUN = function ( a ) {
                                               ana[[a]][,"dep_imp"] <- a
                                               return(ana[[a]])})
                                  anaL      <- do.call("rbind", ana)
                                  pooled    <- do.call("rbind", by(data = anaL, INDICES = anaL[,names(group)], FUN = function ( p ) {
                                               subD <- do.call("rbind", by(data = p, INDICES = p[,"reg"], FUN = function ( ss ) {
                                                       choosen <- unlist(lapply(c("Estimate", "Std..Error", "r.squared", "r.nagelkerke", "N.valid"), FUN = function (zz) {
                                                                  ret <- lapply(struktur, FUN = function (mm) {
                                                                         return(ss[ss[,"dep_imp"] %in% mm , zz])}) }), recursive = FALSE )
                                                       ret     <- data.frame(ss[1,c(names(group), "reg","N", "N.valid")], pool.means(m = choosen[1:2], se = choosen[3:4])$summary[c("m.pooled","se.pooled")], pool.R2(r2 = choosen[5:6], N = choosen[9:10]), pool.R2(r2 = choosen[7:8], N = choosen[9:10]), stringsAsFactors = FALSE )
                                                       colnames(ret)[(ncol(ret)-5):ncol(ret)] <- c("beta", "se.beta", "R2", "se.R2", "R.nagelk", "se.R.nagelk")
                                                       return(ret)
                                               }) ) }))
                               } else { cat("\n"); pooled <- ana[[1]] }
                               attr(pooled, "singularities") <- unique(unlist(lapply(ana, FUN = function ( y ) { attr(y, "singularities")})))
                               return(pooled)
                })
            } else {
                analysis <- NULL
            }
        return(analysis)}


### multicore version of jk2.glm()
jk2.glm.M <- function(dat, ID, wgt = NULL, JKZone = NULL, JKrep = NULL, group = list(), independent = list(), reg.statement = NULL, dependent = list(), complete.permutation = c("nothing", "groups", "independent", "all") , glm.family, forceSingularityTreatment = FALSE, multicoreOptions = list(n.cores = NULL, GBcore = NULL, tempFolder = NULL, nameLogfile = NULL))    {
             if(is.null(multicoreOptions[["nameLogfile"]])) { multicoreOptions[["nameLogfile"]] <- "analyse.log" }
             beginn               <- Sys.time()
             complete.permutation <- match.arg ( complete.permutation )
             use.cores    <- chooseCores(cores = multicoreOptions[["n.cores"]], GBcore = multicoreOptions[["GBcore"]])
             groupList    <- .checkAndCreateGroupList ( dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = group , dependent = dependent, independent = independent, use.cores = use.cores)
             logFile      <- .manageMulticoreLogfile  (tempFolder = multicoreOptions[["tempFolder"]], nameLogfile = multicoreOptions[["nameLogfile"]])
             flush.console()
             mach.es <- function ( laufnummer, ...  ) {
                        if(!exists("jk2.mean"))    {library(eatRep)}
                        datS             <- dat                                 ### initialisiere Datensatz
                        if(length(groupList[["splitGroup"]])>0)    {
                           for (u in names(groupList[["splitGroup"]]))    {     ### selektiere Datensatz
                                datS        <- datS[datS[,groupList[["splitGroup"]][[u]]] == groupList[["groupList"]][laufnummer,u] , ]
                           }
                        }
                        attr(datS, "logFile") <- logFile
                        dep              <- dependent[groupList[["groupList"]][laufnummer,"dependent"]]
                        mod              <- jk2.glm(dat = datS , ID=ID , wgt=wgt , JKZone=JKZone , JKrep=JKrep , group=groupList[["restGroup"]], independent =independent, dependent=dep ,  reg.statement=reg.statement, complete.permutation=complete.permutation, glm.family=glm.family, forceSingularityTreatment = forceSingularityTreatment)
                        if(length(groupList[["splitGroup"]])>0)  {
                           mod           <- lapply(mod, FUN = function (m) {
                                            groupNames <- groupList[["groupList"]][laufnummer,c(-1,-ncol(groupList[["groupList"]])), drop=FALSE]
                                            for (uu in 1:ncol(groupNames)) {
                                                 m[[colnames(groupNames)[uu]]] <- groupNames[1,uu]
                                            }
                                            return(m) })
                        }
                        return(mod)}
             cl <- makeCluster(groupList[["use.cores"]], type = "SOCK")
             counts  <- clusterApply(cl = cl, x = 1:nrow(groupList[["groupList"]]), fun = mach.es, tempFolder = multicoreOptions[["tempFolder"]], dat , ID , wgt , restGroup = groupList[["restGroup"]], JKZone , JKrep , independent, dependent , reg.statement, complete.permutation , glm.family, forceSingularityTreatment, groupList = groupList[["groupList"]], logFile )
             stopCluster(cl)
             ende    <- Sys.time()
             cat("Analysis finished: "); print(ende-beginn)
             return(counts) }
             

doJK <- function ( dat, JKZone , JKrep, forcePooling ) {
        if(is.null(JKZone) | is.null(JKrep))   {
           cat("No jacknifing variables. Assume no cluster structure.\n")
           if(forcePooling == TRUE) {
              cat("'forcePooling' is set to 'FALSE' without a cluster structure. This is to avoid biased standard errors for variances and standard deviations.\n")
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


### GBcores ueberschreibt noetigenfalls cores
### GBcores sagt: wieviele GB pro core
### chooseCores(); chooseCores(12, 0.2)
chooseCores <- function(cores = NULL, GBcore = NULL) {
#             if(!exists("detectCores"))   {library(parallel)}
             n.cores <- detectCores()
             if(is.na(n.cores))     {cat("Cannot detect cores. Computer does not seem to be suited for multicore analysis.\n")}
             ram     <- memory.limit()
             if(!is.null(cores)) {
                use.cores <- as.integer(cores)
                if(use.cores == 1 ) {cat("Not useful to choose 1 core in multicore option.\n")}
                if(use.cores > n.cores) {
                   cat(paste("Fail to use ", use.cores," cores. Found only ",n.cores," cores which will now be purposed to use.\n",sep=""))
                   use.cores <- n.cores
                }
             } else { use.cores <- n.cores }
             if(!is.null(GBcore)) {
                if(GBcore > ram/1000 ) {
                   cat(paste("Warning: Not able to use ",GBcore, " giga byte per core. Only ",round(ram/1000,digits = 2)," giga byte found altogether.\n",sep=""))
                   GBcore <- ram/1000
                }
                use.cores.new <- (ram/1000) / GBcore
                if(use.cores.new < use.cores ) {use.cores <- use.cores.new}
             }
             return( use.cores ) }


### Hilfsfunktion fuer multigroup jackknife
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


### Hilfsfunktion fuer multicore jackknife
.manageMulticoreLogfile <- function ( tempFolder = NULL, nameLogfile ) {
             if(is.null(tempFolder) ) { logFile <- NULL} else {
                if (file.exists(file.path(tempFolder, nameLogfile)) )    {
                    cat(paste("    Logfile '",nameLogfile, "' already found.\n",sep=""))
                    nameLogfileSep <- as.vector(halve.string(string = nameLogfile, pattern="\\.", first = FALSE )  )
                    laufnr <- 0
                    while(file.exists(file.path(tempFolder, paste(nameLogfileSep[1],laufnr,".",nameLogfileSep[2],sep="")))) {
                          laufnr <- laufnr + 1
                    }
                    nameLogfile  <- paste(nameLogfileSep[1],laufnr,".",nameLogfileSep[2],sep="")
                    cat(paste("    Logfile renamed in '",nameLogfile,"'.\n",sep=""))
                 }
                 cat(paste("Note: Multicore analysis does not provide messages on console. Please check the logfile '",file.path(tempFolder,nameLogfile ),"' for possible warnings/problems.\n",sep=""))
                 logFile         <- file.path(tempFolder,nameLogfile )
                 foo             <- file.create(file.path(tempFolder,nameLogfile ))
              }
              return(logFile)}


assignNames <- function ( x = list ( groupSet = NULL, dependentSet = NULL, independentSet = NULL  ) ) {
               ret <- lapply(names(x), FUN = function (y) {
                      if ( length( names ( x[[y]] ) ) == 0 & length( x[[y]] ) > 0 )  {
                           names ( x[[y]] ) <- paste (y, 1:length(y),sep="")
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


jackknife.glm <- function (imp, group, reg.statement, glm.family, independent, dat.i , replicates , ID , wgt, forceSingularityTreatment ) {
                 group.names       <- as.character(imp[names(group)])
                 independent.names <- setdiff(as.character(imp), group.names)
                 independent.names <- setdiff(independent.names, as.character(imp[["dep"]]) )
                 cat("."); flush.console()
                 stopifnot(length(imp[["dep"]]) == 1 ); stopifnot(length(independent.names) == length( independent) )
                 sub.ana <- by(data = dat.i, INDICES = dat.i[,group.names], FUN = function (sub.dat) {
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
                                sub.replicates <- replicates[replicates[,"ID"] %in% sub.dat[,ID] ,-1, drop = FALSE]
                                design         <- svrepdesign(data = sub.dat[,as.character(imp)], weights = sub.dat[,wgt], type="JKn", scale = 1, rscales = 1, repweights = sub.replicates, combined.weights = TRUE, mse = TRUE)
                                if(length(singular) == 0 & forceSingularityTreatment == FALSE ) {
                                   glm.ii         <- svyglm(formula = formel, design = design, return.replicates = FALSE, family = glm.family)
                                }
                            }
                            r.squared      <- data.frame ( r.squared = var(glm.ii$fitted.values)/var(glm.ii$y) , N = nrow(sub.dat) , N.valid = length(glm.ii$fitted.values) )
                            r.nagelkerke   <- NagelkerkeR2(glm.ii)
                            res.bl         <- data.frame(group.values, reg = rownames(summary(glm.ii)$coefficients[,c(1:2)]), summary(glm.ii)$coefficients[,c(1:2)], r.squared, r.nagelkerke = r.nagelkerke$R2, stringsAsFactors = FALSE )
                            if(!is.null(replicates)) {                          ### jetzt kommt die Behandlung, wen zusaetzlich zu JK noch singularitaet auftritt. das ueberschreibt nun die bisherige "res.bl"
                                if(length(which(is.na(glm.ii$coefficients))) > 0 ) {
                                   cat(paste("Singularity problem in regression estimation for ", length(singular)," coefficient(s): ",paste(singular, collapse = ", "),". Try workaround ... \n", sep = "")); flush.console()
                                }
                                if(forceSingularityTreatment == TRUE ) {
                                   cat("Compute coefficients in the expectation of singularities ... \n"); flush.console()
                                }
                                if(length(singular) > 0 | forceSingularityTreatment == TRUE ) {
                                   if(!is.null(reg.statement)) { formel <- paste(imp[["dep"]],"~", reg.statement) } else {formel <- paste(imp[["dep"]],"~", paste( imp[names(independent)], collapse = " + "), sep = "")}
                                   string     <- paste("resRoh <- data.frame( withReplicates(design, quote(eatRep:::getOutputIfSingular(glm(formula = ",formel,", weights=.weights, family = ",glm.family$family,"(link=\"", glm.family$link,"\"))))), stringsAsFactors = FALSE)",sep="")
                                   eval ( parse ( text = string ) )
                                   colnames(resRoh) <- c("Estimate", "Std..Error")
                                   res.bl     <- data.frame ( group.values, reg = rownames( resRoh)[-((nrow(resRoh)-2) :nrow(resRoh))], resRoh[-((nrow(resRoh)-2) :nrow(resRoh)),], r.squared = resRoh[nrow(resRoh)-2,"Estimate"], N = nrow(sub.dat), N.valid = NA, r.nagelkerke = resRoh[nrow(resRoh),"Estimate"], stringsAsFactors = FALSE )
                                }
                            }
                            attr(res.bl, "singularities") <- singular           ### [1,] 0.075734 0.0143
                            return(res.bl) })
                 sub.ana1 <- do.call("rbind", sub.ana)
                 attr(sub.ana1, "singularities") <- unique(unlist(lapply(sub.ana, FUN = function ( y ) { attr(y, "singularities")})))
                 return(sub.ana1) }


### Hilfsfunktion fuer jk2.glm() wenn Regression singulaere Terme enthaelt
getOutputIfSingular <- function ( glmRes ) {
                       coefs <- na.omit(coef(glmRes))
                       coefs <- c(coefs, var(glmRes$fitted.values)/var(glmRes$y), unlist(NagelkerkeR2(glmRes)))
                       return(coefs)}


conv.quantile      <- function ( imp, dat.i, dep, ID, wgt , probs ) {
                      ret  <- do.call("rbind", by(data = dat.i, INDICES = dat.i[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                              if(is.null(wgt)) {
                                 ret   <- Hmisc::hdquantile(x = sub.dat[,imp[["dep"]]], se = TRUE, probs = probs )
                                 ret   <- data.frame(per.number = names(ret), se = attr(ret, "se"), V = ret, stringsAsFactors = FALSE )
                                 } else {
                                 ret   <- Hmisc::wtd.quantile(x = sub.dat[,imp[["dep"]]], weights = sub.dat[,wgt], probs = probs )
                                 ret   <- data.frame(per.number = as.numeric(remove.non.numeric(names(ret)))/100, se = NA, V = ret, stringsAsFactors = FALSE)
                              }
                              ret  <- data.frame(sub.dat[1,as.character(imp[-length(imp)]), drop = FALSE], ret, stringsAsFactors = FALSE, row.names = NULL)
                              return(ret)}))
                      return(ret)}


jackknife.quantile <- function ( imp, dat.i, dep, replicates , ID, wgt , probs ) {
                      cat("."); flush.console()
                      design         <- svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep) ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                      formel         <- as.formula(paste("~ ",imp[["dep"]], sep = "") )
                      quantile.imp   <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svyquantile, quantiles = probs, return.replicates = TRUE, na.rm = TRUE)
                      molt           <- reshape2::melt(data=quantile.imp, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                      xx             <- colsplit(gsub("([[:digit:]]+)", "\\.\\1", molt$variable), "\\.", names = c("var", "per.number"))
                      molt           <- data.frame(molt[,-match("variable", colnames(molt))], xx, stringsAsFactors = FALSE )
                      quantile.cast  <- reshape2::dcast(molt, ... ~ var)       ### Jetzt sollen noch die verwendeten Perzentilgroessen angebunden werden
                      probnamen      <- data.frame(Nummer = 1:length(probs), per.number = probs, stringsAsFactors = FALSE )
                      quantile.cast[,"per.number"] <- probnamen[ match(quantile.cast[,"per.number"], probnamen[,1])  ,2]
                      return(quantile.cast) }


jackknife.table <- function ( imp, dat.i, dep,  replicates , ID , wgt , catPrms  ) {
                   cat("."); flush.console()
                   stopifnot(length(imp[["dep"]]) == 1 )
                   original.levels   <- names(table(dat.i[,imp[["dep"]]]))
                   if(!is.null( attr(dep,"expected") ))   {
                       if(length(setdiff(original.levels, attr(dep,"expected"))) >0) {stop()}
                       original.levels <- sort(attr(dep,"expected"))
                   }
                   dat.ana  <- dat.i
                   if(attr(dep, "separate.missing") == TRUE ) {
                      original.levels <- sort(c(original.levels, "missing"))
                      weg             <- which(is.na(dat.ana[,imp[["dep"]]]))
                      if ( length(weg)>0) {
                           dat.ana[weg,imp[["dep"]]] <- "missing"
                      }
                   }  else {
                      weg <- which(is.na(dat.ana[,imp[["dep"]]]))
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
                   if(!is.null(replicates))  {
                       design    <- svrepdesign(data = dat.ana[,c(as.character(imp[-length(imp)]), "what.I.want")], weights = dat.ana[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[match(dat.ana[,"idstud"], replicates[,"ID"], ),-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                       means     <- svyby(formula = ~factor(what.I.want, levels = original.levels), by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svymean, deff = FALSE, return.replicates = TRUE)
                       cols      <- match(paste("factor(what.I.want, levels = original.levels)",original.levels,sep=""), colnames(means))
                       colnames(means)[cols] <- paste("mittelmean",original.levels, sep="____________")
                       cols.se   <- grep("^se[[:digit:]]{1,5}$", colnames(means) )
                       stopifnot(length(cols) == length(cols.se))
                       colnames(means)[cols.se] <- paste("se____________", original.levels, sep="")
                       molt           <- reshape2::melt(data=means, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                       molt           <- data.frame(molt[,-match("variable", colnames(molt))], as.numeric.if.possible(matrix(halve.string(molt[,"variable"], "____________"), ncol = 2, dimnames = list(NULL, c("var", "suffix") ) ) , verbose = FALSE), stringsAsFactors = FALSE )
                       table.cast     <- reshape2::dcast(molt, ... ~ var)
                   } else {
                       table.cast <- do.call("rbind", by(data = dat.ana, INDICES = dat.ana[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                                     prefix <- data.frame(sub.dat[1,as.character(imp[-length(imp)]), drop=FALSE], row.names = NULL, stringsAsFactors = FALSE )
                                     foo    <- make.indikator(variable = sub.dat[,"what.I.want"], name.var = "ind")
                                     if(!is.null(wgt))  { ret    <- data.frame ( prefix , suffix = names(table( sub.dat[,"what.I.want"] )), desk(foo[,-1, drop = FALSE], p.weights = sub.dat[,wgt])[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )}
                                     if(is.null(wgt))   { ret    <- data.frame ( prefix , suffix = names(table( sub.dat[,"what.I.want"] )), desk(foo[,-1, drop = FALSE])[,c("Mittelwert", "std.err")], stringsAsFactors = FALSE )}
                                     colnames(ret) <- c(as.character(imp[-length(imp)]), "suffix", "mittelmean", "se")
                                     return(ret)}) )
                   }
                   return(table.cast)}


conv.mean      <- function (imp, dat.i , dep, ID, wgt, na.rm, group.differences.by, workbook  )   {
                  deskr    <- do.call("rbind", by(data = dat.i[,c(as.character(imp[-length(imp)]), dep, wgt)], INDICES = dat.i[,as.character(imp[-length(imp)])], FUN = function ( sub.dat) {
                              prefix <- sub.dat[1,as.character(imp[-length(imp)]), drop=FALSE]
                              if(is.null(wgt))  { ret    <- desk(sub.dat[, imp[["dep"]] ])[,c("Mittelwert", "std.err", "Varianz", "Streuung")]}
                              if(!is.null(wgt)) { ret    <- desk(sub.dat[, imp[["dep"]] ], p.weights = sub.dat[,wgt])[,c("Mittelwert", "std.err", "Varianz", "Streuung")]}
                              if( is.null(wgt) )  {N_weighted <- nrow(sub.dat)} else {N_weighted <- sum(sub.dat[,wgt])}
                              ret  <- data.frame(prefix, N_weighted = N_weighted, se.N_weighted = NA, mittelwert = ret[["Mittelwert"]], se.mean = ret[["std.err"]], variance = ret[["Varianz"]], se.variance = NA, N_unweighted = nrow(sub.dat), se.N_unweighted = NA  ,  SD = ret[["Streuung"]], se.SD = NA, stringsAsFactors = FALSE )
                              names(ret) <- c(names(imp[-length(imp)]) , "N_weighted", "se.N_weighted", imp[["dep"]], "se.mean", "variance", "se.variance", "N_unweighted", "se.N_unweighted", "SD", "se.SD")
                              return(ret)}))
                  difs <- NULL
                  if(!is.null(group.differences.by))   {
                      m            <- deskr
                      m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[names(imp[-length(imp)])], collapse = "."))})
                      m$all.group  <- 1
                      res.group    <- tempR <- setdiff(names(workbook$group.origin), group.differences.by)
                      if(length(res.group) == 0 ) {res.group <- "all.group"} else {res.group <- names(imp[-length(imp)][res.group])}
                      difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                        true.diff <- diff(iii[,which(colnames(iii) %in% dep)])
                                        scumm     <- sapply(iii[,res.group,drop = FALSE], as.character)
                                        group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                        dif.iii   <- data.frame(group = group, dif = true.diff, se = sqrt( sum(iii[,"SD"]^2 / iii[,"N_unweighted"]) ) , stringsAsFactors = FALSE )
                                        return(dif.iii)                         ### siehe http://www.vassarstats.net/dist2.html
                      }))                                                       ### http://onlinestatbook.com/2/tests_of_means/difference_means.html
                  }
                  attr(deskr, "difs") <- difs
                  return(deskr)}


jackknife.mean <- function (imp, dat.i , dep, replicates , ID, wgt, na.rm, group.differences.by, workbook  )   {
                  cat("."); flush.console()
                  dat.i[,"N_weighted"] <- 1
                  design         <- svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep,"N_weighted") ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                  formel         <- as.formula(paste("~ ",imp[["dep"]], sep = "") )
                  formelSum      <- as.formula("~ N_weighted")
                  means          <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svymean, na.rm=na.rm, deff = FALSE, return.replicates = TRUE)
                  totals         <- svyby(formula = formelSum, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svytotal, na.rm=na.rm, deff = FALSE, return.replicates = TRUE)
                  colnames(totals) <- gsub("^se$", "se.N_weighted", colnames(totals))
                  colnames(means) <- gsub("^se$", "se.mean", colnames(means) )
                  vars           <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svyvar, na.rm=na.rm, deff = FALSE, return.replicates = TRUE)
                  colnames(vars) <- gsub("^V1$", "variance", colnames(vars))    ### Standardabweichungen muessen separat bestimmt werden (Lumley, Mail 17. Oktober 2012). Delta method
                  colnames(vars) <- gsub("^se$", "se.variance", colnames(vars)) ### da die delta method "von hand" programmiert werden muss, kann sie nicht mittels svyby() uebergeben werden, muss also aehnlich wie svyglm() von hand mittels lapply() auf die verschiedenen abhaengigen variablen verteilt werden
                  sds            <- do.call("rbind", by(data = dat.i, INDICES =  dat.i[,as.character(imp[-length(imp)])], FUN = function (uu) {
                                    namen          <- sapply(uu[,as.character(imp[-length(imp)]), drop = FALSE], FUN = function ( yy ) {
                                                      retu <- table(yy)
                                                      return(names(retu)[retu>0]) })
                                    sub.replicates <- replicates[ match(uu[,"idstud"], replicates[,"ID"] ) ,  ]
                                    design.uu      <- svrepdesign(data = uu[ ,c(as.character(imp[-length(imp)]), dep)], weights = uu[,wgt], type="JKn", scale = 1, rscales = 1, repweights = sub.replicates[,-1, drop = FALSE], combined.weights = TRUE, mse = TRUE)
                                    var.uu         <- svyvar(x = as.formula(paste("~",imp[["dep"]],sep="")), design = design.uu, deff = FALSE, return.replicates = TRUE, na.rm = na.rm)
                                    ret            <- data.frame(t(namen), N_unweighted = nrow(uu), se.N_unweighted = NA, SD = as.numeric(sqrt(coef(var.uu))), se.SD =  as.numeric(sqrt(vcov(var.uu)/(4*coef(var.uu)))), stringsAsFactors = FALSE )
                                    return(ret)}) )
                  difs           <- NULL
                  if(!is.null(group.differences.by))   {
                      m            <- means
                      m$comb.group <- apply(m, 1, FUN = function (ii) {crop(paste( ii[as.character(imp[-length(imp)])], collapse = "."))})
                      repl         <- data.frame(t(attr(means, "replicates")), stringsAsFactors = FALSE )
                      repl[,"comb.group"] <- rownames(repl)
                      m              <- merge(m, repl, by = "comb.group" )
                      m$all.group    <- 1
                      res.group      <- tempR  <- setdiff(names(workbook$group.origin), group.differences.by)
                      if(length(res.group) == 0 ) {res.group <- "all.group"} else {res.group <- as.character(imp[-length(imp)][res.group])}
                      difs           <- do.call("rbind", by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                        true.diff <- diff(iii[,which(colnames(iii) %in% dep)])
                                        cols      <- grep("^X[[:digit:]]{1,3}$", colnames(iii) )
                                        other.diffs <- apply(iii[,cols], 2, diff)
                                        scumm     <- sapply(iii[,res.group,drop = FALSE], as.character)
                                        if ( length(tempR)>0) {colnames(scumm) <- names(imp[-length(imp)])[match( colnames(scumm), imp[-length(imp)])]}
                                        group     <- paste( paste( colnames(scumm), scumm[1,], sep="="), sep="", collapse = ", ")
                                        dif.iii   <- data.frame(group = group, dif = true.diff, se =  sqrt(sum((true.diff - other.diffs)^2)), stringsAsFactors = FALSE )
                                        return(dif.iii)
                      }))
                  }
                  common            <- as.character(imp[-length(imp)])
                  ret               <- merge(totals, means, by = common, all = TRUE)
                  ret               <- merge(ret, vars, by = common, all = TRUE)
                  ret               <- merge(ret, sds , by = common, all = TRUE )
                  impG              <- imp[!imp %in% imp[["dep"]] ]
                  if(length(impG)>0) {                                          ### Jetzt werden noch die Namen der Gruppenvariablen vereinheitlicht
                     for ( u in 1:length(impG))    {                            ### notwendig, wenn durch Imputationen etc. dieselbe Gruppenvariable verschiedene Variablennamen hat
                           colnames(ret) <- gsub(impG[u], names(impG)[u], colnames(ret))
                     }}
                     attr(ret, "difs") <- difs
                     return(ret) }


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
                       reps <- sapply(zonen , FUN = function(ii) {
                               rep.ii <- dat.i[,"wgt"]
                               rep.ii[dat.i[,"JKZone"] == ii ] <- ifelse(dat.i[ dat.i[,"JKZone"] == ii ,"JKrep"] == 1, 0, 2 * rep.ii[dat.i[,"JKZone"] == ii ] )
                               return(rep.ii) })
                       colnames(reps) <- paste(wgt, colnames(reps), sep="_")
                       ret            <- data.frame(ID = dat.i$ID, reps, stringsAsFactors = FALSE)
                       attr(ret, "n.replicates") <- length(zonen)
                       return(ret) }


