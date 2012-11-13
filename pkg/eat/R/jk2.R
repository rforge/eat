jk2.mean <- function(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), group.differences.by = NULL, dependent = list(), complete.permutation = c("nothing", "groups", "all") )    {
            complete.permutation <- match.arg ( complete.permutation )
            if(!is.null(group.differences.by)) {
               if(!group.differences.by %in% names(group)) {stop()} }
            if(is.null(wgt))   {
               cat("No weights specified. Use weight of 1 for each case.\n",sep = "")
               dat$weight_one <- 1
               wgt <- "weight_one"
            }
            replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )
            if(length(group) == 0) {
               cat("No group(s) specified. Analyses will be computed only for the whole sample.\n")
               dat$whole_group <- "whole_group"
               group           <- list(whole_group = "whole_group")
            }
            allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(group), dependent = unlist(dependent) )
            all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
            dat.i       <- dat[,unlist(all.Names), drop = FALSE]
            missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
            if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
            cat(paste("Found ",length(group)," grouping variable(s).\n",sep=""))
            .checkGroupConsistency(dat = dat, group = group)
            cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""))
            if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(group, stringsAsFactors = FALSE))}
            analysis    <- lapply(dependent, FUN = function ( dep ) {
                           group.origin <- group
                           if(complete.permutation == "all" ) {
                              group$dep <- dep
                              workbook  <- expand.grid(group, stringsAsFactors = FALSE)
                           }  else  {
                              max.elements <- max ( c( unlist(lapply(group, length)) , length(dep) ) )
                              group$dep    <- dep
                              group        <- lapply(group, FUN = function (uu) {rep(uu, times = max.elements)[1:max.elements]})
                              workbook     <- data.frame(group, stringsAsFactors = FALSE)
                           }
                           cat(paste("Use ",nrow(workbook)," replication(s) overall.\n",sep=""))
                           ana <- apply(workbook, MARGIN = 1, FUN = function (imp) {
                                  cat("."); flush.console()
                                  design         <- svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep) ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1], combined.weights = TRUE, mse = TRUE)
                                  formel         <- as.formula(paste("~ ",imp[["dep"]], sep = "") )
                                  means          <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svymean, deff = FALSE, return.replicates = TRUE)
                                  colnames(means) <- gsub("^se$", "se.mean", colnames(means) )
                                  vars           <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svyvar, deff = FALSE, return.replicates = TRUE)
                                  colnames(vars) <- gsub("^V1$", "variance", colnames(vars))
                                  colnames(vars) <- gsub("^se$", "se.variance", colnames(vars))
                                  ### Standardabweichungen muessen separat bestimmt werden (Lumley, Mail 17. Oktober 2012). Delta method
                                  ### da die delta method "von hand" programmiert werden muss, kann sie nicht mittels svyby() uebergeben werden, muss also aehnlich wie svyglm() von hand mittels lapply() auf die verschiedenen abhaengigen variablen verteilt werden
                                  sds            <- do.call("rbind", by(data = dat.i, INDICES =  dat.i[,as.character(imp[-length(imp)])], FUN = function (uu) {
                                                    namen          <- sapply(uu[,as.character(imp[-length(imp)]), drop = FALSE], FUN = function ( yy ) {
                                                                      retu <- table(yy)
                                                                      return(names(retu)[retu>0]) })
                                                    sub.replicates <- replicates[ match(uu[,"idstud"], replicates[,"ID"] ) ,  ]
                                                    design.uu      <- svrepdesign(data = uu[ ,c(as.character(imp[-length(imp)]), dep)], weights = uu$wgtSTUD, type="JKn", scale = 1, rscales = 1, repweights = sub.replicates[,-1], combined.weights = TRUE, mse = TRUE)
                                                    var.uu         <- svyvar(x = as.formula(paste("~",imp[["dep"]],sep="")), design = design.uu, deff = FALSE, return.replicates = TRUE)
                                                    ret            <- data.frame(t(namen), SD = as.numeric(sqrt(coef(var.uu))), se.SD =  as.numeric(sqrt(vcov(var.uu)/(4*coef(var.uu)))), stringsAsFactors = FALSE ) }) )
                                  difs           <- NULL
                                  if(!is.null(group.differences.by))   {
                                     m            <- means
                                     m$comb.group <- apply(m, 1, FUN = function (ii) { crop(paste( ii[as.character(imp[-length(imp)])], collapse = "."))})
                                     repl         <- data.frame(t(attr(means, "replicates")), stringsAsFactors = FALSE )
                                     repl[,"comb.group"] <- rownames(repl)
                                     m              <- merge(m, repl, by = "comb.group" )
                                     m$all.group    <- 1
                                     res.group      <- setdiff(names(group.origin), group.differences.by)
                                     if(length(res.group) == 0 ) {res.group <- "all.group"} else {res.group <- as.character(imp[-length(imp)][res.group])}
                                     difs           <- by(data = m, INDICES = m[,res.group], FUN = function (iii)   {
                                                       true.diff <- diff(iii[,which(colnames(iii) %in% dep)])
                                                       cols      <- grep("^X[[:digit:]]{1,3}$", colnames(iii) )
                                                       other.diffs <- apply(iii[,cols], 2, diff)
                                                       dif.iii   <- data.frame(dif = true.diff, se =  sqrt(sum((true.diff - other.diffs)^2)), stringsAsFactors = FALSE )
                                                       return(dif.iii)
                                     })
                                     difs           <- do.call("rbind", difs)
                                  }
                                  common            <- as.character(imp[-length(imp)])
                                  ret               <- merge(means, vars, by = common, all = TRUE)
                                  ret               <- merge(ret, sds , by = common, all = TRUE )
                                  attr(ret, "difs") <- difs
                                  return(ret)
                           })
                           cat("\nPooling Standard errors.\n")
                           ana.frame           <- do.call("cbind", ana)
                           mean.cols           <- unlist(lapply(dep, FUN = function ( uu ) {grep(paste(uu,"$",sep=""), colnames(ana.frame))}))
                           se.mean.cols        <- grep("^se.mean",colnames(ana.frame))
                           sd.cols             <- grep("^SD",colnames(ana.frame))
                           se.sd.cols          <- grep("^se.SD", colnames(ana.frame))
                           var.cols            <- grep("^variance", colnames(ana.frame))
                           se.var.cols         <- grep("^se.variance", colnames(ana.frame))
                           stopifnot(length(mean.cols ) == length(se.mean.cols) )
                           stopifnot(length(se.mean.cols ) == length(sd.cols) )
                           stopifnot(length(sd.cols ) == length(se.sd.cols) )
                           stopifnot(length(var.cols ) == length(se.var.cols) )
                           if(length(se.mean.cols)>1) {                         ### Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                              pooled              <- t(apply(ana.frame, MARGIN = 1, FUN = function (iii) {
                                                     unlist(c(pool.means(m = as.numeric(iii[mean.cols]), se = as.numeric(iii[se.mean.cols]))$summary[c("m.pooled","se.pooled")],
                                                     pool.means(m = as.numeric(iii[sd.cols]), se = as.numeric(iii[se.sd.cols]))$summary[c("m.pooled","se.pooled")],
                                                     pool.means(m = as.numeric(iii[var.cols]), se = as.numeric(iii[se.var.cols]))$summary[c("m.pooled","se.pooled")]))
                              }))
                              colnames(pooled)    <- paste(rep(c("", "se."), 3)  , rep(c("mean", "SD", "variance"), each = 2), sep = "")
                              pooled              <- data.frame(ana.frame[,1:length(group.origin),drop = FALSE], pooled, stringsAsFactors = FALSE )
                           }
                           if(length(se.mean.cols)==1) {
                              pooled              <- data.frame(ana.frame[,1:length(group.origin),drop = FALSE], mean = ana.frame[,mean.cols], se.mean = ana.frame[,se.mean.cols], SD = ana.frame[,sd.cols], se.SD = ana.frame[,se.sd.cols], variance = ana.frame[,var.cols], se.variance = ana.frame[,se.var.cols], stringsAsFactors = FALSE )
                           }
                           colnames(pooled)[1:length(group.origin)] <- names(group.origin)
                           pooled.dif <- NULL
                           if(!is.null(attr(ana[[1]], "difs")))   {
                               if(length(ana) == 1) {pooled.dif <- attr(ana[[1]], "difs")}
                               if(length(ana) > 1)  {
                                  dif.frame    <- do.call("cbind", lapply(ana, FUN = function (iii) {attr(iii, "difs")}) )
                                  mean.cols    <- grep("^dif",colnames(dif.frame))
                                  se.cols      <- grep("^se",colnames(dif.frame))
                                  pooled.dif   <- t(apply(dif.frame, MARGIN = 1, FUN = function (iii) {
                                         unlist(c(pool.means(m = as.numeric(iii[mean.cols]), se = as.numeric(iii[se.cols]))$summary[c("m.pooled","se.pooled")]))
                                }))
                               }
                           }
                           attr(pooled, "difference") <- pooled.dif
                           attr(pooled, "unpooled")   <- ana.frame
                           return(pooled)
            })
            return(analysis)}


### separate.missing.indikator ... Soll eine separate Kategorie für missings definiert werden?
### expected.values            ... optional (und empfohlen): Vorgabe für erwartete Werte, vgl. "table.muster"
###                                kann entweder eine benannte Liste sein, mit Namen wie in "dependent", oder ein einfacher character Vektor, dann werden diese Vorgaben für alle abhängigen Variablen übernommen
###                                bleibt "expected.values" leer, dann wird es automatisch mit den Werten der Variablen in ihrer Gesamtheit belegt!
### separate.missing.indikator ... list of logical elemente or logical scalar
jk2.table <- function(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), dependent = list(), separate.missing.indikator = FALSE, expected.values = list(), complete.permutation = c("nothing", "groups", "all") )    {
            complete.permutation <- match.arg ( complete.permutation )
            if(is.null(wgt))   {
               cat("No weights specified. Use weight of 1 for each case.\n",sep = "")
               dat$weight_one <- 1
               wgt <- "weight_one"
            }
            replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )
            if(length(group) == 0) {
               cat("No group(s) specified. Analyses will be computed only for the whole sample.\n")
               dat$whole_group <- "whole_group"
               group           <- list(whole_group = "whole_group")
            }
            allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(group), dependent = unlist(dependent) )
            all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
            dat.i       <- dat[,unlist(all.Names)]
            missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
            if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
            cat(paste("Found ",length(group)," grouping variable(s).\n",sep=""))
            .checkGroupConsistency(dat = dat, group = group)
            cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""))
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
            if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(group, stringsAsFactors = FALSE))}
            analysis    <- lapply(dependent, FUN = function ( dep ) {
                           group.origin <- group
                           if(complete.permutation == "all" ) {
                              group$dep <- dep
                              workbook  <- expand.grid(group, stringsAsFactors = FALSE)
                           }  else  {
                              max.elements <- max ( c( unlist(lapply(group, length)) , length(dep) ) )
                              group$dep    <- dep
                              group        <- lapply(group, FUN = function (uu) {rep(uu, times = max.elements)[1:max.elements]})
                              workbook     <- data.frame(group, stringsAsFactors = FALSE)
                           }
                           cat(paste("Use ",nrow(workbook)," replication(s) overall.\n",sep=""))
                           ana <- apply(workbook, MARGIN = 1, FUN = function (imp) {
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
                                  }
                                  if(attr(dep, "separate.missing") == FALSE ) {
                                     weg <- which(is.na(dat.ana[,imp[["dep"]]]))
                                     if(length(weg)>0) {
                                        cat(paste(" Warning: No seperate missing categorie was chosen. ", length(weg), " missings were found anyhow for variable ",imp[["dep"]],". Missings will be deteted from the data.\n",sep=""))
                                        dat.ana   <- dat.ana[-weg,]
                                     }
                                  }
                                  dat.ana$what.I.want <- factor(dat.ana[,imp[["dep"]]], levels = original.levels)
                                  design    <- svrepdesign(data = dat.ana[,c(as.character(imp[-length(imp)]), "what.I.want")], weights = dat.ana[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[match(dat.ana[,"idstud"], replicates[,"ID"], ),-1], combined.weights = TRUE, mse = TRUE)
                                  means     <- svyby(formula = ~factor(what.I.want, levels = original.levels), by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svymean, deff = FALSE, return.replicates = TRUE)
                                  cols      <- match(paste("factor(what.I.want, levels = original.levels)",original.levels,sep=""), colnames(means))
                                  colnames(means)[cols] <- paste("mittelmean",original.levels, sep="____________")
                                  cols.se   <- grep("^se", colnames(means) )
                                  stopifnot(length(cols) == length(cols.se))
                                  colnames(means)[cols.se] <- paste("se____________", original.levels, sep="")
                                  molt           <- melt.data.frame(data=means, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                                  xx             <- data.frame(matrix(unlist(strsplit(as.character(molt$variable), "____________")), ncol= 2, byrow = TRUE ), stringsAsFactors = FALSE )
                                  colnames(xx)   <- c("var", "suffix")
                                  molt           <- data.frame(molt[,-match("variable", colnames(molt))], xx, stringsAsFactors = FALSE )
                                  table.cast     <- cast(molt, ... ~ var)
                                  return(table.cast)
                           })
                           cat("\nPooling Standard errors.\n")
                           ana.frame           <- do.call("cbind", ana)
                           kategorie.cols      <- grep("suffix$", colnames(ana.frame))[1]
                           mean.cols           <- grep("mittelmean$", colnames(ana.frame))
                           se.cols             <- grep("se$",colnames(ana.frame))
                           stopifnot(length(mean.cols) == length(se.cols))
                           group.cols          <- unlist(lapply(group.origin, FUN = function(u) {grep(u[1], colnames(ana.frame))[1]}))
                           if(length(se.cols)>1) {                              ## Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                              pooled              <- t(apply(ana.frame, MARGIN = 1, FUN = function (iii) {
                                                     unlist(c(pool.means(m = as.numeric(iii[mean.cols]), se = as.numeric(iii[se.cols]))$summary[c("m.pooled","se.pooled")]))}))
                              pooled              <- data.frame(ana.frame[,c(group.cols,kategorie.cols)], pooled, stringsAsFactors = FALSE )
                           }
                           if(length(se.cols)==1) {
                              pooled              <- data.frame(ana.frame[,c(group.cols, kategorie.cols)], m = ana.frame[,mean.cols], se = ana.frame[,se.cols], stringsAsFactors = FALSE )
                           }
                           colnames(pooled)[1:length(group.origin)] <- names(group.origin)
                           attr(pooled, "unpooled") <- ana.frame
                           return(pooled)
            })
            return(analysis)}


jk2.quantile <- function(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), dependent = list(), probs = seq(0, 1, 0.25),  complete.permutation = c("nothing", "groups", "all") )    {
            complete.permutation <- match.arg ( complete.permutation )
            if(is.null(wgt))   {
               cat("No weights specified. Use weight of 1 for each case.\n",sep = "")
               dat$weight_one <- 1
               wgt <- "weight_one"
            }
            replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )
            if(length(group) == 0) {
               cat("No group(s) specified. Analyses will be computed only for the whole sample.\n")
               dat$whole_group <- "whole_group"
               group           <- list(whole_group = "whole_group")
            }
            allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(group), dependent = unlist(dependent) )
            all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
            dat.i       <- dat[,unlist(all.Names)]
            missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
            if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
            cat(paste("Found ",length(group)," grouping variable(s).\n",sep=""))
            .checkGroupConsistency(dat = dat, group = group)
            groupsize   <- sapply(group, FUN = function (iii ) {length(iii)})
            cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""))
            if ( complete.permutation == "groups" ) {group <- as.list(expand.grid(group, stringsAsFactors = FALSE))}
            analysis    <- lapply(dependent, FUN = function ( dep ) {
                           group.origin <- group
                           if(complete.permutation == "all" ) {
                              group$dep <- dep
                              workbook  <- expand.grid(group, stringsAsFactors = FALSE)
                           }  else  {
                              max.elements <- max ( c( unlist(lapply(group, length)) , length(dep) ) )
                              group$dep    <- dep
                              group        <- lapply(group, FUN = function (uu) {rep(uu, times = max.elements)[1:max.elements]})
                              workbook    <- data.frame(group, stringsAsFactors = FALSE)
                           }
                           cat(paste("Use ",nrow(workbook)," replications overall.\n",sep=""))
                           ana <- apply(workbook, MARGIN = 1, FUN = function (imp) {
                                  cat("."); flush.console()
                                  design         <- svrepdesign(data = dat.i[,c(as.character(imp[-length(imp)]), dep) ], weights = dat.i[,wgt], type="JKn", scale = 1, rscales = 1, repweights = replicates[,-1], combined.weights = TRUE, mse = TRUE)
                                  formel         <- as.formula(paste("~ ",imp[["dep"]], sep = "") )
                                  quantile.imp   <- svyby(formula = formel, by = as.formula(paste("~", paste(as.character(imp[-length(imp)]), collapse = " + "))), design = design, FUN = svyquantile, quantiles = probs, return.replicates = TRUE, na.rm = TRUE)
                                  molt           <- melt.data.frame(data=quantile.imp, id.vars=as.character(imp[-length(imp)]), na.rm=TRUE)
                                  xx             <- colsplit(gsub("([[:digit:]]+)", "\\.\\1", molt$variable), "\\.", names = c("var", "per.number"))
                                  molt           <- data.frame(molt[,-match("variable", colnames(molt))], xx, stringsAsFactors = FALSE )
                                  quantile.cast  <- cast(molt, ... ~ var)       ### Jetzt sollen noch die verwendeten Perzentilgroessen angebunden werden
                                  probnamen      <- data.frame(Nummer = 1:length(probs), per.number = probs, stringsAsFactors = FALSE )
                                  quantile.cast[,"per.number"] <- probnamen[ match(quantile.cast[,"per.number"], probnamen[,1])  ,2]
                                  return(quantile.cast)
                           })
                           cat("\nPooling Standard errors.\n")
                           ana.frame           <- do.call("cbind", ana)
                           mean.cols           <- grep("V$",colnames(ana.frame))
                           se.cols             <- grep("se$",colnames(ana.frame))
                           group.cols          <- unlist(lapply(c(as.character(workbook[1,names(group.origin)]), "per.number"), FUN = function (hh)  {grep(hh, colnames(ana.frame))[1]}))
                           if(length(se.cols)>1) {                              ## Es wird nur gepoolt, wenn es mehr als einen Standardfehler gibt
                              pooled              <- t(apply(ana.frame, MARGIN = 1, FUN = function (iii) {
                                                     unlist(c(pool.means(m = as.numeric(iii[mean.cols]), se = as.numeric(iii[se.cols]))$summary[c("m.pooled","se.pooled")]))  }))
                           }
                           if(length(se.cols)==1) {
                              pooled              <- data.frame(m = ana.frame[,mean.cols, drop = FALSE], se = ana.frame[,se.cols, drop = FALSE], stringsAsFactors = FALSE )
                           }
                           pooled              <- data.frame(ana.frame[,group.cols, drop = FALSE], pooled, stringsAsFactors = FALSE )
                           attr(pooled, "unpooled") <- ana.frame
                           return(pooled)
            })
            return(analysis)}


jk2.glm <- function(dat, ID, wgt = NULL, JKZone, JKrep, group = list(), independent = list(), dependent = list(), complete.permutation = c("nothing", "groups", "independent", "all") , glm.family)    {
            complete.permutation <- match.arg ( complete.permutation )
            .GlobalEnv$glm.family <- glm.family                                 ### Hotfix!
            if(is.null(wgt))   {
               cat("No weights specified. Use weight of 1 for each case.\n",sep = "")
               dat$weight_one <- 1
               wgt <- "weight_one"
            }
            replicates  <- generate.replicates(dat = dat, ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep )
            if(length(group) == 0) {
               cat("No group(s) specified. Analyses will be computed only for the whole sample.\n")
               dat$whole_group <- "whole_group"
               group           <- list(whole_group = "whole_group")
            }
            allVars     <- list(ID = ID, wgt = wgt, JKZone = JKZone, JKrep = JKrep, group = unlist(group), independent = unlist(independent), dependent = unlist(dependent) )
            all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
            dat.i       <- dat[,unlist(all.Names)]
            missings    <- sapply(dat.i, FUN = function (uu) {length(which(is.na(uu)))})
            if(!all(missings == 0)) {stop(paste("Found NAs in variable(s) ",paste(names(missings[missings!=0]), collapse = ", "), "\n",sep = "") )}
            cat(paste("Found ",length(group)," grouping variable(s).\n",sep=""))
            .checkGroupConsistency(dat = dat, group = group)
            cat(paste("Run ",length(dependent)," analyses overall.\n", sep = ""))
            if ( complete.permutation == "groups" )      {group       <- as.list(expand.grid(group, stringsAsFactors = FALSE))}
            if ( complete.permutation == "independent" ) {independent <- as.list(expand.grid(independent, stringsAsFactors = FALSE))}
            pre.workbook   <- c(group, independent)
            analysis    <- lapply(dependent, FUN = function ( dep ) {
                           if(complete.permutation == "all" ) {
                              pre.workbook$dep <- dep
                              workbook  <- expand.grid(pre.workbook, stringsAsFactors = FALSE)
                           }  else  {
                              max.elements        <- max ( c( unlist(lapply(group, length)) , length(dep) ) )
                              pre.workbook$dep    <- dep
                              pre.workbook        <- lapply(pre.workbook, FUN = function (uu) {rep(uu, times = max.elements)[1:max.elements]})
                              workbook            <- data.frame(pre.workbook, stringsAsFactors = FALSE)
                           }
                           cat(paste("Use ",nrow(workbook)," replications overall.\n",sep=""))
                           ana <- apply(workbook, MARGIN = 1, FUN = function (imp) {
                                  group.names       <- as.character(imp[names(group)])
                                  independent.names <- setdiff(as.character(imp), group.names)
                                  independent.names <- setdiff(independent.names, as.character(imp[["dep"]]) )
                                  cat("."); flush.console()
                                  stopifnot(length(imp[["dep"]]) == 1 )
                                  sub.ana <- by(data = dat.i, INDICES = dat.i[,group.names], FUN = function (sub.dat) {
                                             sub.replicates <- replicates[replicates[,"ID"] %in% sub.dat[,ID] ,-1]
                                             design         <- svrepdesign(data = sub.dat[,as.character(imp)], weights = sub.dat[,wgt], type="JKn", scale = 1, rscales = 1, repweights = sub.replicates, combined.weights = TRUE, mse = TRUE)
                                             formel         <- as.formula(paste(imp[["dep"]],"~", paste( independent.names, collapse = " + "), sep = ""))
                                             glm.ii         <- svyglm(formula = formel, design = design, return.replicates = FALSE, family = glm.family)
                                             r.squared      <- data.frame ( r.squared = var(glm.ii$fitted.values)/var(glm.ii$y) , N = nrow(sub.dat) , N.valid = length(glm.ii$fitted.values) )
                                             r.nagelkerke   <- NagelkerkeR2(glm.ii)
                                             group.values   <- data.frame(matrix(sapply(sub.dat[,as.character(imp[names(group)]), drop = FALSE], FUN = function(uu) {names(table(uu))}), nrow = 1), stringsAsFactors = FALSE )
                                             colnames(group.values) <- names(group)
                                             res.bl         <- data.frame(group.values, reg = rownames(summary(glm.ii)$coefficients[,c(1:2)]), summary(glm.ii)$coefficients[,c(1:2)], r.squared, r.nagelkerke = r.nagelkerke$R2, stringsAsFactors = FALSE )
                                             # stopifnot(nrow(res.bl) == length(independent)+1)
                                             # res.bl[-1,"reg"]  <- names(independent)
                                             return(res.bl)
                                  })
                                  sub.ana <- do.call("rbind", sub.ana)
                                  return(sub.ana)
                           })
                           cat("\nPooling Standard errors.\n")
                           ana.frame           <- do.call("cbind", ana)
                           mean.cols           <- grep("Estimate",colnames(ana.frame))
                           se.cols             <- grep("Std..Error",colnames(ana.frame))
                           r.squar.cols        <- grep("r.squared",colnames(ana.frame))
                           r.nagel.cols        <- grep("r.nagelkerke", colnames(ana.frame))
                           n.valid.cols        <- grep("N.valid",colnames(ana.frame))
                           group.cols          <- sapply(names(group), FUN = function (uu) {grep(uu, colnames(ana.frame))[1]})
                           coef.col            <- grep("reg$", colnames(ana.frame))[1]
                           stopifnot(length(mean.cols ) == length(se.cols) )
                           stopifnot(length(se.cols ) == length(r.squar.cols) )
                           stopifnot(length(r.squar.cols ) == length(r.nagel.cols) )
                           stopifnot(length(r.squar.cols ) == length(n.valid.cols) )
                           if(length(se.cols) > 1)  {
                               pooled         <- do.call("rbind", apply(X = ana.frame, MARGIN = 1, FUN = function (ii ) {
                                                 frame.1 <- data.frame(pool.means(m = as.numeric(ii[mean.cols]), se = as.numeric(ii[se.cols]))$summary[c("m.pooled","se.pooled")], stringsAsFactors = FALSE)
                                                 colnames(frame.1) <- c("estimate", "se.pooled")
                                                 frame.2 <- data.frame(pool.R2 ( r2 = as.numeric(ii[r.squar.cols]), N = as.numeric(ii[n.valid.cols]), quiet = TRUE ), stringsAsFactors = FALSE)
                                                 colnames(frame.2) <- c("pooled.R2", "se.R2")
                                                 frame.3 <- data.frame(pool.R2 ( r2 = as.numeric(ii[r.nagel.cols]), N = as.numeric(ii[n.valid.cols]), quiet = TRUE ), stringsAsFactors = FALSE)
                                                 colnames(frame.3) <- c("pooled.R2.nagelkerke", "se.R2.nagelkerke")
                                                 return( data.frame(frame.1, frame.2, frame.3, stringsAsFactors = FALSE)) }))
                              pooled <- data.frame(ana.frame[,c(group.cols, coef.col, drop = FALSE)], pooled, stringsAsFactors = FALSE )
                           }
                           if(length(se.cols) == 1)  {
                              pooled <- data.frame(ana.frame[,c(group.cols, coef.col, mean.cols, se.cols, r.squar.cols, r.nagel.cols)], stringsAsFactors = FALSE )
                           }
                           attr(pooled, "unpooled") <- ana.frame
                           return(pooled)
            })
            return(analysis) }


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


.checkGroupConsistency <- function (dat, group)   {
       consistent <- lapply(group, FUN = function (ii) {
                     if(length(ii) > 1) {
                        first <- names(table(as.character(dat[,ii[1]])))
                        cons  <- lapply(dat[,ii], FUN = function (iii) {
                                 actual <- names(table(as.character(iii)))
                                 stopifnot(all ( actual == first ) )
                        })
                        }})}