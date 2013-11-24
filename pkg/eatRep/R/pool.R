pool.means <- function (m, se, na.rm = FALSE) {
     if(!is.list(m))  { listM  <- list(m)}  else {listM  <- m }
     createSeList <- FALSE
     if ( length(unlist(se)) == 0 ) {createSeList <- TRUE} else {  if(all(is.na(unlist(se)))) { createSeList <- TRUE}}
     if( createSeList == FALSE) { if(!is.list(se)) { listSE <- list(se)} else {listSE <- se}  }
     if( createSeList == TRUE ) { listSE <- lapply(listM, FUN = function ( toNA ) { toNA <- rep(NA,length(toNA))} ) }
     stopifnot(all(unlist(lapply(listM, length)) == unlist(lapply(listSE, length)) ) )
     if( length(which(unlist(lapply(listM, length)) == 0 )) > 0 ) {listM <- listM[-which(unlist(lapply(listM, length)) == 0 )]}
     if( length(which(unlist(lapply(listSE, length)) == 0 )) > 0 ) {listSE <- listSE[-which(unlist(lapply(listSE, length)) == 0 )]}
     listM  <- data.frame(do.call("cbind", listM),  stringsAsFactors = FALSE)   ### Obere Zeile: Listeneintraege mit 0 Elementen werden geloescht
     listSE <- data.frame(do.call("cbind", listSE), stringsAsFactors = FALSE)
     M        <- length(listM[[1]])
     N        <- length(listM)                                                  ### wenn nicht genestet, muss N == 1!
     Q.all    <- mean(unlist(lapply(listM, mean)))                              ### Rubin 2003b, S. 6, unterste Formel
     Q.m      <- apply(listM, 1, mean)                                          ### Rubin 2003b, S. 7, 1. Formel. hier muss immer ein Vektor stehen, egal ob nested oder nicht!
     U        <- mean(unlist(lapply(listSE, FUN = function ( se ) {mean(se^2)})))## Rubin 2003b, S. 7, 2. Formel
     MS.b     <- N/(M-1) * sum((unlist(Q.m) - Q.all)^2)
     MS.omega <- 1/(M*(N-1)) *  sum((listM - Q.m)^2)                            ### Rubin 2003b, S. 7, 4. Formel
     if(all(is.na(MS.omega))) {MS.omega <- 0}                                   ### wenn keine nestung vorliegt, wird within-nest SQ zu Null
     var.total <- U+1/N*(1+1/M)*MS.b + (1-1/N)*MS.omega                         ### Rubin 2003b, "The quantity T", S. 7, 5. Formel
     se.pooled <- sqrt(var.total)
     betweenN  <- ifelse(N>1, ( ( 1-1/N)*MS.omega / var.total )^2 * 1/(M*(N-1)), 0 )
     df        <- 1 / ( (1/N*(1+1/M)*MS.b / var.total)^2 * 1/(M-1) + betweenN )
     pooled <- list(m = listM, var = lapply(listSE, FUN = function (x) {x^2}), summary = data.frame ( m.pooled = Q.all, se.pooled = se.pooled, df = df, stringsAsFactors = FALSE ) )
     return(pooled) }


### r2         ... Vektor von R^2-Werten aus multiple imputieren Analysen
### N          ... Optional. Vektor aus Ns der jeweiligen Imputationen, muss genausop lang wie r2 sein. Ohne den Vektor gibt's keinen Standardfehler, nur ein gepooltes R^2
###                nicht genestet: pool.R2 ( r2 = c(0.6597448, 0.9392199, 0.9422953, 0.6853930, 0.6263835, 0.6114845, 0.9400144, 0.7177461, 0.7882264, 0.8173687), N = c(1730, 1623, 1593, 1223,  295, 1611, 1021,  975, 1672,  607) )  ## 0.814648 0.1848399
###                genestet:       pool.R2 ( r2 = lapply(1:3, FUN = function (x) { runif(10,0.6,0.95) } ), N = lapply(1:3, FUN = function (x) { sample(200:2000,10,FALSE) } ) )
pool.R2 <- function ( r2, N, quiet = FALSE ) {
           if(!is.list(r2)) {r2 <- list(r2)}
           if(!is.list(N))  {N  <- list(N)}
           if (!missing(N)) {
               stopifnot(length(N) == length(r2) )
               mis.N <- FALSE
               stopifnot( all ( sapply(N, length) == sapply(r2, length) ) )
           }
           if (missing(N))  {
               if(quiet == FALSE ) {cat("No sample size given. Will not compute standard error of pooled R squared.\n")}
               N <- lapply(r2, FUN = function (x) { rep ( 1000, length( x ) ) } )
               mis.N <- TRUE
           }
           Q.i     <- lapply(r2, FUN = function (x) {0.5*log( (1 + sqrt(x)) / (1-sqrt(x))  )})
           Q.i.err <- lapply(N,  FUN = function (n) {1 / (n-3)})
           untransformed <- pool.means(m = Q.i, se = Q.i.err)$summary[c("m.pooled","se.pooled")]
           transformed   <- ((exp(2*untransformed)-1) / (exp(2*untransformed)+1) )^2
           if(mis.N) {return(transformed[1])} else {return(transformed)} }


jk2.pool <- function ( datLong, groupNames ) {                                  ### untere Zeile: Hotfix!
            if( length(which(is.na(datLong[,groupNames[1]])))>0 ) {groupingVar <- "group"} else {groupingVar <- groupNames}
            retList <- do.call("rbind", by(data = datLong, INDICES = datLong[, c(groupingVar,"parameter")], FUN = function ( u ) {
                ### untere Zeile: falls es nicht genauso viele Standardfehler wie Estimates gibt, konnten fuer einige Estimates keine SE berechnet werden. SEs werden daher nicht gepoolt und aus den Daten entfernt.
                if ( length(table ( table(as.character(u[,"coefficient"])))) !=1) {u <- u[u[,"coefficient"] == "est",]}
                if(u[1,"parameter"] %in% c("R2", "R2nagel")) {
                   getNvalid <- merge(u,datLong[datLong[,"parameter"] == "Nvalid",], by = setdiff(colnames(u),c("parameter","value")))
                   pooled    <- t(pool.R2 ( r2 = by(getNvalid, INDICES =getNvalid[,"nesting"], FUN = function ( uu ) {uu[,"value.x"]}), N = by(getNvalid, INDICES =getNvalid[,"nesting"], FUN = function ( uu ) {uu[,"value.y"]}), quiet=TRUE))
                } else {
                   toPool <- by(data = u, INDICES = factor(u[,"coefficient"],levels = c("est","se")),FUN = function ( uu ) {  by(data = uu, INDICES = uu[,"nesting"], FUN = function (uuu) {uuu[,"value"]}) })
                   pooled <- pool.means(m = toPool[[1]], se = toPool[[2]])$summary[c("m.pooled","se.pooled")]
                }
                ret    <- data.frame ( group = names(table(u[,"group"])), parameter = names(table(u[,"parameter"])), coefficient = c("est","se"), value = unlist(pooled), u[1,groupNames,drop=FALSE], stringsAsFactors = FALSE)
                return(ret)}))
            return(retList)}