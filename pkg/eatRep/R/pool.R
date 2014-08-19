pool.means <- function (m, se, na.rm = FALSE) {
     if(is.list(m))   {if(length(m) == 1)  { stopifnot(length(se)==1); m <- unlist(m); se <- unlist(se)}}
     if(!is.list(m))  {                                                         ### keine genestete Struktur: wird an pool.scalar aus "mice" uebergeben
        pooled <- mice::pool.scalar(Q=m, U=se^2)
        pooled <- data.frame ( m.pooled = pooled$qbar, se.pooled = sqrt(pooled$t), df = pooled$df, stringsAsFactors = FALSE)
     }  else  {                                                                 ### genestete Struktur
        stopifnot(all(unlist(lapply(m, length)) == unlist(lapply(se, length)) ) )## keine missings erlaubt 
        M      <- length(m)
        N      <- length(m[[1]])
        Q.m    <- lapply(m,mean)                                                ### Rubin 2003b, S. 7, 1. Formel. 
        Qbar   <- mean(unlist(Q.m))                                             ### Rubin 2003b, S. 6, unterste Formel
        U      <- mean(unlist(lapply(se, FUN = function ( x ) { mean(x^2) } ))) ### Rubin 2003b, S. 7, 2. Formel
        MS.b   <- N/(M-1) * sum((unlist(Q.m) - Qbar)^2)                         ### untere Zeile: Rubin 2003b, S. 7, 4. Formel
        MS.omeg<- 1/(M*(N-1)) *  sum(unlist(lapply(m, FUN = function ( x ) { sum((x-mean(x))^2) })))
        varT   <- U+1/N*(1+1/M)*MS.b + (1-1/N)*MS.omeg                          ### Rubin 2003b, "The quantity T", S. 7, 5. Formel
        dfN    <- 1 / ( (1/N*(1+1/M)*MS.b / varT)^2 * 1/(M-1) + (  ((1-1/N)*MS.omeg )/varT)^2 * 1/ (M * (N-1)) )
        pooled <- data.frame ( m.pooled = Qbar, se.pooled = sqrt(varT), df = dfN, stringsAsFactors = FALSE)
     }
     return(list(summary=pooled))}


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


jk2.pool <- function ( datLong, allNam ) {                                    
            retList <- do.call("rbind", by(data = datLong, INDICES = datLong[, c("group","parameter")], FUN = function ( u ) {
               uM   <- by(u, INDICES = u[,c(allNam[["nest"]] )], FUN = function ( uN ) { uN[which(uN[,"coefficient"] == "est"),"value"]})
               uSE  <- by(u, INDICES = u[,c(allNam[["nest"]] )], FUN = function ( uN ) { uN[which(uN[,"coefficient"] == "se"),"value"]})
               if(u[1,"parameter"] %in% c("R2", "R2nagel")) {    
                  getNvalid <- datLong[ intersect( intersect(  which(datLong[,"group"] == u[1,"group"]), which( datLong[,"parameter"] == "Nvalid")), which( datLong[,"coefficient"] == "est") ) ,]
                  pooled    <- t(pool.R2(r2 = u[,"value"], N = getNvalid[,"value"]))
               } else {
                  pooled <- pool.means(m = uM, se = uSE)$summary[c("m.pooled","se.pooled")]
               }
               ret    <- data.frame ( group = names(table(u[,"group"])), depVar = allNam[["dependent"]], modus="noch_leer", parameter = names(table(u[,"parameter"])), coefficient = c("est","se"), value = unlist(pooled), u[1,allNam[["group"]],drop=FALSE], stringsAsFactors = FALSE)
               return(ret)}))
           return(retList)}
