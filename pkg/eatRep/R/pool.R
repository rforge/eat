pool.means <- function (m, se, na.rm = FALSE) {
     if(!is.list(m))  { listM  <- list(m)}  else {listM  <- m }
     if(!is.list(se)) { listSE <- list(se)} else {listSE <- se}
     stopifnot(all(unlist(lapply(listM, length)) == unlist(lapply(listSE, length)) ) )
     stopifnot(is.list(listM))
     stopifnot(is.list(listSE))                                                 ### Untere Zeile: Listeneintraege mit 0 Elementen werden geloescht
     if( length(which(unlist(lapply(listM, length)) == 0 )) > 0 ) {listM <- listM[-which(unlist(lapply(listM, length)) == 0 )]}
     if( length(which(unlist(lapply(listSE, length)) == 0 )) > 0 ) {listSE <- listSE[-which(unlist(lapply(listSE, length)) == 0 )]}
     listM  <- data.frame(t(data.frame(listM,  stringsAsFactors = FALSE)), stringsAsFactors = FALSE )
     listSE <- data.frame(t(data.frame(listSE, stringsAsFactors = FALSE)), stringsAsFactors = FALSE )
     na.m  <- lapply(listM, FUN = function ( m ) {which(is.na(m))})
     na.se <- lapply(listSE, FUN = function ( se ) {which(is.na(se))})
     if(na.rm == FALSE) {
       if(!all(lapply(na.m, length) == 0 ) ) {stop("Find unexpected missings in means.\n")}
       if(!all(lapply(na.se, length) == 0) ) {stop("Find unexpected missings in standard errors.\n")}
     }
     if(na.rm == TRUE) {
       if(!all(unlist(lapply(na.m, length)) == unlist(lapply(na.se, length))) ) {stop("Location(s) of missings in means differ from location(s) of missings in standard errors.\n")}
       listM  <- lapply(listM, FUN = function (m) {m <- na.omit(m)})
       listSE <- lapply(listSE, FUN = function (se) {se <- na.omit(se)})
     }
     N        <- length(listM[[1]])
     M        <- length(listM)
     Q.all    <- mean(unlist(lapply(listM, mean)))                              ### Rubin 2003b, S. 6, unterste Formel
     Q.m      <- lapply(listM, mean)                                            ### Rubin 2003b, S. 7, 1. Formel
     U        <- mean(unlist(lapply(listSE, FUN = function ( se ) {mean(se^2)})))## Rubin 2003b, S. 7, 2. Formel
     MS.b     <- N/(M-1) * sum((unlist(Q.m) - Q.all)^2)
     MS.omega <- 1/(M*(N-1)) * sum(unlist(lapply(listM, FUN = function ( mm ) { sum((mm - mean(mm))^2)})))
     if(is.na(MS.omega)) {MS.omega <- 0}                                        ### wenn es keine Nestungen gibt, soll die Varianz zwischen Nestungen auf 0 gesetzt sein
     var.total <- U+1/N*(1+1/M)*MS.b + (1-1/N)*MS.omega                         ### Rubin 2003b, "The quantity T", S. 7, 5. Formel
     se.pooled <- sqrt(var.total)
     df        <- NA
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

