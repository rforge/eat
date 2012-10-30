pool.means <- function (m, se, NA.rm = FALSE) {
     na.m  <- which(is.na(m))
     na.se <- which(is.na(se))
     if(NA.rm == FALSE) {
       if(length(na.m)>0) {stop("Find unexpected missings in means.\n")}
       if(length(na.se)>0) {stop("Find unexpected missings in standard errors.\n")}
     }
     if(NA.rm == TRUE) {
       if(!all(na.m == na.se)) {stop("Location(s) of missings in means differ from location(s) of missings in standard errors.\n")}
       m <- na.omit(m)
       se <- na.omit(se)
     }
    n <- length(m)
    m.pooled <- mean(m)
    var.within <- mean(se^2)
    var.between    <- 1 / ( n - 1 ) * sum((m - m.pooled )^2)
    var.total <- var.within + (n + 1) * var.between/n
    se.pooled <- sqrt(var.total)
    pooled <- list(m = m, var = se^2, summary = data.frame ( n = n, var.within = var.within,
        var.between = var.between, var.total = var.total,  m.pooled = m.pooled, se.pooled = se.pooled, stringsAsFactors = FALSE ) )
    return(pooled) }


### r2         ... Vektor von R^2-Werten aus multiple imputieren Analysen
### N          ... Optional. Vektor aus Ns der jeweiligen Imputationen, muss genauso lang wie r2 sein. Ohne den Vektor gibt's keinen Standardfehler, nur ein gepooltes R^2
pool.R2 <- function ( r2, N, quiet = FALSE ) {
           if (!missing(N)) {stopifnot(length(N) == length(r2) ); mis.N <- FALSE }
           if (missing(N))  {
               if(quiet == FALSE ) {cat("No sample size given. Will not compute standard error of pooled R squared.\n")}
               N <- 1000
               mis.N <- TRUE
           }
           Q.i     <- 0.5*log( (1 + sqrt(r2)) / (1-sqrt(r2))  )
           Q.i.err <- 1 / (N-3)
           untransformed <- pool.means(m = Q.i, se = Q.i.err)$summary[c("m.pooled","se.pooled")]
           transformed   <- ((exp(2*untransformed)-1) / (exp(2*untransformed)+1) )^2
           if(mis.N) {return(transformed[1])} else {return(transformed)} }
