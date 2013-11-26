
### subroutine for combining correlations for multiply imputed data - basiert auf Funktion von Alexander Robitzsch
pool.corr <- function( corrs , N , conf.level = .05){
        fisher.corrs <- 1/2*log( ( 1 + corrs) / ( 1 - corrs ) )                 ### convert correlations to Fisher transformed values
        var.fisher <- rep( 1/(N-3) , length(corrs) )
        if(!exists("pool.scalar"))   {library(mice)}                            ### combination of point estimators according Rubin's formula
        fisher.cor.combine <- pool.scalar( fisher.corrs , var.fisher)
        zr <- fisher.cor.combine$qbar
        zr.se <- sqrt( fisher.cor.combine$t )
        t.zr <- zr / zr.se
        fisher2cor <- function(z){ ( exp(2*z) - 1 )/ ( exp(2*z) + 1 ) }
        res <- c( "r" = fisher2cor(zr)  ,
            "fisher_r" = zr ,
            "fisher_rse" = zr.se ,
            "t" = t.zr  ,
            "p" = 2 * pnorm( abs(t.zr) , lower.tail = FALSE ) ,
             fisher2cor( zr + qnorm( ( 1 - conf.level ) / 2 ) * zr.se ) ,
             fisher2cor( zr - qnorm( ( 1 - conf.level ) / 2 ) * zr.se ) )
            names(res)[6] <- paste( "lower" , round(100*conf.level,2),sep="")
            names(res)[7] <- paste( "upper" , round(100*conf.level,2),sep="")
        res <- c( res , ( res[6] - res[7] ) / ( 2* qnorm( ( 1 - conf.level )/2 ) ) )
        names(res)[8] <- "rse"
        res <- res[ c(1,8,2:7) ]
        res <- round(res, 6)
        return(res) }
