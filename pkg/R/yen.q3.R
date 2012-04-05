# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:    Alexander Robitzsch
#			 a.robitzsch@bifie.at
#			 https://www.bifie.at/user/robitzsch-alexander
#			 
# Quelle:  	 Kopie aus sirtr_??, Version nicht mehr identifizierbar
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Yen's Q3 statistic (1984)
yen.q3 <- function( dat , theta , b , progress = T ){
        # INPUT:
        # dat       ... data frame
        # theta     ... theta estimate
        # b         ... item difficulty estimate
        cat("Yen's Q3 Statistic  \n" )
        I <- ncol(dat)
        cat(I , "Items \n" )
        # expected probability
        expected <- .prob.rasch( theta , b )
        # residual
        residual <- dat - expected
        # initialize matrix of Q3 values
        I <- ncol(dat)
        q3.matr <- matrix( NA , I , I )
        q3.long <- matrix( NA , (I-1)*I/2 , 4 )
        colnames(q3.matr) <- rownames(q3.matr) <- colnames(dat)
        v <- 1
        for (ii in 1:(I-1)){
            for (jj in (ii+1):I){
            r.ii.jj <- na.omit( residual[ ,c(ii,jj) ] )
            if (nrow(r.ii.jj) > 0 ){
                q3.long[ v , 3] <- q3.matr[ii,jj] <- q3.matr[jj,ii] <- cor( r.ii.jj )[1,2]
                q3.long[v,1] <- colnames(dat)[ii]
                q3.long[v,2] <- colnames(dat)[jj]
                q3.long[v,4] <- nrow( r.ii.jj )
                v <- v + 1
                    }
                    }
                if (progress == T){ if ( ii %% 15 == 0) { cat(" " , ii , " \n" ) } else { cat( " " , ii ) }
                        flush.console()
                }
            }
        cat("\n" )
        q3.long <- as.data.frame( q3.long )
        q3.long[,3] <- as.numeric( paste( q3.long[,3] ))
        q3.long <- q3.long[ order( q3.long[,3] ) , ]
        colnames(q3.long) <- c("Item1" , "Item2" , "Q3" , "N" )
        q3.long <-   q3.long[ !is.na( q3.long[,3] ) , ]
        res <- list( "q3.matrix" = q3.matr , "q3.long" = q3.long )
        return(res)
        }

### Formatiert die Rückgabe von "yen.q3" passend zur Ergebnisstruktur
.q3.to.structure <- function(q3.results)  {
                    long      <- q3.results$q3.long
                    for (i in 1:2) {long[,i] <- as.character(long[,i])}
                    long$kombination <- paste(long$Item1,long$Item2,sep="")
                    all.items <- unique(c(long$Item1,long$Item2))
                    res <- lapply(all.items,FUN=function(ii)
                           {choice <- grep(ii,long$kombination)
                            werte  <- long$Q3[choice]
                            names(werte) <- gsub(ii,"",long$kombination[choice])
                            werte  <- as.list(werte)})
                    names(res) <- all.items
                    return(res)}

# calculate P_i ( theta)
.prob.rasch <- function( theta , b ){
    plogis( outer( theta , b , "-" ) ) 
    }

