# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author:    Alexander Robitzsch
#			 a.robitzsch@bifie.at
#			 https://www.bifie.at/user/robitzsch-alexander
#			 
# Quelle:  	 Kopie aus sirtr_0.7-04.R (09.11.2011)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Change Log:
# 2012-09-06 NH
# ADDED: included descriptives for variance and linking error
# ADDED: computation of DIF for item parameters
# 0000-00-00 AA

# MH 26.07.12
# modified so that transformed parameters are calculated with linking constant that is determined by 'method'

#***************************************************************************************************
# Vertical Equating in the Rasch model
equating.rasch <- function( x , y , theta = seq( -4 , 4 , len=100) , method = c("Mean-Mean" , "Haebara" , "Stocking-Lord"), compute.dif = FALSE){
    # INPUT:
    # x ... data frame: 1st column Item labels group 1, 2nd column: item difficulties group 1, 3rd column: standard error of item difficulties
    # y ... data frame: 1st column Item labels group 2, 2nd column: item difficulties group 2, 3rd column: standard error of item difficulties
    # theta ... theta values where the test characteristic curves are evaluated
    #****************************
    # Data preparation 
    method <- match.arg(method)
    b.xy <- data.frame( merge( x , y , by.x = 1 , by.y = 1 ) )
    if(compute.dif == TRUE) {
    colnames(b.xy) <- c("item" , "Itempar.Gr1" , "seItempar.Gr1", "Itempar.Gr2", "seItempar.Gr2" )
    } else {
    colnames(b.xy) <- c("item" , "Itempar.Gr1" , "Itempar.Gr2" )
    }
    # mean-mean method 
        B.mm <- mean(b.xy[,"Itempar.Gr2"]) - mean(b.xy[,"Itempar.Gr1"])
    # Haebara function
        ha <- function(B){
            sum( ( 1 / ( 1+  exp( outer( theta , b.xy[,"Itempar.Gr1"] , "-" )  ) ) - 1 / ( 1 + exp( outer( theta , b.xy[,"Itempar.Gr2"] - B , "-" ) ) ) )^2 )
            }
        B.ha <- optimize(  ha , c(-7,7) )$minimum
    # Stocking and Lord Approach
        sl <- function(B){
            sum( ( rowSums( 1 / ( 1+  exp( outer( theta , b.xy[,"Itempar.Gr1"] , "-" )  ) ) - 1 / ( 1 + exp( outer( theta , b.xy[,"Itempar.Gr2"] - B , "-" ) ) ) ) )^2 )
            }
        B.sl <- optimize(  sl , c(-7,7) )$minimum
    # all parameter estimates    
    B.est <- c( B.mm , B.ha , B.sl )
    names(B.est) <- c("Mean-Mean" , "Haebara" , "Stocking-Lord")
    # Transformation of item parameters (according to 'method')
    b.xy$TransfItempar.Gr1 <- b.xy[,"Itempar.Gr1"] + B.est[method]
    x[,2] <- x[,2] + B.est[method]
    # transformed parameters
    transf.par <- merge( x , y , 1 , 1 , all=T )
    if(compute.dif == TRUE) {
    colnames(transf.par) <- c("item" , "TransfItempar.Gr1" , "seItempar.Gr1", "Itempar.Gr2", "seItempar.Gr2" )
    anchor         <- b.xy
    anchor         <- cbind(anchor, dif.lord(anchor$TransfItempar.Gr1, anchor$Itempar.Gr2, anchor$seItempar.Gr1, anchor$seItempar.Gr2))
    } else {
    colnames(transf.par) <- c("item" , "TransfItempar.Gr1" , "Itempar.Gr2" )
    anchor         <- b.xy[ , c(1,2,4,3)]
    }
    transf.par <- transf.par[ order( paste(transf.par$item ) ) , ]
    # calculate variance and linking error
    des <- data.frame( "N.Items" = nrow(b.xy) , "SD" = sd( b.xy$TransfItempar.Gr1 - b.xy$Itempar.Gr2 ) )
    des$Var <- des$SD^2
    des$linkerror  <- as.vector( sqrt( des["SD"]^2 / des["N.Items"] ) )[1,1]
    # OUTPUT:
    # B.est ... estimated shift parameter according to the three methods    
    # anchor ... original and transformed item parameters of anchor items   
    # transf.par   ... transformed item paramters
    return( list( "B.est" = B.est , "descriptives" = des , 
                 "anchor" = anchor , "transf.par" = transf.par 
                 ) )
        }
#***************************************************************************************************

## Implementiere Formel nach Lord (1980) und ETS-Klassifikation von DIF
dif.lord <- function( b.f, b.r, se.f, se.r) {
  dif     <- b.r - b.f  
  se.dif  <- sqrt(se.r^2 + se.f^2 )
  z.dif   <- ( b.r - b.f )/ se.dif
  sig.dif <- 1* (abs(z.dif) > 1.96)
  KI.90.o <- dif - 2 * 1.645 * se.dif
  KI.90.u <- dif + 2 * 1.645 * se.dif
  KI.95.o <- dif - 2 * 1.96 * se.dif
  KI.95.u <- dif + 2 * 1.96 * se.dif
  class.90 <- ifelse(abs(dif) > .64 & abs(KI.90.o) > .43 & abs(KI.90.u) > .43, "C", ifelse(abs(dif) > .43 & abs(z.dif) > 1.645, "B", "A") )
  class.95 <- ifelse(abs(dif) > .64 & abs(KI.95.o) > .43 & abs(KI.95.u) > .43, "C", ifelse(abs(dif) > .43 & abs(z.dif) > 1.96, "B", "A") )
  result <- data.frame( dif, se.dif, class.90, class.95, z.dif, sig.dif, stringsAsFactors = F)
  return(result)
}
