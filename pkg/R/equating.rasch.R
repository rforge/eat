# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# equating.rasch
# Description: 
# Version: 	0.1.0
# Status: beta
# Release Date: 	2011-11-09
# Author:    Alexander Robitzsch
# Change Log:
#				09.11.2011 MH: Kopie aus sirtr_0.7-04.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#***************************************************************************************************
# Vertical Equating in the Rasch model
equating.rasch <- function( x , y , theta = seq( -4 , 4 , len=100) ){
    # INPUT:
    # x ... data frame: 1st column Item labels group 1, 2nd column: item difficulties group 1
    # y ... data frame: 1st column Item labels group 2, 2nd column: item difficulties group 2
    # theta ... theta values where the test characteristic curves are evaluated
    #****************************
    # Data preparation 
    b.xy <- data.frame( merge( x , y , by.x = 1 , by.y = 1 ) )
    colnames(b.xy) <- c("item" , "Itempar.Gr1" , "Itempar.Gr2" )
    # mean-mean method 
        B.mm <- mean(b.xy[,3]) - mean(b.xy[,2])
    # Haebara function
        ha <- function(B){
            sum( ( 1 / ( 1+  exp( outer( theta , b.xy[,2] , "-" )  ) ) - 1/ ( 1 + exp( outer( theta , b.xy[,3] - B , "-" ) ) ) )^2 )
            }
        B.ha <- optimize(  ha , c(-7,7) )$minimum
    # Stocking and Lord Approach
        sl <- function(B){
            sum( ( rowSums( 1 / ( 1+  exp( outer( theta , b.xy[,2] , "-" )  ) ) - 1/ ( 1 + exp( outer( theta , b.xy[,3] - B , "-" ) ) ) ) )^2 )
            }
        B.sl <- optimize(  sl , c(-7,7) )$minimum
    # all parameter estimates    
    B.est <- c( B.mm , B.ha , B.sl )
    names(B.est) <- c("Mean-Mean" , "Haebara" , "Stocking-Lord")
    # Transformation of item parameters (according to Stocking-Lord)
    b.xy$TransfItempar.Gr1 <- b.xy[,2] + B.est[3]
    x[,2] <- x[,2] + B.est[3]
    # transformed parameters
    transf.par <- merge( x , y , 1 , 1 , all=T )
    colnames(transf.par) <- c("item" , "TransfItempar.Gr1" , "Itempar.Gr2"  )
    transf.par <- transf.par[ order( paste(transf.par$item ) ) , ]
    # OUTPUT:
    # B.est ... estimated shift parameter according to the three methods    
    # anchor ... original and transformed item parameters of anchor items   
    # transf.par   ... transformed item paramters
    return( list( "B.est" = B.est , "anchor" = b.xy[ , c(1,2,4,3)] , "transf.par" = transf.par ) )
        }
#***************************************************************************************************
