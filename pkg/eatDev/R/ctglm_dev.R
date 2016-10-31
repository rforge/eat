
# alles löschen
rm(list=ls(all=TRUE))

# Pakete
if (! "reshape2" %in% installed.packages()[,"Package"]) install.packages("reshape2", repos="http://cran.uni-muenster.de" )
library ( reshape2 )

# sourcen
source ( "c:/users/hechtmaz/Desktop/eat/pkg/eatRest/R/source.it.all.R" )
source.it.all ( "c:/users/hechtmaz/Desktop/eat/pkg/eatDev/R", FALSE, exclude=c("bias.rmse.R","MH_ccv.sampling.R","MH_find.devidable.number.R","MH_get.tam.effects.R","MH_lmer.fixef.to.dfr.R","MH_Merge.R","MH_nested.pooling.R","MH_pool.pv.R","MH_tabulate.vars.R","pool.corr.R","SW_get.lmer.effects.R","SW_MH_resJAGS.R") )

# Daten
load ( "p:/forschung/01_martin/10_ct_AnomAuth/05b_ctsem6man/00_daten/AnomAuth_wide.Rdata" )

# LAMBDA
# LAMBDA <- matrix( c(1,NA,1,0,0,0,0,0,0,1,1,1), nrow=6, ncol=2 )
LAMBDA <- matrix( c(1,1,1,0,0,0,0,0,0,1,1,1), nrow=6, ncol=2 )
rownames(LAMBDA) <- c("Y1","Y3","Y5","Y2","Y4","Y6")
colnames(LAMBDA) <- c("lat1","lat2")

# beta
# beta <- c(1,2,NA,NA,5,6)
# names(beta) <- c("Y1","Y3","Y5","Y2","Y4","Y6")

# E
# E <- diag(NA,6)
# rownames(E) <- colnames(E) <- c("Y1","Y3","Y5","Y2","Y4","Y6")

# ctirt.model
# model <- ctirt.model( d=d )
# model <- ctirt.model( d=d, Lambda=LAMBDA, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctirt.model( d=d, Lambda=LAMBDA, beta=beta, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctirt.model( d=d, Lambda=LAMBDA, A=matrix( c( NA ), 3, 3 ), b=rep(NA,3), priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)"), verbose=FALSE )
# model <- ctirt.model( d=d, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctirt.model( d=d, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)"), verbose=FALSE )
# m <- ctirt.model( d=d, Lambda=LAMBDA, A=matrix( c(1.1, "A21", NA, NA ), 2, 2 ), priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# m <- ctirt.model( d=d, Lambda=LAMBDA, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# m <- ctirt.model( d=d, Lambda=LAMBDA, A=matrix(c(1,2,"A12",3),2,2 ), priors=list("A22"="dsfsdfdf") )
# m <- ctirt.model( d=d, Lambda=LAMBDA, A=matrix(c(1,2,"A12",3),2,2 ), A.prior=matrix(c(1,2,"A12",3),2,2 ) )
# m <- ctirt.model( d=d )
# m <- ctirt.model( d=d, Lambda=LAMBDA, b=matrix(c("y","x"),ncol=1) )
# m <- ctirt.model( d=d, Lambda=LAMBDA, beta=matrix(0,nrow=6,ncol=1), measurement.model=gaussian(link="identity") )
# m <- ctirt.model( d=d, Lambda=LAMBDA, beta=matrix(c(0,0,0,0,0,0),nrow=6,ncol=1), measurement.model=gaussian(link="identity") )
m <- ctirt.model( d=d, Lambda=LAMBDA, measurement.model=gaussian(link="identity") )

s <- ctirt.syntax( m=m )
# s$syntax[43,1] <- "mu.beta <- 0"  
# s$syntax[46,1] <- "prec.beta ~ dnorm(0,0.001)"  

r <- ctirt.run( s=s, work.dir="C:/users/hechtmaz/Desktop/temp", iter=3 )

e <- ctirt.results( r=r, burnin=50, plot.dir="C:/temp/plots" )



