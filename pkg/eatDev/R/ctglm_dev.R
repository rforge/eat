
# alles löschen
rm(list=ls(all=TRUE))

# Pakete
# if (! "reshape2" %in% installed.packages()[,"Package"]) install.packages("reshape2", repos="http://cran.uni-muenster.de" )
# library ( reshape2 )

# sourcen
source ( "c:/users/hechtmaz/Desktop/eat/pkg/eatRest/R/source.it.all.R" )
source.it.all ( "c:/users/hechtmaz/Desktop/eat/pkg/eatDev/R", FALSE, exclude=c("ctglm_dev.R","bias.rmse.R","MH_ccv.sampling.R","MH_find.devidable.number.R","MH_get.tam.effects.R","MH_lmer.fixef.to.dfr.R","MH_Merge.R","MH_nested.pooling.R","MH_pool.pv.R","MH_tabulate.vars.R","pool.corr.R","SW_get.lmer.effects.R","SW_MH_resJAGS.R") )

# Daten
# load ( "p:/forschung/01_martin/10_ct_AnomAuth/05b_ctsem6man/00_daten/AnomAuth_wide.Rdata" )
# dl <- ctWideToLong(datawide = d, Tpoints=5, n.manifest=6, manifestNames = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6"), n.TDpred=0, TDpredNames = "TD1",n.TIpred=0)
# dl[,"dT"] <- rep( c(0,1,1,2,2), length( unique( dl[,"id"] ) ) )
# dl <- ctDeintervalise(datalong = dl, id='id', dT='dT')
# d <- dl
# save( d, file="c:/users/hechtmaz/Desktop/eat/pkg/eatDev/data/d_6items_2processes_5timepoints.Rdata" )
load ( "c:/users/hechtmaz/Desktop/eat/pkg/eatDev/data/d_6items_2processes_5timepoints.Rdata" )
load ( "c:/users/hechtmaz/Desktop/eat/pkg/eatDev/data/ctExample2.Rdata" )
ctEx2 <- ctExample2[,-4]



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

# ctglm.model
# model <- ctglm.model( d=d )
# model <- ctglm.model( d=d, Lambda=LAMBDA, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctglm.model( d=d, Lambda=LAMBDA, beta=beta, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctglm.model( d=d, Lambda=LAMBDA, A=matrix( c( NA ), 3, 3 ), b=rep(NA,3), priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)"), verbose=FALSE )
# model <- ctglm.model( d=d, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# model <- ctglm.model( d=d, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)"), verbose=FALSE )
# m <- ctglm.model( d=d, Lambda=LAMBDA, A=matrix( c(1.1, "A21", NA, NA ), 2, 2 ), priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# m <- ctglm.model( d=d, Lambda=LAMBDA, priors=list("A21"="dnorm(0.21,21)","A[2,2]"="dnorm(0.22,22)") )
# m <- ctglm.model( d=d, Lambda=LAMBDA, A=matrix(c(1,2,"A12",3),2,2 ), priors=list("A22"="dsfsdfdf") )
# m <- ctglm.model( d=d, Lambda=LAMBDA, A=matrix(c(1,2,"A12",3),2,2 ), A.prior=matrix(c(1,2,"A12",3),2,2 ) )
# m <- ctglm.model( d=d )
# m <- ctglm.model( d=d, Lambda=LAMBDA, b=matrix(c("y","x"),ncol=1) )
# m <- ctglm.model( d=d, Lambda=LAMBDA, beta=matrix(0,nrow=6,ncol=1), measurement.model=gaussian(link="identity") )
# m <- ctglm.model( d=d, Lambda=LAMBDA, beta=matrix(c(0,0,0,0,0,0),nrow=6,ncol=1), measurement.model=gaussian(link="identity") )
# m <- ctglm.model( engine="ctstan", d=d, Lambda=LAMBDA, measurement.model=gaussian(link="identity") )
# m <- ctglm.model( engine="jags", d=d, Lambda=LAMBDA, measurement.model=gaussian(link="identity") )
m <- ctglm.model( engine="ctstan", d=ctEx2, measurement.model=gaussian(link="identity") )

s <- ctglm.syntax( m=m )
# ctglm.save.syntax( s, "C:/users/hechtmaz/Desktop/temp" )

r <- ctglm.run( s=s, work.dir="C:/users/hechtmaz/Desktop/temp", iter=3 )

source.it.all ( "c:/users/hechtmaz/Desktop/eat/pkg/eatDev/R", FALSE, exclude=c("ctglm_dev.R","bias.rmse.R","MH_ccv.sampling.R","MH_find.devidable.number.R","MH_get.tam.effects.R","MH_lmer.fixef.to.dfr.R","MH_Merge.R","MH_nested.pooling.R","MH_pool.pv.R","MH_tabulate.vars.R","pool.corr.R","SW_get.lmer.effects.R","SW_MH_resJAGS.R") )
e <- ctglm.results( r=r, burnin=50, plot.dir="C:/temp/plots" )







