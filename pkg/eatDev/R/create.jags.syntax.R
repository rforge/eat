
create.jags.syntax <- function ( env ) {

		# get variables from env
		eval( parse( text=paste0( "assign( '",ls(envir=env), "' , get('",ls(envir=env),"', envir=env ) )" ) ) )
		
		# syntax matrix (1 column)
		x<-matrix(  "model", 1, 1 )
		x<-rbind(x, "{                                                                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### DATA ###                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # data d in long format                                          ")
		x<-rbind(x, "    # d[,col.id]: person                                             ")
		x<-rbind(x, "    # d[,col.item]: item                                             ")
		x<-rbind(x, "    # d[,col.time]: time point                                       ")
		x<-rbind(x, "    # d[,col.y]: responses                                           ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "    ### MEASUREMENT MODEL ###                                        ")
		x<-rbind(x, "                                                                     ")
		# x<-rbind(x, "    # estimated parameters                                           ")
		# x<-rbind(x, "    # theta:        J (person)    x F (faktor)    x T (time point)   ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over rows in long data set d                              ")
		x<-rbind(x, "    for (r in 1:R) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # distributional assumption                                  ")
		if( measurement.model$family %in% "binomial" ) {
		x<-rbind(x, "        d[r,col.y] ~ dbern( mu.y[r] )                                ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # link                                                       ")
		if( measurement.model$link %in% "logit" ) {
		x<-rbind(x, "        mu.y[r] <- ilogit( eta[r] )                                  ") }
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "        # linear predictor                                           ")
		x<-rbind(x, "        eta[r] <- sum( Lambda[ d[r,col.item],  , d[r,col.time] ] * theta[ d[r,col.id], , d[r,col.time] ] )  +  beta[ d[r,col.item], 1 ]  ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    }                                                                ")
		# x<-rbind(x, "                                                                     ")
		# x<-rbind(x, "    # precision of measurement errors                                ")
		# x<-rbind(x, "    for (i in 1:I) {                                                 ")
		# x<-rbind(x, "        E[i] ~ dgamma(1,0.005)                                       ")
		# x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/priors of item easiness                                 ")
		x<-rbind(x, make.str( "beta" ) )
		x<-rbind(x, "                                                                     ")
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of mean of item easiness                         ")
		x<-rbind(x, make.str( "mu.beta" ) )
		x<-rbind(x, "                                                                     ") }
		if ( exists( "mu.beta" ) ) {
		x<-rbind(x, "    # values/priors of precision of item easiness                    ")
		x<-rbind(x, make.str( "prec.beta" ) )
		x<-rbind(x, "                                                                     ") }	
		# x<-rbind(x, "    # standard deviation of latent scale                             ")
		# x<-rbind(x, "    #sd.eta <- sd(eta[])                                             ")
		# x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ### CONTINUOUS TIME MODEL ###                                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # loop over persons j                                            ")
		x<-rbind(x, "    for (j in 1:J) {                                                 ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # first time point                                       ")
		x<-rbind(x, "            theta[j,1:F,1] ~ dmnorm( mu.t1, prec.t1 )                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "            # loop over t=2,...,Tj personal time point               ")
		x<-rbind(x, "            for (t in 2:Tj[j]) {                                     ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "                    theta[j,1:F,t] ~ dmnorm( At[,,t-1,Lpat.group[j]] %*% theta[j,,t-1] + bt[,t-1,Lpat.group[j]], Qt.prec.adj[1:2,1:2,t,Lpat.group[j]] ) ")
		x<-rbind(x, "                                                                     ")		
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of mean of first time point                       ")
		x<-rbind(x, make.str( "mu.t1" ) )
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of precision of first time point                  ")
		x<-rbind(x, ifelse( exists("prec.t1.prior") && is.null( dim(prec.t1.prior) ), paste0( "    prec.t1 ~ ", prec.t1.prior, "                                      " ), make.str( "prec.t1" ) ) )
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # values/prior of drift matrix                                   ")
		x<-rbind(x, make.str( "A" ) )
		x<-rbind(x, "                                                                     ")		
## TODO symmetric matrix		
		x<-rbind(x, "    # values/prior of diffusion matrix                               ")
		x<-rbind(x, make.str( "Q" ) )
		x<-rbind(x, "                                                                     ")	
		# x<-rbind(x, "    # Q Matrix                                                       ")
		# x<-rbind(x, "    Q[1,1] ~ dgamma(1,1)                                             ")
		# x<-rbind(x, "    Q[1,2] ~ dnorm(0,16)                                             ")
		# x<-rbind(x, "    Q[2,1] <- Q[1,2]                                                 ")
		# x<-rbind(x, "    Q[2,2] ~ dgamma(1,1)                                             ")
		x<-rbind(x, "    # values/prior of continuous time intercepts                     ")
		x<-rbind(x, make.str( "b" ) )
		x<-rbind(x, "                                                                     ")			
		
# browser()		

		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # rowQ: stack Q row-wise into a column vector                    ")
		x<-rbind(x, "    for ( z in (1:Aw) ) {                                            ")
		x<-rbind(x, "        for ( c in (1:Aw) ) {                                        ")
		x<-rbind(x, "            rowQ[ c+Aw*(z-1), 1 ] <- Q[z,c]                          ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## discrete/continuous time relations                            ")
		x<-rbind(x, "    # Qtv for first time point                                       ")
		x<-rbind(x, "    # for technical reasons, same values are set for all lag patterns (not necessary)  ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            Qtv[1:4,1,p] <- ( -1 * Ah.inv[,] ) %*% rowQ[,1]          ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # loop over p=1,...,P lag patterns                               ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            # loop over t=2,...,Tp pattern-specific time point       ")
		x<-rbind(x, "            for (t in 2:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # autoregression matrix / drift matrix           ")
		x<-rbind(x, "                    At[1:2,1:2,t-1,p] <- mexp( A[,] * Lpat[p,t-1] )  ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # intercepts                                     ")
		x<-rbind(x, "                    bt[1:2,t-1,p] <- ( A.inv[,] %*% ( mexp( A[,] * Lpat[p,t-1] ) - I1 ) ) %*% b[]         ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qtv is vectorized Qt matrix                    ")
		x<-rbind(x, "                    #Qtv[1:4,t,p] <- ifelse( t==1, ( -1 * Ah.inv[,] ) %*% rowQ[,1] , ( Ah.inv[,] %*% ( mexp( Ah[,] * Lpat[p,t-1] ) - I2 ) ) %*% rowQ[,1] )     ")
		x<-rbind(x, "                    Qtv[1:4,t,p] <- Ah.inv[,] %*% ( mexp( Ah[,] * Lpat[p,t-1] ) - I2 ) %*% rowQ[,1]     ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## calculate Qt.prec                                             ")
		x<-rbind(x, "    for(p in 1:P) {                                                  ")
		x<-rbind(x, "            for (t in 1:Tp[p]) {                                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # Qt matrix (create with 'irow')                 ")
		x<-rbind(x, "                    for ( z in (1:Aw) ) {                            ")
		x<-rbind(x, "                        for ( c in (1:Aw) ) {                        ")
		x<-rbind(x, "                            Qt[z,c,t,p] <- Qtv[ c+Aw*(z-1),t,p]      ")
		x<-rbind(x, "                        }                                            ")
		x<-rbind(x, "                    }                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    ### creation of Qt precision matrix              ")
		x<-rbind(x, "                    ## inverse of Qt (!!only 2x2!!)                  ")
		x<-rbind(x, "                    # determinant of Qt                              ")
		x<-rbind(x, "                    Qt.det[t,p] <- Qt[1,1,t,p]*Qt[2,2,t,p] - Qt[2,1,t,p]*Qt[1,2,t,p]     ")
		x<-rbind(x, "                    # Qt is invertible if determinant is not 0       ")
		x<-rbind(x, "                    Qt.invertible[t,p] <- ifelse( Qt.det[t,p]==0, 0, 1 )     ")
		x<-rbind(x, "                    # if not invertible, modest mod of determinant   ")
		x<-rbind(x, "                    detmod5[t,p] ~ dbern(0.5)                        ")
		x<-rbind(x, "                    detmod6[t,p] <- ifelse( Qt.invertible[t,p]==1, 0, (detmod5[t,p]-0.5)/500 )     ")
		x<-rbind(x, "                    Qt.det.adj[t,p] <- Qt.det[t,p] + detmod6[t,p]    ")
		x<-rbind(x, "                    # inverse of Qt                                  ")
		x<-rbind(x, "                    Qt.help[1,1,t,p] <- Qt[2,2,t,p]                  ")
		x<-rbind(x, "                    Qt.help[2,2,t,p] <- Qt[1,1,t,p]                  ")
		x<-rbind(x, "                    Qt.help[1,2,t,p] <- -1*Qt[1,2,t,p]               ")
		x<-rbind(x, "                    Qt.help[2,1,t,p] <- -1*Qt[2,1,t,p]               ")
		x<-rbind(x, "                    Qt.prec[1:Aw,1:Aw,t,p] <- (1/Qt.det.adj[t,p] * Qt.help[,,t,p])     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # check if Qt.prec is positive definite          ")
		x<-rbind(x, "                    Qt.prec.det[t,p] <- Qt.prec[1,1,t,p]*Qt.prec[2,2,t,p] - Qt.prec[2,1,t,p]*Qt.prec[1,2,t,p]     ")
		x<-rbind(x, "                    Qt.prec.posdef[t,p] <- ifelse(  Qt.prec[1,1,t,p] > 0 && Qt.prec.det[t,p] > 0, 1, 0 )     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "                    # if Qt.prec is not pos def then replace by a pos def matrix     ")
		x<-rbind(x, "                    Qt.prec.adj[1:2,1:2,t,p] <- ifelse( Qt.prec.posdef[t,p]==1, Qt.prec[,,t,p] , Qt.prec.replace[,]  )     ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## inverse of A (!!only 2x2!!)                                   ")
		x<-rbind(x, "    # determinant of A                                               ")
		x<-rbind(x, "    A.det <- A[1,1]*A[2,2] - A[2,1]*A[1,2]                           ")
		x<-rbind(x, "    # A is invertible if determinant is not 0                        ")
		x<-rbind(x, "    A.invertible <- ifelse( A.det==0, 0, 1 )                         ")
		x<-rbind(x, "    # if not invertible, modest mod of determinant                   ")
		x<-rbind(x, "    detmod1 ~ dbern(0.5)                                             ")
		x<-rbind(x, "    detmod2 <- ifelse( A.invertible==1, 0, (detmod1-0.5)/500 )       ")
		x<-rbind(x, "    A.det.adj <- A.det + detmod2                                     ")
		x<-rbind(x, "    # inverse of A                                                   ")
		x<-rbind(x, "    A.help[1,1] <- A[2,2]                                            ")
		x<-rbind(x, "    A.help[2,2] <- A[1,1]                                            ")
		x<-rbind(x, "    A.help[1,2] <- -1*A[1,2]                                         ")
		x<-rbind(x, "    A.help[2,1] <- -1*A[2,1]                                         ")
		x<-rbind(x, "    A.inv[1:Aw,1:Aw] <- (1/A.det.adj * A.help[,])                    ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    ## inverse of Ah (!!only 4x4!!)                                  ")
		x<-rbind(x, "    # http://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix                                                                                                     ")
		x<-rbind(x, "    Ah.inv.help[1,1] <- Ah[2,2]*Ah[3,3]*Ah[4,4]-Ah[2,2]*Ah[3,4]*Ah[4,3]-Ah[3,2]*Ah[2,3]*Ah[4,4]+Ah[3,2]*Ah[2,4]*Ah[4,3]+Ah[4,2]*Ah[2,3]*Ah[3,4]-Ah[4,2]*Ah[2,4]*Ah[3,3]     ")
		x<-rbind(x, "    Ah.inv.help[2,1] <- -Ah[2,1]*Ah[3,3]*Ah[4,4]+Ah[2,1]*Ah[3,4]*Ah[4,3]+Ah[3,1]*Ah[2,3]*Ah[4,4]-Ah[3,1]*Ah[2,4]*Ah[4,3]-Ah[4,1]*Ah[2,3]*Ah[3,4]+Ah[4,1]*Ah[2,4]*Ah[3,3]    ")
		x<-rbind(x, "    Ah.inv.help[3,1] <- Ah[2,1]*Ah[3,2]*Ah[4,4]-Ah[2,1]*Ah[3,4]*Ah[4,2]-Ah[3,1]*Ah[2,2]*Ah[4,4]+Ah[3,1]*Ah[2,4]*Ah[4,2]+Ah[4,1]*Ah[2,2]*Ah[3,4]-Ah[4,1]*Ah[2,4]*Ah[3,2]     ")
		x<-rbind(x, "    Ah.inv.help[4,1] <- -Ah[2,1]*Ah[3,2]*Ah[4,3]+Ah[2,1]*Ah[3,3]*Ah[4,2]+Ah[3,1]*Ah[2,2]*Ah[4,3]-Ah[3,1]*Ah[2,3]*Ah[4,2]-Ah[4,1]*Ah[2,2]*Ah[3,3]+Ah[4,1]*Ah[2,3]*Ah[3,2]    ")
		x<-rbind(x, "    Ah.inv.help[1,2] <- -Ah[1,2]*Ah[3,3]*Ah[4,4]+Ah[1,2]*Ah[3,4]*Ah[4,3]+Ah[3,2]*Ah[1,3]*Ah[4,4]-Ah[3,2]*Ah[1,4]*Ah[4,3]-Ah[4,2]*Ah[1,3]*Ah[3,4]+Ah[4,2]*Ah[1,4]*Ah[3,3]    ")
		x<-rbind(x, "    Ah.inv.help[2,2] <- Ah[1,1]*Ah[3,3]*Ah[4,4]-Ah[1,1]*Ah[3,4]*Ah[4,3]-Ah[3,1]*Ah[1,3]*Ah[4,4]+Ah[3,1]*Ah[1,4]*Ah[4,3]+Ah[4,1]*Ah[1,3]*Ah[3,4]-Ah[4,1]*Ah[1,4]*Ah[3,3]     ")
		x<-rbind(x, "    Ah.inv.help[3,2] <- -Ah[1,1]*Ah[3,2]*Ah[4,4]+Ah[1,1]*Ah[3,4]*Ah[4,2]+Ah[3,1]*Ah[1,2]*Ah[4,4]-Ah[3,1]*Ah[1,4]*Ah[4,2]-Ah[4,1]*Ah[1,2]*Ah[3,4]+Ah[4,1]*Ah[1,4]*Ah[3,2]    ")
		x<-rbind(x, "    Ah.inv.help[4,2] <- Ah[1,1]*Ah[3,2]*Ah[4,3]-Ah[1,1]*Ah[3,3]*Ah[4,2]-Ah[3,1]*Ah[1,2]*Ah[4,3]+Ah[3,1]*Ah[1,3]*Ah[4,2]+Ah[4,1]*Ah[1,2]*Ah[3,3]-Ah[4,1]*Ah[1,3]*Ah[3,2]     ")
		x<-rbind(x, "    Ah.inv.help[1,3] <- Ah[1,2]*Ah[2,3]*Ah[4,4]-Ah[1,2]*Ah[2,4]*Ah[4,3]-Ah[2,2]*Ah[1,3]*Ah[4,4]+Ah[2,2]*Ah[1,4]*Ah[4,3]+Ah[4,2]*Ah[1,3]*Ah[2,4]-Ah[4,2]*Ah[1,4]*Ah[2,3]     ")
		x<-rbind(x, "    Ah.inv.help[2,3] <- -Ah[1,1]*Ah[2,3]*Ah[4,4]+Ah[1,1]*Ah[2,4]*Ah[4,3]+Ah[2,1]*Ah[1,3]*Ah[4,4]-Ah[2,1]*Ah[1,4]*Ah[4,3]-Ah[4,1]*Ah[1,3]*Ah[2,4]+Ah[4,1]*Ah[1,4]*Ah[2,3]    ")
		x<-rbind(x, "    Ah.inv.help[3,3] <- Ah[1,1]*Ah[2,2]*Ah[4,4]-Ah[1,1]*Ah[2,4]*Ah[4,2]-Ah[2,1]*Ah[1,2]*Ah[4,4]+Ah[2,1]*Ah[1,4]*Ah[4,2]+Ah[4,1]*Ah[1,2]*Ah[2,4]-Ah[4,1]*Ah[1,4]*Ah[2,2]     ")
		x<-rbind(x, "    Ah.inv.help[4,3] <- -Ah[1,1]*Ah[2,2]*Ah[4,3]+Ah[1,1]*Ah[2,3]*Ah[4,2]+Ah[2,1]*Ah[1,2]*Ah[4,3]-Ah[2,1]*Ah[1,3]*Ah[4,2]-Ah[4,1]*Ah[1,2]*Ah[2,3]+Ah[4,1]*Ah[1,3]*Ah[2,2]    ")
		x<-rbind(x, "    Ah.inv.help[1,4] <- -Ah[1,2]*Ah[2,3]*Ah[3,4]+Ah[1,2]*Ah[2,4]*Ah[3,3]+Ah[2,2]*Ah[1,3]*Ah[3,4]-Ah[2,2]*Ah[1,4]*Ah[3,3]-Ah[3,2]*Ah[1,3]*Ah[2,4]+Ah[3,2]*Ah[1,4]*Ah[2,3]    ")
		x<-rbind(x, "    Ah.inv.help[2,4] <- Ah[1,1]*Ah[2,3]*Ah[3,4]-Ah[1,1]*Ah[2,4]*Ah[3,3]-Ah[2,1]*Ah[1,3]*Ah[3,4]+Ah[2,1]*Ah[1,4]*Ah[3,3]+Ah[3,1]*Ah[1,3]*Ah[2,4]-Ah[3,1]*Ah[1,4]*Ah[2,3]     ")
		x<-rbind(x, "    Ah.inv.help[3,4] <- -Ah[1,1]*Ah[2,2]*Ah[3,4]+Ah[1,1]*Ah[2,4]*Ah[3,2]+Ah[2,1]*Ah[1,2]*Ah[3,4]-Ah[2,1]*Ah[1,4]*Ah[3,2]-Ah[3,1]*Ah[1,2]*Ah[2,4]+Ah[3,1]*Ah[1,4]*Ah[2,2]    ")
		x<-rbind(x, "    Ah.inv.help[4,4] <- Ah[1,1]*Ah[2,2]*Ah[3,3]-Ah[1,1]*Ah[2,3]*Ah[3,2]-Ah[2,1]*Ah[1,2]*Ah[3,3]+Ah[2,1]*Ah[1,3]*Ah[3,2]+Ah[3,1]*Ah[1,2]*Ah[2,3]-Ah[3,1]*Ah[1,3]*Ah[2,2]     ")
		x<-rbind(x, "    Ah.det <- Ah[1,1]*Ah.inv.help[1,1]+Ah[1,2]*Ah.inv.help[2,1]+Ah[1,3]*Ah.inv.help[3,1]+Ah[1,4]*Ah.inv.help[4,1]                                                           ")
		x<-rbind(x, "    # Ah is invertible if determinant is not 0                       ")
		x<-rbind(x, "    Ah.invertible <- ifelse( Ah.det==0, 0, 1 )                       ")
		x<-rbind(x, "    # if not invertible, modest mod of determinant                   ")
		x<-rbind(x, "    detmod3 ~ dbern(0.5)                                             ")
		x<-rbind(x, "    detmod4 <- ifelse( A.invertible==1, 0, (detmod3-0.5)/500 )       ")
		x<-rbind(x, "    Ah.det.adj <- Ah.det + detmod4                                   ")
		x<-rbind(x, "    Ah.inv <- Ah.inv.help / Ah.det.adj                               ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "    # Kronecker of A and I1                                          ")
		x<-rbind(x, "    # https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/17ddb2ce/     ")
		x<-rbind(x, "    for (i in 1:Aw){              # Aw = width of A                  ")
		x<-rbind(x, "        for (j in 1:Aw){                                             ")
		x<-rbind(x, "            for (k in 1:I1w){     # I1w = width of I1                ")
		x<-rbind(x, "                for (l in 1:I1w){                                    ")
		x<-rbind(x, "                     Ah1[(i-1)*I1w+k,(j-1)*I1w+l] <- A[i,j]*I1[k,l]  ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Kronecker of I1 and A                                          ")
		x<-rbind(x, "    for (i in 1:I1w){             # I1w = width of I1                ")
		x<-rbind(x, "        for (j in 1:I1w){                                            ")
		x<-rbind(x, "            for (k in 1:Aw){      # Aw = width of A                  ")
		x<-rbind(x, "                for (l in 1:Aw){                                     ")
		x<-rbind(x, "                     Ah2[(i-1)*Aw+k,(j-1)*Aw+l] <- I1[i,j]*A[k,l]    ")
		x<-rbind(x, "                }                                                    ")
		x<-rbind(x, "            }                                                        ")
		x<-rbind(x, "        }                                                            ")
		x<-rbind(x, "    }                                                                ")
		x<-rbind(x, "    # Ah matrix                                                      ")
		x<-rbind(x, "    Ah[1:(I1w*Aw),1:(I1w*Aw)] <- Ah1[,] + Ah2[,]                     ")
		x<-rbind(x, "                                                                     ")
		x<-rbind(x, "}                                                                    ")
		
		
		### (over)write relevant variables to environment ###
		# jags.syntax <- x
		# obj <- c( "jags.syntax" )
		# eval( parse ( text=paste0( "assign( '",obj, "' , get('",obj,"') , envir=env )" ) ) )
		
		## create return object
		ret <- list()
		# first entry: engine
		ret$engine <- engine
		# second entry: call
		
		# third entry: syntax
		ret$syntax <- x
		
		
		# return
		return( ret )
}

make.str <- function( y.name ) {
		
		# environment where objects are
		env <- parent.frame()
		
		# dimensions of structure, get from parent env
		dim.y <- eval( parse( text=paste0( "dim(", y.name, ")" ) ), envir=env )

		
		# if structure without dimensions (e.g. vector)
		if( is.null( dim.y ) ) {
				
				# long structure
				y. <- data.frame( matrix( seq( along= eval( parse( text=paste0( "", y.name, "" ) ), envir=env ) ), ncol=1 ) )
				
		} else {
		# if structure with dimensions (e.g. matrix)
		
				# long structure
				y. <- eval(parse(text=paste0( "Reduce(function(x, y) merge(x, y, by=NULL, all=TRUE),list(", paste( paste0( "1:", dim.y ), collapse="," ), "),accumulate=FALSE )" )))

		}
		
		# loop over long structure, generate strings
		make.str <- function( z ) {

				# name of parameter
				nam <- paste0( y.name, "[", paste( z, collapse=","), "]" )
	
				# determine if fixed value or freely estimated
				as.num <- suppressWarnings( as.numeric( eval( parse( text=nam ), envir=env ) ) )
				is.fixed <- !is.na( as.num )
				
				# operator: <- or ~
				op <- ifelse( is.fixed, " <- ", " ~ ")
				
				# value
				val <- ifelse( is.fixed, as.character( as.num ), eval( parse( text=paste0( y.name, ".prior", "[", paste( z, collapse=","), "]" ) ), envir=env ) )
				
				# string
				s <- paste0( "    ", nam, op, val )
				
				# return
				return( s )
		}
		s <- apply( y., 1, make.str )

		# append some white space (for optical reasons)
		s <- sapply( s, function( x ) paste0( x, paste( rep( " ", 69 - nchar(x) ), collapse="" ) ) )
		
		# as column vector
		s <- matrix( s, ncol=1 )
		
		# return
		return( s )

}
