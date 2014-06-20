
# Rubin, D. B. (2003). Nested multiple imputation of NMES via partially incompatible MCMC. Statistica Neerlandica, 57(1), 3–18.

nested.pooling <- function ( d.long , Value.varname = colnames(d.long)[1] , M.varname = colnames(d.long)[2] , N.varname = colnames(d.long)[3] ) {
		
		Value.vec <- eval ( parse ( text = paste0 ( "d.long$" , Value.varname ) ) )
		M.vec <- eval ( parse ( text = paste0 ( "d.long$" , M.varname ) ) )
		N.vec <- eval ( parse ( text = paste0 ( "d.long$" , N.varname ) ) )
		
		# Standardfehler des Means
		se <- function(x) sqrt(var(x)/length(x))
		# quadriert
		se2 <- function ( x ) se(x)^2

		# Formeln aus Rubin 2003
		M <- length ( unique ( M.vec ) )
		N <- length ( unique ( N.vec ) )

		Qmn <- data.frame ( tapply ( Value.vec , list ( N.vec , M.vec ) , mean ) )
		Umn <- data.frame ( tapply ( Value.vec , list ( N.vec , M.vec ) , se2 ) )

		Q_ <- 1/(M*N) * sum ( sapply ( Qmn , mean ) )

		Qm_ <- 1/N * sapply ( Qmn , mean )

		U_ <- 1/(M*N) * sum ( sapply ( Umn , mean ) )

		MSb <- N / ( M - 1 ) * sum ( (Qm_ - Q_)^2 )

		MSw <- 1 / ( M * ( N - 1 ) ) * sum ( mapply ( function ( x , y ) sum ( (x-y)^2 ) , Qmn , Qm_ ) )

		T <- U_  +  1/N * ( 1 + 1/M ) * MSb  +  ( 1 - 1/N ) * MSw
		
		Q_se <- sqrt ( T )
		
		t <- 1 / sqrt ( T )
		
		df <- 1 / (      (1/N * ( 1 + 1/M ) * MSb  / T)^2 * 1 / ( M - 1 )  +  (( 1 - 1/N ) * MSw / T)^2 * 1 / ( M*(N-1) )    )
		
		p.oneside <- pt( q=t, df=df, lower.tail = FALSE, log.p = FALSE)
		
		d <- data.frame ( "parameter" = c("M","N","Q_","T","Q_se","t","df","p.oneside"), "value" = c(M,N,Q_,T,Q_se,t,df,p.oneside) , stringsAsFactors = FALSE )
		
		return ( d )
}
