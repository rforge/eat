
### References
# Rubin, D. B. (2003). Nested multiple imputation of NMES via partially incompatible MCMC. Statistica Neerlandica, 57(1), 3–18.
# Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. New York: Wiley.

### Doku
# d.long ... data.frame in long format
# Value.varname ... name of column that values are stored in
# N.varname ... name of column that identifies the imputation
# M.varname ... name of column that identifies the nest (in case of nested imputation); if NULL nonnested pooling is applied

nested.pooling <- function ( d.long , Value.varname = colnames(d.long)[1] , N.varname = colnames(d.long)[2] , M.varname = NULL , FUN = mean , SE = function(x) sqrt(var(x)/length(x)) ) {

		# wenn Datensatz 3 Spalten hat, dann M.varname als die übrig gebliebene defaulten, wenn M.varname NULL ist
		if ( ncol ( d.long ) %in% 3 & is.null ( M.varname ) ) {
				M.varname <- colnames ( d.long ) [ ! colnames ( d.long ) %in% c ( Value.varname , N.varname ) ]
		}
		
		# genestetes oder nicht genestetes Pooling
		if ( is.null ( M.varname ) | length ( unique ( d.long[,M.varname] ) ) %in% 1 ) {
				nestedPooling <- FALSE
		} else {
				nestedPooling <- TRUE
		}

		# SE function quadrieren, damit Varianz
		SE2 <- function ( x ) SE(x)^2
		
		# Value-Vektor aus Datensatz extrahieren
		Value.vec <- eval ( parse ( text = paste0 ( "d.long$" , Value.varname ) ) )
		
		### genestetes Pooling
		if ( nestedPooling ) {
	
				# Vektoren aus Datensatz extrahieren
				M.vec <- eval ( parse ( text = paste0 ( "d.long$" , M.varname ) ) )
				N.vec <- eval ( parse ( text = paste0 ( "d.long$" , N.varname ) ) )

				# Formeln aus Rubin 2003
				M <- length ( unique ( M.vec ) )
				N <- length ( unique ( N.vec ) )

				Qmn <- data.frame ( tapply ( Value.vec , list ( N.vec , M.vec ) , FUN ) )
				Umn <- data.frame ( tapply ( Value.vec , list ( N.vec , M.vec ) , SE2 ) )

				Q_ <- 1/(M*N) * sum ( sapply ( Qmn , sum ) )

				Qm_ <- 1/N * sapply ( Qmn , sum )

				U_ <- 1/(M*N) * sum ( sapply ( Umn , sum ) )

				MSb <- N / ( M - 1 ) * sum ( (Qm_ - Q_)^2 )

				MSw <- 1 / ( M * ( N - 1 ) ) * sum ( mapply ( function ( x , y ) sum ( (x-y)^2 ) , Qmn , Qm_ ) )

				T <- U_  +  1/N * ( 1 + 1/M ) * MSb  +  ( 1 - 1/N ) * MSw
				
				Q_se <- sqrt ( T )
				
				t <- Q_ / Q_se
				
				df <- 1 / (      (1/N * ( 1 + 1/M ) * MSb  / T)^2 * 1 / ( M - 1 )  +  (( 1 - 1/N ) * MSw / T)^2 * 1 / ( M*(N-1) )    )
				
				p <- 2*pt( q=t, df=df, lower.tail = FALSE, log.p = FALSE)
		
				d <- data.frame ( "parameter" = c("Q_","t","df","p","M","N","U_","MSb","MSw","T","Q_se"), "value" = c(Q_,t,df,p,M,N,U_,MSb,MSw,T,Q_se) , stringsAsFactors = FALSE )

		} else {
				
				# Vektor aus Datensatz extrahieren
				N.vec <- eval ( parse ( text = paste0 ( "d.long$" , N.varname ) ) )
				
				# Formeln aus Rubin 1987
				# laut http://sites.stat.psu.edu/~jls/mifaq.html
				N <- length ( unique ( N.vec ) )
				
				Qn <- tapply ( Value.vec , list ( N.vec ) , FUN )
				Un <- tapply ( Value.vec , list ( N.vec ) , SE2 )
				
				Q_ <- mean ( Qn )
				U_ <- mean ( Un )
				
				B <- ( 1 / ( N - 1 ) ) * sum ( (Qn - Q_)^2 )
				
				T <- U_ + ( 1 + 1 / N ) * B
				
				Q_se <- sqrt ( T )
				
				t <- Q_ / Q_se
				
				df <- ( N - 1 ) * ( 1 + ( N * U_ ) / ( ( N + 1 ) * B ) )^2
				
				p <- 2*pt( q=t, df=df, lower.tail = FALSE, log.p = FALSE)
			
				d <- data.frame ( "parameter" = c("Q_","t","df","p","N","U_","B","T","Q_se"), "value" = c(Q_,t,df,p,N,U_,B,T,Q_se) , stringsAsFactors = FALSE )
				
		}
		
		# Funktionsnamen noch ran
		# make.name <- function ( FUNname ) {
				# if ( class ( FUNname ) %in% "name" ) {
						# FUNname <- as.character ( FUNname )
				# } else if ( class ( FUNname ) %in% "call" ) {
						# FUNname <- as.character ( FUNname )[3]
				# } else {
						# FUNname <- ""
				# }
				# return ( FUNname )
		# }
		# FUNname <- make.name ( substitute(FUN) )
		# SEname <- make.name ( substitute(SE) )
		# d <- rbind ( d , data.frame ( "parameter" = c ( "FUN" , "SE" ) , "value" = c ( FUNname , SEname ) , stringsAsFactors = FALSE ) )
		
		return ( d )
}
