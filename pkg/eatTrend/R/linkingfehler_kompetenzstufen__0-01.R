
seKompstuf <- function(resCuts, cutScores, M1 , SD1 , M2 , SD2 , linkerror  ){

	resCuts1 <- resCuts[-which(is.na(resCuts[,1])),]

	for(i in 1:dim(resCuts1)[1]) {
		# Anteil Studie 1
		p1 <- resCuts1$estT1[i]
		# Anteil Studie 2
		p2 <- resCuts1$estT2[i]
		# Kompetenzstufenverteilungsdifferenz
		delta <- p2 - p1 
		# Varianz von delta
		komp <- NULL
		komp[1] <- cutScores[resCuts1$parameter[i]]
		if(which(cutScores %in% cutScores[resCuts1$parameter[i]]) =="1") {
			komp[2] <- 10000 } else {
			komp[2] <- unname(cutScores[which(cutScores %in% cutScores[resCuts1$parameter[i]])-1])
		}
		a1 <- sum( dnorm( ( komp - M1 ) / SD1 ) * c(-1,1) / SD1 ) 
		a2 <- sum( dnorm( ( komp - M2 ) / SD2 ) * c(-1,1) / SD2 ) 
		var_delta <- (  a1^2 + a2^2 ) * linkerror^2 / 2  
		# Linkfehler = sqrt( var_delta )
		resCuts1$seTrend[i] <- sqrt(resCuts1$seT1[i]^2+resCuts1$seT2[i]^2+ var_delta ) 
  
	}
	
	return( resCuts1 )
           
	}
