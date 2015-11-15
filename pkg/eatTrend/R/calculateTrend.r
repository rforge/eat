calcTrend <- function(intParsT1, intParsT2, natParsT1, natParsT2, PVnatT1, PVnatT2) {

	# Achtung, alles auf 500er-Metrik einspeisen!
	
	res <- list()
	
	Link1 <- sirt:::equating.rasch(natParsT1,intParsT1)
	#Link1SL <- Link1$B.est[3]
	Link1MM <- Link1$B.est[1]
	
	Link2 <- sirt:::equating.rasch(natParsT2,intParsT2)	
	#Link2SL <- Link2$B.est[3]
	Link2MM <- Link2$B.est[1]
	
	Link3 <- sirt:::equating.rasch(intParsT1,intParsT2)
	#Link3SL <- Link3$B.est[3]
	Link3MM <- Link3$B.est[1]
	
	#(tE["trendSL"] <- (mPVnatT2 - Link3SL + Link2SL) - (mPVnatT1 + Link1SL)) 
	#(tE["trendMM"] <- (mPVnatT2 - Link3MM + Link2MM) - (mPVnatT1 + Link1MM)) 

	res[["T1"]] <- PVnatT1
	res[["T1"]][,-1] <- (apply(PVnatT1[,-1],2, function(u) "+"(u,as.numeric(unname(Link1MM)))))
	
	res[["T2"]] <- PVnatT2
	res[["T2"]][,-1] <- data.frame(apply((apply(PVnatT2[,-1],2, function(u) "-"(u,as.numeric(unname(Link3MM))))),2, function(uu) "+"(uu,as.numeric(unname(Link2MM)))))

	return(res)
}