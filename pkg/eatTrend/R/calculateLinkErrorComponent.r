
calculateLinkErrorComponent <- function(intParsT1, intParsT2, natParsT1, natParsT2) {
	
	cat("Warning! If items are clustered in units (testlets), the linkerror will be underestimated! \n")
	
	res <- list()
	anc <- intersect(intersect(intParsT1$item, intParsT2$item), intersect(natParsT1$item, natParsT2$item))
	
	L1 <- sirt::equating.rasch(natParsT1,intParsT1)
	L1dif <- L1$transf.par$TransfItempar.Gr1 - L1$transf.par$Itempar.Gr2
	names(L1dif) <- L1$transf.par$item
	# die Gesamtankeritems vorne anstellen, damit die dann bei Kovarianzberechnung genau da steehn, wo sie auch bei T2 stehen
	L1dif <- c(L1dif[match(anc,L1$transf.par$item)],L1dif[setdiff(1:length(L1dif),match(anc,L1$transf.par$item))])
	
	L2 <- sirt::equating.rasch(natParsT2,intParsT2)
	L2dif <- L2$transf.par$TransfItempar.Gr1 - L2$transf.par$Itempar.Gr2
	names(L2dif) <- L2$transf.par$item
	# analog s.o.:
	L2dif <- c(L2dif[match(anc,L2$transf.par$item)],L2dif[setdiff(1:length(L2dif),match(anc,L2$transf.par$item))])

	if(any(is.na(c(L2dif,L1dif)))){
		minb <- length(paste(L2dif,L1dif)[-grep("NA",paste(L2dif,L1dif))])
	} else {
		minb <- length(L1dif)
	}
	if(length(L2dif)!= length(L1dif)) {
		if(length(L2dif)< length(L1dif)) L2dif <- c(L2dif, rep(NA, length(L1dif)-length(L2dif)))
		if(length(L2dif)> length(L1dif)) L1dif <- c(L1dif, rep(NA, length(L2dif)-length(L1dif)))
	}
	covLi1Li2 <- (cov(L2dif,L1dif,use="pairwise.complete.obs")/minb)
	
	
	L3 <- sirt::equating.rasch(intParsT1,intParsT2)

	res[[1]] <- sqrt(L1$descriptives$linkerror^2 + L2$descriptives$linkerror^2 + L3$descriptives$linkerror^2 - 2*covLi1Li2)
	
	res[[2]] <- L3$descriptives$linkerror
	
	res[[3]] <- L1dif
	
	res[[4]] <- L2dif
	
	names(res) <- c("seL3D", "sePISA", "L1dif", "L2dif")
	
	return(res)

}