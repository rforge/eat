
transformTo500 <- function(pars, mRefPop=NULL, sdRefPop=NULL, mtT=500, sdtT=100, wgts=NULL, type=c("itPar", "persPar"), cutScores=NULL) {

	wgts <-as.numeric(wgts)
	res <- pars
	
	if(type == "itPar") {
	
	if(is.null(mRefPop) | is.null(sdRefPop)) {stop("if type is itPar, mRefPop and sdRefPop have to be specified")}
	
	res[,2] <- ((pars[,2]+ log(0.625/0.375)-mRefPop)/sdRefPop)*sdtT+mtT
	
	if(!is.null(cutScores)) res[,3] <- addCuts(res[,2], cutScores)
	
	} else {
		
		if(type == "persPar") {
			dp <- dim(pars)[2]
			if(!is.null(mRefPop) & !is.null(sdRefPop)) {
				for(i in 2:dp) {
					res[,i] <- ((pars[,i]-mRefPop)/sdRefPop)*sdtT+mtT
				}
			} else {
				if(is.null(wgts)) {
					for(i in 2:dp) {
						res[,i] <- ((pars[,i]-mean(pars[,i],na.rm=TRUE))/sd(pars[,i],na.rm=TRUE))*sdtT+mtT	
					}
				} else {
					for(i in 2:dp) {
						mRefPop <- SDMTools::wt.mean(pars[,i],wgts)
						sdRefPop <- SDMTools::wt.sd(pars[,i],wgts)
						res[,i] <- ((pars[,i]-mRefPop)/sdRefPop)*sdtT+mtT
					}
				}
			}
			
			if(!is.null(cutScores)) {
				for(i in 2:dp) {
					res[,(dp+i-1)] <- addCuts(res[,i], cutScores)
				}
			}
	
		} else { stop("type not specified") }

	}
	return(res)	
}

addCuts <- function(valVec, cutVec) {

	if(is.null(names(cutVec))) {
		nam <- length(cutVec):1
	} else {nam <- names(cutVec)}

	a <- NULL
	for(i in seq(along = cutVec)) {
		a <- c(a, paste0("ifelse(valVec >= ", cutVec[i], ", \"", nam[i], "\","))
	}
	b <- paste0(rep(")", length(cutVec)),collapse="")
	a <- paste0(a, collapse="")

	ep <- parse(text = paste0(a, "\"ERR\"", b))
	res <- eval(ep)

	# if(any(res == "ERR")) {
		# cat(paste("Warning! No cutscore defined for parameter value",  valVec[which(res == "ERR")] ,"in line:", which(res == "ERR"), "\n"))
	# }

	return(res)
}
