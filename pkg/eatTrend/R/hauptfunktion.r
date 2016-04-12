
# evtl.CDIFplot noch 
eatTrend <- function(itParsIntT1, PVsT1, countriesT1, 
itParsNatT1=NULL, jkzoneT1=NULL, jkrepT1=NULL, weightsT1=NULL, itParsIntT2, PVsT2, 
countriesT2, itParsNatT2=NULL, weightsT2=NULL, jkzoneT2=NULL, jkrepT2=NULL, testletNam=NULL,
transfTo500=TRUE, mtT=500, sdtT=100, mRefPop=NULL, sdRefPop=NULL, cutScores=NULL, type =c("FCIP", "MM"), writeCsv=FALSE, path=NULL, plots=FALSE, backwards=FALSE) {

	cat ( paste ("Hi! ", Sys.time(), "\n" ) ) 
	if(backwards) {
		if(type=="MM") {
			stop("'backwards' is not yet implemented for type=MM \n")
		} else {
			cat("'backwards' is beta. please be cautious \n")
		}
	}
	stopifnot(class(itParsIntT1) == "data.frame")
	stopifnot(class(PVsT1) == "data.frame")
	stopifnot(class(itParsIntT2) == "data.frame")
	stopifnot(class(PVsT2) == "data.frame")
	stopifnot(dim(PVsT1)[1] == length(countriesT1))
	stopifnot(dim(PVsT2)[1] == length(countriesT2))
	# die anderen Argumente werden erst dann durchgecheckt, wenn sie benutzt werden
	
	if(is.null(jkzoneT1)) {cat("Warning! jkzoneT1 is empty and will be defaulted to nonsense values \n"); jkzoneT1 <- paste0(countriesT1, sample(c(0,1,2,3), length(countriesT1), replace = TRUE))}
	if(is.null(jkzoneT2)) {cat("Warning! jkzoneT2 is empty and will be defaulted to nonsense values \n"); jkzoneT2 <- paste0(countriesT2, sample(c(0,1,2,3), length(countriesT2), replace = TRUE))}
	if(is.null(jkrepT1)) {cat("Warning! jkrepT1 is empty and will be defaulted to nonsense values \n"); jkrepT1 <- rbinom(length(countriesT1),1,.5)}
	if(is.null(jkrepT2)) {cat("Warning! jkrepT2 is empty and will be defaulted to nonsense values \n"); jkrepT2 <- rbinom(length(countriesT2),1,.5)} 
	
	
	if(writeCsv & is.null(path)) {stop("please specify path if 'writeCsv=TRUE'")}
	if(plots & is.null(path)) {stop("please specify path if 'plots=TRUE'")}
	
	if(!identical(dir(path), character(0))) {
	   cat(paste("Data already exists in: ", path, ", which might be overwritten. Press 'Esc' to cancel.\n", sep =""))
	   temp <- mapply ( function ( nr ) {
			cat ( paste ( nr , " " , sep="" ) )
			flush.console ( )
			Sys.sleep ( 1 )
		} , 5:1 )
		cat("\n")
	}
	
	# Pfad checken & anlegen
	if(inherits(ret <- try(dir.create(path, showWarnings = FALSE, recursive=TRUE)),"try-error")) {
		stop("Error while creating folder.")
	}
		
	# Schreibrechte vorhanden?
	if(unname ( file.access(path, mode = 2 ) )  == 0 ) {
	   ret <- TRUE
	   } else {
		stop(paste("No writing access", path))
	}

	itParsIntT1 <- eatPrep:::set.col.type(itParsIntT1, list(character=names(itParsIntT1)[1], numeric=names(itParsIntT1)[2:dim(itParsIntT1)[2]]))
	itParsIntT2 <- eatPrep:::set.col.type(itParsIntT2, list(character=names(itParsIntT2)[1], numeric=names(itParsIntT2)[2:dim(itParsIntT2)[2]]))
	PVsT1 <- eatPrep:::set.col.type(PVsT1, list(character=names(PVsT1)[1], numeric=names(PVsT1)[2:dim(PVsT1)[2]]))
	PVsT2 <- eatPrep:::set.col.type(PVsT2, list(character=names(PVsT2)[1], numeric=names(PVsT2)[2:dim(PVsT2)[2]]))
	names(PVsT1)[1] <- names(PVsT2)[1] <-"idstud"
	
	if(type == "MM") {

		cat("Warning! If items are clustered in units (testlets), the linkerror will be underestimated! Adjustment for type='MM' is not yet implemented. \n")
	
		stopifnot(class(itParsNatT1) == "data.frame" || class(itParsNatT1) == "list")
		stopifnot(class(itParsNatT2) == "data.frame" || class(itParsNatT2) == "list")
		
		#if(is.list(itParsNatT1)) stopifnot(any(names(itParsNatT1) %in% countriesT1))
		#if(is.list(itParsNatT2)) stopifnot(any(names(itParsNatT2) %in% countriesT2))
		if(class(itParsNatT1) == "data.frame") {itParsNatT1 <- list(itParsNatT1)}
		if(class(itParsNatT2) == "data.frame") {itParsNatT2 <- list(itParsNatT2)}
		
		# Automatischer Abgleich von Laendern noch nicht implementiert, dafuer muss der Nutzer erstmal Sorge tragen
		stopifnot(length(itParsNatT1) == length(itParsNatT2))
		
		itParsNatT1 <- lapply(itParsNatT1, function(ii) eatPrep:::set.col.type(ii, list(character=names(ii)[1], numeric=names(ii)[2:dim(ii)[2]])))
		itParsNatT2 <- lapply(itParsNatT2, function(ii) eatPrep:::set.col.type(ii, list(character=names(ii)[1], numeric=names(ii)[2:dim(ii)[2]])))
		
		#mPVnatT1 <- tapply((apply(PVsT1[,-1],1,mean,na.rm=TRUE)), countriesT1, mean, na.rm=TRUE)
		#mPVnatT2 <- tapply((apply(PVsT2[,-1],1,mean,na.rm=TRUE)), countriesT2, mean, na.rm=TRUE)
		# Gesamt-PV-Trafo d'abord
		#res <- mapply(calcTrend, natParsT1=itParsNatT1, natParsT2=itParsNatT2, mPVnatT1=mPVnatT1, mPVnatT2=mPVnatT2, MoreArgs = list(intParsT1=itParsIntT1, intParsT2=itParsIntT2))
		
		PVnatT1 <- lapply(unique(countriesT1), function(kk) PVsT1[countriesT1 == kk,])
		names(PVnatT1) <- unique(countriesT1)
		PVnatT2 <- lapply(unique(countriesT2), function(kk) PVsT2[countriesT2 == kk,])
		names(PVnatT2) <- unique(countriesT2)

		res <- mapply(calcTrend, natParsT1=itParsNatT1, natParsT2=itParsNatT2, PVnatT1=PVnatT1, PVnatT2=PVnatT2, MoreArgs = list(intParsT1=itParsIntT1, intParsT2=itParsIntT2),SIMPLIFY=FALSE)
		
		seres <- mapply(calculateLinkErrorComponent, natParsT1=itParsNatT1, natParsT2=itParsNatT2, MoreArgs = list(intParsT1=itParsIntT1, intParsT2=itParsIntT2),SIMPLIFY=FALSE)
		
		PV500T1 <- eatPrep:::mergeData("idstud", lapply(res, function(qd) qd[[1]]))
		PV500T2 <- eatPrep:::mergeData("idstud", lapply(res, function(qd) qd[[2]]))
		
		if(transfTo500) {
			
			PV500T1 <- transformTo500(pars=PV500T1, mtT=mtT, sdtT=sdtT, wgts=weightsT1, type="persPar", cutScores=cutScores)
			PV500T2 <- transformTo500(pars=PVsT2, mtT=mtT, sdtT=sdtT, mRefPop=mRefPop, sdRefPop=sdRefPop, type="persPar", cutScores=cutScores)
					
		} 
		
	} else {
		if(type == "FCIP") {

			Link3 <- sirt:::equating.rasch(itParsIntT1,itParsIntT2)
			
			if(is.null(testletNam)) {
				cat("Warning! If items are clustered in units (testlets), the linkerror will be underestimated unless argument 'testletNam' is specified! \n")
				flush.console ( )
				seres <- Link3$descriptives$linkerror
			} else {
				stopifnot(class(testletNam) == "character")
				testletNam <- unique(testletNam)
				names(itParsIntT2)[2] <- "b.2"
				pd <- mergeData("item", list(itParsIntT1, itParsIntT2))
				tl1 <- lapply(testletNam, function(xx) grep(xx, pd$item))
				names(tl1) <- testletNam
				pd$testlet <- unname(unlist(sapply(unlist(tl1), function(bb) testletNam[unlist(as.logical(lapply(tl1, function(uu) sum(grepl(paste0("^",bb,"$"), uu)))))])))
				if(any(is.na(pd$testlet))) {
					cat("Please specify all testlets/item units in testletNam.\n")
					pd$testlet[is.na(pd$testlet)] <- pd$item[is.na(pd$testlet)]
				}
				pd <- reinsort.col(pd, c("b", "b.2", "item"), "testlet")
				pd <- reinsort.col(pd, "item", "b.2")		
				Link3b <- sirt:::equating.rasch.jackknife(pars.data=pd, display = TRUE,
				se.linkerror = FALSE, alpha1 = 0, alpha2 = 0)
				seres <- Link3b$descriptives$linkerror
			}
			
			Link3MM <- Link3$B.est[1]
			
		
			PV500T1 <- PVsT1
			PV500T2 <- PVsT2
			PV500T2[,-1] <- apply(PV500T2[,-1],2,function(u) "-"(u,as.numeric(unname(Link3MM)))) 

			if(transfTo500) {

				if(backwards) {
				datBB <- subset(PVsT2, substr(PVsT2[,1],1,2) == "19" | substr(PVsT2[,1],1,2) == "59")
					mPop2 <- mean(apply(PVsT2[,-1],2,SDMTools:::wt.mean,as.numeric(weightsT2)))
					sdPop2 <- mean(apply(PVsT2[,-1],2,SDMTools:::wt.sd,as.numeric(weightsT2)))
					logitCutsT2 <- (((cutScores-mtT)/sdtT)*sdPop2)+mPop2
					cutScores <- ((logitCutsT2-as.numeric(unname(Link3MM))-mRefPop)/sdRefPop)*sdtT+mtT					
				}
			
				PV500T1 <- transformTo500(pars=PV500T1, mtT=mtT, sdtT=sdtT, wgts=weightsT1, type="persPar", cutScores=cutScores)
				PV500T2 <- transformTo500(pars=PV500T2, mtT=mtT, sdtT=sdtT, mRefPop=mRefPop, sdRefPop=sdRefPop, type="persPar", cutScores=cutScores)
	
			}
				
		} else {
			stop("'type' has to be specified")
		}
	}
	
	#T1
	PV500T1 <- eatPrep:::mergeData("idstud", list(data.frame(idstud=PVsT1[,1], countriesT1, jkzoneT1, jkrepT1, weightsT1, stringsAsFactors=FALSE),PV500T1))
	dp2 <- dim(data.frame(idstud=PVsT1[,1], countriesT1, jkzoneT1, jkrepT1, weightsT1, stringsAsFactors=FALSE))[2]
	if(!is.null(cutScores)) {dp3 <- (dim(PV500T1)[2] - dp2)/2} else {dp3 <- (dim(PV500T1)[2] - dp2)}
	PV500T1m <- reshape2:::melt(PV500T1[,c(1:dp2,(dp2+1):(dp2+dp3))], id=1:dp2)
	if(!is.null(weightsT1)) {
		PV500T1m <- eatPrep:::set.col.type(PV500T1m, list(character = "variable", numeric = c("weightsT1", "value")))
		if(any(is.na(c(PV500T1m$jkzoneT1,PV500T1m$jkrepT1,PV500T1m$weightsT1)))) {
			PV500T1m <- PV500T1m[-c(which(is.na(PV500T1m$jkzoneT1)|is.na(PV500T1m$jkrepT1)|is.na(PV500T1m$weightsT1))),]
		}	
		meansT1  <- eatRep:::jk2.mean(datL = PV500T1m, ID="idstud", wgt="weightsT1", type = "JK2", 
                PSU = "jkzoneT1", repInd = "jkrepT1", imp="variable", groups = "countriesT1", 
                dependent = "value", na.rm=FALSE, doCheck=TRUE)
	} else {
		PV500T1m <- eatPrep:::set.col.type(PV500T1m, list(character = "variable", numeric = "value"))
		if(any(is.na(c(PV500T1m$jkzoneT1,PV500T1m$jkrepT1)))) {
			PV500T1m <- PV500T1m[-c(which(is.na(PV500T1m$jkzoneT1)|is.na(PV500T1m$jkrepT1))),]
		}
		meansT1  <- eatRep:::jk2.mean(datL = PV500T1m, ID="idstud", type = "JK2", 
                PSU = "jkzoneT1", repInd = "jkrepT1", imp="variable", groups = "countriesT1", 
                dependent = "value", na.rm=FALSE, doCheck=TRUE)
	}
	if(!is.null(cutScores)) {
		PV500T1c <- reshape2:::melt(PV500T1[,c(1:dp2,(dp2+dp3+1):(dp2+dp3+dp3))], id=1:dp2)
		if(!is.null(weightsT1)) {
			PV500T1c <- eatPrep:::set.col.type(PV500T1c, list(character = "variable", numeric = "weightsT1"))
			if(any(is.na(c(PV500T1c$jkzoneT1,PV500T1c$jkrepT1,PV500T1c$weightsT1)))) {
				PV500T1c <- PV500T1c[-c(which(is.na(PV500T1c$jkzoneT1)|is.na(PV500T1c$jkrepT1)|is.na(PV500T1c$weightsT1))),]
			}
			cutsT1  <- eatRep:::jk2.table(datL = PV500T1c, ID="idstud", wgt="weightsT1", type = "JK2", 
					PSU = "jkzoneT1", repInd = "jkrepT1", imp="variable", groups = "countriesT1", 
					dependent = "value", na.rm=FALSE, doCheck=TRUE)
		} else {
			PV500T1c <- eatPrep:::set.col.type(PV500T1c, list(character = "variable"))
			if(any(is.na(c(PV500T1c$jkzoneT1,PV500T1c$jkrepT1)))) {
				PV500T1c <- PV500T1c[-c(which(is.na(PV500T1c$jkzoneT1)|is.na(PV500T1c$jkrepT1))),]
			}
			cutsT1  <- eatRep:::jk2.table(datL = PV500T1c, ID="idstud", type = "JK2", 
					PSU = "jkzoneT1", repInd = "jkrepT1", imp="variable", groups = "countriesT1", 
					dependent = "value", na.rm=FALSE, doCheck=TRUE)
		}
		resCutsT1 <- reshape2:::dcast(cutsT1[,-c(1:3)], parameter+countriesT1 ~ coefficient,margins="value")
	}
	resMeanT1 <- reshape2:::dcast(subset(meansT1[,-c(1:3)], meansT1$parameter == "mean"), parameter+countriesT1 ~ coefficient,margins="value")
	
	#T2
	PV500T2 <- eatPrep:::mergeData("idstud", list(data.frame(idstud=PVsT2[,1], countriesT2, jkzoneT2, jkrepT2, weightsT2, stringsAsFactors=FALSE),PV500T2))
	dp2 <- dim(data.frame(idstud=PVsT2[,1], countriesT2, jkzoneT2, jkrepT2, weightsT2, stringsAsFactors=FALSE))[2]
	if(!is.null(cutScores)) {dp3 <- (dim(PV500T2)[2] - dp2)/2} else {dp3 <- (dim(PV500T2)[2] - dp2)}
	PV500T2m <- reshape2:::melt(PV500T2[,c(1:dp2,(dp2+1):(dp2+dp3))], id=1:dp2)
	if(!is.null(weightsT2)) {
		PV500T2m <- eatPrep:::set.col.type(PV500T2m, list(character = "variable", numeric = c("weightsT2", "value")))
		if(any(is.na(c(PV500T2m$jkzoneT2,PV500T2m$jkrepT2,PV500T2m$weightsT2)))) {
			PV500T2m <- PV500T2m[-c(which(is.na(PV500T2m$jkzoneT2)|is.na(PV500T2m$jkrepT2)|is.na(PV500T2m$weightsT2))),]
		}
		meansT2  <- eatRep:::jk2.mean(datL = PV500T2m, ID="idstud", wgt="weightsT2", type = "JK2", 
                PSU = "jkzoneT2", repInd = "jkrepT2", imp="variable", groups = "countriesT2", 
                dependent = "value", na.rm=FALSE, doCheck=TRUE)
	} else {
		PV500T2m <- eatPrep:::set.col.type(PV500T2m, list(character = "variable", numeric = "value"))
		if(any(is.na(c(PV500T2m$jkzoneT2,PV500T2m$jkrepT2)))) {
			PV500T2m <- PV500T2m[-c(which(is.na(PV500T2m$jkzoneT2)|is.na(PV500T2m$jkrepT2))),]
		}
		meansT2  <- eatRep:::jk2.mean(datL = PV500T2m, ID="idstud", type = "JK2", 
                PSU = "jkzoneT2", repInd = "jkrepT2", imp="variable", groups = "countriesT2", 
                dependent = "value", na.rm=FALSE, doCheck=TRUE)
	}
	if(!is.null(cutScores)) {
		PV500T2c <- reshape2:::melt(PV500T2[,c(1:dp2,(dp2+dp3+1):(dp2+dp3+dp3))], id=1:dp2)
		if(!is.null(weightsT2)) {
			PV500T2c <- eatPrep:::set.col.type(PV500T2c, list(character = "variable", numeric = "weightsT2"))
			if(any(is.na(c(PV500T2c$jkzoneT2,PV500T2c$jkrepT2,PV500T2c$weightsT2)))) {
				PV500T2c <- PV500T2c[-c(which(is.na(PV500T2c$jkzoneT2)|is.na(PV500T2c$jkrepT2)|is.na(PV500T2c$weightsT2))),]
			}
			cutsT2  <- eatRep:::jk2.table(datL = PV500T2c, ID="idstud", wgt="weightsT2", type = "JK2", 
					PSU = "jkzoneT2", repInd = "jkrepT2", imp="variable", groups = "countriesT2", 
					dependent = "value", na.rm=FALSE, doCheck=TRUE)
		} else {
			PV500T2c <- eatPrep:::set.col.type(PV500T2c, list(character = "variable"))
			if(any(is.na(c(PV500T2c$jkzoneT2,PV500T2c$jkrepT2)))) {
				PV500T2c <- PV500T2c[-c(which(is.na(PV500T2c$jkzoneT2)|is.na(PV500T2c$jkrepT2))),]
			}
			cutsT2  <- eatRep:::jk2.table(datL = PV500T2c, ID="idstud", type = "JK2", 
					PSU = "jkzoneT2", repInd = "jkrepT2", imp="variable", groups = "countriesT2", 
					dependent = "value", na.rm=FALSE, doCheck=TRUE)
		}
		resCutsT2 <- reshape2:::dcast(cutsT2[,-c(1:3)], parameter+countriesT2 ~ coefficient,margins="value")
	}
	resMeanT2 <- reshape2:::dcast(subset(meansT2[,-c(1:3)], meansT2$parameter == "mean"), parameter+countriesT2 ~ coefficient,margins="value")
	
	names(resMeanT1) <- c("x", "country", "meanT1", "seT1")
	names(resMeanT2) <- c("x", "country", "meanT2", "seT2")
	
	resMeans <- eatPrep:::mergeData("country", list(resMeanT1[,-1], resMeanT2[,-1]))
	resMeans$meanTrend <- resMeans$meanT2 - resMeans$meanT1
	if(type == "MM") {
		l3d <- data.frame(country=names(unlist(lapply(seres,function(tt) tt[[1]]))),seTrendL3D=unlist(lapply(seres,function(tt) tt[[1]])))
		pisa <- data.frame(country=names(unlist(lapply(seres,function(tt) tt[[2]]))),seTrendpisa=unlist(lapply(seres,function(tt) tt[[2]])))
		resMeans <- eatPrep:::mergeData("country", list(resMeans, l3d, pisa))
		resMeans$seTrendL3D <- sqrt(resMeans$seT1^2+resMeans$seT2^2+((resMeans$seTrendL3D/sdRefPop)*100)^2)
		resMeans$seTrendpisa <- sqrt(resMeans$seT1^2+resMeans$seT2^2+((resMeans$seTrendpisa/sdRefPop)*100)^2)
	
		if(plots) {
			pdf(file =paste0(path, "/CountryDIF_", Sys.Date(), ".pdf"))
				for(uu in names(seres)) {
					vv <- round(cor(seres[[uu]][[3]],seres[[uu]][[4]],use="pairwise.complete.obs"),2)
					
					plot(seres[[uu]][[4]][1:50], type="l", lty=3, main = paste0("Laender-DIF LV 2009 vs. 2015: ", uu, " (Teil 1), r_ges = ", vv), xlab="Item", ylab="DIF", ylim=c(-1.4,1.4),xaxt = "n")
					abline(h=0, col="grey50")
					abline(v=1:length(seres[[uu]][[3]][1:50]), col="grey80")
					lines(seres[[uu]][[3]][1:50], col="gray30",  type="l")
					lines(seres[[uu]][[4]][1:50], type="l", lty=3)
					axis(1, at=1:length(seres[[uu]][[3]][1:50]), labels=names(seres[[uu]][[3]][1:50]), las=3)#, cex.axis=0.5)
					legend("topleft", lty=c(3,1), col=c("black","gray30"), c("2015", "2009"), lwd=1, cex=1)
					
					if(length(seres[[uu]][[4]])>51) {
						plot(seres[[uu]][[4]][51:100], type="l", lty=3, main = paste0("Laender-DIF LV 2009 vs. 2015: ", uu, " (Teil 2), r_ges = ", vv), xlab="Item", ylab="DIF", ylim=c(-1.4,1.4),xaxt = "n")
						abline(h=0, col="grey50")
						abline(v=1:length(seres[[uu]][[3]][51:100]), col="grey80")
						lines(seres[[uu]][[3]][51:100], col="gray30",  type="l")
						lines(seres[[uu]][[4]][51:100], type="l", lty=3)
						axis(1, at=1:length(seres[[uu]][[3]][51:100]), labels=names(seres[[uu]][[3]][51:100]), las=3)#, cex.axis=0.5)
						legend("topleft", lty=c(3,1), col=c("black","gray30"), c("2015", "2009"), lwd=1, cex=1)
					}
					if(length(seres[[uu]][[4]])>101) {
						plot(seres[[uu]][[4]][101:150], type="l", lty=3, main = paste0("Laender-DIF LV 2009 vs. 2015: ", uu, " (Teil 3), r_ges = ", vv), xlab="Item", ylab="DIF", ylim=c(-1.4,1.4),xaxt = "n")
						abline(h=0, col="grey50")
						abline(v=1:length(seres[[uu]][[3]][101:150]), col="grey80")
						lines(seres[[uu]][[3]][101:150], col="gray30",  type="l")
						lines(seres[[uu]][[4]][101:150], type="l", lty=3)
						axis(1, at=1:length(seres[[uu]][[3]][101:150]), labels=names(seres[[uu]][[3]][101:150]), las=3)#, cex.axis=0.5)
						legend("topleft", lty=c(3,1), col=c("black","gray30"), c("2015", "2009"), lwd=1, cex=1)
					}	
					if(length(seres[[uu]][[4]])>151) {					
						plot(seres[[uu]][[4]][151:length(seres[[uu]][[3]])], type="l", lty=3, main = paste0("Laender-DIF LV 2009 vs. 2015: ", uu, " (Teil 4), r_ges = ", vv), xlab="Item", ylab="DIF", ylim=c(-1.4,1.4),xaxt = "n")
						abline(h=0, col="grey50")
						abline(v=1:length(seres[[uu]][[3]][151:length(seres[[uu]][[3]])]), col="grey80")
						lines(seres[[uu]][[3]][151:length(seres[[uu]][[3]])], col="gray30",  type="l")
						lines(seres[[uu]][[4]][151:length(seres[[uu]][[3]])], type="l", lty=3)
						axis(1, at=1:length(seres[[uu]][[3]][151:length(seres[[uu]][[3]])]), labels=names(seres[[uu]][[3]][151:length(seres[[uu]][[3]])]), las=3)#, cex.axis=0.5)
						legend("topleft", lty=c(3,1), col=c("black","gray30"), c("2015", "2009"), lwd=1, cex=1)
					}
				}
			dev.off()					
		}
		
	}	else {
	
		seres <- data.frame(country=unique(countriesT2), seTrendpisa=seres, stringsAsFactors=FALSE)
		resMeans <- eatPrep:::mergeData("country", list(resMeans, seres))
		resMeans$seTrendpisa <- sqrt(resMeans$seT1^2+resMeans$seT2^2+((resMeans$seTrendpisa/sdRefPop)*100)^2)
	}
	resCutsT2$id <- paste(resCutsT2$parameter, resCutsT2$countriesT2)
	names(resCutsT2)[3:4] <- c("estT2", "seT2")
	resCutsT1$id <- paste(resCutsT1$parameter, resCutsT1$countriesT1)
	names(resCutsT1)[3:4] <- c("estT1", "seT1")
	resCuts <- eatPrep:::mergeData("id", list(resCutsT1, resCutsT2))
	resCuts <- resCuts[,-c(2,6)]
	names(resCuts)[2] <- "country"
	resCuts <- resCuts[,c(2,1,3,4,5,6)]
	resCuts <- resCuts[order(resCuts$country),]
	resCuts$estTrend <- resCuts$estT2 - resCuts$estT1
	resCuts$seTrendUnderestimated <- sqrt(resCuts$seT2^2 + resCuts$seT1^2)
	linkerror <- mean(sqrt(resMeans$seTrendpisa^2-(resMeans$seT1^2+resMeans$seT2^2)), na.rm=TRUE)
	M2 <- SD2 <- NULL	
	for(i in 6:max(which(unlist(lapply(PV500T2,class))=="numeric"))) {
		M2 <- c(M2, SDMTools:::wt.mean(PV500T2[,i],as.numeric(PV500T2$weightsT2)))
		SD2 <- c(SD2, SDMTools:::wt.sd(PV500T2[,i],as.numeric(PV500T2$weightsT2)))
	}
	M2 <- mean(M2)
	SD2 <- mean(SD2)
	M1 <- SD1 <- NULL	
	for(i in 6:max(which(unlist(lapply(PV500T1,class))=="numeric"))) {
		M1 <- c(M1, SDMTools:::wt.mean(PV500T1[,i],as.numeric(PV500T1$weightsT1)))
		SD1 <- c(SD1, SDMTools:::wt.sd(PV500T1[,i],as.numeric(PV500T1$weightsT1)))
	}
	M1 <- mean(M1)
	SD1 <- mean(SD1)
	
	
	seKompstuf <- function(resCuts, cutScores, M1 , SD1 , M2 , SD2 , linkerror  ){
		if(any(is.na(resCuts[,1]))) {
			resCuts1 <- resCuts[-which(is.na(resCuts[,1])),]
		} else {
			resCuts1 <- resCuts
		}	
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
				komp[2] <- 100000000 } else {
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
	
	resCuts <- seKompstuf(resCuts, cutScores, M1 , SD1 , M2 , SD2 , linkerror)
	
	if(writeCsv) {
		stopifnot(!is.null(path))
		write.csv2(PV500T1, file=paste0(path, "/PV500T1_", type, "_", Sys.Date(), ".csv"), row.names=FALSE)
		write.csv2(PV500T2, file=paste0(path, "/PV500T2_", type, "_", Sys.Date(), ".csv"), row.names=FALSE)
		write.csv2(resCuts, file=paste0(path, "/levelTrend_", type, "_", Sys.Date(), ".csv"), row.names=FALSE)
		write.csv2(resMeans, file=paste0(path, "/meanTrend_", type, "_", Sys.Date(), ".csv"), row.names=FALSE)
	}
	
	cat ( paste ("Bye! ", Sys.time(), "\n" ) ) 
	
	if(!is.null(cutScores)) {return(list(resMeans, resCuts)) } else {
	return(list(resMeans))}
	
}