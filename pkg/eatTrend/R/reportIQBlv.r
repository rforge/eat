reportIQBlv <- function(path, chapter=c("4", "5a", "5b", "6"),domain="Englisch Leseverstehen",frzRep=FALSE) {

	if(chapter == "6") {
		files <- dir(path)[c(grep("meanTrend_FCIP_GES_", dir(path)),grep("PV500T1", dir(path)))]
		fileList <- list()
		for(ll in files) {
			fileList[[ll]] <- read.csv2(file.path(path,ll), stringsAsFactors=FALSE)
		}
		PV500T1m <- reshape2::melt(fileList[[2]][,c(1:20)], id=1:5)
		PV500T1m <- eatPrep::set.col.type(PV500T1m, list(character = "variable", numeric = c("weightsT1", "value")))
		if(any(is.na(c(PV500T1m$jkzoneT1,PV500T1m$jkrepT1,PV500T1m$weightsT1)))) {
				PV500T1m <- PV500T1m[-c(which(is.na(PV500T1m$jkzoneT1)|is.na(PV500T1m$jkrepT1)|is.na(PV500T1m$weightsT1))),]
			}
		quantiles  <- jk2.quantile(PV500T1m, ID = "idstud", wgt = "weightsT1",
                          type = "JK2", PSU = "jkzoneT1", repInd = "jkrepT1",
                          imp = "variable", groups = "countriesT1", dependent="value", doCheck=TRUE,
							probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95))
		quantiles  <- dcast(quantiles, group ~ parameter + coefficient)
		quantiles2  <- jk2.quantile(PV500T1m, ID = "idstud", wgt = "weightsT1",
                          type = "JK2", PSU = "jkzoneT1", repInd = "jkrepT1",
                          imp = "variable", dependent="value", doCheck=TRUE,
							probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95))
		quantiles2  <- dcast(quantiles2, group ~ parameter + coefficient)
		quantiles2[1,1] <- "GES"
		quantiles <- rbind(quantiles, quantiles2)
		quantiles[,1] <- landerNam(quantiles[,1])
		resTab <- mergeData("country", list(fileList[[1]][,1:5], quantiles), c("country", "group"))
		resTab <- resTab[,c(which(!grepl("_se", names(resTab))),which(grepl("_se", names(resTab))))]
		write.csv2(resTab, file=paste0(path, "/chapter6_", Sys.Date(), ".csv"), row.names=FALSE)
		}

	if(chapter == "4") {
		files <- dir(path)[grep("levelTrend", dir(path))]
		groups <- unlist(lapply(strsplit(files, "_"), function(uu) uu[3]))
		stopifnot(setequal(c("GES", "GESoSPF", "GY","GESopt", "GESoSPFopt", "GYopt","GESstu", "GESoSPFstu", "GYstu"),groups))
		fileList <- fileList2 <- list()
		for(ll in groups) {
			fileList2[[ll]] <- read.csv2(file.path(path,files[which(groups %in% ll)]), stringsAsFactors=FALSE)
		}
		fileList[["GES"]] <- rbind(fileList2[["GES"]],fileList2[["GESopt"]],fileList2[["GESstu"]])
		fileList[["GY"]] <- rbind(fileList2[["GY"]],fileList2[["GYopt"]],fileList2[["GYstu"]])
		fileList[["GESoSPF"]] <- rbind(fileList2[["GESoSPF"]],fileList2[["GESoSPFopt"]],fileList2[["GESoSPFstu"]])
		
		#stufen <- unique(fileList[[1]]$parameter)[c(grep("Mindeststandard",unique(fileList[[1]]$parameter)),grep("Regelstandard",unique(fileList[[1]]$parameter)),grep("Optimalstandard",unique(fileList[[1]]$parameter)))]
		stufen <- unique(fileList[[1]]$parameter)[-c(grep("zwischen",unique(fileList[[1]]$parameter)),grep("unter",unique(fileList[[1]]$parameter)))]
			
		countries <- unique(fileList[[1]]$country)
		resTabs <- list()
		for(co in countries) {
			resTabs[[co]] <- data.frame(country=rep(co,7), stringsAsFactors=FALSE)
			resTabs[[co]]$domain <- rep(domain,7)
			resTabs[[co]]$gruppe <- c("9. Jg. insgesamt 2015", "9. Jg. insgesamt 2015 ohne SuS mit SPF","9. Jg. insgesamt 2009 ohne SuS mit SPF","9. Jg. insgesamt ohne SuS mit SPF: Differenz 2015 - 2009",  "Gymnasium 2015",  "Gymnasium 2009","Gymnasium: Differenz 2015 - 2009")
			for(jj in stufen) {
				resTabs[[co]][1,jj] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$estT1
				resTabs[[co]][1,paste("SE", jj)] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$seT1
				resTabs[[co]][2,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT1
				resTabs[[co]][2,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT1
				resTabs[[co]][3,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT2
				resTabs[[co]][3,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT2
				resTabs[[co]][4,jj] <- -subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estTrend
				resTabs[[co]][4,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seTrend
				resTabs[[co]][5,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT1
				resTabs[[co]][5,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT1
				resTabs[[co]][6,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT2
				resTabs[[co]][6,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT2
				resTabs[[co]][7,jj] <- -subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estTrend
				resTabs[[co]][7,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seTrend
			}
		}
		resTab <- do.call("rbind", resTabs)
		resTab[,1] <- landerNam(resTab[,1])
		# sortieren
		resTab <- resTab[,c(1:9,(grep("^SE",names(resTab)[-c(1:9)])+8),(grep("^SE",names(resTab)[-c(1:9)])+9))]
		write.csv2(resTab, file=paste0(path, "/chapter4_", Sys.Date(), ".csv"), row.names=FALSE)
	}


	if(frzRep) {
		if(chapter == "5a") {
			files <- dir(path)[grep("levelTrend", dir(path))]
			groups <- unlist(lapply(strsplit(files, "_"), function(uu) uu[3]))
			stopifnot(setequal(c("GES", "GY", "GESoSPF", "HSA", "HSAoSPF", "MSA", "MSAoSPF", "MSAoGYoSPF", "MSAoGY"),groups))
			fileList <- list()
			for(ll in groups) {
				fileList[[ll]] <- read.csv2(file.path(path,files[which(groups %in% ll)]), stringsAsFactors=FALSE)
			}
			
			stufen <- unique(fileList[[1]]$parameter)
			countries <- unique(fileList[[1]]$country)
				
			resTabs <- list()
			for(co in countries) {
				resTabs[[co]] <- data.frame(country=rep(co,19), stringsAsFactors=FALSE)
				resTabs[[co]]$domain <- rep(domain,19)
				resTabs[[co]]$gruppe <- c("9. Jg. insgesamt 2015", "9. Jg. insgesamt 2015 ohne SuS mit SPF", "9. Jg. insgesamt 2009 ohne SuS mit SPF", "9. Jg. insgesamt ohne SuS mit SPF: Differenz 2015 - 2009", "Gymnasium 2015", "Gymnasium 2009", "Gymnasium: Differenz 2015 - 2009","MSA insgesamt 2015", "MSA insgesamt 2015 ohne SuS mit SPF", "MSA insgesamt 2009 ohne SuS mit SPF", "MSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","HSA insgesamt 2015", "HSA insgesamt 2015 ohne SuS mit SPF", "HSA insgesamt 2009 ohne SuS mit SPF", "HSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","MSA ohne Gymnasium 2015", "MSA ohne Gymnasium 2015 ohne SuS mit SPF", "MSA ohne Gymnasium 2009 ohne SuS mit SPF", "MSA ohne Gymnasium ohne SuS mit SPF: Differenz 2015 - 2009")
				for(jj in stufen) {
					resTabs[[co]][1,jj] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$estT2
					resTabs[[co]][2,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][3,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][4,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estTrend
						
					resTabs[[co]][5,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT2
					resTabs[[co]][6,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT1
					resTabs[[co]][7,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estTrend
					
					resTabs[[co]][8,jj] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$estT2
					resTabs[[co]][9,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][10,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][11,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][12,jj] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$estT2
					resTabs[[co]][13,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][14,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][15,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][16,jj] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$estT2
					resTabs[[co]][17,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][18,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][19,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estTrend
					}
				for(jj in stufen) {
					resTabs[[co]][1,paste("SE", jj)] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$seT2
					resTabs[[co]][2,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][3,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][4,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][5,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT2
					resTabs[[co]][6,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT1
					resTabs[[co]][7,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seTrend
					resTabs[[co]][8,paste("SE", jj)] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$seT2
					resTabs[[co]][9,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][10,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][11,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][12,paste("SE", jj)] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$seT2
					resTabs[[co]][13,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][14,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][15,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][16,paste("SE", jj)] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$seT2
					resTabs[[co]][17,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][18,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][19,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seTrend
					
				}
			}
			resTab <- do.call("rbind", resTabs)
			resTab[,1] <- landerNam(resTab[,1])
			write.csv2(resTab, file=paste0(path, "/chapter5a_", Sys.Date(), ".csv"), row.names=FALSE)
		}

		if(chapter == "5b") {
			files <- dir(path)[grep("levelTrend", dir(path))]
			groups <- unlist(lapply(strsplit(files, "_"), function(uu) uu[3]))
			stopifnot(setequal(c("GES", "GY", "GESoSPF", "HSA", "HSAoSPF", "MSA", "MSAoSPF", "MSAoGYoSPF", "MSAoGY", "GESh", "GYh", "GESoSPFh", "HSAh", "HSAoSPFh", "MSAh", "MSAoSPFh", "MSAoGYoSPFh", "MSAoGYh", "GESopt", "GYopt", "GESoSPFopt", "HSAopt", "HSAoSPFopt", "MSAopt", "MSAoSPFopt", "MSAoGYoSPFopt", "MSAoGYopt", "GESopth", "GYopth", "GESoSPFopth", "HSAopth", "HSAoSPFopth", "MSAopth", "MSAoSPFopth", "MSAoGYoSPFopth", "MSAoGYopth"),groups))
			
			fileList <- fileList2 <- list()
			for(ll in groups) {
				fileList2[[ll]] <- read.csv2(file.path(path,files[which(groups %in% ll)]), stringsAsFactors=FALSE)
			}
			fileList[["GES"]] <- rbind(fileList2[["GES"]],fileList2[["GESopt"]],fileList2[["GESh"]],fileList2[["GESopth"]])
			fileList[["GY"]] <- rbind(fileList2[["GY"]],fileList2[["GYopt"]],fileList2[["GYh"]],fileList2[["GYopth"]])
			fileList[["GESoSPF"]] <- rbind(fileList2[["GESoSPF"]],fileList2[["GESoSPFopt"]],fileList2[["GESoSPFh"]],fileList2[["GESoSPFopth"]])
			fileList[["HSA"]] <- rbind(fileList2[["HSA"]],fileList2[["HSAopt"]],fileList2[["HSAh"]],fileList2[["HSAopth"]])
			fileList[["HSAoSPF"]] <- rbind(fileList2[["HSAoSPF"]],fileList2[["HSAoSPFopt"]],fileList2[["HSAoSPFh"]],fileList2[["HSAoSPFopth"]])
			fileList[["MSA"]] <- rbind(fileList2[["MSA"]],fileList2[["MSAopt"]],fileList2[["MSAh"]],fileList2[["MSAopth"]])
			fileList[["MSAoSPF"]] <- rbind(fileList2[["MSAoSPF"]],fileList2[["MSAoSPFopt"]],fileList2[["MSAoSPFh"]],fileList2[["MSAoSPFopth"]])
			fileList[["MSAoGY"]] <- rbind(fileList2[["MSAoGY"]],fileList2[["MSAoGYopt"]],fileList2[["MSAoGYh"]],fileList2[["MSAoGYopth"]])
			fileList[["MSAoGYoSPF"]] <- rbind(fileList2[["MSAoGYoSPF"]],fileList2[["MSAoGYoSPFopt"]],fileList2[["MSAoGYoSPFh"]],fileList2[["MSAoGYoSPFopth"]])
			
			stufen <- unique(fileList[[1]]$parameter)[c(grep("Mindeststandard",unique(fileList[[1]]$parameter)),grep("Regelstandard",unique(fileList[[1]]$parameter)),grep("Optimalstandard",unique(fileList[[1]]$parameter)))]
			countries <- unique(fileList[[1]]$country)
			resTabs <- list()
		
			for(co in countries) {
				resTabs[[co]] <- data.frame(country=rep(co,19), stringsAsFactors=FALSE)
				resTabs[[co]]$domain <- rep(domain,19)
				resTabs[[co]]$gruppe <- c("9. Jg. insgesamt 2015", "9. Jg. insgesamt 2015 ohne SuS mit SPF", "9. Jg. insgesamt 2009 ohne SuS mit SPF", "9. Jg. insgesamt ohne SuS mit SPF: Differenz 2015 - 2009", "Gymnasium 2015", "Gymnasium 2009", "Gymnasium: Differenz 2015 - 2009","MSA insgesamt 2015", "MSA insgesamt 2015 ohne SuS mit SPF", "MSA insgesamt 2009 ohne SuS mit SPF", "MSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","HSA insgesamt 2015", "HSA insgesamt 2015 ohne SuS mit SPF", "HSA insgesamt 2009 ohne SuS mit SPF", "HSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","MSA ohne Gymnasium 2015", "MSA ohne Gymnasium 2015 ohne SuS mit SPF", "MSA ohne Gymnasium 2009 ohne SuS mit SPF", "MSA ohne Gymnasium ohne SuS mit SPF: Differenz 2015 - 2009")
				for(jj in stufen) {
					resTabs[[co]][1,jj] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$estT2
					resTabs[[co]][2,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][3,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][4,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estTrend
						
					resTabs[[co]][5,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT2
					resTabs[[co]][6,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT1
					resTabs[[co]][7,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estTrend
					
					resTabs[[co]][8,jj] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$estT2
					resTabs[[co]][9,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][10,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][11,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][12,jj] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$estT2
					resTabs[[co]][13,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][14,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][15,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][16,jj] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$estT2
					resTabs[[co]][17,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][18,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][19,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estTrend
					}
				for(jj in stufen) {
					resTabs[[co]][1,paste("SE", jj)] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$seT2
					resTabs[[co]][2,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][3,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][4,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][5,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT2
					resTabs[[co]][6,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT1
					resTabs[[co]][7,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seTrend
					resTabs[[co]][8,paste("SE", jj)] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$seT2
					resTabs[[co]][9,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][10,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][11,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][12,paste("SE", jj)] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$seT2
					resTabs[[co]][13,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][14,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][15,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][16,paste("SE", jj)] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$seT2
					resTabs[[co]][17,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][18,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][19,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seTrend
					}
			}
			resTab <- do.call("rbind", resTabs)
			resTab[,1] <- landerNam(resTab[,1])
			# sortieren
			resTab <- resTab[,c(1:3,grep("(HSA)", names(resTab))[1:3],grep("(MSA)", names(resTab))[1:3],grep("(HSA)", names(resTab))[4:6],grep("(MSA)", names(resTab))[4:6])]
			write.csv2(resTab, file=paste0(path, "/chapter5b_", Sys.Date(), ".csv"), row.names=FALSE)
		}
	
	} else {
		if(chapter == "5a") {
			files <- dir(path)[grep("levelTrend", dir(path))]
			groups <- unlist(lapply(strsplit(files, "_"), function(uu) uu[3]))
			stopifnot(setequal(c("GES", "GY", "GESoSPF", "HSA", "HSAoSPF", "MSA", "MSAoSPF", "MSAoGYoSPF", "MSAoGY"),groups))
			fileList <- list()
			for(ll in groups) {
				fileList[[ll]] <- read.csv2(file.path(path,files[which(groups %in% ll)]), stringsAsFactors=FALSE)
			}
			
			stufen <- unique(fileList[[1]]$parameter)
			countries <- unique(fileList[[1]]$country)
				
			resTabs <- list()
			for(co in countries) {
				resTabs[[co]] <- data.frame(country=rep(co,19), stringsAsFactors=FALSE)
				resTabs[[co]]$domain <- rep(domain,19)
				resTabs[[co]]$gruppe <- c("9. Jg. insgesamt 2015", "9. Jg. insgesamt 2015 ohne SuS mit SPF", "9. Jg. insgesamt 2009 ohne SuS mit SPF", "9. Jg. insgesamt ohne SuS mit SPF: Differenz 2015 - 2009", "Gymnasium 2015", "Gymnasium 2009", "Gymnasium: Differenz 2015 - 2009","MSA insgesamt 2015", "MSA insgesamt 2015 ohne SuS mit SPF", "MSA insgesamt 2009 ohne SuS mit SPF", "MSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","HSA insgesamt 2015", "HSA insgesamt 2015 ohne SuS mit SPF", "HSA insgesamt 2009 ohne SuS mit SPF", "HSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","MSA ohne Gymnasium 2015", "MSA ohne Gymnasium 2015 ohne SuS mit SPF", "MSA ohne Gymnasium 2009 ohne SuS mit SPF", "MSA ohne Gymnasium ohne SuS mit SPF: Differenz 2015 - 2009")
				for(jj in stufen) {
					resTabs[[co]][1,jj] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$estT1
					resTabs[[co]][2,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][3,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][4,jj] <- -subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estTrend
						
					resTabs[[co]][5,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT1
					resTabs[[co]][6,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT2
					resTabs[[co]][7,jj] <- -subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estTrend
					
					resTabs[[co]][8,jj] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$estT1
					resTabs[[co]][9,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][10,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][11,jj] <- -subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][12,jj] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$estT1
					resTabs[[co]][13,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][14,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][15,jj] <- -subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][16,jj] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$estT1
					resTabs[[co]][17,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][18,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][19,jj] <- -subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estTrend
					}
				for(jj in stufen) {
					resTabs[[co]][1,paste("SE", jj)] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$seT1
					resTabs[[co]][2,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][3,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][4,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][5,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT1
					resTabs[[co]][6,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT2
					resTabs[[co]][7,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seTrend
					resTabs[[co]][8,paste("SE", jj)] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$seT1
					resTabs[[co]][9,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][10,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][11,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][12,paste("SE", jj)] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$seT1
					resTabs[[co]][13,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][14,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][15,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][16,paste("SE", jj)] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$seT1
					resTabs[[co]][17,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][18,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][19,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seTrend
					
				}
			}
			resTab <- do.call("rbind", resTabs)
			resTab[,1] <- landerNam(resTab[,1])
			write.csv2(resTab, file=paste0(path, "/chapter5a_", Sys.Date(), ".csv"), row.names=FALSE)
		}

		if(chapter == "5b") {
			files <- dir(path)[grep("levelTrend", dir(path))]
			groups <- unlist(lapply(strsplit(files, "_"), function(uu) uu[3]))
			stopifnot(setequal(c("GES", "GY", "GESoSPF", "HSA", "HSAoSPF", "MSA", "MSAoSPF", "MSAoGYoSPF", "MSAoGY", "GESh", "GYh", "GESoSPFh", "HSAh", "HSAoSPFh", "MSAh", "MSAoSPFh", "MSAoGYoSPFh", "MSAoGYh", "GESopt", "GYopt", "GESoSPFopt", "HSAopt", "HSAoSPFopt", "MSAopt", "MSAoSPFopt", "MSAoGYoSPFopt", "MSAoGYopt", "GESopth", "GYopth", "GESoSPFopth", "HSAopth", "HSAoSPFopth", "MSAopth", "MSAoSPFopth", "MSAoGYoSPFopth", "MSAoGYopth"),groups))
			
			fileList <- fileList2 <- list()
			for(ll in groups) {
				fileList2[[ll]] <- read.csv2(file.path(path,files[which(groups %in% ll)]), stringsAsFactors=FALSE)
			}
			fileList[["GES"]] <- rbind(fileList2[["GES"]],fileList2[["GESopt"]],fileList2[["GESh"]],fileList2[["GESopth"]])
			fileList[["GY"]] <- rbind(fileList2[["GY"]],fileList2[["GYopt"]],fileList2[["GYh"]],fileList2[["GYopth"]])
			fileList[["GESoSPF"]] <- rbind(fileList2[["GESoSPF"]],fileList2[["GESoSPFopt"]],fileList2[["GESoSPFh"]],fileList2[["GESoSPFopth"]])
			fileList[["HSA"]] <- rbind(fileList2[["HSA"]],fileList2[["HSAopt"]],fileList2[["HSAh"]],fileList2[["HSAopth"]])
			fileList[["HSAoSPF"]] <- rbind(fileList2[["HSAoSPF"]],fileList2[["HSAoSPFopt"]],fileList2[["HSAoSPFh"]],fileList2[["HSAoSPFopth"]])
			fileList[["MSA"]] <- rbind(fileList2[["MSA"]],fileList2[["MSAopt"]],fileList2[["MSAh"]],fileList2[["MSAopth"]])
			fileList[["MSAoSPF"]] <- rbind(fileList2[["MSAoSPF"]],fileList2[["MSAoSPFopt"]],fileList2[["MSAoSPFh"]],fileList2[["MSAoSPFopth"]])
			fileList[["MSAoGY"]] <- rbind(fileList2[["MSAoGY"]],fileList2[["MSAoGYopt"]],fileList2[["MSAoGYh"]],fileList2[["MSAoGYopth"]])
			fileList[["MSAoGYoSPF"]] <- rbind(fileList2[["MSAoGYoSPF"]],fileList2[["MSAoGYoSPFopt"]],fileList2[["MSAoGYoSPFh"]],fileList2[["MSAoGYoSPFopth"]])
			
			stufen <- unique(fileList[[1]]$parameter)[c(grep("Mindeststandard",unique(fileList[[1]]$parameter)),grep("Regelstandard",unique(fileList[[1]]$parameter)),grep("Optimalstandard",unique(fileList[[1]]$parameter)))]
			countries <- unique(fileList[[1]]$country)
			resTabs <- list()
		
			for(co in countries) {
				resTabs[[co]] <- data.frame(country=rep(co,19), stringsAsFactors=FALSE)
				resTabs[[co]]$domain <- rep(domain,19)
				resTabs[[co]]$gruppe <- c("9. Jg. insgesamt 2015", "9. Jg. insgesamt 2015 ohne SuS mit SPF", "9. Jg. insgesamt 2009 ohne SuS mit SPF", "9. Jg. insgesamt ohne SuS mit SPF: Differenz 2015 - 2009", "Gymnasium 2015", "Gymnasium 2009", "Gymnasium: Differenz 2015 - 2009","MSA insgesamt 2015", "MSA insgesamt 2015 ohne SuS mit SPF", "MSA insgesamt 2009 ohne SuS mit SPF", "MSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","HSA insgesamt 2015", "HSA insgesamt 2015 ohne SuS mit SPF", "HSA insgesamt 2009 ohne SuS mit SPF", "HSA insgesamt ohne SuS mit SPF: Differenz 2015 - 2009","MSA ohne Gymnasium 2015", "MSA ohne Gymnasium 2015 ohne SuS mit SPF", "MSA ohne Gymnasium 2009 ohne SuS mit SPF", "MSA ohne Gymnasium ohne SuS mit SPF: Differenz 2015 - 2009")
				for(jj in stufen) {
					resTabs[[co]][1,jj] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$estT1
					resTabs[[co]][2,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][3,jj] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][4,jj] <- -subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$estTrend
						
					resTabs[[co]][5,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT1
					resTabs[[co]][6,jj] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estT2
					resTabs[[co]][7,jj] <- -subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$estTrend
					
					resTabs[[co]][8,jj] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$estT1
					resTabs[[co]][9,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][10,jj] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][11,jj] <- -subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][12,jj] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$estT1
					resTabs[[co]][13,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][14,jj] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][15,jj] <- -subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$estTrend
					
					resTabs[[co]][16,jj] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$estT1
					resTabs[[co]][17,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT1
					resTabs[[co]][18,jj] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estT2
					resTabs[[co]][19,jj] <- -subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$estTrend
					}
				for(jj in stufen) {
					resTabs[[co]][1,paste("SE", jj)] <- subset(fileList[["GES"]],fileList[["GES"]]$country == co & fileList[["GES"]]$parameter == jj)$seT1
					resTabs[[co]][2,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][3,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][4,paste("SE", jj)] <- subset(fileList[["GESoSPF"]],fileList[["GESoSPF"]]$country == co & fileList[["GESoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][5,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT1
					resTabs[[co]][6,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seT2
					resTabs[[co]][7,paste("SE", jj)] <- subset(fileList[["GY"]],fileList[["GY"]]$country == co & fileList[["GY"]]$parameter == jj)$seTrend
					resTabs[[co]][8,paste("SE", jj)] <- subset(fileList[["MSA"]],fileList[["MSA"]]$country == co & fileList[["MSA"]]$parameter == jj)$seT1
					resTabs[[co]][9,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][10,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][11,paste("SE", jj)] <- subset(fileList[["MSAoSPF"]],fileList[["MSAoSPF"]]$country == co & fileList[["MSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][12,paste("SE", jj)] <- subset(fileList[["HSA"]],fileList[["HSA"]]$country == co & fileList[["HSA"]]$parameter == jj)$seT1
					resTabs[[co]][13,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][14,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][15,paste("SE", jj)] <- subset(fileList[["HSAoSPF"]],fileList[["HSAoSPF"]]$country == co & fileList[["HSAoSPF"]]$parameter == jj)$seTrend
					resTabs[[co]][16,paste("SE", jj)] <- subset(fileList[["MSAoGY"]],fileList[["MSAoGY"]]$country == co & fileList[["MSAoGY"]]$parameter == jj)$seT1
					resTabs[[co]][17,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT1
					resTabs[[co]][18,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seT2
					resTabs[[co]][19,paste("SE", jj)] <- subset(fileList[["MSAoGYoSPF"]],fileList[["MSAoGYoSPF"]]$country == co & fileList[["MSAoGYoSPF"]]$parameter == jj)$seTrend
					}
			}
			resTab <- do.call("rbind", resTabs)
			resTab[,1] <- landerNam(resTab[,1])
			# sortieren
			resTab <- resTab[,c(1:3,grep("(HSA)", names(resTab))[1:3],grep("(MSA)", names(resTab))[1:3],grep("(HSA)", names(resTab))[4:6],grep("(MSA)", names(resTab))[4:6])]
			write.csv2(resTab, file=paste0(path, "/chapter5b_", Sys.Date(), ".csv"), row.names=FALSE)
		}
	}
	
	return(resTab)
}

landerNam <- function(kurzelVec) {
	landVec <- car::recode(kurzelVec, "'SL'='Saarland'; 'RP'='Rheinland-Pfalz'; 'NW'='Nordrhein-Westfalen'; 'NI'='Niedersachsen'; 'HB'='Bremen'; 'SH'='Schleswig-Holstein'; 'HH'='Hamburg'; 'GES'='Deutschland'; 'MV'='Mecklenburg-Vorpommern'; 'BB'='Brandenburg'; 'BE'='Berlin'; 'SN'='Sachsen'; 'BY'='Bayern'; 'BW'='Baden-Württemberg'; 'HE'='Hessen'; 'TH'='Thüringen'; 'ST'='Sachsen-Anhalt'")
	return(landVec)
}


