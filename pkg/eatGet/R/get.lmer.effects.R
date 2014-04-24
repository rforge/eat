
get.lmer.effects.forBootMer <- function ( lmerObj ) {get.lmer.effects ( lmerObj=lmerObj , saveData = FALSE)@results[,"value"]}

get.lmer.effects <- function ( lmerObj , bootMerObj = NULL, conf = .95, saveData = FALSE) {
             model    <- as.character(substitute(lmerObj))                      ### implementieren wie in p:\ZKD\07_Code\dev\get.lmer.effects\get.lmer.effects_Konzept.xlsx
             checkForReshape()                                                  ### Beispiel in c:\diskdrv\Winword\Psycho\IQB\Dropbox\Literatur\R_help\Bates_2010_lme4_book.rsy
            # if(!exists("fixef"))        {library(lme4)}
             lme4Ver  <- "1"
            # lme4Ver  <- substr(lme4Ver[lme4Ver[,"Package"] == "lme4","Version"],1,1)
             random   <- VarCorr( lmerObj ) 
             fixed    <- fixef(lmerObj)                                   ### zunaechst werden die random effects extrahiert
             randomF  <- do.call("rbind", lapply(names(random), FUN = function ( y ) {
                         ret <- data.frame(model = model, Var1 = colnames(random[[y]]), Var2 = NA, type = "random", random.group = y, par = "var", derived.par = NA, value = diag(random[[y]]), stringsAsFactors = FALSE )
                         ret <- rbind(ret, data.frame(model = model, Var1 = colnames(random[[y]]), Var2 = NA, type = "random", random.group = y, par = "sd", derived.par = NA, value = sqrt(diag(random[[y]])), stringsAsFactors = FALSE ))
                         if(nrow(attr(random[[y]], "correlation")) > 1)  {      ### Gibt es Korrelationen?
                            korTab   <- melt(attr(random[[y]], "correlation"))
                            wahl2    <- which(!korTab[,1] == korTab[,2])
                            for (u in 1:2) {korTab[,u] <- as.character(korTab[,u])}
                            wahl1    <- which(!duplicated(apply(korTab, 1, FUN = function ( xx ) { paste( sort(c(xx[1], xx[2])), collapse="_") })))
                            ret      <- rbind.fill( ret, data.frame ( model=model, korTab[intersect(wahl1, wahl2),], type = "random", random.group = y, par = "correlation", derived.par = NA, stringsAsFactors = FALSE ) )
                         }
                         return(ret)}))
             if ( !is.na(attr(random, "sc"))) {                                 ### vergleichen zwischen Versionen!
                  randomF  <- rbind.fill ( randomF, rbind ( data.frame ( model=model, Var1 = "residual", type = "random", par = "var", derived.par = NA, value = attr(random, "sc")^2, stringsAsFactors = FALSE ), data.frame ( model=model, Var1 = "residual", type = "random", par = "sd", derived.par = NA, value = attr(random, "sc"), stringsAsFactors = FALSE )))
             }                                                                  ### jetzt kommen die fixed effects
             fixedF   <- data.frame ( model=model, Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, par = "est", derived.par = NA, value = as.numeric(fixed), stringsAsFactors = FALSE )
             fixedF   <- rbind(fixedF, data.frame ( model=model, Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, par = "se", derived.par = NA, value = sqrt(diag(vcov(lmerObj))), stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( model=model, Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, par = "z.value", derived.par = NA, value = fixedF[fixedF[,"par"] == "est","value"] / fixedF[fixedF[,"par"] == "se","value"], stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( model=model, Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, par = "p.value", derived.par = NA, value = 2*(1-pnorm(abs(fixedF[fixedF[,"par"] == "z.value","value"]))), stringsAsFactors = FALSE ))
             if(lme4Ver == "0"){
                 rr       <- as(sigma(lmerObj)^2 * chol2inv(lmerObj@RX, size = lmerObj@dims['p']), "dpoMatrix")
                 nms      <- colnames(lmerObj@X)                                ### extract matrix of fixed effects, for lme4 version < 1 
			       } else { 
                 rr       <- as(sigma(lmerObj)^2 * chol2inv(getME(lmerObj, name = "RX")), "dpoMatrix")
                 nms      <- colnames(lmerObj@pp$X)                             ### for lme4 version > 1
             }    
             dimnames(rr) <- list(nms, nms)
             if(is.null(nms)) {dimnames(rr) <- list(names(fixed), names(fixed))}
             korMat <- as.matrix(as(rr, "corMatrix"))	
             if(nrow( korMat ) > 1 ) {
                 korTab   <- melt(korMat)
                 wahl2    <- which(!korTab[,1] == korTab[,2])
                 namenFixed <- gsub(":", "________XX________",names(fixed))     ### That's the problem: a <- 1:3; car::recode(a, "1 = 'test'; 2 = 'mist'; 3 = 'test:mist'")
                 recodeString <- paste("'", 1:length(fixed),"' = '", namenFixed,"'", sep = "", collapse = "; ")
                 for ( u in 1:2) {
                       korTab[,u] <- recode(korTab[,u], recodeString)
                       korTab[,u] <- gsub("________XX________",":",korTab[,u])
                 }
                 wahl1    <- which(!duplicated(apply(korTab, 1, FUN = function ( xx ) { paste( sort(c(xx[1], xx[2])), collapse="_") })))
                 fixedF   <- rbind.fill( fixedF, data.frame ( model=model, korTab[intersect(wahl1, wahl2),], type = "fixed", random.group = NA, par = "correlation", derived.par=NA, stringsAsFactors = FALSE ) )
             }
             LogLik   <- logLik(lmerObj)                                        ### nun kommen die deviance measures
             if(lme4Ver == "0"){ deviancF <- data.frame(model=model, type = "model", par = c("LogLik", "df", paste("Deviance",names(lmerObj@deviance),sep="_"), "AIC", "BIC"), value = c(LogLik[[1]], attr(LogLik, "df"), lmerObj@deviance, AIC(LogLik ), BIC(LogLik)), stringsAsFactors = FALSE ) 
                        } else { deviancF <- data.frame(model=model, type = "model", par = c("LogLik", "df", paste("Deviance",names(lmerObj@devcomp$cmp),sep="_"), "AIC", "BIC"), value = c(LogLik[[1]], attr(LogLik, "df"), lmerObj@devcomp$cmp, AIC(LogLik ), BIC(LogLik)), stringsAsFactors = FALSE )  }
             ret      <- rbind.fill(randomF, fixedF)
             ret      <- rbind.fill(ret, deviancF)
             groups   <- lapply(names(table(randomF[,"random.group"])), FUN = function ( rg ) {
                         checkVar <- rg %in% colnames(lmerObj@frame)
                         if(checkVar == TRUE) {return(length(unique(lmerObj@frame[,rg])))} else { return(NULL)} })
             groups   <- groups[ which ( unlist(lapply(groups, is.null)) == FALSE)]
             names(groups) <- names(table(randomF[,"random.group"])) [ which(unlist ( lapply(names(table(randomF[,"random.group"])), FUN = function ( rg ) {
                              checkVar <- rg %in% colnames(lmerObj@frame)
                              if(checkVar == TRUE) {return(TRUE)} else { return(FALSE)} })))]
             groups$obs    <- nrow(lmerObj@frame)
             attr(ret, "groups")  <- groups
             if(lme4Ver == "0"){ attr(ret, "formula") <- lmerObj@call } else {  ### dies beides fuer lme4 version < 1; pruefen ob das andere auch klappt ...
                attr(ret, "formula") <- list(completeCall =  gsub(" +"," ", paste(deparse(lmerObj@call),collapse="", sep="")), sepCall = as.character(lmerObj@call))}
             if(saveData == TRUE ) { attr(ret, "data")    <- lmerObj@frame }
             ret      <- data.frame ( ret[,1,drop=FALSE], source = attr(ret, "formula")$sepCall[1], ret[,-1], stringsAsFactors = FALSE)
             class(ret) <- c("data.frame", "lmer.effects")                      ### untere Zeile: wenn bootMerObj uebergeben, dann werden jetzt bootstrap-Parameter angebunden
             if(!is.null(bootMerObj)) { 
                stopifnot(length(bootMerObj$t0) == nrow(ret))                   ### check: passen bootMerObj und lmerObj zusammen?
                stopifnot(all ( round(na.omit(bootMerObj$t0),digits=4) == round(na.omit(ret[,"value"]), digits=4)))
                btSE<- sapply(data.frame ( bootMerObj$t), sd)                   ### bootstrap standard errors 
                btBs<- sapply(data.frame ( bootMerObj$t), mean) - bootMerObj$t0 ### bootstrap bias 
                btMe<- sapply(data.frame ( bootMerObj$t), mean)                 ### bootstrap mean
                btMd<- sapply(data.frame ( bootMerObj$t), median)               ### bootstrap median
                ci  <- do.call("rbind", lapply(1:length(bootMerObj$t0), FUN = function ( l ) { 
                       if(!is.na(bootMerObj$t0[l]) & length(table(bootMerObj$t[,l])) > 1) { 
                          ci.l <- boot.ci(bootMerObj, type=c("norm", "basic"), conf=conf, index=l)
                          if(is.null(ci.l)) { ci.d <- NULL} else { 
                             ci.nb<- data.frame ( source = "boot.ci", derived.par = paste ( rep(c("lb","ub"), 2), ci.l$normal[1]*10^(nchar(ci.l$normal[1])-2) , rep(c("normal", "basic"),each=2),sep="."), value = c ( ci.l$normal[(length(ci.l$normal)-1):length(ci.l$normal)], ci.l$basic[(length(ci.l$basic)-1):length(ci.l$basic)]), stringsAsFactors = FALSE)
                             bt.SE<- data.frame ( source = "boot.ci", derived.par = c("bootSE", "bootMean", "bootMedian", "bootBias"), value = c(btSE[l], btMe[l], btMd[l], btBs[l]), stringsAsFactors = FALSE)
                             bootM<- data.frame ( source = "bootMer", derived.par = paste("Iteration", gsub(" ", "0", formatC(1:nrow(bootMerObj$t), width= nchar(nrow(bootMerObj$t)))),sep=""), value = bootMerObj$t[,l], stringsAsFactors = FALSE)
                             ci.d <- data.frame ( ret[l,-match(c("source", "derived.par", "value"), colnames(ret)) ], rbind(ci.nb, bt.SE, bootM), stringsAsFactors = FALSE)
                          }
                       } else { ci.d <- NULL}
                       return(ci.d)}))   
                ret <- rbind(ret, ci)
             }   
             colnames(ret) <- tolower(colnames(ret))
			 # return-Object der Klasse eatGot
			 retObj <- new ( "eatGot" )
			 retObj@results <- ret
			 return(retObj) }
			 
checkForReshape <- function () {
        if("package:reshape" %in% search() ) {
           cat("Warning: Package 'reshape' is attached. Functions in package 'eatRep' depend on 'reshape2'. 'reshape' and 'reshape2' conflict in some way.\n  'reshape' therefore will be detached now. \n")
           detach(package:reshape) } }			 
