### 10.02.2014
### Funktion identifiziert lme4-Version und variiert das Auslesen je nachdem ob Version < 1 oder > 1
get.lmer.effects <- function ( lmerObj , saveData = TRUE) {
             if(!exists("fixef"))        {library(lme4)}
             lme4Ver  <- installed.packages()
             lme4Ver  <- substr(lme4Ver[lme4Ver[,"Package"] == "lme4","Version"],1,1)
             random   <- VarCorr( lmerObj ) 
             fixed    <- lme4::fixef(lmerObj)                                   ### zunaechst werden die random effects extrahiert
             randomF  <- do.call("rbind", lapply(names(random), FUN = function ( y ) {
                         ret <- data.frame(Var1 = colnames(random[[y]]), Var2 = NA, type = "random", random.group = y, parameter = "var", value = diag(random[[y]]), stringsAsFactors = FALSE )
                         ret <- rbind(ret, data.frame(Var1 = colnames(random[[y]]), Var2 = NA, type = "random", random.group = y, parameter = "sd", value = sqrt(diag(random[[y]])), stringsAsFactors = FALSE ))
                         if(nrow(attr(random[[y]], "correlation")) > 1)  {      ### Gibt es Korrelationen?
                            korTab   <- reshape2::melt(attr(random[[y]], "correlation"))
                            wahl2    <- which(!korTab[,1] == korTab[,2])
                            for (u in 1:2) {korTab[,u] <- as.character(korTab[,u])}
                            wahl1    <- which(!duplicated(apply(korTab, 1, FUN = function ( xx ) { paste( sort(c(xx[1], xx[2])), collapse="_") })))
                            ret      <- plyr::rbind.fill( ret, data.frame ( korTab[intersect(wahl1, wahl2),], type = "random", random.group = y, parameter = "correlation", stringsAsFactors = FALSE ) )
                         }
                         return(ret)}))
             if ( !is.na(attr(random, "sc"))) {                                 ### vergleichen zwischen Versionen!
                  randomF  <- plyr::rbind.fill ( randomF, rbind ( data.frame ( Var1 = "residual", type = "random", parameter = "var", value = attr(random, "sc")^2, stringsAsFactors = FALSE ), data.frame ( Var1 = "residual", type = "random", parameter = "sd", value = attr(random, "sc"), stringsAsFactors = FALSE )))
             }                                                                  ### jetzt kommen die fixed effects
             fixedF   <- data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "est", value = as.numeric(fixed), stringsAsFactors = FALSE )
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "se", value = sqrt(diag(vcov(lmerObj))), stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "z.value", value = fixedF[fixedF[,"parameter"] == "est","value"] / fixedF[fixedF[,"parameter"] == "se","value"], stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "p.value", value = 2*(1-pnorm(abs(fixedF[fixedF[,"parameter"] == "z.value","value"]))), stringsAsFactors = FALSE ))
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
                 korTab   <- reshape2::melt(korMat)
                 wahl2    <- which(!korTab[,1] == korTab[,2])
                 namenFixed <- gsub(":", "________XX________",names(fixed))     ### That's the problem: a <- 1:3; car::recode(a, "1 = 'test'; 2 = 'mist'; 3 = 'test:mist'")
                 recodeString <- paste("'", 1:length(fixed),"' = '", namenFixed,"'", sep = "", collapse = "; ")
                 for ( u in 1:2) {
                       korTab[,u] <- car::recode(korTab[,u], recodeString)
                       korTab[,u] <- gsub("________XX________",":",korTab[,u])
                 }
                 wahl1    <- which(!duplicated(apply(korTab, 1, FUN = function ( xx ) { paste( sort(c(xx[1], xx[2])), collapse="_") })))
                 fixedF   <- plyr::rbind.fill( fixedF, data.frame ( korTab[intersect(wahl1, wahl2),], type = "fixed", random.group = NA, parameter = "correlation", stringsAsFactors = FALSE ) )
             }
             LogLik   <- logLik(lmerObj)                                        ### nun kommen die deviance measures
             if(lme4Ver == "0"){ deviancF <- data.frame(type = "model", parameter = c("LogLik", "df", paste("Deviance",names(lmerObj@deviance),sep="_"), "AIC", "BIC"), value = c(LogLik[[1]], attr(LogLik, "df"), lmerObj@deviance, AIC(LogLik ), BIC(LogLik)), stringsAsFactors = FALSE ) 
                        } else { deviancF <- data.frame(type = "model", parameter = c("LogLik", "df", paste("Deviance",names(lmerObj@devcomp$cmp),sep="_"), "AIC", "BIC"), value = c(LogLik[[1]], attr(LogLik, "df"), lmerObj@devcomp$cmp, AIC(LogLik ), BIC(LogLik)), stringsAsFactors = FALSE )  }
             ret      <- plyr::rbind.fill(randomF, fixedF)
             ret      <- plyr::rbind.fill(ret, deviancF)
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
                attr(ret, "formula") <- list(completeCall =  paste(deparse(lmerObj@call),collapse="", sep=""), sepCall = as.character(lmerObj@call))}
             if(saveData == TRUE ) { attr(ret, "data")    <- lmerObj@frame }
             class(ret) <- c("data.frame", "lmer.effects")
             return(ret)}



get.fixef <- function ( lmer.effects, easy.to.difficult = FALSE, withCorrelation = FALSE) {
             if(!"lmer.effects" %in% class(lmer.effects) ) {lmer.effects <- get.lmer.effects(lmer.effects)}
             withoutCorr <- lmer.effects[intersect ( which(lmer.effects[,"type"] == "fixed"), which (lmer.effects[,"parameter"] != "correlation" ) ) ,]
             withoutCorr <- reshape2::dcast(withoutCorr, Var1~parameter, value.var = "value")[,c("Var1", "est", "se", "z.value", "p.value")]
             if(easy.to.difficult == TRUE) { withoutCorr[,"est"] <- -1 * withoutCorr[,"est"]}
             if(withCorrelation == TRUE ) {
                onlyCorr    <- lmer.effects[intersect ( which (lmer.effects[,"parameter"] == "correlation" ), which(lmer.effects[,"type"] == "fixed")) ,]
                if(nrow(onlyCorr)>0) {
                   onlyCorr    <- reshape2::dcast(onlyCorr, Var1~Var2, value.var = "value")
                   colnames(onlyCorr)[-1] <- paste("corr", colnames(onlyCorr)[-1], sep="_")
                   withoutCorr <- merge(withoutCorr, onlyCorr, by = "Var1", all = TRUE)
                }
             }
             return(withoutCorr)}
             

get.ranef <- function ( lmer.effects ) {
             if(!"lmer.effects" %in% class(lmer.effects) ) {lmer.effects <- get.lmer.effects(lmer.effects)}
             withoutCorr <- lmer.effects[lmer.effects[,"type"] == "random" & lmer.effects[,"parameter"] != "correlation",]
             obj     <- reshape2::dcast(withoutCorr, Var1+random.group~parameter, value.var = "value")
             match1  <- match(c("random.group", "Var1", "var", "sd"), colnames(obj))
             obj     <- obj[,match1]
             onlyCor <- lmer.effects[lmer.effects[,"type"] == "random" & lmer.effects[,"parameter"] == "correlation",]
             if(nrow(onlyCor)>0) {
                obj2 <- reshape2::dcast(onlyCor, Var1+random.group~Var2, value.var = "value")
                obj  <- merge(obj, obj2, by = c("random.group", "Var1"), all = TRUE)
             }
             match2  <- na.omit(match("residual", obj[,"Var1"]))
             if(length(match2)>0) {obj <- obj[c(setdiff(1:nrow(obj),match2),match2) ,]}
             return(obj)}