get.lmer.effects <- function ( lmerObj ) {
             if(!exists("fixef"))        {library(lme4)}
             if(!exists("rbind.fill"))   {library(plyr)}
             if(!exists("dcast"))        {library(reshape2)}
             if(!exists("boxCox"))       {library(car)}
             random   <- VarCorr( lmerObj )                                     ### zunaechst werden die random effects extrahiert
             fixed    <- lme4::fixef(lmerObj)
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
             if ( !is.na(attr(random, "sc"))) {
                  randomF  <- plyr::rbind.fill ( randomF, rbind ( data.frame ( Var1 = "residual", type = "random", parameter = "var", value = attr(random, "sc")^2, stringsAsFactors = FALSE ), data.frame ( Var1 = "residual", type = "random", parameter = "sd", value = attr(random, "sc"), stringsAsFactors = FALSE )))
             }                                                                  ### jetzt kommen die fixed effects
             fixedF   <- data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "est", value = as.numeric(fixed), stringsAsFactors = FALSE )
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "se", value = sqrt(diag(vcov(lmerObj))), stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "z.value", value = fixedF[fixedF[,"parameter"] == "est","value"] / fixedF[fixedF[,"parameter"] == "se","value"], stringsAsFactors = FALSE ))
             fixedF   <- rbind(fixedF, data.frame ( Var1 = names(fixed), Var2 = NA, type = "fixed", random.group = NA, parameter = "p.value", value = 2*(1-pnorm(abs(fixedF[fixedF[,"parameter"] == "z.value","value"]))), stringsAsFactors = FALSE ))
             rr       <- as(sigma(lmerObj)^2 * chol2inv(lmerObj@RX, size = lmerObj@dims['p']), "dpoMatrix")
             nms      <- colnames(lmerObj@X)                                    ### extract matrix of fixed effects
             dimnames(rr) <- list(nms, nms)
             korTab   <- reshape2::melt(as.matrix(as(rr, "corMatrix")))
             if(nrow( as.matrix(as(rr, "corMatrix")) ) > 1 ) {
                 wahl2    <- which(!korTab[,1] == korTab[,2])
                 recodeString <- paste("'", 1:length(fixed),"' = '", names(fixed),"'", sep = "", collapse = "; ")
                 for ( u in 1:2) {korTab[,u] <- car::recode(korTab[,u], recodeString)}
                 wahl1    <- which(!duplicated(apply(korTab, 1, FUN = function ( xx ) { paste( sort(c(xx[1], xx[2])), collapse="_") })))
                 fixedF   <- plyr::rbind.fill( fixedF, data.frame ( korTab[intersect(wahl1, wahl2),], type = "fixed", random.group = NA, parameter = "correlation", stringsAsFactors = FALSE ) )
             }
             LogLik   <- logLik(lmerObj)                                        ### nun kommen die deviance measures
             deviancF <- data.frame(type = "model", parameter = c("LogLik", "df", paste("Deviance",names(lmerObj@deviance),sep="_"), "AIC", "BIC"), value = c(LogLik[[1]], attr(LogLik, "df"), lmerObj@deviance, AIC(LogLik ), BIC(LogLik)), stringsAsFactors = FALSE )
             ret      <- plyr::rbind.fill(randomF, fixedF)
             ret      <- plyr::rbind.fill(ret, deviancF)
             return(ret)}
