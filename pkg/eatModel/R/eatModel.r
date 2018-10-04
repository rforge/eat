### keep = unbenutze wertelabels behalten?
### also wenn Variable x den Wert 6 = 'keine Angabe' gar nicht enthaelt, soll dann trotzdem ein Label
### dafuer geschrieben werden?
createSpssSyntaxSnippet <- function ( dat , file, keep = TRUE ) {
      sink(file=file)
    ### Schritt 1: Variablenlabels
      cat("  VARIABLE LABELS\n")
      for ( i in 1:ncol(dat)) {
            varLab <- attr(dat[,i],"varLabel")
            if ( length(varLab)>0) { cat(paste0("    ",colnames(dat)[i], " \"",varLab,"\" \n"))}
      }
      cat(".\n \n")
    ### Schritt 2: Wertelabels
      for ( i in 1:ncol(dat)) {
            valLab <- attr(dat[,i],"valLabel")
            if ( length(valLab)>0) {
                 if ( keep == FALSE ) {
                      weg <- setdiff ( valLab, dat[,i])
                      if ( length(weg)>0) { valLab <- valLab[-match(weg, valLab)] }
                 }
                 cat(paste(paste0("add value labels ", colnames(dat)[i]), valLab, paste0("'",names(valLab),"'"), sep=" ", collapse = ".\n"))
                 cat(".\n")
            }
      }
    ### Schritt 3: nach dem letzten Wertelabel muss Schraegstrich durch Punkt ersetzt werden
      cat("EXECUTE.\n")
      sink() }

### gsub for more than one pattern
gsubAll <- function(string, old, new) {
  stopifnot ( is.character(string), is.character(old), is.character(new), length(old) == length(new))
  for(i in seq_along(old)) {
      string <- gsub(old[i], new[i], string)
  }
  return(string)
}

### transformiert spss labels in zkd konvention ... achtung! das macht spss liste zu spss data.frame!
convertLabel <- function ( spssList , stringsAsFactors = TRUE, useZkdConvention = TRUE, replaceSpecialSigns = TRUE) {
                if ( "data.frame" %in% class(spssList )) { stop ( "'spssList' must not be of class 'data.frame'.\n")}
                varLabs<- attr(spssList, "variable.labels")
                valLabs<- lapply ( spssList, attr, "value.labels")
                if ( replaceSpecialSigns == TRUE ) {
                     varLabs <- gsubAll ( string = varLabs, old = c("ä", "ü", "ö", "ß", "Ü"), new = c("ae", "ue", "oe", "ss", "Ue"))
                     valLabs <- lapply ( valLabs, FUN = function ( w ) {
                                if ( !is.null(w)) {
                                     names(w) <- gsubAll ( string = names(w), old = c("ä", "ü", "ö", "ß", "Ü"), new = c("ae", "ue", "oe", "ss", "Ue"))
                                }
                                return(w) } )
                }
                if ( useZkdConvention == TRUE ) { zielnam <- c("varLabel", "valLabel") }  else  { zielnam <- c("variable.labels", "value.labels") }
                datFr  <- data.frame ( spssList, stringsAsFactors = stringsAsFactors)
                for ( u in 1:ncol(datFr)) {
                      if ( !is.na(varLabs[colnames(datFr)[u]] )) { attr(datFr[,u], zielnam[1]) <- varLabs[[u]]}
                      if ( !is.null(valLabs[[colnames(datFr)[u]]] )) { attr(datFr[,u], zielnam[2]) <- valLabs[[u]]}
                }
                return(datFr)}

### Hilfsfunktion zur Bestimmung der Anzahl der Beobachtungen je Itempaar
nObsItemPairs <- function ( responseMatrix, q3MinType) {
                 spl <- data.frame ( combn(colnames(responseMatrix),2), stringsAsFactors = FALSE)
                 splM<- do.call("rbind", lapply ( spl, FUN = function ( y ) {
                        if ( q3MinType == "singleObs" ) {
                             minVal <- min ( table(data.frame ( responseMatrix[,y])))
                        }  else  {
                             minVal <- min(c(rowSums(table(data.frame ( responseMatrix[,y]))), colSums(table(data.frame ( responseMatrix[,y])))))
                        }
                        ret <- data.frame ( Var1 = sort(y)[1], Var2 = sort(y)[2], minValue = minVal)
                        return(ret)}))
                 return(splM)}

### mergen mit Attributen, das kann 'merge()' nicht:
### http://stackoverflow.com/questions/20306853/maintain-attributes-of-data-frame-columns-after-merge
mergeAttr <- function ( x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x",".y"), setAttr = TRUE, onlyVarValLabs = TRUE, homoClass = TRUE) {
     ### erstmal von allen by-variablen die Klassen homogenisieren, falls gewuenscht
             byvars<- data.frame ( x=by.x, y=by.x, clx = sapply(x[,by.x,drop=FALSE], class), cly = sapply(y[,by.y,drop=FALSE], class), stringsAsFactors = FALSE)
             for ( i in 1:nrow(byvars) ) {
                   if ( length(unique(unlist(byvars[i,c("clx", "cly")]))) > 1 ) {
                        cat(paste0("   Merging variable pair '", paste(unlist(byvars[i,c("x", "y")]), collapse = "'<==>'"), "' has different classes: '", paste(unlist(byvars[i,c("clx", "cly")]), collapse = "'<==>'"),"'. Classes will be homogenized to 'character'.\n   Use 'homoClass = FALSE' to depreciate this behavior.\n"))
                        if ( byvars[i,"clx"] != "character" ) { x[, byvars[i,"x"]] <- as.character(x[, byvars[i,"x"]]) }
                        if ( byvars[i,"cly"] != "character" ) { y[, byvars[i,"y"]] <- as.character(y[, byvars[i,"y"]]) }
                   }
             }
     ### jetzt mergen und DANACH die Attribute rekonstruieren
             datM  <- merge ( x=x, y=y, by.x=by.x, by.y=by.y, all=all, all.x=all.x, all.y=all.y, sort=sort, suffixes =suffixes)
             if ( setAttr == TRUE ) {
                   dats<- list(x=x, y=y)
                   for ( d in names(dats)) {
                         for ( v in colnames(dats[[d]])) {
                               vsuf <- paste0(v, suffixes[2])
                               if ( vsuf %in% colnames(datM) ) {
                                    if ( onlyVarValLabs == FALSE ) {
                                         attributes(datM[,vsuf]) <- attributes(dats[[d]][,v])
                                    }  else  {
                                         attr(datM[,vsuf], "varLabel") <- attr(dats[[d]][,v], "varLabel")
                                         attr(datM[,vsuf], "valLabel") <- attr(dats[[d]][,v], "valLabel")
                                    }
                               }  else  {
                                    if ( v %in% colnames(datM) ) {
                                         if ( onlyVarValLabs == FALSE ) {
                                              attributes(datM[,v]) <- attributes(dats[[d]][,v])
                                         }  else  {
                                              attr(datM[,v], "varLabel") <- attr(dats[[d]][,v], "varLabel")
                                              attr(datM[,v], "valLabel") <- attr(dats[[d]][,v], "valLabel")
                                         }
                                    }
                               }
                         }
                   }
             }
             return(datM)}

multiseq <- function ( v ) {
		names ( v ) <- seq ( along = v )
		s <- mapply ( function ( vv , v ) {
								vvv <- v[v==vv]
								s <- seq ( along = vvv )
								names ( s ) <- names( vvv )
								return ( s )
					} , unique ( v ) , MoreArgs = list ( v ) , USE.NAMES = FALSE , SIMPLIFY = FALSE )
		s <- do.call ( "c" , s )
		s <- unname ( s [ match ( names ( v ) , names ( s ) ) ] )
		return(s)
}

### Voraussetzung: Datensatz im Long-Format, wie ihn das Codiertool ausgibt
### idCol: Nummer oder Name der ID
### varCol: Nummer oder Name der Variablenspalte
### codCol: Nummer oder Name der Kodiererspalte
### n.pseudo: wieviele Pseudocodierer?
### randomize.order: soll die Reihenfolge der Pseudocodes nach Zufall bestimmt werden?
make.pseudo <- function(datLong, idCol, varCol, codCol, valueCol, n.pseudo, randomize.order = TRUE)   {
               allVars     <- list(idCol = idCol, varCol = varCol, codCol = codCol, valueCol=valueCol)
               all.Names   <- lapply(allVars, FUN=function(ii) {existsBackgroundVariables(dat = datLong, variable=ii)})
               if(length(all.Names) != length(unique(all.Names)) ) {stop("'idCol', 'varCol', 'codCol' and 'valueCol' overlap.\n")}
               dat.i       <- datLong[,unlist(all.Names), drop = FALSE]         ### untere zeilen "only for the sake of speed": wir sortieren alle Faelle VORHER aus, wo nichts gesampelt werden kann!
               dat.i[,"index"] <- paste( dat.i[,unlist(all.Names[c("idCol")])], dat.i[,unlist(all.Names[c("varCol")])], sep="_")
               index       <- table(dat.i[,"index"])
               datWeg      <- dat.i[ which ( dat.i[,"index"] %in% names(index)[which ( index <= n.pseudo )] ) , ]
               datSample   <- dat.i[ which ( dat.i[,"index"] %in% names(index)[which ( index > n.pseudo )] ) , ]
               if(nrow(datSample)>0) {
                  datPseudo   <- do.call("rbind", by(data = datSample, INDICES = datSample[, unlist(all.Names[c("idCol", "varCol")])], FUN = function ( sub.dat) {
                                 stopifnot(nrow(sub.dat)>n.pseudo)
                                 auswahl     <- sample(1:nrow(sub.dat), n.pseudo, replace = FALSE )
                                 if(!randomize.order) { auswahl <- sort(auswahl) }
                                 sub.dat     <- sub.dat[auswahl,]
                                 return(sub.dat)}))
                  datWeg   <- rbind(datWeg, datPseudo)
               }
               if(n.pseudo>1) { datWeg[,unlist(all.Names[c("codCol")])] <- paste("Cod",multiseq(datWeg[,"index"]),sep="_") }
               return(datWeg)}

### function by Alexander Robitzsch calculates mean agreement among raters
mean.agree <- function( dat , tolerance = 0 , weight.mean = TRUE ){
    # INPUT:
    # dat       ... dataframe
    # tolerance ... see function agree
    # weight.mean ... = T, if agreement is weighted by number of rater subjects,
    #            = F, if it is averaged among all rater pairs
    R <- ncol(dat)
    dfr <- NULL
    for (ii in 1:(R-1)){
        for (jj in (ii+1):R){
        dat.ii.jj <- na.omit(  dat[ , c(ii,jj)]  )
        if ( dim(dat.ii.jj)[1] > 0 ){
            a.ii.jj <-  agree( dat.ii.jj , tolerance )
            dfr <- rbind( dfr , c( colnames(dat)[ii] , colnames(dat)[jj] , a.ii.jj$subjects , a.ii.jj$value / 100 )  )
                    }
            } }
    dfr <- data.frame(dfr)
    colnames(dfr) <- c("Coder1","Coder2","N" , "agree" )
    for (vv in 3:4){ dfr[,vv] <- as.numeric( paste( dfr[,vv] ) ) }
    meanagree <- ifelse( weight.mean == TRUE , weighted.mean( dfr$agree , dfr$N ) , mean( dfr$agree ) )
    list( "agree.pairwise" = dfr , "meanagree" = meanagree )   }

### function by Alexander Robitzsch calculates mean Cohen's kappa among raters
mean.kappa <- function( dat , weight = "unweighted" , weight.mean = TRUE ){
    # INPUT:
    # dat       ... dataframe
    # weight    ... see function kappa2 in irr
    # weight.mean ... = T, if agreement is weighted by number of rater subjects,
    #            = F, if it is averaged among all rater pairs
    R <- ncol(dat)
    dfr <- NULL
    for (ii in 1:(R-1)){
        for (jj in (ii+1):R){
        dat.ii.jj <- na.omit(  dat[ , c(ii,jj)]  )
        if ( dim(dat.ii.jj)[1] > 0 ){
            a.ii.jj <-  kappa2( dat.ii.jj , weight )
            dfr <- rbind( dfr , c( colnames(dat)[ii] , colnames(dat)[jj] , a.ii.jj$subjects , a.ii.jj$value  ) )
                    }
            } }
    dfr <- data.frame(dfr)
    colnames(dfr) <- c("Coder1","Coder2","N" , "kappa" )
    for (vv in 3:4){ dfr[,vv] <- as.numeric( paste( dfr[,vv] ) ) }
    meankappa <- ifelse( weight == TRUE , weighted.mean( dfr$kappa , dfr$N ) , mean( dfr$kappa , na.rm = TRUE) )
    list( "agree.pairwise" = dfr , "meankappa" = meankappa )     }

aggregate.data <- function(all.daten,spalten, unexpected.pattern.as.na = TRUE, verboseAll = FALSE ) {
                   if(missing(spalten)) {spalten <- colnames(all.daten)} else {spalten <- colnames(all.daten[,spalten,drop=FALSE])}
                   noAgg <- setdiff(colnames(all.daten), spalten)
                   daten <- all.daten[,spalten, drop=FALSE]
                   foo   <- table(nchar(colnames(daten)))                       ### Haben alle Variablennamen die gleiche Anzahl Zeichen?
                   if(length(foo)>1) {cat("Variable names with mixed numbers of characters.\n")}
                   items     <- unique(substr(colnames(daten),1,nchar(colnames(daten))-1))                       ### wieviele Items wird es geben?
                   cat(paste("Aggregate ",ncol(daten)," variable(s) to ",length(items)," item(s).\n",sep="")); flush.console()
                   dat.sum <- NULL; dat.agg <- NULL; list.pc <- NULL            ### erstelle leere Datenobjekte fuer Summendatensatz, aggregierten Datensatz und Liste mit partial-credit-Items
                   for (i in 1:length(items))      {
                        sub.dat      <- data.frame ( lapply( data.frame(daten[, which(substr(colnames(daten),1,nchar(colnames(daten))-1) %in% items[i]), drop=FALSE ], stringsAsFactors = FALSE), as.numeric), stringsAsFactors = FALSE)
                        ncol.sub.dat <- ncol(sub.dat)
                        last.sign    <- names(table(substr(colnames(sub.dat),nchar(colnames(sub.dat)),nchar(colnames(sub.dat)))))
                        toCheck      <- sum((last.sign)==letters[1:length(last.sign)])==length(last.sign)
                        ### Check: Ist das letzte Zeichen des Variablennamens immer ein Buchstabe und aufsteigend?
                        if(!toCheck) { cat(paste("Item ",items[i],": last character of variable names does not correspond to assumed alphabetic sequence.\n", sep="")); flush.console()}
                        isNA         <- table(rowSums(is.na(sub.dat)))
                        isNA.names   <- as.numeric(names(isNA))
                        unexpected   <- setdiff(isNA.names, c(0,ncol.sub.dat))
                        if( length( unexpected ) > 0  )   {
                          cases      <- sum(as.numeric(isNA[as.character(unexpected)]))
                          cat(paste("Caution! Found unexpected missing pattern in variables for item ",items[i], " in ",cases," cases.\n", sep= "" ) ) ; flush.console()
                          whichUnexp <- which( rowSums(is.na(sub.dat)) %in% unexpected)
                          if (verboseAll == TRUE) {cat("   Cases in question: "); cat(paste(whichUnexp, collapse=", ")); cat("\n")}
                        }
                        if(ncol.sub.dat == 1) {sub.dat[,"summe"] <- sub.dat[,1]}
                        if(ncol.sub.dat >  1) {sub.dat[,"summe"] <- apply(sub.dat, 1, FUN = function ( uu ) {ifelse( all(is.na(uu)), NA, sum(uu, na.rm=!unexpected.pattern.as.na))}) }
                        sub.dat[,"aggregiert"] <- ifelse(sub.dat$summe == ncol.sub.dat,1,0)
                        if(is.null(dat.sum)) { dat.sum <- sub.dat[,"summe", drop=FALSE] }     else { dat.sum <- cbind(dat.sum,sub.dat[,"summe", drop=FALSE]) }
                        if(is.null(dat.agg)) { dat.agg <- sub.dat[,"aggregiert",drop=FALSE] } else { dat.agg <- cbind(dat.agg,sub.dat[,"aggregiert",drop=FALSE]) }
                        colnames(dat.sum)[i] <- items[i]
                        colnames(dat.agg)[i] <- items[i]
                        maximum <- max(dat.sum[,i],na.rm = TRUE)                ### ist das i-te Item partial credit?
                        if(maximum>1) {list.pc <- rbind(list.pc, data.frame(Var=items[i],pc=paste(names(table(dat.sum[,i])),collapse=", "),max=max(as.numeric(names(table(dat.sum[,i])))),stringsAsFactors = FALSE))}}
                   if(length(noAgg) > 0) {dat.sum <- data.frame(all.daten[,noAgg, drop=FALSE],dat.sum,stringsAsFactors = FALSE)
                                          dat.agg <- data.frame(all.daten[,noAgg, drop=FALSE],dat.agg,stringsAsFactors = FALSE)}
                   return(list(sum=dat.sum, agg=dat.agg, pc.list=list.pc))}

item.logit <- function(z,slope=1,thr)  {
     n <- length(z)
     k <- length(thr)
     m.zts <- matrix(rep(z*slope,k),ncol=k)
     m.thr <- matrix(rep(thr*slope,n),ncol=k,byrow=T)
     cum.prob  <- 1.0 / (1.0 + exp(-(m.zts - m.thr)))
     u <- matrix(rep(runif(n),k),ncol=k)
     x <- apply(u < cum.prob,1,sum)
     des <- cbind(-1*diag(k),rep(0,k))+cbind(rep(0,k),1*diag(k))
     prob <- cum.prob %*% des
     prob[,1] <- 1 + prob[,1]
     colnames(prob) <- c(paste("p",1:(k+1),sep=""))
     return(list(x=x,psc=prob))}

### 16. April 2012
### wandelt kontinuierliche in kategorielle Variable um (eine der aeltesten Operationen in R)
num.to.cat <- function(x, cut.points, cat.values = NULL)    {
              stopifnot(is.numeric(x))
              if(is.null(cat.values)) {cat.values <- 1:(length(cut.points)+1)}
              stopifnot(length(cut.points)+1 == length(cat.values))
              isNa<- which ( is.na ( x ) )
              ret <- rep ( cat.values[1], times = length(x))
              for ( a in 1:length(cut.points) ) {
                   crit <- which(x > cut.points[a])
                   if ( length(crit) > 0 ) { ret[crit] <- cat.values[a+1] }
              }
              if ( length ( isNa ) > 0 ) { ret[isNa] <- NA }
              attr(ret, "cat.values") <- cat.values
              return(ret)}

### anchor            ... data frame with anchor parameters. first column is item identifier, second column parameter value
### mRef              ... numeric value: mean of the reference population
### sdRef             ... numeric value: standard deviation of the reference population
### addConst          ... additive constant for transformation
### multConst         ... multiplicative constant for transformation
### cutScores         ... list of two elements. "values" is a numeric vector of cut scores (increasing),
###                       "labels" is an optional character vector of cut score labels
###                       "labels" has be be of length(values)+1
simEquiTable <- function ( anchor, mRef, sdRef, addConst = 500, multConst = 100, cutScores , dir , n = 2000, conquest.folder ) {
                if ( length(which ( duplicated(anchor[,1])))>0) {
                     cat(paste("Warning: Remove ",length(duplicated(anchor[,1]))," entries in the anchor parameter frame.\n",sep=""))
                     anchor <- anchor[!duplicated(anchor[,1]),]
                }
                it  <- matrix ( data = anchor[,2], ncol = length(anchor[,2]), nrow = n, byrow = TRUE)
                colnames(it) <- anchor[,1]
                pop <- melt ( data.frame ( idstud = paste("P",1:n,sep=""), theta = rnorm (n = n, mean = mRef, sd = sdRef), it), id.vars = c("idstud", "theta"), value.name = "itemPar", variable.name = "item")
                pop[,"resp"] <- item.logit(z = pop[,"theta"]-pop[,"itemPar"], thr = c(0.0), slope = 1)$x
                popW<- dcast(pop,idstud~item, value.var = "resp")
                mDef<- defineModel ( dat = popW, items = -1, id = "idstud", anchor = anchor, dir = dir, conquest.folder = conquest.folder, compute.fit = FALSE, analysis.name = "equSimTest")
                mRun<- runModel(mDef)
                equ <- get.equ ( file.path ( dir, "equSimTest.equ"))[[1]]
                equ[,"estBista"] <- (equ[,"Estimate"] - mRef) / sdRef * multConst + addConst
                equ[,"ks"]       <- num.to.cat ( x = equ[,"estBista"], cut.points = cutScores[["values"]], cat.values = cutScores[["labels"]])
    ### jetzt noch die shortversion der Aequivalenztabelle erzeugen
                shrt<- do.call("rbind", by ( data = equ, INDICES = equ[,"ks"], FUN = function ( sks ) {
                       sks1<- data.frame ( do.call("cbind", lapply ( setdiff ( colnames(sks), "std.error"), FUN = function ( col ) {
                              if ( length( unique ( sks[,col] )) > 1) {
    ### hier werden spaltenspezifisch die Nachkommastellen bestimmt, auf die gerundet werden soll
                                   dig <- as.numeric(recode ( col, "'Score'=0; 'Estimate'=2; 'estBista'=0"))
                                   ret <- paste ( round(min(sks[,col]), digits = dig), round(max(sks[,col]), digits = dig), sep=" bis ")
                              }  else  {
                                   ret <- unique ( sks[,col] )
                              }
                              return(ret)})) )
                       colnames(sks1) <- setdiff ( colnames(sks), "std.error")
                       return(sks1)}))
                return(list ( complete = equ, short = shrt))}
### Test:
### ret <- simEquiTable( anchor = data.frame ( item = paste("i",1:20,sep=""), par = rnorm(20, mean = -.1, sd = 1.5)), mRef = -0.05, sdRef = 0.9, cutScores = list ( values = 330+0:4*75, labels = c("1a", "1b", 2:5) ), dir = "c:/users/weirichs/test", conquest.folder = "N:/console_Feb2007.exe")

getResults <- function ( runModelObj, overwrite = FALSE, Q3 = TRUE, q3theta = c("pv", "wle", "eap"), q3MinObs = 0, q3MinType = c("singleObs", "marginalSum"), omitFit = FALSE, omitRegr = FALSE, omitWle = FALSE, omitPV = FALSE, abs.dif.bound = 0.6, sig.dif.bound = 0.3, p.value = 0.9,
              pvMethod = c("regular", "bayesian"), nplausible = NULL, ntheta = 2000, normal.approx = FALSE, samp.regr = FALSE, theta.model=FALSE, np.adj=8, group = NULL, beta_groups = TRUE, level = .95, n.iter = 1000, n.burnin = 500, adj_MH = .5, adj_change_MH = .05, refresh_MH = 50, accrate_bound_MH = c(.45, .55),	print_iter = 20, verbose = TRUE) {
            pvMethod <- match.arg(pvMethod)
            q3MinType<- match.arg(q3MinType)
            q3theta  <- match.arg(q3theta )
            if("runMultiple" %in% class(runModelObj)) {                         ### Mehrmodellfall
                if(is.null ( attr(runModelObj, "split")[["nCores"]] ) || attr(runModelObj, "split")[["nCores"]] == 1 ) {
                   res <- lapply( runModelObj, FUN = function ( r ) {           ### erstmal single core auswertung
                          do  <- paste ( "ret <- getResults ( ", paste(names(formals(getResults)), recode(names(formals(getResults)), "'runModelObj'='r'"), sep =" = ", collapse = ", "), ")",sep="")
                          eval(parse(text=do))
                          return(ret)})
                   }  else  {
                          # if(!exists("detectCores"))   {library(parallel)}    ### jetzt multicore: muss dasselbe Objekt zurueckgeben!
                          doIt<- function (laufnummer,  ... ) {
                                 if(!exists("getResults"))  { library(eatModel) }
                                 if(!exists("tam.mml") &  length(grep("tam.", class(runModelObj[[1]])))>0 ) {library(TAM, quietly = TRUE)}
                                 do    <- paste ( "ret <- getResults ( ", paste(names(formals(getResults)), recode(names(formals(getResults)), "'runModelObj'='runModelObj[[laufnummer]]'"), sep =" = ", collapse = ", "), ")",sep="")
                                 eval(parse(text=do))
                                 return(ret)}
                          beg <- Sys.time()
                          if ( attr(runModelObj, "split")[["mcPackage"]] == "parallel") {
                               cl  <- makeCluster(attr(runModelObj, "split")[["nCores"]], type = "SOCK")
                          }  else  {
                               cl  <- makeClusterPSOCK(attr(runModelObj, "split")[["nCores"]], verbose=FALSE)
                          }
                          res <- clusterApply(cl = cl, x = 1:length(runModelObj), fun = doIt , overwrite = overwrite, omitFit = omitFit, omitRegr = omitRegr, omitWle = omitWle, omitPV = omitPV, abs.dif.bound = abs.dif.bound, sig.dif.bound = sig.dif.bound, p.value = p.value)
                          stopCluster(cl)
                          cat(paste ( "Results of ",length(runModelObj), " analyses processed: ", sep="")); print( Sys.time() - beg)
                   }
               res <- do.call("rbind", res )
               class(res) <- c("data.frame", "multipleResults")
               rownames(res) <- NULL
               return(res)
     ### hier ist der rekursive Aufruf beendet: das folgende geschieht fuer jedes Modell einzeln, technisch auf zweierlei Weisen, je nachdem ob Conquest oder TAM gerechnet wurde
            }  else {                                                           ### Einmodellfall
               isTa  <- FALSE
               if( "runConquest" %in% class(runModelObj) ) {                    ### wurde mit Conquest gerechnet?
                    if ( Q3 == TRUE ) {
                        if ( ncol ( runModelObj[["qMatrix"]]) !=2 ) {
                            cat("Q3 is only available for unidimensional models. Estimation will be skipped.\n")
                            Q3 <- FALSE
                        }
                    }
                    do    <- paste ( "res <- getConquestResults ( ", paste(names(formals(getConquestResults)), recode(names(formals(getConquestResults)), "'path'='runModelObj$dir'; 'analysis.name'='runModelObj$analysis.name'; 'model.name'='runModelObj$model.name'; 'qMatrix'='runModelObj$qMatrix'; 'all.Names'='runModelObj$all.Names'; 'deskRes'='runModelObj$deskRes'; 'discrim'='runModelObj$discrim'; 'daten'='runModelObj$daten'"), sep =" = ", collapse = ", "), ")",sep="")
                    eval(parse(text=do))                                        ### obere Zeile: baue Aufruf zusammen; rufe 'getConquestResults' mit seinen eigenen Argumenten auf
                    dir <- runModelObj[["dir"]]                                 ### wo Argumente neu vergeben werden, geschieht das in dem 'recode'-Befehl; so wird als 'path'-
                    name<- runModelObj[["analysis.name"]]                       ### Argument 'runModelObj$dir' uebergeben
                    allN<- runModelObj[["all.Names"]]                           ### Alternativ: es wurde mit TAM gerechnet
               }  else  {                                                       ### logisches Argument: wurde mit Tam gerechnet?
                    isTa<- TRUE                                                 ### hier wird ggf. die Anzahl der zu ziehenden PVs ueberschrieben
                    if ( Q3 == TRUE ) {
                        if ( ncol ( attr(runModelObj, "qMatrix")) !=2 ) {
                            cat("Q3 is only available for unidimensional models. Estimation will be skipped.\n")
                            Q3 <- FALSE
                        }
                    }
                    if(!is.null(nplausible)) { attr(runModelObj, "n.plausible") <- nplausible }  else  { nplausible <- attr(runModelObj, "n.plausible") }
                    do    <- paste ( "res <- getTamResults ( ", paste(names(formals(getTamResults)), names(formals(getTamResults)), sep =" = ", collapse = ", "), ")",sep="")
                    eval(parse(text=do))
                    dir <- attr(runModelObj, "dir")                             ### untere zeilen(n): tam summary ergaenzen, wenn mit tam gerechnet wurde
                    name<- attr(runModelObj, "analysis.name")                   ### wird als Attribut in Ergebnisstruktur angehangen (nicht so superclever;
                    allN<- attr(runModelObj, "all.Names")                       ### schoener waers, man wuerde das direkt in die Ergebnisstruktur einbauen)
               }
               if(!is.null(res)) {                                              ### wenn es ein Rueckgabeobjekt gibt, wird das jetzt um einige technische Eintraege erweitert
                    stopifnot ( length(unique(res[,"model"])) == 1)             ### (das was frueher ueber Attribute gemacht wurde, aber das ist zu krass schlimm beschissen scheiss untransparent, das muss weg!!)
     ### Rueckgabeobjekt mit technischen Parametern 'anreichern', first: 'all.Names'
                    alln<- do.call("rbind", lapply(names(allN), FUN = function ( x ) {
                           if ( length( allN[[x]] ) > 0 ) {
                                res <- data.frame ( type = "tech", par = x, derived.par = allN[[x]])
                           }  else  {
                                res <- NULL
                           }
                           return(res)}))
                    res <- rbind.fill ( res, data.frame ( res[1,c("model", "source")], alln, stringsAsFactors = FALSE) )
     ### Rueckgabeobjekt mit technischen Parametern 'anreichern', second: 'dif.setting'
                    difS<- list (abs.dif.bound = abs.dif.bound, sig.dif.bound = sig.dif.bound, p.value = p.value)
                    resD<- data.frame ( res[1,c("model", "source")], type = "tech", par = "dif", derived.par = names(difS), value = unlist(difS), stringsAsFactors = FALSE)
                    res <- rbind.fill ( res, resD )
     ### jetzt wird der ganze scheiss bei Bedarf (wenn der user das will) noch auf der Festplatte gespeichert
                    id  <- unique(res[intersect(which(res[,"type"] == "tech"), which(res[,"par"] == "ID")),"derived.par"])
                   if(!is.null(dir)) {
                        stopifnot(length(id)==1)
                        item<-itemFromRes ( res )
                        if ( file.exists(file.path(dir, paste(name, "_items.csv",sep=""))) & overwrite == FALSE) {
                             cat(paste("Item results cannot be saved, file '",  file.path(dir, paste(name, "_items.csv",sep="")),"' already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                        }  else  {
                             write.csv2(item, file.path(dir, paste(name, "_items.csv",sep="")), na="", row.names = FALSE)
                        }                                                       ### untere Zeilen: speichere wunschgemaess alle Personenparameter in einer Tabelle im Wideformat
                        txt <- capture.output ( wle <- wleFromRes(res) )        ### 'capture.output' wird benutzt um Warnungen in wleFromRes() zu unterdruecken
                        if (!is.null ( wle ) ) {
                             wleL<- melt ( wle, id.vars = c(id, "dimension"), measure.vars = c("wle_est", "wle_se"), na.rm = TRUE)
                             form<- as.formula ( paste ( id, "~dimension+variable",sep=""))
                             wleW<- dcast ( wleL, form, value.var = "value" )
                        }
                        txt <- capture.output ( pv  <- pvFromRes(res) )
                        if(!is.null(pv)) {
                             pvL <- melt ( pv, id.vars = c( id , "dimension"), na.rm = TRUE)
                             form<- as.formula ( paste ( id, "~dimension+variable",sep=""))
                             pvW <- dcast ( pvL, form, value.var = "value" )
                        }
                        txt <- capture.output ( eap <- eapFromRes(res) )
                        if(!is.null(eap)) {
                             eapL<- melt ( eap, id.vars = c(id, "dimension"), measure.vars = c("EAP", "SE.EAP"), na.rm = TRUE)
                             form<- as.formula ( paste ( id, "~dimension+variable",sep=""))
                             eapW<- dcast ( eapL, form, value.var = "value" )
                        }                                                       ### Hier wird geprueft, welche Personenparameter vorliegen
                        alls<- list ( wle, pv, eap )                            ### wenn es Personenparameter gibt, werden sie eingelesen
                        allP<- NULL                                             ### alle vorhandenen Personenparameter werden zum Speichern in einen gemeinsamen Dataframe gemergt
                        notN<- which ( unlist(lapply ( alls, FUN = function ( x ) { !is.null(x)})) )
                        if ( length( notN ) >= 1 ) { allP <- alls[[notN[1]]] }
                        if ( length( notN ) > 1 )  {
                             for ( u in notN[-1] )   {
                                   allP <- merge ( allP, alls[[u]], by = c ( id, "dimension"), all = TRUE)
                             }
                        }
                        if ( !is.null(allP)) {
                              if ( file.exists(file.path(dir, paste(name, "_persons.csv",sep=""))) & overwrite == FALSE) {
                                   cat(paste("Person estimates cannot be saved, file '",  file.path(dir, paste(name, "_persons.csv",sep="")),"' already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                              }  else  {
                                   write.csv2(allP, file.path(dir, paste(name, "_persons.csv",sep="")), na="", row.names = FALSE)
                              }
                        }
                        if ( Q3 == TRUE ) {
                              q3m <- q3FromRes ( res )                          ### q3FromRes liest so viele q3-tabellen aus, wie es gibt, hier will ich nur die erste
                              stopifnot(length(q3m)==1)
                              q3m <- q3m[[1]]
                              if ( file.exists(file.path(dir, paste(name, "_q3.csv",sep=""))) & overwrite == FALSE) {
                                   cat(paste("Item results cannot be saved, file '",  file.path(dir, paste(name, "_q3.csv",sep="")),"' already exists.\n    Please remove/rename existing file or use 'overwrite=TRUE'.\n",sep=""))
                              }  else  {
                                   write.csv2(q3m, file.path(dir, paste(name, "_q3.csv",sep="")), na="", row.names = FALSE)
                              }
                        }
                   }
               }
               rownames(res) <- NULL
               return(res)
               }}

equat1pl<- function ( results , prmNorm , item = NULL, domain = NULL, testlet = NULL, value = NULL, excludeLinkingDif = TRUE, difBound = 1, iterativ = FALSE, method = c("Mean.Mean", "Haebara", "Stocking.Lord"),
           itemF = NULL, domainF = NULL, valueF = NULL) {
           method<- match.arg(method)
    ### Achtung! Funktion kann (was der default ist) sowohl Output aus 'runModel' weiterverarbeiten oder einfach nur zum Equaten zweiter verschiedener Itemparameterlisten genutzt werden
    ### Also muss die Funktion erstmal rauskriegen, was von beidem gemacht werden soll. Wenn 'isRunM' gleich TRUE, dann default
           isRunM<- all(c("model" , "source" , "var1" , "var2" , "type" , "indicator.group", "group", "par", "derived.par", "value") %in% names(results))
           if ( isRunM == TRUE ) {
                nMods <- table(results[,"model"])
                cat(paste("Found ", length(nMods), " model(s).\n   Equating is executed for each dimension in each model separately.\n",sep=""))
                dims  <- unique(unlist(by ( data = results, INDICES = results[,"model"], FUN = function ( x ) { names(table(as.character(itemFromRes(x)[,"dimension"])))})))
    ### jetzt beginnt der Fall, dass einfach nur zwei Itemparameterlisten equatet werden sollen. Dazu transformiert die Funktion den Input 'results' in das benoetigte Format
           }  else  {
                allF <- list(itemF=itemF, domainF = domainF, valueF = valueF)
                allF <- lapply(allF, FUN=function(ii) {existsBackgroundVariables(dat = results, variable=ii)})
                if (!is.null(allF[["domainF"]])) {
                    dims <- names(table(results[,allF[["domainF"]]]))
                }  else  {
                    allF[["domainF"]] <- "domaene"
                    results[, allF[["domainF"]]] <- dims <- "global"
                }
                nMods<- dims
                results[,"model"] <- results[, allF[["domainF"]]]
                weg  <- intersect ( colnames (results ) , setdiff  ( c("item", "dimension", "est"), unlist(allF) ))
                if ( length ( weg ) > 0 )  {                                    ### damit keine Spalten durch 'recode' doppelt benannt werden,
                     results <- results[, -match(weg, colnames(results))]       ### muessen spalten, die sich durch die Recodierung aendern
                }                                                               ### und zugleich schon im datensatz 'results' vergeben sind, raus
                toRec<- lapply(names(allF), FUN = function ( ff ) { paste ( "'",allF[[ff]],"'='",recode(ff, "'itemF'='item'; 'domainF'='dimension'; 'valueF'='est'"),"'",sep="")})
                toRec<- paste(toRec, collapse = "; ")
                colnames(results) <- recode (colnames(results), toRec)
           }
           if ( missing ( prmNorm) ) {                                          ### kein Equating: Rueckgabe NULL, 'results' wird durchgeschleift
                if ( isRunM == FALSE ) { stop("No norm parameter defined ('prmNorm' is missing).\n")}
                cat("No norm parameter defined ('prmNorm' is missing). Treat current sample as drawn from the reference population.\n")
    ### Kein equating: baue 'leeres' Rueckgabeobjekt
                items <- by ( data = results, INDICES = results[,"model"], FUN = function ( d ) {
                         it  <- itemFromRes(d)
                         if ( "estOffset" %in% colnames ( it ) ) {
                              d[,"par"] <- recode ( d[,"par"], "'offset'='est'")
                              it <- itemFromRes(d)
                         }
                         dimN <- by ( data = it, INDICES = it[,"dimension"], FUN = function ( prmDim ) {
                                 eq <- list(B.est = c(Mean.Mean=0 , Haebara =0, Stocking.Lord=0), descriptives = c(N.Items =0, SD=NA,  Var=NA, linkerror=NA))
                                 return ( list ( eq = eq, items = prmDim, method = method ) ) }, simplify = FALSE)}, simplify = FALSE)
                return(list(items = items, results = results))
    ### Equating: baue 'befuelltes' Rueckgabeobjekt
           }  else {                                                            ### plausibility checks
              stopifnot ( "data.frame" %in% class(prmNorm))
              if ( ncol ( prmNorm ) == 2 ) {
                   if (!length(prmNorm[,1]) == length(unique(prmNorm[,1]))) { stop("Item identifiers are not unique in 'prmNorm'.\n")}
                   if(is.null(item)) { item <- colnames(prmNorm)[1] }
                   if(is.null(value)){ value<- colnames(prmNorm)[2] }
              }  else  {
                   if ( !is.null(item) & !is.null(value) == FALSE ) { stop("If 'prmNorm' has more than two columns, 'item' and 'value' columns must be specified explicitly.\n") }
              }
              allV <- list(item=item, domain = domain, testlet = testlet, value = value)
              allN <- lapply(allV, FUN=function(ii) {existsBackgroundVariables(dat = prmNorm, variable=ii)})
              if (!is.null ( allN[["domain"]] )) {                              ### check: match domain names
                   mis <- setdiff ( dims,  names(table(prmNorm[, allN[["domain"]] ])) )
                   if ( length( mis ) > 0 ) { stop ( paste ( "Domain '",mis,"' is missing in 'prmNorm'.\n",sep="")) }
                   uni <- by ( data = prmNorm, INDICES = prmNorm[, allN[["domain"]] ], FUN = function ( g ) {
                          if (!length(g[,allN[["item"]]]) == length(unique(g[,allN[["item"]]]))) { stop(paste ( "Item identifiers are not unique in 'prmNorm' for domain '",g[1,allN[["domain"]]],"'.\n",sep=""))}
                   }, simplify = FALSE)                                         ### check: items unique within domains?
              }
    ### Fuer jedes Modell und jede Dimension innerhalb jedes Modells findet Equating separat statt
              items <- by ( data = results, INDICES = results[,"model"], FUN = function ( d ) {
                       if ( isRunM == TRUE ) {
                            it  <- itemFromRes(d)
                       }  else  {
                            it  <- d
                       }
                       if ( "estOffset" %in% colnames ( it ) ) {
                            cat(paste("W A R N I N G:  Model '",d[1,"model"],"' was estimated with (at least partially) anchored items parameters. Equating seems questionable.\n",sep=""))
                            d[,"par"] <- recode ( d[,"par"], "'offset'='est'")
                            it <- itemFromRes(d)
                       }
                       dimN <- by ( data = it, INDICES = it[,"dimension"], FUN = function ( prmDim ) {
                               if(!is.null(allN[["domain"]]) ) {
                                   prmM<- prmNorm [ which(prmNorm[,allN[["domain"]]] %in% unique(it[,"dimension"])) ,]
                               }  else  {
                                   prmM<- prmNorm
                               }
    ### items muessen unique sein
                               if ( length(prmDim[, "item"]) != length(unique(prmDim[, "item"])) ) {  stop(paste("Items are not unique for model '",as.character(d[1,"model"]),"'.\n",sep="")) }
                               if ( length(prmM[,allN[["item"]] ]) != length(unique(prmM[,allN[["item"]] ])) ) {  stop(paste("Items are not unique for model '",as.character(d[1,"model"]),"'.\n",sep="")) }
                               eq  <- equAux ( x = prmDim[ ,c("item", "est")], y = prmM[,c(allN[["item"]], allN[["value"]], allN[["testlet"]])] )
                               dif <- eq[["anchor"]][,"TransfItempar.Gr1"] - eq[["anchor"]][,"Itempar.Gr2"]
                               prbl<- which ( abs ( dif ) > difBound )
                               cat(paste("\n",paste(rep("=",100),collapse=""),"\n \nModel No. ",match(d[1,"model"], names(nMods)),"\n    Model name:              ",d[1,"model"],"\n    Number of dimension(s):  ",length(unique(it[,"dimension"])),"\n    Name(s) of dimension(s): ", paste( names(table(as.character(it[,"dimension"]))), collapse = ", "),"\n",sep=""))
                               if  ( length(names(table(as.character(it[,"dimension"])))) > 1) {  cat(paste("    Name of current dimension: ",names(table(prmDim[,"dimension"]))," \n",sep=""))}
                               cat(paste("    Number of linking items: " , eq[["descriptives"]][["N.Items"]],"\n",sep=""))
                               if ( !is.null(allN[["testlet"]]) ) { cat(paste( "    Number of testlets:      ",  eq[["ntl"]],"\n",sep="")) }
                               if ( length( prbl ) > 0 ) {                      ### Gibt es Items mit linking dif?
                                    cat(paste ( "\nDimension '", prmDim[1,"dimension"], "': ", length( prbl), " of ", nrow( eq[["anchor"]]), " items with linking DIF > ",difBound," identified.\n",sep=""))
                                    dskr <- data.frame ( item = eq[["anchor"]][prbl,"item"], dif = dif[prbl], linking.constant = eq[["B.est"]][[method]], linkerror = eq[["descriptives"]][["linkerror"]] )
                                    if ( !excludeLinkingDif) { info<- dskr }
                                    if ( excludeLinkingDif ) {
                                         if ( iterativ == FALSE ) {
                                              cat(paste("   Exclude ",length( prbl), " items.\n",sep=""))
                                              qp1 <- prmM[-match ( dskr[,"item"], prmM[,allN[["item"]]]),]
                                              eq1 <- equAux ( x=prmDim[ ,c("item", "est")], y = qp1[,c(allN[["item"]], allN[["value"]], allN[["testlet"]])] )
                                              info<- data.frame ( method = "nonIterativ", rbind ( data.frame ( itemExcluded = "" , linking.constant = eq[["B.est"]][[method]], linkerror = eq[["descriptives"]][["linkerror"]] ), data.frame ( itemExcluded = paste ( prmM[match ( dskr[,"item"], prmM[,allN[["item"]]]),allN[["item"]]] , collapse = ", "), linking.constant = eq1[["B.est"]][[method]], linkerror = eq1[["descriptives"]][["linkerror"]] ) ))
                                              eq  <- eq1
    ### hier beginnt iterativer Ausschluss von Linking-DIF
                                         }  else  {
                                              info<- data.frame ( method = "iterativ", iter = 0 , itemExcluded = "" , linking.constant = eq[["B.est"]][[method]], linkerror = eq[["descriptives"]][["linkerror"]] )
                                              qp1 <- prmM
                                              iter<- 1
                                              while  ( length ( prbl ) > 0 ) {
                                                  maxD<- which ( abs ( eq[["anchor"]][,"TransfItempar.Gr1"] - eq[["anchor"]][,"Itempar.Gr2"] ) == max ( abs (eq[["anchor"]][,"TransfItempar.Gr1"] - eq[["anchor"]][,"Itempar.Gr2"])) )
                                                  wegI<- eq[["anchor"]][maxD,"item"]
                                                  cat ( paste ( "   Iteration ", iter,": Exclude item '",wegI,"'.\n",sep=""))
                                                  qp1 <- qp1[-match ( wegI, qp1[,allN[["item"]]]),]
                                                  eq  <- equAux ( x = prmDim[ ,c("item", "est")], y = qp1[,c(allN[["item"]], allN[["value"]], allN[["testlet"]])] )
                                                  dif <- eq[["anchor"]][,"TransfItempar.Gr1"] - eq[["anchor"]][,"Itempar.Gr2"]
                                                  prbl<- which ( abs ( dif ) > difBound )
                                                  info<- rbind(info, data.frame ( method = "iterativ", iter = iter , itemExcluded = wegI, linking.constant = round ( eq[["B.est"]][[method]],digits = 3), linkerror = round ( eq[["descriptives"]][["linkerror"]], digits = 3) ))
                                                  iter<- iter + 1
                                              }
                                         }
                                    }
                               }  else  {                                       ### hier folgt der Abschnitt, wenn es keine Linking-Dif Items gibt
                                    info <- data.frame ( linking.constant = eq[["B.est"]][[method]], linkerror = eq[["descriptives"]][["linkerror"]] )
                               }
                               cat("\n")
                               infPrint <- data.frame ( lapply ( info , FUN = function ( x ) {if(class(x) == "numeric") { x <- round(x, digits = 3)}; return(x) }))
                               print(infPrint); flush.console()
                               cat("\n")
                               ret <- list ( eq = eq, items = prmDim, info = info, method = method )
                               return ( ret ) }, simplify =FALSE)
                       return(dimN) }, simplify = FALSE)
              return(list ( items = items, results = results))                  ### "results"-Objekt wird durchgeschleift
              }  }

### Hilfsfunktion fuer equat1pl
equAux  <- function ( x, y ) {
           eq  <- equating.rasch(x = x, y = y[,1:2])                      ### kein Jackknife
           if ( ncol(y)==3) {                                                   ### jackknife
                colnames(x)[1] <- colnames(y)[1] <- "item"
                dfr <- merge( x, y, by = "item", all = FALSE)
                stopifnot ( ncol ( dfr ) == 4 )
                if ( nrow ( dfr ) < 1 ) { stop ( "No common items for linking.\n")}
                txt <- capture.output ( eqJk<- equating.rasch.jackknife(dfr[ , c(4 , 2  , 3 , 1 ) ], display = FALSE ) )
                if(!all ( unlist(lapply(txt, nchar)) == 0  ) ) { cat(txt, sep="\n")}
                eq[["descriptives"]][["linkerror"]] <- eqJk[["descriptives"]][["linkerror.jackknife"]]
                eq[["ntl"]]  <- length(unique(dfr[,4]))
           }
           return(eq)}

### Hilfsfuntion fuer "transformToBista", solange eatrep im Uebergangsstadium ist
adaptEatRepVersion <- function ( x ) {
     if ( "data.frame" %in% class(x) ) {
           return ( x )
     }  else  {
           x <- x[[1]][[1]]
           stopifnot ( "data.frame" %in% class(x) )
           return(x)
     } }
     
transformToBista <- function ( equatingList, refPop, cuts, weights = NULL, defaultM = 500, defaultSD = 100, roman = FALSE ) {
    ### wenn equatet wurde, sollte auch 'refPop' definiert sein (es sei denn, es wurde verankert skaliert)
    ### wenn 'refPop' fehlt, wird es fuer alle gegebenen Dimensionen anhand der Gesamtstichprobe berechnet
       mr  <- FALSE                                                             ### default: 'refPop' fehlt nicht. Wenn doch, wird es aus Daten generiert und spaeter
       if(missing(refPop)) {                                                    ### (nachdem ggf. transformiert wurde!) auf Konsole angezeigt
          mr  <- TRUE
          cat("'refPop' was not defined. Treat current sample as drawn from the reference population.\n")
          flush.console()
       }
       if( missing(cuts)) { cutsMis <- TRUE }  else  { cutsMis <- FALSE }
       nam1<- names(equatingList[["items"]])                                    ### hier stehen in der regel die beiden Modellnamen, also quasi names(table(equatingList[["results"]][,"model"]))
    ### Fuer jedes Modell und jede Dimension findet Transformation separat statt
    ### Schritt 1: 'refPop' bestimmen, wenn es sie noch nicht gibt ... 'refPop' ist ggf. verschieden ueber Itemgruppen, aber immer gleich ueber Personengruppen!
    ### fuer die Bestimmung von 'refPop' wird nur ueber Itemgruppen gesplittet!
       it     <- itemFromRes(equatingList[["results"]])
       dims   <- unique(it[,"dimension"])
    ### Hotfix: 'id'-Variable identifizieren
       id     <- unique(equatingList[["results"]][intersect(which(equatingList[["results"]][,"type"] == "tech"), which(equatingList[["results"]][,"par"] == "ID")),"derived.par"])
       stopifnot(length(id)==1)
       refList<- lapply ( dims, FUN = function (dimname) {
                 rex  <- pvFromRes(equatingList[["results"]][unique(c(which(equatingList[["results"]][,"group"] == dimname),which(equatingList[["results"]][,"type"] == "tech"))), ], toWideFormat = FALSE)
                 if ( is.null(weights) ) {
                      txt <- capture.output ( msd <- jk2.mean ( datL = rex, ID = id, imp = "imp", dependent = "value", na.rm = TRUE))
    ### Achtung!! ggf. anpassen fuer neue eatRep-Version
                      msd <- adaptEatRepVersion(msd)
                 }  else  {
                    if ( !class ( weights ) %in% "data.frame") {
                         cat("'weights' has to be of class 'data.frame'. 'weights' object will be converted.\n")
                         weights <- data.frame ( weights )
                    }
                    nd  <- setdiff ( rex[,id] , weights[,1])
                    if ( length(nd) > 0 ) {
                         stop(paste ( "Plausible values data for dimension '",dimname,"' contain ",length(nd)," cases for which no valid weights exist in the 'weights' frame.\n",sep=""))
                    }
                    rex <- merge ( rex, weights , by.x = id, by.y = colnames(weights)[1], all.x = TRUE, all.y = FALSE)
                    mis <- which(is.na(rex[,colnames(weights)[2]]))
                    if ( length(mis) > 0 ) {                                    ### missings in the weights frame are not allowed
                         cat(paste ( "Found ",length(mis)," missing values in the 'weights' frame.\n    Cases with missing values on weighting variable will be ignored for transformation.\n",sep=""))
                         rex <- rex[-mis,]
                    }
                    txt <- capture.output ( msd <- jk2.mean ( datL = rex, ID = id, imp = "imp", wgt = colnames(weights)[2], dependent = "value", na.rm = TRUE) )
                    msd <- adaptEatRepVersion(msd)
                 }
                 rp <- data.frame ( domain = dimname , m = msd[intersect(which(msd[,"parameter"] == "mean"), which(msd[,"coefficient"] == "est")),"value"], sd = msd[intersect(which(msd[,"parameter"] == "sd"), which(msd[,"coefficient"] == "est")),"value"])
                 return(list (msd = msd , rp=rp))})
       names(refList) <- dims
    ### wenn 'refPop' nicht definiert wurde, wird es hier mit Werten gesetzt, die direkt aus der Stichprobe (= Normpopulation) berechnet wurden
       ref    <- do.call("rbind", lapply(refList, FUN = function ( u ) { u[["rp"]] }))
       if ( mr == TRUE ) {
          refPop <- ref
       }   else  {
    ### wenn 'refPop' NUR FUER EINE DIMENSION nicht definiert wurde, werden hier die nicht definierten ('NA') Werte durch Werte aus der Stichprobe (= Normpopulation fuer genau diese Dimension) ersetzt
          mis <- which(is.na(refPop))
          if ( length(mis) >0) {
               stopifnot ( nrow(ref ) == nrow(refPop))
               mat <- merge( 1:nrow(refPop), 1:ncol(refPop), by = NULL)         ### rauskriegen, in welchen Zeilen und Spalten die werte fehlen
               refPop[unique(mat[mis,"x"]), unique(mat[mis,"y"])] <- ref[unique(mat[mis,"x"]), unique(mat[mis,"y"])]
          }
       }
       if(ncol ( refPop ) == 3) {
          cat ( paste("The 'refPop' data.frame does not include information about reference population mean/SD on Bista metric. Values will be defaulted to ",defaultM,"/",defaultSD,".\n",sep=""))
          refPop[,4] <- defaultM; refPop[,5] <- defaultSD
       }  else  {
          if ( ncol ( refPop) != 5 ) { stop ( "Invalid 'refPop'.\n") }
       }
    ### fuer die Transformation selbst wird nach Modellen und Dimensionen getrennt
       modN<- lapply(nam1, FUN = function ( mod ) {                             ### aeussere Schleife: geht ueber modelle
              nam2 <- names(equatingList[["items"]][[mod]])
              dimN <- lapply(nam2, FUN = function ( dims ) {                    ### innere Schleife: geht ueber Dimensionen (innerhalb von modellen)
    ### check: sind Personen innerhalb jeder Dimension (und jeder Imputation) unique? ... 'redMD' ist ein reduziertes Results-Objekt: nur die interessierende Dimension des interessierenden Modells + saemtliche "tech"-Variablen
                      resMD<- equatingList[["results"]][unique(c(intersect(which(equatingList[["results"]][,"model"] == mod), which(equatingList[["results"]][,"group"] == dims)),  which(equatingList[["results"]][,"type"] == "tech"))),]
                      rex  <- pvFromRes(resMD, toWideFormat = TRUE)
                      if ( length ( rex[,id]) != unique(length ( rex[,id])) ) {
                           stop(paste( "Model '",mod,"', Dimension '",dims,"': cases according to '", id,"' variable are not unique.\n",sep=""))
                      }
    ### check: keine verankerten parameter?
                      offSet  <- grep("offset", as.character(resMD[,"par"]))
                      if(length(offSet)>0) {  resMD[,"par"] <- recode ( resMD[,"par"], "'offset'='est'") }
                      itFrame <- itemFromRes(resMD)
                      if ( !itFrame[1,"dimension"] %in% refPop[,1] ) {
                            cat(paste("Cannot found dimension '",itFrame[1,"dimension"],"' in the first column of the 'refPop' argument. Skip transformation ... \n",sep=""))
                            return ( list ( itempars = NULL, personpars = NULL, rp = NULL))
                      }  else  {
                            if ( cutsMis == FALSE ) {
                                 if ( !itFrame[1,"dimension"] %in% names(cuts) ) { stop(paste("Cannot found dimension '",itFrame[1,"dimension"],"' in the 'cuts' list.\n",sep=""))}
                                 mat1<- match( itFrame[1,"dimension"], names(cuts))
                            }
                            mat <- match( itFrame[1,"dimension"], refPop[,1])
    ### 1. Transformation fuer Itemparameter
                            itFrame[,"estTransf"] <- itFrame[,"est"] + equatingList[["items"]][[mod]][[dims]][["eq"]][["B.est"]][[ equatingList[["items"]][[mod]][[dims]][["method"]] ]]
    ### Achtung, heikel: wenn equatet wurde, aber der Datensatz aus der Normpopulation kommt, werden hier die empirischen Mittelwerte,
    ### die oben (mit oder ohne Gewichte) berechnet wurden, nochmal transformiert ... sollte praktisch nie der Fall sein.
                            if ( mr == TRUE ) {
                                 if ( equatingList[["items"]][[mod]][[dims]][["eq"]][["B.est"]][[ equatingList[["items"]][[mod]][[dims]][["method"]] ]] != 0) {
                                      cat("W A R N I N G: Preceding Equating without 'refPop' definition. Sure you want to use current sample as drawn from the reference population?\n")
                                      refPop[mat,2] <- refPop[mat,2]+ equatingList[["items"]][[mod]][[dims]][["eq"]][["B.est"]][[ equatingList[["items"]][[mod]][[dims]][["method"]] ]]
                                 }
                            }
                            itFrame[,"estTransf625"]   <- itFrame[,"estTransf"] + log(0.625/(1-0.625))
                            itFrame[,"estTransfBista"] <- (itFrame[,"estTransf625"] - refPop[mat,2]) / refPop[mat,3] * refPop[mat,5] + refPop[mat,4]
                            if ( cutsMis == FALSE ) {
    ### Achtung: dieser Umweg ist notwendig, weil 'num.to.cat' Attribute ausgibt die unten wieder gebraucht werden!
                                 traitLevel            <- num.to.cat(x = itFrame[,"estTransfBista"], cut.points = cuts[[mat1]][["values"]], cat.values = cuts[[mat1]][["labels"]])
                                 itFrame[,"traitLevel"]<- traitLevel
                            }
                            itFrame[,"linkingConstant"]<- equatingList[["items"]][[mod]][[dims]][["eq"]][["B.est"]][[ equatingList[["items"]][[mod]][[dims]][["method"]] ]]
                            itFrame[,"linkingMethod"]  <- equatingList[["items"]][[mod]][[dims]][["method"]]
                            itFrame[,"nLinkitems"]     <- equatingList[["items"]][[mod]][[dims]][["eq"]][["descriptives"]][["N.Items"]]
                            itFrame[,"linkingError"]   <- equatingList[["items"]][[mod]][[dims]][["eq"]][["descriptives"]][["linkerror"]]
    ### Transformation des Linkingfehlers entsprechend der Rechenregeln fuer Varianzen. ist geprueft, dass dasselbe rauskommt, wie wenn man Parameter transformiert und dann Linkingfehler bestimmt
                            itFrame[,"linkingErrorTransfBista"] <- ( (itFrame[,"linkingError"]^2) * (refPop[mat,5]^2) / (refPop[mat,3]^2) )^0.5
    ### Deltamethode, wie in eatTrend (Funktion 'seKompstuf'). Dazu wird MW und SD der Fokuspopulation benoetigt! (wurde oben als 'msd' berechnet)
    ### das ganze findet nur statt, wenn sowohl cut scores bereits definiert sind und wenn equatet wurde (denn nur dann gibt es einen Linkingfehler, den man transformieren kann)
                            pv  <- pvFromRes(resMD, toWideFormat = FALSE)
                            equ <- equatingList[["items"]][[mod]][[dims]][["eq"]][["B.est"]][[ equatingList[["items"]][[mod]][[dims]][["method"]] ]]
                            pv[,"valueTransfBista"] <- (pv[,"value"] + equ - refPop[mat,2]) / refPop[mat,3] * refPop[mat,5] + refPop[mat,4]
    ### Dazu muss zuerst Mittelwert und SD der Fokuspopulation bestimmt werden.
                            if ( is.null(weights) ) {
                                 txt <- capture.output ( msdF <- jk2.mean ( datL = pv, ID = id, imp = "imp", dependent = "valueTransfBista", na.rm = TRUE))
                                 msdF<- adaptEatRepVersion(msdF)
                            }  else  {
                                 if ( !class ( weights ) %in% "data.frame") {
                                      cat("'weights' has to be of class 'data.frame'. 'weights' object will be converted.\n")
                                      weights <- data.frame ( weights )
                                 }
                                 nd  <- setdiff ( pv[,id] , weights[,1])
                                 if ( length(nd) > 0 ) {
                                      stop(paste ( "Plausible values data for dimension '",dims,"' contain ",length(nd)," cases for which no valid weights exist in the 'weights' frame.\n",sep=""))
                                 }
                                 pvF <- merge ( pv, weights , by.x = id, by.y = colnames(weights)[1], all.x = TRUE, all.y = FALSE)
                                 mis <- which(is.na(pvF[,colnames(weights)[2]]))
                                 if ( length(mis) > 0 ) {                       ### missings in the weights frame are not allowed
                                      cat(paste ( "Found ",length(mis)," missing values in the 'weights' frame.\n    Cases with missing values on weighting variable will be ignored for transformation.\n",sep=""))
                                      pvF <- pvF[-mis,]
                                 }
                                 txt <- capture.output ( msdF <- jk2.mean ( datL = pvF, ID = id, imp = "imp", wgt = colnames(weights)[2], dependent = "valueTransfBista", na.rm = TRUE) )
                                 msdF<- adaptEatRepVersion(msdF)
                            }
                            msdFok <- c(msdF[intersect(which(msdF[,"parameter"] == "mean"), which(msdF[,"coefficient"] == "est")),"value"], msdF[intersect(which(msdF[,"parameter"] == "sd"), which(msdF[,"coefficient"] == "est")),"value"])
                            if ( cutsMis == FALSE & !is.null ( equatingList[["items"]] )) {
                                 cts <- c( -10^6, cuts[[mat1]][["values"]], 10^6)## Cuts mit Schwelle nach unten und nach oben offen
                                 le  <- do.call("rbind", lapply ( (length(cts)-1):1 , FUN = function ( l ) {
                                        kmp<- c(cts[l], cts[l+1])               ### Linkingfehler fuer einzelnen Kompetenzintervalle; absteigend wie bei karoline
                                        a1 <- sum ( dnorm ( ( kmp - refPop[mat,4]) / refPop[mat,5] ) * c(-1,1) / refPop[mat,5] )
                                        a2 <- sum ( dnorm ( ( kmp - msdFok[1]) / msdFok[2] ) * c(-1,1) / msdFok[2] )
                                        if(a2 == 0 ) {cat("mutmasslicher fehler.\n")}
                                        del<- ( (  a1^2 + a2^2 ) * (unique(itFrame[,"linkingErrorTransfBista"])^2) / 2  )^0.5
               			                    del<- data.frame ( traitLevel = attr(traitLevel, "cat.values")[l], linkingErrorTraitLevel = del )
               			                    return(del)}))
    ### ggf. weg! 'linkingErrorTraitLevel' ergibt ja fuer Items keinen Sinn, nur fuer Personenparameter
                                 ori <- colnames(itFrame)
                                 chk <- unique(le[,"traitLevel"]) %in% unique(itFrame[,"traitLevel"])
                                 if ( length( which(chk == FALSE)) > 0) {
                                     cat(paste("Warning: No items on trait level(s) '",paste( unique(le[,"traitLevel"])[which(chk == FALSE)], collapse = "', '"), "'. \n", sep=""))
                                 }
                                 itFrame <- data.frame ( merge ( itFrame, le, by = "traitLevel", sort = FALSE, all.x = TRUE, all.y = FALSE) )
                                 itFrame <- itFrame[,c(ori, "linkingErrorTraitLevel")]
                            }
                            itFrame[,"refMean"]        <- refPop[mat,2]
                            itFrame["refSD"]           <- refPop[mat,3]
                            itFrame[,"refTransfMean"]  <- refPop[mat,4]
                            itFrame[,"refTransfSD"]    <- refPop[mat,5]
    ### 2. Transformation der Personenparameter
                            if ( cutsMis == FALSE ) { pv[,"traitLevel"]   <- num.to.cat(x = pv[,"valueTransfBista"], cut.points = cuts[[mat1]][["values"]], cat.values = cuts[[mat1]][["labels"]])}
                            pv[,"dimension"]  <- pv[,"group"]
                            chk <- unique(le[,"traitLevel"]) %in% unique(pv[,"traitLevel"])
                            if ( length( which(chk == FALSE)) > 0) {
                                 cat(paste("Warning: No plausible values on trait level(s) '",paste( unique(le[,"traitLevel"])[which(chk == FALSE)], collapse = "', '"), "'. \n", sep=""))
                            }
                            stopifnot ( length( unique ( na.omit(itFrame[,"linkingErrorTransfBista"]))) %in% 0:1)
                            pv[,"linkingError"] <- equatingList[["items"]][[mod]][[dims]][["eq"]][["descriptives"]][["linkerror"]]
                            pv[,"linkingErrorTransfBista"] <- unique ( itFrame[,"linkingErrorTransfBista"])
                            ori <- colnames(pv)                                 ### nur wenn untere Bedingung == TRUE, gibt es das Objekt 'le', das gemergt werden soll
                            if ( cutsMis == FALSE & !is.null ( equatingList[["items"]] )) {
                                 pv  <- data.frame ( merge ( pv, le, by = "traitLevel", sort = FALSE, all.x = TRUE, all.y = FALSE) )
                                 pv  <- pv[,c(ori, "linkingErrorTraitLevel")]
                            }
    ### ggf. Gewichte an Personenframe mit dranhaengen
                            if (!is.null(weights)) {
                                 pv  <- merge ( pv, weights , by.x = id, by.y = colnames(weights)[1], all.x = TRUE, all.y = FALSE)
                            }
    ### 'refPop' Informationstabelle bauen
                            rp  <- refPop[mat,]
                            colnames(rp) <- c("domain", "refMean", "refSD", "bistaMean", "bistaSD")
                            rp  <- cbind ( model = mod, rp, focusMean = msdFok[1], focusSD = msdFok[2])
                            return(list ( itempars = itFrame, personpars = pv, rp = rp))
                      }  })
              itempars<- do.call("rbind", lapply ( dimN, FUN = function ( x ) { x[["itempars"]]}))
              perspar <- do.call("rbind", lapply ( dimN, FUN = function ( x ) { x[["personpars"]]}))
              rp      <- do.call("rbind", lapply ( dimN, FUN = function ( x ) { x[["rp"]]}))
              return( list ( itempars = itempars, personpars = perspar, rp=rp)) } )
       personpars <- do.call("rbind", lapply ( modN, FUN = function ( x ) { x[["personpars"]]}))
       itempars   <- do.call("rbind", lapply ( modN, FUN = function ( x ) { x[["itempars"]]}))
       rp         <- do.call("rbind", lapply ( modN, FUN = function ( x ) { x[["rp"]]}))
    ### jetzt noch die Itemparameterliste fuer die Vergleichsarbeiten reduzieren und aufbereiten
       pCols      <- colnames(itempars)[grep("^itemP", colnames(itempars))]
       allCols    <- na.omit(match ( c("dimension","item", pCols, "itemDiscrim", "estTransf", "infit", "estTransfBista", "traitLevel"), colnames(itempars)))
       itemVera   <- itempars[,allCols]
       colnames(itemVera) <- recode ( colnames(itemVera), "'dimension'='domain'; 'item'='iqbitem_id'; 'itemDiscrim'='trennschaerfe'; 'estTransf'='logit'; 'estTransfBista'='bista'; 'traitLevel'='kstufe'")
       colnames(itemVera)[match(pCols, colnames(itemVera))] <- paste0("lh", removePattern ( string = pCols, pattern = "^itemP"))
       if ( roman == TRUE ) {                                                   ### sollen roemische Zahlen fuer Kompetenzstufen verwendet werden?
            if (!all(itemVera[,"kstufe"] %in% c("1a", "1b", 1:5))) {stop(paste("Competence levels do not match allowed values. '1a', '1b', '1', '2', '3', '4', '5' is allowed. '",paste(names(table(itemVera[,"kstufe"])), collapse = "', '"),"' was found.\n",sep=""))}
            itemVera[,"kstufe"] <- recode (itemVera[,"kstufe"], "'1a'='Ia'; '1b'='Ib'; '1'='I'; '2'='II'; '3'='III'; '4'='IV'; '5'='V'")
       }
    ### jetzt den scheiss reshapen fuer vera-3 mathe, wenn es separate werte fuer global- und domaenenspezifische modelle gibt
       if ( length ( unique ( itemVera[,"iqbitem_id"])) != length ( itemVera[,"iqbitem_id"]) ) {
            cat("Found duplicated entries in 'item-ID' column. This should only occur for subject 'math' in grade 3.\n")
    ### vorher rauskriegen, ob jedes item nur zu einer domaene und einem globalmodell gehoert, dann 'domain' umbenennen, um NAs im Ergebnis zu vermeiden
            tab  <- table(itemVera[,c("domain", "iqbitem_id")])
            if ( !"GL" %in% rownames(tab)) {
                 cat("Cannot find 'global' entry in the 'domain' column. Cancel reshaping.\n")
            }  else  {
                 if ( !sum(tab[which(rownames(tab) == "GL"),]) == ncol(tab)) {  ### hat jedes Item einen Wert auf 'global'?
                     cat("Found items without values on the 'glbal' domain. Cancel reshaping.\n")
                 }  else  {
                     if ( !all(colSums(tab) == 2) ) {
                         cat("Found items which do not have one 'global' and one domain-specific parameter. Cancel reshaping.\n")
                     }  else  {
                         itemVera[,"dummy"] <- recode ( itemVera[,"domain"], "'GL'='GL'; else = 'domain'")
                         colsValid <- c("lh", "trennschaerfe", "logit", "infit", "bista", "kstufe")
                         colsValid <- colsValid[which(colsValid %in% colnames(itemVera))]
                         long      <- melt ( itemVera, id.vars = c("iqbitem_id", "dummy"), measure.vars = colsValid, na.rm=TRUE)
                         itemVera  <- asNumericIfPossible(dcast ( long , iqbitem_id ~ dummy + variable, value.var = "value"), verbose = FALSE, ignoreAttributes = TRUE)
                     }
                 }
            }
       }
       context    <- equatingList[["results"]][which(equatingList[["results"]][,"type"]=="tech"),]
       ret        <- list ( itempars = itempars, personpars = personpars, refPop = refPop, means = rp, all.Names = context, itemparsVera = itemVera)
       class(ret) <- c("list", "transfBista")
       return( ret ) }


runModel <- function(defineModelObj, show.output.on.console = FALSE, show.dos.console = TRUE, wait = TRUE) {
            if ("defineMultiple" %in% class( defineModelObj ) ) {               ### erstmal fuer den Multimodellfall: nur dafuer wird single core und multicore unterschieden
                if(is.null ( attr(defineModelObj, "split")[["nCores"]] ) || attr(defineModelObj, "split")[["nCores"]] == 1 ) {
                   res <- lapply(defineModelObj, FUN = function ( r ) {         ### erstmal: single core
                          ret <- runModel ( defineModelObj = r, show.output.on.console = show.output.on.console, show.dos.console = show.dos.console, wait = wait)
                          return(ret)})
                }  else  {                                                      ### multicore
                   # if(!exists("detectCores"))   {library(parallel)}
                   doIt<- function (laufnummer,  ... ) {
                          if(!exists("runModel"))  { library(eatModel) }
                          ret <- runModel ( defineModelObj = defineModelObj[[laufnummer]], show.output.on.console = show.output.on.console, show.dos.console = show.dos.console, wait = TRUE)
                          return(ret) }
                   beg <- Sys.time()
                   if ( attr(defineModelObj, "split")[["mcPackage"]] == "parallel") {
                        cl  <- makeCluster(attr(defineModelObj, "split")[["nCores"]], type = "SOCK")
                   }  else  {
                        cl  <- makeClusterPSOCK(attr(defineModelObj, "split")[["nCores"]], verbose=FALSE)
                   }
                   res <- clusterApply(cl = cl, x = 1:length(defineModelObj), fun = doIt , show.output.on.console = show.output.on.console, show.dos.console = show.dos.console, wait = wait)
                   stopCluster(cl)
                   cat(paste ( length(defineModelObj), " analyses finished: ", sep="")); print( Sys.time() - beg)
                }
                class(res) <- c("runMultiple", "list")
                attr(res, "split") <- attr(defineModelObj, "split")
                return(res)
            } else {                                                            ### ab hier fuer den single model Fall
                if("defineConquest" %in% class(defineModelObj)) {               ### hier fuer conquest
                   oldPfad <- getwd()
                   setwd(defineModelObj$dir)
                   suppressWarnings(system(paste(defineModelObj$conquest.folder," ",defineModelObj$input,sep=""),invisible=!show.dos.console,show.output.on.console=show.output.on.console, wait=wait) )
                   if(wait == FALSE) { Sys.sleep(0.2) }
                   setwd(oldPfad)                                               ### untere Zeile: Rueckgabeobjekt definieren: Conquest
                   class(defineModelObj) <- c("runConquest", "list")
                   return ( defineModelObj )
                }
                if("defineTam" %in% class(defineModelObj)) {                    ### exportiere alle Objekte aus defineModelObj in environment
                   for ( i in names( defineModelObj )) { assign(i, defineModelObj[[i]]) }
                   if ( show.output.on.console == TRUE ) { control$progress <- TRUE }
                   # if(!exists("tam.mml"))       {library(TAM, quietly = TRUE)}  ### March, 2, 2013: fuer's erste ohne DIF, ohne polytome Items, ohne mehrgruppenanalyse, ohne 2PL
                   if(!is.null(anchor)) {
                       stopifnot(ncol(anchor) == 2 )                            ### Untere Zeile: Wichtig! Sicherstellen, dass Reihenfolge der Items in Anker-Statement
                       notInData   <- setdiff(anchor[,1], all.Names[["variablen"]])
                       if(length(notInData)>0)  {
                          cat(paste("Found following ", length(notInData)," item(s) in anchor list which are not in the data:\n",sep=""))
                          cat(paste(notInData, collapse = ", ")); cat("\n")
                          cat("Delete missing item(s) from anchor list.\n")
                          anchor <- anchor[-match(notInData, anchor[,1]),]
                       }
                       anchor[,1]    <- match(as.character(anchor[,1]), all.Names[["variablen"]])
                   }
                   if(length( all.Names[["HG.var"]])>0)     { Y <- daten[,all.Names[["HG.var"]], drop=FALSE] } else { Y <- NULL }
                   if(length( all.Names[["weight.var"]])>0) { wgt <- as.vector(daten[,all.Names[["weight.var"]]])} else {wgt <- NULL}
                   stopifnot(all(qMatrix[,1] == all.Names[["variablen"]]))
                   if(length(all.Names[["DIF.var"]]) == 0 ) {
                      if( irtmodel %in% c("1PL", "PCM", "PCM2", "RSM") ) {
                          mod     <- tam.mml(resp = daten[,all.Names[["variablen"]]], constraint = constraint, pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = control)
                      }
                      if( irtmodel %in% c("2PL", "GPCM", "2PL.groups", "GPCM.design", "3PL") )  {
                          if(!is.null(est.slopegroups))  {
                              weg1            <- setdiff(all.Names[["variablen"]], est.slopegroups[,1])
                              if(length(weg1)>0) {stop("Items in dataset which are not defined in design matrix for item groups with common slopes ('est.slopegroups').\n")}
                              weg2            <- setdiff(est.slopegroups[,1], all.Names[["variablen"]])
                              if(length(weg2)>0) {
                                 cat(paste("Following ",length(weg2), " Items in design matrix for item groups with common slopes ('est.slopegroups') which are not in dataset:\n",sep=""))
                                 cat("   "); cat(paste(weg2, collapse=", ")); cat("\n")
                                 cat("Remove these item(s) from design matrix.\n")
                                 est.slopegroups <- est.slopegroups[-match(weg2,est.slopegroups[,1]),]
                              }
                              est.slopegroups <- as.numeric(as.factor(est.slopegroups[match(all.Names[["variablen"]], est.slopegroups[,1]),2]))
                          }
                          if(!is.null(fixSlopeMat))  {                          ### Achtung: wenn Items identifiers NICHT unique sind (z.B., Item gibt es global und domaenenspezifisch,
                              fixSlopeMat <- facToChar(fixSlopeMat)    ### dann wird jetzt 'fixSlopeMat' auf die Dimension in der Q Matrix angepasst ... das ist nur erlaubt, wenn es ein eindimensionales Modell ist!!
                              if(!is.null( slopeMatDomainCol ) ) {
                                  allV <- list(slopeMatDomainCol=slopeMatDomainCol , slopeMatItemCol=slopeMatItemCol, slopeMatValueCol =slopeMatValueCol)
                                  all.Names <- c(all.Names, lapply(allV, FUN=function(ii) {existsBackgroundVariables(dat = fixSlopeMat, variable=ii)}))
                                  if ( ncol(qMatrix) != 2) { stop ( "Duplicated item identifiers in 'fixSlopeMat' are only allowed for unidimensional models.\n") }
                                  mtch <- wo.sind( colnames(qMatrix)[2], fixSlopeMat[, all.Names[["slopeMatDomainCol"]]], quiet = TRUE)
                                  if ( length( mtch) < 2 ) { stop(cat(paste ( "Cannot found dimension '",colnames(qMatrix)[2],"' in 'fixSlopeMat'. Found following values in '",all.Names[["slopeMatDomainCol"]],"' column of 'fixSlopeMat': \n    '", paste( sort(unique(fixSlopeMat[, all.Names[["slopeMatDomainCol"]] ])), collapse="', '"),"'.\n",sep="")))}
                                  fixSlopeMat <- fixSlopeMat[mtch, c(all.Names[["slopeMatItemCol"]],all.Names[["slopeMatValueCol"]])]
                              }
                              cat ( "W A R N I N G:  To date, fixing slopes only works for dichotomous unidimensional or between-item multidimensional models.\n")
                              estVar          <- TRUE
                              weg2            <- setdiff(fixSlopeMat[,1], all.Names[["variablen"]])
                              if(length(weg2)>0) {
                                 cat(paste("Following ",length(weg2), " Items in matrix for items with fixed slopes ('fixSlopeMat') which are not in dataset:\n",sep=""))
                                 cat("   "); cat(paste(weg2, collapse=", ")); cat("\n")
                                 cat("Remove these item(s) from 'fixSlopeMat' matrix.\n")
                                 fixSlopeMat <- fixSlopeMat[-match(weg2,fixSlopeMat[,1]),]
                              }                                                 ### Achtung, grosser Scheiss: wenn man nicht (wie oben) eine Reihenfolgespalte angibt,
                              if ( nrow(fixSlopeMat) != length(unique(fixSlopeMat[,1])) ) { stop ( "Item identifiers in 'fixSlopeMat' are not unique.\n")}
                              fixSlopeMat[,"reihenfolge"] <- 1:nrow(fixSlopeMat)### aendert die untere 'by'-Schleife die Sortierung!
                              dims  <- (1:ncol(qMatrix))[-1]                    ### Slopematrix muss itemweise zusammengebaut werden
                              slopMa<- do.call("rbind", by ( data = fixSlopeMat, INDICES = fixSlopeMat[,"reihenfolge"], FUN = function (zeile ) {
                                       zeile <- zeile[,-ncol(zeile)]
                                       stopifnot ( nrow(zeile) == 1 )
                                       qSel  <- qMatrix[which( qMatrix[,1] == zeile[[1]]),]
                                       anzKat<- length(unique(na.omit(daten[,as.character(zeile[[1]])])))
                                       zeilen<- anzKat * length(dims)           ### fuer jedes Items gibt es [Anzahl Kategorien] * [Anzahl Dimensionen] Zeilen in der TAM matrix
                                       block <- cbind ( rep ( match(zeile[[1]], all.Names[["variablen"]]), times = zeilen), rep ( 1:anzKat, each = length(dims) ), dimsI <- rep ( 1:length(dims), times = anzKat), rep(0, zeilen))
                                       matchD<- which ( qSel[,-1] != 0 )
                                       stopifnot ( length( matchD ) == 1)
                                       match2<- intersect(which(block[,2] == max(block[,2])), which(block[,3] == (matchD)))
                                       stopifnot ( length( na.omit(match2 )) == 1)
                                       block[match2,4] <- zeile[[2]]
                                       return(block) }))
                          }  else  {
                              estVar          <- FALSE
                              slopMa          <- NULL
                          }
                          if( irtmodel == "3PL") {
                              if(!is.null(guessMat)) {
                                 weg1          <- setdiff(all.Names[["variablen"]], guessMat[,1])
                                 if(length(weg1)>0) {cat(paste(length(weg1), " item(s) in dataset which are not defined in guessing matrix. No guessing parameter will be estimated for these/this item(s).\n",sep="")) }
                                 weg2          <- setdiff(guessMat[,1], all.Names[["variablen"]])
                                 if(length(weg2)>0) {
                                    cat(paste(length(weg2), " item(s) in guessing matrix missing in dataset. Remove these items from guessing matrix.\n",sep=""))
                                    guessMat   <- guessMat[-match( weg2, guessMat[,1])  ,]
                                 }
                                 gues <- guessMat[ match( all.Names[["variablen"]], guessMat[,1]) , "guessingGroup"]
                                 gues[which(is.na(gues))] <- 0
                              }  else  { gues <- NULL }
                              mod  <- tam.mml.3pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, pweights = wgt, est.guess =gues,  est.variance = estVar, control = control)
                          }  else { mod     <- tam.mml.2pl(resp = daten[,all.Names[["variablen"]]], pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, est.slopegroups=est.slopegroups,pweights = wgt, B.fixed = slopMa, est.variance = estVar, control = control) }
                      }
                   } else {
                     assign(paste("DIF_",all.Names[["DIF.var"]],sep="") , as.data.frame (daten[,all.Names[["DIF.var"]]]) )
                     formel   <- as.formula(paste("~item - ",paste("DIF_",all.Names[["DIF.var"]],sep="")," + item * ",paste("DIF_",all.Names[["DIF.var"]],sep=""),sep=""))
                     facetten <- as.data.frame (daten[,all.Names[["DIF.var"]]])
                     colnames(facetten) <- paste("DIF_",all.Names[["DIF.var"]],sep="")
                     mod      <- tam.mml.mfr(resp = daten[,all.Names[["variablen"]]], facets = facetten, constraint = constraint, formulaA = formel, pid = daten[,"ID"], Y = Y, Q = qMatrix[,-1,drop=FALSE], xsi.fixed = anchor, irtmodel = irtmodel, pweights = wgt, control = control)
                   }
                   attr(mod, "qMatrix")      <- defineModelObj[["qMatrix"]]     ### hier werden fuer 'tam' zusaetzliche Objekte als Attribute an das Rueckgabeobjekt angehangen
                   attr(mod, "n.plausible")  <- defineModelObj[["n.plausible"]] ### Grund: Rueckgabeobjekt soll weitgehend beibehalten werden, damit alle 'tam'-Funktionen, die darauf aufsetzen, lauffaehig sind
                   attr(mod, "dir")          <- defineModelObj[["dir"]]
                   attr(mod, "analysis.name")<- defineModelObj[["analysis.name"]]
                   attr(mod, "all.Names")    <- defineModelObj[["all.Names"]]
                   attr(mod, "deskRes")      <- defineModelObj[["deskRes"]]
                   attr(mod, "discrim")      <- defineModelObj[["discrim"]]
                   attr(mod, "irtmodel")     <- defineModelObj[["irtmodel"]]
                   attr(mod, "Y")            <- Y
                   return(mod)  }  }   }

### Hilfsfunktiuon fuer 'defineModel': Jetzt wird die aufbereitete Liste aus 'splitModels' abgearbeitet. ACHTUNG: Argumente in 'splittedModels' ueberschreiben default- und vom Nutzer gesetzte Argumente in 'defineModel'!
### Der Funktionsaufruf von 'doAufb' variiert je nach single- oder multicore handling. hier: single core
doAufb <- function ( m, matchCall, anf, verbose ) {
          for ( i in 1:length(matchCall) ) { assign ( names(matchCall)[i], matchCall[[i]]) }
          matchL <- match(m, unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) { l[["model.no"]] } )))
          mess1  <- NULL
          if(!is.null(splittedModels[["models.splitted"]][[matchL]][["qMatrix"]])) {
     ### check: wenn superSplitter BERUHEND AUF ITEM GROUPING genutzt wird, wird 'items'-Argument von 'defineModel' ignoriert; wenn das also im 'matchCall' NICHT NULL ist, wird es ignoriert
             if ( !is.null(matchCall[["items"]]) )  {                           ### Warnung nur beim ersten Schleifendurchlauf anzeigen!
                  if(m == anf) { cat("Warning: 'defineModel' was called using 'splitModels' argument. Model split according to item groups is intended. Item selection is defined \n    via 'splittedModels' object. Hence, 'items' argument is expected to be missed in 'defineModel()' and will be ignored.\n") }
             }
             itemMis<- setdiff ( splittedModels[["models.splitted"]][[matchL]][["qMatrix"]][,1], colnames(dat))
             if( length ( itemMis ) > 0) {
                  mess1 <- paste( "Warning! Model No. ",splittedModels[["models.splitted"]][[matchL]][["model.no"]], ", model name: '",splittedModels[["models.splitted"]][[matchL]][["model.name"]],"': ", length(itemMis) ," from ",nrow(splittedModels[["models.splitted"]][[matchL]][["qMatrix"]])," items listed the Q matrix not found in data:\n    ", paste(itemMis,collapse=", "),"\n",sep="")
             }
             itemSel<- intersect ( splittedModels[["models.splitted"]][[matchL]][["qMatrix"]][,1], colnames(dat))
             qMatrix<- splittedModels[["models.splitted"]][[matchL]][["qMatrix"]]
          }  else  {
             if ( is.null(matchCall[["items"]]) )  { stop(paste("Model no. ",m," ('",splittedModels[["models.splitted"]][[matchL]][["model.name"]],"'): no items defined.\n",sep=""))}
             itemSel<- items                                                    ### itemSel = "items selected"
          }
     ### Personen im Datensatz selektieren: Achtung: wenn keine Personen in "person.grouping", nimm alle!
          if(!is.null(splittedModels[["models.splitted"]][[matchL]][["person.grouping"]])) {
             persMis<- setdiff ( splittedModels[["models.splitted"]][[matchL]][["person.grouping"]][,1], dat[,id])
             if( length ( persMis ) > 0) {
                 cat(paste( "Warning: ",length(persMis) ," from ",nrow(splittedModels[["models.splitted"]][[matchL]][["person.grouping"]])," persons not found in data.\n",sep=""))
             }
             persons<- intersect ( splittedModels[["models.splitted"]][[matchL]][["person.grouping"]][,1], dat[,id])
             datSel <- dat[match(persons, dat[,id]),]
          }  else  { datSel <- dat }
     ### Unterverzeichnisse definieren
          if(is.null(matchCall[["dir"]])) { dirI <- NULL }  else  { dirI   <- file.path(dir, substring(splittedModels[["models.splitted"]][[matchL]][["model.subpath"]],3)) }
          nameI  <- splittedModels[["models.splitted"]][[matchL]][["model.name"]]
          if(!exists("qMatrix") ) { nDim <- 1; qMatrix <- NULL } else { nDim <- ncol(qMatrix)-1 }
     ### Aufruf von 'defineModel' generieren, Teil 1. Achtung: wenn der Nutzer eigenhaendig neue Argumente in <models>[["models"]] einfuegt, muessen die hier in <models>[["models.splitted"]] uebernommen werden!
          overwr1<- list( dat=datSel, items = itemSel, qMatrix = qMatrix, analysis.name = nameI, dir = dirI)
          overwrF<- setdiff ( colnames(splittedModels[["models"]]), c("model.no", "model.name", "model.subpath", "dim", "Ndim", "group", "Ngroup"))
          if(length(overwrF)>0) {                                               ### wenn der Nutzer zusaetzliche Spalten in <models>$models spezifiziert, muessen
             notAllow <- setdiff ( overwrF, names(formals(defineModel)))        ### die Spaltennamen zu Argumenten von 'defineModel' passen, sonst werden die ignoriert
             if ( length ( notAllow ) > 0 ) {
                  if ( m == anf ) {                                             ### folgende Warnung soll nur einmal erscheinen, obwohl es fuer jedes Modell geschieht (Konsole nicht mit Meldungen zumuellen)
                       cat(paste("Column(s) '",paste(notAllow, collapse = "', '"),"' of 'splittedModels' definition frame do not match arguments of 'defineModel()'. Columns will be ignored.\n", sep=""))
                  }
                  overwrF <- setdiff (overwrF, notAllow)
             }                                                                  ### wenn der Nutzer zusaetzliche Spalten in <models>$models spezifiziert, duerfen bestimmte
             notAllow2<- intersect ( overwrF, names(overwr1))                   ### Argumente von 'defineModel' NICHT benutzt werden, die werden dann auch ignoriert
             if ( length ( notAllow2 ) > 0 ) {
                  if ( m == anf ) {                                             ### folgende Warnung soll nur einmal erscheinen, obwohl es fuer jedes Modell geschieht (Konsole nicht mit Meldungen zumuellen)
                       cat(paste("Column(s) '",paste(notAllow2, collapse = "', '"),"' of 'splittedModels' definition frame are not allowed to be modified by user. Columns will be ignored.\n", sep=""))
                  }
                  overwrF <- setdiff (overwrF, notAllow2)
             }
             notAllow3<- intersect ( overwrF, names(matchCall))                 ### wenn der Nutzer bspw. im Splitter die nodes modellspezifisch setzt, duerfen die im Aufruf von 'defineModel' nicht nochmal gesetzt werden ...
             if ( length ( notAllow3 ) > 0 ) {                                  ### gibt nur eine Warnung, das Ignorieren geschieht automatisch
                  if ( m == anf ) {
                       cat(paste("Column(s) '",paste(notAllow3, collapse = "', '"),"' were defined twice, in <models>$models and 'defineModel'. The latter one will be ignored.\n", sep=""))
                  }
             }                                                                  ### wenn nach den ganzen checks immer noch zusaetzliche Argumente uebrig sind, werden die jetzt
             if ( length ( overwrF ) > 0 ) {                                    ### in 'overwr1' ergaenzt ... und in 'splittedModels' fuer die 'sprechenden Ausgaben'
                  for ( hh in overwrF ) {
                        overwr1[[hh]] <- splittedModels[["models"]][m,hh]
                        splittedModels[["models.splitted"]][[matchL]][[hh]] <- splittedModels[["models"]][m,hh]
                  }
             }
          }  else  { overwrF <-  NULL }
     ### Aufruf von 'defineModel' generieren, Teil 2. Fuege Argumente aus 'matchCall' in 'overwr1' hinzu ... aber nur, die es nicht schon gibt
          zusatz <- setdiff ( setdiff ( names(matchCall), "splittedModels"), names( overwr1))
          if ( length ( zusatz ) > 0 ) { overwr1 <- c(overwr1, matchCall[zusatz]) }
     ### sprechende Ausgaben, wenn verbose == TRUE
          if(exists("items"))   {allVars<- list(variablen=items)}
          if(exists("itemSel")) {allVars<- list(variablen=itemSel)}             ### Hotfix: anzahl der Items bestimmen
          allNams<- lapply(allVars, FUN=function(ii) {existsBackgroundVariables(dat = dat, variable=ii)})
          overwr3<- data.frame ( arg = c("Model name", "Number of items", "Number of persons", "Number of dimensions"), eval = as.character(c(splittedModels[["models.splitted"]][[matchL]][["model.name"]],length(allNams[["variablen"]]), nrow(datSel) , nDim)), stringsAsFactors = FALSE)
           if ( length ( overwrF) > 0 )  {
                zusatz <- lapply ( overwrF, FUN = function ( y ) { splittedModels[["models"]][m,y] })
                zusatz <- data.frame ( arg = overwrF, eval = as.character ( zusatz ) )
                overwr3<- rbind ( overwr3, zusatz)
           }
           overwr3["leerz"] <- max (nchar(overwr3[,"arg"])) - nchar(overwr3[,"arg"]) + 1
           txt    <- apply(overwr3, MARGIN = 1, FUN = function ( j ) { paste("\n    ", j[["arg"]], ":", paste(rep(" ", times = j[["leerz"]]), sep="", collapse=""), j[["eval"]], sep="")})
           nDots  <- max(nchar(overwr3[,"arg"])) + max(nchar(overwr3[,"eval"])) + 6
           if(verbose == TRUE ) {
              cat(paste("\n\n",paste(rep("=",times = nDots), sep="", collapse=""),"\nModel No. ",m, paste(txt,sep="", collapse=""), "\n",paste(rep("=",times = nDots), sep="", collapse=""),"\n\n", sep=""))
              if(!is.null(mess1)) { cat(mess1)}
           }
     ### Achtung! Rueckgabe haengt davon ab, ob multicore Handling stattfinden soll! zuerst single core
          if(is.null ( splittedModels[["nCores"]] ) | splittedModels[["nCores"]] == 1 ) {
             ret    <- do.call("defineModel", args = overwr1)                   ### single core handling: die verschiedenen Modelle werden
          }  else  {                                                            ### bereits jetzt an "defineModel" zurueckgegeben und seriell verarbeitet
             ret    <- overwr1                                                  ### multicore: die verschiedenen Modelle werden noch nicht weiter verarbeitet,
          }                                                                     ### es wird lediglich der Modellaufruf generiert, der dann spaeter an die einzelnen
          return(ret) }                                                         ### cores weitergegeben wird

### dat               ... Datensatz als R-Dataframe
### items             ... wo stehen Items im datensatz, z.B. 5:120 oder -c(1:5)
### id                ... wo steht ID-variable, entweder Spaltennummer oder Variablenname als String
### qMatrix           ... falls nicht definiert: eindimensional, ansonsten muss hier qMatrix als R-Dataframe uebergeben werden;
###                       Dabei enthaelt die erste Spalte stets den Item-Bezeichner, die folgenden die Zugehoerigkeit zu Dimensionen
### DIF.var           ... eine DIF-Variable, entweder als Spaltennummer oder Variablenname als String
### HG.var            ... ein oder mehrere Hintergrundvariablen, entweder als Spaltennummern oder Variablennamen, z.B. c(4,6), c("alter","geschlecht")
### weight.var        ... Gewichtungsvariable, entweder als Vektor/Skalar mit Spaltennummern oder Variablennamen
### anchor            ... optional: data.frame mit Ankerparametern, 1. Spalte muss Itembezeichner sein, zweite Spalte Parameter
### dir               ... wo sollen Conquest-Inputdateien hingeschrieben werden?
### analysis.name     ... Dateiname fuer Conquest-Input (nur Praefix, Suffixe werden automatisch vergeben)
### conquest.folder   ... optional: vollstaendiger Conquest-Pfad, z.B. "N:/iqb/console.exe" Wenn spezifiziert, wird Batchdatei ausgegeben
### set.constraints   ... "none" , "cases" (default) , "items" ; bei Anchor wird automatisch auf "none" gesetzt
### boundary          ... Es wird eine Nachricht ausgegeben, wenn Personen im Datensatz sind, die weniger als [boundary] Items bearbeitet haben
### remove.boundary   ... Logical: Sollen Personen, die weniger als [boundary] Items beantwortet haben, aus den Daten entfernt werden?
### nodes             ... (optional): positive ganzzahlige zahl (integer)
### f.nodes  			    ... positive ganzahlige Zahl (integer); Conquest-Handbuch S. 225
### p.nodes    		    ... positive ganzahlige Zahl (integer); Conquest-Handbuch S. 225
### method		  	    ... optional: "gauss" (default), "quadrature", "montecarlo"; Conquest-Handbuch S. 225
### std.err           ... optional: "full", "quick" (default), "none"; Conquest-Handbuch S. 167ff
### n.interations	    ... positive ganzahlige Zahl (integer); maximale Anzahl von Iterationen (geht fuer tam und conquest)
### converge		      ... Gleitkommazahl; Conquest-Handbuch S. 225
### distribution      ... "normal" (default), "discrete"; Conquest-Handbuch S. 167ff
### equivalence.table ... Gibt ggf. Tabelle mit Umrechnungen Rohwert-Normwert; moegliche Werte sind "wle" (default); "mle" oder NULL (keine Tabelle wird ausgegeben)
###                       (Conquest-handbuch, S.166)
### use.letters       ... optional: sollen Daten in Buchstaben umcodiert werden und fuer Conquest-Analyse Buchstaben statt Ziffern benutzt werden? Ist moeglicherweise bei Partial Credit mit mehr als zehn Kategorien
###                       bedeutsam, da Conquest Probleme mit zweistelligen Codes hat. Buchstaben erlauben also bis maximal 26 Kategorien und bleiben einstellig.
### model.statement   ... model statement, wie es in Conquest erscheinen soll
### suppress.logfile  ... logical: should creation of logfile in Conquest be suppressed? Maybe important in Simulations studies (logfile is asked to overwrite in Conquest)
### check.for.linking ... logical: check whether all items are connected by design
### software          ... specifies software for analysis. If software == "lme4", only the long-format data.frame is returned. if software == "tam", dataset is prepared and tam is evaluated
### est.slopegroups   ... Matrix; erste Spalte: Item-ID, zweite Spalte: numerisch, definiert Gruppen fuer die eine gleiche Trennschaerfe angenommen werden soll ("restringierte 2pl-Modelle"); siehe auch Hilfe zu TAM
### guessMat          ... Matrix; erste Spalte: Item-ID, zweite Spalte: numerisch, definiert Gruppen mit gleichem Guessingparameter, unterschiedliche Zahlen bedeuten, dass jeweils ein separater
###                       Rateparameter geschaetzt wird. 0 oder fehlender Eintrag: kein Rateparameter wird geschaetzt
defineModel <- function(dat, items, id, splittedModels = NULL, irtmodel = c("1PL", "2PL", "PCM", "PCM2", "RSM", "GPCM", "2PL.groups", "GPCM.design", "3PL"),
               qMatrix=NULL, DIF.var=NULL, HG.var=NULL, group.var=NULL, weight.var=NULL, anchor = NULL, domainCol=NULL, itemCol=NULL, valueCol=NULL,check.for.linking = TRUE,
               minNperItem = 50, removeMinNperItem = FALSE, boundary = 6, remove.boundary = FALSE, remove.no.answers = TRUE, remove.no.answersHG = TRUE, remove.missing.items = TRUE, remove.constant.items = TRUE,
               remove.failures = FALSE, remove.vars.DIF.missing = TRUE, remove.vars.DIF.constant = TRUE, verbose=TRUE, software = c("conquest","tam"), dir = NULL,
               analysis.name, schooltype.var = NULL, model.statement = "item",  compute.fit = TRUE, n.plausible=5, seed = NULL, conquest.folder=NULL,
               constraints=c("cases","none","items"),std.err=c("quick","full","none"), distribution=c("normal","discrete"), method=c("gauss", "quadrature", "montecarlo"),
               n.iterations=2000,nodes=NULL, p.nodes=2000, f.nodes=2000,converge=0.001,deviancechange=0.0001, equivalence.table=c("wle","mle","NULL"), use.letters=FALSE,
               allowAllScoresEverywhere = TRUE, guessMat = NULL, est.slopegroups = NULL, fixSlopeMat = NULL, slopeMatDomainCol=NULL, slopeMatItemCol=NULL, slopeMatValueCol=NULL,
               progress = FALSE, increment.factor=1 , fac.oldxsi=0, export = list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE) )   {
                  if(!"data.frame" %in% class(dat) ) { cat("Convert 'dat' to a data.frame.\n"); dat <- data.frame ( dat, stringsAsFactors = FALSE)}
     ### Sektion 'multiple models handling': jedes Modell einzeln von 'defineModel' aufbereiten lassen
     ### Hier wird jetzt erstmal nur die bescheuerte Liste aus 'splitModels' aufbereitet (wenn der Nutzer sie verhunzt hat)
     ### das findet natuerlich nur statt, wenn es 'splitModels' gibt, wenn also MEHRERE Modelle simultan verarbeitet werden sollen
                  if(!is.null(splittedModels)) {
                     if(length(splittedModels) == 4L & !is.null(splittedModels[["models"]]) &  length(nrow( splittedModels[["models"]]) > 0)>0 ) {
                        if ( !missing ( analysis.name ) ) {
                           cat(paste("Analysis name is already specified by the 'splittedModels' object. User-defined analysis name '",analysis.name,"' will be used as prefix.\n",sep=""))
                           splittedModels[["models"]][,"model.name"] <- paste(analysis.name, "_", splittedModels[["models"]][,"model.name"],sep="")
                           for ( u in 1:length(splittedModels[["models.splitted"]]) ) { splittedModels[["models.splitted"]][[u]][["model.name"]] <- paste(analysis.name, "_", splittedModels[["models.splitted"]][[u]][["model.name"]],sep="") }
                        }
                        if(nrow(splittedModels[[1L]])>0) {
                           mods   <- intersect(splittedModels[["models"]][,"model.no"], unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]})))
                        }  else  {
                           mods <- unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]}))
                        }
                     }  else  {
                        mods <- unlist(lapply(splittedModels[["models.splitted"]], FUN = function ( l ) {l[["model.no"]]}))
                     }
                     if(length(mods) == 0) { stop("Inconsistent model specification in 'splittedModels'.\n") } else { if(verbose == TRUE) { cat(paste("\nSpecification of 'qMatrix' and 'person.groups' results in ",length(mods)," model(s).\n",sep="")) } }
                     if(!is.null(splittedModels[["nCores"]] ) ) {
                         if( splittedModels[["nCores"]] > 1 ) {
                             cat(paste ( "Use multicore processing. Models are allocated to ",splittedModels[["nCores"]]," cores.\n",sep=""))
                             flush.console()
                         }
                     }
     ### ok, die bescheuerte Liste aus 'splitModels' ist aufbereitet. Jetzt wird, sofern mehrere Modelle spezifiziert wurden, 'defineModel' mehrmals
     ### hintereinander aufgerufen (= rekursiv). Das geschieht ueber die Funktion 'doAufb' und kann prinzipiell ueber single- oder multicore erfolgen
     ### WICHTIG: 'doAufb' ruft irgendwann 'defineModel' auf, braucht also alle Argumente dieser Funktion ... sonst werden defaults genommen, und das will man ja hier nicht mehr
     ### in einem ersten Schritt muessen also alle Argumente von 'default Models' gesammelt werden
                     cll <- as.list(match.call(definition = defineModel))       ### sammle hier alle Argumente, die der Nutzer bei Aufruf von 'defineModel' selbststaendig definiert hat!
                     cll[["splittedModels"]] <- splittedModels                  ### ersetze nun alle Argumente, die die Funktion bis hierher geaendert hat!
                     cll <- lapply ( cll[2:length(cll)], eval )
                     anf <- mods[1]
     ### single core handling: Funktion "doAufb" wird seriell fuer alle "mods" aufgerufen
                     if(is.null ( splittedModels[["nCores"]] ) | splittedModels[["nCores"]] == 1 ) {
                        models <- lapply ( mods, FUN = doAufb, matchCall = cll, anf=anf, verbose=verbose)
     ### wenn multicore handling, dann wird das Objekt "model" an cores verteilt und dort weiter verarbeitet. Ausserdem werden Konsolenausgaben in stringobjekt "txt" weitergeleitet
                     }  else  {
                        txt <- capture.output ( models <- lapply ( mods, FUN = doAufb, matchCall = cll, anf=anf, verbose=verbose) )
                        # if(!exists("detectCores"))   {library(parallel)}
                        doIt<- function (laufnummer,  ... ) {
                               if(!exists("getResults"))  { library(eatModel) }
                               txt <- capture.output ( res <- do.call("defineModel", args = models[[laufnummer]] ) )
                               return(list ( res=res, txt=txt)) }
                        beg <- Sys.time()
                        if(splittedModels[["mcPackage"]] == "parallel") {
                           cl  <- makeCluster(splittedModels[["nCores"]], type = "SOCK")
                        }  else  {
                           cl  <- makeClusterPSOCK(splittedModels[["nCores"]], verbose=FALSE)
                        }
                        mods<- clusterApply(cl = cl, x = 1:length(models), fun = doIt)
                        stopCluster(cl)
                        cat(paste ( length(models), " models were prepared for estimation: ", sep="")); print( Sys.time() - beg)
     ### Trenne Aufbereitungsergebnisse von Konsolennachrichten
                        models <- lapply(mods, FUN = function ( m ) { m[["res"]] } )
     ### multicore gibt keine Ausgaben auf die Konsole, die muessen ueber "capture.output" eingefangen und separat ausgegeben werden
                        txts<- lapply(mods, FUN = function ( m ) { m[["txt"]] } )
                        luec<- which(txt == "")
                        pos <- luec[which ( diff(luec) == 1L )]
                        dif2<- which(diff(pos) == 1L)                           ### Hotfix!
                        if(length(dif2)>0) { pos <- pos [ -dif2 ] }
                        pos <- c(pos, length(txt)+1)
                        txtP<- lapply ( 1:(length(pos)-1), FUN = function ( u ) { txt[ pos[u] : (pos[u+1]-1) ] })
                        txtG<- NULL
                        stopifnot(length(txtP) == length(txts))
                        for ( j in 1:length(txtP) ) {
                              txtG <- c(txtG, txtP[[j]], txts[[j]])
                        }
                        cat(txtG, sep="\n")
                     }
                  attr(models, "split") <- splittedModels
                  class(models)    <- c("defineMultiple", "list")
                  return(models)                                                ### Das ist die Rueckgabe fuer den Mehrmodellfall
                  }  else  {
     ### ACHTUNG: hier beginnt jetzt der 'single model Fall' von 'defineModel' ###
                     irtmodel <- match.arg(irtmodel)
                     software <- match.arg(software)
                     method   <- match.arg(method)
                     if(software == "conquest") {
                        original.options <- options("scipen")                   ### lese Option fuer Anzahl der Nachkommastellen
                        options(scipen = 20)                                    ### setze Option fuer Anzahl der Nachkommastellen
                        if(missing(analysis.name)) {stop("Please specify 'analysis.name' or use 'software = \"tam\"'\n")}
                     }  else  {
                        if(missing(analysis.name)) {analysis.name <- "not_specified"}
                     }
                     if(length(model.statement)!=1)            {stop("'model.statement' has to be of length 1.\n")}
                     if(class(model.statement)!="character")   {stop("'model.statement' has to be of class 'character'.\n")}
                     if(missing(dat))   {stop("No dataset specified.\n") }      ### 11.04.2014: nutzt Hilfsfunktionen von jk2.mean etc.
                     if(is.null(items)) {stop("Argument 'items' must not be NULL.\n",sep="")}
                     if(length(items) == 0 ) {stop("Argument 'items' has no elements.\n",sep="")}
                     if ( length(items) != length(unique(items)) ) {
                          cat("Warning: Item identifier is not unique. Only unique item identifiers will be used.\n")
                          items <- unique(items)
                     }
                     if(length(id) != 1 ) {stop("Argument 'id' must be of length 1.\n",sep="")}
                     allVars     <- list(ID = id, variablen=items, DIF.var=DIF.var, HG.var=HG.var, group.var=group.var, weight.var=weight.var, schooltype.var = schooltype.var)
                     all.Names   <- lapply(allVars, FUN=function(ii) {existsBackgroundVariables(dat = dat, variable=ii)})
     ### wenn software = conquest, duerfen variablennamen nicht mehr als 11 Zeichen haben!
                     if(software == "conquest") {
                         if(max(nchar(all.Names[["variablen"]]))>11) {stop("In Conquest, maximum length of variable names must not exceed 11 characters. Please shorten variables names.\n")}
                     }
     ### ID-Variable pruefen und ggf. aendern
                     dat[,all.Names[["ID"]] ] <- as.character(dat[,all.Names[["ID"]] ])
                     doppelt     <- which(duplicated(dat[,all.Names[["ID"]]]))
                     if(length(doppelt)>0)  {stop(paste( length(doppelt) , " duplicate IDs found!",sep=""))}
                     if(software == "conquest") {
                        notAllowed  <- grep("-|\\.", dat[,all.Names[["ID"]] ])  ### fuer Conquest: unerlaubte Zeichen aus ID-Variable loeschen
                        if ( length(notAllowed)>0) {
                                cat("Conquest neither allows '.' nor '-' in ID variable. Delete signs from ID variable.\n")
                                dat[,all.Names[["ID"]] ] <- removePattern(string = removePattern(string=dat[,all.Names[["ID"]] ], pattern="\\."), pattern = "-")
                                if ( length ( which(duplicated(dat[,all.Names[["ID"]]])))>0) {
                                     dat[,all.Names[["ID"]] ] <- paste0(1:nrow(dat),dat[,all.Names[["ID"]] ])
                                }                                               ### wenn ID jetzt nicht mehr unique ist, wieder unique machen
                        }
                     }
                     if(!is.null(dir)) {                                        ### Sofern ein verzeichnis angegeben wurde (nicht NULL),
                        dir         <- crop(dir,"/")                   ### das Verzeichnis aber nicht existiert, wird es jetzt erzeugt
                        if(dir.exists(dir) == FALSE) {
                           cat(paste("Warning: Specified folder '",dir,"' does not exist. Create folder ... \n",sep="")); flush.console()
                           dir.create(dir, recursive = TRUE)
                        }
                     }
     ### pruefen, ob es Personen gibt, die weniger als <boundary> items gesehen haben (muss VOR den Konsistenzpruefungen geschehen)
                     datL.valid  <- melt(dat, id.vars = all.Names[["ID"]], meaure.vars = all.Names[["variablen"]], na.rm=TRUE)
                     nValid      <- table(datL.valid[,all.Names[["ID"]]])
                     rm(datL.valid)                                             ### Speicher sparen
                     inval       <- nValid[which(nValid<boundary)]
                     if(length(inval)>0) {
                        if ( length( inval > 5)) { auswahl  <- sort ( inval)[c(1, round(length(inval)/2)  ,length(inval))] }  else { auswahl <- sort (inval)[c(1, 3 , length(inval))] }
                        cat(paste( length(inval), " subject(s) with less than ",boundary," valid item responses: ", paste(names(auswahl),auswahl,sep=": ", collapse="; ")," ... \n",sep=""))
                        if(remove.boundary==TRUE) {
                           cat(paste("subjects with less than ",boundary," valid responses will be removed.\n    Caution! This can result in loosing some items likewise.\n",sep="") )
                           weg <- match(names(inval), dat[,all.Names[["ID"]]])
                           stopifnot(length(which(is.na(weg))) == 0 ) ; flush.console()
                           dat <- dat[-weg,]
                        }
                     }
     ### Sektion 'explizite Variablennamen ggf. aendern' ###
                     subsNam <- .substituteSigns(dat=dat, variable=unlist(all.Names[-c(1:2)]), all.Names = all.Names)
                     if(software == "conquest" || !is.null(all.Names[["DIF.var"]])) {
                        if(!all(subsNam$old == subsNam$new)) {                  ### Conquest erlaubt keine gross geschriebenen und expliziten Variablennamen, die ein "." oder "_" enthalten
                           sn     <- subsNam[which( subsNam$old != subsNam$new),]
                           cat("Conquest neither allows '.', '-' and '_' nor upper case letters in explicit variable names. Delete signs from variables names for explicit variables.\n"); flush.console()
                           recStr <- paste("'",sn[,"old"] , "' = '" , sn[,"new"], "'" ,sep = "", collapse="; ")
                           colnames(dat) <- recode(colnames(dat), recStr)
                           all.Names     <- lapply(all.Names, FUN = function ( y ) { recode(y, recStr) })
                           if(model.statement != "item") {
                              cat("    Remove deleted signs from variables names for explicit variables also in the model statement. Please check afterwards for consistency!\n")
                              for ( uu in 1:nrow(sn))  {model.statement <- gsub(sn[uu,"old"], sn[uu,"new"], model.statement)}
                           }
                        }
                        if("item" %in% unlist(all.Names[-c(1:2)])) { stop("Conquest does not allow labelling explicit variable(s) with 'Item' or 'item'.\n") }
                     }                                                          ### untere Zeilen: Dif-Variablen und Testitems duerfen sich nicht ueberschneiden
                     if(length(intersect(all.Names$DIF.var, all.Names$variablen))>0)    {stop("Test items and DIF variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$weight.var, all.Names$variablen))>0) {stop("Test items and weighting variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$HG.var, all.Names$variablen))>0)     {stop("Test items and HG variable have to be mutually exclusive.\n")}
                     if(length(intersect(all.Names$group.var, all.Names$variablen))>0)  {stop("Test items and group variable have to be mutually exclusive.\n")}
     ### Sektion 'Q matrix ggf. erstellen und auf Konsistenz zu sich selbst und zu den Daten pruefen' ###
                     if(is.null(qMatrix)) { qMatrix <- data.frame ( Item = all.Names$variablen, Dim1 = 1, stringsAsFactors = FALSE) } else {
                         qMatrix <- checkQmatrixConsistency(qMatrix)            ### pruefe Konsistenz der q-matrix
                         notInDat<- setdiff(qMatrix[,1], all.Names$variablen)
                         notInQ  <- setdiff( all.Names$variablen , qMatrix[,1])
                         if(length(notInDat)>0) {
                            cat(paste("Following ", length(notInDat)," item(s) missed in data frame will removed from Q matrix: \n    ",paste(notInDat,collapse=", "),"\n",sep=""))
                            qMatrix <- qMatrix[-match(notInDat, qMatrix[,1]),]
                            if(nrow(qMatrix) == 0) { stop("No common items in Q matrix and data.\n")}
                         }
                         if(length(notInQ)>0) {
                            cat(paste("Following ", length(notInQ)," item(s) missed in Q matrix will removed from data: \n    ",paste(notInQ,collapse=", "),"\n",sep=""))
                         }                                                      ### Wichtig! Sicherstellen, dass Reihenfolge der Items in Q-Matrix mit Reihenfolge der Items im Data.frame uebereinstimmt!
                         all.Names[["variablen"]] <- qMatrix[,1]  } ;   flush.console()
     ### Sektion 'Alle Items auf einfache Konsistenz pruefen' ###
                      namen.items.weg <- NULL
                      is.NaN <- do.call("cbind", lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function (uu) { is.nan(uu) } ) )
                      if(sum(is.NaN) > 0 ) {                                    ### Wandle NaN in NA, falls es welche gibt
                         cat(paste("Found ",sum(is.NaN)," 'NaN' values in the data. Convert 'NaN' to 'NA'.\n",sep=""))
                         for ( j in all.Names[["variablen"]]) {
                               weg <- which ( is.nan(dat[,j] ))
                               if(length(weg)>0) {  dat[weg,j] <- NA }
                         }
                      }
                      rm(is.NaN)                                                ### Speicher sparen
                      n.werte <- tableUnlist(dat[,all.Names[["variablen"]], drop = FALSE])
                      zahl    <- grep("[[:digit:]]", names(n.werte))            ### sind das alles Ziffern? (auch wenn die Spalten als "character" klassifiziert sind
                      noZahl  <- setdiff(1:length(n.werte), zahl)
                      if (length( zahl ) == 0 )  { stop("Please use numeric values for item responses.\n")}
                      if (length( noZahl ) > 0 ) {
                          cat(paste(" W A R N I N G !  Found ",sum(n.werte[noZahl])," non-numeric values in the item responses. These values will be treated as missing responses!\n",sep="")) }
                      klasse  <- unlist( lapply(dat[,all.Names[["variablen"]], drop = FALSE], class) )
                      if( "character" %in% klasse | "factor" %in% klasse | "logical" %in% klasse ) {
                          cat(paste(" W A R N I N G !  Found unexpected class type(s) in item response columns: ",paste(setdiff(klasse, c("numeric", "integer")), collapse = ", "), "\n",sep=""))
                          cat("                  All item columns will be transformed to be 'numeric'. Recommend to edit your data manually prior to analysis.\n")
                          for ( uu in all.Names[["variablen"]] ) { dat[,uu] <- as.numeric(dat[,uu])}
                      }
                      values  <- lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function ( ii ) { table(ii)})
                      isDichot<- unlist(lapply(values, FUN = function ( vv ) { identical(c("0","1"), names(vv)) }))
                      n.werte <- sapply(values, FUN=function(ii) {length(ii)})
                      n.mis   <- which(n.werte == 0)
                      if(length(n.mis) >0) {                                    ### identifiziere Items ohne jegliche gueltige Werte
                         cat(paste("Serious warning: ",length(n.mis)," testitems(s) without any values.\n",sep=""))
                         if(verbose == TRUE) {cat(paste("    ", paste(names(n.mis), collapse=", "), "\n", sep=""))}
                         if(remove.missing.items == TRUE) {
                         cat(paste("Remove ",length(n.mis)," variable(s) due to solely missing values.\n",sep=""))
                         namen.items.weg <- c(namen.items.weg, names(n.mis))}
                      }
                      if ( removeMinNperItem == TRUE ) {                        ### identifiziere Items mit weniger gueltigen Werte als in 'minNperItem' angegeben (nur wenn 'removeMinNperItem' = TRUE)
                           nValid <- unlist(lapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN = function ( ii ) { length(na.omit ( ii )) }))
                           below  <- which ( nValid < minNperItem )
                           if ( length ( below ) > 0 ) {
                                cat (paste ( "Found ", length(below), " items with less than ", minNperItem, " valid responses. These items will be removed.\n", sep=""))
                                namen.items.weg <- unique ( c(namen.items.weg, names(below)))
                           }
                      }
                      constant <- which(n.werte == 1)
                      if(length(constant) >0) {                                 ### identifiziere konstante Items (Items ohne Varianz)
                         cat(paste("Warning: ",length(constant)," testitems(s) are constants.\n",sep=""))
                         if(verbose == TRUE) {foo <- lapply(names(constant),FUN=function(ii) {cat(paste(ii,": ",names(table(dat[,ii])), " ... ",length(na.omit(dat[,ii]))," valid responses", sep="")); cat("\n")})}
                         if(remove.constant.items == TRUE) {
                         cat(paste("Remove ",length(constant)," variable(s) due to solely constant values.\n",sep=""))
                         namen.items.weg <- c(namen.items.weg, names(constant))}
                      }                                                         ### identifiziere alle Items, die nicht dichotom (="ND") sind
                      n.rasch   <- which( !isDichot )                           ### (aber nicht die, die bereits wegen konstanter Werte aussortiert wurden!)
                      if(length(n.rasch) >0 )   {                               ### also polytome Items oder Items, die mit 1/2 anstatt 0/1 kodiert sind
                         valND <- values[ which(names(values) %in% names(n.rasch)) ]
                         valND <- valND[which(sapply(valND, length) > 1)]
                         if(length(valND)>0) {
                            cat(paste("Warning: ",length(valND)," variable(s) are not strictly dichotomous with 0/1.\n",sep=""))
                            for (ii in 1:length(valND))  {
                                 max.nchar <-  max(nchar(names(table(dat[,names(valND)[ii]]))))
                                 if(max.nchar>1) {
                                    cat(paste("Arity of variable",names(valND)[ii],"exceeds 1.\n"))
                                 }
                                 if(verbose == TRUE) {
                                    cat(paste(names(valND)[ii],": ", paste( names(table(dat[,names(valND)[ii]])),collapse=", "),"\n",sep=""))
                                 }
                            }
                            cat("Expect a rating scale model or partial credit model.\n")
                            if(model.statement == "item") {
                               cat("WARNING: Sure you want to use 'model statement = item' even when items are not dichotomous?\n")
                            }
                         }
                      }
     ### Sektion 'Hintergrundvariablen auf Konsistenz zu sich selbst und zu den Itemdaten pruefen'. Ausserdem Stelligkeit (Anzahl der benoetigten character) fuer jede Variable herausfinden ###
                      weg.dif <- NULL; weg.hg <- NULL; weg.weight <- NULL; weg.group <- NULL
                      if(length(all.Names[["HG.var"]])>0)    {
                         varClass<- sapply(all.Names[["HG.var"]], FUN = function(ii) {class(dat[,ii])})
                         notNum  <- which(varClass %in% c("factor", "character"))
                         if(length(notNum)>0) {
                            cat(paste("Warning: Background variables '",paste(names(varClass)[notNum], collapse="', '"),"' of class \n    '",paste(varClass[notNum],collapse="', '"),"' will be converted to indicator variables.\n",sep=""))
                            ind <- do.call("cbind", lapply ( names(varClass)[notNum], FUN = function ( yy ) {
                                   newFr <- model.matrix( as.formula (paste("~",yy,sep="")), data = dat)[,-1,drop=FALSE]
                                   cat(paste("    Variable '",yy,"' was converted to ",ncol(newFr)," indicator(s) with name(s) '",paste(colnames(newFr), collapse= "', '"), "'.\n",sep=""))
                                   flush.console()
                                   return(newFr) }))
                            if(software == "conquest") {                        ### ggf. fuer Conquest Namen der HG-Variablen aendern
                               subNm <- .substituteSigns(dat=ind, variable=colnames(ind))
                               if(!all(subNm$old == subNm$new)) {
                                  sn  <- subNm[which( subNm$old != subNm$new),]
                                  reSt<- paste("'",sn[,"old"] , "' = '" , sn[,"new"], "'" ,sep = "", collapse="; ")
                                  colnames(ind) <- recode(colnames(ind), reSt)
                               }                                                ### entferne Originalnamen aus all.Names[["HG.var"]] und ergaenze neue Namen
                            }                                                   ### ergaenze neue Variablen im Datensatz
                            all.Names[["HG.var"]] <- setdiff ( all.Names[["HG.var"]], names(varClass)[notNum])
                            all.Names[["HG.var"]] <- c(all.Names[["HG.var"]], colnames(ind))
                            dat <- data.frame ( dat, ind )
                         }
                         hg.info <- lapply(all.Names[["HG.var"]], FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="HG", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE], suppressAbort = TRUE )})
                         for ( i in 1:length(hg.info)) { dat[, hg.info[[i]][["varname"]] ] <- hg.info[[i]]$x }
                         wegVar  <- unlist(lapply(hg.info, FUN = function ( uu ) { uu[["toRemove"]] }))
                         if(length(wegVar)>0) { all.Names[["HG.var"]] <- setdiff ( all.Names[["HG.var"]], wegVar) }
                         weg.hg  <- unique(unlist(lapply(hg.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.hg)>0) {                                 ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                            if ( remove.no.answersHG == TRUE ) {
                                 cat(paste("Remove ",length(weg.hg)," cases with missings on at least one HG variable.\n",sep=""))
                            }  else  {
                                 cat(paste(length(weg.hg)," cases with missings on at least one HG variable will be kept according to 'remove.no.answersHG = FALSE'.\n",sep=""))
                                 weg.hg <- NULL
                            }
                         }
                      }
                      if(length(all.Names$group.var)>0)  {
                         group.info <- lapply(all.Names$group.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="group", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         for ( i in 1:length(group.info)) { dat[, group.info[[i]]$varname ] <- group.info[[i]]$x }
                         weg.group  <- unique(unlist(lapply(group.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.group)>0)                                ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.group)," cases with missings on group variable.\n",sep=""))}
                      }
                      if(length(all.Names$DIF.var)>0)  {
                         dif.info <- lapply(all.Names$DIF.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="DIF", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         if ( remove.vars.DIF.missing == TRUE ) {
                              for ( uu in 1:length(dif.info)) { if (length(dif.info[[uu]]$wegDifMis) >0) {
                                    cat(paste("Remove item(s) which only have missing values in at least one group of DIF variable '",dif.info[[uu]]$varname,"'.\n", sep=""))
                                    namen.items.weg <- c(namen.items.weg,dif.info[[uu]]$wegDifMis) } }
                         }
                         if ( remove.vars.DIF.constant == TRUE ) {
                              for ( uu in 1:length(dif.info)) { if (length(dif.info[[uu]]$wegDifConst) >0) {
                                    cat(paste("Remove item(s) which are constant in at least one group of DIF variable '",dif.info[[uu]]$varname,"'.\n",sep=""))
                                    namen.items.weg <- c(namen.items.weg,dif.info[[uu]]$wegDifConst) } }
                         }
                         for ( i in 1:length(dif.info)) { dat[, dif.info[[i]]$varname ] <- dif.info[[i]]$x }
                         weg.dif  <- unique(unlist(lapply(dif.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.dif)>0)                                  ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.dif)," cases with missings on DIF variable.\n",sep=""))}
                      }
                      if(length(all.Names$weight.var)>0)  {
                         if(length(all.Names$weight.var)!=1) {stop("Use only one weight variable.")}
                         weight.info <- lapply(all.Names$weight.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="weight", itemdaten=dat[,all.Names[["variablen"]], drop = FALSE])})
                         for ( i in 1:length(weight.info)) { dat[, weight.info[[i]]$varname ] <- weight.info[[i]]$x }
                         weg.weight  <- unique(unlist(lapply(weight.info, FUN = function ( y ) {y$weg})))
                         if(length(weg.weight)>0)                               ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                           {cat(paste("Remove ",length(weg.weight)," cases with missings on weight variable.\n",sep=""))}
                      }                                                         ### untere Zeile, Achtung: group- und DIF- bzw. group- und HG-Variablen duerfen sich ueberschneiden!
                      namen.all.hg <- unique(c(all.Names$HG.var,all.Names$group.var,all.Names$DIF.var,all.Names$weight.var))
                      weg.all <- unique(c(weg.dif, weg.hg, weg.weight, weg.group))
                      perExHG <- NULL
                      if(length(weg.all)>0) {
                         cat(paste("Remove",length(weg.all),"case(s) overall due to missings on at least one explicit variable.\n"))
                         perExHG<- dat[weg.all, all.Names[["ID"]] ]
                         dat    <- dat[-weg.all,]
                      }
     ### Sektion 'Itemdatensatz zusammenbauen' (fuer Conquest ggf. mit Buchstaben statt Ziffern) ###
                      if(length(namen.items.weg)>0)  {
                         cat(paste("Remove ",length(unique(namen.items.weg))," test item(s) overall.\n",sep=""))
                         all.Names$variablen <- setdiff(all.Names$variablen, unique(namen.items.weg) )
                         qMatrix             <- qMatrix[match(all.Names$variablen, qMatrix[,1]),]
                      }
     ### Sektion 'Personen ohne gueltige Werte identifizieren und ggf. loeschen' ###
                      if(inherits(try(datL  <- melt(data = dat, id.vars = unique(unlist(all.Names[-match("variablen", names(all.Names))])), measure.vars = all.Names[["variablen"]], na.rm=TRUE)  ),"try-error"))  {
                         cat("W A R N I N G ! ! !   Error in melting for unknown reasons. Try workaround.\n"); flush.console()
                         allHG <- setdiff(unique(unlist(all.Names[-match("variablen", names(all.Names))])), all.Names[["ID"]] )
                         stopifnot(length(allHG)>0)                             ### dies ist ein Workaround, wenn "melt" fehltschlaegt (Fehler nicht reproduzierbar)
                         datL  <- melt(data = dat, id.vars = all.Names[["ID"]], measure.vars = all.Names[["variablen"]], na.rm=TRUE)
                         datL  <- merge(datL, dat[,unique(unlist(all.Names[-match("variablen", names(all.Names))]))], by = all.Names[["ID"]], all=TRUE)
                      }
                      wegNV <- setdiff(dat[,all.Names[["ID"]]], unique(datL[,all.Names[["ID"]]]))
                      perNA <- NULL
                      if(length(wegNV)>0)   {                                   ### identifiziere Faelle mit ausschliesslich missings
                         cat(paste("Found ",length(wegNV)," cases with missings on all items.\n",sep=""))
                         if( remove.no.answers == TRUE)  {
                             cat("Cases with missings on all items will be deleted.\n")
                             perNA<- dat[match(wegNV,dat[,all.Names[["ID"]]] ), all.Names[["ID"]]]
                             dat  <- dat[-match(wegNV,dat[,all.Names[["ID"]]] ) ,]
                         }
                         if( remove.no.answers == FALSE) {
                             cat("Cases with missings on all items will be kept.\n")
                             perNA<- NULL
                         }
                      }
     ### Sektion 'Summenscores fuer Personen pruefen' ###
                      minMax<- do.call("rbind", by ( data = datL, INDICES = datL[,"variable"], FUN = function ( v ) {
                               v[,"valueMin"] <- min(v[,"value"])               ### obere Zeile: hier wird variablenweise der kleinstmoegliche Wert gesucht
                               v[,"valueMax"] <- max(v[,"value"])               ### da der hier verwendete Longdatensatz 'datL' oben mit 'na.rm = TRUE' erzeugt wurde,
                               return(v)}))                                     ### sind hier diejenigen Personen mit ausschliesslich Missings bereits eliminiert
                      rm(datL)                                                  ### um Speicher zu sparen, wird der Longdatensatz wieder entfernt
                      datW  <- dcast(minMax, as.formula(paste(all.Names[["ID"]], "~variable",sep="")), value.var = "value")
                      datMin<- dcast(minMax, as.formula(paste(all.Names[["ID"]], "~variable",sep="")), value.var = "valueMin")
                      datMax<- dcast(minMax, as.formula(paste(all.Names[["ID"]], "~variable",sep="")), value.var = "valueMax")
                      rm(minMax)                                                ### Speicher sparen
                      allFal<- datW[ which ( rowSums ( datW[,-1], na.rm = TRUE ) == rowSums ( datMin[,-1], na.rm = TRUE ) ), all.Names[["ID"]] ]
                      allTru<- datW[ which ( rowSums ( datW[,-1], na.rm = TRUE ) == rowSums ( datMax[,-1], na.rm = TRUE ) ), all.Names[["ID"]] ]
                      per0  <- NULL; perA <- NULL
                      if(length(allFal)>0) {
                         num <- rowSums(datMax[ which ( datMax[,1] %in% allFal), -1], na.rm = TRUE)
                         numF<- data.frame ( id = allFal, anzahl = num)
                         numF<- data.frame(numF[sort(numF[,"anzahl"],decreasing=FALSE,index.return=TRUE)$ix,])
                         if ( nrow( numF) > 5) { auswahl  <- numF[c(1, round(nrow(numF)/2), nrow(numF)),] }  else { auswahl <- na.omit(numF[c(1, 2, nrow(numF)),]) }
                         cat(paste( length(allFal), " subject(s) do not solve any item:\n   ", paste(auswahl[,"id"], " (",auswahl[,"anzahl"]," false)",sep="",collapse=", ")," ... \n",sep=""))
                         weg0<- na.omit(match(allFal, dat[,all.Names[["ID"]]]))
                         per0<- dat[weg0, all.Names[["ID"]] ]
                         if (remove.failures == TRUE)  {
                             cat("   Remove subjects without any correct response.\n"); flush.console()
                             dat <- dat[-weg0,]
                         }
                      }
                      if(length(allTru)>0) {
                         num <- rowSums(datMax[ which ( datMax[,1] %in% allTru), -1], na.rm = TRUE)
                         numT<- data.frame ( id = allTru, anzahl = num)
                         numT<- data.frame(numT[sort(numT[,"anzahl"],decreasing=FALSE,index.return=TRUE)$ix,])
                         if ( nrow( numT) > 5) { auswahl  <- numT[c(1, round(nrow(numT)/2), nrow(numT)),] }  else { auswahl <- na.omit(numT[c(1, 2, nrow(numT)),]) }
                         cat(paste( length(allTru), " subject(s) solved each item: ", paste(auswahl[,"id"], " (",auswahl[,"anzahl"] ," correct)",sep="", collapse=", ")," ... \n",sep=""))
                         alle<- na.omit(match(allTru, dat[,all.Names[["ID"]]]))
                         perA<- dat[alle, all.Names[["ID"]] ]
                      }
                      rm(datMin); rm(datMax); rm(datW)                          ### Speicher sparen
     ### Sektion 'Verlinkung pruefen' ###
                      if(check.for.linking == TRUE) {                           ### Dies geschieht auf dem nutzerspezifisch reduzierten/selektierten Datensatz
                         linkNaKeep <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = FALSE, verbose = FALSE )
                         linkNaOmit <- checkLink(dataFrame = dat[,all.Names[["variablen"]], drop = FALSE], remove.non.responser = TRUE, verbose = FALSE )
                         if(linkNaKeep == FALSE & linkNaOmit == TRUE )  {cat("Note: Dataset is not completely linked. This is probably only due to missings on all cases.\n")}
                         if(linkNaKeep == TRUE )                        {cat("Dataset is completely linked.\n")}
                      }
     ### Sektion 'Anpassung der Methode (gauss, monte carlo) und der Nodes'
                      if(method == "montecarlo")  {
                        if(nodes < 100 ) {
    				               cat(paste("Warning: Due to user specification, only ",nodes," nodes are used for '",method,"' estimation. Please note or re-specify your analysis.\n",sep=""))
    				            }
                        if(is.null(nodes) )   {
                          cat(paste("'",method,"' has been chosen for estimation method. Number of nodes was not explicitly specified. Set nodes to 1000.\n",sep=""))
    				              if(software == "conquest") {nodes <- 1000}
      		                if(software == "tam" )     {nodes <- NULL; snodes <- 1000; QMC <- TRUE}
    				            }  else  { if(software == "tam" )     {snodes <- nodes; nodes <- NULL; QMC <- TRUE} }
    			           }
    			           if(method != "montecarlo") {
                        if ( is.null(nodes) )   {
                             cat(paste("Number of nodes was not explicitly specified. Set nodes to 20 for method '",method,"'.\n",sep=""))
    				                 if ( software == "conquest" ) { nodes <- 20 }
    				                 if ( software == "tam" )      { nodes <- seq(-6,6,len=20); snodes <- 0; QMC <- FALSE }
    				            }  else {
     				                 if ( software == "tam" )      { nodes <- seq(-6,6,len=nodes); snodes <- 0; QMC <- FALSE }
                        }
    				            if ( !is.null(seed)) {
                              if ( software == "conquest" ) {  cat("Warning! 'seed'-Parameter is appropriate only in Monte Carlo estimation method. (see conquest manual, p. 225) Recommend to set 'seed' to NULL.\n") }
                        }
    			           }
     ### Sektion 'Datensaetze softwarespezifisch aufbereiten: Conquest' ###
                      if(length(namen.all.hg)>0) {all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(dat[,ii]))))})} else {all.hg.char <- NULL}
                      if ( software == "conquest" )   {                         ### untere Zeile: wieviele character muss ich fuer jedes Item reservieren?
                          var.char  <- sapply(dat[,all.Names[["variablen"]], drop = FALSE], FUN=function(ii) {max(nchar(as.character(na.omit(ii))))})
                          no.number <- setdiff(1:length(var.char), grep("[[:digit:]]",var.char))
                          if(length(no.number)>0) {var.char[no.number] <- 1}    ### -Inf steht dort, wo nur missings sind, hier soll die Characterbreite auf 1 gesetzt sein
                          if(use.letters == TRUE)   {                           ### sollen Buchstaben statt Ziffern benutzt werden? Dann erfolgt hier Recodierung.
                             rec.statement <- paste(0:25,"='",LETTERS,"'",sep="",collapse="; ")
                             for (i in all.Names[["variablen"]])  {             ### Warum erst hier? Weil Pruefungen (auf Dichotomitaet etc. vorher stattfinden sollen)
                                  dat[,i] <- recode(dat[,i], rec.statement)}
                             var.char <- rep(1,length(all.Names[["variablen"]]))}## var.char muss nun neu geschrieben werden, da nun alles wieder einstellig ist!
                      }
     ### Sektion 'deskriptive Ergebnisse berechnen und durchschleifen' ###
                      daten   <- data.frame(ID=as.character(dat[,all.Names[["ID"]]]), dat[,namen.all.hg, drop = FALSE], dat[,all.Names[["variablen"]], drop = FALSE], stringsAsFactors = FALSE)
                      deskRes <- desk.irt(daten = daten, itemspalten = match(all.Names[["variablen"]], colnames(daten)), percent = TRUE)
                      crit    <- which (deskRes[,"valid"] < minNperItem)
                      if ( length(crit)>0) {
                           cat ( paste ( "Following ",length(crit), " items with less than ",minNperItem," item responses:\n",sep=""))
                           options(width=1000)
                           print(deskRes[crit,-match(c("item.nr", "Label", "KB", "Codes", "Abs.Freq", "Rel.Freq"), colnames(deskRes))], digits = 3)
                      }
                      discrim <- item.diskrim(daten,match(all.Names[["variablen"]], colnames(daten)))
                      if ( length ( all.Names[["schooltype.var"]] ) > 0 ) { ### jetzt ggf. noch schulformspezifische p-Werte, falls gewuenscht
                           deskS <- by ( data = dat, INDICES = dat[, all.Names[["schooltype.var"]] ], FUN = function ( st ) {
                                    drst <- desk.irt(daten = st, itemspalten = match(all.Names[["variablen"]], colnames(st)), percent = TRUE)
                                    colnames(drst) <- recode (colnames(drst) , paste0("'item.p'='item.p.",st[1,all.Names[["schooltype.var"]]],"'") )
                                    return(drst)})
                           for ( uu in 1:length( deskS) ) {
                                 matchU <- match(c("item.nr","Label", "KB", "cases", "Missing", "valid", "Codes" , "Abs.Freq", "Rel.Freq"), colnames(deskS[[uu]]))
                                 stopifnot ( length (which(is.na(matchU))) == 0 , ncol(deskS[[uu]]) - length ( matchU) == 2)
                                 deskRes <- merge ( deskRes, deskS[[uu]][,-matchU], by = "item.name", all = TRUE)
                           }
                      }
                      rm(dat)                                                   ### Speicher freigeben
                      lab <- data.frame(itemNr = 1:length(all.Names[["variablen"]]), item = all.Names[["variablen"]], stringsAsFactors = FALSE)
                      if(!is.null(anchor))  { ankFrame <- anker (lab = lab, prm = anchor, qMatrix = qMatrix, domainCol=domainCol, itemCol=itemCol, valueCol=valueCol ) } else { ankFrame <- NULL}
                      if ( software == "conquest" )   {
                          daten$ID <- gsub ( " ", "0", formatC(daten$ID, width=max(as.numeric(names(table(nchar(daten$ID)))))) )
                          fixed.width <- c(as.numeric(names(table(nchar(daten[,"ID"])))), all.hg.char, rep(max(var.char),length(var.char)))
     ### erstmal testen, ob die Characterzahl wirklich einheitlich ist ... datensatz wird dazu nicht auf festplatte geschrieben
                          txt  <-  capture.output ( write.fwf(daten , colnames = FALSE,rownames = FALSE, sep="",quote = FALSE,na=".", width=fixed.width))
                          stopifnot(length(table(nchar(txt)))==1)               ### Check: hat der Resultdatensatz eine einheitliche Spaltenanzahl? Muss unbedingt sein!
                          rm(txt)                                               ### Speicher sparen
                          write.fwf(daten , file.path(dir,paste(analysis.name,".dat",sep="")), colnames = FALSE,rownames = FALSE, sep="",quote = FALSE,na=".", width=fixed.width)
                          colnames(lab) <- c("===>","item")                     ### schreibe Labels!
                          write.table(lab,file.path(dir,paste(analysis.name,".lab",sep="")),col.names = TRUE,row.names = FALSE, dec = ",", sep = " ", quote = FALSE)
                          if(!is.null(conquest.folder))     {
                             batch <- paste( normalize.path(conquest.folder),paste(analysis.name,".cqc",sep=""), sep=" ")
                             write(batch, file.path(dir,paste(analysis.name,".bat",sep="")))}
                          foo <- gen.syntax(Name=analysis.name, daten=daten, all.Names = all.Names, namen.all.hg = namen.all.hg, all.hg.char = all.hg.char, var.char= max(var.char), model=qMatrix, anchored=anchor, pfad=dir, n.plausible=n.plausible, compute.fit = compute.fit,
                                            constraints=constraints, std.err=std.err, distribution=distribution, method=method, n.iterations=n.iterations, nodes=nodes, p.nodes=p.nodes, f.nodes=f.nodes, converge=converge,deviancechange=deviancechange, equivalence.table=equivalence.table, use.letters=use.letters, model.statement=model.statement, conquest.folder = conquest.folder, allowAllScoresEverywhere = allowAllScoresEverywhere, seed = seed, export = export)
                          if(!is.null(anchor))  {
                             write.table(ankFrame[["resConquest"]], file.path(dir,paste(analysis.name,".ank",sep="")) ,sep=" ", col.names = FALSE, row.names = FALSE, quote = FALSE)
                          }
     ### wenn Conquest gewaehlt, dann ggf. Logfile umbenennen, falls es bereits (unter demselben namen) existiert
                          if(file.exists( file.path ( dir,  paste(analysis.name,".log",sep=""))) )  {
                             cat(paste("Found existing log file '",paste(analysis.name,".log",sep=""), "' in folder '",dir,"'\nConquest analysis will overwrite log file. Original log file will be saved as '",paste(analysis.name,"_old.log'\n",sep=""),sep=""))
                             do <- file.rename(from = file.path(dir, paste(analysis.name,".log",sep="")), to = file.path(dir, paste(analysis.name,"_old.log",sep="")))
                          }
     ### Sektion 'Rueckgabeobjekt bauen', hier fuer Conquest                    ### setze Optionen wieder in Ausgangszustand
                          options(scipen = original.options); flush.console()   ### Achtung: setze Konsolenpfade in Hochkommas, da andernfalls keine Leerzeichen in den Ordner- bzw. Dateinamen erlaubt sind!
                          ret <- list ( software = software, input = paste("\"", file.path(dir, paste(analysis.name,"cqc",sep=".")), "\"", sep=""), conquest.folder = paste("\"", conquest.folder, "\"", sep=""), dir=dir, analysis.name=analysis.name, model.name = analysis.name, qMatrix=qMatrix, all.Names=all.Names, deskRes = deskRes, discrim = discrim, perNA=perNA, per0=per0, perA = perA, perExHG = perExHG, itemsExcluded = namen.items.weg, daten=daten)
                          class(ret) <-  c("defineConquest", "list")
                          return ( ret )  }
     ### Sektion 'Rueckgabeobjekt fuer tam'
                      if ( software == "tam" )   {
                          cat(paste("Q matrix specifies ",ncol(qMatrix)-1," dimension(s).\n",sep=""))
                          control <- list ( snodes = snodes , QMC=QMC, convD = deviancechange ,conv = converge , convM = .0001 , Msteps = 4 , maxiter = n.iterations, max.increment = 1 ,
                                     min.variance = .001 , progress = progress , ridge=0 , seed = seed , xsi.start0=FALSE,  increment.factor=increment.factor , fac.oldxsi= fac.oldxsi)
                          if ( !is.null(nodes)) { control$nodes <- nodes }
                          ret     <- list ( software = software, constraint = match.arg(constraints) , qMatrix=qMatrix, anchor=ankFrame[["resTam"]],  all.Names=all.Names, daten=daten, irtmodel=irtmodel, est.slopegroups = est.slopegroups, guessMat=guessMat, control = control, n.plausible=n.plausible, dir = dir, analysis.name=analysis.name, deskRes = deskRes, discrim = discrim, perNA=perNA, per0=per0, perA = perA, perExHG = perExHG, itemsExcluded = namen.items.weg, fixSlopeMat = fixSlopeMat, slopeMatDomainCol=slopeMatDomainCol, slopeMatItemCol=slopeMatItemCol, slopeMatValueCol=slopeMatValueCol )
                          class(ret) <-  c("defineTam", "list")
                          return ( ret )    }   }  }

### Hilfsfunktionen fuer defineModel
.checkContextVars <- function(x, varname, type, itemdaten, suppressAbort = FALSE)   {
                     if(missing(varname))  {varname <- "ohne Namen"}
                     if(class(x) != "numeric")  {                               ### ist Variable numerisch?
                        if (type == "weight") {stop(paste(type, " variable has to be 'numeric' necessarily. Automatic transformation is not recommended. Please transform by yourself.\n",sep=""))}
                        cat(paste(type, " variable has to be 'numeric'. Variable '",varname,"' of class '",class(x),"' will be transformed to 'numeric'.\n",sep=""))
                        x <- unlist(asNumericIfPossible(dataFrame = data.frame(x, stringsAsFactors = FALSE), transform.factors = TRUE, maintain.factor.scores = FALSE, verbose=FALSE, ignoreAttributes = TRUE))
                        if(class(x) != "numeric")  {                            ### erst wenn asNumericIfPossible fehlschlaegt, wird mit Gewalt numerisch gemacht, denn fuer Conquest MUSS es numerisch sein
                           x <- as.numeric(as.factor(x))
                        }
                        cat(paste("    '", varname, "' was converted into numeric variable of ",length(table(x))," categories. Please check whether this was intended.\n",sep=""))
                        if(length(table(x)) < 12 ) { cat(paste("    Values of '", varname, "' are: ",paste(names(table(x)), collapse = ", "),"\n",sep=""))}
                     }
                     toRemove<- NULL
                     mis     <- length(table(x))
                     if(mis == 0 )  {
                        if ( suppressAbort == FALSE ) {
                             stop(paste("Error: ",type," Variable '",varname,"' without any values.",sep=""))
                        }  else  {
                             cat(paste("Warning: ",type," Variable '",varname,"' without any values. '",varname,"' will be removed.\n",sep=""))
                             toRemove <- varname
                        }
                     }
                     if(mis == 1 )  {
                        if ( suppressAbort == FALSE ) {
                             stop(paste("Error: ",type," Variable '",varname,"' is a constant.",sep=""))
                        }  else  {
                             cat(paste("Warning: ",type," Variable '",varname,"' is a constant. '",varname,"' will be removed.\n",sep=""))
                             toRemove <- varname
                        }
                     }
                     if(type == "DIF" | type == "group") {if(mis > 10)   {cat(paste("Serious warning: ",type," Variable '",varname,"' with more than 10 categories. Recommend recoding. \n",sep=""))}}
                     wegDifMis <- NULL; wegDifConst <- NULL; char <- 1; weg <- which(is.na(1:12))
                     if ( is.null(toRemove)) {
                          char    <- max(nchar(as.character(na.omit(x))))
                          weg     <- which(is.na(x))
                          if(length(weg) > 0 ) {cat(paste("Warning: Found ",length(weg)," cases with missing on ",type," variable '",varname,"'. Conquest probably will collapse unless cases are not deleted.\n",sep=""))}
                          if(type == "DIF" ) {
                                        if(mis > 2 )   {cat(paste(type, " Variable '",varname,"' does not seem to be dichotomous.\n",sep=""))}
                                        n.werte <- lapply(itemdaten, FUN=function(iii){by(iii, INDICES=list(x), FUN=table)})
                                        completeMissingGroupwise <- data.frame(t(sapply(n.werte, function(ll){lapply(ll, FUN = function (uu) { length(uu[uu>0])}  )})), stringsAsFactors = FALSE)
                                        for (iii in seq(along=completeMissingGroupwise)) {
                                             missingCat.i <- which(completeMissingGroupwise[,iii] == 0)
                                             if(length(missingCat.i) > 0) {
                                                cat(paste("Warning: Following items with no values in ",type," variable '",varname,"', group ",iii,": \n",sep=""))
                                                wegDifMis <- c(wegDifMis, rownames(completeMissingGroupwise)[missingCat.i] )
                                                cat(paste(rownames(completeMissingGroupwise)[missingCat.i],collapse=", ")); cat("\n")
                                             }
                                             constantCat.i <- which(completeMissingGroupwise[,iii] == 1)
                                             if(length(constantCat.i) > 0) {
                                                cat(paste("Warning: Following items are constants in ",type," variable '",varname,"', group ",iii,":\n",sep=""))
                                                wegDifConst <- c(wegDifConst, rownames(completeMissingGroupwise)[constantCat.i] )
                                                cat(paste(rownames(completeMissingGroupwise)[constantCat.i],collapse=", ")); cat("\n")
                                             }
                                        }
                          }
                     }
                     return(list(x = x, char = char, weg = weg, varname=varname, wegDifMis = wegDifMis, wegDifConst = wegDifConst, toRemove = toRemove))}

.substituteSigns <- function(dat, variable, all.Names = NULL ) {
                    if(!is.null(variable)) {
           					   variableNew <- tolower(gsub("_|\\.|-", "", variable))
     ### DIF-variablen duerfen zusaetzlich keine Zahlen enthalten
                       if ( !is.null(all.Names)) {
                            if (!is.null(all.Names[["DIF.var"]])) {
                                variableNew <- unlist(isLetter(variableNew))
                            }
                       }
                       cols        <- match(variable, colnames(dat))
           					   return(data.frame(cols=cols, old=variable,new=variableNew, stringsAsFactors = FALSE))
           					}
                    if(is.null(variable)) {return(data.frame(old=TRUE,new=TRUE))}
                    }

checkQmatrixConsistency <-  function(qmat) {
             if(class(qmat) != "data.frame")    { qmat     <- data.frame(qmat, stringsAsFactors = FALSE)}
             if(class(qmat[,1]) != "character") { qmat[,1] <- as.character(qmat[,1])}
             nClass<- sapply(qmat, class)
             ind   <- which (!nClass %in% c("integer", "numeric"))
             if ( length ( ind ) > 1) {
                  cat(paste("Warning: Found non-numeric indicator column(s) in the Q matrix. Transform column(s) '",paste(colnames(qmat)[ind[-1]], collapse = "', '") ,"' into numeric format.\n",sep=""))
                  for ( a in ind[-1] ) { qmat[,a] <- as.numeric(as.character(qmat[,a] ))}
             }
             werte <- tableUnlist(qmat[,-1,drop=FALSE], useNA="always")
             if(length(setdiff( names(werte) , c("0","1", "NA")))<0) {stop("Q matrix must not contain entries except '0' and '1'.\n")}
             if(werte[match("NA", names(werte))] > 0) {stop("Missing values in Q matrix.\n")}
             doppel<- which(duplicated(qmat[,1]))
             if(length(doppel)>0) {
                cat("Found duplicated elements in the item id column of the q matrix. Duplicated elements will be removed.\n")
                chk  <- table(qmat[,1])
                chk  <- chk[which(chk > 1)]
                chkL <- lapply(names(chk), FUN = function ( ch ) {
                        qChk <- qmat[which(qmat[,1] == ch),]
                        pste <- apply(qChk, 1, FUN = function ( x ) { paste(x[-1], collapse="")})
                        if( !all ( pste == pste[1] )) { stop("Inconsistent q matrix.\n")}
                        })
                qmat <- qmat[!duplicated(qmat[,1]),]
             }
             zeilen<- apply(qmat, 1, FUN = function ( y ) { all ( names(table(y[-1])) == "0")  })
             weg   <- which(zeilen == TRUE)
             if(length(weg)>0) {
                cat(paste("Note: Following ",length(weg)," item(s) in Q matrix do not belong to any dimension. Delete these item(s) from Q matrix.\n",sep=""))
                cat("    "); cat(paste(qmat[weg,1],collapse=", ")); cat("\n")
                qmat  <- qmat[-weg,]
             }
             return(qmat)}


checkLink <- function(dataFrame, remove.non.responser = FALSE, sysmis = NA, verbose = TRUE)   {
             if(!is.na(sysmis))  {
               na <- which(is.na(dataFrame))
               if(length(na)>0)  {
                  cat(paste("Warning: '",sysmis,"' was specified to denote 'sysmis' in the data. ",length(na)," 'NA'-values were found in the dataset anyway. \n         Hence, ",sysmis," and 'NA' will be handled as 'sysmis'.\n",sep=""))
               }
               dataFrame <- as.data.frame(lapply(dataFrame, FUN=function(ii) {recode(ii, paste(sysmis,"= NA", collapse = "; ") ) } ) )
             }
             if ( remove.non.responser == TRUE ) {
                na <- which( rowSums(is.na(dataFrame)) == ncol ( dataFrame ) )
                if(length(na)>0) {
                   dataFrame <- dataFrame[-na,]
                   if(verbose == TRUE ) {cat(paste("Remove ",length(na)," cases with missing on all items.\n", sep = ""))}
                }
             }
             non.missing.cases <- lapply(dataFrame, FUN=function(ii) {which(!is.na(ii))})
             all.cases <- non.missing.cases[[1]]
             i <- 2
             total.abbruch     <- FALSE
             while( (i < length(non.missing.cases) + 1 ) & !total.abbruch )  {
                  if(length( intersect(all.cases,non.missing.cases[[i]])) > 0 )  {
                     all.cases <- unique(c(all.cases, non.missing.cases[[i]] ) )
                  }  else   {
                     overlap        <- FALSE
                     remain.columns <- length(non.missing.cases) + 1 - i
                     ii             <- 1
                     while (overlap == FALSE & ii < remain.columns )  {
                           non.missing.cases <- non.missing.cases[c(setdiff(1:length(non.missing.cases),i),i)]
                          if(length( intersect(all.cases,non.missing.cases[[i]])) > 0 ) {overlap <- TRUE}
                           ii <- ii + 1
                     }
                     if (overlap == FALSE) {total.abbruch <- TRUE}
                     if (overlap == TRUE)  {all.cases <- unique(c(all.cases, non.missing.cases[[i]] ) ) }
                  }
                  i <- i + 1
             }
             if (length(all.cases) != nrow(dataFrame))   {
                if (verbose == TRUE) {cat("WARNING! Dataset is not completely linked.\n") }
                if ( remove.non.responser == TRUE ) {
                     missed <- setdiff ( 1:nrow(dataFrame), all.cases)
                     misFra <- melt ( data.frame ( id = 1:length(missed), dataFrame[missed,]), id.vars = "id", na.rm=TRUE)
                     cat ( paste ( "W A R N I N G !   Dataset is NOT completely linked (even if cases with missings on all items are removed).\n                  ",length(missed)," cases unconnected. Following items are unconnected: \n",sep=""))
                     cat("                  "); cat ( paste ( unique(as.character(misFra[,"variable"])), collapse = ", ")); cat("\n")
                }
                return(FALSE)
             }
             if (length(all.cases) == nrow(dataFrame))   {
                if (verbose == TRUE) {cat("Dataset is completely linked.\n") }
                return(TRUE)
             }  }

converged<- function (dir, logFile) {
            isConv <- TRUE
            if (!file.exists(file.path ( dir, logFile ))) {
                 cat(paste("Warning: Model seems not to have converged. Cannot find log file '",file.path ( dir, logFile ),"'.\n",sep=""))
                 isConv <- FALSE
            }  else  {
                 logF  <- scan(file = file.path ( dir, logFile ), what="character",sep="\n",quiet=TRUE)
                 if(length(logF) == 0 ) {
                    cat(paste("Warning: Model seems not to have converged. Log file '",file.path ( dir, logFile ),"' is empty.\n",sep=""))
                    isConv <- FALSE
                 }  else  {
                    last  <- logF[length(logF)]
                    if ( !crop(last) == "=>quit;" ) {
                       if ( length( grep("quit;" , last)) == 0 ) {
                           cat(paste("Warning: Model seems not to have converged. Log file unexpectedly finishs with '",last,"'.\nReading in model output might fail.\n", sep=""))
                           isConv <- FALSE
                       }  }  }  }
            return(isConv)  }

getConquestResults<- function(path, analysis.name, model.name, qMatrix, all.Names, abs.dif.bound , sig.dif.bound, p.value, deskRes, discrim, omitFit, omitRegr, omitWle, omitPV, daten, Q3=Q3, q3theta=q3theta, q3MinObs =  q3MinObs, q3MinType = q3MinType) {
         allFiles <- list.files(path=path, pattern = analysis.name, recursive = FALSE)
         qL       <- melt(qMatrix, id.vars = colnames(qMatrix)[1], variable.name = "dimensionName", na.rm=TRUE)
         qL       <- qL[which(qL[,"value"] != 0 ) , ]
         varName  <- colnames(qMatrix)[1]
         ret      <- NULL                                                       ### Rueckgabeobjekt initialisieren
    ### Sektion 'Konvergenz pruefen' (log)
         logFile  <- paste(analysis.name, "log", sep=".")
         isConv   <- converged ( dir = path, logFile = logFile )
         isPoly   <- length(unique(deskRes[,"Codes"]))>1                        ### war modell polytom? damit das geht, muss es immer deskriptive Statistiken geben, muss also in 'defineModel' obligatorisch sein!
    ### Sektion 'Itemparameter auslesen' (itn)
         itnFile  <- paste(analysis.name, "itn", sep=".")
         if (!itnFile %in% allFiles) {
             cat("Cannot find Conquest itn-file.\n")
         } else {
             itn  <- get.itn( file.path(path, itnFile) )
             allID<- c("dif.name", "dif.value", "item.name", "Label")
             drin <- allID[which(allID %in% colnames(itn))]
             # itn1 <- data.frame ( model = model.name, source = "conquest", var1 = itnS[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],itnS[,"item.name"]),"dimensionName"], par = "itemP",  derived.par = NA, value = as.numeric(itnS[,"item.p"]), stringsAsFactors = FALSE)
             # itn2 <- data.frame ( model = model.name, source = "conquest", var1 = itnS[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],itnS[,"item.name"]),"dimensionName"], par = "Nvalid",  derived.par = NA, value = as.numeric(itnS[,"n.valid"]), stringsAsFactors = FALSE)
             itnL <- melt(itn, id.vars = drin, measure.vars = "pt.bis", value.name = "ptBis", variable.name = "pointBiserialCorrelation", na.rm=FALSE)
             both <- merge(qL, itnL, by.x = colnames(qMatrix)[1], by.y = "item.name", all=TRUE)
             drin2<- setdiff ( drin, "item.name")
             both[,"var2"] <- apply(X = both, MARGIN = 1, FUN = function ( zeile ) { paste( names ( zeile[drin2]), zeile[drin2], sep="=", collapse= ", ") })
             itn3 <- data.frame ( model = model.name, source = "conquest", var1 = both[,colnames(qMatrix)[1]], var2 = NA , type = "fixed", indicator.group = "items", group = both[,"dimensionName"], par = "ptBis",  derived.par = both[,"var2"], value = as.numeric(both[,"ptBis"]), stringsAsFactors = FALSE)
             ret  <- rbind(ret, itn3)
    ### Achtung!! wenn das Modell polytom war, muss p-Wert aus 'itn'-File ausgelesen werden!
             if ( isPoly == TRUE ) {
                  pval<- melt(itn, id.vars = drin, measure.vars = "Rel.Freq", variable.name = " itemP", value.name = "pval", na.rm=FALSE)
                  both<- merge(qL, pval, by.x = colnames(qMatrix)[1], by.y = "item.name", all=TRUE)
                  dri <- setdiff ( drin, "item.name")
                  both[,"var2"] <- apply(X = both, MARGIN = 1, FUN = function ( zeile ) { paste( names ( zeile[dri]), zeile[dri], sep="=", collapse= ", ") })
                  itn4 <- data.frame ( model = model.name, source = "conquest", var1 = both[,colnames(qMatrix)[1]], var2 = NA , type = "fixed", indicator.group = "items", group = both[,"dimensionName"], par = "itemP",  derived.par = both[,"var2"], value = as.numeric(both[,"pval"])/100, stringsAsFactors = FALSE)
                  ret  <- rbind(ret, itn4)
             }
         }
    ### Sektion 'Itemparameter auslesen' (shw)
         shwFile  <- paste(analysis.name, "shw", sep=".")
         if (!shwFile %in% allFiles) {
             cat("Cannot find Conquest showfile.\n")
         } else {
             shw  <- get.shw( file = file.path(path, shwFile) )                 ### Untere Zeile: Dimensionen analog zu Bezeichnung in Q Matrix benennen
             if(is.null( dim(shw$cov.structure) )) {from <- NA} else { from <- shw$cov.structure[-ncol(shw$cov.structure),1]}
             altN <- data.frame ( nr = 1:(ncol(qMatrix)-1), pv = paste("dim", 1:(ncol(qMatrix)-1),sep="."), from = from ,  to = colnames(qMatrix)[-1], stringsAsFactors = FALSE)
             shw[["item"]]  <- merge(shw[["item"]], qL[,-match("value", colnames(qL))], by.x = "item", by.y = colnames(qMatrix)[1], all=TRUE)
             shw1 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = NA, value = as.numeric(shw$item[,"ESTIMATE"]), stringsAsFactors = FALSE)
             shw2 <- data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items",group = shw$item[,"dimensionName"], par = "est",  derived.par = "se", value = as.numeric(shw$item[,"ERROR"]), stringsAsFactors = FALSE)
             toOff<- shw2[ which(is.na(shw2[,"value"])), "var1"]
             if(length(toOff)>0) {
                shw1[match(toOff, shw1[,"var1"]), "par"] <- "offset"
                shw2  <- shw2[-which(is.na(shw2[,"value"])),]                   ### entferne Zeilen aus shw2, die in der "value"-Spalte NA haben
             }
             if(!is.null ( deskRes ) ) {
                deskR<- merge(deskRes, qL[,-match("value", colnames(qL))], by.x = "item.name", by.y = colnames(qMatrix)[1], all=TRUE)
                if ( isPoly == FALSE ) {
                     shw3 <- data.frame ( model = model.name, source = "conquest", var1 = deskR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,"item.p"], stringsAsFactors = FALSE)
                }  else  {
                     shw3 <- NULL
                }
                shw4 <- data.frame ( model = model.name, source = "conquest", var1 = deskR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "Nvalid",  derived.par = NA, value = deskR[,"valid"], stringsAsFactors = FALSE)
                shw4 <- shw4[!duplicated(shw4[,"var1"]),]
    ### Achtung! wenn in dem 'deskRes'-Objekt noch mehr p-Werte (schulformspezifische p-Werte drinstehen, werden die jetzt auch in die Ergebnisstruktur eingetragen)
                cols <- setdiff ( colnames(deskR)[grep("^item.p", colnames(deskR))], "item.p")
                if ( length ( cols ) > 0 ) {
                     colsR <- data.frame ( original = cols, reduziert = removePattern ( string = cols, pattern = "item.p.") , stringsAsFactors = FALSE)
                     shw31 <- do.call("rbind", apply ( colsR, MARGIN = 1, FUN = function ( zeile ) { data.frame ( model = model.name, source = "conquest", var1 = deskR[,"item.name"], var2 = zeile[["reduziert"]] , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,zeile[["original"]]], stringsAsFactors = FALSE) }))
                }  else {
                     shw31 <- NULL
                }

             }  else  {
                shw3 <- shw4 <- shw31 <- NULL
             }
             if( !is.null(discrim) )  {
                 discR<- merge(discrim, qL[,-match("value", colnames(qL))], by.x = "item.name", by.y = colnames(qMatrix)[1], all=TRUE)
                 shw5 <- data.frame ( model = model.name, source = "conquest", var1 = discR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = discR[,"dimensionName"], par = "itemDiscrim",  derived.par = NA, value = discR[,"item.diskrim"], stringsAsFactors = FALSE)
             }  else  {
                 shw5 <- NULL
             }
             ret  <- rbind(ret, shw1, shw2, shw3, shw31, shw4, shw5)            ### Rueckgabeobjekt befuellen, danach infit auslesen
             ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = "infit", value = as.numeric(shw$item[,"MNSQ.1"]), stringsAsFactors = FALSE),
                                data.frame ( model = model.name, source = "conquest", var1 = shw$item[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = shw$item[,"dimensionName"], par = "est",  derived.par = "outfit", value = as.numeric(shw$item[,"MNSQ"]), stringsAsFactors = FALSE) )
             if(length(shw) > 4 )  {                                            ### ggf. Parameter zusaetzlicher Conquest-Terme einlesen
                read  <- 2 : (length(shw) - 3)                                  ### Diese Terme muessen eingelesen werden
                for ( i in names(shw)[read] ) {
                     cols <- unlist(isLetter(i))                                ### versuche Spalte(n) zu identifizieren
                     if( !all(cols %in% colnames(shw[[i]])) ) {
                         cat(paste("Cannot identify variable identifier for term '",i,"' in file '",shwFile,"'. Skip procedure.\n",sep=""))
                     }  else  {
                         if(length(cols) == 1 ) {var1 <- paste( cols, shw[[i]][,cols],sep="_") } else { var1 <- unlist(apply(shw[[i]][,cols], MARGIN=1, FUN = function ( y ) {
                            paste ( unlist(lapply ( 1:length(y), FUN = function ( yy ) { paste(names(y)[yy], y[yy],sep="_")})), sep="", collapse = "_X_")  }))}
                         if(ncol(qMatrix) != 2 ){
                            cat(paste("Warning: Cannot identify the group the term '",i,"' in file '",shwFile,"' belongs to. Insert 'NA' to the 'group' column.\n",sep=""))
                            gr <- NA
                         }  else { gr <- colnames(qMatrix)[2]}
                         for ( u in c("ESTIMATE", "MNSQ", "MNSQ.1", "ERROR")) { ### Hotfix
                               if ( !class ( shw[[i]][,u] ) %in% c("numeric", "integer")) {
                                    cat(paste("Warning: Expect column '",u,"' in file '",shwFile,"' (statement '",i,"') to be numeric. Current column format is: '",class ( shw[[i]][,u] ),"'. Column will be transformed.\n",sep=""))
                                    shw[[i]][,u] <- as.numeric(shw[[i]][,u])
                               }
                         }
                         shwE <- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = NA, value = shw[[i]][,"ESTIMATE"], stringsAsFactors = FALSE)
                         shwE2<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "infit", value = shw[[i]][,"MNSQ.1"], stringsAsFactors = FALSE)
                         shwE3<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "outfit", value = shw[[i]][,"MNSQ"], stringsAsFactors = FALSE)
                         shwSE<- data.frame ( model = model.name, source = "conquest", var1 = var1, var2 = NA , type = "fixed", indicator.group = "items", group = gr, par = "est",  derived.par = "se", value = shw[[i]][,"ERROR"], stringsAsFactors = FALSE)
                         toOff<- shwSE[ which(is.na(shwSE[,"value"])), "var1"]
                         if(length(toOff)>0) {
                            shwE[match(toOff, shwE[,"var1"]), "par"] <- "offset"
                            shwSE <- shwSE[-which(is.na(shwSE[,"value"])),] }
                         ret  <- rbind(ret, shwE, shwE2, shwE3, shwSE)
                     }}}
    ### Sektion 'Populationsparameter auslesen' (shw)
             if(ncol(qMatrix) == 2) {                                           ### eindimensionaler Fall
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = colnames(qMatrix)[2], var2 = NA , type = "distrpar", indicator.group = NA, group = "persons", par = "var",  derived.par = NA, value = shw$cov.structure, stringsAsFactors = FALSE))
             }  else  {                                                         ### mehrdimensional
                stopifnot(nrow(shw$cov.structure) == ncol(qMatrix))             ### (Residual-)Varianzen und (Residual-)Korrelationen der lat. Dimensionen
                shw$cov.structure[-nrow(shw$cov.structure),1] <- colnames(qMatrix)[-1]
                cov1 <- shw$cov.structure[,-1]
                cov1[upper.tri(shw$cov.structure[,-1])] <- NA
                cov1 <- data.frame ( shw$cov.structure[,1,drop=FALSE], cov1, stringsAsFactors = FALSE)
                colnames(cov1)[-1] <- cov1[-nrow(cov1),1]
                cov2 <- facToChar( dataFrame = melt(cov1[-nrow(cov1),], id.vars = colnames(cov1)[1], na.rm=TRUE))
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = c(colnames(qMatrix)[-1], cov2[,1]), var2 = c(rep(NA, ncol(qMatrix)-1), cov2[,2]) , type = "random", indicator.group = NA, group = "persons", par = c(rep("var",ncol(qMatrix)-1), rep("correlation", nrow(cov2))) ,  derived.par = NA, value = unlist(c(cov1[nrow(cov1),-1], cov2[,3])) , stringsAsFactors = FALSE))
             }
    ### Sektion 'Regressionsparameter auslesen' (shw)
             if(nrow(shw$regression)>1) {
                reg  <- shw$regression                                          ### untere Zeile: Dimensionen analog zu Q matrix umbenennen
                if(!is.null( dim(shw$cov.structure) )) {
                   for ( i in 1:nrow(altN)) { colnames(reg) <- gsub(altN[i,"from"], altN[i,"to"], colnames(reg))}
                }  else  {
                   index  <- grep("_$", colnames(reg))
                   colnames(reg)[index] <- paste(colnames(reg)[index], altN[,"to"], sep="")
                }
                regL <- melt(reg, id.vars = colnames(reg)[1], measure.vars = colnames(reg)[-c(1, ncol(reg))], na.rm=TRUE)
                foo  <- data.frame ( do.call("rbind", strsplit(as.character(regL[,"variable"]), "_")), stringsAsFactors = FALSE)
                colnames(foo) <- c("par", "group")
                foo[,"derived.par"] <- recode(foo[,"par"], "'error'='se'; else = NA")
                foo[,"par"] <- "est"
                regL <- data.frame ( regL[,-match("variable", colnames(regL)), drop=FALSE], foo, stringsAsFactors = FALSE)
                regL[,"reg.var"] <- recode(regL[,"reg.var"], "'CONSTANT'='(Intercept)'")
                ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = regL[,"reg.var"], var2 = NA , type = "regcoef", indicator.group = NA, group = regL[,"group"], par = regL[,"par"],  derived.par = regL[,"derived.par"], value = regL[,"value"] , stringsAsFactors = FALSE))
             }
    ### Sektion 'Modellindizes auslesen' (shw)
             ret  <- rbind(ret, data.frame ( model = model.name, source = "conquest", var1 = NA, var2 = NA , type = "model", indicator.group = NA, group = NA, par = c("deviance", "Npar"),  derived.par = NA, value = shw$final.deviance , stringsAsFactors = FALSE))
         }                                                                      ### schliesst die Bedingung 'shw file vorhanden'
    ### Sektion 'Personenparameter auslesen' (wle)
         wleFile  <- paste(analysis.name, "wle", sep=".")
         if ( omitWle == FALSE ) {
              if (!wleFile %in% allFiles) {
                  cat("Cannot find Conquest WLE file.\n")
              } else {
                  wle  <- get.wle( file.path(path, wleFile) )
                  for ( i in 1:nrow(altN)) { colnames(wle) <- gsub(  paste(".",altN[i,"nr"],"$",sep=""), paste("_", altN[i,"to"],sep="") , colnames(wle))}
                  wleL <- melt(wle, id.vars = "ID", measure.vars = colnames(wle)[-c(1:2)], na.rm=TRUE)
                  foo  <- data.frame ( do.call("rbind", strsplit(as.character(wleL[,"variable"]), "_")), stringsAsFactors = FALSE)
                  colnames(foo) <- c("par", "group")
                  foo[,"derived.par"] <- recode(foo[,"par"], "'wle'='est'; 'std.wle'='se'; else=NA")
                  foo[,"par"]         <- recode(foo[,"par"], "'wle'='wle'; 'std.wle'='wle'; 'n.solved'='NitemsSolved'; 'n.total'='NitemsTotal'")
                  wleL <- data.frame ( wleL[,-match("variable", colnames(wleL)), drop=FALSE], foo, stringsAsFactors = FALSE)
                  ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = wleL[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = wleL[,"group"], par = wleL[,"par"],  derived.par = wleL[,"derived.par"], value = wleL[,"value"] , stringsAsFactors = FALSE))
              }
         }
    ### Sektion 'Personenparameter auslesen' (PVs)
         pvFile   <- paste(analysis.name, "pvl", sep=".")
         if ( omitPV == FALSE ) {
              if (!pvFile %in% allFiles) {
                  cat("Cannot find Conquest PV file.\n")
              } else {
                  pv   <- get.plausible( file.path(path, pvFile), forConquestResults = TRUE )
                  rec  <- paste("'",altN[,"pv"] , "' = '" , altN[,"to"], "'" ,sep = "", collapse="; ")
                  pv$pvLong[,"variable"] <- recode( pv$pvLong[,"variable"], rec)
                  ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = pv$pvLong[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = pv$pvLong[,"variable"], par = "pv",  derived.par = paste("pv", as.numeric(pv$pvLong[,"PV.Nr"]),sep=""), value = as.numeric(pv$pvLong[,"value"]) , stringsAsFactors = FALSE))
                  eaps <- melt ( data.frame ( pv$pvWide[,"ID", drop=FALSE], pv$eap, stringsAsFactors = FALSE), id.vars = "ID", na.rm=TRUE)
                  foo  <- data.frame ( do.call("rbind", strsplit(as.character(eaps[,"variable"]), "_")), stringsAsFactors = FALSE)
                  colnames(foo) <- c("par", "group")
                  foo[,"derived.par"] <- recode(foo[,"par"], "'eap'='est'; 'se.eap'='se'; else=NA")
                  foo[,"par"]         <- "eap"
                  foo[,"group"]       <- recode(tolower(foo[,"group"]), rec)
                  ret  <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = eaps[,"ID"], var2 = NA , type = "indicator", indicator.group = "persons", group = foo[,"group"], par = "eap",  derived.par = foo[,"derived.par"], value = eaps[,"value"] , stringsAsFactors = FALSE))
              }
         }
         if(!is.null(ret)) {
             attr(ret, "isConverged") <- isConv
             attr(ret, "available")   <- list ( itn = itnFile %in% allFiles, shw = shwFile %in% allFiles, wle = (wleFile %in% allFiles) & (omitWle == FALSE), pv = (pvFile %in% allFiles) & (omitPV == FALSE))
         }
    ### Sektion 'Q3 erzeugen'
         theta <- NULL
         if ( Q3 == TRUE ) {
              if ( q3theta == "pv") {
                  if ( omitPV == TRUE ) {
                      cat("Cannot compute Q3 if 'omitPV == TRUE' and 'q3theta == \"pv\"'. Skip computation.\n")
                  }  else  {
                      theta <- pv[["pvWide"]][,2:3]
                  }
              }
              if ( q3theta == "wle") {
                  if ( omitWle == TRUE ) {
                      cat("Cannot compute Q3 if 'omitWle == TRUE' and 'q3theta == \"wle\"'. Skip computation.\n")
                  }  else  {
                      colW  <- grep("^wle", colnames(wle))[1]
                      theta <- wle[,c(2,colW)]
                  }
              }
              if ( q3theta == "eap") {
                  if ( omitPV == TRUE ) {
                      cat("Cannot compute Q3 if 'omitPV == TRUE' and 'q3theta == \"eap\"'. Skip computation.\n")
                  }  else  {
                      colEAP<- grep("^eap", colnames(pv[["pvWide"]]))[1]
                      theta <- pv[["pvWide"]][,c(2,colEAP)]
                  }
              }
              if(!is.null(theta)) {
                  drinI <- match( shw[["item"]][,"item"], colnames(daten))      ### ggf.: welche Items im Datensatz stehen nicht im Showfile (*.shw)?
                  drinP <- match(theta[,1], daten[,"ID"])                       ### ggf.: welche Personen im Datensatz stehen nicht im PV-File
                  stopifnot(length(which(is.na(drinP))) == 0 , length(which(is.na(drinI))) == 0 )
                  q3.res<- Q3(dat = daten[drinP,drinI], theta = theta[,2], b = shw[["item"]][,"ESTIMATE"], progress = FALSE)
                  nObs  <- NULL
                  if ( !is.null(q3MinObs) ) {                                   ### untere Zeile: paarweise Anzahl Beobachtungen je Itempaar
                        if ( q3MinObs > 1 ) { nObs <- nObsItemPairs ( responseMatrix = daten[,all.Names[["variablen"]]], q3MinType = q3MinType ) }
                  }
                  matL  <- reshapeQ3 (mat = q3.res$q3.matrix, q3MinObs = q3MinObs, nObs = nObs)
                  if( nrow(matL)>0) {
                        ret   <- rbind ( ret, data.frame ( model = model.name, source = "conquest", var1 = matL[,"Var1"],  var2 = matL[,"Var2"] , type = "fixed",indicator.group = "items",group = paste(names(table(shw1[,"group"])), collapse="_"), par = "q3", derived.par = NA, value = matL[,"value"] , stringsAsFactors = FALSE))
                  }
              }
         }
         return(ret)}


getTamResults     <- function(runModelObj, omitFit, omitRegr, omitWle, omitPV, nplausible , ntheta , normal.approx, samp.regr, theta.model, np.adj, Q3=Q3, q3MinObs =  q3MinObs, q3MinType = q3MinType,
                     pvMethod , group, beta_groups , level , n.iter , n.burnin, adj_MH , adj_change_MH , refresh_MH, accrate_bound_MH,	print_iter , verbose) {
         if( omitRegr == FALSE ) { txt <- capture.output ( regr     <- tam.se(runModelObj)) }
         qMatrix  <- attr(runModelObj, "qMatrix")
         qL       <- melt(qMatrix, id.vars = colnames(qMatrix)[1], variable.name = "dimensionName", na.rm=TRUE)
         qL       <- qL[which(qL[,"value"] != 0 ) , ]
         varName  <- colnames(qMatrix)[1]
         ret      <- NULL                                                       ### Rueckgabeobjekt initialisieren
    ### Sektion 'Itemparameter auslesen' (shw)
         xsis <- merge(data.frame ( item = rownames(runModelObj[["xsi"]]), runModelObj[["xsi"]]), qL[,-match("value", colnames(qL))],  by.x = "item", by.y = colnames(qMatrix)[1], all = TRUE)
         shw1 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = xsis[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = xsis[,"dimensionName"], par = "est",  derived.par = NA, value = xsis[,"xsi"], stringsAsFactors = FALSE)
         shw2 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = xsis[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = xsis[,"dimensionName"], par = "est",  derived.par = "se", value = xsis[,"se.xsi"], stringsAsFactors = FALSE)
         toOff<- shw2[ which(shw2[,"value"] == 0 ), "var1"]
         if(length(toOff)>0) {
            shw1[match(toOff, shw1[,"var1"]), "par"] <- "offset"
            shw2  <- shw2[-which(shw2[,"value"] == 0 ),] }                      ### entferne Zeilen aus shw2, die in der "value"-Spalte NA haben, danach: p-Werte einfuegen
        ### nValid und p-werte aus tam auszulesen, misslingt wenn DIF modelle gerechnet werden ... werden statt dessen aus Itemdaten ausgelesen
        # shw3 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(runModelObj[["item"]][,"item"]), var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],runModelObj[["item"]][,"item"]),"dimensionName"], par = "itemP",  derived.par = NA, value = runModelObj[["item"]][,"M"], stringsAsFactors = FALSE)
        # shw4 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(runModelObj[["item"]][,"item"]), var2 = NA , type = "fixed", indicator.group = "items", group = qL[match(qL[,varName],runModelObj[["item"]][,"item"]),"dimensionName"], par = "Nvalid",  derived.par = NA, value = runModelObj[["item"]][,"N"], stringsAsFactors = FALSE)
         if(!is.null ( attr(runModelObj, "deskRes") ) ) {
            deskR<- merge(attr(runModelObj, "deskRes"), qL[,-match("value", colnames(qL))],  by.x = "item.name", by.y = colnames(qMatrix)[1], all = TRUE)
            shw3 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(deskR[,"item.name"]), var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,"item.p"], stringsAsFactors = FALSE)
            shw4 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(deskR[,"item.name"]), var2 = NA , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "Nvalid",  derived.par = NA, value = deskR[,"valid"], stringsAsFactors = FALSE)
    ### Achtung! wenn in dem 'deskRes'-Objekt noch mehr p-Werte (schulformspezifische p-Werte drinstehen, werden die jetzt auch in die Ergebnisstruktur eingetragen)
            cols <- setdiff ( colnames(deskR)[grep("^item.p", colnames(deskR))], "item.p")
            if ( length ( cols ) > 0 ) {
                 colsR <- data.frame ( original = cols, reduziert = removePattern ( string = cols, pattern = "item.p.") , stringsAsFactors = FALSE)
                 shw31 <- do.call("rbind", apply ( colsR, MARGIN = 1, FUN = function ( zeile ) { data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = as.character(deskR[,"item.name"]), var2 = zeile[["reduziert"]] , type = "fixed", indicator.group = "items", group = deskR[,"dimensionName"], par = "itemP",  derived.par = NA, value = deskR[,zeile[["original"]]], stringsAsFactors = FALSE) }))
            }  else {
                 shw31 <- NULL
            }
         }  else  {
            shw3 <- shw4 <- shw31 <- NULL
         }
         if(!is.null ( attr(runModelObj, "discrim") ) ) {
            discR<- merge( attr(runModelObj, "discrim") , qL[,-match("value", colnames(qL))],  by.x = "item.name", by.y = colnames(qMatrix)[1], all = TRUE)
            shw5 <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = discR[,"item.name"], var2 = NA , type = "fixed", indicator.group = "items", group = discR[,"dimensionName"], par = "itemDiscrim",  derived.par = NA, value = discR[,"item.diskrim"], stringsAsFactors = FALSE)
         }  else  {
            shw5 <- NULL
         }                                                                      ### untere Zeile: Diskriminationsparameter im 2pl-Fall auslesen
         if(attr(runModelObj, "irtmodel") %in% c("2PL", "2PL.groups", "GPCM", "3PL") & omitRegr == FALSE) {
            shw6 <- do.call("rbind", lapply (  1 : length ( colnames( qMatrix ) [-1] ) , FUN = function ( dims ) {
                    cols  <- grep(paste0(".Dim",dims,"$" ), colnames(regr[["B"]]))
                    tamMat<- regr[["B"]][,c(1,cols)]
                    weg   <- which(tamMat[,2] == 0)
                    if(length(weg)>0) {tamMat <- tamMat[-weg,]}
                    shw6D <- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = tamMat[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = colnames(qMatrix)[dims+1], par = "estSlope",  derived.par = NA, value = tamMat[,2], stringsAsFactors = FALSE)
                    shw6se<- data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = tamMat[,"item"], var2 = NA , type = "fixed", indicator.group = "items", group = colnames(qMatrix)[dims+1], par = "estSlope",  derived.par = "se", value = tamMat[,3], stringsAsFactors = FALSE)
                    return(rbind(shw6D, shw6se)) }))
         }  else  {
            shw6 <- NULL
         }
         ret  <- rbind(ret, shw1, shw2, shw3, shw31, shw4, shw5, shw6)          ### Rueckgabeobjekt befuellen, danach infit auslesen
         if ( omitFit == FALSE ) {
              infit<- tam.fit(runModelObj, progress=FALSE)                      ### Achtung: wenn DIF-Analyse, dann misslingt untere Zeile: Workarond!
              fits <- merge(infit[["itemfit"]], qL[,-match("value", colnames(qL))],  by.x = "parameter", by.y = colnames(qMatrix)[1], all = TRUE)
              if ( is.null(attr(runModelObj, "all.Names")[["DIF.var"]])) {      ### wenn kein DIF: mergen
                   ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = fits[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = fits[,"dimensionName"], par = "est",  derived.par = "infit", value = fits[,"Infit"], stringsAsFactors = FALSE),
                                      data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = fits[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = fits[,"dimensionName"], par = "est",  derived.par = "outfit", value = fits[,"Outfit"], stringsAsFactors = FALSE))
              }  else  {                                                        ### wenn DIF: workaround ... DIF-Parameter umbenennen, so dass es konsistent zu "getConquestResults" ist
                   ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = NA, par = "est",  derived.par = "infit", value = infit$itemfit[,"Infit"], stringsAsFactors = FALSE),
                                      data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = infit$itemfit[,"parameter"], var2 = NA , type = "fixed", indicator.group = "items", group = NA, par = "est",  derived.par = "outfit", value = infit$itemfit[,"Outfit"], stringsAsFactors = FALSE) )
                   ret[,"var1"]    <- as.character(ret[,"var1"])
                   ret[,"toMerge"] <- halveString(ret[,"var1"], ":", first=TRUE)[,1]
                   ret  <- merge(ret, qL[,c("item", "dimensionName")], by.x = "toMerge", by.y = "item", all.x = TRUE)
                   ret  <- ret[,-match(c("group", "toMerge"), colnames(ret))]
                   colnames(ret) <- recode(colnames(ret), "'dimensionName'='group'")
                   indD5<- setdiff( 1:nrow(ret), grep(":DIF", ret[,"var1"]))
                   indD5<- setdiff( ret[indD5,"var1"], qL[,"item"])
                   to   <- removePattern(string = indD5, pattern = "DIF_")
                   to1  <- removeNumeric(to)
                   to2  <- removeNonNumeric(to)
                   indD5<- data.frame ( from = indD5, to = paste(to1, to2, sep="_"), stringsAsFactors = FALSE)
                   recSt<- paste("'",indD5[,"from"] , "' = '" , indD5[,"to"],"'", collapse="; ",sep="")
                   indD <- grep(":DIF", ret[,"var1"])
                   indD2<- halveString(ret[indD,"var1"], pattern = ":DIF_", first=TRUE)
                   indD3<- removeNumeric(indD2[,2])
                   indD4<- removeNonNumeric(indD2[,2])
                   ret[indD,"var1"] <- paste("item_", indD2[,1], "_X_", paste(indD3, indD4, sep="_"), sep="")
                   ret[,"var1"]     <- recode(ret[,"var1"], recSt)
              }
         }
    ### Sektion 'Populationsparameter auslesen' (shw)
         if(ncol(qMatrix) == 2) {                                               ### eindimensionaler Fall
            ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = colnames(qMatrix)[2], var2 = NA , type = "distrpar", indicator.group = NA, group = "persons", par = "var",  derived.par = NA, value = runModelObj[["variance"]][1,1] , stringsAsFactors = FALSE))
         }  else  {                                                             ### mehrdimensional: (Residual-)Varianzen und (Residual-)Korrelationen der lat. Dimensionen
            cov1 <- runModelObj[["variance"]]
            colnames(cov1) <- colnames(qMatrix)[-1]
            rownames(cov1) <- colnames(qMatrix)[-1]
            cor1 <- cov2cor(cov1)
            for (ii in 1:nrow(cor1))   {                                        ### loesche alles oberhalb der Hauptdiagonalen
                 cor1[ii,ii:ncol(cor1)] <- NA}
            cor1 <- melt(cor1, measure.vars = colnames(cor1), na.rm=TRUE)
            vars <- diag(cov1)
            ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = c(names(vars),as.character(cor1[,"Var1"])) , var2 = c(rep(NA, length(vars)), as.character(cor1[,"Var2"])) , type = "random", indicator.group = NA, group = "persons", par = c(rep("var",length(vars)), rep("correlation", nrow(cor1))) ,  derived.par = NA, value = c(unlist(vars), cor1[,"value"]), stringsAsFactors = FALSE))
         }
    ### Sektion 'Regressionsparameter auslesen' (shw)
         if( omitRegr == FALSE ) {
             if( !isTRUE(all.equal ( dim(runModelObj$beta) , c(1,1))))  {       ### wird nur gemacht, wenns auch Regressionsparameter gibt
                 regr <- data.frame ( reg.var = rownames(regr$beta), regr$beta, stringsAsFactors = FALSE)
                 regr <- melt(regr, id.vars = "reg.var", na.rm=TRUE)
                 regr2<- data.frame ( par = "est", derived.par = recode(unlist(lapply(strsplit(as.character(regr[,"variable"]),"\\."), FUN = function ( l ) {l[1]})), "'se'='se'; else=NA"), group = colnames(qMatrix)[as.numeric(removePattern( string = unlist(lapply(strsplit(as.character(regr[,"variable"]),"\\."), FUN = function ( l ) {l[2]})), pattern = "Dim")) + 1], regr, stringsAsFactors = FALSE)
                 ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = regr2[,"reg.var"], var2 = NA , type = "regcoef", indicator.group = NA, group = regr2[,"group"], par = regr2[,"par"],  derived.par = regr2[,"derived.par"], value = regr2[,"value"] , stringsAsFactors = FALSE))
             }
         }
    ### Sektion 'Modellindizes auslesen' (shw)
             ret  <- rbind(ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = NA, var2 = NA , type = "model", indicator.group = NA, group = NA, par = c("deviance", "Npar", "AIC", "BIC"), derived.par = NA, value = unlist(runModelObj[["ic"]][c("deviance", "Npars", "AIC", "BIC")]), stringsAsFactors = FALSE))
    ### Sektion 'Personenparameter auslesen' (wle)
         if ( omitWle == FALSE ) {
              wle  <- tam.wle(runModelObj, progress = FALSE)                    ### Achtung: im eindimensionalen Fall enthalten die Spaltennamen keine Benennung der Dimension
              eind1<- ncol(wle) == 7
              if(eind1 == TRUE) {
                 cols1<- grep("theta$", colnames(wle))
                 cols2<- grep("error$", colnames(wle))
                 stopifnot(length(cols1) ==1, length(cols2) ==1)
                 colnames(wle)[c(cols1,cols2)] <- paste(colnames(wle)[c(cols1,cols2)], ".Dim1", sep="")
              }
              weg1 <- grep("WLE.rel", colnames(wle))
              wleL <- melt(wle, id.vars = "pid", measure.vars = colnames(wle)[-c(1:2,weg1)], na.rm=TRUE)
              wleL[,"group"] <- colnames(qMatrix)[as.numeric(removePattern(string = unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[2]})), pattern = "Dim"))+1]
              wleL[,"par"]   <- recode(unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[1]})), "'PersonScores'='NitemsSolved'; 'PersonMax'='NitemsTotal'; 'theta'='wle'; 'error'='wle'")
              wleL[,"derived.par"] <- recode(unlist(lapply(strsplit(as.character(wleL[,"variable"]),"\\."), FUN = function (l) {l[1]})), "'theta'='est'; 'error'='se';else=NA")
              ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = wleL[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = wleL[,"group"], par = wleL[,"par"],  derived.par = wleL[,"derived.par"], value = wleL[,"value"] , stringsAsFactors = FALSE))
         }
    ### Sektion 'Personenparameter auslesen' (PVs)
         if ( omitPV == FALSE ) {
              if ( pvMethod == "regular" ) {                                    ### konventionell
                   do   <- paste ( "pv <- tam.pv ( ", paste(names(formals(tam.pv)), recode ( names(formals(tam.pv)), "'tamobj'='runModelObj'"), sep =" = ", collapse = ", "), ")",sep="")
              } else {                                                          ### bayesianisch
                   if ( is.null ( attr(runModelObj, "Y") ) ) {
                        cat("Warning: Conditioning model was not defined ('Y' is NULL).\n")
                        Y1 <- NULL
                   } else {
                        Y1 <- data.frame ( intercpt = 1, attr(runModelObj, "Y"))
                   }
                   do   <- paste ( "pv <- tam.pv.mcmc ( ", paste(names(formals(tam.pv.mcmc)), recode ( names(formals(tam.pv.mcmc)), "'tamobj'='runModelObj'; 'Y'='Y1'"), sep =" = ", collapse = ", "), ")",sep="")
              }
              eval(parse(text=do))
              pvL  <- melt(pv$pv, id.vars = "pid", na.rm=TRUE)
              pvL[,"PV.Nr"] <- as.numeric(removePattern(string = unlist(lapply(strsplit(as.character(pvL[,"variable"]),"\\."), FUN = function (l) {l[1]})), pattern = "PV"))
              pvL[,"group"] <- colnames(qMatrix)[as.numeric(removePattern(string = unlist(lapply(strsplit(as.character(pvL[,"variable"]),"\\."), FUN = function (l) {l[2]})), pattern = "Dim"))+1]
              ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = pvL[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = pvL[,"group"], par = "pv",  derived.par = paste("pv", pvL[,"PV.Nr"],sep=""), value = pvL[,"value"] , stringsAsFactors = FALSE))
              eaps <- runModelObj[["person"]]                                   ### Achtung: im eindimensionalen Fall enthalten die Spaltennamen keine Benennung der Dimension
              eind1<- ncol(eaps) == 7                                           ### (uneinheitlich zu pvs, wo es immer eine Benennung gibt.)
              #eind2<- length(grep(".Dim1$", colnames(eaps))) == 0              ### Im eindimensionalen Fall muss Benennung ergaenzt werden
              #stopifnot ( eind1 == eind2)                                      ### zur Sicherheit werden hier zwei Indikatoren fuer Eindimensionalitaet genutzt. Fehlermeldung bei Widerspruch
              if(eind1 == TRUE) {                                               ### ggf. muss diese Passage nach Release neuerer TAM-Versionen korrigiert werden
                 cols <- grep("EAP$", colnames(eaps))
                 stopifnot(length(cols) == 2)
                 colnames(eaps)[cols] <- paste(colnames(eaps)[cols], ".Dim1", sep="")
              }
              eaps <- melt(eaps, id.vars = "pid", measure.vars = grep("EAP", colnames(eaps)), na.rm=TRUE)
              eaps[,"group"] <- colnames(qMatrix)[as.numeric(removePattern ( string = halveString(string = as.character(eaps[,"variable"]), pattern = "\\.", first = FALSE)[,"X2"], pattern = "Dim"))+1]
              eaps[,"par"]   <- "est"
              eaps[grep("^SD.",as.character(eaps[,"variable"])),"par"]   <- "se"
              ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = eaps[,"pid"], var2 = NA , type = "indicator", indicator.group = "persons", group = eaps[,"group"], par = "eap", derived.par = eaps[,"par"], value = eaps[,"value"] , stringsAsFactors = FALSE))
         }
    ### Sektion 'Q3 erzeugen'
         if ( Q3 == TRUE ) {
              nObs <- NULL
              if ( !is.null(q3MinObs) ) {                                       ### untere Zeile: paarweise Anzahl Beobachtungen je Itempaar
                    if ( q3MinObs > 1 ) { nObs <- nObsItemPairs ( responseMatrix = runModelObj$resp, q3MinType = q3MinType ) }
              }
              mat  <- tam.modelfit ( tamobj = runModelObj, progress = FALSE )
              matL <- reshapeQ3 (mat = mat$Q3.matr, q3MinObs = q3MinObs, nObs = nObs)
              if( nrow(matL)>0) {
                  ret  <- rbind ( ret, data.frame ( model = attr(runModelObj, "analysis.name"), source = "tam", var1 = matL[,"Var1"], var2 = matL[,"Var2"] , type = "fixed",indicator.group = "items", group = paste(names(table(shw1[,"group"])), collapse="_"), par = "q3", derived.par = NA, value = matL[,"value"] , stringsAsFactors = FALSE))
              }
         }
         return(ret)}

### Hilfsfunktion zur Vereinheitlichung der Q3-Matrix
reshapeQ3 <- function ( mat, q3MinObs, nObs ) {
             for (ii in 1:(nrow(mat)-1)) { mat[ii,ii:ncol(mat)] <- NA}          ### entferne alles oberhalb der Hauptdiagonale
             matL <- melt ( mat , na.rm = TRUE)                       ### das entfernt alle doppelten Eintraege
             if ( !is.null(nObs)) {                                             ### dass hier soll nur passieren, wenn Eintraege aus der Q3 Matrix ggf. entfernt werden
                   check<- do.call("rbind", apply(matL[,-ncol(matL)], MARGIN = 1, FUN = function ( y ) { ret <- sort ( y); ret <- data.frame ( Var1 = ret[1], Var2 = ret[2], stringsAsFactors = FALSE); return(ret)}))
                   matL <- data.frame ( check, value = matL[,"value"], stringsAsFactors = FALSE)
                   matL <- merge ( matL, nObs, by = c("Var1", "Var2"), all = TRUE)
                   matL <- matL[which(!is.na(matL[,"value"])),]
                   weg  <- which(matL[,"minValue"] < q3MinObs)
                   if (length(weg)>0) { matL <- matL[-weg,]}
             }
             if ( nrow(matL) == 0 ) {
                   cat("No observations left in Q3 matrix.\n")
             }
             return(matL)}

### Extraktorfunktionen
eapFromRes <- function ( resultsObj ) {
          eapRo<- intersect( which(resultsObj[,"par"] == "eap"),which(resultsObj[,"indicator.group"] == "persons"))
          if ( length ( eapRo ) == 0 ) {
               cat("Warning: 'resultsObj' does not contain any eap values.\n")
               return ( NULL )
          }  else  {
             sel  <- resultsObj[eapRo,]
             sel  <- do.call("rbind", by(sel, INDICES = sel[,c("model", "group")], FUN = function ( gr ) {
                     res  <- dcast ( gr , model+group+var1~derived.par, value.var = "value")
    ### Hotfix: ID namen identifizieren
                     id   <- unique(resultsObj[intersect(which(resultsObj[,"type"] == "tech"), which(resultsObj[,"par"] == "ID")),"derived.par"])
                     stopifnot(length(id)==1)
                     colnames(res)[-c(1:2)] <- c(id, "EAP", "SE.EAP")
                     weg  <- match(c("model", id), colnames(res))
                     res  <- data.frame ( res[,c("model", id)], dimension = as.character(gr[1,"group"]), res[,-weg,drop=FALSE], stringsAsFactors = FALSE)
                     return(res)}))
             return(sel)
          }  }

pvFromRes  <- function ( resultsObj, toWideFormat = TRUE) {
          pvRow<- intersect( which(resultsObj[,"par"] == "pv"),which(resultsObj[,"indicator.group"] == "persons"))
          if ( length ( pvRow ) == 0 ) {
               cat("Warning: 'resultsObj' does not contain any pv values.\n")
               return ( NULL )
          }  else  {
             sel  <- resultsObj[pvRow, ]                                        ### Hotfix: ID namen identifizieren
             id   <- unique(resultsObj[intersect(which(resultsObj[,"type"] == "tech"), which(resultsObj[,"par"] == "ID")),"derived.par"])
             stopifnot(length(id)==1)
             if (toWideFormat == TRUE ) {
                 sel  <- do.call("rbind", by(sel, INDICES = sel[,c("model","group")], FUN = function ( gr ) {
                         res  <- dcast ( gr , model+var1~derived.par, value.var = "value")
                         colnames(res)[2] <- id
                         weg  <- match(c("model", id), colnames(res))
                         res  <- data.frame ( res[,c("model", id)], dimension = as.character(gr[1,"group"]), res[,-weg,drop=FALSE], stringsAsFactors = FALSE)
                         return(res)}))
             }  else  {
                 sel  <- sel[,c("model", "var1", "group", "derived.par", "value")]
                 recSt<- paste ( "'var1'='",id,"'; 'derived.par'='imp'",sep="")
                 colnames(sel) <- recode ( colnames(sel), recSt)
             }
             return(sel)
         }  }

itemFromRes<- function ( resultsObj ) {                                         ### Funktion wird so oft ausgefuehrt, wie es Modelle gibt
     ### hier muss "rbind.fill" genommen werden, denn 1pl und 2pl Modelle unterscheiden sich in den Spalten (bei 2pl gibt es zusaetzliche Diskriminationsspalten)
          res <- do.call("rbind.fill", by ( data = resultsObj, INDICES = resultsObj[,"model"], FUN = function ( mod ) {
                 sel  <- mod[intersect( which(mod[,"par"] %in% c("est", "estSlope", "Nvalid", "itemP", "ptBis", "itemDiscrim", "offset")),which(mod[,"indicator.group"] == "items")),]
                 if (nrow(sel)==0) {
                     return(NULL)
                 }  else  {
     ### gibt es DIF? wenn ja, wird das separat ausgelesen
                     isDif<- intersect(which(mod[,"type"] == "tech"), which(mod[,"par"] == "DIF.var"))
                     if ( length( isDif ) > 0 ) {                               ### untere Zeile: alle Variablen auslesen (quasi ein Hilfsobjekt)
                           vars     <- mod[intersect(which(mod[,"type"] == "tech"),which(mod[,"par"] == "variablen")),"derived.par"]
                           itemList <- do.call("rbind", lapply ( vars, FUN = function ( v ) {
                                       ind <- grep( paste0("_",v,"_"), sel[,"var1"])
                                       it  <- sort ( unique ( sel[ind,"var1"]))
                                       if(length(it)>2) {
                                          cat(paste("Warning! DIF variable '",mod[isDif,"derived.par"],"' seems to have more than two categories. To date, this is not supported by 'eatModel'.\n",sep=""))
                                       }
                                       return ( data.frame ( item = v, dif = it[1], weg = it[length(it)] , stringsAsFactors = FALSE) ) }))
                           weg      <- wo.sind ( itemList[,"weg"], sel[,"var1"], quiet = TRUE)
                           forDif   <- wo.sind ( itemList[,"dif"], sel[,"var1"], quiet = TRUE)
                           stopifnot(length( intersect(weg, forDif)) == 0 )
                           selForDif<- sel[forDif, ]
                           sel      <- sel[-c(weg, forDif) , ]
                           sel      <- sel[which ( sel[,"par"] != "ptBis" ) , ] ### Hotfix: wenn DIF ausgegeben, wird keine ptBis berechnet
                           selDIF   <- do.call("rbind", by(selForDif, INDICES = selForDif[,"group"], FUN = function ( gr ) {
                                       res  <- dcast ( gr , model+var1~par+derived.par, value.var = "value")
                                       mat  <- lapply( vars, FUN = function ( v ) { grep(paste0("_",v,"_"), res[,"var1"])})
                                       stopifnot (  all ( sapply(mat, length) == 1) )
                                       res[unlist(mat),"item"]  <- vars
                                       colnames(res) <- recode ( colnames(res) , "'est_infit'='infitDif'; 'est_se'='seDif'; 'est_NA'='estDif'")
                                       res[,"absDif"]<- abs ( res[,"estDif"]  * 2 )
                                       pval <- intersect(intersect(which(mod[,"type"] == "tech"), which(mod[,"par"] == "dif")), which(mod[,"derived.par"] == "p.value"))
                                       stopifnot (length(pval) == 1)
                                       pval <- mod[pval, "value"]               ### untere Zeile: adb = 'abs.dif.bound'; sdb = 'sig.dif.bound'
                                       adb  <- mod[intersect(intersect(which(mod[,"type"] == "tech"), which(mod[,"par"] == "dif")), which(mod[,"derived.par"] == "abs.dif.bound")),"value"]
                                       sdb  <- mod[intersect(intersect(which(mod[,"type"] == "tech"), which(mod[,"par"] == "dif")), which(mod[,"derived.par"] == "sig.dif.bound")),"value"]
                                       res[,paste("CI__", pval ,"__lb",sep="")] <- res[,"absDif"] - 2*abs(qnorm(0.5*(1-pval))) * res[,"seDif"]
                                       res[,paste("CI__", pval ,"__ub",sep="")] <- res[,"absDif"] + 2*abs(qnorm(0.5*(1-pval))) * res[,"seDif"]
                                       res  <- data.frame ( res, do.call("rbind", apply(res[,c("absDif", "seDif", paste("CI__",pval,"__lb",sep=""), paste("CI__",pval,"__ub",sep=""))], MARGIN = 1, FUN = function ( d ) {
                                                           check <- all ( !is.na(d) )
                                                           if(check == TRUE) {
                                                              crit1 <- d[["absDif"]] > adb
                                                              crit2 <- !all ( sort ( c ( d[[paste("CI__",pval,"__lb",sep="")]], sdb , d[[paste("CI__",pval,"__ub",sep="")]]), index.return = TRUE)$ix == 1:3 )
                                                              if ( crit1 == TRUE & crit2 == TRUE) { res <- 1 }  else { res <- 0}
     ### Implementiere Formel nach Lord (1980) und ETS-Klassifikation von DIF; siehe Funktion equating.rasch aus 'eatRest'
                                                              ets   <- "A"
                                                              ets1  <- d[["absDif"]] > 0.43 & d[["absDif"]] < 0.64
                                                              ets2  <- !all ( sort ( c ( d[[paste("CI__",pval,"__lb",sep="")]], 0 , d[[paste("CI__",pval,"__ub",sep="")]]), index.return = TRUE)$ix == 1:3 )
                                                              if ( ets1 == TRUE & ets2 == TRUE) { ets <- "B" }
                                                              etsC1 <- d[["absDif"]] > 0.64
                                                              etsC2 <- !all ( sort ( c ( d[[paste("CI__",pval,"__lb",sep="")]], 0.43 , d[[paste("CI__",pval,"__ub",sep="")]]), index.return = TRUE)$ix == 1:3 )
                                                              if ( etsC1 == TRUE & etsC2 == TRUE) { ets <- "C" }
                                                              res   <- data.frame(difIndex = res, ETS = ets )
                                                           }  else  {
                                                              res   <- data.frame(difIndex = NA, ETS = NA )
                                                           }
                                                           return(res)}) ) )
                                       return(res)}))
                     }
                     sel  <- do.call("rbind", by(sel, INDICES = sel[,"group"], FUN = function ( gr ) {
     ### erstmal ohne schulformspezifische p-Werte (die kommen spaeter dazu)
                             sfp  <- intersect ( which ( gr[,"par"] == "itemP"), which ( !is.na(gr[,"var2"])))
                             if ( length ( sfp ) > 0 ) {
                                  res  <- dcast ( gr[-sfp,] , model+var1~par+derived.par, value.var = "value")
                                  sfp  <- dcast ( gr[sfp,] , model+var1~par+var2, value.var = "value")
                                  res  <- merge ( res, sfp, by = c("model", "var1"), all = TRUE)
                             }  else  {
                                  res  <- dcast ( gr , model+var1~par+derived.par, value.var = "value")
                             }
                             colnames(res) <- recode ( colnames(res) , "'var1'='item'; 'est_infit'='infit'; 'est_outfit'='outfit'; 'est_se'='se'; 'est_NA'='est'; 'estSlope_se'='seSlope'; 'estSlope_NA'='estSlope'; 'offset_NA'='estOffset'; 'Nvalid_NA'='Nvalid'; 'ptBis_NA'='ptBis'; 'itemP_NA'='itemP'; 'itemDiscrim_NA'='itemDiscrim'")
                             cols <- c("Nvalid", "itemP", "itemDiscrim", "est", "estOffset", "se", "estSlope", "seSlope", "infit","outfit", "ptBis")
                             drin1<- which(cols %in% colnames(res))
                             drin2<- grep("ptBis_", colnames(res))
                             drin3<- grep("itemP", colnames(res))
                             res  <- data.frame ( res[,c("model", "item")], dimension = as.character(gr[1,"group"]), res[,c(cols[drin1], colnames(res)[drin2] , setdiff (colnames(res)[drin3], cols[drin1])),drop=FALSE], stringsAsFactors = FALSE)
                             return(res)}))
                     if ( length( isDif ) > 0 ) {
                           ciCo<- colnames(selDIF)[grep("^CI__", colnames(selDIF))]
                           sel <- merge(sel, selDIF[,c("item", "model", "estDif", "seDif", "infitDif", "absDif", ciCo, "difIndex", "ETS")], by=c("item","model"), all=TRUE)
                     }
                     return(sel)
                 }
          }))
          return (res )}

q3FromRes<- function ( resultsObj ) {
       selM  <- by(data = resultsObj, INDICES = resultsObj[,"model"], FUN = function ( mr ) {
                sel  <- mr[which(mr[,"par"] == "q3"),]
                if ( nrow(sel)>0) {
                     sel  <- dcast( sel, var1~var2, value.var = "value")
                } else  { sel <- NULL }
                return(sel)})}

wleFromRes <- function ( resultsObj ) {
          wleRo<- intersect( which(resultsObj[,"par"] %in% c("wle","NitemsSolved", "NitemsTotal")),which(resultsObj[,"indicator.group"] == "persons"))
          if(length(wleRo) == 0 ) {
             cat("Error: 'resultsObj' does not contain any WLE values.\n")
             return(NULL)
          }  else  {
             sel  <- resultsObj[wleRo,]
             sel  <- do.call("rbind", by(sel, INDICES = sel[,c("model", "group")], FUN = function ( gr ) {
                     res  <- dcast ( gr , model+var1~par+derived.par, value.var = "value")
    ### Hotfix: 'id' finden
                     id   <- resultsObj[intersect(intersect(which(resultsObj[,"model"] == gr[1,"model"]),which(resultsObj[,"type"] == "tech")), which(resultsObj[,"par"] == "ID")),"derived.par"]
                     stopifnot(length(id)==1)
                     recSt<- paste("'var1'='",id,"'; 'NitemsSolved_NA'='NitemsSolved'; 'NitemsTotal_NA'='NitemsTotal'",sep="")
                     colnames(res) <- recode ( colnames(res) , recSt)
                     weg  <- match(c("model", id), colnames(res))
                     res  <- data.frame ( res[,c("model", id),], dimension = as.character(gr[1,"group"]), res[,-weg,drop=FALSE], stringsAsFactors = FALSE)
                     return(res)}))
             return(sel)
          }  }

### 19. Oktober 2011: Funktion wird "generisch". Wenn 'datei' ein string ist, werden PVs aus Conquest eingelesen,
### wenn Datei vom Typ 'mer' ist, werden PVs aus lmer generiert.
### 22. August 2014: Funktion hoert auf, generisch zu sein
get.plausible <- function(file, quiet = FALSE, forConquestResults = FALSE)  {   ### hier beginnt Einlesen fuer Plausible Values aus Conquest
                 input           <- scan(file,what="character",sep="\n",quiet=TRUE)
                 input           <- strsplit(crop(gsub("-"," -",input) ) ," +") ### Untere Zeile gibt die maximale Spaltenanzahl
                 n.spalten       <- max ( sapply(input,FUN=function(ii){ length(ii) }) )
                 input           <- data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input), byrow = FALSE), stringsAsFactors = FALSE)
                 pv.pro.person   <- sum (input[-1,1]==1:(nrow(input)-1) )       ### Problem: wieviele PVs gibt es pro Person? Kann nicht suchen, ob erste Ziffer ganzzahlig, denn das kommt manchmal auch bei Zeile 7/8 vor, wenn entsprechende Werte = 0.0000
                 n.person        <- nrow(input)/(pv.pro.person+3)               ### Anzahl an PVs pro Person wird bestimmt anhand der uebereinstimmung der ersten Spalte mit aufsteigenden 1,2,3,4...
                 weg             <- c(1, as.numeric( sapply(1:n.person,FUN=function(ii){((pv.pro.person+3)*ii-1):((pv.pro.person+3)*ii+1)}) ) )
                 cases           <- input[(1:n.person)*(pv.pro.person+3)-(pv.pro.person+2),1:2]
                 input.sel       <- input[-weg,]
                 n.dim <- dim(input.sel)[2]-1                                   ### Anzahl der Dimensionen
                 if(quiet == FALSE) {cat(paste(n.person,"persons and",n.dim,"dimensions(s) found.\n"))
                               cat(paste(pv.pro.person,"plausible values were drawn for each person on each dimension.\n"))}
                 ID              <- input[  (pv.pro.person + 3) *  (1:n.person) - (pv.pro.person + 2) ,2]
                 colnames(input.sel) <- c("PV.Nr", paste("dim.",1:(ncol(input.sel)-1),sep=""))
                 input.sel[,1]   <- gsub( " ", "0", formatC(input.sel[,1],width = max(nchar(input.sel[,1]))))
                 input.sel$ID    <- rep(ID, each = pv.pro.person)
                 is.na.ID        <- FALSE
                 if(is.na(input.sel$ID[1])) {                                   ### wenn keine ID im PV-File, wird hier eine erzeugt (Fall-Nr), da sonst reshapen misslingt
                    is.na.ID        <- TRUE                                     ### Die ID wird spaeter wieder geloescht. Um das machen zu koennen, wird Indikatorvariable erzeugt, die sagt, ob ID fehlend war.
                    input.sel$ID    <- rep( 1: n.person, each = pv.pro.person)
                 }
                 input.melt      <- melt(input.sel, id.vars = c("ID", "PV.Nr") , stringsAsFactors = FALSE)
                 input.melt[,"value"] <- as.numeric(input.melt[,"value"])
                 input.wide      <- data.frame( case = gsub(" ", "0",formatC(as.character(1:n.person),width = nchar(n.person))) , dcast(input.melt, ... ~ variable + PV.Nr) , stringsAsFactors = FALSE)
                 colnames(input.wide)[-c(1:2)] <- paste("pv.", paste( rep(1:pv.pro.person,n.dim), rep(1:n.dim, each = pv.pro.person), sep = "."), sep = "")
                 weg.eap         <- (1:n.person)*(pv.pro.person+3) - (pv.pro.person+2)
                 input.eap    <- input[setdiff(weg,weg.eap),]                   ### nimm EAPs und deren Standardfehler und haenge sie an Datensatz - all rows that have not been used before
                 input.eap    <- na.omit(input.eap[,-ncol(input.eap),drop=FALSE])## find EAPs and posterior standard deviations
                 stopifnot(ncol(input.eap) ==  n.dim)
                 input.eap    <- lapply(1:n.dim, FUN=function(ii) {matrix(unlist(as.numeric(input.eap[,ii])), ncol=2,byrow = TRUE)})
                 input.eap    <- do.call("data.frame",input.eap)
                 colnames(input.eap) <- paste(rep(c("eap","se.eap"),n.dim), rep(paste("Dim",1:n.dim,sep="."),each=2),sep="_")
                 PV           <- data.frame(input.wide,input.eap, stringsAsFactors = FALSE)
                 numericColumns <- grep("pv.|eap_|case",colnames(PV))
                 if(is.na.ID == TRUE) {PV$ID <- NA}
                 for (ii in numericColumns) {PV[,ii] <- as.numeric(as.character(PV[,ii]))  }
                 if(  forConquestResults == TRUE ) {
                      return(list ( pvWide = PV, pvLong = input.melt, eap = input.eap))
                 }  else {
                 return(PV)}}

### umgeschrieben fuer 2pl (mit fixiertem slope)
get.wle <- function(file)      {
            input <- crop(scan(file, what = "character", sep = "\n", quiet = TRUE))
            input <- strsplit(input," +")
            n.spalten <- max ( sapply(input,FUN=function(ii){ length(ii) }) )   ### Untere Zeile gibt die maximale Spaltenanzahl:
            n.wle <- floor((n.spalten-1) / 4)                                   ### Dies minus eins und dann geteilt durch 4 ergibt Anzahl an WLEs (mehr oder weniger)
            input <- asNumericIfPossible(data.frame( matrix( t( sapply(input,FUN=function(ii){ ii[1:n.spalten] }) ),length(input),byrow = FALSE), stringsAsFactors = FALSE), set.numeric = TRUE, verbose = FALSE, ignoreAttributes = TRUE)
            valid <- na.omit(input)
            cat(paste("Found valid WLEs of ", nrow(valid)," person(s) for ", n.wle, " dimension(s).\n",sep=""))
            if (nrow(valid) != nrow(input)) { cat(paste("    ",nrow(input)-nrow(valid)," persons with missings on at least one latent dimension.\n",sep="")) }
            namen1<- rep ( x = c("n.solved", "n.total", "wle", "std.wle"), times = n.wle)
            namen2<- rep ( paste(".", 1:n.wle, sep=""), each = 4)               ### untere Zeile: wenn es keine Spalte 'case' gibt, wird die erste Spalte mit 'ID' benannt
            colnames(valid)[(ncol(valid)-length(namen2)):1] <- c("ID","case")[1:(ncol(valid)-length(namen2))]
            colnames(valid)[(ncol(valid)-length(namen2)+1):ncol(valid)] <- paste(namen1,namen2,sep="")
            return(valid)}

### liest Conquest-Outputfiles (*.shw) als R-Objekte ein (siehe auch P:\Aufgabenentwicklung\Grundschule\Daten\Misc\R-Routinen\R2Conquest.R)
### "dif.term" definiert dabei die Variable, nach der ggf. DIF-Analysen ausgelesen werden sollen, z.B. "item*sex". Wird "dif.term" nicht
### spezifiziert, werden keine DIF-Daten ausgegeben.
get.shw <- function(file, dif.term, split.dif = TRUE, abs.dif.bound = 0.6, sig.dif.bound = 0.3, p.value = 0.9) {
            all.output <- list();   all.terms <- NULL                           ### "dif.term" muss nur angegeben werden, wenn DIF-Analysen geschehen sollen.
            input.all <- scan(file,what="character",sep="\n",quiet=TRUE)        ### ginge auch mit:   input <- readLines(file)
            rowToFind <- c("Final Deviance","Total number of estimated parameters")
            rowToFind <- sapply(rowToFind, FUN = function(ii) {                 ### Find the rows indicated in "rowToFind"
                         row.ii <- grep(ii,input.all)                           ### get the parameter of desired rows
                         stopifnot(length(row.ii) == 1)
                         row.ii <- as.numeric(unlist(lapply (strsplit(input.all[row.ii], " +"), FUN=function(ll) {ll[length(ll)]}) ))
                         return(row.ii)})
            ind <- grep("TERM",input.all)                                       ### Wieviele Tabellen gibt es einzulesen?
            grenzen <- grep("An asterisk",input.all)
            if(length(ind)==0) {stop(paste("No TERM-statement found in file ",file,".\n",sep=""))}
            for (i in 1:length(ind)) {
                 term <- input.all[ind[i]];  steps <- NULL
                 doppelpunkt <- which( sapply(1:nchar(term),FUN=function(ii){u <- substr(term,ii,ii); b <- u==":"  }) )
                 term <- substr(term,doppelpunkt+2,nchar(term))
                 cat(paste("Found TERM ",i,": '",term,"' \n",sep=""))
                 all.terms <- c(all.terms,term)                                 ### Dies dient nur dazu, hinterher die Liste mit ausgelesenen Tabellen beschriften zu koennen.
                 bereich <- (ind[i]+6) : (grenzen[i] -2)                        ### Dies der Bereich, der ausgewaehlt werden muss
                 namen   <- c("No.", strsplit(input.all[bereich[1]-2]," +")[[1]][-1])
                 namen   <- gsub("\\^","",namen)
                 index   <- grep("CI",namen)                                    ### Wenn ein "CI" als Spaltenname erscheint, muessen daraus im R-Dataframe zwei Spalten werden!
                 if(length(index) > 0)  {
                    for (ii in 1:length(index)) {
                         namen  <- c(namen[1:index[ii]], "CI",namen[(index[ii]+1):length(namen)] )}}
                 input.sel  <- crop( input.all[bereich] )              ### Textfile wird reduziert, und voranstehende und abschliessende Leerzeichen werden entfernt
                 input.sel  <- gsub("\\(|)|,"," ",input.sel)                    ### entferne Klammern und Kommas (wenn's welche gibt)
                 input.sel  <- gsub("\\*    ", "  NA", input.sel)               ### hier: gefaehrlich: wenn mittendrin Werte fehlen, wuerde stringsplit eine unterschiedliche Anzahl Elemente je Zeile finden
                 foo        <- strsplit(input.sel," +")                         ### und die fehlenden Elemente stets ans Ende setzen. Fatal!
                 maxColumns <- max(sapply(foo, FUN=function(ii){ length(ii)}))  ### Gefahr 2: '*' bezeichnet fixierte Parameter, die keinen Standardfehloeer haben. Manchmal steht aber trotzdem einer da (z.B. in DIF). Ersetzung soll nur stattfinden, wenn mehr als vier Leerzeichen hinterher
                 nDifferentColumns <- length( table(sapply(foo, FUN=function(ii){ length(ii)  })))
                 maxColumns <- which( sapply(foo, FUN=function(ii){ length(ii) == maxColumns  }) ) [1]
    ### untere Zeile: WICHTIG! wo stehen in der Zeile mit den meisten nicht fehlenden Werten Leerzeichen?
                 foo.2      <- which( sapply(1:nchar(input.sel[maxColumns]),FUN=function(ii){u <- substr(input.sel[maxColumns],ii,ii); b <- u==" "  }) )
                 foo.3      <- diff(foo.2)                                      ### zeige die Position des letzten Leerzeichens vor einem Nicht-Leerzeichen
                 foo.3      <- foo.2[foo.3 !=1]                                 ### suche nun in jeder Zeile von input.sel: ist das Zeichen zwei Stellen nach foo.3 ein Leerzeichen? Wenn ja: NA!
                 ESTIMATE   <- which( sapply(1:nchar(input.all[ind[i] + 4] ),FUN=function(ii){u <- substr(input.all[ind[i] + 4],ii,ii+7); b <- u=="ESTIMATE"  }) )
                 foo.3      <- foo.3[foo.3>(ESTIMATE-3)]                        ### Achtung: das alles soll aber nur fuer Spalten beginnen, die hinter ESTIMATE stehen! (missraet sonst fuer Produktterme, z.B. "item*sex")
                 if(nDifferentColumns>1) {
                    if(length(foo.3)>0) {                                       ### Und nochmal: das soll NUR geschehen, wenn es in mindestens einer Zeile nicht die vollstaendige (=maximale) Anzahl von Elementen gibt!
                       for (ii in 1:length(input.sel)) {                        ### also wenn nDifferentColumns groesser als EINS ist (kleiner darf es nicht sein)
                            for (iii in 1:length(foo.3)) {
                                 if(substr( input.sel[ii], foo.3[iii] + 2 , foo.3[iii] + 2 ) == " ") {input.sel[ii] <- paste(substr(input.sel[ii],1,foo.3[iii]), "NA", substring(input.sel[ii],foo.3[iii]+3) , sep="")}}}}
                    if(length(foo.3)==0) {cat(paste("There seem to be no values in any columns behind 'ESTIMATE'. Check outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))}}
                 input.sel <- strsplit(input.sel," +")
                 if(length(input.sel[[1]]) == 0 ) {cat(paste("There seem to be no valid values associated with term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                                                   all.terms <- all.terms[-i]}
                 if(length(input.sel[[1]]) > 0 ) {
                    referenzlaenge <- max (sapply( input.sel, FUN=function(ii ){  length(ii)    }) )
                    if(referenzlaenge < length(namen) ) {
                       cat(paste("Several columns seem to be empty for term '",all.terms[length(all.terms)],"' in file: '",file,"'.\n",sep=""))
    ### bloeder spezialfall: wenn dif-Analyse mit 'compute.fit=FALSE' gemacht wurde, fehlen die infit-Spalten ... die eigentlich notwendige zusaetzliche spalte 'add.column' wird dann nicht eingefuegt. Finde raus, ob das der Fall ist
                       head <- crop(input.all[bereich[1]-2])           ### Ueberschrift
                       leerz<- gregexpr(" ", head)[[1]]                         ### suche: wo beginnt der zweite Block aufeinanderfolgender leerzeichen?
                       leerd<- which ( diff ( leerz) > 1 )[2]
                       vgl  <- length(strsplit ( crop(substr(input.all[bereich[1]], 1, leerd)), split = " +")[[1]])
                       if ( vgl == 4 ) {
                            namen <- c(namen[1:2], "add.column1", namen[3:(referenzlaenge-1)])
                       }  else  {
                            referenzlaenge <- length(namen)
                       }
                    }
    ### Ende spezialfall
                    if(referenzlaenge > length(namen) ) {
                       if(referenzlaenge == length(namen) + 1) {
                          cat(paste("There seem to be one more column than columns names. Expect missing column name before 'ESTIMATE'. \nCheck outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                          ind.name <- which(namen == "ESTIMATE")
                          namen    <- c(namen[1:ind.name-1], "add.column",namen[ind.name:length(namen)])}
                       if(referenzlaenge >  length(namen) + 1) {
                          cat(paste("There seem to be more columns than names for it. Check outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"'. \n",sep=""))
                          namen <- c(namen, rep("add.column",referenzlaenge-length(namen) )) }}
                    input.sel <- t(sapply(input.sel, FUN=function(ii){ c(ii, rep(NA,referenzlaenge-length(ii))) }))
                    colnames(input.sel) <- namen                                ### untere Zeile: entferne eventuelle Sternchen und wandle in Dataframe um!
                    input.sel <- asNumericIfPossible(data.frame( gsub("\\*","",input.sel), stringsAsFactors = FALSE), set.numeric = TRUE, verbose = FALSE, ignoreAttributes = TRUE)
                    results.sel <- data.frame(input.sel,filename=file,stringsAsFactors = FALSE)
                    if(is.na(as.numeric(results.sel$ESTIMATE[1]))) {cat(paste("'ESTIMATE' column in Outputfile for term '",all.terms[length(all.terms)],"' in file: '",file,"' does not seem to be a numeric value. Please check!\n",sep=""))}
                    if(!missing(dif.term)) {                                    ### Der absolute DIF-Wert ist 2 * "Betrag des Gruppenunterschieds". Fuer DIF muessen ZWEI Kriterien erfuellt sein:
                       if(all.terms[length(all.terms)] == dif.term) {           ### Der absolute DIF-Wert muss groesser als 'abs.dif.bound' (z.B. 0.6) und zugleich signifikant groesser als 'sig.dif.bound' (z.B. 0.3) sein
                          cat(paste("Treat '",all.terms[length(all.terms)],"' as DIF TERM.\n",sep=""))
                          results.sel <- data.frame(results.sel,abs.dif = 2*results.sel$ESTIMATE,stringsAsFactors=FALSE)
                          konfNiveau  <- round(100*p.value)                     ### Das bedeutet, fuer Werte groesser 0.6 darf 0.3 NICHT im 90 bzw. 95%-Konfidenzintervall liegen. Nur dann haben wir DIF!
                          results.sel[,paste("KI.",konfNiveau,".u",sep="")] <- results.sel$abs.dif-2*abs(qnorm(0.5*(1-p.value)))*results.sel$ERROR
                          results.sel[,paste("KI.",konfNiveau,".o",sep="")] <- results.sel$abs.dif+2*abs(qnorm(0.5*(1-p.value)))*results.sel$ERROR
                          results.sel[,paste("sig.",konfNiveau,sep="")] <- ifelse(abs(results.sel[,"abs.dif"])>abs.dif.bound & abs(results.sel[,paste("KI.",konfNiveau,".u",sep="")])>sig.dif.bound & abs(results.sel[,paste("KI.",konfNiveau,".o",sep="")])>sig.dif.bound,1,0)
                          results.sel$filename <- file
                          if(split.dif==TRUE) {results.sel <- results.sel[1:(dim(results.sel)[1]/2),]
                                               if(dim(results.sel)[1]!=dim(results.sel)[1]) {cat("Warning: missing variables in DIF table.\n")}}}}
                 all.output[[i]] <- results.sel}}
              if(!missing(dif.term)) {if(sum(all.terms==dif.term)==0) {cat(paste("Term declarated as DIF: '",dif.term,"' was not found in file: '",file,"'. \n",sep=""))  }}
              names(all.output) <- all.terms
    ### ggf. Regressionsparameter einlesen!
            	regrStart <- grep("REGRESSION COEFFICIENTS", input.all) + 2
              isRegression <- length(regrStart) > 0
            	if ( isRegression)   {
                  regrEnd <- grep("An asterisk next", input.all)
              		regrEnd <- regrEnd[which(regrEnd > regrStart)][1] - 2
              		regrInput <- crop(input.all[regrStart:regrEnd])
              		zeileDimensions <- grep("Regression Variable",input.all)
                  stopifnot(length(zeileDimensions) ==1)
              		nameDimensions  <- unlist(strsplit(input.all[zeileDimensions], "  +"))[-1]
              		regrRows <- grep("CONSTANT",input.all)
                  regrRows <- regrRows[regrRows<=regrEnd][1]
              		regrNamen <- unlist(lapply(strsplit(input.all[regrRows:regrEnd],"  +"), FUN=function(ii) {unlist(ii)[1]} ))
                  regrInputSel <- crop(input.all[regrRows:regrEnd])
                  regrInputSel <- gsub("\\(","",regrInputSel)
              		regrInputSel <- gsub(")","",regrInputSel)
              		regrInputSel <- gsub("\\*","  NA",regrInputSel)
              		regrInputSel <- unlist( strsplit(regrInputSel," +") )
              		nDimensions  <- (length(  regrInputSel ) / length(regrNamen) - 1 )/2
                  cat(paste("Found ",nDimensions," dimension(s): ",paste(nameDimensions,collapse=", "),"\n",sep=""))
                  cat(paste("Found ",length(regrNamen)-1," regressor(s).\n",sep=""))
                  regrInputSel <- data.frame(matrix(regrInputSel, ncol=2*nDimensions+1, byrow=T),stringsAsFactors=F)
                  for (ii in 2:ncol(regrInputSel))  {regrInputSel[,ii] <- as.numeric(regrInputSel[,ii])}
                  colnames(regrInputSel) <- c("reg.var", paste(rep(c("coef","error"),nDimensions), rep(nameDimensions,each=2),sep="_") )
                  regrInputSel$filename <- file
              		all.output$regression <- regrInputSel
              }
    ### Kovarianz-/ Korrelationsmatrix einlesen: schwierig, also Trennen nach ein- vs. mehrdimensional. Eindimensional: zweimal "-----" zwischen Beginn und Ende des COVARIANCE-Statements
              korStart <- grep("COVARIANCE/CORRELATION MATRIX", input.all)
              korEnd   <- grep("An asterisk next", input.all)
              korEnd   <- min(korEnd[korEnd > korStart])
              korStriche <- grep("-----",input.all)
              korStriche <- korStriche[korStriche > korStart & korStriche < korEnd]
              if(length(korStriche) == 2) {                                     ### eindimensional!
                 varRow    <- grep("Variance", input.all)
                 variance  <- as.numeric( unlist( lapply(strsplit(input.all[varRow]," +"), FUN=function(ll) {ll[length(ll)]}) ) )
                 names(variance) <- "variance"
                 all.output$cov.structure <- variance
              }
              if(length(korStriche) > 2) {                                      ### mehrdimensional!
                 bereich     <- input.all[ (min(korStriche) + 1) : (max(korStriche) - 1 ) ]
                 bereich     <- bereich[ -grep("----",bereich)]
                 bereich     <- strsplit(crop(bereich),"  +")
                 for (ii in 2:(length(bereich)-1) )  {
                     if(ii <= length(bereich[[ii]]) )  {
                        bereich[[ii]] <- c(bereich[[ii]][1:(ii-1)], NA, bereich[[ii]][ii:length(bereich[[ii]])])
                     }
                     if(ii > length(bereich[[ii]]) )  {
                        bereich[[ii]] <- c(bereich[[ii]][1:(ii-1)], NA)
                     }
                 }
                 bereich.data.frame <- asNumericIfPossible(data.frame(do.call("rbind", bereich[-1]),stringsAsFactors=FALSE), verbose = FALSE, ignoreAttributes = TRUE)
                 colnames(bereich.data.frame) <- bereich[[1]]
                 all.output$cov.structure <- bereich.data.frame
              }
            all.output$final.deviance <- rowToFind
            return(all.output)}

get.prm <- function(file)   {
            input <- scan(file,what="character",sep="\n",quiet=TRUE)
            input <- strsplit( gsub("\\\t"," ",crop(input)), "/\\*")   ### Hier ist es wichtig, gsub() anstelle von sub() zu verwenden! sub() loescht nur das erste Tabulatorzeichen
            ret   <- data.frame ( do.call("rbind", strsplit( crop(unlist(lapply(input, FUN = function ( l ) {l[1]}))), " +")), stringsAsFactors = FALSE)
            nameI <- crop(removePattern ( crop( crop(unlist(lapply(input, FUN = function ( l ) {l[length(l)]}))), char = "item"), pattern = "\\*/"))
            ret   <- data.frame ( Case= as.numeric(ret[,1]), item = nameI, parameter= as.numeric(ret[,2]) ,stringsAsFactors = FALSE)
            return(ret)}

get.itn <- function(file)  {
            input <- scan(file, what = "character", sep="\n", quiet = TRUE)
            ind.1 <- grep("==========",input)
            items <- grep( "item:", input )
            diff.last <- ind.1[length(ind.1)-1] - items[length(items)] + 4
            items <- cbind(1:length(items),items,c(diff(items),diff.last))      ### dort wo diff(items) != 13 , ist das entsprechende Item partial credit. (Fuer das letzte Item ist das komplizierter, da length(diff(items))<length(items).    )
            ind.2 <- gregexpr(":", input[items[,2]])                            ### Folgende Zeilen dienen dazu zu pruefen, ob DIFs in der Tabelle vorkommen oder nicht (falls ja, dann gibt es zwei Doppelpunkte pro input[items[,2]]
            ind.3 <- unlist(ind.2)                                              ### Dann ist ind.3 auch doppelt so lang wie ind.2, weil jedes Element aus ind.2 ein Vektor mit zwei Elementen ist
            ind.3 <- matrix(ind.3,length(ind.2),byrow=T)
            item.namen <- substr(input[items[,2]], ind.3[,dim(ind.3)[2]]+1+nchar(as.character(items[,1])),100)
            item.namen <- gsub(" ","",item.namen)                               ### Leider funktioniert gsub() nicht fuer Klammern, da diese fuer regular expression reserviert sind, aber...
            item.namen <- gsub("\\)","",item.namen); item.namen <- gsub("\\(","",item.namen)
            if(dim(ind.3)[2]>1)                                                 ### kommen DIFs din vor? Ja, falls Bedingung TRUE
              {stopifnot(length(table(ind.3[,1]))==1)                           ### sollte 1 sein; da es immer dieselbe DIF-Variable mit ergo derselben Zeichenlaenge ist.
               dif.name <- rep(substr(input[items[,2]], 1, ind.3[,1]-1),(items[,3]-11))                          ### Auslesen der Variablennamen fuer DIF
               dif.value <- rep(as.numeric(substr(input[items[,2]], ind.3[,1]+1, ind.3[,1]+1)),(items[,3]-11))}  ### Auslesen des Wertes der DIF-Variablen
            zeilen <- list(); reihe <- NULL                                     ### Was geschieht oben? Die DIF-Variable wird fuer Item repetiert, und zwar zweimal, wenn es ein normales, dreimal, wenn es ein partial credit-Item ist. Die entsprechende Information steht in items[,3]; vgl.: rep(1:4,1:4)
            for (i in 1:dim(items)[1])                                          ### finde die Zeilen fuer jedes Item
                {zeilen[[i]] <- (items[i,2]+7) : (items[i,2]+ (items[i,3]-5) )  ### kein partial credit: beginne sieben Zeilen unter "item:" und ende bei acht Zeilen (= 13-5) unter "item:". Fuer partial credit, ende items[i,3]-5 Zeilen unter "items:"
                 cases       <- gsub("NA ","NA",input[zeilen[[i]]])             ### Untere Zeile: Korrektur, wenn die zwei Datenzeilen leere felder enthalten (NA wird nachtraeglich eingetragen)
                 cases <- gsub("_BIG_ ","NA",cases)
                 cases <- gsub("_BIG_","NA",cases)
                 if(length(table(sapply(1:length(cases),FUN=function(ii){length(unlist(strsplit(cases[ii]," +"))) }) ) )>1 )
                   {cases <- gsub("          ","    NA    ",cases)}             ### Perfekt! ueberall dort, wo zehn Leerzeichen infolge stehen, muss eine Auslassung sein! Hier wird ein Ersetzung gemacht!
                 cases       <- data.frame( matrix ( unlist( strsplit(crop(gsub(" +"," ", cases))," ") ), nrow=length(zeilen[[i]]),byrow=T ) , stringsAsFactors=F)
                 ind         <- grep("\\)",cases[1,]); cases[,ind] <- gsub("\\)","",cases[,ind] )
                 cases       <- data.frame(cases[,1:(ind-1)],matrix(unlist(strsplit(cases[,6],"\\(")),nrow=length(zeilen[[i]]),byrow=T),cases[,-c(1:ind)],stringsAsFactors=F)
                 for(jj in 1:ncol(cases)) {cases[,jj] <- as.numeric(cases[,jj])}
                 colnames(cases) <- c("Label","Score","Abs.Freq","Rel.Freq","pt.bis","t.value","p.value",paste(rep(c("PV1.Avg.","PV1.SD."),((ncol(cases)-7)/2) ),rep(1:((ncol(cases)-7)/2),each=2),sep=""))
                 threshold.zeile   <- input[items[i,2]+2]; threshold <- NULL; delta <- NULL
                 bereich <- ifelse( (items[i,3]-12)<1,1,(items[i,3]-12))        ### Sicherheitsbedingung, falls Variable nur eine Kategorie hat
                 if((items[i,3]-12)<1) {cat(paste("Item",i,"hat nur eine Antwortkategorie.\n"))}
                 for (j in 1: bereich )
                     {threshold  <- c(threshold ,as.numeric(substr(threshold.zeile,  6*j+16,6*j+21)))
                      delta      <- c(delta,     as.numeric(substr(input[items[i,2]+3],6*j+13,6*j+18)))}
                 while(length(threshold) < nrow(cases)) {threshold <- c(threshold,NA)}
                 while(length(delta) < nrow(cases)) {delta <- c(delta,NA)}
                 item.p <- NA                                                   ### Manchmal kann kein p-wert bestimmt werden. Wenn doch, wird das NA ueberschrieben
                 valid.p <- which(is.na(cases$Score))
                 if(length(valid.p) == 0)
                    {item.p <- cases[which(cases$Score == max(cases$Score)),"Abs.Freq"] / sum(cases$Abs.Freq)}
                 sub.reihe   <- data.frame(item.nr=i, item.name=item.namen[i], cases[,1:2], n.valid = sum(cases$Abs.Freq), cases[,3:4], item.p = item.p, diskrim=as.numeric(substr(input[items[i,2]+1],45,55)),cases[,-c(1:4)], threshold, delta, stringsAsFactors=F)
                 reihe <- rbind(reihe,sub.reihe)}
             if(dim(ind.3)[2]>1)
               {reihe <- data.frame(dif.name,dif.value,reihe,stringsAsFactors=FALSE)}
             return(reihe)}

get.dsc <- function(file) {
            input     <- scan(file,what="character",sep="\n",quiet=TRUE)
            n.gruppen    <- grep("Group: ",input)
            gruppennamen <- unlist( lapply( strsplit(input[n.gruppen]," ") , function(ll) {paste(ll[-1],collapse=" ")} ) )
            cat(paste("Found ",length(n.gruppen)," group(s) in ",file,".\n",sep=""))
            trenner.1 <- grep("------------------",input)
            trenner.2 <- grep("\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.",input)
            stopifnot(length(trenner.1) == length(trenner.2))
            daten     <- lapply(1:(length(trenner.1)/2), FUN=function(ii) {
                 dat <- strsplit(input[(trenner.1[2*ii]+1):(trenner.2[2*ii-1]-1)]," +")
                 dat <- data.frame(matrix(unlist(lapply(dat, FUN=function(iii) {  c(paste(iii[1:(length(iii)-4)],collapse=" "),iii[-c(1:(length(iii)-4))])  })), ncol=5,byrow=T) , stringsAsFactors=F)
                 dat <- data.frame(group.name = gruppennamen[ii], dat, stringsAsFactors = FALSE)
                 colnames(dat) <- c("group.name","dimension","N","mean","std.dev","variance")
                 for (iii in 3:ncol(dat)) {dat[,iii] <- as.numeric(dat[,iii])}
                 desc <- strsplit(input[(trenner.2[2*ii-1]+1):(trenner.2[2*ii]-1)]," +")
                 desc <- data.frame(matrix(unlist(lapply(desc, FUN=function(iii) {  c(paste(iii[1:(length(iii)-3)],collapse=" "),iii[-c(1:(length(iii)-3))])  })), ncol=4,byrow=T) , stringsAsFactors=F)
                 colnames(desc) <- c("dimension","mean","std.dev","variance")
                 for (iii in 2:ncol(desc)) {desc[,iii] <- as.numeric(desc[,iii])}
                 dat.list <- list( single.values=dat, aggregates=desc)
                 return(dat.list) } )
            names(daten) <- gruppennamen
            n.dim        <- names(table(unlist(lapply(1:length(daten), FUN=function(ii) {length( grep("Error", daten[[ii]]$aggregates$dimension))}) ) ))
            stopifnot(length(n.dim)==1)
            cat(paste("Found ",n.dim," dimension(s) in ",file,".\n",sep=""))
            return(daten)}


get.equ <- function(file)  {
            input       <- scan(file,what="character",sep="\n",quiet = TRUE)
            dimensionen <- grep("Equivalence Table for",input)
            cat(paste("Finde ",length(dimensionen), " Dimension(en).\n",sep=""))
            ende        <- grep("================",input)
            ende        <- sapply(dimensionen, FUN=function(ii) {ende[ende>ii][1]})
            tabellen    <- lapply(1:length(dimensionen), FUN=function(ii)
                           {part <- crop(input[(dimensionen[ii]+6):(ende[ii]-1)])
                            part <- data.frame(matrix(as.numeric(unlist(strsplit(part," +"))),ncol=3,byrow=T),stringsAsFactors=F)
                            colnames(part) <- c("Score","Estimate","std.error")
                            return(part)})
            regr.model  <- grep("The regression model",input)
            item.model  <- grep("The item model",input)
            stopifnot(length(regr.model) == length(item.model))
            name.dimensionen <- unlist( lapply(dimensionen,FUN=function(ii) {unlist(lapply(strsplit(input[ii], "\\(|)"),FUN=function(iii){iii[length(iii)]}))}) )
            model       <- lapply(1:length(regr.model), FUN=function(ii) {rbind ( crop(gsub("The regression model:","",input[regr.model[ii]])), crop(gsub("The item model:","",input[item.model[ii]])) ) })
            model       <- do.call("data.frame",args=list(model,row.names=c("regression.model","item.model"),stringsAsFactors=F))
            colnames(model) <- name.dimensionen
            tabellen$model.specs <- model
            names(tabellen)[1:length(dimensionen)] <- name.dimensionen
            return(tabellen)}

normalize.path <- function(string)
                  {string <- gsub("//","/",string)
                   string <- gsub("/","//",string)
                   string <- gsub("//","\\\\",string)
                   return(string)}

### Funktion generiert und schreibt Syntax. Voraussetzung: Datensatz ist da.
### gen.syntax wird von defineModel aufgerufen,  keine user-level Funktion!
### Funktion ist von Restrukturierung ("defineModel" statt "prep.conquest") erstmal nicht betroffen, alles bleibt wie's ist
gen.syntax     <- function(Name,daten, all.Names, namen.all.hg = NULL, all.hg.char = NULL, var.char, model = NULL, anchored, constraints=c("cases","none","items"), pfad=NULL, Title=NULL,n.plausible=5,std.err=c("quick","full","none"), compute.fit ,
                           distribution=c("normal","discrete"), method=c("gauss", "quadrature", "montecarlo"), n.iterations=200, nodes=NULL, p.nodes=2000, f.nodes=2000, converge=0.001,deviancechange=0.0001, equivalence.table=c("wle","mle","NULL"), use.letters=use.letters, model.statement=model.statement, conquest.folder = NULL, allowAllScoresEverywhere,
                           seed , export = list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE) )  {
                   if(is.null(anchored)) {anchored <- FALSE} else {anchored <- TRUE}
                   export.default <- list(logfile = TRUE, systemfile = FALSE, history = TRUE, covariance = TRUE, reg_coefficients = TRUE, designmatrix = FALSE)
                   mustersyntax <- c("title = ####hier.title.einfuegen####;",
                   "export logfile >> ####hier.name.einfuegen####.log;",
                   "datafile ####hier.name.einfuegen####.dat;",
                   "Format pid ####hier.id.einfuegen####",
                   "group",
                   "codes ####hier.erlaubte.codes.einfuegen####;",
                   "labels  << ####hier.name.einfuegen####.lab;",
                   "import anchor_parameters << ####hier.name.einfuegen####.ank;",
                   "caseweight",
                   "set constraints=####hier.constraints.einfuegen####;",
                   "set warnings=no,update=yes,n_plausible=####hier.anzahl.pv.einfuegen####,p_nodes=####hier.anzahl.p.nodes.einfuegen####,f_nodes=####hier.anzahl.f.nodes.einfuegen####;",
                   "set seed=####hier.seed.einfuegen####;",
                   "export par    >> ####hier.name.einfuegen####.prm;",
                   "regression",
                   "model ####hier.model.statement.einfuegen####;",
                   "estimate ! fit=####hier.fitberechnen.einfuegen####,method=####hier.method.einfuegen####,iter=####hier.anzahl.iterations.einfuegen####,nodes=####hier.anzahl.nodes.einfuegen####,converge=####hier.converge.einfuegen####,deviancechange=####hier.deviancechange.einfuegen####,stderr=####hier.std.err.einfuegen####,distribution=####hier.distribution.einfuegen####;",
                   "Itanal >> ####hier.name.einfuegen####.itn;",
                   "show cases! estimates=latent >> ####hier.name.einfuegen####.pvl;",
                   "show cases! estimate=wle >> ####hier.name.einfuegen####.wle;",
                   "equivalence ####hier.equivalence.table.einfuegen#### >> ####hier.name.einfuegen####.equ;",
                   "show >> ####hier.name.einfuegen####.shw;",
                   "export history >> ####hier.name.einfuegen####.his;",
									 "export covariance >> ####hier.name.einfuegen####.cov;",
									 "export reg_coefficients >> ####hier.name.einfuegen####.reg;",
									 "export designmatrix >> ####hier.name.einfuegen####.mat;",
                   "put >> ####hier.name.einfuegen####.cqs;  /* export systemfile */",
                   "descriptives !estimates=pv >> ####hier.name.einfuegen####_pvl.dsc;",
                   "descriptives !estimates=wle >> ####hier.name.einfuegen####_wle.dsc;",
                   "quit;")
                   if(is.null(Title))   {                                       ### wenn kein Titel gesetzt, erstelle ihn aus Sys.getenv()
                      all.inf  <- Sys.getenv()
                      Title    <- paste("Analysis name: ",Name, ", User: ",all.inf["USERNAME"],", Computername: ",all.inf["COMPUTERNAME"],", ", R.version$version.string , ", Time: ",date(),sep="")}
                   converge <- paste("0",substring(as.character(converge+1),2),sep="")
                   deviancechange <- paste("0",substring(as.character(deviancechange+1),2),sep="")
                   syntax    <- gsub("####hier.title.einfuegen####",Title,mustersyntax)
                   if(is.null(n.plausible))   {n.plausible <- 0}  ; if(is.na(n.plausible))     {n.plausible <- 0}
                   if(n.plausible == 0 )     {                                  ### wenn Anzahl PVs = 0 oder NULL, loesche Statement; andernfalls: setze Anzahl zu ziehender PVs ein!
                      syntax    <- gsub("n_plausible=####hier.anzahl.pv.einfuegen####,","",syntax) } else {
                      syntax    <- gsub("####hier.anzahl.pv.einfuegen####",n.plausible,syntax)
                   }
                   syntax    <- gsub("####hier.name.einfuegen####",Name,syntax)
                   ID.char   <- max(as.numeric(names(table(nchar(daten[,"ID"])))))
                   syntax    <- gsub("####hier.id.einfuegen####",paste("1-",as.character(ID.char)," ",sep="" ) ,syntax)
                   syntax    <- gsub("####hier.anzahl.iterations.einfuegen####",n.iterations,syntax)
                   syntax    <- gsub("####hier.anzahl.p.nodes.einfuegen####",p.nodes,syntax)
                   syntax    <- gsub("####hier.anzahl.f.nodes.einfuegen####",f.nodes,syntax)
                   syntax    <- gsub("####hier.converge.einfuegen####",converge,syntax)
                   syntax    <- gsub("####hier.deviancechange.einfuegen####",deviancechange,syntax)
                   if(!is.null(seed)) {syntax    <- gsub("####hier.seed.einfuegen####",seed,syntax)}
                   syntax    <- gsub("####hier.constraints.einfuegen####",match.arg(constraints),syntax)
                   compute.fit  <- if(compute.fit == TRUE ) compute.fit <- "yes" else compute.fit <- "no"
                   syntax    <- gsub("####hier.fitberechnen.einfuegen####",compute.fit,syntax)
                   syntax    <- gsub("####hier.anzahl.nodes.einfuegen####",nodes,syntax)
                   syntax    <- gsub("####hier.std.err.einfuegen####",match.arg(std.err),syntax)
                   syntax    <- gsub("####hier.distribution.einfuegen####",match.arg(distribution),syntax)
                   syntax    <- gsub("####hier.equivalence.table.einfuegen####",match.arg(equivalence.table),syntax)
                   syntax    <- gsub("####hier.model.statement.einfuegen####",tolower(model.statement),syntax)
                   erlaubte.codes <- paste(gsub("_","",sort(gsub(" ","_",formatC(names(tableUnlist(daten[, all.Names[["variablen"]], drop=FALSE ])),width=var.char)),decreasing=TRUE)),collapse=",")
                   syntax    <- gsub("####hier.erlaubte.codes.einfuegen####",erlaubte.codes, syntax )
                   ind       <- grep("Format pid",syntax)
                   beginn    <- NULL                                            ### setze "beginn" auf NULL. Wenn DIF-Variablen spezifiziert sind, wird "beginn" bereits
                   if(length(namen.all.hg)>0)    {                              ### untere Zeile: wieviele "character" haben Hintergrundvariablen?
                     all.hg.char.kontroll <- all.hg.char
                     all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(daten[,ii]))))})
                     stopifnot(all(all.hg.char == all.hg.char.kontroll))        ### Trage nun die Spalten in das Format-Statement ein: Fuer ALLE expliziten Variablen
                     for (ii in 1:length(namen.all.hg))  {
                          if(is.null(beginn)) {beginn <- ID.char+1}
                          ende   <- beginn-1+all.hg.char[ii]
                          if (beginn != ende) {syntax[ind] <- paste(syntax[ind],namen.all.hg[ii], " ", beginn,"-",ende," ",sep="")}
                          if (beginn == ende) {syntax[ind] <- paste(syntax[ind],namen.all.hg[ii], " ", beginn," ",sep="")}
                          beginn  <- ende+1 }
                   }
                   if(length(all.Names[["DIF.var"]])>0)   {                     ### in folgender Schleife ueberschrieben und dann in der Schleife "if(!is.null(HG.var))" ergaenzt, nicht neu geschrieben
                      if(model.statement != "item") {
                        cat(paste("Caution! DIF variable was specified. Expected model statement is: 'item - ",tolower(all.Names[["DIF.var"]])," + item*",tolower(all.Names[["DIF.var"]]),"'.\n",sep=""))
                        cat(paste("However, '",tolower(model.statement),"' will used as 'model statement' to accomplish your will.\n",sep=""))
                      }
                      if(model.statement == "item") {
                         ind.model <- grep("model item", syntax)                ### Aendere model statement
                         stopifnot(length(ind.model)==1)
                         syntax[ind.model] <- paste("model item - ",paste(tolower(all.Names[["DIF.var"]]),collapse=" - ") ," + ", paste("item*",tolower(all.Names[["DIF.var"]]),collapse=" + "), ";",sep="")
                      }
                   }
                   if(length(all.Names[["HG.var"]])>0)  {
                      ind.2   <- grep("^regression$",syntax)
                      syntax[ind.2] <- paste(crop(paste( c(syntax[ind.2], tolower(all.Names[["HG.var"]])), collapse=" ")),";",sep="")
                      if(method == "gauss") {cat("Warning: Gaussian quadrature is only available for models without latent regressors.\n")
                                             cat("         Use 'Bock-Aiken quadrature' for estimation.\n")
                                             method <- "quadrature"} }          ### method muss "quadrature" oder "montecarlo" sein
                   syntax    <- gsub("####hier.method.einfuegen####",method,syntax)
                   if(length(all.Names[["weight.var"]])>0)  {                   ### Method wird erst hier gesetzt, weil sie davon abhaengt, ob es ein HG-Modell gibt
                      ind.4   <- grep("caseweight",syntax)
                      syntax[ind.4] <- paste( syntax[ind.4], " ", tolower(all.Names[["weight.var"]]),";",sep="") }
                   if(length(all.Names[["group.var"]])>0) {
                       ind.3   <- grep("^group$",syntax)
                       stopifnot(length(ind.3) == 1)
                       syntax[ind.3] <- paste(crop(paste( c(syntax[ind.3], tolower(all.Names[["group.var"]])), collapse=" ")),";",sep="")
                       ### gebe gruppenspezifische Descriptives
                       add.syntax.pv  <- as.vector(sapply(all.Names[["group.var"]], FUN=function(ii) {paste("descriptives !estimates=pv, group=",tolower(ii)," >> ", Name,"_",tolower(ii),"_pvl.dsc;",sep="")} ))
                       add.syntax.wle <- as.vector(sapply(all.Names[["group.var"]], FUN=function(ii) {paste("descriptives !estimates=wle, group=",tolower(ii)," >> ", Name,"_",tolower(ii),"_wle.dsc;",sep="")} ))
                       ind.3    <- grep("quit",syntax)
                       stopifnot(length(ind.3)==1)
                       syntax   <- c(syntax[1:(ind.3-1)],add.syntax.pv, add.syntax.wle, syntax[ind.3:length(syntax)]) }
                   if(is.null(beginn)) {beginn <- ID.char+1}
                   syntax[ind] <- paste(syntax[ind], "responses ",beginn,"-",beginn-1+var.char*ncol(data.frame(daten[,all.Names[["variablen"]]],stringsAsFactors = FALSE)),";",sep="")
                   if(var.char>1)  {                                            ### Items haben mehr als eine Spalte Stelligkeit (Conquest-Handbuch, S.177)
                      syntax[ind] <- paste(gsub(";","",syntax[ind]), " (a",var.char,");",sep="")}
                   score.statement <- .writeScoreStatementMultidim (data=daten, itemCols=all.Names[["variablen"]], qmatrix=model, columnItemNames = 1 ,use.letters=use.letters, allowAllScoresEverywhere = allowAllScoresEverywhere )
                   expected.nodes  <- nodes^(ncol(model)-1)
                   if(expected.nodes>3500 & method != "montecarlo") {cat(paste("Specified model probably will use ",expected.nodes," nodes. Choosen method ",method," may not appropriate. Recommend to use 'montecarlo' instead.\n",sep=""))}
                   ind <- grep("labels ",syntax)
                   stopifnot(length(ind)==1)
                   syntax <- c(syntax[1:ind],score.statement,syntax[(ind+1):length(syntax)])
                   if(length(all.Names[["HG.var"]])==0) {                       ### wenn kein HG-model, loesche entsprechende Syntaxzeilen
                      ind.2 <- grep("^regression$",syntax)
                      stopifnot(length(ind.2)==1)
                      syntax <- syntax[-ind.2]
                      ind.3 <- grep("export reg_coefficients",syntax)
                      stopifnot(length(ind.3)==1)
                      syntax <- syntax[-ind.3] }
                   if(length(all.Names[["group.var"]]) ==0) {                   ### wenn keine Gruppen definiert, loesche Statement
                      ind.3 <- grep("^group$",syntax)
                      stopifnot(length(ind.3)==1)
                      syntax <- syntax[-ind.3]}
                   if(length(all.Names[["weight.var"]]) ==0) {                  ### wenn keine Gewichte definiert, loesche Statement
                      ind.4 <- grep("^caseweight$",syntax)
                      stopifnot(length(ind.4)==1)
                      syntax <- syntax[-ind.4]}
                   if(match.arg(equivalence.table) == "NULL") {                 ### wenn keine Equivalence-Statement definiert, loesche Zeile
                      ind.5   <- grep("^equivalence",syntax)
                      stopifnot(length(ind.5)==1)
                      syntax <- syntax[-ind.5]}
                   if(is.null(seed)) {                                          ### wenn keine seed-Statement definiert, loesche Zeile
                      ind.7   <- grep("^set seed",syntax)
                      stopifnot(length(ind.7)==1)
                      syntax <- syntax[-ind.7]}
                   if(n.plausible == 0)     {                                   ### wenn Anzahl PVs = 0 oder NULL, loesche Statement
                      ind.6   <- grep("^show cases! estimates=latent", syntax)
                      stopifnot(length(ind.6) == 1)
                      syntax  <- syntax[-ind.6]}
                   if(anchored == FALSE) {ind.2 <- grep("anchor_parameter",syntax)# wenn keine ANKER gesetzt, loesche entsprechende Syntaxzeile
                                        syntax <- syntax[-ind.2]}
                   if(anchored == TRUE)  {ind.2 <- grep("^set constraints",syntax)# wenn ANKER gesetzt, setze constraints auf "none"
                                        if(match.arg(constraints) != "none") { cat("Anchorparameter were defined. Set constraints to 'none'.\n")}
                                        syntax[ind.2]  <- "set constraints=none;"}
                   classes.export <- sapply(export, FUN = function(ii) {class(ii)})
                   if(!all(classes.export == "logical"))  {stop("All list elements of argument 'export' have to be of class 'logical'.\n")}
                   export <- as.list(userSpecifiedList ( l = export, l.default = export.default ))
                   weg <- names(export[which(export == FALSE)])
                   if(length(weg)>0)    {                                       ### hier wird, was nicht exportiert werden soll, aus Syntax geloescht.
                      for (ii in seq(along=weg) ) {
                           ind.x <- grep(paste("export ", weg[ii], sep=""), syntax)
                           stopifnot(length(ind.x) == 1)
                           syntax <- syntax[-ind.x]}}
                   if(export["history"] == TRUE)  {
                      if(!is.null(conquest.folder))  {
                         cq.version <- getConquestVersion( path.conquest = conquest.folder, path.temp = pfad)
                         if(cq.version < as.date("1Jan2007") )   {
   									      ind.3 <- grep("^export history",syntax)               ### wenn Conquest aelter als 2007, soll history geloescht werden,
                           stopifnot(length(ind.3) == 1 )                       ### auch dann, wenn der Benutzer History ausgeben will
                           syntax <- syntax[-ind.3]
                         }
                      }
                      if(is.null(conquest.folder)) {cat("Warning! Conquest folder was not specified. Unable to detect Conquest version. When you propose to use 2005 version,\nhistory statement will invoke to crash Conquest analysis. Please remove history statement manually if you work with 2005 version.\n")} }
                   write(syntax,file.path(pfad,paste(Name,".cqc",sep="")),sep="\n")}

anker <- function(lab, prm, qMatrix, domainCol, itemCol, valueCol )  {
                  stopifnot(ncol(lab)==2)
                  if ( !ncol(prm) == 2 )   {                                    ### wenn itemliste nicht unique ... 'domain'-Spalte kann ausgelassen werden
                       if ( is.null(itemCol))  { stop("If anchor parameter frame has more than two columns, 'itemCol' must be specified.\n")}
                       if ( is.null(valueCol)) { stop("If anchor parameter frame has more than two columns, 'valueCol' must be specified.\n")}
                       allVars <- list(domainCol = domainCol, itemCol=itemCol, valueCol=valueCol)
                       allNams <- lapply(allVars, FUN=function(ii) {existsBackgroundVariables(dat = prm, variable=ii)})
                       notIncl <- setdiff ( colnames(qMatrix)[-1], prm[,allNams[["domainCol"]]])
                       if ( length( notIncl ) > 0 ) { stop(paste ( "Q matrix contains domain(s) ",paste("'",paste(notIncl, collapse="', '"),"'",sep="")," which are not included in the '",allNams[["domainCol"]],"' column of the anchor parameter frame.\n",sep="")) }
                       weg     <- setdiff ( unique(prm[,allNams[["domainCol"]]]), colnames(qMatrix)[-1])
                       if ( length ( weg ) > 0 ) {
                            ind <- wo.sind ( weg, prm[,allNams[["domainCol"]]], quiet = TRUE)
                            cat(paste("Remove ",length(ind)," rows from the anchor parameter frame which do not belong to any of the specified domains in the Q matrix.\n",sep=""))
                            prm <- prm[-ind,]
                       }
                       prm     <- prm[,c(allNams[["itemCol"]], allNams[["valueCol"]])]
                  }
                  colnames(prm) <- c("item","parameter")
                  dopp<- which(duplicated(prm[,"item"]))
                  if(length(dopp)>0) { cat(paste("W A R N I N G !!   Found ",length(dopp)," duplicate item identifiers in anchor list. Duplicated entries will be deleted.\n",sep="")) ; prm <- prm[which(!duplicated(prm[,"item"])), ] }
                  ind <- intersect(lab[,"item"],prm[,"item"])
                  if(length(ind) == 0) {stop("No common items found in 'lab.file' and 'prm.file'.\n")}
                  if(length(ind) > 0)  {cat(paste(length(ind), " common items found in 'lab.file' and 'prm.file'.\n",sep="")) }
                  resT<- merge(lab, prm, by = "item", sort = FALSE, all = FALSE)
                  res <- data.frame(resT[sort(resT[,2],decreasing=FALSE,index.return=TRUE)$ix,], stringsAsFactors = FALSE)[,-1]
                  stopifnot(nrow(res) == length(ind))
                  return(list ( resConquest = res, resTam = resT[,-2]))}

isLetter <- function ( string ) {
            splt <- strsplit(string, "")
            isL  <- lapply(splt, FUN = function ( x ) {
                    ind <- which ( x %in% c( letters , LETTERS ))
                    x[setdiff(1:length(x),ind)] <- " "
                    x <- crop(paste(x, sep="", collapse=""))
                    x <- unlist ( strsplit(x, " +") )
                    return(x)  } )
            return(isL)}

### columnItemNames         ... in welcher Spalte der q-Matrix stehen Itemnamen?
### columnsDimension        ... in welchen Spalten der Q-Matrix stehen die Dimensionen?
###                             Default: in erster Spalte stehen Itemnamen, in allen uebrigen Spalten stehen Indikatoren fuer Dimensionen
.writeScoreStatementMultidim <- function(data, itemCols, qmatrix, columnItemNames = 1 ,columnsDimensions = -1, use.letters=use.letters , allowAllScoresEverywhere) {
            n.dim      <- (1:ncol(qmatrix) )[-columnItemNames]                  ### diese Spalten bezeichnen Dimensionen. untere Zeile: Items, die auf keiner Dimension laden, werden bereits in prep.conquest entfernt. hier nur check
            stopifnot(length( which( rowSums(qmatrix[,n.dim,drop = FALSE]) == 0))==0)
      	    if(length(setdiff(names(tableUnlist(qmatrix[,-1, drop = FALSE])), c("0","1"))) > 0 )  {
               cat("Found unequal factor loadings for at least one dimension. This will result in a 2PL model.\n")
               for (u in 2:ncol(qmatrix)) {qmatrix[,u] <- as.character(round(qmatrix[,u], digits = 3))}
            }                                                                   ### obere Zeile: Identifiziere Items mit Trennschaerfe ungleich 1.
            stopifnot(all(qmatrix[,1] == itemCols))                             ### untere Zeile: Items im Datensatz, aber nicht in Q-Matrix? wird bereits in prep.conquest behandelt
            cat(paste("Q matrix specifies ",length(n.dim)," dimension(s).\n",sep=""))
            stopifnot(length(setdiff(colnames(data[,itemCols]),  qmatrix[,columnItemNames]) )==0)
            unique.patter <- qmatrix[which(!duplicated(do.call("paste", qmatrix[,-1, drop = FALSE] ))), -1, drop = FALSE]
            colnames(unique.patter) <- paste("Var",1:ncol(unique.patter), sep="")## obere Zeile: Finde alle uniquen Pattern in qmatrix! Jedes unique Pattern muss in Conquest einzeln adressiert werden!
            score.matrix  <- data.frame(score=1, unique.patter, matrix(NA, nrow= nrow(unique.patter), ncol=length(itemCols), dimnames=list(NULL, paste("X",1:length(itemCols),sep=""))),stringsAsFactors = FALSE)
            scoreColumns  <- grep("^Var",colnames(score.matrix))
            for (i in 1:length(itemCols))  {                                    ### gebe alle Items auf den jeweiligen Dimensionen
               qmatrix.i    <- qmatrix[qmatrix[,columnItemNames] == itemCols[i],]## auf welcher Dimension laedt Variable i? Untere Zeile: in diese Zeile von score.matrix muss ich variable i eintragen
               matchRow     <- which(sapply ( 1:nrow(score.matrix) , function(ii) {all ( as.numeric(qmatrix.i[,n.dim]) == as.numeric(score.matrix[ii,scoreColumns])) }))
               stopifnot(length(matchRow) == 1)
               matchColumn  <- min(which(is.na(score.matrix[matchRow,])))       ### in welche spalte von Score.matrix muss ich variable i eintragen?
               stopifnot(length(matchColumn) == 1)
               score.matrix[matchRow,matchColumn] <- i
		        }
            rowsToDelete <- which(is.na(score.matrix[, max(scoreColumns) + 1])) ### welche Zeilen in Score.matrix koennen geloescht werden?
            if(length(rowsToDelete)>0) {score.matrix <- score.matrix[-rowsToDelete, ]}
            for (ii in 1:nrow(score.matrix)) {score.matrix[,ii] <- as.character(score.matrix[,ii])}
            score.matrix <- fromMinToMax(dat = data[,itemCols, drop = FALSE], score.matrix = score.matrix, qmatrix = qmatrix, allowAllScoresEverywhere = allowAllScoresEverywhere, use.letters = use.letters)
            kollapse <- lapply(1:nrow(score.matrix), FUN=function(ii) {na.omit(as.numeric(score.matrix[ii,-c(1,scoreColumns)]))})
            kollapse.diff   <- lapply(kollapse,FUN=function(ii) {c(diff(ii),1000)})
            kollapse.ascend <- lapply(kollapse.diff, FUN=function(ii) {unique(c(0, which(ii!=1)))})
            kollapse.string <- list()
            for (a in 1:length(kollapse.ascend))  {
                string   <- list()
                for (i in 2:length(kollapse.ascend[[a]]))   {
                    string.i <- unique( c(kollapse[[a]][kollapse.ascend[[a]][i-1]+1], kollapse[[a]][kollapse.ascend[[a]][i]]))
                    string.i <- ifelse(length(string.i) == 2,paste(string.i[1],"-",string.i[2],sep=""),as.character(string.i))
                    string[[i]] <- string.i
				        }
                string <- paste(unlist(string),collapse=", ")
                kollapse.string[[a]] <- string
			      }
            ### Pruefung, ob "tranformation" des score-statements ok ist
            control <- lapply(kollapse.string,FUN=function(ii) {eval(parse(text=paste("c(",gsub("-",":",ii),")",sep="")))})
            if (!all(unlist(lapply(1:length(control), FUN=function(ii) {all(kollapse[[ii]] == control[[ii]])})))) {
                cat("Error in creating score statement.\n")
			      }
            score.matrix <- data.frame(prefix="score",score.matrix[,c(1,scoreColumns)],items="! items(",kollapse.string=unlist(kollapse.string),suffix=");",stringsAsFactors=F)
            score.statement <- sapply(1:nrow(score.matrix), FUN=function(ii) { paste(score.matrix[ii,],collapse=" ")})
            return(score.statement) }


### Hilfsfunktion fuer .writeScoreStatementMultidim()
fromMinToMax <- function(dat, score.matrix, qmatrix, allowAllScoresEverywhere, use.letters)    {
                all.values <- alply(as.matrix(score.matrix), .margins = 1, .fun = function(ii) {names(tableUnlist(dat[,na.omit(as.numeric(ii[grep("^X", names(ii))])), drop = FALSE]))  })
                if ( allowAllScoresEverywhere == TRUE ) {                       ### obere Zeile: WICHTIG: "alply" ersetzt "apply"! http://stackoverflow.com/questions/6241236/force-apply-to-return-a-list
                    all.values <- lapply(all.values, FUN = function(ii) {sort(asNumericIfPossible(unique( unlist ( all.values ) ), verbose = FALSE, ignoreAttributes = TRUE ) ) } )
                }
                if(use.letters == TRUE )  {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(LETTERS[which(LETTERS == ii[1]) : which(LETTERS == ii[length(ii)])], collapse=" "),")") } ) ) }
                if(use.letters == FALSE ) {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(ii[1] : ii[length(ii)],collapse = " "),")")  } ) ) }
                scoring <- unlist( lapply( minMaxRawdata , FUN = function(ii) { paste("(", paste( 0 : (length(unlist(strsplit(ii, " ")))-3), collapse = " "),")")}) )
                stopifnot(length(scoring) == length( minMaxRawdata ), length(scoring) == nrow(score.matrix )  )
                options(warn = -1)                                              ### warnungen aus
                for (i in 1:nrow(score.matrix))    {
                    score.matrix$score[i] <- minMaxRawdata[i]
                    targetColumns         <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) != 0 ) )
                    stopifnot(length(targetColumns) > 0 )
                    score.matrix[i,targetColumns]  <- unlist(lapply(score.matrix[i,targetColumns], FUN = function ( y ) {paste( "(", paste(as.numeric(y) * na.omit(as.numeric(unlist(strsplit(scoring[i]," ")))), collapse = " "), ")")}))
                    nonTargetColumns      <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) == 0 ) )
                    if ( length ( nonTargetColumns ) > 0 )    {
                       score.matrix[i,nonTargetColumns]  <- "()"
                    }
                }
                options(warn = 0)                                               ### warnungen wieder an
                return(score.matrix)}

getConquestVersion <- function ( path.conquest , path.temp , asDate = TRUE ) {
    wd <- path.temp
		f <- file.path ( wd , "delete.cqc" )
		write ( "quit;" , f )
		f <- normalizePath ( f )
		path.conquest <- normalizePath ( path.conquest )
		cmd <- paste ( "\"", path.conquest, "\" \"", f , "\"" , sep ="")
		r <- NULL
		suppressWarnings(try ( r <- system ( command = cmd , intern = TRUE ) , silent = TRUE ))
		file.remove ( f )
		if ( !is.null ( r ) ) {
				r <- r[1]
				r <- sub ( "ConQuest build: " , "" , r )
				r <- gsub ( "\\s+" , "-" , r )
				if ( asDate ) r <- as.date(r)
		}
		return (r)}

### Hilfsfunktionen fuer gen.syntax
userSpecifiedList <- function ( l, l.default ) {
		if ( !is.null ( names ( l ) ) ) {
				names ( l ) <- match.arg ( names(l) , names(l.default) , several.ok = TRUE )
		} else {
        if(length(l) > length(l.default) )  {
           stop("Length of user-specified list with more elements than default list.\n")
        }
				names ( l ) <- names ( l.default )[seq(along=l)]
		}
		if ( length(l) < length(l.default) ) {
				l <- c ( l , l.default )
				l <- l[!duplicated(names(l))]
				l <- l[match ( names (l) , names(l.default) )]
		}
		return(l)}

### splits the string only on the first or the last occurrence of the separator
### uses no longer package "stringr"
halveString <- function (string, pattern, first = TRUE )  {
     allSplit<- strsplit(x = string, split = pattern)
     if(first==TRUE)  {
        ret <- as.matrix(data.frame(X1 = unlist(lapply(allSplit, FUN = function (l) { l[1]})),
               X2 = unlist(lapply(allSplit, FUN = function (l) {
                    if(length(l) == 1 )  { ret <- NA } else { ret <- paste(l[-1], collapse=pattern)}
                    return(ret)})), stringsAsFactors = FALSE))
     }  else  {
        ret <- as.matrix(data.frame(X1 = unlist(lapply(allSplit, FUN = function (l) {paste(l[1:(length(l)-1)], collapse=pattern)})),
               X2 = unlist(lapply(allSplit, FUN = function (l) {
                    if(length(l)==1) { ret <- NA}  else { ret <- l[length(l)]}
                    return(ret)})), stringsAsFactors = FALSE))
     }
     return(ret)}

### Funktion komplett neu geschrieben, 1. Dezember 2011; nutzt Funktion "table.muster"
desk.irt <- function(daten, itemspalten, na=NA,percent=FALSE,reduce=TRUE,codebook=list(datei=NULL,item=NULL,value=NULL,lab=NULL, komp=NULL), quiet = FALSE ) {
            if( !"data.frame" %in% class(daten) ) {stop("'daten' must be of class 'data.frame'.\n")}
             if(!missing(itemspalten)) {daten <- daten[,itemspalten,drop=FALSE]}
             if (is.na(na[1])==FALSE) {                                         ### wenn spezifiziert, werden hier missings recodiert
                 recode.statement <- paste(na,"= NA",collapse="; ")
                 daten            <- data.frame(sapply(daten,FUN=function(ii) {recode(ii,recode.statement)}),stringsAsFactors=FALSE)
             }
             specific.codes <- lapply(daten,function(ii){NULL})                 ### definiert ggf. Spezifische Codes, nach denen je Variable gesucht werden soll
             if(!is.null(codebook$datei) & !is.null(codebook$value))  {
               specific.codes <- lapply(as.list(colnames(daten)), FUN=function(ii) {
                                 codebook$datei[codebook$datei[,codebook$item] == ii,c(codebook$item,codebook$value)] } )
               kein.eintrag   <- which(sapply(specific.codes,FUN=function(ii) {nrow(ii)==0}))
               if(length(kein.eintrag)>0)  {cat(paste(length(kein.eintrag)," item(s) missing in codebook:\n",sep=""))
                                            cat(paste(colnames(daten)[kein.eintrag],collapse=", ")); cat("\n")}
             }
             results        <- lapply(1:ncol(daten), FUN=function(ii) {
                               res.i <- table.muster(vektor=daten[,ii], mustervektor=specific.codes[[ii]]$value)
                               namen.res.i <- names(res.i)
                               if(length(res.i)==0) {
                                  if(quiet == FALSE ) { cat(paste("Item '",colnames(daten)[ii],"' without any valid values.\n",sep=""))}
                                  res.i <- 0
                                  namen.res.i <- NA}
                               Label <- NA
                               KB <- NA
                               if(!is.null(codebook$lab))  {Label <- codebook$datei[codebook$datei[,codebook$item] == colnames(daten)[ii],codebook$lab]}
                               if(!is.null(codebook$komp)) {KB    <- codebook$datei[codebook$datei[,codebook$item] == colnames(daten)[ii],codebook$komp]}
                               res.i <- data.frame(item.nr = ii, item.name = colnames(daten)[ii], Label = Label, KB = KB, cases = length(daten[,ii]),Missing=sum(is.na(daten[,ii])),valid=sum(!is.na(daten[,ii])),Codes=namen.res.i,Abs.Freq=as.numeric(res.i),Rel.Freq=as.numeric(res.i)/sum(!is.na(daten[,ii])), item.p=mean(na.omit(daten[,ii])), stringsAsFactors=FALSE)
             })
             results        <- do.call("rbind",results)
             if(reduce == TRUE)  {
                weg   <- which ( results[,"Codes"] == min(results[,"Codes"]) )
                drin  <- setdiff ( 1:nrow(results), weg )
                zusatz<- setdiff ( results[weg,"item.name"], results[drin,"item.name"])
                if ( length(zusatz)>0) {
                     zusatz <- match ( zusatz, results[,"item.name"])
                     drin   <- unique ( c ( zusatz, drin ))
                     weg    <- setdiff ( 1:nrow(results), drin )
                }
                results <- results[drin,]
             }
             if(percent == TRUE) {results$Rel.Freq <- 100 * results$Rel.Freq}
             return(results)}

### Hilfsfunktion, wie table(), nur mit festen Werten
### z.B. table.muster(a, c(1,5,7,8,9)) sucht, wie oft jede Zahl in "mustervektor" in "vektor" vorkommt
### Warnung, wenn zusaetzliche Zahlen in Vektor stehen
table.muster <- function(vektor, mustervektor = NULL, weights, na.rm = TRUE, useNA = c("no", "ifany", "always")) {
               if(length(mustervektor)>0) {
                  additional.values <- na.omit( setdiff(vektor,mustervektor))
                  if( length( additional.values ) > 0 )   {
                     cat("Warning: Found additional values not defined in 'mustervektor':\n")
                     cat(paste(additional.values,collapse=", ")); cat("\n")
                     cat("Concatenate additional values to 'mustervektor'.\n")
                     mustervektor <- c(mustervektor, additional.values)
                  }
                  notInData <- setdiff(mustervektor, vektor)
                  if ( length(notInData) > 0) {
                       vektor <- factor(vektor, levels = as.character(mustervektor))
                  }
                }
                  if(missing(weights)) {
                     Table <- table(vektor, useNA =useNA )
                  }  else  {
                     if ( length(notInData) > 0) {
                          vektor <- c(vektor, notInData)
                          weights<- c(weights, rep(0, length(notInData)))
                     }
                     Table <- wtd.table(x = vektor, weights = weights, na.rm =na.rm  )$sum.of.weights
                  }
                return(Table)}

wo.sind <- function(a,b,quiet=FALSE) {
            b <- data.frame(1:length(b),b,stringsAsFactors=FALSE)               ### zusaetzliche Syntaxbefehle sind notwendig, damit die Funktion mit missing values umgehen kann.
            if(sum(which(is.na(a)))>0)     {cat("a contains missing values. \n")}
            if(sum(which(is.na(b[,2])))>0) {cat("b contains missing values. \n")}
            if(length(na.omit(a)) > length(unique(na.omit(a))))     {cat("a contains duplicate elements. \n")}
            if(length(intersect(a,b[,2])) == 0) {
               cat("No common elements in a and b. \n")
               reihe <- NULL
            }  else {
               if(quiet==FALSE) { if(length(intersect(a,b[,2])) > 0) {if(length(setdiff(a,b[,2]))>0)      {cat("Not all Elemente of a included in b. \n")} } }
               a <- na.omit(unique(a))                                          ### Sofern vorhanden, werden missing values aus a entfernt
               b <- na.omit(b)                                                  ### Sofern vorhanden, werden missing values aus b entfernt; aber: Rangplatz der
               if(length(a)>0) {                                                ### der nicht fehlenden Elemente in b bleibt erhalten
                  reihe <- b[ which(b[,2] %in% a)  ,1]
                  if(quiet==FALSE) { cat(paste("Found",length(reihe),"elements.\n")) }
               }
               if(length(a)==0) {cat("No valid values in a.\n")}
            }
            return(reihe)}

### angelehnt an das Skript von Alexander Robitzsch, "R_Skalierung.odt", Seite 11
item.diskrim <- function(daten, itemspalten, na = NA, streng = TRUE) {
                 if(!missing(itemspalten))  {daten <- daten[,itemspalten]}
                 if(is.null(dim(daten)))    {
                    daten <- as.matrix(daten)
                    colnames(daten) <- "variable"
                 }
                 namen <- colnames(daten)
                 if (is.na(na[1])==FALSE) {
                     for (i in na)  {
                          daten[daten==i] <- NA
                     }
                 }
                 daten <- data.frame(daten, stringsAsFactors = FALSE)           ### Trennschaerfe ist eigentlich Korrelation des Items mit dem Summenscore ohne dieses Item. Dieses macht die Option "streng = T"; die andere berechnet Korrelation mit Summenscore einschliesslich dieses Items
                 daten <- data.frame(sapply(daten, FUN = function (uu ) {as.numeric(uu)}))
                 if(streng == TRUE)  {trennsch  <- sapply(colnames(daten), FUN = function(i) {
                    if(inherits(try(res <- cor(daten[,i],rowMeans(daten[,-match(i, colnames(daten)), drop = FALSE],na.rm = TRUE) ,use = "complete.obs"), silent = TRUE  ),"try-error"))  {
                       res <- NA
                    }
                    return(res)})
                 }
                 if(streng == FALSE) {trennsch  <- sapply(daten, FUN = function(i) {cor(i,rowMeans(daten,na.rm = TRUE) ,use="complete.obs")})}
                 trennsch  <- data.frame(item.name=colnames(daten),item.diskrim = trennsch,stringsAsFactors = FALSE)
                 return(trennsch)}

### foo <- prepRep( T.t1t2, dfrT1P, dfrT2P)
prepRep <- function ( calibT2, bistaTransfT1, bistaTransfT2, makeIdsUnique = TRUE) {
           if ( !"transfBista" %in% class(calibT2) ) { stop("'calibT2' object must be of class 'transfBista'.\n")}
           if ( !"transfBista" %in% class(bistaTransfT1) ) { stop("'bistaTransfT2' object must be of class 'transfBista'.\n")}
           if ( !"transfBista" %in% class(bistaTransfT2) ) { stop("'bistaTransfT2' object must be of class 'transfBista'.\n")}
           if (!nrow(calibT2[["itempars"]]) < nrow(bistaTransfT1[["itempars"]])) { stop("Mismatch between 'calibT2' and 'bistaTransfT1'. \n")}
           if (!nrow(calibT2[["itempars"]]) < nrow(bistaTransfT2[["itempars"]])) { stop("Mismatch between 'calibT2' and 'bistaTransfT2'. \n")}
     ### check: heissen die ID-Variablen etc. in beiden Datensaetzen gleich? ... falls nicht, misslingt unten das 'rbind' ... ggf. neue ID (falls nicht identisch in beiden Datensaetzen)
           idT1<- unique(bistaTransfT1[["all.Names"]][which(bistaTransfT1[["all.Names"]][,"par"] == "ID"),"derived.par"])
           idT2<- unique(bistaTransfT2[["all.Names"]][which(bistaTransfT2[["all.Names"]][,"par"] == "ID"),"derived.par"])
           stopifnot(length(idT1)==1, length(idT2)==1)
           if ( idT1 != idT2 ) {
                cat(paste("Warning: ID variables do not match between t1 and t2. ID for t1: '",idT1,"'. ID for t2: '",idT2,"'. \n    IDs will be unified with '",idT1,"'.\n",sep=""))
                recStat <- paste ( "'", idT2 , "' = '", idT1, "'", sep="")
                colnames ( bistaTransfT2[["personpars"]] ) <- recode ( colnames ( bistaTransfT2[["personpars"]] ), recStat)
           }
     ### finde Spalten mit Linkingfehlern
           lc  <- colnames( calibT2[["personpars"]] ) [grep("^linking", colnames(calibT2[["personpars"]]) )]
           if(length(lc)==0) { stop("No columns with linking error information found in 'calibT2'.\n")}
     ### benenne spalten in 'trend...' um
           lcn <- paste("trend", removePattern(string = lc, pattern = "linking"), sep="")
           colnames( calibT2[["personpars"]] ) [grep("^linking", colnames(calibT2[["personpars"]]) )] <- lcn
     ### suche Spalten zum Mergen
           merg<- c("group", "imp", "traitLevel", "dimension")
           frms<- list ( calibT2=calibT2, bistaTransfT1=bistaTransfT1, bistaTransfT2=bistaTransfT2 )
           toM <- unique(unlist(lapply ( names(frms), FUN = function ( l.Name ) {
                  l    <- frms[[l.Name]]
                  drin <- merg %in% colnames(l[["personpars"]])
                  fehlt<- merg[which(drin==FALSE)]
                  if (!all(drin == TRUE)) { cat(paste("Warning: Column(s) '",paste(fehlt, collapse = "', '"), "' are unexpectedly missing in '",l.Name,"'.\n",sep=""))}
                  keep <- merg[which(drin==TRUE)]
                  return(keep)})))
           if(length(toM)==0) { stop("Merging impossible.\n")}
     ### Reduziere Kalibrierungs-'datensatz' auf das Noetigste
           red <- calibT2[["personpars"]][,c(toM,  lcn)]
           red <- red[!duplicated(red),]
           dat1<- data.frame ( trend = "T1" , merge ( bistaTransfT1[["personpars"]], red, by = toM, all = TRUE))
     ### checks (sollten eigentlich ueberfluessig sein)
           stopifnot ( nrow(dat1) == nrow(bistaTransfT1[["personpars"]]))
           dat2<- data.frame ( trend = "T2" , merge ( bistaTransfT2[["personpars"]], red, by = toM, all = TRUE))
           stopifnot ( nrow(dat2) == nrow(bistaTransfT2[["personpars"]]))
     ### IDs unique machen (wenn gewuenscht)
           if ( makeIdsUnique == TRUE ) {
                dat1[, paste(idT1, "unique", sep="_")] <- paste(dat1[, "trend"], dat1[, idT1], sep="_")
                dat2[, paste(idT1, "unique", sep="_")] <- paste(dat2[, "trend"], dat2[, idT1], sep="_")
           }
           return(rbind ( dat1, dat2))}

plotICC <- function ( resultsObj, defineModelObj, item = NULL, personsPerGroup = 30, pdfFolder = NULL, smooth = 20 ) {
           if (smooth<5) {smooth <- 5}
           it  <- itemFromRes ( resultsObj )
           if ( !"est" %in% colnames(it) ) { it[,"est"] <- NA }
           if ( !"estOffset" %in% colnames(it) ) { it[,"estOffset"] <- NA }
           it[,"est"] <- rowSums(it[,c("est", "estOffset")], na.rm = TRUE)      ### untere Zeilen: wenn 1pl und 2pl gemeinsam im resultsobjekt auftauchen, gibt es fuer 1pl keinen
           if ( !"estSlope" %in% colnames(it) ) { it[,"estSlope"] <- 1 }        ### slope parameter; die werte sind NA. Zum Plotten muessen sie daher fuer das Raschmodell auf 1 gesetzt werden
           if ( length(which(is.na(it[,"estSlope"]))) > 0) { it[which(is.na(it[,"estSlope"])), "estSlope"] <- 1 }
           eapA<- eapFromRes (resultsObj)                                       ### eap fuer alle; muss wideformat haben!!!
           cat("Note: To date, only 1pl/2pl dichotomous models are supported.\n"); flush.console()
           if ( is.null(item) & is.null(pdfFolder)) {stop("If ICCs for more than one item should be displayed, please specify an output folder for pdf.\n")}
           if ( !is.null(pdfFolder)) { pdf(file = pdfFolder, width = 10, height = 7.5) }
           if ( !is.null ( item ) )  {
                if ( !item %in% it[,"item"]) { stop (paste("Item '",item,"' was not found in 'resultsObj'.\n",sep=""))}
                it <- it[which(it[,"item"] == item),]
           }
     ### Plotten findet fuer jedes Item separat statt
           pl  <- by ( data = it, INDICES = it[,c("model", "item")], FUN = function ( i ) {
                  xlm <- c(i[["est"]]+2, i[["est"]]-2)
                  anf <- -6                                                     # anf <- if ( min(xlm) < -4 ) { anf <- floor(min(xlm)) } else { anf  <- -4}
                  ende<- 6                                                      # ende<- if ( max(xlm) >  4 ) { ende<- ceiling(max(xlm)) } else { ende <- 4}
                  x   <- seq ( anf, ende, l = 400)
                  y   <- exp( i[["estSlope"]]*x - i[["est"]] ) / (1+exp( i[["estSlope"]]*x - i[["est"]] ))
                  plot (x, y, type = "l", main = paste("Item '",as.character(i[["item"]]),"'\n\n",sep=""), xlim = c(-6,6), ylim = c(0,1), xlab = "theta", ylab = "P(X=1)", col = "darkred", cex = 8, lwd = 2)
                  mtext( paste("Model = ",i[["model"]],"  |  Dimension = ",i[["dimension"]], "  |  difficulty = ",round(i[["est"]], digits = 3),"  |  Infit = ",round(i[["infit"]], digits = 3),"\n",sep=""))
                  eap <- eapA[intersect ( which (eapA[,"dimension"] == i[["dimension"]]) , which (eapA[,"model"] == i[["model"]])),]
                  if ( "defineMultiple" %in% class (defineModelObj)) {          ### Problem: je nachdem ob modelle gesplittet wurden oder nicht, muss der Itemdatensatz woanders gesucht werden ... Hotfix
                       woIst<- which ( lapply ( defineModelObj, FUN = function ( g ) {   g[["analysis.name"]] == i[["model"]] }) == TRUE)
                       stopifnot(length(woIst) == 1)
                       dat  <-defineModelObj[[woIst]][["daten"]]
                  }  else  {
                       dat  <- defineModelObj[["daten"]]
                  }
     ### Hotfix: ID namen identifizieren
                  id  <- unique(resultsObj[intersect(which(resultsObj[,"type"] == "tech"), which(resultsObj[,"par"] == "ID")),"derived.par"])
                  stopifnot(length(id)==1)
                  prbs<- na.omit ( merge ( dat[,c( "ID", as.character(i[["item"]]))], eap[,c( id, "EAP")], by.x = "ID", by.y = id))
                  anz <- round ( nrow(prbs) / personsPerGroup ) + 1             ### mindestens 'personsPerGroup' Personen pro Gruppe
                  if ( anz < 3 ) { anz <- 3 }
                  if ( anz > smooth) { anz <- round(smooth)}
                  eapQ<- quantile ( prbs[,"EAP"], probs = seq(0,1,l = anz))
                  prbs[,"gr"] <- num.to.cat ( x = prbs[,"EAP"], cut.points = eapQ[-c(1,length(eapQ))])
                  prbs<- do.call("rbind", by ( data = prbs, INDICES = prbs[,"gr"], FUN = function ( g ) {
                         g[,"mw"] <- mean(g[,"EAP"])
                         g[,"anz"]<- length(g[,"EAP"])
                         g[,"lh"] <- mean(g[, as.character(i[["item"]]) ])
                         return(g)}))
                  matr<- prbs[!duplicated(prbs[,c("mw", "lh")]),c("mw", "lh")]
                  matr<- data.frame(matr[sort(matr[,"mw"],decreasing=FALSE,index.return=TRUE)$ix,])
                  points ( x = matr[,"mw"], y = matr[,"lh"], cex = 1, pch = 21, bg = "darkblue")
                  lines ( x = matr[,"mw"], y = matr[,"lh"], col = "blue", lty = 3, lwd = 3) } )
           if ( !is.null(pdfFolder)) { dev.off() } }


