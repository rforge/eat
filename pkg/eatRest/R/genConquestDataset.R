####################################################################################################################
#
# genConquestDataset
# erzeugt Conquest Datensatz
# hervorgegangen aus prep.Conquest
#
# Version: 	0.8.0
# Depends:  gdata
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
#
# 2011-12-12 SW
# CHANGED: remove assign() from genConquestSynLab. table(unlist (...) ) replaced by eatTools:::table.unlist( ... )
# 0000-00-00 AA
#
# 22.06.2011: unerlaubte Variablennamen fuer explizite Variablen werden geaendert
# 27.06.2011: Alle Meldungen geben Funktionsname und Versionsnummer
# 07.07.2011: variiertes Handling wenn Variablen mit complete missing!
# 14.07.2011: group statement
# 15.07.2011: Pruefung, ob alle explizit definierten Variablen auch im Datensatz sind
# 06.08.2011: Pruefung, ob unrecodierte Missings im Datensatz sind, sollte nun schneller
#             gehen und auch fuer sehr grosse Datensaetze den Speicher nicht ueberlasten
#             Ausserdem Gewichtungsvariable
# 08.08.2011: MH auf stable gesetzt wegen besserer sourcebarkeit
# 20.09.2011: Moeglichkeit zur Missingrecodierung; separat fuer Testitems, HG-Var, DIF-Var, etc.
# 14.10.2011: MH gestabled
# 20.10.2011: MH library statements auskommentiert
# 15.11.2011: MH gestabled
# 25.11.2011: SW 'cat' durch 'eatTools:::sunk' ersetzt
# 12.12.2011: SW assign-befehl entfernt 
# 10.05.2012: SW more than one DIF variable
#
####################################################################################################################

### dat          ... Datensatz im zkdMaster-Format
### variablen    ... wo stehen Items im Datensatz, z.B. 5:120 oder -c(1:5)
### ID           ... wo steht ID-variable, entweder Spaltennummer oder Variablenname als String
### model        ... falls nicht definiert: eindimensional, ansonsten muss hier qmatrix uebergeben werden
###                  qmatrix als R-Dataframe; die spalte mit Itembezeichnung muss die Kennung "item" im
###                  Variablennamen haben, die Spalten mit den Dimensionen die kennung "dim"
### DIF.var      ... eine DIF-Variable, entweder als Spaltennummer oder Variablenname als String
### HG.var       ... ein oder mehrere Hintergrundvariablen, entweder als Spaltennummern oder Variablennamen,
###                  z.B. c(4,6), c("alter","geschlecht")
### weight.var   ... optional: Gewichtungsvariable, als Spaltennummer oder Variablenname
### anker        ... uebergeben wird R-Dataframe mit Ankerparametern.
###                  erwartet wird ein Dataframe mit zwei Spalten, eine mit Variablennamen, eine mit
###                  zu verankernden Parametern. Welche welche ist, wird darueber erkannt, ob die Spalte numerisch oder character ist.
### pfad.dataset ... wo soll der Conquest-Datensatz abgelegt werden?
###                  (vollstaendige Pfadangabe noetig, z.B. "P:/ZKD/model")
### name.dataset ... vollstaendiger Dateiname des zu erstellenden Conquest-Datensatzes, z.B. "testdaten.dat"
### pfad         ... wo sollen syntax-, label- und Outputdateien abgelegt werden?
###                  (vollstaendiger Pfad noetig)
### name.analyse ... Dateiname fuer Conquest-Input (nur Praefix, Suffixe werden automatisch vergeben)
### na           ... Liste mit Codes, die als NA zu behandeln sind, fuer Items und Hintergrundvariablen separat;
###                  z.B. na=list(items=c(6,7,8,9,96,97,98,99), DIF=9)

## genConquestDataset <- function(zkdMasterDataset, variablen, ID, model=NULL, DIF.var=NULL, HG.var=NULL, anker = NULL, pfad=getwd(), name.analyse,name.dataset,pfad.dataset)

genConquestDataset <- function(dat, variablen, ID, DIF.var=NULL, HG.var=NULL, group.var=NULL, weight.var=NULL, na=list(items=NULL, DIF=NULL, HG=NULL, group=NULL, weight=NULL), verbose=TRUE,
                               model.statement="item", remove.no.answers = FALSE,use.letters=FALSE, checkLink = FALSE)
                 {ver          <- "0.8.0"
                  # if(!exists("write.fwf")) {library(gdata)}                     ### Ankerstatement wird hier nicht verarbeitet, sondern zuletzt einfach an "gen.syntax" uebergeben
                  # if(!exists("recode")) {library(car)}
                  ### Datensatz darf keine missingstatements "mbd","mci" etc enthalten
                  nicht.erlaubt <-  c("mvi","mnr", "mci", "mbd", "mir", "mbi")
                  ### folgendes geht schneller als das bedeutungsgleiche      
                  ### all.codes     <- names(table(unlist(dat)))
                  all.codes     <- unique(unlist(lapply(dat, FUN=function(ii) {names(table(ii))})))
                  if(!sum(nicht.erlaubt %in% all.codes ) == 0) {eatTools:::sunk(paste("genConquestDataset_",ver,": Found uncollapsed missings in dataset: ",paste(nicht.erlaubt[which(nicht.erlaubt %in% all.codes)],collapse=", "),"\n",sep=""))
                                                                stop("Please run 'collapseMissings' for a start.\n")}
                  if(class(variablen) == "factor" )   {
                     eatTools:::sunk("Class of variable argument was 'factor'. Expect class 'numeric' or 'character'. \nVariables will be treated as 'character' anywhere. Please intervene if this is not intended!\n")
                     variablen <- as.character(variablen)
                  }
                  if(class(variablen) == "character") {
                     nicht.im.datensatz <- setdiff(variablen, colnames(dat))
                     if(length(nicht.im.datensatz) > 0 ) { stop(paste("Following variables not in dataset: ", paste(nicht.im.datensatz, collapse = ", "),".\n", sep=""))}
                  }                                                             ### untere Zeile: Hier stehen erstmal NUR die Testitems. Diese werden nun, sofern spezifiziert, recodiert
                  daten  <- data.frame( dat[,variablen,drop = FALSE], stringsAsFactors = FALSE)
                  is.NaN <- do.call("cbind", lapply(daten, FUN = function (uu) { is.nan(uu) } ) )
                  if(sum(is.NaN) > 0 ) {daten[is.NaN] <- NA}                    ### Wandle NaN in NA, falls es welche gibt
                  if(!is.null(na$items))
                    {rec.items <- paste(na$items,"=NA",collapse="; ")           ### definiere recodierungsvorschrift
                     for (i in 1:ncol(daten))
                         {daten[,i] <- car:::recode(daten[,i], rec.items)}}
   				        if(checkLink == TRUE) {
                     linkNaKeep <- checkLink(dat = daten, remove.non.responser = FALSE, verbose = FALSE )
                     linkNaOmit <- checkLink(dat = daten, remove.non.responser = TRUE, verbose = FALSE )
                     if(linkNaKeep == FALSE & linkNaOmit == FALSE ) {eatTools:::sunk("WARNING! Dataset is NOT completely linked (even if cases with missings on all items are removed).\n")}
                     if(linkNaKeep == FALSE & linkNaOmit == TRUE )  {eatTools:::sunk("Note: Dataset is not completely linked. This is probably only due to missings on all cases.\n")}
                     if(linkNaKeep == TRUE )                        {eatTools:::sunk("Dataset is completely linked.\n")}
                  }
                  namen.items <- colnames(daten)
                  allVars     <- list(namen.hg.var=HG.var, namen.dif.var=DIF.var, namen.weight.var=weight.var, namen.group.var=group.var)
                  all.Names   <- lapply(allVars, FUN=function(ii) {eatTools:::.existsBackgroundVariables(dat=dat,variable=ii)})
                  namen.hg.var <- all.Names$namen.hg.var
                  namen.dif.var <- all.Names$namen.dif.var
                  namen.weight.var <- all.Names$namen.weight.var
                  namen.group.var <- all.Names$namen.group.var
                  ### for (ii in seq(along=all.Names)) {assign(names(allVars)[ii], all.Names[[ii]])}
                  ### Conquest erlaubt keine expliziten Variablennamen, die ein "." oder "_" enthalten
                  namen.all.hg.vars <- list(namen.hg.var=namen.hg.var, namen.dif.var=namen.dif.var, namen.weight.var=namen.weight.var, namen.group.var=namen.group.var)
                  for ( i in seq(along=namen.all.hg.vars)) {
                       substituteNames <- .substituteSigns(dat=dat, variable= namen.all.hg.vars[[i]])
                       if(!all(substituteNames$old == substituteNames$new)) {
                          namen.all.hg.vars[[i]] <- substituteNames$new
                          eatTools:::sunk(paste("genConquestDataset_",ver,": Conquest does not allow '.', '-' and '_' in explicit variable names. Delete signs from variables names for explicit variables.\n",sep=""))
                          colnames(dat)[substituteNames$cols] <- substituteNames$new
                       }   
                  }
				  for (ii in seq(along=namen.all.hg.vars)) {assign(names(namen.all.hg.vars)[ii], namen.all.hg.vars[[ii]])}
                  ### Dif-Variablen und Testitems duerfen sich nicht ueberschneiden
                  if(length(intersect(namen.dif.var, namen.items))>0) {stop(paste("genConquestDataset_",ver,": Testitems and DIF variable overlap.\n",sep=""))}
   
                  ### weight-Variablen und Testitems duerfen sich nicht ueberschneiden
                  if(length(intersect(namen.weight.var, namen.items))>0) {stop(paste("genConquestDataset_",ver,": Testitems and weight variables overlap.\n",sep=""))}
   
                  ### HG-Variablen und Testitems duerfen sich nicht ueberschneiden
                  if(length(intersect(namen.hg.var, namen.items))>0) {stop(paste("genConquestDataset_",ver,": Testitems and HG variables overlap.\n",sep=""))}
                  
                  ### group Variablen und Testitems duerfen sich nicht ueberschneiden
                  if(length(intersect(namen.group.var, namen.items))>0) {stop(paste("genConquestDataset_",ver,": Testitems and group variables overlap.\n",sep=""))}
                                    
                  ### geprueft wird: enthaelt IRGENDEIN Testitem gar keine gueltigen Werte?
                  n.werte <- lapply(daten, FUN=function(ii) {table(ii)})
                  options(warn = -1)                                            ### zuvor: schalte Warnungen aus!
                  only.null.eins <- unlist( lapply(n.werte, FUN=function(ii) {all( names(ii) == c("0","1") ) }) )
                  options(warn = 0)                                             ### danach: schalte Warnungen wieder an!
                  n.werte <- sapply(n.werte, FUN=function(ii) {length(ii)})
                  n.mis   <- which(n.werte == 0)
				          namen.items.weg <- NULL
                  if(length(n.mis) >0) {eatTools:::sunk(paste("genConquestDataset_",ver,": Serious warning: ",length(n.mis)," testitems(s) without any values.\n",sep=""))
                                        if(verbose == TRUE) {eatTools:::sunk(paste(colnames(daten)[which(n.werte == 0)], collapse=", ")); eatTools:::sunk("\n") }
                                        stop()										
                                       }
                  n.constant <- which(n.werte == 1)
                  if(length(n.constant) >0) {eatTools:::sunk(paste("genConquestDataset_",ver,": Warning: ",length(n.constant)," testitems(s) are constants.\n",sep=""))
                                             if(verbose == TRUE) {foo <- lapply(n.constant,FUN=function(ii) {eatTools:::sunk(paste(colnames(daten)[ii],": ",names(table(daten[,ii])),sep="")); eatTools:::sunk("\n")})}
											stop()
											}
                  n.rasch   <- which( !only.null.eins )
                  if(length(n.rasch) >0 )   {eatTools:::sunk(paste("genConquestDataset_",ver,": Warning: ",length(n.rasch)," variable(s) are not strictly dichotomous with 0/1.\n",sep=""))
                                             for (ii in n.rasch)
                                                 {max.nchar <-  max(nchar(names(table(daten[,ii]))))
                                                  max.nchar.stacked <- c(max.nchar)
                                                  if(max.nchar>1) {eatTools:::sunk(paste("genConquestDataset_",ver,": Arity of variable ",colnames(daten)[ii]," exceeds 1.\n"))}
                                                  if(verbose == TRUE) {eatTools:::sunk(paste(colnames(daten)[ii],": ", paste( names(table(daten[,ii])),collapse=", "),"\n",sep=""))}}
                                             eatTools:::sunk("By default, all values except for 0 and 1 are treated as sysmis.\n")
                                             if(model.statement == "item")
                                               {eatTools:::sunk("WARNING: Sure you want to use 'model statement = item' even when items are not dichotomous?\n")} }
                  
                  ### identifiziere Faelle mit ausschliesslich missings
                 all.values   <- table(unique(unlist(lapply(daten, FUN=function(ii) {names(table(ii))}))))
                  if(length(all.values)!=2) {eatTools:::sunk(paste("genConquestDataset_",ver,": Warning: Found more than two non missing codes in overall testitems. Data does not seem to fit to the Rasch model.\n",sep=""))}
                  if(length(all.values)==2) {if(!all(names(all.values) == c("0","1"))) {eatTools:::sunk("Warning: Found codes departing from 0 and 1 in testitems. Data does not seem to fit to the Rasch model.\n")}}
                  weg.variablen <- rowSums(is.na(daten))                        ### identifiziere Fälle mit ausschließlich missings
                  weg.variablen <- which(weg.variablen == ncol(daten))
                  if(length(weg.variablen)>0) 
                    {eatTools:::sunk(paste("genConquestDataset_",ver,": Found ",length(weg.variablen)," cases with missings on all items.\n",sep=""))
                     if( remove.no.answers == TRUE)  {eatTools:::sunk("    Cases with missings on all items will be deleted.\n")}
                     if( remove.no.answers == FALSE) {weg.variablen <- NULL     ### WICHTIG: Wenn missings on all items beibehalten werden sollen, muß weg.variablen wieder zurückgesetzt werden!
                                                      eatTools:::sunk("Cases with missings on all items will be kept.\n")}}
                  hg.char <- NULL; DIF.char <- NULL; weight.char <- NULL; all.hg.char <- NULL        ### obere Zeile: wieviele Character haben die Variablen?
                  weg.dif <- NULL; weg.hg <- NULL; weg.weight <- NULL; namen.all.hg <- NULL; weg.group <- NULL
                              if(!is.null(HG.var))    {
                     if(!is.null(na$HG))                                        ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die HG-Variablen recodiert
                       {rec.hg <- paste(na$HG,"=NA",collapse="; ")              ### definiere recodierungsvorschrift
                        for (i in 1:ncol(dat[,namen.hg.var,drop=F]))
                            {dat[,namen.hg.var[i]] <- car:::recode(dat[,namen.hg.var[i]], rec.hg)}}## untere Zeile: wieviele "character" haben Hintergrundvariablen?
                     hg.info <- lapply(namen.hg.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="HG", itemdaten=daten)})
                     dat[,namen.hg.var] <- do.call("cbind", unlist(hg.info, recursive = FALSE)[3*(1:length(hg.info))-2])
                     weg.hg             <- unique(do.call("c", unlist(hg.info, recursive = FALSE)[3*(1:length(hg.info))]))
                     hg.char            <- do.call("c", unlist(hg.info, recursive = FALSE)[3*(1:length(hg.info))-1])
                     if(length(weg.hg)>0)                                       ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {eatTools:::sunk(paste("genConquestDataset_",ver,": Found ",length(weg.hg)," cases with missings on at least one HG variable.\n",sep=""))}
                  }
                  if(!is.null(group.var))  {
                     if(!is.null(na$group))                                     ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die HG-Variablen recodiert
                       {rec.group <- paste(na$group,"=NA",collapse="; ")        ### definiere recodierungsvorschrift
                        for (i in 1:ncol(dat[,namen.group.var,drop=F]))
                            {dat[,namen.group.var[i]] <- car:::recode(dat[,namen.group.var[i]], rec.group)}}
                     group.info <- lapply(namen.group.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="group", itemdaten=daten)})
                     dat[,namen.group.var] <- do.call("cbind", unlist(group.info, recursive = FALSE)[3*(1:length(group.info))-2])
                     weg.group             <- unique(do.call("c", unlist(group.info, recursive = FALSE)[3*(1:length(group.info))]))
                     group.char            <- do.call("c", unlist(group.info, recursive = FALSE)[3*(1:length(group.info))-1])
                     if(length(weg.group)>0)                                       ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {eatTools:::sunk(paste("genConquestDataset_",ver,": Found ",length(weg.group)," cases with missings on group variable.\n",sep=""))}
                   }
                  if(!is.null(DIF.var))  {
                     if(!is.null(na$DIF))                                       ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die DIF-Variablen recodiert
                       {rec.dif <- paste(na$DIF,"=NA",collapse="; ")            ### definiere recodierungsvorschrift
                        for (i in 1:ncol(dat[,namen.dif.var,drop=F]))
                            {dat[,namen.dif.var[i]] <- car:::recode(dat[,namen.dif.var[i]], rec.hg)}}
                     dif.info <- lapply(namen.dif.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="DIF", itemdaten=daten)})
                     dat[,namen.dif.var] <- do.call("cbind", unlist(dif.info, recursive = FALSE)[3*(1:length(dif.info))-2])
                     weg.dif             <- unique(do.call("c", unlist(dif.info, recursive = FALSE)[3*(1:length(dif.info))]))
                     dif.char            <- do.call("c", unlist(dif.info, recursive = FALSE)[3*(1:length(dif.info))-1])
                     if(length(weg.dif)>0)                                      ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {eatTools:::sunk(paste("genConquestDataset_",ver,": Found ",length(weg.dif)," cases with missings on DIF variable.\n",sep=""))}
                  }
                  if(!is.null(weight.var))
                    {if(length(weight.var)!=1) {stop("Use only one weight variable.")}
                     if(!is.null(na$weight))                                    ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die DIF-Variablen recodiert
                       {rec.weight <- paste(na$weight,"=NA",collapse="; ")      ### definiere recodierungsvorschrift
                        dat[,namen.weight.var] <- car:::recode(dat[,namen.weight.var], rec.weight)}
                     weight.info <- lapply(namen.weight.var, FUN = function(ii) {.checkContextVars(x = dat[,ii], varname=ii, type="weight", itemdaten=daten)})
                     dat[,namen.weight.var] <- do.call("cbind", unlist(weight.info, recursive = FALSE)[3*(1:length(weight.info))-2])
                     weg.weight             <- unique(do.call("c", unlist(weight.info, recursive = FALSE)[3*(1:length(weight.info))]))
                     weight.char            <- do.call("c", unlist(weight.info, recursive = FALSE)[3*(1:length(weight.info))-1])
                     if(length(weg.weight)>0)                                   ### untere Zeile: dies geschieht erst etwas später, wenn datensatz zusammengebaut ist
                       {eatTools:::sunk(paste("genConquestDataset_",ver,": Found ",length(weg.weight)," cases with missings on weight variable.\n",sep=""))}}
                  namen.all.hg <- unique(c(namen.dif.var,namen.hg.var,namen.group.var,namen.weight.var))## Achtung: group- und DIF- bzw. group- und HG-Variablen duerfen sich ueberschneiden!
                  if(!is.null(namen.all.hg)) {all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(dat[,ii]))))})}
                  var.char <- sapply(daten, FUN=function(ii) {max(nchar(as.character(na.omit(ii))))})
                  no.number <- setdiff(1:length(var.char), grep("[[:digit:]]",var.char))
                  if(length(no.number)>0) {var.char[no.number] <- 1}            ### -Inf steht dort, wo nur missings sind, hier soll die Characterbreite auf 1 gesetzt sein
                  if(use.letters == TRUE)                                       ### sollen Buchstaben statt Ziffern beutzt werden? Dann erfolgt hier Recodierung.
                    {rec.statement <- paste(0:25,"='",LETTERS,"'",sep="",collapse="; ")
                     for (i in 1:ncol(daten))                                   ### Warum erst hier? Weil Prüfungen (auf Dichotomität etc. vorher stattfinden sollen)
                         {daten[,i] <- car:::recode(daten[,i], rec.statement)}
                     var.char <- rep(1,ncol(daten))
				    }                            ### var.char muß nun neu geschrieben werden, da nun alles wieder einstellig ist!
                  daten <- data.frame(ID=as.character(dat[,ID]), dat[,namen.all.hg,drop=F], daten, stringsAsFactors=F)
                  daten$ID <- formatC(daten$ID, width=max(as.numeric(names(table(nchar(daten$ID))))))
                  daten$ID <- gsub(" ","0",daten$ID)                            ### vereinheitliche Laenge der IDs!
                  stopifnot(length( as.numeric(names(table(nchar(daten[,"ID"])))) ) == 1)
                  #if(length(weg.dif)>0 | length(weg.hg)>0 | length(weg.variablen)>0 )
                  #  {weg.all <- unique(c(weg.dif, weg.hg, weg.weight, weg.variablen))       ### obere Zeile: Remove cases with missings on DIF variable and/or HG variables and/or items
                  #   cat(paste("Remove ",length(weg.all)," cases altogether.\n",sep=""))
                  #   daten <- daten[-weg.all,]}
                  fixed.width <- c(as.numeric(names(table(nchar(daten[,"ID"])))), all.hg.char, rep(max(var.char),length(var.char)))
                  return(list(daten.dat = daten, daten.width = fixed.width,namen.items=namen.items, namen.hg.var=namen.hg.var, namen.dif.var=namen.dif.var,DIF.char=DIF.char,namen.group.var=namen.group.var, namen.weight.var=namen.weight.var, weight.char=weight.char,namen.all.hg=namen.all.hg,all.hg.char=all.hg.char,var.char=max(var.char)))
				}
				  
.checkContextVars <- function(x, varname, type, itemdaten)   {
                     if(missing(varname))  {varname <- "ohne Namen"}
                     if(class(x) != "numeric")  {                               ### ist Variable numerisch?
                        if (type == "weight") {stop(paste(type, " variable has to be 'numeric' necessarily. Automatic transformation is not recommended. Please transform by yourself.\n",sep=""))}
                        eatTools:::sunk(paste(type, " variable has to be 'numeric'. Variable '",varname,"' of class '",class(x),"' will be transformed to 'numeric'.\n",sep=""))
                        x <- unlist(asNumericIfPossible(dat = data.frame(x, stringsAsFactors = FALSE), transform.factors = TRUE, maintain.factor.scores = FALSE, verbose=FALSE))
                        if(class(x) != "numeric")  {                            ### erst wenn asNumericIfPossible fehlschlägt, wird mit Gewalt numerisch gemacht, denn für Conquest MUSS es numerisch sein
                           x <- as.numeric(as.factor(x))
                        }
                        eatTools:::sunk(paste("    '", varname, "' was converted into numeric variable of ",length(table(x))," categories. Please check whether this was intended.\n",sep=""))
                     }
                     mis     <- length(table(x))
                     if(mis == 0 )  {stop(paste("Error: ",type," Variable '",varname,"' without any values.",sep=""))}
                     if(mis == 1 )  {stop(paste("Error: ",type," Variable '",varname,"' is a constant.",sep=""))}
                     if(type == "DIF" | type == "group") {if(mis > 10)   {eatTools:::sunk(paste("Serious warning: ",type," Variable '",varname,"' with more than 10 categories. Recommend recoding. \n",sep=""))}}
                     char    <- max(nchar(as.character(na.omit(x))))
                     weg     <- which(is.na(x))
                     if(length(weg) > 0 ) {eatTools:::sunk(paste("Warning: Found ",length(weg)," cases with missing on ",type," variable '",varname,"'. Conquest may collapse if those cases are not deleted.\n",sep=""))}
                     if(type == "DIF" ) {
                                   if(mis > 2 )   {eatTools:::sunk(paste(type, " Variable '",varname,"' does not seem to be dichotomous.\n",sep=""))}
                                   n.werte <- lapply(itemdaten, FUN=function(iii){by(iii, INDICES=list(x), FUN=table)})
                                   completeMissingGroupwise <- data.frame(t(sapply(n.werte, function(ll){lapply(ll, FUN = function (uu) { length(uu[uu>0])}  )})), stringsAsFactors = FALSE)
                                   for (iii in seq(along=completeMissingGroupwise)) {
                                        missingCat.i <- which(completeMissingGroupwise[,iii] == 0)
                                        if(length(missingCat.i) > 0) {
                                           eatTools:::sunk(paste("Warning: Following items with no values in ",type," variable '",varname,"', group ",iii,": \n",sep=""))
                                           eatTools:::sunk(paste(rownames(completeMissingGroupwise)[missingCat.i],collapse=", ")); cat("\n")
                                        }
                                        constantCat.i <- which(completeMissingGroupwise[,iii] == 1)
                                        if(length(constantCat.i) > 0) {
                                           eatTools:::sunk(paste("Warning: Following items are constants in ",type," variable '",varname,"', group ",iii,":\n",sep=""))
                                           eatTools:::sunk(paste(rownames(completeMissingGroupwise)[constantCat.i],collapse=", ")); cat("\n")
                                        }
                                   }
                     }
                     return(list(x = x, char = char, weg = weg))}
				  
  
                   							
.substituteSigns <- function(dat, variable ) {
                    if(!is.null(variable)) {
           					   variableNew <- gsub("_|\\.|-", "", variable)
                       cols        <- match(variable, colnames(dat)) 
           					   return(list(cols=cols, old=variable,new=variableNew))
           					}
                    if(is.null(variable)) {return(list(old=TRUE,new=TRUE))}
                    }		
