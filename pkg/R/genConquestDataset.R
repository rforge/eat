####################################################################################################################
#
# genConquestDataset
# erzeugt Conquest Datensatz
# hervorgegangen aus prep.Conquest
#
# Version: 	0.7.0
# Depends:  gdata
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
#
# 2011-12-12 SW
# CHANGED: remove assign() from genConquestSynLab. table(unlist (...) ) replaced by table.unlist( ... )
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
# 25.11.2011: SW 'cat' durch 'sunk' ersetzt
# 12.12.2011: SW assign-befehl entfernt 
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
                               model.statement="item", remove.no.answers = FALSE,use.letters=FALSE)
                 {ver          <- "0.7.0"
                  # if(!exists("write.fwf")) {library(gdata)}                     ### Ankerstatement wird hier nicht verarbeitet, sondern zuletzt einfach an "gen.syntax" uebergeben
                  # if(!exists("recode")) {library(car)}
                  ### Datensatz darf keine missingstatements "mbd","mci" etc enthalten
                  nicht.erlaubt <-  c("mvi","mnr", "mci", "mbd", "mir", "mbi")
                  ### folgendes geht schneller als das bedeutungsgleiche      
                  ### all.codes     <- names(table(unlist(dat)))
                  all.codes     <- unique(unlist(lapply(dat, FUN=function(ii) {names(table(ii))})))
                  if(!sum(nicht.erlaubt %in% all.codes ) == 0) {sunk(paste("genConquestDataset_",ver,": Found uncollapsed missings in dataset: ",paste(nicht.erlaubt[which(nicht.erlaubt %in% all.codes)],collapse=", "),"\n",sep=""))
                                                                stop("Please run 'collapseMissings' for a start.\n")}
                  daten <- data.frame( dat[,variablen,drop=F], stringsAsFactors=FALSE)# Hier stehen erstmal NUR die Testitems. Diese werden nun, sofern spezifiziert, recodiert
                  if(!is.null(na$items)) 
                    {rec.items <- paste(na$items,"=NA",collapse="; ")           ### definiere recodierungsvorschrift
                     for (i in 1:ncol(daten))
                         {daten[,i] <- recode(daten[,i], rec.items)}}

                  namen.items <- colnames(daten)
                  allVars     <- list(namen.hg.var=HG.var, namen.dif.var=DIF.var, namen.weight.var=weight.var, namen.group.var=group.var)
                  all.Names   <- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat=dat,variable=ii)})
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
                          sunk(paste("genConquestDataset_",ver,": Conquest does not allow '.', '-' and '_' in explicit variable names. Delete signs from variables names for explicit variables.\n",sep=""))
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
                  n.werte <- lapply(1:ncol(daten), FUN=function(ii) {table(daten[,ii])})
                  options(warn = -1)                                            ### zuvor: schalte Warnungen aus!
                  only.null.eins <- unlist( lapply(n.werte, FUN=function(ii) {all( names(ii) == c("0","1") ) }) )
                  options(warn = 0)                                             ### danach: schalte Warnungen wieder an!
                  n.werte <- sapply(1:length(n.werte), FUN=function(ii) {length(n.werte[[ii]])})
                  n.mis   <- which(n.werte == 0)
				  namen.items.weg <- NULL
                  if(length(n.mis) >0) {sunk(paste("genConquestDataset_",ver,": Serious warning: ",length(n.mis)," testitems(s) without any values.\n",sep=""))
                                        if(verbose == TRUE) {sunk(paste(colnames(daten)[which(n.werte == 0)], collapse=", ")); sunk("\n") }
                                        stop()										
                                       }
                  n.constant <- which(n.werte == 1)
                  if(length(n.constant) >0) {sunk(paste("genConquestDataset_",ver,": Warning: ",length(n.constant)," testitems(s) are constants.\n",sep=""))
                                             if(verbose == TRUE) {foo <- lapply(n.constant,FUN=function(ii) {sunk(paste(colnames(daten)[ii],": ",names(table(daten[,ii])),sep="")); sunk("\n")})}
											stop()
											}
                  n.rasch   <- which( !only.null.eins )
                  if(length(n.rasch) >0 )   {sunk(paste("genConquestDataset_",ver,": Warning: ",length(n.rasch)," variable(s) are not strictly dichotomous with 0/1.\n",sep=""))
                                             for (ii in n.rasch)
                                                 {max.nchar <-  max(nchar(names(table(daten[,ii]))))
                                                  max.nchar.stacked <- c(max.nchar)
                                                  if(max.nchar>1) {sunk(paste("genConquestDataset_",ver,": Arity of variable ",colnames(daten)[ii]," exceeds 1.\n"))}
                                                  if(verbose == TRUE) {sunk(paste(colnames(daten)[ii],": ", paste( names(table(daten[,ii])),collapse=", "),"\n",sep=""))}}
                                             sunk("By default, all values except for 0 and 1 are treated as sysmis.\n")
                                             if(model.statement == "item")
                                               {sunk("WARNING: Sure you want to use 'model statement = item' even when items are not dichotomous?\n")} }
                  
                  ### identifiziere Faelle mit ausschliesslich missings
                 all.values   <- table(unique(unlist(lapply(1:ncol(daten), FUN=function(ii) {names(table(daten[,ii]))}))))
                  if(length(all.values)!=2) {sunk(paste("genConquestDataset_",ver,": Warning: Found more than two non missing codes in overall testitems. Data does not seem to fit to the Rasch model.\n",sep=""))}
                  if(length(all.values)==2) {if(!all(names(all.values) == c("0","1"))) {sunk("Warning: Found codes departing from 0 and 1 in testitems. Data does not seem to fit to the Rasch model.\n")}}
                  weg.variablen <- rowSums(is.na(daten))                        ### identifiziere Fälle mit ausschließlich missings
                  weg.variablen <- which(weg.variablen == ncol(daten))
                  if(length(weg.variablen)>0) 
                    {sunk(paste("genConquestDataset_",ver,": Found ",length(weg.variablen)," cases with missings on all items.\n",sep=""))
                     if( remove.no.answers == TRUE)  {sunk("    Cases with missings on all items will be deleted.\n")}
                     if( remove.no.answers == FALSE) {weg.variablen <- NULL     ### WICHTIG: Wenn missings on all items beibehalten werden sollen, muß weg.variablen wieder zurückgesetzt werden!
                                                      sunk("Cases with missings on all items will be kept.\n")}}
                  hg.char <- NULL; DIF.char <- NULL; weight.char <- NULL; all.hg.char <- NULL        ### obere Zeile: wieviele Character haben die Variablen?
                  weg.dif <- NULL; weg.hg <- NULL; weg.weight <- NULL; namen.all.hg <- NULL
                  
				    if(!is.null(HG.var))                                          
                    {if(!is.null(na$HG))                                        ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die HG-Variablen recodiert
                       {rec.hg <- paste(na$HG,"=NA",collapse="; ")              ### definiere recodierungsvorschrift
                        for (i in 1:ncol(dat[,namen.hg.var,drop=F]))
                            {dat[,namen.hg.var[i]] <- recode(dat[,namen.hg.var[i]], rec.hg)}}## untere Zeile: wieviele "character" haben Hintergrundvariablen?
                     mis     <- sapply(1:length(namen.hg.var), FUN=function(ii) {length(table(dat[,namen.hg.var[ii]]))})
                     mis.1   <- which(mis == 0)
                     if(length(mis.1)>0) {stop(paste("genConquestDataset_",ver,": At least one HG-variable without any values.",sep=""))}
                     mis.2   <- which(mis == 1)
                     if(length(mis.1)>0) {sunk(paste("genConquestDataset_",ver,": Warning: At least one HG-variable is a constant.\n"))}
                     hg.char <- sapply(1:length(namen.hg.var), FUN=function(ii) {max(nchar(as.character(na.omit(dat[,namen.hg.var[ii]]))))})
                  ## hg.char[hg.char>12] <- 12                                  ### begrenze Characterzahl nach oben
                     num     <- unlist(lapply(1:length(namen.hg.var),FUN=function(ii) {length( unique(c(grep("[[:digit:]]",dat[,namen.hg.var[ii]]),which(is.na(dat[,namen.hg.var[ii]])))))}))
                     if(!all(num==nrow(dat)))                                   ### HG-Variablen müssen numerisch sein, das will conquest so. Für diese Prüfung sind also NA und numerische Werte erlaubt
                       {sunk(paste("genConquestDataset_",ver,": Warning: Found ",length(which(num!=nrow(dat)))," HG variable(s) with non missing non-numeric values. Conquest will collapse.\n",sep=""))
                        if(verbose==TRUE) {sunk(paste( namen.hg.var[which(num!=nrow(dat))],collapse=", ")); sunk("\n")}}
                     mis.spec <- unlist(lapply(1:length(namen.hg.var), FUN=function(ii) { sum(is.na(dat[,namen.hg.var[ii]]))}))
                     if(!all(mis.spec==0))                                      ### auf welchen HG-Variablen gibt es wieviele missings?
                       {sunk(paste("genConquestDataset_",ver,": Warning: Found ",length(which(mis.spec!=0))," HG variable(s) with missing value(s). Conquest probably will collapse.\n",sep=""))
                        if(verbose==TRUE) {sunk(paste( namen.hg.var[which(mis.spec!=0)],collapse=", ")); sunk("\n") } }
                     dat.hg  <- data.frame(dat[,namen.hg.var,drop=F],stringsAsFactors=F)
                     weg.hg  <- attr(na.omit(dat.hg), "na.action")
                     # if(length(weg.hg)>0)
                     #  {cat(paste("Remove ",length(weg.hg)," cases with missings on HG variables.\n",sep=""))}
                    }
					   
                  if(!is.null(group.var))  {                                       
                     if(!is.null(na$group))                                     ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die HG-Variablen recodiert
                       {rec.group <- paste(na$group,"=NA",collapse="; ")        ### definiere recodierungsvorschrift
                        for (i in 1:ncol(dat[,namen.group.var,drop=F]))
                            {dat[,namen.group.var[i]] <- recode(dat[,namen.group.var[i]], rec.group)}}
                     ### missings auf Gruppenvariablen?
                     mis.group <- unlist( lapply(1:length(namen.group.var), FUN=function(ii){sum(is.na(dat[,namen.group.var[ii]]))}))
                     if(!all(mis.group==0))
                       {sunk(paste("genConquestDataset_",ver,": Warning: Found ",length(which(mis.group!=0))," group variable(s) with missing value(s). Conquest probably will collapse.\n",sep=""))
                        sunk(paste( namen.group.var[which(mis.group!=0)],collapse=", ")); sunk("\n") }
                    }
                  
                   if(!is.null(DIF.var))  {
                     if(length(DIF.var)!=1) {stop("Use only one DIF-variable.")}
                     if(!is.null(na$DIF))                                       ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die DIF-Variablen recodiert
                       {rec.dif <- paste(na$DIF,"=NA",collapse="; ")            ### definiere recodierungsvorschrift
                        dat[,namen.dif.var] <- recode(dat[,namen.dif.var], rec.dif)}
                     if(length(table(dat[,namen.dif.var])) == 0) {stop("No valid values in DIF variable.")}
                     DIF.char <- max(nchar(as.character(na.omit(dat[,namen.dif.var]))))
                     if(length(table(dat[,namen.dif.var]))!=2) {sunk("Serious problem: DIF-variable does not seem to be dichotomous.\n")}
                     weg.dif <- which(is.na(dat[,namen.dif.var]))
                       if(length(weg.dif)>0)                                      ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                       {sunk(paste("genConquestDataset_",ver,": Found ",length(weg.dif)," cases with missings on DIF variable.\n",sep=""))}
					 ### geprüft werden Testitems: keine Werte? konstant? aber diesmal für DIF-Gruppen getrennt
                     n.werte <- lapply(daten, FUN=function(ii){by(ii, INDICES=list(dat[,namen.dif.var]), FUN=table)})
                     completeMissingGroupwise <- data.frame(t(sapply(n.werte, function(ll){lapply(ll, length)})), stringsAsFactors = FALSE)
                     for (i in seq(along=completeMissingGroupwise)) {
                          missingCat.i <- which(completeMissingGroupwise[,i] == 0)
                          if(length(missingCat.i) > 0) {
                             sunk(paste("genConquestDataset_",ver,": Warning: Following items with no values in DIF group ",i,": \n",sep=""))
                             sunk(paste(rownames(completeMissingGroupwise)[missingCat.i],collapse=", ")); sunk("\n")
                          }
                          constantCat.i <- which(completeMissingGroupwise[,i] == 1)
                          if(length(constantCat.i) > 0) {
                             sunk(paste("genConquestDataset_",ver,": Warning: Following items are constants in DIF group ",i,":\n",sep=""))
                             sunk(paste(rownames(completeMissingGroupwise)[constantCat.i],collapse=", ")); sunk("\n")
                          }
                     }
                   }
                  if(!is.null(weight.var)) {
                     if(length(weight.var)!=1) {stop("Use only one weight variable.")}
                     if(!is.null(na$weight))                                    ### bevor irgendwas anderes geschieht, werden, sofern spezifiziert, die DIF-Variablen recodiert
                       {rec.weight <- paste(na$weight,"=NA",collapse="; ")      ### definiere recodierungsvorschrift
                        dat[,namen.weight.var] <- recode(dat[,namen.weight.var], rec.weight)}
                     if(length(table(dat[,namen.weight.var])) == 0) {stop("No valid values in weight variable.")}
                     weight.char <- max(nchar(as.character(na.omit(dat[,namen.weight.var]))))
                     weg.weight <- which(is.na(dat[,namen.weight.var]))
                       if(length(weg.weight)>0)                                   ### untere Zeile: dies geschieht erst etwas spaeter, wenn datensatz zusammengebaut ist
                       {sunk(paste("genConquestDataset_",ver,": Found ",length(weg.weight)," cases with missings on weight variable.\n",sep=""))}
				   }
                  namen.all.hg <- unique(c(namen.dif.var,namen.hg.var,namen.group.var,namen.weight.var))## Achtung: group- und DIF- bzw. group- und HG-Variablen duerfen sich ueberschneiden!
                  if(!is.null(namen.all.hg)) {all.hg.char <- sapply(1:length(namen.all.hg), FUN=function(ii) {max(nchar(as.character(na.omit(dat[,namen.all.hg[ii]]))))})}
                  var.char <- sapply(1:ncol(daten), FUN=function(ii) {max(nchar(as.character(na.omit(daten[,ii]))))})
                  no.number <- setdiff(1:length(var.char), grep("[[:digit:]]",var.char))
                  if(length(no.number)>0) {var.char[no.number] <- 1}            ### -Inf steht dort, wo nur missings sind, hier soll die Characterbreite auf 1 gesetzt sein
                  if(use.letters == TRUE)                                       ### sollen Buchstaben statt Ziffern beutzt werden? Dann erfolgt hier Recodierung.
                    {rec.statement <- paste(0:25,"='",LETTERS,"'",sep="",collapse="; ")
                     for (i in 1:ncol(daten))                                   ### Warum erst hier? Weil Prüfungen (auf Dichotomität etc. vorher stattfinden sollen)
                         {daten[,i] <- recode(daten[,i], rec.statement)}
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
				  
				  
				  
				  
.existsBackgroundVariables <- function(dat, variable )  {
                             if(!is.null(variable))  {
            								 if(is.character(variable))  {
            									 misVariable <- setdiff(variable, colnames(dat))
            									 if(length(misVariable)>0) {sunk(paste("Can't find ",length(misVariable)," variables in dataset.\n",sep=""))
            									 sunk(paste(misVariable,collapse=", ")); sunk("\n"); stop()}
            									 varColumn <- match(variable, colnames(dat))
            								 }
            								 if(is.numeric(variable))   {varColumn <- variable}
                                             return(colnames(dat)[varColumn])
            							 }
                             if(is.null(variable)) {return(NULL)}
                            }  
                             							 
							 
.substituteSigns <- function(dat, variable ) {
                    if(!is.null(variable)) {
           					   variableNew <- gsub("_|\\.|-", "", variable)
                       cols        <- match(variable, colnames(dat)) 
           					   return(list(cols=cols, old=variable,new=variableNew))
           					}
                    if(is.null(variable)) {return(list(old=TRUE,new=TRUE))}
                    }		
