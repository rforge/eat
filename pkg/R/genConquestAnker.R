####################################################################################################################
#
# genConquestAnker
# erzeugt Files mit Quellparametern zur Verankerung in Conquest
#
#
# Version: 	0.4.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
# 25.11.2011 SW: 'cat' durch 'sunk' ersetzt
# 16.11.2011 SW: Konvention geaendert: Funktion erwartet als prm.file nun IMMER einen data.frame
#                mit genau zwei Spalten; 1. Itemnamen, 2. Parameter
# 14.10.2011 MH: gestabled
# 27.09.2011 SW: Faktorspalten in prm file werden in Characterspalten umkonvertiert 
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 27.06.2011 SW: Gebe Funktionsmane und Versionsnummer vor jeder Nachricht
#
####################################################################################################################

### daten                ... aggregierter Itemdatensatz, wird benötigt, um Itemnamen auszulesen
### itemspalten          ... wo stehen Testitems im Datensatz?
### prm.file             ... Tabelle mit Quellparametern, als R-Dataframe

genConquestAnker <- function(daten ,itemspalten, prm.file) {
                    ver <- "0.4.0"
                    lab <- data.frame(1:ncol(daten[,itemspalten]), colnames(daten[,itemspalten]) , stringsAsFactors=F)
                    colnames(lab) <- c("===>","item")
                    prm <- prm.file
                    if(ncol(prm.file)!=2) {stop(paste("genConquestAnker_",ver,": Unexpected column numbers in anchor parameter file.\n    (Expect 2 columns, found ",ncol(prm.file),".\n",sep=""))}
                    ind.character <- which( sapply(1:ncol(prm),FUN=function(ii){is.character(prm[1,ii])}) )
                    ind.numeric   <- which( sapply(1:ncol(prm),FUN=function(ii){is.numeric(prm[1,ii])}) )
                    error.1       <- length(ind.character) != 1 | length(ind.numeric) != 1
                    error.2       <- !all(ind.character == 1)
                    error.3       <- !all(ind.numeric == 2)
                    if(error.1 | error.2 | error.3) {
                       sunk(paste("genConquestAnker_",ver,": Unexpected column format in anchor parameter file.\n",sep=""))
                       sunk("    Treat first column as item names, second as anchor parameters. If not intended, please correct anchor parameter file.\n")
                       prm[,1] <- as.character(prm[,1])
                       if (is.character(prm[1,2])) {
                           prm[,2] <- as.numeric(prm[,2])
                       }
                       if (is.factor(prm[1,2])) {
                           prm[,2] <- as.numeric(as.character(prm[,2]))
                       }
                    }
                    colnames(prm) <- c("item","parameter")
                    ind <- intersect(lab$item,prm$item)
                    ind <- wo.sind(ind,prm$item,quiet=T)
                    prm <- prm[ind,]
                    ind <- wo.sind(prm$item,lab$item,quiet=T)
                    res <- data.frame(ind,prm$parameter,stringsAsFactors=F)
                    save(prm,file="info.tmp")                                     ### hier wird eine Informationsdatei geschrieben, die später einzig dazu da ist, den Anker-Check zu machen
                    ## if(!missing(opt.name))                          {write.table(res,opt.name,sep=" ",col.names=F,row.names=F,quote=F)}
                    return(res)}


### Anmerkungen:
### Anders als ursprünglich, gewinnt die Funktion die Itemnamen nicht aus dem Labelfile (das existiert zu dem Zeitpunkt ja
### noch nicht, da genConquestAnker() VOR genConquestsynLab() aufgerufen werden muß. itemnamen werden folglich aus dem Datensatz
### selbst ausgelesen. Funktion sucht übereinstimmende namen aus Quellparameterdatei und colnames(daten)
