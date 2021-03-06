####################################################################################################################
#
# genConquestAnker
# erzeugt Files mit Quellparametern zur Verankerung in Conquest
#
#
# Version: 	0.5.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
# 25.11.2011 SW: 'cat' durch 'eatTools:::sunk' ersetzt
# 16.11.2011 SW: Konvention geaendert: Funktion erwartet als prm.file nun IMMER einen data.frame
#                mit genau zwei Spalten; 1. Itemnamen, 2. Parameter
# 14.10.2011 MH: gestabled
# 27.09.2011 SW: Faktorspalten in prm file werden in Characterspalten umkonvertiert 
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 27.06.2011 SW: Gebe Funktionsmane und Versionsnummer vor jeder Nachricht
#
####################################################################################################################

### daten                ... aggregierter Itemdatensatz, wird ben�tigt, um Itemnamen auszulesen
### itemspalten          ... wo stehen Testitems im Datensatz?
### prm.file             ... Tabelle mit Quellparametern, als R-Dataframe

genConquestAnker <- function(daten ,itemspalten, prm.file, verbose = TRUE) {
                    ver <- "0.5.0"
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
                       eatTools:::sunk(paste("genConquestAnker_",ver,": Unexpected column format in anchor parameter file.\n",sep=""))
                       eatTools:::sunk("    Treat first column as item names, second as anchor parameters. If not intended, please correct anchor parameter file.\n")
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
					if ( verbose == TRUE )  {
					   eatTools:::sunk(paste("genConquestAnker_",ver,": Found ",nrow(lab), " items in data set.\n",sep=""))
					   eatTools:::sunk(paste("    Found ",nrow(prm), " items in anchor set.\n",sep=""))
  					   if(length(ind) == 0) {eatTools:::sunk("Error: No common items found in 'lab.file' and 'prm.file'.\n"); stop()}
					   if(length(ind) >  0) 
					     {eatTools:::sunk(paste("genConquestAnker_",ver,": Found ",length(ind), " common anchor items.\n",sep=""))
						  if (length(ind) < nrow(lab) )  {
						     missingInAnchors <- setdiff(lab$item, prm$item)
						     eatTools:::sunk(paste("Following ",length(missingInAnchors)," items in data set without anchor parameters:\n",sep=""))
							 eatTools:::sunk(paste(paste(missingInAnchors,collapse=", "),"\n",sep=""))
						  }	
						  if (length(ind) < nrow(prm) )    {
						     missingInData <- setdiff(prm$item, lab$item)
						     eatTools:::sunk(paste("Following ",length(missingInData)," items in anchor set were not found in data set:\n",sep=""))
							 eatTools:::sunk(paste(paste(missingInData,collapse=", "),"\n",sep=""))
						  }
					    }
					}		
					ind <- wo.sind(ind,prm$item,quiet=T)
                    prm <- prm[ind,]
                    ind <- wo.sind(prm$item,lab$item,quiet=T)
                    res <- data.frame(ind,prm$parameter,stringsAsFactors=F)
                    save(prm,file="info.tmp")                                     ### hier wird eine Informationsdatei geschrieben, die sp�ter einzig dazu da ist, den Anker-Check zu machen
                    ## if(!missing(opt.name))                          {write.table(res,opt.name,sep=" ",col.names=F,row.names=F,quote=F)}
                    return(res)}


### Anmerkungen:
### Anders als urspr�nglich, gewinnt die Funktion die Itemnamen nicht aus dem Labelfile (das existiert zu dem Zeitpunkt ja
### noch nicht, da genConquestAnker() VOR genConquestsynLab() aufgerufen werden mu�. itemnamen werden folglich aus dem Datensatz
### selbst ausgelesen. Funktion sucht �bereinstimmende namen aus Quellparameterdatei und colnames(daten)
