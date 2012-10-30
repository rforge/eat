####################################################################################################################
#
# writeConquestToDisk
# schreibt von genConquestDataset und genConquestSynLab erzeugtes Inputmaterial auf die Festplatte
#
#
# Version: 	0.1.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 27.05.2011 (SW): Speichern des batch-Aufrufs
# 27.05.2011 (SW): Fehlerabfrage
# 16.06.2011 (SW): relative Pfade. Ausserdem werden Unterverzeichnisse, wenn sie nicht existieren, erzeugt (mit Warnung).
#                  ausserdem werden, wenn relative Pfade fuer den Output angegeben wurden, diese aber nicht existieren, 
#                  hier erzeugt (mit Warnung)
# 27.06.2011 (SW): Gebe Funktionsmane und Versionsnummer vor jeder Nachricht
#
####################################################################################################################

### conquestDataset      ... Conquest-datensatz, wie ihn genConquestDataset erzeugt
### conquestDatasetWidth ... Spaltenbreiten des zu erzeugenden ASCII-datensatzes (wird von genConquestDataset uebergeben)
### pathConquestDataset  ... wo soll der Conquest-Datensatz abgelegt werden?
### nameConquestDataset  ... Dateiname des Datensatzes
### ConquestSyntax       ... Syntax, wie sie von genConquestSynLab erzeugt wird
### ConquestLab          ... Labels, wie sie von genConquestsynLab erzeugt werden
### conquestAnker        ... optional: Generiertes Ankerparameterfile, wie es genConquestAnker erzeugt
### jobFolder            ... Verzeichnis fuer die Analysen
### subFolder            ... optional: Liste mit Unterverzeichnissen
### name.analyse         ... Dateiname fuer Conquest-Input (nur Praefix, Suffixe werden automatisch vergeben)

writeConquestToDisk <- function(conquestDataset , conquestDatasetWidth, nameConquestDataset, conquestSyntax , conquestLabels, conquestBatchString, conquestAnker=NULL, jobFolder, subFolder=NULL, name.analyse )
                       {ver <- "0.1.0"
                        ret <- TRUE
                        
                        ### schreibe Datensatz
                        if(!is.null(subFolder$data))                            ### subFolder gesetzt?
                          {ziel.pfad <- file.path(jobFolder, subFolder$data)}
                        if(is.null(subFolder$data))
                          {ziel.pfad <- jobFolder}
                    
                        if(!file.exists(ziel.pfad))
                          {cat(paste("writeConquestToDisk_",ver,": Zielverzeichnis fuer Datensatz ",ziel.pfad," nicht gefunden. Erzeuge Verzeichnis.\n",sep=""))
                           dir.create(ziel.pfad) }
                        if(inherits(try(  write.fwf(conquestDataset , file.path(ziel.pfad,nameConquestDataset), colnames=F,rownames=F,sep="",quote=F,na=".", width=conquestDatasetWidth)  ),"try-error"))
                          { ret <- FALSE; cat(paste("writeConquestToDisk_",ver,": Fehler beim Speichern des Conquest-Datensatzes.\n",sep=""))}
                        
                        test <- readLines(file.path(ziel.pfad,nameConquestDataset)) 
                        stopifnot(length(table(nchar(test)))==1)                ### Check: hat der Resultdatensatz eine einheitliche Spaltenanzahl? Muss unbedingt sein!

                        ### schreibe syntax
                        if(!file.exists(jobFolder))
                          {cat(paste("writeConquestToDisk_",ver,": Jobverzeichnis ",jobFolder," nicht gefunden. Erzeuge Verzeichnis.\n",sep=""))
                           dir.create(jobFolder) }
                        if(inherits(try(  write(conquestSyntax, file.path(jobFolder,paste(name.analyse,".cqc",sep="") ) ,sep="\n") ),"try-error"))
                          { ret <- FALSE; cat(paste("writeConquestToDisk_",ver,": Fehler beim Speichern der Syntax.\n",sep=""))}
                        
                        ### schreibe Labels
                        if(inherits(try( write.table(conquestLabels, file.path(jobFolder,paste(name.analyse,".lab",sep="")), col.names=T,row.names=F,dec=",",sep=" ",quote=F) ),"try-error"))
                          { ret <- FALSE; cat(paste("writeConquestToDisk_",ver,": Fehler beim Speichern der Labels.\n",sep=""))}
                        
                        ### schreibe Ankerparameter
                        if(!is.null(conquestAnker))  
                          {if(inherits(try( write.table(conquestAnker,file.path(jobFolder,paste(name.analyse,".ank",sep="")),sep=" ",col.names=F,row.names=F,quote=F) ),"try-error"))
                             { ret <- FALSE; cat(paste("writeConquestTo Disk_",ver,": Fehler beim Speichern der Ankerparameter.\n",sep=""))}  } 
                        
                        ### schreibe Batch-String
                        if(inherits(try( write(conquestBatchString, file.path(jobFolder,paste(name.analyse,".bat",sep="")) ) ),"try-error"))
                          { ret <- FALSE; cat(paste("writeConquestToDisk_",ver,": Fehler beim Speichern des Batch-Strings.\n",sep=""))}
                        
                        ### Falls sie nicht existieren: Lege Zielverzeichnisse fuer Output an!
                        if(!is.null(subFolder$out))
                          {if(!file.exists(file.path(jobFolder, subFolder$out)))
                             {cat(paste("writeConquestToDisk_",ver,": Unterverzeichnis fuer Output ",subFolder$out," nicht gefunden. Erzeuge Verzeichnis.\n",sep=""))
                              dir.create(file.path(jobFolder, subFolder$out)) }} 
                        
                        if (!ret) stop() else return(ret) }
                            
                        
                        
