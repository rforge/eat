####################################################################################################################
#
# genConquestSynLab
# erzeugt Conquest Syntax und Labels
#
# Version: 	0.19.0
# Imports:
# Published:
# Author:   Sebastian Weirich
# Maintainer:
#
# Change Log:
#
# 2011-12-12 SW
# CHANGED: table(unlist (...) ) replaced by table.unlist( ... ) in genConquestSynLab()
# 2011-12-05 SW
# CHANGED: 'id' replaced by 'pid'. logfile export in genConquestSynLab()
# 2011-11-29 SW
# FIXED: multidimensional naming in genConquestSynLab()
# 0000-00-00 AA
#
# 07.06.2011 (SW): Relative Pfade, descriptives, Labels enthalten ggf. auch Dimensionen 
#                  (diese werden aus Spaltennamen der Q-Matrix ausgelesen) 
# 16.06.2011 (SW): Relative Pfade Reloaded.
# 27.06.2011 (SW): Alle Meldungen geben Funktionsname und Versionsnummer
# 03.07.2011 (SW): score statement fuer Conquest Syntax gibt nun items (1-12,24-30) statt (1,2,3,4, ... )
# 14.07.2011 (SW): neues group statement. Ausserdem: HG-, DIF- und group-Variablen werden SIMULTAN ins
#                  Format-Statement geschrieben, da sie sich ueberschneiden duerfen
# 06.08.2011 (SW): Pruefung, ob Leistungsdaten dichotom sind, sollte nun schneller gehen und 
#                  den Speicher auch bei grossen datensaetzen nicht mehr ueberlasten
#                  Gewichtungsvariable
# 08.08.2011 (SW): nodes, deviancechange, iterations etc. variabel
# 08.08.2011 (MH): auf stable gesetzt wegen besserer sourcebarkeit
# 10.08.2011 (SW): gaussian quadrature is only available for models without regressors
# 16.08.2011 (SW): Kompatibitaet zu Conquest 2007
# 17.08.2011 (MH): auf stable gesetzt wegen besserer Sourcebarkeit
# 26.08.2011 (MH): auf stable gesetzt wegen besserer Sourcebarkeit
# 19.09.2011 (SW): Equivalence table ergaenzt (Conquest-Handbuch, S.166)
# 13.10.2011 (SW): auf stable gesetzt wegen besserer Sourcebarkeit
# 14.10.2011 (SW): "trim" durch "crop" ersetzt
# 14.10.2011 (MH): gestabled
# 20.10.2011 (MH): in Zeile 284	": Fehler beim Übertragen des Score-Statements!\n" geändert in:
#								   "Fehler beim Uebertragen des Score-Statements!\n"
# 15.11.2011 (MH): auf stable gesetzt
# 23.11.2011 (TS): "gaussian quadrature is only available for models without regressors" nur für method=="gauss"
# 25.11.2011 (SW): 'cat' durch 'sunk' ersetzt
# 28.11.2011 (SW): Namen der Dimensionen werden nun ins Labfile uebertragen
# 05.12.2011 (SW): 'id' durch 'pid' ersetzt; log-file exported
# 12.12.2011 (SW): table(unlist (...) ) replaced by table.unlist( ... )
# 23.02.2012 (SW/MH): Conquest History eingefuegt 
# 07.03.2012 (MH) INIT Parameter eingefügt
#
####################################################################################################################

### jobName       ... Name des Conquest-Laufs, sinnvollerweise ohne Suffix, z.B. "VERA_Lesen"
### datConquest   ... Datensatz, wie er von GenConquestDataset erzeugt wurde 
### namen.items   ... wird von genConquest dataset uebergeben!
### namen.hg.var  ... wird von genConquest dataset uebergeben!
### namen.dif.var ... wird von genConquest dataset uebergeben!
### model         ... optional: Q-Matrix zur Spezifizierung der Dimensionen, gegeben als R-dataframe
###                   (erste Spalte enthaelt Variablenbezeichner, die Spalten danach die Dimensionen)
### ANKER         ... optional: File mit Ankerparametern
### subFolder     ... Liste mit optionalen Suchpfaden
### pfad.dataset ... wo soll der Conquest-Datensatz abgelegt werden?
###                  (vollstaendige Pfadangabe noetig, z.B. "P:/ZKD/model")
### name.dataset ... vollstaendiger Dateiname des zu erstellenden Conquest-Datensatzes, z.B. "testdaten.dat"
### name.unidim  ... optional: Name der Dimension, wenn es nur eine gibt
### equivalence.table ... Gibt ggf. Tabelle mit Umrechnungen Rohwert-Normwert; moegliche Werte sind "wle" (default); "mle" oder NULL (keine Tabelle wird ausgegeben)
###                       (Conquest-handbuch, S.166)


### ACHTUNG! Theoretisch duerfen sich HG-, DIF- und Groupvariablen ueberschneiden! Damit sie im Datensatz jedoch nicht (mit
### falschen Variablennamen) doppelt aneinandergebunden werden, wird ein Vektor "namen.all.hg" erzeugt. Dieser ist relevant
### fuer das Format-Statement, denn hier ist nur entscheidend, welche expliziten Variablen zusaetzlich in den Daten auftreten,
### aber nicht, welche dieser Variablen DIF, welche HG ist, usw.
### Fuer die Statements "group", "regression" und "model" werden aber die bedeutungsspezifischen Hintergrundvariablendefinitionen
### "namen.dif.var", "namen.hg.var" und "namen.group.var" benutzt. Bitte die Konsistenz unbedingt pruefen!

genConquestSynLab <- function(jobName, datConquest, namen.items, namen.hg.var, namen.dif.var , DIF.char, namen.weight.var, weight.char, namen.all.hg,all.hg.char, namen.group.var=NULL, model = NULL, ANKER = NULL,std.err=c("quick","full","none"),name.unidim="dimension_1",
                              model.statement="item", distribution=c("normal","discrete"), jobFolder, subFolder=NULL, name.dataset=NULL, Title=NULL,constraints =c("cases","none","items"), method=c("gauss", "quadrature", "montecarlo"), n.plausible=5,n.iterations=1000,nodes=NULL, p.nodes=2000,f.nodes=2000,converge=0.0001,deviancechange=0.0001,
                              equivalence.table=c("wle","mle","NULL"),var.char,use.letters=use.letters, allowAllScoresEverywhere, pathConquest, import = list () )       {
                   ver           <- "0.19.0"
                   .mustersyntax <- c("title = ####hier.title.einfuegen####;",
                                      "export logfile >> ####hier.name.einfuegen####.log;",
                                      "datafile ####hier.Pfad.und.Dateiname.einfuegen####;",
                                      "Format pid ####hier.id.einfuegen####",
                                      "group",
                                      "codes ####hier.erlaubte.codes.einfuegen####;",
                                      "labels  << ####hier.name.einfuegen####.lab;",
                                      "import anchor_parameters << ####hier.name.einfuegen####.ank;",
                                      "/* import init_parameters << ####hier.init_parameters.einfuegen####; */",
                                      "/* import init_reg_coefficients << ####hier.init_reg_coefficients.einfuegen####; */",
                                      "/* import init_covariance << ####hier.init_covariance.einfuegen####; */",
                                      "caseweight",
                                      "set constraints=####hier.constraints.einfuegen####;",
                                      "set warnings=no,update=yes,n_plausible=####hier.anzahl.pv.einfuegen####,p_nodes=####hier.anzahl.p.nodes.einfuegen####,f_nodes=####hier.anzahl.f.nodes.einfuegen####;",
                                      "regression",
                                      "model ####hier.model.statement.einfuegen####;",
                                      "estimate ! method=####hier.method.einfuegen####,iter=####hier.anzahl.iterations.einfuegen####,nodes=####hier.anzahl.nodes.einfuegen####,converge=####hier.converge.einfuegen####,deviancechange=####hier.deviancechange.einfuegen####,stderr=####hier.std.err.einfuegen####,distribution=####hier.distribution.einfuegen####;",
                                      "Itanal >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####.itn;",
                                      "show cases! estimates=latent >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####.pvl;",
                                      "show cases! estimate=wle >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####.wle;",
                                      "equivalence ####hier.equivalence.table.einfuegen#### >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####.equ;",
                                      "show >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####.shw;",
									  "export history >> ####hier.name.einfuegen####.his;",
									  "export par    >> ####hier.name.einfuegen####.prm;",
									  "export covariance >> ####hier.name.einfuegen####.cov;",
									  "export reg_coefficients >> ####hier.name.einfuegen####.reg;",
									  "descriptives !estimates=pv >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####_pvl.dsc;",
                                      "descriptives !estimates=wle >> ####hier.outfolder.einfuegen####\\####hier.name.einfuegen####_wle.dsc;",
                                      "quit;")
                   ### Conquest akzeptiert explizite Variablennamen nur in Kleinschreibung!
                   if(!all(namen.hg.var == tolower(namen.hg.var)))
                     {sunk(paste("genConquestSynLab_",ver,": Warning: Conquest allows only lower case letters for explicit variables. Print HG variables in lower cases.\n",sep=""))}
                   if(!all(namen.dif.var == tolower(namen.dif.var)))
                     {sunk(paste("genConquestSynLab_",ver,": Warning: Conquest allows only lower case letters for explicit variables. Print DIF variables in lower cases.\n",sep=""))}
                   if(!all(namen.weight.var == tolower(namen.weight.var)))
                     {sunk(paste("genConquestSynLab_",ver,": Warning: Conquest allows only lower case letters for explicit variables. Print weighting variables in lower cases.\n",sep=""))}
                   if(!all(namen.group.var == tolower(namen.group.var)))
                     {sunk(paste("genConquestSynLab_",ver,": Warning: Conquest allows only lower case letters for explicit variables. Print grouping variables in lower cases.\n",sep=""))}
                   ### wenn kein Title gesetzt, erstelle ihn aus Sys.getenv
                   converge <- as.character(converge+1)
                   converge <- paste("0",substring(converge,2),sep="")
                   deviancechange <- as.character(deviancechange+1)
                   deviancechange <- paste("0",substring(deviancechange,2),sep="")
                   if(is.null(Title)) 
                     {all.inf  <- Sys.getenv()
                      Title    <- paste("Analysis name: ",jobName, ", User: ",all.inf["USERNAME"],", Computername: ",all.inf["COMPUTERNAME"],", ",R.version$version.string,", Time: ",date(),sep="")}
                   syntax    <- gsub("####hier.title.einfuegen####",Title,.mustersyntax)
                   syntax    <- gsub("####hier.anzahl.pv.einfuegen####",n.plausible,syntax)
                   syntax    <- gsub("####hier.anzahl.iterations.einfuegen####",n.iterations,syntax)
                   syntax    <- gsub("####hier.anzahl.p.nodes.einfuegen####",p.nodes,syntax)
                   syntax    <- gsub("####hier.anzahl.f.nodes.einfuegen####",f.nodes,syntax)
                   syntax    <- gsub("####hier.converge.einfuegen####",converge,syntax)
                   syntax    <- gsub("####hier.deviancechange.einfuegen####",deviancechange,syntax)
                   syntax    <- gsub("####hier.constraints.einfuegen####",match.arg(constraints),syntax)
                   method    <- match.arg(method)
				   if(method == "montecarlo")   {
				     if (is.null(nodes) )   {
					    sunk(paste("genConquestSynLab_",ver,": '",method,"' has been chosen for estimation method. Number of nodes was not explicitly specified. Set nodes to 1000.\n",sep=""))
					    nodes <- 1000
				     }
					 if(nodes < 100 ) {
					    sunk(paste("genConquestSynLab_",ver,": Warning: Due to user specification, only ",nodes," nodes are used for '",method,"' estimation. Please note or re-specify your analysis.\n",sep=""))
					 }
					} 
				   if(method != "montecarlo" & is.null(nodes) )   {
					  sunk(paste("genConquestSynLab_",ver,": Number of nodes was not explicitly specified. Set nodes to 15 for method '",method,"'.\n",sep=""))
					  nodes <- 15
				    }
				   syntax    <- gsub("####hier.anzahl.nodes.einfuegen####",nodes,syntax)
                   syntax    <- gsub("####hier.std.err.einfuegen####",match.arg(std.err),syntax)
                   syntax    <- gsub("####hier.distribution.einfuegen####",match.arg(distribution),syntax)
                   syntax    <- gsub("####hier.equivalence.table.einfuegen####",match.arg(equivalence.table),syntax)
				   syntax    <- gsub("####hier.model.statement.einfuegen####",model.statement,syntax)

				   # INIT Parameter
				   syntax    <- gsub("####hier.init_parameters.einfuegen####",paste(jobName,"_INIT.prm",sep=""),syntax)
				   syntax    <- gsub("####hier.init_reg_coefficients.einfuegen####",paste(jobName,"_INIT.reg",sep=""),syntax)
				   syntax    <- gsub("####hier.init_covariance.einfuegen####",paste(jobName,"_INIT.cov",sep=""),syntax)
				   
				   # if(!is.null(import$init_parameters))  {
				      # syntax  <- gsub("####hier.init_parameters.einfuegen####",normalize.path(import$init_parameters))
				   # }	 else {
				      # ind.1 <- grep("import init_parameters",syntax)
                      # stopifnot(length(ind.1) == 1)
 				      # syntax <- syntax[-ind.1]
				   # }  
				   # if(!is.null(import$init_reg_coefficients))  {
				      # syntax  <- gsub("####hier.init_reg_coefficients.einfuegen####",normalize.path(import$init_reg_coefficients))
				   # }	 else {
				      # ind.2 <- grep("import init_reg_coefficients",syntax)
                      # stopifnot(length(ind.2) == 1)
 				      # syntax <- syntax[-ind.2]
				   # }
				   # if(!is.null(import$init_covariance))  {
				      # syntax  <- gsub("####hier.init_covariance.einfuegen####",normalize.path(import$init_covariance))
				   # }	 else {
				      # ind.3 <- grep("import init_covariance",syntax)
                      # stopifnot(length(ind.3) == 1)
 				      # syntax <- syntax[-ind.3]
				   # }
				   if(!is.null(subFolder$out)) 
                   ### entferne ggf. abschliessende Schraegstriche: erledigt nun automateConquestModels
                     {### for (ii in 1:nchar(subFolder$out)) {subFolder$out <- gsub("/$","",subFolder$out)} 
                      syntax <- gsub("####hier.outfolder.einfuegen####",normalize.path(subFolder$out),syntax,fixed=T) }
                   ### untere Zeile: kein relativer Pfad angegeben: behalte jobFolder bei!
                   if(is.null(subFolder$out))  { syntax <- gsub("####hier.outfolder.einfuegen####\\","",syntax,fixed=T)}
                   if(class(datConquest) == "data.frame") {daten <- datConquest}
                   ## if(class(datConquest) == "list")       {daten <- datConquest$daten.dat}
                   
                   ### identifiziere ID
                   ### types <- sapply(1:length(zkdDatasetAgg$varinfo),FUN=function(ii) {zkdDatasetAgg$varinfo[[ii]]$type})
                   id.spalte <- 1                                               ### per Definition aus GenConquestDataset!
                   ### es kann nur eine "ID" geben!
                   ### if(length(id.spalte) > 1)  {stop("More than one ID variable found.")}
                   ### if(length(id.spalte) == 0) {stop("No ID variable found.")}
                   ### if(id.spalte != 1)         {stop("ID variable has to occur in first column of dataset.")}

                   ### identifiziere Itemspalten ("TI" = test items)
                   itemspalten <- sort ( match(namen.items, colnames(daten)) )  ### in welchen spalten stehen Items? 
                   erlaubte.codes <- paste(gsub("_","",sort(gsub(" ","_",formatC(names(table.unlist(daten[,itemspalten])),width=var.char)),decreasing=TRUE)),collapse=",")
                   syntax    <- gsub("####hier.erlaubte.codes.einfuegen####",erlaubte.codes, syntax )
                   namen.items <- colnames(daten)[itemspalten]                  ### folgendes wenn Reihenfolge geaendert wurde (sollte nicht sein)
                   sunk(paste("genConquestSynLab_",ver,": ",length(itemspalten), " test items identified.\n",sep=""))
                   lab <- data.frame(1:length(itemspalten), namen.items , stringsAsFactors=F)
                   colnames(lab) <- c("===>","item")
                   for (i in 1:ncol(lab)) {lab[,i] <- as.character(lab[,i])}

                   ### sind Leistungsdaten dichotom? (Geht schneller als alte Variante: auskommentiert) 
                   testdaten <- daten[,itemspalten,drop=FALSE]
                   poo <- unique(names(unlist(lapply(testdaten, FUN=function(ii) {table(ii)}))))
                   if(length(poo) !=2 ) {sunk(paste("genConquestSynLab_",ver,": Warning: data does not seem to be dichotomous.\n",sep=""))}
                   # if(length(table(unlist(daten[,itemspalten]))) !=2 ) {cat("genConquestSynLab_",ver,": Warning: data does not seem to be dichotomous.\n")}
                   
                   ### wie werden HG-Variablen spezifiziert? ("CV" = context variables)
                   HG.var <- match(namen.hg.var, colnames(daten))
                   sunk(paste("genConquestSynLab_",ver,": ",length(HG.var), " context variables identified.\n",sep=""))
                   
                   DIF.var <- match(namen.dif.var, colnames(daten))                   
                   ### wenn es DIF-variablen gibt, werden die aus dem regression-Statement herausgenommen, bleiben aber fuer das 
                   ### Format-Statement erhalten. Deshalb wird nun, falls noetig, eine Indikatorvariable definiert, die ggf. DIF-
                   ### Variablen aus dem Regression-Statement entfernt
                   # hg.weg <- NULL                                               ### wenn if.null(hg.weg), bleiben alle variablen im Regression-Statement 
                   # if(!is.null(DIF.var))
                   #   {if (!DIF.var %in% colnames(daten)) {stop("Can't find DIF variable in dataset.")}
                   #    cat(paste("Treat ",DIF.var," as DIF variable. Remove ",DIF.var," from latent regression model.\n",sep=""))
                   #    dif.item  <- match(DIF.var, colnames(daten) )
                   #    hg.weg <- which(HG.var == dif.item)}
                   # if(length(HG.var)>0)                                         ### untere Zeile: wieviele "character" haben Hintergrundvariablen?
                   #  {hg.char <- sapply(1:length(HG.var), FUN=function(ii) {max(nchar(as.character(na.omit(daten[,HG.var[ii]]))))})   }
                   syntax    <- gsub("####hier.name.einfuegen####",jobName,syntax)
                   ID.char   <- max(as.numeric(names(table(nchar(as.character(daten[,id.spalte]))))))
                   syntax    <- gsub("####hier.id.einfuegen####",paste("1-",as.character(ID.char)," ",sep="" ) ,syntax)
                   ind       <- grep("Format pid",syntax)
                   if(is.null(name.dataset)) {name.dataset <- paste(jobName,".dat",sep="")}
                   all.hg.char.kontroll <- all.hg.char                          ### Hier: eintragen der Variablennamen fuer explizite Variablen in Format-Statement
                   beginn    <- NULL
                   if(length(namen.all.hg)>0)                                   ### untere Zeile: wieviele "character" haben Hintergrundvariablen?
                    {all.hg.char.kontroll <- all.hg.char
                     all.hg.char <- sapply(namen.all.hg, FUN=function(ii) {max(nchar(as.character(na.omit(daten[,ii]))))})
                     if(!all(all.hg.char == all.hg.char.kontroll)) {stop(paste("genConquestSynLab_",ver,": Error: Unconsistent column definition for HG variables.\n",sep="")) }
                     ### Trage nun die Spalten in das Format-Statement ein: Fuer ALLE expliziten Variablen
                     for (ii in 1:length(namen.all.hg))
                         {if(is.null(beginn)) {beginn <- ID.char+1}
                          ende   <- beginn-1+all.hg.char[ii]
                          if (beginn != ende) {syntax[ind] <- paste(syntax[ind],tolower(namen.all.hg[ii]), " ", beginn,"-",ende," ",sep="")}
                          if (beginn == ende) {syntax[ind] <- paste(syntax[ind],tolower(namen.all.hg[ii]), " ", beginn," ",sep="")}
                          beginn  <- ende+1
                         }
					}
                   if(!is.null(subFolder$data))
                   ### entferne ggf abschliessende Schraegstriche. erledigt nun automateConquestModel   
                     { #for (ii in 1:nchar(subFolder$data)) {subFolder$data <- gsub("/$","",subFolder$data)} 
                      pfad.dataset <- file.path(subFolder$data,name.dataset)}
                   if(is.null(subFolder$data))  {pfad.dataset <- name.dataset}
                   conq.data.pfad <- gsub("//","/",pfad.dataset)
                   conq.data.pfad <- gsub("/","//",conq.data.pfad)
                   conq.data.pfad <- normalize.path(conq.data.pfad)
                   # WICHTIG: Unten muss unbedingt "fixed=T" stehen!
                   syntax    <- gsub("####hier.Pfad.und.Dateiname.einfuegen####", conq.data.pfad ,syntax,fixed=T)
				   if(length(DIF.var)>0)  {                                        
                     if(model.statement != "item") {
                        sunk(paste("genConquestSynLab_",ver," Caution! DIF variable was specified. Expect model statement to be: 'item - ",paste("model item - ",paste(tolower(namen.dif.var),collapse=" - ") ," + ", paste("item*",tolower(namen.dif.var),collapse=" + "), ";",sep=""),"'.\n",sep=""))
                        sunk(paste("    However, '",model.statement,"' will uses as 'model statement'.\n",sep=""))
                      }
                     if(model.statement == "item") {
                        ind.model <- grep("model item", syntax)                   ### Ändere model statement
                        stopifnot(length(ind.model)==1)
                        syntax[ind.model] <- paste("model item - ",paste(tolower(namen.dif.var),collapse=" - ") ," + ", paste("item*",tolower(namen.dif.var),collapse=" + "), ";",sep="")
					  }
					}  
                   if(length(HG.var)>0) {ind.2   <- grep("^regression$",syntax)
                                         syntax[ind.2] <- paste(crop(paste( c(syntax[ind.2], tolower(namen.hg.var)), collapse=" ")),";",sep="")
                                         if(method=="gauss") {sunk(paste("genConquestSynLab_",ver," Gaussian quadrature is only available for models without latent regressors. Use 'Bock-Aitken' instead.\n",sep=""))
                                                                   method <- "quadrature"}
                                         ### method muss "quadrature" sein
                                         }
                   if(length(namen.group.var)>0)
                                        {ind.3   <- grep("^group$",syntax)
                                         stopifnot(length(ind.3) == 1)
                                         syntax[ind.3] <- paste(crop(paste( c(syntax[ind.3], tolower(namen.group.var)), collapse=" ")),";",sep="")
                                         ### gebe gruppenspezifische Descriptives
                                         add.syntax.pv  <- as.vector(sapply(namen.group.var, FUN=function(ii) {paste("descriptives !estimates=pv, group=",tolower(ii)," >> ",normalize.path(subFolder$out),rep("\\",length(subFolder$out)),jobName,"_",tolower(ii),"_pvl.dsc;",sep="")} ) )
                                         add.syntax.wle <- as.vector(sapply(namen.group.var, FUN=function(ii) {paste("descriptives !estimates=wle, group=",tolower(ii)," >> ",normalize.path(subFolder$out),rep("\\",length(subFolder$out)),jobName,"_",tolower(ii),"_wle.dsc;",sep="")} ))
                                         ind.3    <- grep("quit",syntax)
                                         stopifnot(length(ind.3)==1)
                                         syntax   <- c(syntax[1:(ind.3-1)],add.syntax.pv, add.syntax.wle, syntax[ind.3:length(syntax)])
                                         }
                   if(length(namen.weight.var)>0)
                                        {ind.4   <- grep("caseweight",syntax)
                                         stopifnot(length(ind.4)==1)
                                         syntax[ind.4] <- paste( syntax[ind.4], " ",tolower(namen.weight.var),";", sep="")
                                         }                      
                   if(is.null(beginn)) {beginn <- ID.char+1}
                   syntax[ind] <- paste(syntax[ind], "responses ",beginn,"-",beginn-1+var.char*ncol(data.frame(daten[,itemspalten],stringsAsFactors=F)),";",sep="")
                   if(var.char>1)                                               ### Items haben mehr als eine Spalte Stelligkeit (Conquest-Handbuch, S.177)
                     {syntax[ind] <- paste(gsub(";","",syntax[ind]), " (a",var.char,");",sep="")}
                   ### Method wird erst hier gesetzt, weil sie davon abhaengt, ob es ein HG-Modell gibt 
                   syntax    <- gsub("####hier.method.einfuegen####",method,syntax)
                   if(is.null(model)) {
                      sunk("No Q matrix indicated. Specify one-dimensional model.\n")
                      model <- data.frame(item=colnames(daten[,itemspalten]), dim.1=1,stringsAsFactors=F) 
                   }
                   if(is.null(dim(model)))  {                                ### Wenn Dimensionalität nicht als Q-Matrix, sondern als Liste spezifiziert ist, wird die benötigte Q-Matrix erzeugt
                     model <- .genQMatrix(dimSpecification=model, data=daten,itemCols=itemspalten) 
                   }
				   if(method != "montecarlo" & nodes^(ncol(model)-1) > 10000 )  {
				      sunk(paste("genConquestSynLab_",ver," Caution! Specified model will use '",method,"' estimation with ",nodes^(ncol(model)-1)," nodes.\n",sep=""))
				   }
                   namen.dim <- colnames(model)[-1]
                   score.statement <- .writeScoreStatementMultidim (data=daten, itemCols=itemspalten, qmatrix=model, columnItemNames = 1 ,use.letters=use.letters, allowAllScoresEverywhere = allowAllScoresEverywhere  )
                   ind <- grep("labels ",syntax)
				stopifnot(length(ind)==1)
                   syntax <- c(syntax[1:ind],score.statement,syntax[(ind+1):length(syntax)])
                   if(length(HG.var)==0) {ind.2 <- grep("^regression$",syntax)    ### wenn kein HG-model, loesche entsprechende Syntaxzeilen
                                          stopifnot(length(ind.2)==1)
										  syntax <- syntax[-ind.2]
										  ind.3 <- grep("export reg_coefficients",syntax)
										  stopifnot(length(ind.3)==1)
										  syntax <- syntax[-ind.3]
										  }
                   if(length(namen.group.var) ==0) { ind.3 <- grep("^group$",syntax)    ### wenn keine Gruppen definiert, loesche Statement
                                               stopifnot(length(ind.3)==1)
                                               syntax <- syntax[-ind.3]}
                   if(length(namen.weight.var) ==0) { ind.4 <- grep("^caseweight$",syntax)    ### wenn keine Gewichtungsvariable definiert, loesche Statement
                                               stopifnot(length(ind.4)==1)
                                               syntax <- syntax[-ind.4]}
                   if(match.arg(equivalence.table) == "NULL") { ind.5 <- grep("^equivalence",syntax) ## wenn keine Equivalence-Statement definiert, lösche Zeile
                                                     stopifnot(length(ind.5)==1)
                                                     syntax <- syntax[-ind.5]}                                              
                   if(is.null(ANKER))  {ind.2 <- grep("anchor_parameter",syntax)### wenn keine Anker gesetzt, loesche entsprechende Syntaxzeile
                                        syntax <- syntax[-ind.2]}
                   if(!is.null(ANKER)) {ind.2 <- grep("^set constraints",syntax) ### wenn Anker gesetzt, setze constraints auf "none"
                                        if(match.arg(constraints) != "none") { sunk(paste("genConquestSynLab_",ver,": Anchorparameter were defined. Set constraints to 'none'.\n",sep=""))}
                                        syntax[ind.2]  <- "set constraints=none;"}
                   if(!is.null(namen.dim))
                                       {lab.dim   <- data.frame(lab.dim.1=c("===>",1:length(namen.dim)), lab.dim.2=c("dimensions",namen.dim), stringsAsFactors=F)
                                        colnames(lab.dim) <- colnames(lab)
                                        lab       <- rbind(lab,lab.dim)}
				   cq.version <- getConquestVersion( pathConquest )
				   if(cq.version < as.date("1Jan2007") )
									   {ind.3 <- grep("^export history",syntax)   ### wenn Conquest aelter als 2007, soll history geloescht werden
                                        syntax <- syntax[-ind.3]}
				   ## write(syntax,paste(pfad,"/",Name,".cqc",sep=""),sep="\n")
                   return(list(syntax=syntax, lab=lab))}

				   
### Hilfsfunktionen für gen.syntax
.genQMatrix <- function(dimSpecification, data, itemCols) {
               sunk("Dimensionen als Liste spezifiziert. Wandle in Q-Matrix um!\n")
               allVariables <- colnames(data.frame(data[,itemCols],stringsAsFactors=F))
               if(length(allVariables) != length(unique(unlist(dimSpecification))) )  {
                  stop("Numbers of specified variables does not match numbers of variables in data set.")
               }
			   n.dim <- length(dimSpecification)
               q.mat <- data.frame(item=allVariables, matrix(0, ncol=n.dim), stringsAsFactors=F)
               colnames(q.mat)[-1] <- paste("dim.",1:(ncol(q.mat)-1),sep="")
               for (i in seq(along=dimSpecification)) {
                    q.mat[dimSpecification[[i]],i+1] <- 1
			   }
               return(q.mat) 
}


### columnItemNames         ... in welcher Spalte der q-Matrix stehen Itemnamen? 
### columnsDimension        ... in welchen Spalten der Q-Matrix stehen die Dimensionen?
###                             Default: in erster Spalte stehen Itemnamen, in allen übrigen Spalten stehen Indikatoren für Dimensionen
.writeScoreStatementMultidim <- function(data, itemCols, qmatrix, columnItemNames = 1 ,columnsDimensions = -1, use.letters=use.letters, allowAllScoresEverywhere ) {
            n.dim      <- (1:ncol(qmatrix) )[-columnItemNames]                  ### wieviele Dimensionen?
            deleteRows <- which( rowSums(qmatrix[,n.dim,drop=F]) == 0)          ### lösche Items aus Q-Matrix, die auf keiner Dimension laden
            if(length(deleteRows)>0)   {
              qmatrix <- qmatrix[-deleteRows,]
      			  sunk(paste(length(deleteRows)," items in Q matrix does not depend to any dimension. Items were deleted from q matrix. \n",sep=""))
      	    }
            sunk(paste("Q matrix specifies ",length(n.dim)," dimensions.\n",sep=""))
            # columnItemNames <- grep("item",colnames(qmatrix))                  ### wo stehen Items in Q-Matrix?
		        # if(length(columnItemNames)!=1) {stop("Kann Itemspalte in Q-Matrix nicht eindeutig zuordnen.\n")}
            misInQmatrix <- setdiff(colnames(data[,itemCols]),  qmatrix[,columnItemNames])                       ### Items im Datensatz, aber nicht in Q-Matrix?
            if(length(misInQmatrix)>0) {
      		     sunk("Items in dataset without specification in Q matrix.\n")
               sunk(paste(misInQmatrix,collapse=", ")); cat("\n"); stop()
      		  }
            all.variables <- colnames(data)[itemCols]                           ### alle Items im datasatz
            dim.need     <- lapply(n.dim,FUN=function(ii) {0:1})                ### Kompliziert: Hier werden, abhängig der Anzahl der Dimensionen, alle Möglichkeiten der Ladungsbelegungen durchpermutiert; bei N Dimensionen sind das 2^N Belegungen, wenn Mehrfachbelegungen (within Item dimensionality) erlaubt sind
            score.matrix <- data.frame(score=1, expand.grid(dim.need), matrix(NA,nrow=nrow(expand.grid(dim.need)), ncol=length(all.variables)),stringsAsFactors=F)
            score.matrix <- score.matrix[-1,]
            scoreColumns <- grep("Var",colnames(score.matrix))
            for (i in 1:length(all.variables))  {                               ### gebe alle Items auf den jeweiligen Dimensionen
               qmatrix.i    <- qmatrix[qmatrix[,columnItemNames] == all.variables[i],]# auf welcher Dimension lädt Variable i? Untere zeile: in diese Zeile von score.matrix muß ich variable i eintragen
               matchRow     <- which(sapply ( 1:nrow(score.matrix) , function(ii) {all ( as.numeric(qmatrix.i[,n.dim]) == as.numeric(score.matrix[ii,scoreColumns])) }))
               matchColumn  <- min(which(is.na(score.matrix[matchRow,])))       ### in welche spalte von Score.matrix muß ich variable i eintragen?
               score.matrix[matchRow,matchColumn] <- i
		        }
            rowsToDelete <- which(is.na(score.matrix[, max(scoreColumns) + 1])) ### welche Zeilen in Score.matrix können gelöscht werden?
            if(length(rowsToDelete)>0) {score.matrix <- score.matrix[-rowsToDelete, ]}
            for (ii in 1:nrow(score.matrix)) {score.matrix[,ii] <- as.character(score.matrix[,ii])}
            itemdata <- data[,itemCols, drop = FALSE]
			score.matrix <- fromMinToMax(dat = itemdata, score.matrix = score.matrix, qmatrix = qmatrix, allowAllScoresEverywhere = allowAllScoresEverywhere, use.letters = use.letters)
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
            ### Prüfung, ob "tranformation" des score-statements ok ist
            control <- lapply(kollapse.string,FUN=function(ii) {eval(parse(text=paste("c(",gsub("-",":",ii),")",sep="")))})
            if (!all(unlist(lapply(1:length(control), FUN=function(ii) {all(kollapse[[ii]] == control[[ii]])})))) {
                sunk("Error in creating score statement.\n")
			      }
            score.matrix <- data.frame(prefix="score",score.matrix[,c(1,scoreColumns)],items="! items(",kollapse.string=unlist(kollapse.string),suffix=");",stringsAsFactors=F)
            score.statement <- sapply(1:nrow(score.matrix), FUN=function(ii) { paste(score.matrix[ii,],collapse=" ")})
            return(score.statement)
		}	

### Hilfsfunktion für .writeScoreStatementMultidim()
fromMinToMax <- function(dat, score.matrix, qmatrix, allowAllScoresEverywhere, use.letters)    {
			    all.values <- alply(as.matrix(score.matrix), .margin = 1, .fun = function(ii) { names(table.unlist(dat[,na.omit(as.numeric(ii[grep("^X", names(ii))]))]))  })
				if ( allowAllScoresEverywhere == TRUE ) {
                    all.values <- lapply(all.values, FUN = function(ii) {sort(asNumericIfPossible(unique( unlist ( all.values ) ), verbose = FALSE ) ) } )
                }     
                if(use.letters == TRUE )  {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(LETTERS[which(LETTERS == ii[1]) : which(LETTERS == ii[length(ii)])], collapse=" "),")") } ) ) }
                if(use.letters == FALSE ) {minMaxRawdata  <- unlist ( lapply( all.values, FUN = function (ii) {paste("(",paste(ii[1] : ii[length(ii)],collapse = " "),")")  } ) ) }
                scoring <- unlist( lapply( minMaxRawdata , FUN = function(ii) { paste("(", paste( 0 : (length(unlist(strsplit(ii, " ")))-3), collapse = " "),")")}) )
                stopifnot(length(scoring) == length( minMaxRawdata ) )
                stopifnot(length(scoring) == nrow(score.matrix ) )
                options(warn = -1)                                              ### warnungen aus
                for (i in 1:nrow(score.matrix))    {
                    score.matrix$score[i] <- minMaxRawdata[i]
                    targetColumns         <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) == 1 ) ) 
                    stopifnot(length(targetColumns) > 0 )
                    score.matrix[i,targetColumns]  <- scoring[i] 
                    nonTargetColumns      <- intersect ( grep("Var",colnames(score.matrix)), which(as.numeric(score.matrix[i,]) == 0 ) )
                    if ( length ( nonTargetColumns ) > 0 )    {
                       score.matrix[i,nonTargetColumns]  <- "()"
                    }
                }
                options(warn = 0)                                               ### warnungen wieder an
                return(score.matrix)}    