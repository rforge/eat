# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# automateConquestModel
#
# Description: calls Functions to prepare, run and report Conquest Models
# Version: 	0.7.0
# Status: stable
# Release Date: 	2011-11-15
# Author:    Karoline Sachse, Martin Hecht
# Change Log:
#
# 2011-12-05 SW
# CHANGED: in automateConquestModel(): method set to 'montecarlo' when no 'method' specified and more than 3500 nodes are used
# 0000-00-00 AA
#
# 20.06.2011 (SW): Neue Schnittstellen implementiert 
# 22.06.2011 (SW): Funktion gibt TRUE zurück, wenn alles ok ist. 
# 27.06.2011 (SW): Gebe Funktionsmane und Versionsnummer vor jeder Nachricht.
#                  Fehlermeldungen überarbeitet.
# 14.07.2011 (SW): Gruppenstatement überarbeitet +++ default für n.plausible 
#                  aus Kopf raus nach Sektion "Defaults" geschrieben
# 08.08.2011 (SW): nodes, deviancechange, iterations variabel
# 08.08.2011 (MH): auf stable gesetzt wegen besserer sourcebarkeit
# 17.08.2011 (MH): auf stable gesetzt wegen besserer Sourcebarkeit
# 20.09.2011 (SW): Möglichkeit zur Missingrecodierung; separat für Testitems, HG-Var, DIF-Var, etc.
# 14.10.2011 (MH): gestabled, Umlaute ersetzt
# 14.11.2011 (SW): "trimSpace" durch "crop" ersetzt
# 10.11.2011 (SW): Funktion sollte jetzt auch partial credit beherrschen
# 14.11.2011 (MH): pathConquest defaulted
# 15.11.2011 (MH): auf stable gesetzt
# 25.11.2011 (SW): 'cat' durch 'sunk' ersetzt
# 05.12.2011 (SW): Method default montecarlo, wenn mehr als 3500 nodes
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### dat             ... Datensatz als R-dataframe
### ID              ... Name oder Spaltennummer der ID-Variablen
### regression      ... optional: Variablen für das latente Regressionsmodell, entweder als Vektor mit Spaltennummern oder Variablennamen
### DIF             ... optional: DIF-Variable, entweder als Spaltennummer oder als Variablenname
### group.var       ... optional: Gruppenvariable(n), entweder als Vektor/Skalar mit Spaltennummern oder Variablennamen
### weight			    ... optional: Gewichtungsvariable, entweder als Vektor/Skalar mit Spaltennummern oder Variablennamen
### items           ... Spaltennummern oder Variablennamen der Testitems 
### person.grouping ... WIRD HIER NOCH NICHT VERARBEITET
### item.groping    ... Q-Matrix zur Spezifikation der Zugehörigkeit von Items zu latenten Dimensionen 
### m.model         ... WIRD HIER NICHT VERARBEITET
### Title           ... optional: Titel der Analyse
### jobName         ... Name der Analyse (Name für alle Input und Outputdateien)
### jobFolder       ... Verzeichnis für die Analyse
### subFolder       ... optional: Liste mit Unterverzeichnissen für Datensatz und Output (relative Pfaddefinition)
### dataName        ... optional: Name des Datensatzes
### anchor          ... optional: R-Dataframe mit Ankerparametern (1. Spalte: Variablenname, 2. Spalte: Verankerungsparameter)
### pathConquest    ... Pfad und Dateiname der Conquest-Konsole
### n.plausible     ... wieviele plausible values sollen gezogen werden?
### set.constraints ... "none" , "cases" (default) , "items" ; bei Anchor wird automatisch auf "none" gesetzt
### f.nodes			    ... positive ganzahlige Zahl (integer); Conquest-Handbuch S. 225
### p.nodes  		    ... positive ganzahlige Zahl (integer); Conquest-Handbuch S. 225
### method			    ... optional: "gauss" (default), "quadrature", "montecarlo"; Conquest-Handbuch S. 225
### std.err         ... optional: "full", "quick" (default), "none"; Conquest-Handbuch S. 167ff
### n.interations	  ... positive ganzahlige Zahl (integer); Conquest-Handbuch S. 225
### converge		    ... Gleitkommazahl; Conquest-Handbuch S. 225
### distribution    ... "normal" (default), "discrete"; Conquest-Handbuch S. 167ff
### name.unidim     ... optional: Name der Dimension, wenn es nur eine gibt
### equivalence.table ... Gibt ggf. Tabelle mit Umrechnungen Rohwert-Normwert; mögliche Werte sind "wle" (default); "mle" oder NULL (keine Tabelle wird ausgegeben)
###                       (Conquest-handbuch, S.166)
### na           ... Liste mit Codes, die als NA zu behandeln sind, für Items und Hintergrundvariablen separat;
###                  z.B. na=list(items=c(6,7,8,9,96,97,98,99), DIF=9)
### verbose      ... logical: should messages printed on console?

automateConquestModel <- function ( dat, ID, regression=NULL, DIF=NULL, group.var=NULL, weight=NULL, items, na=list(items=NULL, DIF=NULL, HG=NULL, group=NULL, weight=NULL), person.grouping=NULL, item.grouping=NULL,
                                    model.statement="item", m.model="1pl", Title = NULL, jobName, jobFolder, subFolder=list(), dataName=NULL, anchor=NULL, pathConquest="C:/ConQuest/console_Feb2007.exe", method=NULL,std.err=NULL,distribution=NULL,
                                    n.plausible=NULL, set.constraints=NULL, nodes=NULL, p.nodes=NULL, f.nodes=NULL, n.iterations=NULL, converge=NULL, deviancechange=NULL, name.unidim=NULL,
                                    equivalence.table="wle",use.letters=FALSE, checkLink = FALSE, verbose = TRUE)	 {
    
	original.options <- options("scipen")                         ### lese Option für Anzahl der Nachkommastellen 
    options(scipen = 20)                                          ### setze Option für Anzahl der Nachkommastellen
                  
    ver <- "0.7.0"
    ret <- TRUE
    if(missing(dat)) {stop(paste("Error in automateConquestModel_",ver,": No dataset specified.\n",sep="")) }
    if(missing(jobName)) {stop(paste("Error in automateConquestModel_",ver,": No 'jobName' chosen.\n",sep="")) }
    if(missing(ID))      {stop(paste("Error in automateConquestModel_",ver,": No ID specified.\n",sep="")) }
    if(length(DIF)>1)    {stop(paste("Error in automateConquestModel_",ver,": There can only be one DIF variable.\n",sep="")) }
    if(length(weight)>1) {stop(paste("Error in automateConquestModel_",ver,": There can only be one weight variable.\n",sep="")) }

	### Defaults
	if(is.null(dataName))        {dataName <- paste(jobName,".dat",sep="")} 
	if(is.null(subFolder))        {subFolder <- list()} 
  if(is.null(n.plausible))     {n.plausible <- 5}
	if(is.null(set.constraints)) {set.constraints <- "cases"}
    ### if(is.null(nodes))           {nodes <- 15}	               ### nodes werden erst in genConquestSynLab gesetzt, da sie davon abhängen ob Montecarlo gesetzt ist
	if(is.null(method))    {
     method   <- "gauss"
     if(is.null(nodes))   {
	    nodes <- 15
		if(!is.null(item.grouping))   {                               ### wunsch von Thilo: wenn mehr als 3500 nodes und keine 'method' explizit spezifiziert: montecarlo
           used.nodes <- nodes^(ncol(item.grouping)-1 )               ### das alles geschieht NICHT, wenn der Benutzer explizit 'gauss' oder was-auch-immer wünscht
        }
        if(is.null(item.grouping))   {                                            
           used.nodes <- nodes
        }
        if( used.nodes > 3500)  {
          sunk(paste("automateConquestModel_",ver,": Specified model will use ",used.nodes," nodes. Chosen default method '",method,"' probably is not appropriate. \nChange method to 'montecarlo' with 1000 nodes. Otherwise, please specify your settings explicitly.\n",sep=""))
          nodes <- 1000
		  method <- "montecarlo"
       }
	}   
  }
	if(is.null(std.err))         {std.err  <- "quick"}
	if(is.null(distribution))    {distribution <- "normal"}
    if(is.null(n.iterations))    {n.iterations <- 1000}
    if(is.null(converge))        {converge     <- 0.0001}
    if(is.null(deviancechange))  {deviancechange <- 0.0001}
    if(is.null(f.nodes))         {f.nodes <- 2000}
    if(is.null(p.nodes))         {p.nodes <- 2000}
    if(is.null(name.unidim))     {name.unidim <- "dimension_1"}
    if(is.null(model.statement))     {model.statement <- "item"}
    if(is.null(equivalence.table))     {equivalence.table <- "wle"}
    if(is.null(use.letters))     {use.letters <- FALSE}
    if(is.null(pathConquest))     {pathConquest <- get.file.from.dir(dr=file.path(.Library,"eat/winexe/conquest"), ext="exe", vers="newest", crit.level="stop" )}
	
	# Check ob pathConquest in Ordnung
	if ( ! file.exists ( pathConquest ) ) {
			sunk ( paste ( "Error:" , pathConquest , "not found / readable / executable.\n" ) )
			stop ( )
	}		
	
    sunk(paste("automateConquestModel_",ver,": Use following settings:\n",sep=""))
    sunk(paste("    constraints: ",set.constraints,"; method: ",method,"; standard error: ",std.err,"; assumed population distribution: ",distribution,"\n",sep=""))
    sunk(paste("    max. iterations: ",n.iterations,"; converge: ",paste("0",substring(as.character(converge+1),2),sep=""),"; deviancechange: ",paste("0",substring(as.character(deviancechange+1),2),sep=""),"; f.nodes: ",f.nodes,"; p.nodes: ",p.nodes,"\n",sep=""))
        	  
    ### Verzeichnisangaben dürfen weder mit einem Schrägstrich beginnen noch damit enden!
    if(!missing(jobFolder))
    ### entferne ggf. abschließende Schrägstriche
      {jobFolder <- crop(jobFolder,char = "/")}

    if(!is.null(subFolder$out)) 
    ### entferne ggf. abschließende Schrägstriche
      {subFolder$out <- crop(subFolder$out,char = "/")}

    if(!is.null(subFolder$data)) 
    ### entferne ggf abschließende Schrägstriche   
      {subFolder$data <- crop(subFolder$data,char = "/")}
    
		# Datensatz für Conquest erzeugen
		sunk(paste("automateConquestModel_",ver,": Prepare dataset for use in Conquest.\n",sep=""))
    
	# library(debug)
	# mtrace(genConquestDataset)

		if(inherits(try( conquestDataset <- genConquestDataset ( dat=dat, variablen= items, ID=ID, DIF.var=DIF, HG.var=regression, group.var=group.var, weight.var=weight, na=na,
                                                                 use.letters=use.letters, checkLink = checkLink)  ),"try-error"))
      { ret <- FALSE; sunk(paste("automateConquestModel_",ver,": Fehler beim Aufbereiten des Datensatzes fuer Conquest.\n",sep="")); stop()}
    
    flush.console()
    # Rückgabe:
    # Liste mit (mindestens) zwei Einträgen: ein dataframe (datensatz), ein numerischer Vektor mit Angaben über spaltnbreiten 

		# Syntax und Labels für Conquest erzeugen
		sunk(paste("automateConquestModel_",ver,": Generate syntax and label list.\n",sep=""))
    if(inherits(try(     conquestSynLabList <-  genConquestSynLab(jobName=jobName, datConquest=conquestDataset$daten.dat, namen.items=conquestDataset$namen.items, 
                                        namen.hg.var = conquestDataset$namen.hg.var, namen.dif.var = conquestDataset$namen.dif.var, DIF.char=conquestDataset$DIF.char,
                                        namen.all.hg=conquestDataset$namen.all.hg, all.hg.char=conquestDataset$all.hg.char, namen.group.var=conquestDataset$namen.group.var,
                                        namen.weight.var=conquestDataset$namen.weight.var, weight.char=conquestDataset$weight.char,method=method,std.err=std.err,model.statement=model.statement,
										distribution=distribution,model=item.grouping, ANKER=anchor, jobFolder=jobFolder, name.dataset=dataName, subFolder=subFolder, Title=Title, n.plausible=n.plausible,
                                        constraints=set.constraints, n.iterations=n.iterations, nodes=nodes, p.nodes=p.nodes, f.nodes=f.nodes, converge=converge, deviancechange=deviancechange, 
                                        name.unidim=name.unidim, equivalence.table=equivalence.table,use.letters=use.letters,var.char=conquestDataset$var.char,  pathConquest = pathConquest)  ),"try-error"))
      { ret <- FALSE; sunk(paste("automateConquestModel_",ver,": Error in generating syntax and label list.\n",sep="")); stop()}

		flush.console()
    # Rückgabe:
		# Liste mit zwei Einträgen: einen Vektor von Strings (Syntax) und einen Dataframe (Labels)

    # R-dataframe mit Ankerparametern (optional)
    conquestAnker <- NULL
    if(!is.null(anchor))
      {sunk(paste("automateConquestModel_",ver,": Create list with anchor parameter.\n",sep=""))
       if(inherits(try( conquestAnker <- genConquestAnker(daten= dat ,itemspalten= items , prm.file=anchor , verbose = verbose)    ),"try-error"))
		 { ret <- FALSE; sunk(paste("automateConquestModel_",ver,": Error in creating list with anchor parameter.\n",sep="")); stop()}
       flush.console()}
    
    sunk(paste("automateConquestModel_",ver,": Create batch string to call Conquest.\n",sep=""))
    if(inherits(try(     conquestBatch <- genConquestBatch ( pathConquest=pathConquest, jobName =jobName)   ),"try-error"))
      { ret <- FALSE; sunk(paste("automateConquestModel_",ver,": Error in creating batch string to call Conquest.\n",sep="")); stop()}
    flush.console()
    
		### Datensatz, Syntax und Labels für Conquest in Ordner schreiben  --> writeConquest To Disk soll TRUE oder FALSE zurückgeben
		sunk(paste("automateConquestModel_",ver,": Write all input files.\n",sep=""))
    if(inherits(try(     jobBatch <- writeConquestToDisk ( conquestDataset=conquestDataset$daten.dat, conquestDatasetWidth= conquestDataset$daten.width, nameConquestDataset=dataName,
		                                  conquestSyntax = conquestSynLabList$syntax, conquestLabels= conquestSynLabList$lab, conquestBatchString=conquestBatch,
		                                  conquestAnker=conquestAnker, jobFolder=jobFolder, subFolder=subFolder, name.analyse=jobName)  ),"try-error"))
      { ret <- FALSE; sunk(paste("automateConquestModel_",ver,": Error in writing all input files.\n",sep="")); stop()}

    flush.console()
    
	  if (!ret) stop() else return(ret)
	options(scipen = original.options)                             ### setze Optionen wieder in Ausgangszustand
    }
		
		
