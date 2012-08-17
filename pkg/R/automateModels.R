# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# automateModels
# Change Log:
# 2012-01-17 SW/MH
# CHANGED: added option 'all.local.cores' in automateModels
# 2011-12-15 SW/MH
# CHANGED: q3 Option is model specific now in automateModels
# 2011-12-12 SW/MH
# ADDED: make.q3 is called in automateModels
# 2011-12-07 MH
# CHANGED: item evaluation temporarily disabled in automateModels
# CHANGED: paramter adjust.for.regression temporarily disabled in automateModels
# 2011-11-29 SW/MH
# CHANGED: modified results structure in automateModels
# 0000-00-00 AA
#				14.11.2011 MH: 'conquestPath' gelöscht, ist ab jetzt Bestandteil von 'conquestParameters'
#				11.11.2011 MH: Dokumentation ab jetzt in Rd-Files
#							   Conquest default von Oct2005 auf Feb2007 geändert
#							   conquestParameters als Parameter angelegt (genamte Liste)
#									n.plausible nach dort verschoben
#				10.11.2011 MH: neue Version nur wegen neuem Package (mit neuen Rd-files), keine code-änderung
#								(außer "Begrüßung")
#				14.10.2011 MH:
#							   -- Parameter 'develop' entfernt (zur Vorbereitung für Package)
#							      für development (sourcen aus p:\ZKD\development\) muss man
#							      ab jetzt selber .automateModels.init (aus automateModels.init.R)
#							      bemühen
#							   -- Ausgaben auf Englisch	
#				08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#				26.08.2011 MH: adjust.for.regression eingefügt
#				19.08.2011 MH: analyse.name.elements eingefügt
#				18.08.2011 MH: write.txt.dataset und delete.folder.countdown implementiert
#				0.2.6 13.08.2011 item.eval rausgenommen da z.Z. buggy
#				21.06.2011 MH: Version 0.0.1 läuft mit automateConquestModel zusammen testmäßig schon gut
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
automateModels <- function ( dat , id = NULL , context.vars = NULL , items = NULL ,
							 item.grouping = NULL , select.item.group = NULL , 
							 person.grouping.vars = NULL ,
							 person.grouping.vars.include.all = FALSE ,
							 person.grouping = NULL , select.person.group = NULL ,
							 checkLink = FALSE ,
							 additional.item.props = NULL ,
							 folder ,
							 overwrite.folder = TRUE , 
							 analyse.name.prefix = NULL , analyse.name = NULL ,
							 analyse.name.elements = NULL , 
							 data.name = NULL ,
							 m.model = NULL , software = NULL , 
							 dif = NULL , weight = NULL , anchor = NULL , regression = NULL , 
							 q3 = FALSE ,
							 q3.p.est = c ( "wle" , "pv" , "eap" ) ,
							 icc = FALSE ,
							 missing.rule = NULL , 
							 cross = NULL , subfolder.order = NULL , subfolder.mode = NULL ,
							 allNAdelete = TRUE ,
							 additionalSubFolder = NULL ,
							 run.mode = NULL , n.batches = NULL , run.timeout = 1440 , run.status.refresh = 0.2 ,
							 all.local.cores = TRUE ,
							 email = NULL , smtpServer = NULL , 
							 write.txt.dataset = FALSE , 
							 write.xls.results = TRUE ,
							 delete.folder.countdown = 5 ,
							 conquestParameters = NULL ) {
							 
		# hotfix, 22. Mai 2012, der sagt, dass selbst wenn der Benutzer das, was Conquest exportieren soll, als Liste spezifiziert, es nicht als Liste, sondern als (genameder) Vektor an automateModels weitergegeben wird.
		# versteht ihr nicht? wir auch nicht ... 
		if(!is.null(conquestParameters$export)) {conquestParameters$export <- unlist(conquestParameters$export)}
		
		
		### Funktionsname für Meldungen
		f. <- "automateModels"
		f.n <- paste ( f. , ":" , sep = "" )
		### löschen / erstellen von Ausgabeverzeichnis
		if ( overwrite.folder ) .del.or.create.folder ( folder , delete.folder.countdown )
		###_automateModels_ folder erstellen
		folder.aM <- file.path ( folder , "_automateModels_" )
		if ( ! file.exists ( folder.aM ) ) { dir.create ( folder.aM , recursive = TRUE ) }			
	
		### logfile initieren
		sunk.path <- file.path ( folder.aM , "automateModels.Log.txt" )
		
		### Begrüßung
		sunk ( paste ( f.n , "Starting automateModels\n" ) , new.file = TRUE ) 
		sunk ( paste ( f.n , 'Version: 1.5.15-128 (2012-08-14)\n' ) )
		sunk ( paste ( f.n , '         \n' ) )
		sunk ( paste ( f.n , "This version is BETA. Use at your own risk.\n" ) )
		### Definitionen
		m.model.available <- c ( "1pl", "2pl", "3pl" , "4pl" ) 
		software.available <- c ( "conquest" )
		cross.available <- c ( "all" , "item.groups" , "person.groups" ) 
		
		### Datensatz Treatment
		retlist <- .automateModels.datasetTreatment ( dat , id , context.vars , items )
		dat <- retlist$dataset
		id.name <- retlist$id.name
		cont.names <- retlist$cont.names
		item.names <- retlist$item.names
		rm ( retlist )
		### wenn person.grouping.vars spezifiziert, dann hieraus person.grouping bauen
		if ( !is.null ( person.grouping.vars ) ) {				
				person.grouping <- .automateModels.grouping.vars.to.grouping ( dat , person.grouping.vars , person.grouping.vars.include.all , id.name )
				sunk ( paste ( f.n , "Info:" , "person.grouping is" , paste ( colnames ( person.grouping )[-1] , collapse = ", " ) , ".\n" ) ) 
		}
	
		### Handling der Item- bzw. Person-Grouping
		### d.h. Spalten-Reduktion von item.grouping / person.grouping anhand select.item.group / select.person.group
		retlist <- .automateModels.handle.global.grouping ( item.grouping , person.grouping , select.item.group , select.person.group )
		item.grouping <- retlist$item.grouping
		person.grouping <- retlist$person.grouping
		rm ( retlist )
		
		### Default für Item-Grouping und Person-Grouping
		if ( is.null ( item.grouping ) ) { 
					# Default Item-Grouping erzeugen
					item.grouping <- .automateModels.set.item.grouping.default ( dat , item.names )
					# in select.item.group rein, damit später nicht rausgekickt wird
					#select.item.group <- c ( select.item.group , colnames ( item.grouping )[2] )
				}
		if ( is.null ( person.grouping ) ) {
					person.grouping <- .automateModels.set.person.grouping.default ( dat , id.name )
					#select.person.group <- c ( select.person.group , colnames ( person.grouping )[2] )
				}
		if ( ! inherits ( item.grouping , "list" ) ) item.grouping <- list ( item.grouping )
		if ( ! inherits ( person.grouping , "list" ) ) person.grouping <- list ( person.grouping )
		
		### Default für Modell-Parameter
		if ( is.null ( m.model ) ) m.model <- m.model.available[1]
		if ( is.null ( software ) ) software <- software.available[1]
		
		### Default für weitere/softwarespezifische Angaben
		if ( is.null ( run.mode ) ) run.mode <- "serial"
		if ( is.null ( missing.rule ) ) missing.rule <- list ( mvi = 0 , mnr = 0 , mci = NA , mbd = NA , mir = 0 , mbi = 0 )
	    if ( is.null ( analyse.name.elements ) ) analyse.name.elements <- c ( "scale" , "group" , "dif" , "regression" , "anchor" )
		
		### Technische Definitionen
		# Listen bauen um technisch gleich zu treatende Elemente zu gruppieren
		# list1: item- und person-grouping
		list1 <- list ( item.grouping = item.grouping , person.grouping = person.grouping )
		# list2: Elemente mit available-checks
		list2 <- list ( m.model = m.model , software = software , cross = cross )
		# VORSICHT mit Sortierung, muss korrespondieren mit list2 
		list2.available <- list ( m.model.available = m.model.available ,
								  software.available = software.available ,
								  cross.available = cross.available )	
		# list3: Elemente, die "skalar" keine Listen sind
		list3 <- list ( analyse.name = analyse.name , dif = dif , weight = weight , anchor = anchor , 
						regression = regression , data.name = data.name , q3 = q3 )
		list3.checkType <- list ( "character" , "character" , "character" , "data.frame" , 
						"character" , "character" , "logical" )
		# list4: Elemente, die selber Listen sind ( extra behandeln, sind sonst schlecht behandelbar )
		list4 <- list ( missing.rule = missing.rule , conquestParameters = conquestParameters )
		
		### Plausichecks/Aufbereiten von allen Modellspezifikationen
		model.specs <- .automateModels.prepare ( list1 , list2 , list2.available , list3 , list3.checkType , list4 )
	
		### i.model und p.model anhand der Grouping-Informationen setzen
		retlist <- .automateModels.i.p.model.default ( model.specs$item.grouping , model.specs$person.grouping )	
		model.specs$i.model <- retlist$i.model
		model.specs$p.model <- retlist$p.model 	
		rm ( retlist )
	
		### Aufspalten/"Kreuzen" von Item-Gruppen und Personen-Gruppen
		if ( any ( !is.null ( unlist( model.specs$cross ) ) ) ) model.specs <- .automateModels.crossModels ( model.specs )
		
		### spezielle Aufbereitung für Conquest "Multigruppen" Treatment
		model.specs <- .automateModels.conquest.multigroup ( model.specs , dat )
		
		### Datensatz modellspezifisch aufbereiten
		# id.name modellspezifisch setzen 
		model.specs$id.name <- mapply ( function ( dummy ) {
							id.name
					} , model.specs$item.grouping , SIMPLIFY=FALSE )	
					
		# cont.names modellspezifisch setzen 
		model.specs$cont.names <- mapply ( function ( dif , weight , regression , group ) {
							unique ( c ( dif , weight , regression , group ) )
					} , model.specs$dif , model.specs$weight , model.specs$regression , model.specs$group , SIMPLIFY=FALSE )			
		# Item/Person-Grouping reduzieren
		model.specs$item.grouping <- .automateModels.reduce.item.and.person.grouping ( model.specs$item.grouping )
		model.specs$person.grouping <- .automateModels.reduce.item.and.person.grouping ( model.specs$person.grouping )
	
		# item.names modellspezifisch setzen 
		model.specs$item.names <- mapply ( function ( item.grouping , item.names ) {
							items <- item.grouping[ , 1 ]
							# sortieren wie in Datensatz
							items [ na.omit ( match ( item.names , items ) ) ]
					} , model.specs$item.grouping , MoreArgs = list ( item.names ) , SIMPLIFY=FALSE )	
		# missing.rule-spezifische Datensätze aus Gesamtdatensatz bilden
		misrule.datasets <- .automateModels.create.misrule.datasets (
							dat , items=item.names , items.list=model.specs$item.names , 
							mis.rule=model.specs$missing.rule , folder = folder.aM , 
							write.txt.dataset = write.txt.dataset ) 
						
		# modellspezfischen Datensatz erstellen
		model.specs$dataset <- mapply ( genModelDataset ,
					item.grouping = model.specs$item.grouping ,
					person.grouping = model.specs$person.grouping ,
					keep = model.specs$cont.names , 
					mis.rule = model.specs$missing.rule , 
					MoreArgs = list ( datasets = misrule.datasets , id.name = id.name , allNAdelete = allNAdelete )
					, SIMPLIFY = FALSE )
		# löschen von misspezifizierten Modellen
		# d.h. Modelle in denen der Datensatz keine Items oder Personen mehr hat
		if ( ! identical ( delete <- .which.list.element.is.null ( model.specs$dataset ) , integer(0) ) ) {
				model.specs <- .automateModels.remove.from.model.specs ( model.specs , delete )
		}
		# TODO wenn keine Modelle mehr übrig, abbrechen
					
		# item.grouping/person.grouping reduzieren ( anhand des reduzierten Datensatzes )
		# nötig da evtl. Spalten / Zeilen mit komplett NA gelöscht
		model.specs$item.grouping <- .automateModels.reduce.item.grouping ( model.specs$item.grouping , model.specs$dataset )
		model.specs$person.grouping <- .automateModels.reduce.person.grouping ( model.specs$person.grouping , model.specs$dataset , model.specs$id.name )
		model.specs$item.names <- mapply ( function ( item.grouping , item.names ) {
							items <- item.grouping[ , 1 ]
							# sortieren wie in Datensatz
							items [ na.omit ( match ( item.names , items ) ) ]
					} , model.specs$item.grouping , MoreArgs = list ( item.names ) , SIMPLIFY=FALSE )				
		# TODO gucken ob alle in group.names spezifizierten Gruppen noch da sind, und ob group.var überhaupt (dies sollte allerdings so sein)
		
		### Check ob Models erlaubt bzw. implementiert
		if ( !( .automateModels.check ( model.specs , check = "available" ) &
		.automateModels.check ( model.specs , check = "implemented" ) ) )
				stop("Skript stoppt.")			
		
		### Setzen von Namen, die für analyse.folder und default analyse.name gebraucht werden
		model.specs <- .automateModels.setnames ( model.specs )			
		
		### analyse.folder setzen und auf Platte schreiben
		model.specs <- .automateModels.makesubfolder ( model.specs , folder , subfolder.order , subfolder.mode )		
		
		### analyse.name setzen
		model.specs <- .automateModels.makeanalysename ( model.specs , analyse.name.prefix , analyse.name.elements )
		
		### modelConsistencyCheck TODO
		#modelConsistencyCheck ( model.specs ) 
		
		# Modell-Information auf Platte schreiben
		.automateModels.writeModelInfo ( model.specs ) 
		
		# Modelle (Syntax/Data) auf Platte erzeugen
		.automateModels.createModel ( model.specs , additionalSubFolder )
	
		# Batches erzeugen 
		batches <- .automateModels.genBatches ( model.specs , folder.aM , run.mode , n.batches , all.local.cores )
	
		# Batches starten
		check <- .automateModels.runBatches ( batches , run.mode , all.local.cores )
		stopifnot ( check )
		
		# in Monitor-Modus gehen
		model.specs$done <- .automateModels.monitor.progress ( model.specs$folder , additionalSubFolder$out , model.specs$analyse.name ,
										   software = model.specs$software , refresh = run.status.refresh , time.out = run.timeout , 
										   email = email , smtpServer = smtpServer )
		
		# Ergebnisse einsammeln
		results <- .automateModels.collect.results ( model.specs , additionalSubFolder ) 	
		
		# Personenmittel auf 0 (wichtig falls/für regression)
		results <- .automateModels.adjust.for.regression ( results )
		
		# Convergence Summary schreiben
		isConverged ( folder , txt = TRUE )
		
		# Deviance Change Plots
		plotDevianceChange ( folder , plot = TRUE , pdf = TRUE )
		
		# Itembewertung durchführen
		# results <- .automateModels.item.eval ( results )
		# Q3 erzeugen
		results <- make.q3 ( results , model.specs , q3.p.est )
		# Excels erzeugen
		if ( write.xls.results ) check <- .automateModels.writeResultsExcel ( results , model.specs$analyse.name , model.specs$folder , folder.aM , additional.item.props )
		
		# ICCs schreiben
		if ( icc ) temp <- automateModels.plot.icc ( results , model.specs )
		
		# auf Platte schreiben
		save ( model.specs , file = file.path ( folder.aM , "model.specs.Rdata" )  )
		save ( results , file = file.path ( folder.aM , "results.Rdata" )  )
		save ( dat , file = file.path ( folder.aM , "dat.Rdata" )  )
		save ( id.name , file = file.path ( folder.aM , "id.name.Rdata" )  )
		save ( cont.names , file = file.path ( folder.aM , "cont.names.Rdata" )  )
		save ( item.names , file = file.path ( folder.aM , "item.names.Rdata" )  )
	
		# finale Ausgabe 
		sunk ( "\n" )
		sunk ( paste ( f.n , "terminated successfully!\n\n" ) )
	
		# Ergebnisse returnen
		return ( results )
}
