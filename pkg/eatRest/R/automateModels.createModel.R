# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.createModel, .automateModels.createModel.create
# Description: Subroutine von automateModels
# Version: 	0.6.0
# Status: beta
# Release Date: 	2011-11-15
# Author:    Martin Hecht
# Change Log:
#		11.11.2011 MH: umgearbeitet mit conquestParameters
#		14.10.2011 MH: Ausgaben auf Englisch
#		14.09.2011 MH: "\n" in eatTools:::sunk-Aufrüfen gelöscht (für optisch schöner)
#		08.09.2011 MH: cat durch eatTools:::sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.createModel.create <- function (
						software ,
						dataset ,
						id.name ,
						item.names , 
						item.grouping ,
						person.grouping ,
						i.model , 
						p.model , 						
						m.model , 
						dif ,
						weight ,
						regression ,
						anchor ,
						analyse.name ,
						data.name ,
						folder ,
						group ,
						conquestParameters ,
						all.analyse.name ,
						additionalSubFolder ) {

		# return-Variable setzen
		ret <- FALSE
		
		# ConQuest
		if ( software == "conquest" ) {

				# Conquest-Plausichecks
				# ...

				# Einrücken der Ausgabe
				einr <- "           "
				
				# Modell-Nummer String ( nur für Ausgabe )
				model.nr.str <- paste ( "Model" , which ( all.analyse.name %in% analyse.name ) , "of" , length ( all.analyse.name ) )
				
				# Ausgaben / Checks
				eatTools:::sunk ( paste ( ".automateModels.createModel.create: Sending" , 
								model.nr.str , "to automateConquestModel ..." ) )
				stopifnot ( is.data.frame ( dataset ) )
				
				eatTools:::sunk ( paste ( einr , "dat = " , "'data.frame': " , nrow ( dataset ) , " obs. of " , ncol ( dataset ) , " variables" , sep = "" ) )
				stopifnot ( is.character ( id.name ) )
				eatTools:::sunk ( paste ( "cat('" , einr , "ID = " , id.name , "\\n')" , sep = "" ) )
				eatTools:::sunk ( paste ( einr , "regression = " , paste(regression, collapse=", ") , sep = "" ) )				
				eatTools:::sunk ( paste ( einr , "DIF = " , dif , sep = "" ) )								
				eatTools:::sunk ( paste ( einr , "group.var = " , paste ( group , collapse = ", " ) , sep = "" ) )				
				eatTools:::sunk ( paste ( einr , "weight = " , paste ( weight , collapse = ", " ) , sep = "" ) )				
				stopifnot ( is.character ( group ) | is.null ( group ) )
				stopifnot ( is.character ( item.names ) )
				eatTools:::sunk ( paste ( einr , "items = " , "chr [1:" , length ( item.names ) , "] " , sep = "" ) )	
				stopifnot ( is.data.frame ( person.grouping ) )			
				eatTools:::sunk ( paste ( einr , "person.grouping = " , "'data.frame': " , nrow ( person.grouping ) , " obs. of " , ncol ( person.grouping ) , " variables" , sep = "" ) )
				stopifnot ( is.data.frame ( item.grouping )	)			
				eatTools:::sunk ( paste ( einr , "item.grouping = " , "'data.frame': " , nrow ( item.grouping ) , " obs. of " , ncol ( item.grouping ) , " variables" , sep = "" ) )
				stopifnot ( is.character ( m.model ) )
				eatTools:::sunk ( paste ( einr , "m.model = " , m.model , sep = "" ) )
				stopifnot ( is.character ( analyse.name ) )
				eatTools:::sunk ( paste ( einr , "jobName = " , analyse.name , sep = "" ) )
				stopifnot ( is.character ( folder ) )
				eatTools:::sunk ( paste ( einr , "jobFolder = " , folder , sep = "" ) )
				if ( is.null ( additionalSubFolder )) aSF_str <- "" else aSF_str <- paste ( additionalSubFolder , collapse = ", " )
				eatTools:::sunk ( paste ( einr , "subFolder = " , aSF_str , sep = "" ) )
				eatTools:::sunk ( paste ( einr , "dataName = " , data.name , sep = "" ) )
				if ( is.null ( anchor ) ) anch_str <- "" else anch_str <- paste ( "'data.frame': " , nrow ( anchor ) , " obs. of " , ncol ( anchor ) , " variables" , sep = "" )
				eatTools:::sunk ( paste ( einr , "anchor = " , anch_str , sep = "" ) )				
				stopifnot ( is.null ( conquestParameters$pathConquest ) | is.character ( conquestParameters$pathConquest ) )
				eatTools:::sunk ( paste ( "cat('" ,  einr , "compute.fit = " , conquestParameters$compute.fit , sep = "" , "\n')" ) )
				eatTools:::sunk ( paste ( "cat('" ,  einr , "model.statement = " , conquestParameters$model.statement , sep = "" , "\n')" ) )								
				eatTools:::sunk ( paste ( "cat('" , einr , "pathConquest = " , conquestParameters$pathConquest , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "method = " , conquestParameters$method , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "std.err = " , conquestParameters$std.err , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "distribution = " , conquestParameters$distribution , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "n.plausible = " , conquestParameters$n.plausible , sep = "" , "\n')" ) )
				eatTools:::sunk ( paste ( "cat('" ,  einr , "set.constraints = " , conquestParameters$set.constraints , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "nodes = " , conquestParameters$nodes , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "p.nodes = " , conquestParameters$p.nodes , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "f.nodes = " , conquestParameters$f.nodes , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "n.iterations = " , conquestParameters$n.iterations , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "converge = " , conquestParameters$converge , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "deviancechange = " , conquestParameters$deviancechange , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "seed = " , conquestParameters$seed , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "allowAllScoresEverywhere = " , conquestParameters$allowAllScoresEverywhere , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "equivalence.table = " , conquestParameters$equivalence.table , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "use.letters = " , conquestParameters$use.letters , sep = "" , "\n')" ) )				
				eatTools:::sunk ( paste ( "cat('" ,  einr , "checkLink = " , conquestParameters$checkLink , sep = "" , "\n')" ) )				
				if(!is.null(names(conquestParameters$export)))   { string.s <- paste(names(conquestParameters$export), "=", conquestParameters$export) } else { string.s <- conquestParameters$export }
				eatTools:::sunk ( paste ( "cat('" ,  einr , "export = " , paste(string.s , collapse = ", " ), "\n')" ) )
				eatTools:::sunk ( "cat('\n\n')" )
				
				# Übergabe an automateConquestModel speichern fürs debuggen
				eval ( parse ( text = 
						"dat = dataset ;
						ID = id.name ;
						regression = regression ;
						DIF = dif ;
						group.var = group ;
						weight = weight ;
						items = item.names;
						person.grouping = person.grouping ;
						item.grouping = item.grouping ;
						m.model = m.model ;
						jobName = analyse.name ;
						jobFolder = folder ;
						subFolder = additionalSubFolder ;
						dataName = data.name ; 
						anchor = anchor ;
						compute.fit = conquestParameters$compute.fit ;
						model.statement = conquestParameters$model.statement ;
						pathConquest = conquestParameters$pathConquest ;			
						method = conquestParameters$method ;			
						std.err = conquestParameters$std.err ;			
						distribution = conquestParameters$distribution ;			
						n.plausible = conquestParameters$n.plausible ;
						set.constraints = conquestParameters$set.constraints ;			
						nodes = conquestParameters$nodes ;			
						p.nodes = conquestParameters$p.nodes ;			
						f.nodes = conquestParameters$f.nodes ;			
						n.iterations = conquestParameters$n.iterations ;			
						converge = conquestParameters$converge ;			
						deviancechange = conquestParameters$deviancechange ;			
						seed = conquestParameters$seed ;			
						allowAllScoresEverywhere = conquestParameters$allowAllScoresEverywhere ;			
						equivalence.table = conquestParameters$equivalence.table ;			
						use.letters = conquestParameters$use.letters ;
						checkLink = conquestParameters$checkLink ;
						export = conquestParameters$export"
				) )

				save ( list = c (
						"dat",
						"ID",
						"regression",
						"DIF",
						"group.var",
						"weight",
						"items",
						"person.grouping",
						"item.grouping",
						"m.model",
						"jobName",
						"jobFolder",
						"subFolder",
						"dataName",
						"anchor",
						"compute.fit" ,
						"model.statement" ,
						"pathConquest" ,
						"method" ,
						"std.err" ,
						"distribution" ,
						"n.plausible" ,
						"set.constraints" ,
						"nodes" ,
						"p.nodes" ,
						"f.nodes" ,
						"n.iterations" ,
						"converge" ,
						"deviancechange" ,
						"seed" ,
						"allowAllScoresEverywhere" ,
						"equivalence.table" ,
						"use.letters" ,
						"checkLink" ,
						"export"
						)
				, file = file.path ( folder , paste ( analyse.name , "_autConMod_par.Rdata" , sep = "" ) ) )
			
				# automateConquestModel aufrufen
				#ret <- TRUE
				ret <- automateConquestModel ( dat = dataset ,
										ID = id.name ,
										regression = regression ,
										DIF = dif ,
										group.var = group ,
										weight = weight ,
										items = item.names,
										person.grouping = person.grouping ,
										item.grouping = item.grouping ,
										m.model = m.model ,
										jobName = analyse.name ,
										jobFolder = folder ,
										subFolder = additionalSubFolder ,
										dataName = data.name , 
										anchor = anchor ,
										compute.fit = conquestParameters$compute.fit , 
										model.statement = conquestParameters$model.statement ,
										pathConquest = conquestParameters$pathConquest ,			
										method = conquestParameters$method ,			
										std.err = conquestParameters$std.err ,			
										distribution = conquestParameters$distribution ,			
										n.plausible = conquestParameters$n.plausible , 
										set.constraints = conquestParameters$set.constraints ,			
										nodes = conquestParameters$nodes ,			
										p.nodes = conquestParameters$p.nodes ,			
										f.nodes = conquestParameters$f.nodes ,			
										n.iterations = conquestParameters$n.iterations ,			
										converge = conquestParameters$converge ,			
										deviancechange = conquestParameters$deviancechange ,			
										seed = conquestParameters$seed ,			
										allowAllScoresEverywhere = conquestParameters$allowAllScoresEverywhere ,			
										equivalence.table = conquestParameters$equivalence.table ,			
										use.letters = conquestParameters$use.letters ,
										checkLink = conquestParameters$checkLink ,
										export = conquestParameters$export , 									
										)				

				if ( ret ) eatTools:::sunk ( paste ( ".automateModels.createModel.create:" , model.nr.str ,
										"successfully generated.", "\n\n" ) )
				else { 
						eatTools:::sunk ( paste ( ".automateModels.createModel.create:" , model.nr.str ,
										"NOT generated.", "\n" , "Skript stoppt." , "\n\n" ) )
						stop()
					 }
		}

		# returnen 
		return ( ret )
}

		
.automateModels.createModel <- function ( model.specs , additionalSubFolder ) {
		
		eatTools:::sunk ( paste ( ".automateModels.createModel: Generating model files ... " , "\n\n" , sep = "" ) )
		
		check <- mapply ( .automateModels.createModel.create , 
								software = model.specs$software ,
								dataset = model.specs$dataset,
								id.name = model.specs$id.name ,
								item.names = model.specs$item.names ,
								item.grouping = model.specs$item.grouping ,
								person.grouping = model.specs$person.grouping ,
								i.model = model.specs$i.model , 
								p.model = model.specs$p.model , 						
								m.model = model.specs$m.model , 
								dif = model.specs$dif ,
								weight = model.specs$weight ,
								regression = model.specs$regression ,
								anchor = model.specs$anchor ,
								analyse.name = model.specs$analyse.name ,
								data.name = model.specs$data.name ,
								folder = model.specs$folder	, 
								group = model.specs$group ,
								conquestParameters = model.specs$conquestParameters ,
								MoreArgs = list ( all.analyse.name = model.specs$analyse.name , additionalSubFolder = additionalSubFolder )
						)		

		# Plausicheck
		if ( ! all ( check ) ) {
				# stop ( paste ( "Es konnten für Modell(e) " , paste ( model.specs$analyse.name[ which ( !check ) ] , collapse = ", " ) , " die Modell-Dateien nicht erzeugt werden." , sep="" ) )
				stop ( paste ( "Model files could not be created for model(s) " , paste ( model.specs$analyse.name[ which ( !check ) ] , collapse = ", " ) , " ." , sep="" ) )
				ret <- FALSE
		} else { 
				eatTools:::sunk ( paste ( ".automateModels.createModel: All model files successfully created." , "\n\n" , sep ="" ) )
				ret <- TRUE
		}

		return ( ret )		
		
}



