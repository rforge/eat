####################################################################################################################
#
# function:
#     .automateModels.set.person.grouping.default ( dataset, id.name, person.grouping.default.name = "all.p" )
#
# description:
#     erzeugt default person.grouping (alle Personen werden beibehalten)
#
# arguments:
#      dataset (data.frame) : Datensatz
#      id.name (character Skalar) : Name der ID-Spalte im Datensatz
#      item.grouping.default.name (character Skalar)
#
# Version: 	0.1.0
# Imports:
# Published:
# Author:   Karoline Sachse
# Maintainer:
#
# Change Log:
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
# 22.06.2011 (KS):
#
####################################################################################################################



.automateModels.set.person.grouping.default <- function ( dataset, id.name, person.grouping.default.name = "all.p" ){

	   # Plausichecks data.frame
	   stopifnot ( class ( dataset ) == "data.frame" )
	   stopifnot ( !identical ( dataset , data.frame() ) )
	   
	   # Plausicheck id.name
	   stopifnot ( length ( id.name ) == 1 )
	   stopifnot ( is.character ( id.name ) )
	   stopifnot ( id.name %in% colnames ( dataset ) )
		    
     # Ausgabe item.grouping (data.frame)
     person.grouping.default <- data.frame(as.character(dataset[,id.name]), as.numeric(c(rep(1, length(dataset[,id.name])))), stringsAsFactors=FALSE)
     colnames(person.grouping.default) <- c(id.name, person.grouping.default.name)
     
     return(person.grouping.default)

 }