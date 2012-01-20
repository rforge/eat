####################################################################################################################
#
# function:
#     .automateModels.set.item.grouping.default ( dataset, item.names, item.grouping.default.name = "all.i" )
#
# description:
#     erzeugt default item.grouping (alle Items werden beibehalten)
#
# arguments:
#      dataset (data.frame) : Datensatz
#      item.names (character-Vektor) : Vektor mit Namen der Item-Namen
#      item.grouping.default.name (character Skalar)
#
# Version: 	0.2.0
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



.automateModels.set.item.grouping.default <- function ( dataset, item.names, item.grouping.default.name = "all.i" ){

	   # Plausichecks data.frame
	   stopifnot ( class ( dataset ) == "data.frame" )
	   stopifnot ( !identical ( dataset , data.frame() ) )
	   
	   # Plausichecks item.names
	   stopifnot ( length ( unique(item.names) ) > 0 )
	   stopifnot ( class ( item.names ) == "character" )
     # keine doppelten Namen
     stopifnot ( !any ( duplicated ( item.names ) ) )
     # Item.names und Colnames müssen identisch sein, sonst Abbruch
     stopifnot ( all (item.names %in% colnames(dataset)))
     # Warnung wenn nicht alle item.names in Colnames
	   #if(length(setdiff(item.names, colnames(dataset))) != 0) {
		 #   print(paste("Folgende Variablen aus item.names nicht in Datensatz, werden aber trotzdem in item.grouping aufgenommen:")) # ?????? oder nicht?
		 #   print(paste(setdiff(item.names, names(dataset)), sep="")) }
		    
     # Ausgabe item.grouping (data.frame)
     item.grouping.default <- data.frame(as.character(unique(item.names)), as.numeric(c(rep(1, length(unique(item.names))))), stringsAsFactors=FALSE)
     colnames(item.grouping.default) <- c("item", item.grouping.default.name)
     
     return(item.grouping.default)

 }