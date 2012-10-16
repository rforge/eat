# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .automateModels.writeResultsExcel
# Description: Subroutine von automateModels
# Version: 	0.3.0
# Status: beta
# Release Date: 	2011-10-14
# Author:    Martin Hecht
# Change Log:
#		14.10.2011 MH: Ausgaben auf Englisch
#		08.09.2011 MH: cat durch sunk ersetzt (für Logfile)
#		17.08.2011 MH: auf stable gesetzt wegen besserer Sourcebarkeit
# 		08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit
#			
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.automateModels.writeResultsExcel <- function ( results , analyse.name , folder , folder.aM , additional.item.props , write.xls.results ) {

		# Funktionsname für Meldungen
		f. <- ".automateModels.writeResultsExcel"
		f.n <- paste ( f. , ":" , sep = "" )
		
		# Folder-Vektor mit Analyse-Namen benennen
		names ( folder ) <- analyse.name

		# Ausgabe
		# st <- paste ( "\n" , f.n , " Excels werden geschrieben " , sep = "" )
		# st <- paste ( "\n" , f.n , " Excels are being written " , sep = "" )
		# sunk ( "paste ( f.n , ' Excels are being written ' , sep = '' )" ) 
		if ( write.xls.results ) {
				msg <- "Excels and Rdata Files are being written"
		} else {
				msg <- "Excels Files are being written"
		}
		sunk ( msg ) 
		
		# Gesamt-Excel
		check <- write.results.xlsx ( results , folder.aM , additional.item.props , write.xls.results )
		#check <- TRUE

		# Einzel-Excels
		check <- c ( check ,
			mapply ( function ( results , name , folder , additional.item.props ) {
					sunk ( "cat('.')" )
					flush.console()
					results <- list ( results )
					names ( results ) <- name
				
					write.results.xlsx ( results = results , path = unname ( unlist ( folder[ name ] ) ) , additional_itemprops = additional.item.props , write.xls.results )
		
					# 30.07.12 noch Latente Korrelationen / Varianzen nach Excel
					if ( length ( results[[1]] ) > 1 ) nam <- "CorrCovVar" else nam <- "Var"
					fin <- file.path ( unname ( unlist ( folder[ name ] ) ) , paste ( name , "_" , nam , sep = "" ) )
					if ( write.xls.results ) {
							fin.xlsx <- paste ( fin , ".xlsx" , sep = "" )
					} else {
							fin.xlsx <- NULL
					}
					temp <- get.latent.corr ( unname ( unlist ( folder[ name ] ) ) , xlsx = fin.xlsx )
					fin.Rdata <- paste ( fin , ".Rdata" , sep = "" )
					name2 <- gsub ( "-" , "" , name )
					do <- paste ( name2 , "_" , nam , " <- temp" , sep = "" )
					eval ( parse ( text = do ) ) 
					do <- paste ( "save (" , name2 , "_" , nam , ",file='",fin.Rdata,"')", sep = "" ) 
					eval ( parse ( text = do ) )
					
					# model Informationen
					nam <- "model_info"
					fin <- file.path ( unname ( unlist ( folder[ name ] ) ) , paste ( name , "_" , nam , sep = "" ) )
					if ( write.xls.results ) {
							fin.xlsx <- paste ( fin , ".xlsx" , sep = "" )
					} else {
							fin.xlsx <- NULL
					}
					temp <- compareModels ( unname ( unlist ( folder[ name ] ) ) , xlsx = fin.xlsx )
					fin.Rdata <- paste ( fin , ".Rdata" , sep = "" )
					eval ( parse ( text = paste ( name2 , "_" , nam , " <- temp" , sep = "" ) ) ) 
					do <- paste ( "save (" , name2 , "_" , nam , ",file='",fin.Rdata,"')", sep = "" ) 
					eval ( parse ( text = do ) )					
					
			
			} , results , names ( results ) , MoreArgs = list ( folder , additional.item.props ) , SIMPLIFY = FALSE )
		)
		#stopifnot ( all ( check ) )
		
		# 30.07.12: Gesamt latente Korr
		if ( length ( results ) > 1 ) {
				if ( any ( sapply ( results , function ( r ) { length ( r ) > 1 } ) ) ) nam <- "CorrCovVar" else nam <- "Var"
				fin <- file.path ( folder.aM , ( name <- paste ( "All_" , length ( results ) , "_analyses_" , nam , sep = "" ) ) )
				if ( write.xls.results ) {
						fin.xlsx <- paste ( fin , ".xlsx" , sep = "" )
				} else {
						fin.xlsx <- NULL
				}
				temp <- get.latent.corr ( file.path ( folder.aM , ".." ) , xlsx = fin.xlsx )
				fin.Rdata <- paste ( fin , ".Rdata" , sep = "" )
				eval ( parse ( text = paste ( sub ( "-" , "" , name ) , "_" , nam , "<- temp" , sep = "" ) ) ) 
				do <- paste ( "save (" , sub ( "-" , "" , name ) , "_" , nam , ",file='",fin.Rdata,"')", sep = "" ) 
				eval ( parse ( text = do ) )				
		}

		# Gesamt Model Comparison
		if ( length ( results ) > 1 ) {
				nam <- "model_comparison"
				fin <- file.path ( folder.aM , ( name <- paste ( "All_" , length ( results ) , "_analyses_" , nam , sep = "" ) ) )
				if ( write.xls.results ) {
						fin.xlsx <- paste ( fin , ".xlsx" , sep = "" )
				} else {
						fin.xlsx <- NULL
				}
				temp <- compareModels ( file.path ( folder.aM , ".." ) , xlsx = fin.xlsx )
				fin.Rdata <- paste ( fin , ".Rdata" , sep = "" )
				eval ( parse ( text = paste ( sub ( "-" , "" , name ) , "_" , nam , "<- temp" , sep = "" ) ) ) 
				do <- paste ( "save (" , sub ( "-" , "" , name ) , "_" , nam , ",file='",fin.Rdata,"')", sep = "" ) 
				eval ( parse ( text = do ) )				
		}
		
		# Ausgabe
		sunk ( " done\n" )
		
		# returnen 
		return ( check )
		
}

