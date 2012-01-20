# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make.q3
# Change Log:
# 2011-12-12 SW/MH
# NEW: make.q3
# 0000-00-00 AA
#
# Version: 0.2.0 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

make.q3 <- function ( results , model.specs ) { 
    funVersion <- "make.q3_0.2.0"

	.fun1 <- function ( results , analyse.name , model.specs ) {

	
			.fun2 <-  function ( results , dim.name , model.specs , analyse.name ) {

					.fun3 <-  function ( results , group.name , model.specs , analyse.name , dim.name ) {

							analyse.ind <- which ( unlist ( model.specs$analyse.name ) %in% analyse.name )
							stopifnot ( !identical ( analyse.ind , integer(0) ) )
							if(model.specs$q3[[ analyse.ind ]] ) {
									item.grouping <- model.specs$item.grouping[[analyse.ind]]
									dim.ind <- which ( colnames ( item.grouping ) %in% dim.name )
									stopifnot ( !identical ( dim.ind , integer(0) ) )
									item.grouping <- item.grouping[,c(1,dim.ind)]
									item.vec <- item.grouping[ item.grouping[,2] == 1 , 1 ]
									id.d <- model.specs$dataset[[analyse.ind]][,model.specs$id[[analyse.ind]]]
									sub.data <- model.specs$dataset[[analyse.ind]][,item.vec]
									sub.data <- data.frame(sapply(sub.data, FUN=function(ii) {as.numeric(ii)}),stringsAsFactors=FALSE)
									results2 <- list ( results )
									names(results2) <- group.name							
									results2 <- list ( results2 )
									names(results2) <- dim.name
									results2 <- list ( results2 )
									names(results2) <- analyse.name

									### Folgende drei Zeilen: es geht darum, die richtige Reihenfolge zu haben
									b <- get.item.par(results2)[,"b"]
									names (b) <- get.item.par(results2)[,"item"]
									b <- unname ( b[item.vec] )   ### REIHENFOLGE!
							
									ppar <- "eap"
									theta <- as.numeric( get.person.par(results2)[,ppar] )
									id.theta <- get.person.par(results2)[,"person"]
									stopifnot(identical(sort(id.d),sort(id.theta)))
									
									### Warum matchen? das Statement "pid" in Conquest sortiert Personen im Output (WLE und PV)
									### Damit stimmen Faelle im	Output nicht mehr mit Faellen im Datensatz ueberein. 
									### Q3 schlaegt fehl. Muss vorab gematcht werden!
									res.q3 <- yen.q3( dat = sub.data[na.omit(match(id.theta,id.d)),] , theta = theta , b = b )
									res.q3 <- .q3.to.structure(res.q3)
							
									.fun4 <- function ( results , item.name , res.q3 ) {
											results$q3 <- res.q3[[item.name]]
											return ( results )
									}
									orig.names <- names ( results$item )
									results$item <- mapply ( .fun4 , results$item , names ( results$item ) , MoreArgs = list ( res.q3 ) , SIMPLIFY=FALSE )
									names ( results )[1] <- "item"
									names ( results$item ) <- orig.names
									
									### gibt Liste zurück mit so vielen Elementen wie es Dimesionen gibt
									### jede Dimension eine Liste mit so vielen Elementen, wie es Items auf dieser Dimension gibt
									### jedes Item eine List mit so vielen Elementen, mit wievielen anderen items dieses Item Residualkorrelationen hat

									### Die Liste muß nun eingestampft werden, die oberste Ebene reduziert (unlist ... )
									### "write item output list" muß dann den entsprechenden Listeneintrag des jeweiligen Items suchen
									### und die Liste mit Residuen eintragen. zu welcher Dimension das Item gehört, ist erstmal unerheblich,
									### da die Trennung später erfolgt (wo Komplettstruktur zusammengeschustert wird)
							}		
							return ( results )
					}
					mapply ( .fun3 , results , names ( results ) , MoreArgs = list ( model.specs , analyse.name , dim.name ) , SIMPLIFY=FALSE )
	
			}
			mapply ( .fun2 , results , names ( results ) , MoreArgs = list ( model.specs , analyse.name ) , SIMPLIFY=FALSE )

	}
	fertig <- mapply ( .fun1 , results , names ( results ) , MoreArgs = list ( model.specs ) , SIMPLIFY=FALSE )
	return ( fertig )
}






