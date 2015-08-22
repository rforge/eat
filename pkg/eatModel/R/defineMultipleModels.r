defineMultipleModels <- function ( dat, id, items=NULL, item.grouping = NULL,
        person.grouping = NULL, split = c ( "item.grouping" , "person.grouping" ), all.persons = TRUE,
        all.persons.lab = "all",verbose=TRUE, software = c("conquest","tam"), dir=NULL, conquest.folder = NULL )  {
     ### nicht implementiert: wenn 'software == conquest', muss 'dir' angegeben werden
        split  <- recode ( match.arg(split), "'person.grouping'='person.groups'")
        checkID<- .existsBackgroundVariables(dat = dat, variable=id)
        splat  <- splatter ( item.grouping = item.grouping , person.groups = person.grouping , split = split, all.persons = all.persons , all.persons.lab = all.persons.lab , env = FALSE, verbose = FALSE )
     ### sprechende Ausgaben, wenn 'verbose == TRUE'
        if(verbose == TRUE) { cat(paste("Specification of 'item.grouping' and 'person.grouping' results in ",nrow(splat[[1]])," model(s).\n",sep=""))}
     ### jedes Modell abarbeiten
        models <- apply( splat[["models"]], MARGIN = 1, FUN = function ( m ) {
     ### Items im Datensatz selektieren: Achtung: wenn keine Items in "item.grouping", uebergebe "items"-Argument, wenn das auch NULL --> Abbruch!
                  if(!is.null(splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["item.grouping"]][[1]])) {
                     itemMis<- setdiff ( splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["item.grouping"]][,1], colnames(dat))
                     if( length ( itemMis ) > 0) {
                         cat(paste( "Warning: ",length(itemMis) ," from ",nrow(splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["item.grouping"]])," items not found in data.\n",sep=""))
                     }
                     itemSel<- intersect ( splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["item.grouping"]][,1], colnames(dat))
                     qMatrix<- splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["item.grouping"]]
                  }  else  {
                     if(is.null(items)) { stop(paste("Model ",as.numeric(m[["model.no"]]),": no items defined.\n",sep=""))}
                     allVars<- list(variablen=items)
                     all.Nam<- lapply(allVars, FUN=function(ii) {.existsBackgroundVariables(dat = dat, variable=ii)})
                     itemSel<- all.Nam[["variablen"]]
                     qMatrix<- NULL
                  }
     ### Personen im Datensatz selektieren: Achtung: wenn keine Personen in "person.grouping", nimm alle!
                  if(!is.null(splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["person.grouping"]][[1]])) {
                     persMis<- setdiff ( splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["person.grouping"]][,1], dat[,id])
                     if( length ( persMis ) > 0) {
                         cat(paste( "Warning: ",length(persMis) ," from ",nrow(splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["person.grouping"]])," persons not found in data.\n",sep=""))
                     }
                     persons<- intersect ( splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["person.grouping"]][,1], dat[,id])
                     datSel <- dat[match(persons, dat[,id]),]
                  }  else  { datSel <- dat }
     ### Unterverzeichnisse definieren
                  if(is.null(dir)) { dirI <- NULL }  else  { dirI   <- file.path(dir, substring(m[["model.subpath"]],3)) }
                  nameI  <- m[["model.name"]]
     ### sprechende Ausgabe, wenn 'verbose == TRUE'
                  if(is.null (qMatrix)) { nDim <- 1 } else { nDim <- ncol(qMatrix)-1 }
                  txt    <- paste("Model ",as.numeric(m[["model.no"]]),": \n    Model name:           ",splat[["models.splitted"]][[as.numeric(m[["model.no"]])]][["model.name"]],"\n    Number of items:      ",
                            length(itemSel), "\n    Number of persons:    ", nrow(datSel), "\n    Number of dimensions: ",nDim,"\n",sep="")
                  if(verbose == TRUE) { cat(txt) }
     ### 'defineModel' wird nicht aufgerufen, statt dessen Liste zurueckgegeben, die dann 'defineModel' fuer den Mehrmodellfall uebergeben wird
                  ret    <- list ( dat=dat, items = itemSel, id=id, qMatrix=qMatrix, software = software, conquest.folder=conquest.folder,  analysis.name = nameI, dir = dirI, txt = txt )
                  cat("\n===============================================\n\n")
                  return(ret)})
     names(models) <- splat[["models"]][,"model.name"]
     return(models)  }
