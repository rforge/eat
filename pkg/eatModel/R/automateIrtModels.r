automateIrtModels <- function ( dat, id, items=NULL, item.grouping = NULL,
        person.grouping = NULL, split = c ( "item.grouping" , "person.groups" ), all.persons = TRUE,
        all.persons.lab = "all",verbose=TRUE, software = c("conquest","tam"), dir=NULL, conquest.folder = NULL )  {
        checkID<- .existsBackgroundVariables(dat = dat, variable=id)
        splat  <- splatter ( item.grouping = item.grouping , person.groups = person.grouping , split = split, all.persons = all.persons , all.persons.lab = all.persons.lab , env = FALSE )
     ### sprechende Ausgaben, wenn 'verbose == TRUE'
        if(verbose == TRUE) { cat(paste("Specification of 'item.grouping' and 'person.grouping' results in ",nrow(splat[[1]])," model(s).\n",sep=""))}
     ### jedes Modell abarbeiten
        models <- apply( splat[["models"]], MARGIN = 1, FUN = function ( m ) {
                  if(verbose == TRUE) {
                     cat(paste("Model ",as.numeric(m[["model.nr"]]),": \n    Model name:           ",splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["model.name"]],"\n    Number of items:      ",
                         nrow(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]]), "\n    Number of persons:    ", nrow(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["person.grouping"]]),
                         "\n    Number of dimensions: ",ncol(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]])-1,"\n",sep=""))
                  }
     ### Items im Datensatz selektieren: Achtung: wenn keine Items in "item.grouping", uebergebe "items"-Argument, wenn das auch NULL --> Abbruch!
        if(!is.null(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]][[1]])) {
           itemMis<- setdiff ( splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]][,1], colnames(dat))
           if( length ( itemMis ) > 0) {
               cat(paste( "Warning: ",length(itemMis) ," from ",nrow(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]])," items not found in data.\n",sep=""))
           }
           itemSel<- intersect ( splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]][,1], colnames(dat))
           qMatrix<- splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["item.grouping"]]
        }  else  {
           if(is.null(items)) { stop(paste("Model ",as.numeric(m[["model.nr"]]),": no items defined.\n",sep=""))}
           itemSel<- items
           qMatrix<- NULL
        }
     ### Personen im Datensatz selektieren
        persMis<- setdiff ( splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["person.grouping"]][,1], dat[,id])
        if( length ( persMis ) > 0) {
            cat(paste( "Warning: ",length(persMis) ," from ",nrow(splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["person.grouping"]])," persons not found in data.\n",sep=""))
        }
        persons<- intersect ( splat[["models.splitted"]][[as.numeric(m[["model.nr"]])]][["person.grouping"]][,1], dat[,id])
        datSel <- dat[match(persons, dat[,id]),]
     ### Unterverzeichnisse definieren
        if(is.null(dir)) { dirI <- NULL }  else  { dirI   <- file.path(dir, substring(m[["model.subpath"]],3)) }
        nameI  <- m[["model.name"]]
     ### 'defineModel' aufrufen
        if(inherits(try( modI   <- defineModel ( dat = datSel, items = itemSel, id=id, qMatrix=qMatrix, software = software, conquest.folder=conquest.folder,  analysis.name = nameI, dir = dirI ) ),"try-error"))  {        
           modI <- NULL
        }   
        cat("\n===============================================\n\n")
        return(modI)})
     }


#############
### TESTS ###
#############


#library(eatModel)
#data(sciences)

##### first reshape the data set into wide format

#datW <- reshape2::dcast(sciences, id+grade+sex~variable, value.var="value")

#####  second, create the q matrix from the long format data frame

#qMat <- sciences[ which( sciences[,"subject"] == "biology") ,c("variable","domain")]
#qMat <- qMat[!duplicated(qMat[,1]),]
#qMat <- data.frame ( qMat[,1,drop=FALSE], knowledge = as.numeric(qMat[,"domain"] == "knowledge"),
#        procedural = as.numeric(qMat[,"domain"] == "procedural"))


##### person grouping

#pers <- data.frame ( idstud = datW[,"id"] , "group1" = sample ( c ( "cat1" , "cat2" ) , nrow(datW) , replace = TRUE ), "group2" = sample ( c ( "cat1" , "cat2" ) , nrow(datW) , replace = TRUE ) , stringsAsFactors = FALSE )


#mods <- eatModel:::automateIrtModels ( dat=datW, id="id", items=NULL, item.grouping = qMat, person.grouping = pers,
#        software = "conquest", conquest.folder = "C:/programme/conquest/console_Feb2007.exe",
#        dir = "c:/diskdrv/Winword/Psycho/IQB/Temp/eat")

#class(mods)
#length(mods)        ##### entspricht Anzahl der Modelle
#mods[[1]]           ##### Rueckgabe von 'defineModel' fuer das erste Modell


##### um eins dieser Modelle laufen zu lassen:

#run1  <- runModel(mods[[1]])

