# Change-Log
# 08.08.2011 MH: auf stable gesetzt wegen besserer sourcebarkeit

wo.sind <- function(a,b,quiet=FALSE)
           {b <- data.frame(1:length(b),b,stringsAsFactors=FALSE)               ### zusaetzliche Syntaxbefehle sind notwendig, damit die Funktion mit missing values umgehen kann.
            if(sum(which(is.na(a)))>0)     {cat("a enthaelt missing values. \n")}
            if(sum(which(is.na(b[,2])))>0) {cat("b enthaelt missing values. \n")}
            if(length(na.omit(a)) > length(unique(na.omit(a))))     {cat("a enthaelt mehrfach vorhandene Elemente. \n")}
            if(length(intersect(a,b[,2])) == 0) {cat("Keine uebereinstimmenden Elemente gefunden. \n")}
            if(quiet==FALSE) { if(length(intersect(a,b[,2])) > 0) {if(length(setdiff(a,b[,2]))>0)      {cat("Nicht alle Elemente in a sind in b enthalten. \n")} } }
            a <- unique(a)
            if(sum(which(is.na(a)))>0)     {a <- a[-which(is.na(a))]}           ### Sofern vorhanden, werden missing values aus a entfernt
            b <- na.omit(b)                                                     ### Sofern vorhanden, werden missing values aus b entfernt; aber: Rangplatz der
            reihe <- NULL                                                       ### der nicht fehlenden Elemente in b bleibt erhalten
            if(length(a)>0) {for (i in 1:length(a))
                                 {reihe <- c(reihe,b[,1][a[i]==b[,2]])}
                             if(quiet==FALSE) { cat(paste("Es wurden",length(reihe),"Elemente gefunden.")); cat("\n") }}
            if(length(a)==0) {cat("a enthaelt keine gueltigen Werte.\n")}
            return(reihe)}