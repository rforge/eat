### entfernt aus einem String bestimmte Characters
### Bsp.: remove.selection(c("123456789","987654321"), weg=c(1,5,9))
removeCharsFromString   <- function(string, weg=NULL, behalten=NULL)
                      {splitt <- strsplit(string,"")
                       if(!is.null(behalten) & !is.null(weg)) {stop("Logischer Fehler.")}
                       if(is.null(behalten) & is.null(weg)) {stop("Logischer Fehler.")}
                       if(!is.null(weg))      {splitt <- unlist(lapply(1:length(splitt),FUN= function(ii) {paste(splitt[[ii]][-weg],collapse="")})) }
                       if(!is.null(behalten)) {splitt <- unlist(lapply(1:length(splitt),FUN= function(ii) {paste(splitt[[ii]][behalten],collapse="")})) }
                       return(splitt)}