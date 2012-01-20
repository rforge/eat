### This function reads fwf files
### leicht geändert von Weirich, 18. April 2011
read.fiwifo <- function( file , columns.to.delete=NULL, format , variables = NULL){
    ff <- readLines( file )
    if(!is.null(columns.to.delete))                                             ### jetzt rausfinden, welche Spalten beibehalten werden sollen
      {ff <- removeCharsFromString(string=ff, weg=columns.to.delete)}
    ind.ff1 <- c( 1, cumsum(format)[- length(format) ] + 1 )
    ind.ff2 <- cumsum(format)
    I <- length(format)
    n <- length( ff )
    dfr <- data.frame( matrix(0 , nrow= n , ncol=I ) )
    for (ii in 1:I){  dfr[,ii ] <- as.numeric( substring( ff , ind.ff1[ii] , ind.ff2[ii] )  ) }
    if (!is.null(variables)){ colnames(dfr) <- variables }
    return(dfr) }