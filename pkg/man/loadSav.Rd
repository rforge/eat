\name{loadSav}
\alias{loadSav}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
loadSav
}
\description{
read SPSS data files and change id names, if necessary
}
\usage{
loadSav(path = getwd(), savFiles = NULL, oldIDS, newID, correctDigits = FALSE, truncateSpaceChar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{savFiles}{
%%     ~~Describe \code{savFiles} here~~
}
  \item{oldIDS}{
%%     ~~Describe \code{oldIDS} here~~
}
  \item{newID}{
%%     ~~Describe \code{newID} here~~
}
  \item{correctDigits}{
%%     ~~Describe \code{correctDigits} here~~
}
  \item{truncateSpaceChar}{
%%     ~~Describe \code{truncateSpaceChar} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function ( path=getwd(), savFiles=NULL, oldIDS, newID, correctDigits=FALSE, truncateSpaceChar = TRUE ) {
           funVersion <- "loadSAV_0.0.2"
           if(missing(oldIDS))  {stop(paste("Error in ",funVersion,": 'oldIDS' is missing.\n",sep="")) }   
           if(missing(newID))   {stop(paste("Error in ",funVersion,": 'newID' is missing.\n",sep="")) }
           if(length(newID)!=1) {stop(paste("Error in ",funVersion,": 'newID' has to be of length 1.\n",sep="")) }   
           # if(!exists("read.spss"))  {library(foreign)}
           if(!is.null(savFiles)) {
              fileExists <- file.exists(file.path(path,savFiles))
              if(all(!fileExists)) {
                 stop(paste("Error in ",funVersion,": None of the files specified in 'savFiles' were found in ",path,".\n",sep=""))
              }   
              if(!all(fileExists)) {
                 cat(paste(funVersion,": Following files specified in 'savFiles' were not found in ",path,".\n",sep=""))
                 notFoundFiles <- savFiles[!fileExists]
                 FoundFiles    <- savFiles[fileExists]
                 cat(paste(notFoundFiles,collapse=", ")) 
                 cat("\nOnly found files will be read in.\n")
                 savFiles      <- savFiles[fileExists]
              } 
           }     
           if(is.null(savFiles)) {
              savFiles <- list.files(path=path,pattern=".sav|.SAV",recursive=FALSE) 
              if(length(savFiles)==0) {
                 stop(paste("No '.sav'-files found in ",path,".\n",sep=""))
              }   
           cat(paste(funVersion,": Found ", length(savFiles), " 'savFiles' in ",path,".\n",sep=""))
           }
           ### hier beginnt das eigentliche Einlesen
           allDataFrames <- NULL
           for (i in seq(along=savFiles)) {
                file.i <- data.frame(read.spss(file.path(path,savFiles[i]),to.data.frame=FALSE, use.value.labels=FALSE), stringsAsFactors=TRUE)
                idCol  <- unique(unlist(lapply(oldIDS, FUN=function(ii) {grep(ii,colnames(file.i))})))
                if(length(idCol)<1) {
                   stop(paste("Error in ",funVersion,": None of the specified 'oldIDS' were found in dataset ",savFiles[i],".\n",sep="")) 
                }
                if(length(idCol)>1) {
                   stop(paste("Error in ",funVersion,": More than one of the specified 'oldIDS' were found in dataset ",savFiles[i],".\n",sep="")) 
                }
                colnames(file.i)[idCol] <- newID
                ### Leerzeichen abschnipseln 
                if(truncateSpaceChar == TRUE)  {
                   for (ii in 1:ncol(file.i))  {
                        file.i[,ii] <- crop(file.i[,ii])
                   }     
                }
                ### Stelligkeitskorrektur
                if(correctDigits == TRUE) {
                   colsToCorrect <- lapply(1:ncol(file.i), FUN=function(ii) { sort(unique(nchar(file.i[,ii])))})        
                   options(warn = -1)                                          
                   colsToCorrect <- which( unlist( lapply(colsToCorrect,  FUN=function(ii) { all(ii == c(1,2)) })) ) 
                   options(warn = 0)
                   if(length(colsToCorrect)>0) {
                      cat(paste(funVersion,": ",length(colsToCorrect)," columns are corrected for column width in dataset ",savFiles[i],".\n",sep=""))
                      for (ii in colsToCorrect) {
                           file.i[,ii] <- gsub(" ","0", formatC(as.character(file.i[,ii]),width=2))
                      }
                   }
                }   
                allDataFrames[[i]] <- file.i
           }              
          return(allDataFrames)
          }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
