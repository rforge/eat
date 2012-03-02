get.history <- function(historyfile, shw.object)
               {## if(!exists("recode")) {library(car)}
 				if (missing(shw.object))  {
                   cat("No shw-Object specified. Columns of historyfile will not be labeled.\n")
                }
                input           <- read.table(historyfile, header = FALSE, sep = "\t", dec=".")
                while(length(table(input[,ncol(input)])) == 0) {input <- input[,-ncol(input)]}                   
                colnames(input)[1:2] <-  c("iteration","deviance")              ### obere Zeile: letzte Spalte ausschließlich NA? wenn ja, entferne sie!
                if (!missing(shw.object))  {                                    ### alles ist fertig eingelesen; jetzt müssen nur noch die Spalten sinnfällig benannt werden!
                    colnames(input)[ (ncol(input) - nrow(shw.object[[1]]) + 1 ) : ncol(input) ] <- shw.object[[1]]$item.name
                    colDimensions     <- grep("coef", colnames(shw.object$regression))
                    nameDimensions    <- gsub("coef_","",colnames(shw.object$regression)[colDimensions])
                    colnames(input)[ 3 : (2 + nrow(shw.object$regression) * ((ncol(shw.object$regression)-2) / 2) )] <- paste( rep(shw.object$regression[,1], 2), rep(nameDimensions, each = nrow(shw.object$regression) ) , sep = "_")
                    korMatrix         <- shw$cov.structure                      ### lese Korrelationsmatrix ein!
                    colnames(korMatrix)[-1] <- korMatrix[1:(nrow(korMatrix)-1),1]
                    korMatrix.dimNames <- korMatrix
                    for (rows in 1:nrow(korMatrix.dimNames))   {
                         for (cols in 2:ncol(korMatrix.dimNames))  {
                              korMatrix.dimNames[rows,cols] <- paste(korMatrix[rows,1],colnames(korMatrix)[cols],sep="_")
                         }
                    }
                    rowFinalDeviance <- which(round(input$deviance, digits = 2) == round(shw.object$final.deviance, digits = 2) )
                    stopifnot ( length(rowFinalDeviance) == 1)
                    unnamedCols <- (1:ncol(input))[-c(1,2,(ncol(input) - nrow(shw.object[[1]]) + 1 ) : ncol(input), (3 : (2 + nrow(shw.object$regression) * ((ncol(shw.object$regression)-2) / 2) )) )]
                    options(warn = -1)                                          ### warnungen aus
                    matchWerte  <- match(round(input[rowFinalDeviance,unnamedCols], digits = 3), car:::recode(sapply(korMatrix, FUN=function(ii) {as.numeric(ii)}), "NA=1000"))
                    options(warn = 0)                                           ### warnungen wieder an
                    colnames(input)[unnamedCols] <- as.matrix(korMatrix.dimNames)[matchWerte]
                }
                return(input)}
				