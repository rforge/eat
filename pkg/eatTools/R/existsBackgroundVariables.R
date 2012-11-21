.existsBackgroundVariables <- function(dat, variable )  {
                             if(!is.null(variable))  {
            								 if(is.character(variable))  {
            									 misVariable <- setdiff(variable, colnames(dat))
            									 if(length(misVariable)>0) {cat(paste("Can't find ",length(misVariable)," variable(s) in dataset.\n",sep=""))
            									 cat(paste(misVariable,collapse=", ")); cat("\n"); stop()}
            									 varColumn <- match(variable, colnames(dat))
            								 }
            								 if(is.numeric(variable))   {varColumn <- variable}
                                             return(colnames(dat)[varColumn])
            							 }
                             if(is.null(variable)) {return(NULL)}
                             }
