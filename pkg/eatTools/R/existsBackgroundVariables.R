.existsBackgroundVariables <- function(dat, variable)  {
  if(!is.null(variable)){
    if(is.character(variable))  {
      misVariable <- setdiff(variable, colnames(dat))
      if(length(misVariable) > 0) {
        stop(paste(length(misVariable)," variable(s) are not in dataset: ",
        paste(misVariable, collapse=", "), sep = ""))
      }
      varColumn <- match(variable, colnames(dat))
    }
    if(is.numeric(variable))   {varColumn <- variable}
    return(colnames(dat)[varColumn])
  }
  if(is.null(variable)) {return(NULL)}
}
