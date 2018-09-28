import_spss <- function(filePath) {
  # import
  rawDat <- read_spss(file = filePath)

  # 1) check and prepare variable names
  names(rawDat) <- unlist(lapply(names(rawDat), transf_names))

  # 2) extract labels
  label_df <- extract_labels(rawDat = rawDat, type = "SPSS")

  # 3) strip away labels from rawDat
  plainDat <- data.frame(lapply(rawDat, strip_allLabels), stringsAsFactors = FALSE)

  # output
  list(dat = plainDat, labels = label_df)
}

# 01) Prepare data ---------------------------------------------------------
# function for preparing of variable names
transf_names <- function(vec_name) {
  NewName <- vec_name
  if(identical(vec_name, "group")) {
    NewName <- "groupVar"
    message(paste(vec_name, "has been renamed to", NewName))
  }
  if(grepl("\\.", vec_name)) {
    NewName <- gsub("\\.", "_", vec_name)
    message(paste(vec_name, "has been renamed to", NewName))
  }
  NewName
}


# 02) extract labels ---------------------------------------------------------
# actually 2 functions, but important to keep code @ 1 place
extract_labels <- function(rawDat, old_labels = NULL, type = "SPSS") {
  ## spss version of function
  if(identical(type, "SPSS")) {
    # a) extract variable labels
    var_labels <- extract_varLabels(spss_df = rawDat)
    # b) extract values labels
    val_labels <- extract_valueLabels(df = rawDat, type = type)

    # Merge into one label DF
    label_df <- merge(var_labels, val_labels, all = TRUE)
  }

  ## R version of function
  if(identical(type, "R")) {
    # a) extract values labels from factors
    fac_labels <- extract_valueLabels(df = rawDat, type = type)
    # b) create emtpy df if no variable and value labels so far
    if(is.null(old_labels)) {
      old_labels <- data.frame(matrix(ncol = 4, nrow = 0))
      names(old_labels) <- c("varName", "varLabel", "value", "label")
    }
    # Merge into one label DF
    label_df <- merge(old_labels, fac_labels, all = TRUE)
  }

  label_df
}


# a) ----------- variable labels
extract_varLabels <- function(spss_df) {
  varList <- lapply(spss_df, function(var) attr(var, "label"))
  varLabel_df <- data.frame(names(varList), unlist(varList), stringsAsFactors = F)
  # create empty data frame if no variable labels in sav
  if(is.null(varLabel_df)) varLabel_df <- data.frame(matrix(ncol = 2, nrow = 0))
  # names
  names(varLabel_df) <- c("varName", "varLabel")

  varLabel_df
}


# b) ----------- value labels
# all variables, for SPSS and R
extract_valueLabels <- function(df, type = "SPSS") {
  if(identical(type, "SPSS")) {
    FUN = extract_VL_SPSS
  } else if(identical(type, "R")) {
    FUN = extract_VL_R
  } else stop("Invalid type")

  # extract labels into one long format data frame
  valueList <- Map(FUN, var = df, varName = names(df))
  valLabel_df <- do.call(rbind, valueList)
  # add names to data frame, create emtpy data frame if no labels
  if(is.null(valLabel_df)) valLabel_df <- data.frame(matrix(ncol = 3, nrow = 0))
  names(valLabel_df) <- c("varName", "value", "label")
  valLabel_df
}

# single variable for SPSS
extract_VL_SPSS <- function(var, varName) {
  # check if there are value labels
  if(is.null(attributes(var)$labels)) return(NULL)
  # extract value labels and return as long format df
  df <- data.frame(varName = rep(varName, length(attr(var, "labels"))),
                 value = as.numeric(attr(var, "labels")),
                 label = attr(attr(var, "labels"), "names"),
                 stringsAsFactors = FALSE)
  df
}
# single variable for R (factors!)
extract_VL_R <- function(var, varName) {
  # check if it is a factor
  if(!is.factor(var)) return(NULL)
  # extract value labels
  labels <- levels(var)
  # create corresponding integers
  values <- seq_along(labels)
  df <- data.frame(varName = rep(varName, length(values)),
                   value = values,
                   label = labels)
  df
}