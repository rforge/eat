\name{readSpss}
\alias{readSpss}

\title{
Read SPSS Data Files and Truncate Space in String Variables and Change Column Width
}

\description{
This function reads SPSS data files using the function \code{read.spss} from the \code{foreign} package and converts all variables to class \code{character}. Additionally, leading and trailing white spaces can be removed from character variables, numeric values can be padded with leading zeros for uniform width of all entries in a column, and identifier variables can be renamed (i.e. when consistently named identifier varables are desired for automated merging of multiple datasets with \code{mergeData}). 
}

\usage{
readSpss(file, correctDigits = FALSE, truncateSpaceChar = TRUE, 
oldID = NULL, newID = NULL)
}

\arguments{
  \item{file}{
 Name of the SPSS data file to be read in
 }
  \item{correctDigits}{
 Logical: whether values should be transformed to have uniform width in each column, see \sQuote{Details}.
}
  \item{truncateSpaceChar}{
  Logical: whether leading and trailing spaces should be removed from character variables.
}
  \item{oldID}{
  Optional: A character string containing the ID name in the original SPSS dataset.
}
  \item{newID}{
  Optional: A character string containing the ID name after reading in the data.
}
}
\details{
If \code{correctDigits=TRUE}, the values in each column are transformed to have uniform width by adding leading zeros. The width of a column is determined by the longest value in this column, e.g., if a column contains the values \code{1, 10, 100} the transformed column will be \code{001, 010, 100}. This can be useful if the  values had leading zeros which were removed by reading in the SPSS file.
  
If \code{oldID} and \code{newID} are used, the ID variable in the SPSS dataset will be changed to \code{newID} when the file is read.  
}
\value{
  A data frame. All columns are of class \code{character}.
}
\author{
Nicole Haag, Sebastian Weirich
}
