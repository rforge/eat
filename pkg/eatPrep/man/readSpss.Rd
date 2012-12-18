\name{readSpss}
\alias{readSpss}

\title{
Read SPSS Data Files and Truncate Space in String Variables and Change Column Width
}

\description{
Read SPSS Data Files and Truncate Space in String Variables and Change Column Width
}

\usage{
readSpss(file, correctDigits = FALSE, truncateSpaceChar = TRUE, oldID = NULL, newID = NULL)
}

\arguments{
  \item{file}{
 Name of the SPSS data file to be read in
 }
  \item{correctDigits}{
 Logical: whether values should be transformed to have uniform width in each column, see 'Details'.
}
  \item{truncateSpaceChar}{
  Logical: whether string variables should be trimmed to remove leading and trailing spaces.
}
  \item{oldID}{
  Optional: A character string containing the ID name in the original SPSS dataset.
}
  \item{newID}{
  Optional: A character string containing the ID name after reading in the data.
}
}
\details{
If \code{correctDigits=TRUE}, the values in each column are transformed to have 
  uniform width by adding leading zeros. The width of a column is determined by the 
  longest value in this column, e.g., if a column contains the values \code{1, 10, 100} the transformed column will be \code{001, 010, 100}.This can be useful if the  values had leading zeros which were removed by reading in the SPSS file.
  
If \code{oldID} and \code{newID} are used, the ID variable in the SPSS dataset will be changed to \code{newID} when the file is read.  
}
\value{
  A data frame with trimmed character variables and corrected values (if specified). 
  All columns are of mode \code{character}.
}
\author{
Nicole Haag, Sebastian Weirich
}
