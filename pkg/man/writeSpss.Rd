\name{writeSpss}
\alias{writeSpss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Export Datasets to SPSS}
\description{Writes data and SPSS syntax files.}
\usage{
writeSpss(dat, values, subunits, units, 
		filedat = "zkddata.txt", filesps = "readZkdData.sps", 
		missing.rule = list(mvi = 0, mnr = 0, mci = NA, mbd = NA, mir = 0, mbi = 0), 
        path = getwd(), sep = "\t", dec = ",", silent = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{dat}{A data frame}
  
  \item{values}{A data frame with code information. See 'Details'.}
  
  \item{subunits}{A data frame with subunit information. See 'Details'.}
  
  \item{units}{A data frame with unit information. See 'Details'.}
  
  \item{filedat}{A character string with the name of the output data file.}
  
  \item{filesps}{A character string with the name of the output syntax file.}
  
  \item{missing.rule}{A list containing recode information for character missings. See 'References' for description of default values.}
  
  \item{path}{A character string containing the path of the output file. The value in \code{path} is appended to \code{filedat} and \code{filesps}. By default, files are written to the current \R working directory. If \code{path=NULL} then no file path appending is done.}
  
  \item{sep}{The separator between the data fields.}
  
  \item{dec}{The decimal separator for numerical data.}
  
  \item{silent}{A logical flag stating whether the names of the files should be printed.}

}
\details{
This function automates most of the work needed to export a dataset to SPSS. It uses a modified version of \code{writeForeignSPSS()} from the \code{foreign()} package and of \code{mids2spss()} fromt the \code{mice} package. The modified version allows for a choice of the field and decimal separators, makes some improvements to the formatting and provides variable labels and value labels according to the information in the data frames \code{values}, \code{subunits} and \code{units}.

Examples of data frames \code{values}, \code{subunits} and \code{units} can be found on \code{data(inputList)}

The SPSS syntax file has the proper file names and separators set, so in principle it should run and read the data without alteration. SPSS is more strict than \R with respect to the paths. Always use the full path, otherwise SPSS may not be able to find the data file.}
\value{Used for its side effects. The return value is \code{NULL}.}
\references{http://code.google.com/p/zkdlib/wiki/MissingHandling}
\author{Nicole Haag}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}