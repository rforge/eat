\name{catPbc}
\alias{catPbc}
\title{
Calculate Item Discrimination for Each Category of Categorical Variables
}
\description{
\code{catPbc} calculates discrimination statistics for the categories of categorical variables, i.e. the correlations of each category with the total score on the test. This information can be useful in determining which categories of an item are influencing the overall fit and discrimination in item response scaling applications and/or to find mistakes in recoding.
}

\usage{
catPbc(datRaw, datRec, idRaw, idRec, context.vars = NULL, values, subunits, file.name = NULL, verbose = F)
}

\arguments{
	\item{datRaw}{
		A merged unrecoded dataset
	}
	\item{datRec}{
		The same dataset as in \code{datRaw}, in recoded form 
	}
	\item{idRaw}{
		Name or column number of the identifier (ID) variable in unrecoded dataset
	}
	\item{idRec}{
		Name or column number of the identifier (ID) variable in recoded dataset
	}
	\item{context.vars}{
		Names or column numbers of one or more context variables (e.g., sex, school). \code{catPbc} will ignore these variables.
	}
	\item{values}{
		Data frame with information about values, see \code{\link{inputList}} for details.
	}
	\item{subunits}{
		Data frame with information about subunits, see \code{\link{inputList}} for details.
	}
	\item{file.name}{
  Optional: Full path of csv file for results.
	}
		\item{verbose}{
  logical. If \code{TRUE} additional diagnostics are printed.
	}
}
\details{
The column names of \code{datRaw} and \code{datRec} must be consistent with the names provided by the columns \code{subunit} and \code{subunitRecoded} in data.frame \code{subunits}. Otherwise, \code{catPbc} will fail. 
}
\value{
A data frame with the discrimination values for each category of categorical variables. The data frame contains the following columns:
 \item{item}{Name of unrecoded item}
 \item{cat}{Name of category}
 \item{n}{Number of responses for this item}
 \item{freq}{Absolute frequency of the category}
 \item{freq.rel}{Relative frequency of the category}
 \item{catPbc}{Discrimination value for the category (correlation with total score)}
 \item{recodevalue}{Recode value for the category}
 \item{subunitType}{Type of subunit, see \code{\link{inputList}} for details}
}
\author{
Nicole Haag
}
\examples{
data(inputDat)
data(inputList)

datRaw <- mergeData(newID = "ID", datList = inputDat, addMbd = TRUE)
datRec <- recodeData(datRaw, values = inputList$values, 
    subunits=inputList$subunits)
pbcs   <- catPbc(datRaw, datRec, idRaw = "ID", idRec = "ID", 
    context.vars = "hisei", values = inputList$values, 
    subunits = inputList$subunits)
}
