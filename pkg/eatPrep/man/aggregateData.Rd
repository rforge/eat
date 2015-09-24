\name{aggregateData}
\alias{aggregateData}
\title{Aggregate Datasets With Several Kinds of Missing Values}
\description{Aggregate datasets with constraints on missing values}
\usage{
aggregateData(dat, subunits, units, aggregatemissings = NULL, 
    rename = FALSE, recodedData = TRUE, suppressErr = FALSE, 
    recodeErr = "mci", verbose = FALSE)

}
\arguments{
  \item{dat}{A data frame containing the data to be aggregated.}
  \item{subunits}{A data frame with subunit information. See \sQuote{Details}.}
  \item{units}{A data frame with unit information. See \sQuote{Details}.}
  \item{aggregatemissings}{Optional: A symmetrical \emph{n x n} matrix with information on how missing values should be aggregated. If no matrix is given, the default will be used. See 'Examples'.}
  \item{rename}{Logical indicating whether units with only one subunit should be renamed to their unit name? Default is \code{FALSE}.}
  \item{recodedData}{Logical indicating whether colnames in \code{dat} are the subunit names (as in \code{subunits$subunit}) or recoded subunit names (as in \code{subunits$subunitRecoded}). Default is \code{TRUE}, meaning that colnames are recoded subitem names.}
  \item{suppressErr}{Logical indicating whether aggregated cells with \code{err} (see \sQuote{Details}) should be recoded to another value.}
  \item{recodeErr}{Character vector of length 1 indicating to which \code{err} should be recoded. This argument is only evaluated when \code{suppressErr = TRUE} }
  \item{verbose}{Logical. If \code{TRUE} additional information is printed.}
}
\details{
\code{aggregateData} aggregates units in data frames with special consideration of missing values.The aggregation of missing values is specified in the argument \code{aggregatemissings}. The rownames and colnames of this \emph{n x n} matrix correspond to the missing codes in the data (see \code{\link{collapseMissings}} for supported missing values). Additionally, the values \code{vc} (for valid code) and \code{err} (for error) are used. If \code{aggregatemissings} is a data frame, it will be coerced to a matrix with the first column of the data frame being transformed into the rownames of the matrix. A warning will be given if the matrix is not symmetrical. 

\code{aggregateData} combines the subunits one by one, i.e. it aggregates the first two subunits of a unit, then adds the third subunit to the new aggregated variable and continues in this manner until all subunits are aggregated. In every step during the process a value of the first variable (e.g., the aggregated variable from the previous step) is matched with the rownames of \code{aggregatemissings} and the corresponding value of the second variable (e.g., the next subitem to be aggregated) is matched with the colnames of \code{aggregatemissings}. The new value of the aggregated variable will therefore be the value in \code{aggregatemissings[firstVar, secondVar]}.If the value in the final aggregated variable is \code{vc}, either the mean or the sum of subunits will be calculated. The rule given in \code{units$unitAggregateRule} determines which one will be chosen, with \code{SUM} being the default if column \code{units$unitAggregateRule} is empty.

The user can specify combinations of missing values that cannot occur simultaneously in one unit by setting the respective cell in \code{aggregatemissings} to \code{err}. For example, it is unlikely that one subunit is not administered (missing by design, \code{mbd}) and another subunit of the same unit was intentionally left blank by the person working on the test booklet (missing by intention \code{mbi}). Thus, this combination of missing values is defaulted to produce an error (\code{err}) in the aggregated variable. If the aggregation produces \code{err} at any point, it will produce a warning. Values \code{err} can be recoded to a different value by specifying the arguments \code{suppressErr} and \code{recodeErr}. 

Examples of data frames \code{subunits} and \code{units} can be found via \code{data(\link{inputList})}.
}

\value{A data frame with aggregated units and, if \code{rename = TRUE}, renamed subunits.
}

\author{Nicole Haag, Anna Lenski
}
%\note{
%%  ~~further notes~~
%}

\section{Warning}{Missings are only correctly aggregated if their values correspond to the values in \code{aggregatemissings}. \code{aggregateData} does not check for value types or whether codes are valid. Use of \code{checkData} and \code{recodeData} before using \code{aggregateData} is therefore strongly recommended.}

\seealso{
	\code{\link{recodeData}}, \code{\link{checkData}}
}
	
\examples{
data(inputDat)
data(inputList)

dat1 <- inputDat[[1]]  # get first dataset from inputDat

# recode data
datRec <- recodeData(dat1, inputList$values, inputList$subunits)
  
# define matrix for missing aggregation (note: this is the default matrix)
am <- matrix(c(
    "vc" , "mvi", "vc" , "mci", "err", "vc" , "vc" , "err",
    "mvi", "mvi", "err", "mci", "err", "err", "err", "err",
    "vc" , "err", "mnr", "mci", "err", "mir", "mnr", "err",
    "mci", "mci", "mci", "mci", "err", "mci", "mci", "err",
    "err", "err", "err", "err", "mbd", "err", "err", "err",
    "vc" , "err", "mir", "mci", "err", "mir", "mir", "err",
    "vc" , "err", "mnr", "mci", "err", "mir", "mbi", "err",
    "err", "err", "err", "err", "err", "err", "err", "err" ),
    nrow = 8, ncol = 8, byrow = TRUE) 

dimnames(am) <- list(
    c("vc" ,"mvi", "mnr", "mci",  "mbd", "mir", "mbi", "err"),
    c("vc" ,"mvi", "mnr", "mci",  "mbd", "mir", "mbi", "err"))
  
print(am)
  
datAggr <- aggregateData(datRec, inputList$subunits, inputList$units, 
    aggregatemissings = am, rename = TRUE, recodedData = TRUE,
    suppressErr = TRUE, recodeErr = "mci", verbose = TRUE)
}

