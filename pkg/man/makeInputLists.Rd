\name{makeInputLists}
\alias{makeInputLists}
\alias{makeInputCheckData}
\alias{makeInputRecodeData}
\alias{makeInputAggregateData}

\title{
Generate Input Lists for Functions \code{checkData}, \code{recodeData} and \code{aggregateData}}
\description{Transforms information given in \code{values}, \code{subunits} and \code{units} in a format that is used by  \code{checkData}, \code{recodeData} and \code{aggregateData}.
}
\usage{
makeInputLists(values, subunits, units, recodedData = TRUE)
makeInputCheckData(values, subunits, units)
makeInputRecodeData(values, subunits)
makeInputAggregateData(subunits, units, recodedData = TRUE)
}
%- maybe also usage for other objects documented here.
\arguments{
  \item{values}{A data frame with code information. See Details.}
  \item{subunits}{A data frame with subunit information. See Details.}
  \item{units}{A data frame with unit information. See Details.}
  \item{recodedData}{Logical indicating whether colnames in dataset to aggregate are the subunit names (as in \code{subunits$subunit}) or recoded subunit names (as in \code{subunits$subunitRecoded}). Default is \code{TRUE}, meaning that colnames are recoded subitem names. This parameter is only relevant when input for \code{aggregateData} is generated.}

}
\details{
This function generates specific inputs for the data preparation functions \code{checkData}, \code{recodeData} and \code{aggregateData}. It is largely for internal use of these functions, who call their respective version. 

Examples of data frames \code{values}, \code{subunits} and \code{units} can be found via \code{data(inputLists)}.
}
\value{A list with several of the following entries (depending on which version of the function is called): 
  \item{varinfoRaw}{A list with information about variables and their values expected in raw data.}
  \item{varinfoRecoded}{A list with information about variables and their values expected in recoded data.}
  \item{varinfoAggregated}{A list with information about variables and their values expected in aggregated data.}  
  \item{recodeinfo}{A list with information needed for recoding of data.}  
  \item{aggregateinfo}{A list with information needed for aggregation of data.}
 }
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicole Haag
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(inputList)
lists <- makeInputLists(inputList$values, inputList$subunits, inputList$units, recodedData = TRUE)
str(lists)

}
% Add one or more standard keywords, see file KEYWORDS in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
