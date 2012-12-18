\name{checkInput}
\alias{checkInput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check Input Data Frames
}
\description{Check input data frames for consistency and replace missing information with default values (if necessary).
}
\usage{
checkInput(values, subunits, units, checkValues = TRUE, checkUnits = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
A data frame with code information. See 'Details'
}
  \item{subunits}{
A data frame with subunit information. See 'Details'
}
  \item{units}{
A data frame with unit information. See 'Details'.
}
  \item{checkValues}{
Logical: Should data frame \code{values} be checked?
}
  \item{checkUnits}{
Logical: Should data frame \code{units} be checked?
}
}
\details{
This function is largely for internal use and is called by \code{makeInputLists} before lists are generated.

Examples of data frames \code{values}, \code{subunits} and \code{units} can be found via \code{data(inputList)}.
}
\value{
A list containing the checked and (if necessary) defaulted input data frames: 
  \item{values}{
Checked data frame with code information. Will be returned if \code{checkValues = TRUE}.
}
  \item{subunits}{
A data frame with subunit information.
}
  \item{units}{
A data frame with unit information. Will be returned if \code{checkUnits = TRUE}.
}


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
\section{Warning}{Function will not check input data frames if \code{checkValues} and \code{checkUnits} are both \code{FALSE}.
} 

\seealso{
\code{\link{makeInputLists}}
}
\examples{
}
