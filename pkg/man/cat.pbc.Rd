\name{cat.pbc}
\alias{cat.pbc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cat.pbc
}
\description{
Calculate Item Discrimination for Each Category of Categorical Variables
}
\usage{
cat.pbc(datRaw, datRec, idRaw, idRec, context.vars, values, subunits, xlsx = NULL)
}

\arguments{
  \item{datRaw}{
  unrecoded dataset
}
  \item{datRec}{
  the same datset as datRaw, in recoded form
}
  \item{idRaw}{
    Name or column number of the identifier (ID) variable in unrecoded dataset
}
  \item{idRec}{
    Name or column number of the identifier (ID) variable in recoded dataset
}
  \item{context.vars}{
    Names or column numbers of one or more context variables (e.g., sex, school). \code{cat.pbc} will ignore these variables.
}

  \item{values}{
  input table values
}
  \item{subunits}{
  input table subunits
}
  \item{xlsx}{
  full path of excel to be written
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

returns a data.frame with category discrimination
}

\references{

}
\author{
Nicole Haag
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
data(inputDat)
data(inputList)

datRaw <- mergeData(newID = "idstud", datList = inputDat, addMbd = T)
datRec <- recodeData(datRaw, values = inputList$values, subunits=inputList$subunits)
pbcs   <- cat.pbc(datRaw, datRec, idRaw = "idstud", idRec = "idstud", context.vars = "hisei",
                  values = inputList$values, subunits = inputList$subunits)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
