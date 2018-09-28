\name{fdz}
\alias{fdz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{generate SPSS syntax to make individual data anonymous}
\description{Data from large-scale assessments often have to be non-identifiable
on individual level. Function summarizes low-sized categories of polytomous variables
to more general categories. Additionally, character variables are transformed into
numeric factors, providing factor levels als value labels.}
\usage{
fdz ( fileName, boundary = 5, saveFolder = NA, nameListe = NULL, nameSyntax = NULL, exclude = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
%%     ~~Describe \code{file} here~~
Character string of the SPSS file
}
  \item{boundary}{
%%     ~~Describe \code{file} here~~
Integer number: categories with less observations than [boundary] will be summarized to guarantee anonymity
}
  \item{saveFolder}{
%%     ~~Describe \code{file} here~~
Character string of the target folder for SPSS syntax output. Target folder must have writing permission.
}
  \item{nameListe}{
%%     ~~Describe \code{file} here~~
File name of the csv-type variable information file
}
  \item{nameSyntax}{
%%     ~~Describe \code{file} here~~
File name of the SPSS syntax file
}
  \item{exclude}{
%%     ~~Describe \code{file} here~~
Optional: character vector of variable which should be excluded from summarizing and transformation
}
}
\value{
SPSS syntax snippet
}
\author{
Sebastian Weirich
}
\examples{
\dontrun{
dat    <- data.frame ( foreign::read.spss ( "c:/diskdrv/Winword/Psycho/IQB/Daten/LV2016/BS_LV_Primar_2016_Matchingvorlaeufig_09_erweiterteGadsversion.sav", to.data.frame = FALSE, use.value.labels = FALSE, use.missings = TRUE))
classes<- sapply(dat, class)
nCat   <- sapply(dat, FUN = function ( x ) { length(unique(x))})
exclude<- intersect(which(classes=="factor"), which(nCat>90))
exclude<- colnames(dat)[exclude]
syntax <- fdz(fileName = "c:/diskdrv/Winword/Psycho/IQB/Daten/LV2016/BS_LV_Primar_2016_Matchingvorlaeufig_09_erweiterteGadsversion.sav", saveFolder = "c:/diskdrv/Winword/Psycho/IQB/temp/20_fdz", nameListe = "liste2.csv", nameSyntax = "syntax2.txt", exclude=exclude)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
