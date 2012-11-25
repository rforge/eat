\name{make.dummies}
\alias{make.dummies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dummy coded variables
}
\description{
creates dummy coded variables using \link[psych]{dummy.code}
names of dummy coded variables can be customized
and added to the input data.frame
}
\usage{
make.dummies ( dat , cols , colname.as.prefix = TRUE , delimiter = "." , capitalize = FALSE , nchar = NULL , add = TRUE , sort.into.dat = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{ data.frame }
  \item{cols}{ colnames of variables to be dummy coded }
  \item{colname.as.prefix}{ logical, if \code{TRUE} the original variable name is added as prefix }
  \item{delimiter}{ logical, if \code{TRUE} (and colname.as.prefix = TRUE) variable name and "level" name are delimited by \code{delimiter} }
  \item{capitalize}{ logical, if \code{TRUE} "levels" are capitalized }
  \item{nchar}{ numeric, if \code{TRUE} "levels" are truncated to length \code{nchar} }
  \item{add}{ logical, if \code{TRUE} dummy coded variables are added to \code{dat} }
  \item{sort.into.dat}{ logical, if \code{TRUE} (and add = TRUE) dummy coded variables are added and sorted into \code{dat} }
}
\details{
}
\value{
returns data.frame with dummy coded variables, depending on \code{add} either the original data.frame (\code{dat}) is appended with new dummy code variables or they are purely returned
}
\references{

}
\author{
Martin Hecht
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
\dontrun{
data(science1)

science1.dum <- make.dummies ( science1 , c("sex","booklet") )
str ( science1.dum[,3:10] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , nchar = 1 )
str ( science1.dum[,3:10] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , delimiter = "_" )
str ( science1.dum[,3:10] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , delimiter = "" , capitalize = TRUE )
str ( science1.dum[,3:10] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , colname.as.prefix = FALSE )
str ( science1.dum[,3:10] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , sort.into.dat = FALSE )
str ( science1.dum[,(ncol(science1.dum)-5):ncol(science1.dum)] )

science1.dum <- make.dummies ( science1 , c("sex","booklet") , add = FALSE )
str ( science1.dum )

}
}
