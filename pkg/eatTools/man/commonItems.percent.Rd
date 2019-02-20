\name{commonItems.percent}
\alias{commonItems.percent}
\title{
Identify Percentage of Common Items for Several Groups
}
\description{
This function calculates the percentage of items that have been administered to two groups of persons.
}
\usage{
commonItems.percent(dat, group.var, na = NA, xlsx = NULL)
}
\arguments{
  \item{dat}{A data frame with item responses and a grouping variable.}
  \item{group.var}{Name or column number of the group variable in \code{dat}}  
  \item{na}{A character string indicating which value should be considered as not administered (missing by design)}
  \item{xlsx}{Optional: Full path of Excel file for results.
	}
}

\details{
\code{dat} must only contain the group variable and the items, if further variables 
are in \code{dat} they are treated as items. If \code{group.var} specifies more than two groups,
pairwise group comparisons are performed. 
}

\value{
returns a data frame with common item percentage(s)
}

\author{Martin Hecht}

\seealso{\code{\link{commonItems}}}

\examples{
data(science1)
d <- science1[ , c("version", science1.items)]
commonItems.percent(dat = d, group.var = "version", na = "mbd")
}
