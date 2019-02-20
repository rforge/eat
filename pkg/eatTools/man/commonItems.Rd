\name{commonItems}
\alias{commonItems}
\title{Identify Common Items for Several Groups
}
\description{
This function identifies sets of items that have been administered to two groups of persons.
}
\usage{
commonItems(dat, group.var, na = NA, uncommon = FALSE, simplify = TRUE)
}
\arguments{

  \item{dat}{A data frame with item responses and a grouping variable.}
  \item{group.var}{Name or column number of the group variable in \code{dat}}  
  \item{na}{A character string indicating which value should be considered as not administered (missing by design)}
  \item{uncommon}{ if \code{TRUE} a vector of items that have only been administered to one of the two groups is additionally returned. }
  \item{simplify}{ if \code{TRUE} a character vector is returned (only in case of 2 groups and \code{uncommon=FALSE}) }
  
}
\details{
\code{dat} must only contain the group variable and the items, if further variables 
are in \code{dat} they are treated as items. If \code{group.var} specifies more than two groups,
pairwise group comparisons are performed. 
}
\value{
returns a list of all \code{group.var} combinations with character vectors of common item names.
If \code{uncommon=TRUE} a vector of uncommon (unique) items of each group is additionally returned.

The names of list elements are the two group names concatenated by "|".
}
\author{Martin Hecht}

\seealso{
\code{\link{commonItems.percent}}
}

\examples{
data(science1)
d <- science1[ , c("version", science1.items)]

# common items are listed for each combination of groups
str(commonItems(dat = d, group.var = "version", na = "mbd"))

# uncommon items are returned as well
str(commonItems(dat = d, group.var = "version", na = "mbd", uncommon = TRUE))
}
