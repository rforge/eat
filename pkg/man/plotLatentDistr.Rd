\name{plotLatentDistr}
\alias{plotLatentDistr}
\title{
plot latent distribution
}
\description{
creates latent distribution plots of two distribution on the same metric (e.g. persons and items)
}
\usage{
plotLatentDistr ( persons , items , pdf = NULL , title = NULL , scale.unit = "Logit" , person.color = "#E69F00" , item.color = "#56B4E9" , alpha = 0.5 )
}
\arguments{
  \item{persons}{ numeric vector of person estimates }
  \item{items}{ numeric vector of item estimates }
  \item{pdf}{ pdf output file }
  \item{title}{ title for graph }
  \item{scale.unit}{ name of units }
  \item{person.color}{ color of person distribution }
  \item{item.color}{ color of item distribution }
  \item{alpha}{ controls transparency of graph, see \link[ggplot2]{geom_density} }
}
\details{
}
\value{
}
\references{
}
\author{
Martin Hecht
}
\note{
}
\seealso{
}
\examples{
\dontrun{
persons <- rnorm ( 5000 )
items <- rnorm ( 200 ) + 1

plotLatentDistr ( persons , items )
}
}
