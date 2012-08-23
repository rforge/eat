\name{plotDistributions}
\alias{plotDistributions}
\title{
plot latent distribution
}
\description{
creates latent distribution plots of two distributions on the same metric (e.g. persons and items)
}
\usage{
plotDistributions ( distr1 , distr2 , distr1.name = "Persons" , distr2.name = "Items" , pdf = NULL , title = NULL , scale.unit = "Logit" , distr1.color = "#E69F00" , distr2.color = "#56B4E9" , alpha = 0.5 )
}
\arguments{
  \item{distr1}{ numeric vector of person estimates }
  \item{distr2}{ numeric vector of item estimates }
  \item{distr1.name}{ name of distribution 1 (e.g. "Persons") }
  \item{distr2.name}{ name of distribution 2 (e.g. "Items") }
  \item{pdf}{ pdf output file }
  \item{title}{ title for graph }
  \item{scale.unit}{ name of units }
  \item{distr1.color}{ color of distribution 1}
  \item{distr2.color}{ color of distribution 2}
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

plotDistributions ( persons , items )
}
}
