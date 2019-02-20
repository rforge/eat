\name{shade.between.lines}
\alias{shade.between.lines}

\title{
draw a shade between two lines in ggplot2
}

\description{
draw a shade between two lines in ggplot2; adapted from http://mcfromnz.wordpress.com/2014/06/02/shading-between-two-lines-ggplot/
}

\usage{
shade.between.lines ( d )
}

\arguments{
  \item{data}{a data frame; first column contains values on x-axis, second column contains values on y-axis; can contain further columns that are looped through}
}
%\details{
%}
\value{
A data frame with two additional columns: polygr.group are the groups to be passed to geom_polygon, polygr.sort is the order in which the data.frame has to be
}

\author{
adapted from http://mcfromnz.wordpress.com/2014/06/02/shading-between-two-lines-ggplot/
}

%\examples{
%}
