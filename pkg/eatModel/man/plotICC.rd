\name{plotICC}
\alias{plotICC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots item charactestic curves.}
\description{Function provides item characteristic plots for each item. To date, 
only dichotomouls 1pl and 2pl models are supported.}
\usage{plotICC  ( resultsObj, defineModelObj, item = NULL, personsPerGroup = 30, 
       pdfFolder = NULL )}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultsObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{getResults}. 
}
  \item{defineModelObj}{
%%     ~~Describe \code{file} here~~
The object returned by \code{defineModel}. 
}
  \item{item}{
%%     ~~Describe \code{file} here~~
Optional: The item for which the ICC should be plotted. If NULL, ICCs of all items
will be collected in a common pdf. The \code{pdfFolder} argument than must not be NULL.
}
  \item{personsPerGroup}{
%%     ~~Describe \code{file} here~~
Specifies the number of persons in each interval of the theta scale for dividing the 
persons in various groups according to mean EAP score. 
}
  \item{pdfFolder}{
%%     ~~Describe \code{file} here~~
Optional: A folder with writing access for the pdf file. Necessary only if ICCs for more
tnah one item should be plotted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sebastian Weirich
}
\note{
This function is beta! Use with care...
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(sciences)

# first reshape the data set into wide format
datW <- reshape2::dcast(sciences, id+grade+sex~variable, value.var="value")

# defining the model: specifying q matrix is not necessary
mod1 <- defineModel(dat=datW, items= -c(1:3), id="id", software = "tam")

# run the model
run1 <- runModel(mod1)

# get the results
res1 <- getResults(run1)

# plot for one item 
plotICC  ( resultsObj = res1, defineModelObj = mod1, item = "BioPro13")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
