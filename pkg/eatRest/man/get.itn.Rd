\name{get.itn}
\alias{get.itn}
\title{Read ConQuest 'itanal' Output Files
}
\description{
Reads ConQuest files comprising item analyses generated by the 'itanal' statement.
}
\usage{
get.itn(file)
}
\arguments{
  \item{file}{
Character string with the name of the ConQuest item analysis file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame with one row per item response category containing the following columns:

\item{item.nr}{Number of the item in the analysis}
\item{item.name}{Name of the item}
\item{Label}{Response category label}
\item{Score}{Score of this response category}
\item{n.valid}{Total number of students who responded to this item}
\item{Abs.Freq}{Number of students who gave this response}
\item{Rel.Freq}{Number of students who gave this response as a percentage of the total number of respondents to the item}
\item{p}{Percentage of students who answered this item correctly}
\item{pt.bis}{Point-biserial for this response}
\item{t.value}{T-Value of the significance test whether the point-biserial correlation is different from 0}
\item{p.value}{p-Value of the significance test whether the point-biserial correlation is different from 0}
\item{PV1.Avg.1}{Mean ability of students who gave this response (based on plausible values)}
\item{PV1.SD.1}{Standard deviation of ability of students who gave this response (based on plausible values)}
\item{pbc}{Item discrimination}
\item{threshold}{Item threshold}
\item{delta}{Item delta}

If the model is multidimensional, the mean and standard deviation of the ability of students who gave the respective response will be shown for each dimension.

}
\references{
See pp.193 of Wu, M.L., Adams, R.J., Wilson, M.R., & Haldane, S.A. (2007). \emph{ACER ConQuest Version 2.0. Generalised Item Response Modeling Software}. Camberwell, Victoria: ACER Press.
}
\author{
Sebastian Weirich
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
