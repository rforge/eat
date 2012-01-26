\name{readDaemonXlsx}
\alias{readDaemonXlsx}

\title{read xlsx-Files produced by ZKDaemon
}
\description{read xlsx-Files produced by ZKDaemon
}
\usage{
readDaemonXlsx(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
A character string containing path, name and extension of .xlsx produced by
ZKDaemon. Caution! Sheet order is important (see Datails).
}
}
\details{
 Compulsory: 1st sheet: units. 2nd sheet: subunits. 3rd sheet: values.
 Optional: 4th sheet: unitRecodings. 5th sheet: savFiles. 6th sheet: newID.
      7th sheet: aggregateMissings. 8th sheet: unitProperties.
      9th sheet: property labels. 10th sheet: booklets.
}

\value{
A list of data frames containing information that is required by
\code{\link{automateDataPreparation}}
}

\author{
Karoline Sachse
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
 \examples{str(inputList)
}
