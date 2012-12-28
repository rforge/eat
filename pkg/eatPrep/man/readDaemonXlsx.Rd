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
ZKDaemon. Caution! Sheet names are important (see Details).
}
}
\details{
Reads in the following .xlsx sheets: "units", "subunits", "values", "unitrecoding", "sav-files", "params", "aggregate-missings", "itemproperties", "propertylabels", "booklets", "blocks"
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
