\name{readDaemonXlsx}
\alias{readDaemonXlsx}

\title{Read xlsx-Files Produced by ZKDaemon
}
\description{This function is primarily intended for internal use at the Institute for Educational Quality Improvement (IQB). The xlsx-files for this function are produced by the software ZKDaemon using information stored in the IQB-Databases (DB2/DB3/DB4).
}
\usage{
readDaemonXlsx(filename, type = c("ZKDaemon", "Merkmalsauszug") )
}
\arguments{
  \item{filename}{
A character string containing path, name and extension of .xlsx produced by
ZKDaemon.
}
  \item{type}{
A character which spacifies what type of database export file should be read in.
}
}
\details{
The xlsx-file produced by 'ZKDaemon' is expected to have the following sheets: \dQuote{units}, \dQuote{subunits}, \dQuote{values}, \dQuote{unitrecoding}, \dQuote{sav-files}, \dQuote{params}, \dQuote{aggregate-missings}, \dQuote{itemproperties}, \dQuote{propertylabels}, \dQuote{booklets}, and \dQuote{blocks}. \code{readDaemonXlsx} will produce a warning if any sheets are missing or wrongly specified.
The xlsx-file produced by 'Merkmalsauszug' is expected to have the following sheets: \dQuote{Aufgabenmerkmale}, \dQuote{Itemmerkmale}. \code{readDaemonXlsx} will produce a warning if any sheets are missing or wrongly specified.
}
\value{
A list of data frames containing information that is required by the data preparation functions in \code{eatPrep}. \code{\link{inputList}} shows an example of this list.
}
\author{
Karoline Sachse
}
