\name{collapseMissings}
\alias{collapseMissings}
\title{Recode Character Missings of Different Types to 0 or \code{NA}
}
\description{
This function is used to recode character missings in datasets that were prepared 
using the \code{eatPrep} package to 0 or \code{NA}. Additionally, all variables are converted to \code{numeric}. It should be called to recode the missing values prior to passing datasets on to \code{eatModel}.
}
\usage{
collapseMissings(dat, missing.rule = NULL, items)
}

\arguments{
  \item{dat}{
A data frame containing character missings (e.g., \code{mbd} - missing by design). See \sQuote{Details} for supported character missing values.
}
  \item{missing.rule}{
A named list with definitions how to recode the different types of missings in the dataset. The names correspond to the character missings and the list elements correspond to their recode values. If \code{NULL}, the default is used (\code{list(mvi = 0, mnr = 0, mci = NA, mbd = NA, mir = 0, mbi = 0)}). Please note that only the recode value 0 and \code{NA} are currently supported.
}
  \item{items}{
A character vector containing the column names of the data frame for the which character missings should be recoded.
}
}
\details{
One main idea of the \code{eatPrep} package is that different types of 
missing values should remain distinguishable during data preparation, thus allowing the user to flexibly recode them to different values during the IRT scaling process. \code{collapseMissings} facilitates recoding of the different 
types of character missings before IRT analysis or when exporting the data to other software packages (e.g., SPSS). 

The \code{eatPrep} package currently supports six different types of missings, namely

\code{mvi} (text volume insufficient): used in writing tasks if a person wrote to little to evaluate whether they met a specific criterion.

\code{mnr} (missing not reached): used whenever a person did not reach the respective task in his or her test booklet. All consecutive missing values clustered at the end of a test session can be coded \code{mnr}, e.g., by the function \code{\link{mnrCoding}} from package \code{eatPrep}.

\code{mci} (missing coding impossible): used whenever a response cannot be coded due to technical problems (e.g., problems in digitalizing the booklets)

\code{mbd} (missing by design): used whenever an item was not administered to a specific person.

\code{mir} (missing invalid response): used whenever a person attempted to answer an item but this answer cannot be classified in the existing coding scheme. Can also be used for multiple choice-items when the respondent selected more than one option.

\code{mbi} (missing by intention): used whenever a person was expected to answer an item but did not provide a response.

The default recode values for these missing types are: 
text volume insufficient = 0, missing not reached = 0, missing coding impossible = \code{NA}, missing by design = \code{NA}, missing invalid response = 0, missing by intention = 0

}
\value{
A data frame with all missing values coded as 0 or \code{NA} according to the specification in the argument \code{missing.rule}.
}

\references{
OECD (2005). \emph{PISA 2003 Technical Report}. OECD Publishing.
}

\author{
Karoline Sachse, Martin Hecht
}

\examples{
data(inputDat)
dat1 <- inputDat[[1]]  # get first dataset from inputDat
datColMis <- collapseMissings(dat = dat1, 
    missing.rule = list(mvi = 0, mnr = 0, mci = 0, mbd = NA, mir = 0, mbi = 0), 
    items = colnames(dat1)[ -c(1:2) ])
}
