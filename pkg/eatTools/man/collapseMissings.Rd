\name{collapseMissings}
\alias{collapseMissings}
\title{Recode Character Missings of Different Types to 0 or \code{NA}
}
\description{
This function is used to recode character missings in datasets that were prepared 
with functions from the \code{eatPrep} package to 0 or \code{NA}. It is called by
several functions of the \code{eat} package family.
}
\usage{
collapseMissings(dat, missing.rule = NULL, items)
}

\arguments{
  \item{dat}{
data frame containing character missings (e.g. type \code{mbd} - missing by design)
}
  \item{missing.rule}{
A list with definitions how to recode the different types of missings in the dataset. 
          If \code{NULL}, the default described in 'Details' is used.
}
  \item{items}{
A character vector containing the column names of the data frame for which character missings are to be recoded.
}
}
\details{
One of the main ideas of the \code{eat} package family is that different types of 
missing values should remain distinguishable during data preparation, thus allowing
the user to flexibly recode them to different values during the IRT scaling process. 
\code{collapseMissings} can be used to facilitate the recoding of the different 
types of character missings before scaling or when exporting the data to other software 
packages (e. g., SPSS). 

The \code{eat} package family currently supports six different types of missings, namely

\code{mvi} (text volume insufficient): used in writing tasks if a person wrote to little to evaluate whether they met a specific criterion.

\code{mnr} (missing not reached): used whenever a person did not reach the respective task in his or her test booklet. 
            All consecutive missing values clustered at the end of a test session can be coded \code{mnr}, 
            e.g., by the function \code{recodeMbiToMnr} from package \code{eatPrep}.

\code{mci} (missing coding impossible): used whenever a response cannot be coded due to technical problems (e.g., problems in digitalizing the booklets)

\code{mbd} (missing by desing): used whenever an item was not administered to a specific person.

\code{mir} (missing invalid response): used whenever a person attempted to answer 
              an item but this answer cannot be classified in the existing coding scheme. 
              Can also be used for multiple choice-items when the respondent selected more than one option.

\code{mbi} (missing by intention): used whenever a person was expected to answer an item but did not provide a response.

The default recode values for these missing types are: 
text volume insufficient = 0, missing not reached = 0, missing coding impossible = \code{NA}, missing by design = \code{NA}, 
missing invalid response = 0, missing by intention = 0

}
\value{
A data frame with recoded missings.
}

\references{
OECD (2005). \emph{PISA 2003 Technical Report}. OECD Publishing.
}

\author{
Karoline Sachse, Martin Hecht
}
%\examples{
%	data(inputDat)
%	dat1 <- inputDat[[1]]  # get first dataset from inputDat
%	datColMis <- collapseMissings(dat = dat1, 
%	missing.rule = list(mvi = 0 ,mnr = 0 ,mci = 0 ,mbd = NA ,mir = 0 ,mbi = 0), 
%	items=colnames(dat1)[- c(1:2)])
%}
