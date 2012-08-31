\name{recodeMbiToMnr}
\alias{recodeMbiToMnr}
\title{
Recode Missing by Intention to Missing not Reached
}
\description{
\code{recodeMbiToMnr} converts codes for missing by intention to missing not reached
}

\usage{
recodeMbiToMnr (dat, id, booklets, blocks, rotation, nMbi = 2)
}

\arguments{
	\item{dat}{
		A dataset. Missing by intention needs to be coded \code{mbi}.
	}
	\item{id}{
		Name or column number of identifier (ID) variable in dataset.
	}
	\item{booklets}{
		specification of booklets
	}
	\item{blocks}{
		block specification
	}
	\item{rotation}{
		booklet information
	}
	\item{nMbi}{
		Number of \code{mbi}-Codes required at the end of a block to code \code{mnr}. Needs to be >=1. 
	}
		
}
\details{
In order to code \code{mnr}, a certain number of subitems at the end of a block need to be coded \code{mbi}. This number can be specified with the argument \code{nMbi}. The default is 2, i.e. if the last and second to last subitem in a block are coded \code{mbi}, both subitems will be recoded to \code{mnr}. If \code{nMbi} is larger than the number of subitems in a given block, no subitem in this block will be recoded.

If all subitems in a block are coded \code{mbi}, none of them will be recoded to \code{mnr}.
}
\value{
A data frame with missing not reached coded as \code{mnr}. For each person with at least one \code{mnr} in the returned dataset the names of recoded variables are given as an attribute to \code{dat}.
}

\references{

}
\author{
Nicole Haag
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
data(inputDat)
data(inputList)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
