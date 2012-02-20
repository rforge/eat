\name{inputList}
\alias{inputList}
\docType{data}
\title{Data Frames with Code, Subunit and Unit Information for Datasets in \code{\link{inputDat}}
}
\description{
These data frames contain information about codes, subunits and units for the
datasets in \code{\link{inputDat}} and are necessary inputs for functions
\code{\link{automateDataPreparation}}, \code{\link{checkData}},
\code{\link{recodeData}} and \code{aggregateData}.
}
\usage{data(inputList)}
\format{
A list with three data frames:
\enumerate{

    \item units: Unit information, contains the following columns:


    \describe{
      \item{unit}{Unit name.}
      \item{unitType}{Subunit types:
        \code{ID} = ID variable;
        \code{TI} = test item;
        \code{CV} = context variable.}
      \item{unitLabel}{Unit label, to be used by \code{\link{writeSpss}}.}
      \item{unitDescription}{Unit description.}
      \item{unitAggregateRule}{Aggregate rule for unit:
        \code{SUM}; \code{MEAN}.}
      \item{unitScoreRule}{Scoring rule for unit (not sure how this will be used in the future.)}
    }

  \item subunits: Subunit information, contains the following columns:
    
	\describe{
      \item{unit}{Unit name, for which subunits are given.}
      \item{subunit}{Subunit name.}
      \item{subunitType}{Subunit types:\code{'?'}.}
      \item{subunitLabel}{Subunit label, to be used by \code{\link{writeSpss}}.}
      \item{subunitDescription}{Subunit descriptions.}
      \item{subunitPosition}{Subunit position in test booklet (e.g., line 1).}
      \item{subunitTransniveau}{Subunit transformation level.}
      \item{subunitRecoded}{Name of recoded subunit.}
      \item{subunitLabelRecoded}{Label for recoded subunit, to be used when \code{\link{writeSpss}} is applied to a dataset
                                produced by \code{\link{recodeData}}.}
    } 

	  \item values: Value information, contains the following columns:
	\describe{
	  \item{subunit}{Subunit name, for which values are given.}
	  \item{value}{Valid values for the respective subunit.}
	  \item{valueRecode}{Recode values for the respective value.}
	  \item{valueType}{Value types:
		\code{vc} = valid code;
		\code{mbd} = missing -- by design;
		\code{mvi} = missing -- volume insufficient;
		\code{mnr} = missing -- not reached;
		\code{mci} = missing -- coding impossible;
		\code{mbi} = missing -- by intention.
	  }
	  \item{valueLabel}{Value labels, to be used by \code{\link{writeSpss}}.}
	  \item{valueDescription}{Value descriptions.}
	  \item{valueLabelRecoded}{Labels for recoded values, to be used when \code{\link{writeSpss}} is applied to a dataset
								produced by \code{\link{recodeData}}.}
	  \item{valueDescriptionRecoded}{Descriptions for recoded values.}
	}
	
    \item unitRecodings: Unit recoding information, contains the following columns:

	\describe{
      \item{unit}{Unit name}
      \item{value}{Valid values for the respective unit.}
      \item{valueRecode}{Recode values for the respective value.}
 	    \item{valueType}{Value types:
		  \code{vc} = valid code;
		  \code{mbd} = missing -- by design;
		  \code{mvi} = missing -- volume insufficient;
		  \code{mnr} = missing -- not reached;
		  \code{mci} = missing -- coding impossible;
		  \code{mbi} = missing -- by intention.
	    }
  	  \item{valueLabel}{Value labels, to be used by \code{\link{writeSpss}}.}
	  \item{valueDescription}{Value descriptions.}
	  \item{valueLabelRecoded}{Labels for recoded values, to be used when \code{\link{writeSpss}} is applied to a dataset
								produced by \code{\link{recodeData}}.}
    }

        \item savFiles: information for \code{\link{loadSav}}, contains the following columns:

	\describe{
      \item{filename}{SPSS filenames}
      \item{case.id}{ID variable in the respective dataset, used by \code{\link{mergeData}}}
    }
    
 \item newID: information for \code{\link{mergeData}}, contains the following columns:

	\describe{
      \item{key}{one of the entries should be \code{master-id}}
      \item{value}{the corresponding value; how the ID variable in the final dataset shall be named}
    }

 \item aggrMiss: missing aggregation pattern for \code{\link{aggregateData}}


}
}
\details{

}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(inputList)
str(inputList)
}
\keyword{datasets}
