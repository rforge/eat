\name{eatGot-class}
\Rdversion{1.1}
\docType{class}
\alias{eatGot-class}
\alias{show,eatGot-method}

\title{Class \code{"eatGot"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
An object that contains model results.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("eatGot")}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{results}:}{Object of class \code{"data.frame"}
	
	contains model results }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "eatGot")}: displays an object of class "eatGot" }
	 }
}
%% \author{
%% Martin Hecht
%% }
%% \note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning }{
This version is alpha. Use with care.
}

%%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%% \code{\link{defineDesign}}
%%}

\examples{
showClass("eatGot")
}
\keyword{classes}
