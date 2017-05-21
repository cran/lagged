\name{Lagged3d-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged3d-class}
%\alias{[,Lagged3d,numeric-method}

\title{Class Lagged3d}
\description{Class Lagged3d.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Lagged3d", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"array"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Lagged}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Lagged3d", i = "numeric")}: ... }
	 }
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Lagged}},
\code{\linkS4class{Lagged1d}},
\code{\linkS4class{Lagged2d}}
}
\examples{
showClass("Lagged3d")
}
\keyword{classes}
