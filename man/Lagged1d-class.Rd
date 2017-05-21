\name{Lagged1d-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged1d-class}
%\alias{[,Lagged1d,numeric-method}
%\alias{[<-,Lagged1d,numeric-method}

\title{Class Lagged1d}
\description{Class Lagged1d.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Lagged1d", ...)}.
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"vector"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Lagged}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Lagged1d", i = "numeric")}: ... }
    \item{[<-}{\code{signature(x = "Lagged1d", i = "numeric")}: ... }
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
\code{\linkS4class{Lagged2d}},
\code{\linkS4class{Lagged3d}}
}
\examples{
showClass("Lagged1d")
}
\keyword{classes}
