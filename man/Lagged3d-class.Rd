\name{Lagged3d-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged3d-class}

\title{Class Lagged3d}
\description{Class Lagged3d.}
\section{Objects from the Class}{
    
  Objects can be created by calls of the form \code{Lagged(a)} or
  \code{new("Lagged3d", data = a)}, where \code{a} is a 3-d array.
  \code{new("Lagged3d", ...)} also works.

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
    \item{[}{\code{signature(x = "Lagged3d", i = "numeric", j = "missing", drop = "logical")}: ... }
    \item{[}{\code{signature(x = "Lagged3d", i = "numeric", j = "missing", drop = "missing")}: ... }
    \item{[<-}{\code{signature(x = "Lagged3d", i = "numeric")}: ... }
    \item{show}{\code{signature(object = "Lagged3d")}: ... }
    \item{whichLagged}{\code{signature(x = "Lagged3d", y = "missing")}: ... }}
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
## see examples for class "Lagged"
}
\keyword{classes}
