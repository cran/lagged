\name{Lagged2d-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged2d-class}

\title{Class Lagged2d}
\description{Class Lagged2d.}
\section{Objects from the Class}{
  
  Objects can be created by calls of the form \code{Lagged(m)} or
  \code{new("Lagged2d", data = m)}, where \code{m} is a matrix.
  \code{new("Lagged2d", ...)} also works.


  
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Lagged}"}, directly.
}
\section{Methods}{
\describe{
    \item{[}{\code{signature(x = "Lagged2d", i = "numeric", j = "missing", drop = "logical")}: ... }
    \item{[}{\code{signature(x = "Lagged2d", i = "numeric", j = "missing", drop = "missing")}: ... }
    \item{[<-}{\code{signature(x = "Lagged2d", i = "numeric")}: ... }
    \item{show}{\code{signature(object = "Lagged2d")}: ... }
    \item{whichLagged}{\code{signature(x = "Lagged2d", y = "missing")}: ... }
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
\code{\linkS4class{Lagged3d}}
}
\examples{
powers <- Lagged(outer(1:6, 0:6, `^`))
powers[[0]]
powers[[1]]
powers[[2]]
}
\keyword{classes}
